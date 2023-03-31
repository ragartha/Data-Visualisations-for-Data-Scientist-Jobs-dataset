#import necessary libraries
library("dplyr")
library("janitor")
library("tidyr")
library("stringr")
library("naniar")
library("wordcloud")
library("ggplot2")
library("usmap")
library("tm")
library("xtable")
library("ggpubr")
library("plotrix")
library("forcats")

#Create an object for the data file name and import data
csv.name <- "DataScientist.csv"
DataSci = read.csv(csv.name)

#Explore the column types
glimpse(DataSci)

#1-----------Data cleaning-------------
#basic cleaning steps
DataSci<- DataSci%>%drop_na()
DataSci<-DataSci %>% remove_empty(which =c("rows"))
DataSci<-DataSci %>% remove_empty(which =c("cols"))

#remove duplicate rows
DataSci <- DataSci%>% distinct()

#Visualize missing values: replace all -1 and "Unknown / Non-Applicable" values with NA
DataSci <- replace_with_na_all(data = DataSci,
                               condition = ~.x == -1)
DataSci <- replace_with_na_all(data = DataSci,
                               condition = ~.x == "Unknown / Non-Applicable")

#drop the columns with many missing values: competitors, revenue, founded. Also drop index
DataSci<- select(DataSci, -c(index ,Competitors, Revenue, Founded))

#as both have same number of missing values, will replace by unknown industry or sector
DataSci<- DataSci%>%mutate(Sector = replace_na(Sector, "Not specified"))
DataSci<- DataSci%>%mutate(Industry = replace_na(Industry, "Not specified"))

#Change easy.Apply values to Yes or No
DataSci<- DataSci%>%mutate(Easy.Apply = replace_na(Easy.Apply, "No"))
DataSci<- DataSci%>%mutate(Easy.Apply = replace(Easy.Apply,Easy.Apply=="True", "Yes"))

#replace the missing values in ratings with the average rating
DataSci$Rating[is.na(DataSci$Rating)] <- mean(DataSci$Rating, na.rm = TRUE)
DataSci$Rating <- round(DataSci$Rating)
vis_miss(DataSci)

#only 1% of the data is now missing, the rows can be removed.
DataSci <- na.omit(DataSci)
vis_miss(DataSci)

#----------Data transformation

#Creating set categories to analyse: Data Engineer, Data Scientist, and Data Analyst

DataSci <- DataSci%>%mutate(Job = case_when(
  grepl("Analyst", DataSci$Job.Title,ignore.case = TRUE)==TRUE ~ "Data Analyst",
  grepl("Engineer", DataSci$Job.Title,ignore.case = TRUE)==TRUE ~ "Data Engineer",
  grepl("Scientist", DataSci$Job.Title,ignore.case = TRUE)==TRUE ~ "Data Scientist"))
DataSci<- na.omit(DataSci)
DataSci<- select(DataSci, - Job.Title)%>%relocate(Job, .after = Salary.Estimate)

#Transforming the column Salary Estimate

DataSci$Salary.Estimate <- str_replace_all(DataSci$Salary.Estimate,"\\((.*?)\\)","")
DataSci$Salary.Estimate<-gsub("K","", as.character(DataSci$Salary.Estimate))
DataSci$Salary.Estimate<-gsub("\\$","", as.character(DataSci$Salary.Estimate))

#divide salary into min and max, add a column for the average salary
DataSci <- DataSci %>% separate(Salary.Estimate, c("min_salary","max_salary"), "-") 
number1 <- str_extract(DataSci$min_salary,"[0-9]+") %>% as.numeric()
number2 <- str_extract(DataSci$max_salary,"[0-9]+") %>% as.numeric()

#for later calculations, remove dollar sign and K and multiply by 100
pattern <- str_detect(DataSci$max_salary,"Hour")
DataSci$min_salary <- ifelse(pattern == TRUE, number1*40*52, DataSci$min_salary)
DataSci$max_salary <- ifelse(pattern == TRUE, number2*40*52, DataSci$max_salary)
DataSci$min_salary <- as.numeric(DataSci$min_salary)
DataSci$max_salary <- as.numeric(DataSci$max_salary)
DataSci$max_salary <- DataSci$max_salary * 1000
DataSci$min_salary <- DataSci$min_salary * 1000
DataSci <- DataSci %>% mutate(AverageSalaryEstimate = (number1 + number2)/2) %>%relocate(AverageSalaryEstimate, .after = max_salary)
DataSci$AverageSalaryEstimate <-DataSci$AverageSalaryEstimate *1000

#visualise outliers

nums<- unlist(lapply(DataSci, is.numeric))
num_datasci<-DataSci[ , nums]
boxplot(num_datasci)$out
#remove outliers
outliers1 <- boxplot(num_datasci$max_salary, plot=FALSE)$out
outliers2 <- boxplot(num_datasci$min_salary, plot=FALSE)$out

x<-DataSci
x<- x[-which(x$max_salary %in% outliers1),]
x<- x[-which(x$min_salary %in% outliers2),]
DataSci<- x

#Clean company.name column
number <- str_extract(DataSci$Company.Name,"[[:digit:]]+.+[[:digit:]]")
DataSci$Company.Name<- str_replace(DataSci$Company.Name,number, replacement = "")
DataSci$Company.Name<- str_replace(DataSci$Company.Name,",", replacement = "")
DataSci$Company.Name<- str_replace(DataSci$Company.Name,"-", replacement = "")

#Remove NA values
DataSci <- na.omit(DataSci)
vis_miss(DataSci)

#location and headquarters contain both city and state or country, we can separate them.
DataSci <- DataSci %>% separate(Location, c("City","State_country"), ",")
DataSci$State_country<- str_replace(DataSci$State_country," ", replacement = "")
DataSci <- DataSci %>% separate(Headquarters, c("HQCity","HQState_country"), ",") 
DataSci$HQState_country<- str_replace(DataSci$HQState_country," ", replacement = "")

#an outlier shows up  in the HQState _country column. It can be replaced by the state NY 
#based on the city value New York
DataSci$HQState_country[DataSci$HQState_country=="061"]<-"NY"


#-----------------Visualisations---------------------------

# Percentage of job titles

Job_titles <- DataSci %>%group_by(Job) %>%summarise(counts = n())%>%
  arrange(desc(counts))%>%mutate(freq = paste(round(100*counts / sum(counts)),"%"))

pie.labels<- paste(Job_titles$Job, ",", Job_titles$freq, sep = "")
pie(Job_titles$counts, labels =pie.labels, col = brewer.pal(4,"PuBu"))

#average salary by job title analysis
DataSci_job <- select(DataSci, c(Job,AverageSalaryEstimate))

ggboxplot(DataSci_job, x = "Job", y = "AverageSalaryEstimate", 
          color = "Job", palette = c("#d55e00", "#0072b2", "#009e73"),
          ylab = "Average Salary Estimate (in $/year)", xlab = "Job categories")

#ANOVA test to verify results statistically
salary_job<-group_by(DataSci_job, Job) %>%
  summarise(
    count = n(),
    mean = mean(AverageSalaryEstimate, na.rm = TRUE),
    sd = sd(AverageSalaryEstimate, na.rm = TRUE))
jobs.aov <- aov(AverageSalaryEstimate ~ Job, data = DataSci_job)
summary(jobs.aov)

#Job categories and easy apply
salary_easyApply<- select(DataSci, c(AverageSalaryEstimate, Job, Easy.Apply, Size))

ggplot(salary_easyApply, aes(x = Job,fill = Easy.Apply)) + 
  geom_bar(position = "dodge") +
  labs(x=NULL, y=NULL)+scale_fill_brewer(palette="Paired")

#easy apply, salary estimate, and job categories
ggplot(data=DataSci, aes(x=Job, y=AverageSalaryEstimate, group=Easy.Apply, color=Easy.Apply))+ 
  stat_summary(geom = "line", fun='mean') + 
  stat_summary(geom = "point", fun='mean')

#Correlation between salary and company rating by job type
ggplot(DataSci, aes(x = Job, y = Rating, fill = AverageSalaryEstimate)) +
  geom_tile()+
  guides(fill = guide_colourbar(title = "Average salary in $"))+scale_fill_gradient(low="#380400",high="#BB1C1C")


#top 10 sectors and industries in terms of salary
top10_sectors<- DataSci%>%group_by(Sector)%>% summarise(max_salary_sector = round(mean(max_salary)))%>%
  arrange(desc(max_salary_sector))%>%head(10)
top10_sectors$Sector <- reorder(top10_sectors$Sector, top10_sectors$max_salary_sector)


ggplot(top10_sectors, aes(x = Sector, y = max_salary_sector, fill = Sector, label = max_salary_sector)) +
  geom_bar(stat="identity", show.legend = FALSE ) +
  coord_flip() +
  labs(title = "Top 10 highest paying sectors", x = "Sectors", y = "Salary in $") +
  geom_label(aes(fill = Sector),colour = "white", fontface = "bold", show.legend = FALSE)+scale_fill_brewer(palette="Paired")


#top 10 industries and industries in terms of salary
top10_industries<- DataSci%>%group_by(Industry)%>% summarise(max_salary_industry = round(mean(max_salary)))%>%
  arrange(desc(max_salary_industry))%>%head(10)
top10_industries$Industry <- reorder(top10_industries$Industry, top10_industries$max_salary_industry)


ggplot(top10_industries, aes(x = Industry, y = max_salary_industry, fill = Industry, label = max_salary_industry)) +
  geom_bar(stat="identity", show.legend = FALSE ) +
  coord_flip() +
  labs(title = "Top 10 highest paying industries", x = "Industries", y = "Salary in $") +
  geom_label(aes(fill = Industry),colour = "white", fontface = "bold", show.legend = FALSE)+scale_fill_brewer(palette="Paired")

#top 10 employing sectors

sector_employment <- DataSci %>%group_by(Sector) %>%summarise(counts = n())%>%
  arrange(desc(counts))%>%mutate(freq = paste(round(100*counts / sum(counts)),"%"))%>%head(10)

pie.labels<- paste(sector_employment$Sector, ",", sector_employment$freq, sep = "")
pie(sector_employment$counts, labels =pie.labels, main = "Top 10 sectors by employment rate", col = brewer.pal(10,"PRGn"))

#top 10 employing industries
industry_employment <- DataSci %>%group_by(Industry) %>%summarise(counts = n())%>%
  arrange(desc(counts))%>%mutate(freq = paste(round(100*counts / sum(counts)),"%"))%>%head(10)

pie.labels<- paste(industry_employment$Industry, ",", industry_employment$freq, sep = "")
pie(industry_employment$counts, labels =pie.labels, main = "Top 10 industries by employment rate", col = brewer.pal(10,"PiYG"))

#--- word cloud to analyse job description most prevalent words
job_desc <- select(DataSci, Job.Description)
jd<-Corpus(VectorSource(job_desc$Job.Description)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(tolower)  %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(PlainTextDocument)

jd <- tm_map(jd, stripWhitespace)
jd <- Corpus(VectorSource(jd))

jdm<- TermDocumentMatrix(jd)%>%as.matrix(jdm)
v <- sort(rowSums(jdm),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#top 20 most used words in job adverts
top20 <- head(d, 20)
top20$word <- reorder(top20$word, top20$freq)

ggplot(top20, aes(x = word, y = freq, fill = word, label = freq)) +
  geom_bar(stat="identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Top 20 Most Used Words in data science job adverts", x = "Word",
       y = "Word Count") +
  geom_label(aes(fill = word),colour = "white",
             fontface = "bold",
             show.legend = FALSE)


#-----	Analysing salaries by location

salaries<- select(DataSci, c(min_salary,max_salary, State_country, Job))%>%group_by(State_country)%>%summarise(average=mean(min_salary),mean(max_salary))

#first, let's see the average wage in countries and states
Data_location<- select(DataSci, c("AverageSalaryEstimate", "State_country"))

#since the states are mentioned, we can visualise the salaries in the US
Data_location_us <-Data_location[Data_location$State_country %in% c("AK", "AL", "AR", "AS", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "GU", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MP", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UM", "UT", "VA", "VI", "VT", "WA", "WI", "WV", "WY"), ]
Data_location_us <- Data_location_us%>%group_by(State_country)%>%summarise(average=mean(AverageSalaryEstimate))%>%rename(state=State_country)

Data_location_world<-Data_location
Data_location_world$State_country[Data_location_world$State_country %in% c("AK", "AL", "AR", "AS", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "GU", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MP", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UM", "UT", "VA", "VI", "VT", "WA", "WI", "WV", "WY")]<-"United States"
Data_location_world <- Data_location_world%>%group_by(State_country)%>%summarise(average=mean(AverageSalaryEstimate))%>%rename(Country=State_country)


#generate the map showing average salaries in the US by company headquarters 

plot_usmap(
  data = Data_location_us, values = "average", lines = "red", labels = TRUE
) + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Average Salary", label = scales::comma
  ) + 
  labs(title = "US States", subtitle = "States and average salary data") +
  theme(legend.position = "right")

p<- plot_usmap(
  data = Data_location_us, values = "average", lines = "red", labels = TRUE
) + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Average Salary", label = scales::comma
  ) + 
  labs(title = "US States", subtitle = "States and average salary data") +
  theme(legend.position = "right")
p$layers[[2]]$aes_params$size <- 2
print(p)


#job distribution by salaries and location of headquarters
Data_locationHQ<- select(DataSci, c("AverageSalaryEstimate", "HQState_country"))
Data_location_usHQ <-Data_locationHQ[Data_locationHQ$HQState_country %in% c("AK", "AL", "AR", "AS", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "GU", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MP", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UM", "UT", "VA", "VI", "VT", "WA", "WI", "WV", "WY"), ]
Data_location_usHQ <- Data_location_usHQ%>%group_by(HQState_country)%>%summarise(average=mean(AverageSalaryEstimate))%>%rename(state=HQState_country)

#generate the map showing average salaries in the US
plot_usmap(
  data = Data_location_usHQ, values = "average", lines = "red", labels = TRUE
) + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Average Salary", label = scales::comma
  ) + 
  labs(title = "US States", subtitle = "States and average salary data") +
  theme(legend.position = "right")

p<- plot_usmap(
  data = Data_location_usHQ, values = "average", lines = "red", labels = TRUE
) + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Average Salary", label = scales::comma
  ) + 
  labs(title = "US States", subtitle = "States and average salary data") +
  theme(legend.position = "right")
p$layers[[2]]$aes_params$size <- 2
print(p)


#We can also visualise the min and max salary by continent
salaries<- select(DataSci, c(min_salary,max_salary, HQState_country, Job))%>%group_by(HQState_country)%>%
  summarise(min_salary=mean(min_salary),max_salary=mean(max_salary))%>%rename(Country=HQState_country)

Data_location_worldHQ <-salaries
Data_location_worldHQ$Country[Data_location_worldHQ$Country %in% c("AK", "AL", "AR", "AS", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "GU", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "NY (US)","LA", "MA", "MD", "ME", "MI", "MN", "MO", "MP", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UM", "UT", "VA", "VI", "VT", "WA", "WI", "WV", "WY")]<-"United States"

Data_location_worldHQ$Country[Data_location_worldHQ$Country %in%
                                        c("United States","Bermuda")]<-"North America"
Data_location_worldHQ$Country[Data_location_worldHQ$Country %in%
                                c("Brazil", "Mexico")]<-"South America"

Data_location_worldHQ$Country[Data_location_worldHQ$Country %in%
                                        c("South Korea", "Singapore","China","Vietnam","Japan","Iran")]<-"Asia"
Data_location_worldHQ$Country[!Data_location_worldHQ$Country %in%
                                c("North America","South America", "Asia")]<-"Europe"
Data_location_worldHQ <- Data_location_worldHQ%>%group_by(Country)%>%summarise(min_salary=mean(min_salary), max_salary=mean(max_salary))%>%rename(Continent=Country)


Data_location_worldHQ <- Data_location_worldHQ %>% 
  pivot_longer(
    cols = -Continent,
    names_to = "Salary",
    values_to = "values"
  )

ggplot(Data_location_worldHQ, aes(fill=Salary, y=values, x=Continent)) + 
  geom_bar(position="dodge", stat="identity")+ scale_fill_manual(values = 
                                                                   c("max_salary" = "#B0641E",
                                                                     "min_salary" = "#BB4743"))




