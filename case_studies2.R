#PREPARING DATA FOR ANALYSIS

#installing packages
install.packages("tidyverse")
library(tidyverse)

#importing each file separately for further analysis
#importing separately without duplicates

dec <- unique(read_csv("C:/Users/USER/cyclistic_datas/2022/2021_2022/202112-divvy-tripdata.csv"))
jan <- unique(read_csv("C:/Users/USER/cyclistic_datas/2022/2021_2022/202201-divvy-tripdata.csv"))
feb <- unique(read_csv("C:/Users/USER/cyclistic_datas/2022/2021_2022/202202-divvy-tripdata.csv"))
mar <- unique(read_csv("C:/Users/USER/cyclistic_datas/2022/2021_2022/202203-divvy-tripdata.csv"))
apr <- unique(read_csv("C:/Users/USER/cyclistic_datas/2022/2021_2022/202204-divvy-tripdata.csv"))
may <- unique(read_csv("C:/Users/USER/cyclistic_datas/2022/2021_2022/202205-divvy-tripdata.csv"))
jun <- unique(read_csv("C:/Users/USER/cyclistic_datas/2022/2021_2022/202206-divvy-tripdata.csv"))
jul <- unique(read_csv("C:/Users/USER/cyclistic_datas/2022/2021_2022/202207-divvy-tripdata.csv"))
aug <- unique(read_csv("C:/Users/USER/cyclistic_datas/2022/2021_2022/202208-divvy-tripdata.csv"))
sep <- unique(read_csv("C:/Users/USER/cyclistic_datas/2022/2021_2022/202209-divvy-publictripdata.csv"))
oct <- unique(read_csv("C:/Users/USER/cyclistic_datas/2022/2021_2022/202210-divvy-tripdata.csv"))
nov <- unique(read_csv("C:/Users/USER/cyclistic_datas/2022/2021_2022/202211-divvy-tripdata.csv"))


#merging the data frames into one
#viewing the data to check for inconsistencies
#the column names are consistent 
#each column is checked for it's relevance to the case study
#four columns will be dropped. the starting and ending latitude and longitude

cyclist_data <- rbind(dec, jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, deparse.level = 1)
View(cyclist_data)

colnames(cyclist_data)

cyclist_1 <-
  cyclist_data %>%
  select(-start_lat, 
         -start_lng, 
         -end_lat,
         -end_lng)
colnames(cyclist_1)


#columns with unique values are checked for distinct list accuracy
#the unique lists are accurate and consistent; spelling and number of items listed

unique(cyclist_1$rideable_type)
unique(cyclist_1$member_casual)

#continue
#cleaning the data by checking and dealing with missing values na, NaN
#data is better cleaned separately before merging
#no missing values. data is clean

str(cyclist_1)
summary(cyclist_1)

#creating new columns from started_at
#ride lenght for each rider and weekdays that each rider started

cyclist_2 <- cyclist_1 %>%
  mutate (ride_lenght = ended_at - started_at,
          day_of_week = weekdays(started_at))
summary(cyclist_2)

#save file for export for further analysis in SQL
write.csv(cyclist_2, file = "C:/Users/USER/cyclistic_datas/saved_files/cyclist_2.csv")

#save cleaned data in an R file without having to reload from start
#every time am back to analysis after a short break
saveRDS(cyclist_2, "C:/Users/USER/OneDrive/Documents/case_studies_scripts/cyclist_2")
write()

#save imported data into a new variable
cyclists <- read_csv("C:/Users/USER/cyclistic_datas/saved_files/cyclist_2.csv")

#checking and resolving outliers
boxplot(cyclists$ride_lenght)

min(cyclists$ride_lenght)
max(cyclists$ride_lenght)
mean(cyclists$ride_lenght)

##checking for reasons for negative ride_length
##found out that values in started_at and ended_at were misplaced
filter_negrl <- filter(cyclists, ride_lenght <= 0)
View(filter_negrl)
  
View(cyclists)

head(cyclists)
str(cyclists)
min(cyclists$ride_lenght)
max(cyclists$ride_lenght)
mean(cyclists$ride_lenght)

cyclists_filter <- 
  cyclists %>%
  select(rideable_type,
         member_casual,
         ride_lenght,
         day_of_week) %>%
  filter(ride_lenght >= 0)
View(cyclists_filter)

##using \ instead of / in file path will not run. It will bring error
##Error: '\U' used without hex digits in character string starting ""C:\U"
write.csv(cyclists_filter, file = "C:/Users/USER/cyclistic_datas/saved_files/cyclists_filter.csv")

bike <- read_csv("C:/Users/USER/cyclistic_datas/saved_files/cyclists_filter.csv")
head(bike)
colnames(bike)
cyclists_filter2 <- 
  bike %>% 
  select(- ...1)
colnames(cyclists_filter2)
write.csv(cyclists_filter2, file = "C:/Users/USER/cyclistic_datas/saved_files/cyclists_filter2.csv")


ggplot(data = bike, mapping = aes(x = day_of_week, y = ride_lenght)) +
  geom_point() +
  facet_wrap(~member_casual))





bike1 <- 
  cyclists_filter %>%
  group_by(member_casual) %>%
  summarise(total_ride_lenght = sum(ride_lenght))
View(bike1)



bike2 <- 
  cyclists_filter %>%
  aggregate(cyclists_filter$day_of_week, by = list(cyclists_filter$day_of_week), FUN = length)
View(bike2)

bike3 <- 
  filter_cyclist %>%
  group_by(day_of_week) %>%
  summarise(no_of_rides = count(day_of_week, n_mer))

#####OUT OF BOUNDS
#correct outliers
#testing codes for correcting misplaced values. 
## this codes did not work
colnames(cyclist_3)
cyclist_4 <- transform(cyclist_3,
                       cyclist_3$started_at == ifelse(cyclist_3$ride_lenght < 0,
                                                      cyclist_3$ended_at,
                                                      cyclist_3$started_at),
                       cyclist_3$ended_at == ifelse(cyclist_3$ride_lenght < 0,
                                                    cyclist_3$started_at,
                                                    cyclist_3$ended_at))
cyclist_3 <- ifelse(cyclist_3$started_at > cyclist_3$ended_at, 
                    swap(cyclist_3$started_at, cyclist_3$ended_at),
                    cyclist_3)
cyclist_5 <- 
  cyclist_3 %>%
  mutate(ride_lenghts = ended_at - started_at)
colnames(cyclist_5)
min(cyclist_5$ride_lenghts)
##end

##this code did not execute. It brought error
cyclists <- ifelse(cyclists$ride_lenght < 0, 
                   mutate(cyclists$ride_lenght = cyclists$ride_lenght * -1),
                   cyclists$ride_lenght)
##end

###trial = no response
cyclist_5 <- 
  cyclist_4 %>%
  mutate(ride_lenghts = ended_at - started_at)

min(cyclist_5$ride_lenghts)
### end

##This code did not work too
min(cyclist_4$ride_lenght)
head(cyclist_4)

cyclist_5 <- cyclist_3[cyclist_3 %in% cyclist_3$ride_lenght < 0, cyclist_3$started_at == cyclist_3$ended_at & cyclist_3$ended_at == cyclist_3$started_at]

cyclist_6 <- 
  cyclist_5 %>%
  mutate(ride_lenght2 = ended_at - started_at)

min(cyclist_5)
##end

#pend this code
cyclist_3$ride_lenght <- 
  if_else(cyclist_3$ride_lenght < 0,
         mutate(cyclist_3$started_at = cyclist_3$ended_at,
         cyclist_3$ended_at = cyclist_3$started_at),
         cyclist_3$ride_lenght)
##end

#descriptive analysis
