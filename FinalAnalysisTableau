#--------------------------------------------CREATING CSV FILE FOR TABLEAU------------------------------------------

#load libraries 
library(tidyverse) #calculations
library(lubridate) #dates 
library(hms) #time
library(data.table) #exporting data frame

#load original .csv files, a years worth of data from August 2020 to July 2021
aug08_df <- read_csv("202008-divvy-tripdata.csv") 
sep09_df <- read_csv("202009-divvy-tripdata.csv") 
oct10_df <- read_csv("202010-divvy-tripdata.csv")
nov11_df <- read_csv("202011-divvy-tripdata.csv") 
dec12_df <- read_csv("202012-divvy-tripdata.csv")
jan01_df <- read_csv("202101-divvy-tripdata.csv") 
feb02_df <- read_csv("202102-divvy-tripdata.csv") 
mar03_df <- read_csv("202103-divvy-tripdata.csv")
apr04_df <- read_csv("202104-divvy-tripdata.csv")
may05_df <- read_csv("202105-divvy-tripdata.csv") 
jun06_df <- read_csv("202106-divvy-tripdata.csv") 
jul07_df <- read_csv("202107-divvy-tripdata.csv") 

#merge all of the data frames into one year view
cyclistic_df <- rbind (aug08_df, sep09_df, oct10_df, nov11_df, dec12_df, jan01_df, feb02_df, mar03_df, apr04_df, may05_df, jun06_df, jul07_df)

#remove individual month data frames to clear up space in the environment 
remove(aug08_df, sep09_df, oct10_df, nov11_df, dec12_df, jan01_df, feb02_df, mar03_df, apr04_df, may05_df, jun06_df, jul07_df)

#create new data frame to contain new columns
cyclistic_date <- cyclistic_df

#calculate ride length by subtracting ended_at time from started_at time and converted it to minutes
cyclistic_date$ride_length <- difftime(cyclistic_df$ended_at, cyclistic_df$started_at, units = "mins")
cyclistic_date$ride_length <- round(cyclistic_date$ride_length, digits = 1)

#create columnds for: day of week, month, day, year, time, hour
cyclistic_date$date <- as.Date(cyclistic_date$started_at) #default format is yyyy-mm-dd, use start date
cyclistic_date$day_of_week <- wday(cyclistic_df$started_at) #calculate the day of the week 
cyclistic_date$day_of_week <- format(as.Date(cyclistic_date$date), "%A") #create column for day of week
cyclistic_date$month <- format(as.Date(cyclistic_date$date), "%m")#create column for month
cyclistic_date$day <- format(as.Date(cyclistic_date$date), "%d") #create column for day
cyclistic_date$year <- format(as.Date(cyclistic_date$date), "%Y") #create column for year
cyclistic_date$time <- format(as.Date(cyclistic_date$date), "%H:%M:%S") #format time as HH:MM:SS
cyclistic_date$time <- as_hms((cyclistic_df$started_at)) #create new column for time
cyclistic_date$hour <- hour(cyclistic_date$time) #create new column for hour

#create column for different seasons: Spring, Summer, Fall, Winter
cyclistic_date <-cyclistic_date %>% mutate(season = 
                                             case_when(month == "03" ~ "Spring",
                                                       month == "04" ~ "Spring",
                                                       month == "05" ~ "Spring",
                                                       month == "06"  ~ "Summer",
                                                       month == "07"  ~ "Summer",
                                                       month == "08"  ~ "Summer",
                                                       month == "09" ~ "Fall",
                                                       month == "10" ~ "Fall",
                                                       month == "11" ~ "Fall",
                                                       month == "12" ~ "Winter",
                                                       month == "01" ~ "Winter",
                                                       month == "02" ~ "Winter")
)

#create column for different time_of_day: Night, Morning, Afternoon, Evening
cyclistic_date <-cyclistic_date %>% mutate(time_of_day = 
                                             case_when(hour == "0" ~ "Night",
                                                       hour == "1" ~ "Night",
                                                       hour == "2" ~ "Night",
                                                       hour == "3" ~ "Night",
                                                       hour == "4" ~ "Night",
                                                       hour == "5" ~ "Night",
                                                       hour == "6" ~ "Morning",
                                                       hour == "7" ~ "Morning",
                                                       hour == "8" ~ "Morning",
                                                       hour == "9" ~ "Morning",
                                                       hour == "10" ~ "Morning",
                                                       hour == "11" ~ "Morning",
                                                       hour == "12" ~ "Afternoon",
                                                       hour == "13" ~ "Afternoon",
                                                       hour == "14" ~ "Afternoon",
                                                       hour == "15" ~ "Afternoon",
                                                       hour == "16" ~ "Afternoon",
                                                       hour == "17" ~ "Afternoon",
                                                       hour == "18" ~ "Evening",
                                                       hour == "19" ~ "Evening",
                                                       hour == "20" ~ "Evening",
                                                       hour == "21" ~ "Evening",
                                                       hour == "22" ~ "Evening",
                                                       hour == "23" ~ "Evening")
)


#create a column for the month using the full month name
cyclistic_date <-cyclistic_date %>% mutate(month = 
                                             case_when(month == "01" ~ "January",
                                                       month == "02" ~ "February",
                                                       month == "03" ~ "March",
                                                       month == "04" ~ "April",
                                                       month == "05" ~ "May",
                                                       month == "06" ~ "June",
                                                       month == "07" ~ "July",
                                                       month == "08" ~ "August",
                                                       month == "09" ~ "September",
                                                       month == "10" ~ "October",
                                                       month == "11" ~ "November",
                                                       month == "12" ~ "December"
                                                       )
)

#clean the data
cyclistic_date <- na.omit(cyclistic_date) #remove rows with NA values
cyclistic_date <- distinct(cyclistic_date) #remove duplicate rows 
cyclistic_date <- cyclistic_date[!(cyclistic_date$ride_length <=0),] #remove where ride_length is 0 or negative
cyclistic_date <- cyclistic_date %>%  #remove columns not needed: ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng
  select(-c(ride_id, start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng)) 

#view the final data
View(cyclistic_date)

#created a new dataframe to use in Tableau
cyclistic_tableau <- cyclistic_date

#clean the data
cyclistic_tableau <- cyclistic_tableau %>%  #remove columns not needed: start_station_name, end_station_name, time, started_at, ended_at
  select(-c(start_station_name, end_station_name, time, started_at, ended_at))

#download the new data as a .csv file
fwrite(cyclistic_tableau,"cyclistic_data.csv")
