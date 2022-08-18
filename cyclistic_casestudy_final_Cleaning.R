# STEP 1: SETTING UP ENVIRONMENT
# Setting working directory
setwd("/home/samsil/Documents/CaseStudy1-Cyclistic/")

#install packages
install.packages('tidyverse')
install.packages("janitor") 
install.packages("lubridate")
install.packages("devtools")
install.packages("psych")
install.packages("hunspell")
install.packages('https://cran.r-project.org/src/contrib/Archive/hrbrthemes/hrbrthemes_0.1.0.tar.gz', type='source', repos=NULL)

#load packages

library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(readr)
library(psych)
library(hrbrthemes)
library(ggplot2)

#-----------------------------------------------------------------------

# STEP 1: COLLECT DATA
#upload data files. we will run it only once to combine out dataframes into one
december_2020 <- read.csv("202012-divvy-tripdata.csv")
january_2021 <- read.csv("202101-divvy-tripdata.csv")
february_2021 <- read.csv("202102-divvy-tripdata.csv")
march_2021 <- read.csv("202103-divvy-tripdata.csv")
april_2021 <- read.csv("202104-divvy-tripdata.csv")
may_2021 <- read.csv("202105-divvy-tripdata.csv") 
june_2021 <- read.csv("202106-divvy-tripdata.csv")
july_2021 <- read.csv("202107-divvy-tripdata.csv")
august_2021 <- read.csv("202108-divvy-tripdata.csv")
september_2021 <- read.csv("202109-divvy-tripdata.csv")
october_2021 <- read.csv("202110-divvy-tripdata.csv")
november_2021 <- read.csv("202111-divvy-tripdata.csv")
#-------------------------------------------------------------------
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#Check column names to ensure we can join all the data
# Compare column names for each of the files
# While the names don't have to be in the same order,they DO need to
#match perfectly before we can use a command to join them into one file

colnames(december_2020)
colnames(january_2021)
colnames(february_2021)
colnames(march_2021)
colnames(april_2021)
colnames(may_2021)
colnames(june_2021)
colnames(july_2021)
colnames(august_2021)
colnames(september_2021)
colnames(october_2021)
colnames(november_2021)

#Calculate the total number of records in all twelve monthly files
sum(nrow(december_2020) + nrow(january_2021) + nrow(february_2021) 
    + nrow(march_2021) + nrow(april_2021) + nrow(may_2021) 
    + nrow(june_2021) + nrow(july_2021) + nrow(august_2021)
    + nrow(september_2021) + nrow(october_2021) + nrow(november_2021))

#---------------------------------------------------------------------------
#STEP 3: Process DATA
#Aggregate and observe Data
#Aggregate monthly data frames into one data frame 
trip_final <- rbind(december_2020,january_2021,february_2021,march_2021,april_2021,
                    may_2021,june_2021,july_2021,august_2021,september_2021,october_2021,november_2021)

#write the newly combined data file into a new file and save it to disk
write.csv(trip_final,file = "trip_final.csv",row.names = FALSE)

#because of RAM issues i joined the data files into one large file and importing the 
#data file after restarting the R session again using readr function to save RAM space. 
#I repeated the cycle later in the project too.
#Needed to restart R session and upload the csv file. 
trip_final <- read_csv("trip_final.csv")
options(future.globals.maxSize = Inf)

str(trip_final)
view(head(trip_final))
view(tail(trip_final))
dim(trip_final)
summary(trip_final)
names(trip_final)

#STEP 4: Cleaning the data
# Remove rows with missing values
colSums(is.na(trip_final))

# 5% of data with missing values will be removed and save it to a new dataframe
trip_clean_final <- trip_final[complete.cases(trip_final), ]

#remove any duplicates from the new dataframe
trip_clean_final <- distinct(trip_clean_final)

#data with started_at greater than ended_at will be removed
trip_clean_final <- trip_clean_final %>% 
  filter(trip_clean_final$started_at < trip_clean_final$ended_at)

#change a few column names for clarification
trip_clean_final <- rename(trip_clean_final, customer_type = member_casual,bike_type = rideable_type)
trip_clean_final <- rename(trip_clean_final, start_time = started_at)

#remove empty and missing data
drop_na(trip_clean_final)

remove_empty(trip_clean_final)
remove_missing(trip_clean_final)

#Add relevant columns
#From the started_at column, create additional columns for Date, Month, Day, Year, 
#Day of the Week. This allows for more granular analysis of the data by date/day/month.
trip_clean_final$date <- as.Date(trip_clean_final$start_time)
trip_clean_final$week_day <- format(as.Date(trip_clean_final$date), "%A")
trip_clean_final$month <- format(as.Date(trip_clean_final$date), "%b_%y")
trip_clean_final$year<-format(trip_clean_final$date,"%Y")

#add a time started column
trip_clean_final$time <- format(trip_clean_final$start_time, format = "%H:%M")  
#change format for the time column for later use
trip_clean_final$time <- as.POSIXct(trip_clean_final$time, format = "%H:%M") 

#Create a column for duration of rides calculated from start and end time of rides.
trip_clean_final$ride_length <- difftime(trip_clean_final$ended_at,trip_clean_final$start_time, units = "mins")

## Filter out data we will not be using for the analysis
trip_clean_final <- trip_clean_final %>% 
  select(bike_type, customer_type, month, year, time, start_time, week_day, ride_length)

#get rid of too long rides -rides should be limited to 1 day or 1440 minutes(24Hr)
#(cyclistic considers these bikes stolen)).
trip_clean_final <- trip_clean_final[!trip_clean_final$ride_length>1440,] 
#get rid of negative rides
trip_clean_final <- trip_clean_final[!trip_clean_final$ride_length<5,] 

#write the csv file again in the disk and restart R session
write.csv(trip_clean_final,file = "trip_clean_final.csv",row.names = FALSE)

#Look at specifics of the data
view(head(trip_clean_final))
view(tail(trip_clean_final))
summary(trip_clean_final)
str(trip_clean_final)
colnames(trip_clean_final)
dim(trip_clean_final)
#----------------------------------------------------------------------------

