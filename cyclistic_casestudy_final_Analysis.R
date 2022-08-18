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
##STEP 4:Analyzing the data 

trip_clean_final <- read_csv("trip_clean_final.csv")

str(trip_clean_final)
names(trip_clean_final)

trip_clean_final$month <- ordered(trip_clean_final$month,
                                  levels=c("Dec_20", "Jan_21", "Feb_21", "Mar_21", 
                                           "Apr_21", "May_21", "Jun_21", "Jul_21", 
                                           "Aug_21", "Sep_21", "Oct_21", "Nov_21"))
#Sort the week days in order
trip_clean_final$week_day <- ordered(trip_clean_final$week_day, 
                                     levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                                              "Friday", "Saturday"))


#Analysis-1: min, max, median, and average ride lengths
view(describe(trip_clean_final$ride_length, fast=TRUE))

#Analysis-2:Total number of customers by membership details
view(table(trip_clean_final$customer_type))

#Analysis-3: Total rides for each customer type in minutes
view(setNames(aggregate(ride_length ~ customer_type, trip_clean_final, sum), 
              c("customer_type", " total_ride_length(mins)")))

#Analysis-4: Show differences between members and casual riders in terms of 
#length of ride (mean, median, maximum and minimum)
view(trip_clean_final %>% 
       group_by(customer_type) %>% 
       summarise(min_length_minutes = min(ride_length), max_length_minutes = max(ride_length), 
                 median_length_minutes = median(ride_length), mean_length_minutes = mean(ride_length)))

#Analysis-5: Average ride_length for users by day_of_week and 
#Number of total rides by day_of_week
view(trip_clean_final %>% 
       group_by(week_day) %>% 
       summarize(Avg_length = mean(ride_length),
                 number_of_rides = n())
)

#Analysis-6: Number of average rides by month

view(trip_clean_final %>% 
       group_by(month) %>% 
       summarize(Avg_length = mean(ride_length),
                 number_of_rides = n())
) 

#Analysis-7:Average ride length comparison by each week day according to each customer type
view(aggregate(trip_clean_final$ride_length ~ trip_clean_final$customer_type + 
                 trip_clean_final$week_day, FUN = mean))

#Analysis-8:Average ride length comparison by each month  according to each customer type
view(aggregate(trip_clean_final$ride_length ~ trip_clean_final$customer_type + 
                 trip_clean_final$month, FUN = mean))

#Analysis-9:Analyze rider length data by customer type and weekday
view(
  trip_clean_final %>% 
    group_by(customer_type, week_day) %>% 
    summarize(number_of_rides = n(),
              average_duration = mean(ride_length),
              median_duration = median(ride_length),
              max_duration = max(ride_length),
              min_duration = min(ride_length)) 
)

#Analysis-10:Analyze rider length data by customer type and month
view(
  trip_clean_final %>% 
    group_by(customer_type, month) %>% 
    summarize(number_of_rides = n(),
              average_duration = mean(ride_length),
              median_duration = median(ride_length),
              max_duration = max(ride_length),
              min_duration = min(ride_length)) 
)

write.csv(trip_clean_final,file = "trip_clean_final_tableau.csv",row.names = FALSE)

