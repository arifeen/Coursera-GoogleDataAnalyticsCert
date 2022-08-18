# STEP 1: SETTING UP ENVIRONMENT
# Setting working directory
setwd("C:/Users/Arifeen/Downloads/Cyclistic_tableau")

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

## STEP5: Data findings with visualizations

trip_clean_final <- read_csv("trip_clean_final.csv")
trip_clean_final$month <- ordered(trip_clean_final$month,
                                  levels=c("Dec_20", "Jan_21", "Feb_21", "Mar_21", 
                                           "Apr_21", "May_21", "Jun_21", "Jul_21", 
                                           "Aug_21", "Sep_21", "Oct_21", "Nov_21"))
#Sort the week days in order
trip_clean_final$week_day <- ordered(trip_clean_final$week_day, 
                                     levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                                              "Friday", "Saturday"))

#Visualization:1 Looking at Day of week usage by each customer type
#-----------------------------------------------------------------
#The number of  rides per day of week shows casual riders peak on the Saturday and Sunday 
#while members peak Monday through Friday. This indicates members mainly use 
#the bikes for their regular commutes while casual riders for leisure. reff: Analysis-9
trip_clean_final %>%    
  
  group_by(customer_type, week_day) %>% 
  
  summarise(number_of_rides = n() ) %>% 
  
  ggplot(aes(week_day, number_of_rides, fill = customer_type)) + 
  
  geom_col(position = "dodge") + 
  
  facet_wrap(~customer_type)+
  
  labs(x="Week Days", y = "Ride Count", title = "Visualization:1", subtitle = "Rides per Day of Week", 
       fill = 'Type_of_Membership', caption = "Author: Samsil Arifeen", tag = "GoogleCapstone") +
  
  scale_y_continuous(breaks = c(50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 550000), 
                     labels = c("50K", "100K", "150K", "200K", "250K", "300K", "350K", "400K", "550K"))+
  
  theme_ipsum()+
  
  theme(
    plot.title= element_text(size = 20, color = "orangered", face = "bold", family = "Tahoma"),
    
    plot.subtitle = element_text(size = 12, color = "green4", face = "bold", family = "Tahoma"),
    
    plot.caption= element_text(size = 12, color = "blue", face = "bold"),
    
    plot.tag = element_text(size = 12, color = "blue", face = "bold"),
    
    axis.title.x = element_text(size = 18, color = "orangered", face = "bold", angle = 0),
    
    axis.title.y = element_text(size = 18, color = "orangered", face = "bold", angle = 90),
    
    axis.text.x= element_text(family = "Tahoma", face = "bold", colour = "green4", size=12, angle = 90),
    
    axis.text.y = element_text(family = "Tahoma", face = "bold", colour = "green4", size = 12),
    
    legend.title=element_text(color="orangered",face="bold",size=18),
    
    legend.text=element_text(face = "bold", color = "green4", size = 16)
  )

ggsave("viz1.png", width = 16, height = 6, 
       units = "in", scale = 1, 
       dpi = 600, limitsize = TRUE)

#visualization:2  #Looking at usage over a 24 hour/day period by each customer type
#---------------------------------------------------------------------------
trip_clean_final %>%    
  
  group_by(customer_type, time) %>% 
  
  summarise(total_rides = n()) %>% 
  
  rename(Type_of_Membership = customer_type) %>% 
  
  ggplot(aes(time,total_rides, color = Type_of_Membership, group = Type_of_Membership)) +
  
  scale_x_datetime(date_breaks = "2 hour",
                   date_labels = "%H:%M", expand = c(0,0)) +
  
  geom_smooth(alpha = 0.5, size = 0.3)+
  
  facet_wrap(~Type_of_Membership)+
  
  labs(title = "Visualization:2", subtitle = "Usage Throughout the Day (24Hr)",
       x = "24Hr Time Period", y = "Total Rides", tag = "GoogleCapstone", 
       caption = "Author: Samsil Arifeen")+
  
  theme_ipsum()+
  
  theme(
    plot.title= element_text(size = 20, color = "orangered", face = "bold", family = "Tahoma"),
    
    plot.subtitle = element_text(size = 12, color = "green4", face = "bold", family = "Tahoma"),
    
    plot.caption= element_text(size = 12, color = "blue", face = "bold"),
    
    plot.tag = element_text(size = 12, color = "blue", face = "bold"),
    
    axis.title.x = element_text(size = 18, color = "orangered", face = "bold", angle = 0),
    
    axis.title.y = element_text(size = 18, color = "orangered", face = "bold", angle = 90),
    
    axis.text.x= element_text(family = "Tahoma", face = "bold", colour = "green4", size=12, angle = 45),
    
    axis.text.y = element_text(family = "Tahoma", face = "bold", colour = "green4", size = 12),
    
    legend.title=element_text(color="orangered",face="bold",size=18),
    
    legend.text=element_text(face = "bold", color = "green4", size = 16)
  )

ggsave("viz2.png", width = 16, height = 6, 
       units = "in", scale = 1, 
       dpi = 600, limitsize = TRUE)

#The demand for bike usage in a 24 hour span shows that usage by annual members peak 
#during rush hour which indicates many members use the bikes for commutes to and 
#from work especially with the steep drop after the peak at 5pm. Casual riders are not 
#as volatile as there is a steady increase throughout the day with a steady decrease 
#after the peak at 5pm.                                                 


#Visualization:3 Looking at usage per month in a year period by each customer type
#------------------------------------------------------------------------

trip_clean_final %>%   
  
  group_by(customer_type,month) %>%  
  
  summarise(total_rides = n()) %>% 
  
  ggplot(aes(month,total_rides, fill = customer_type)) + 
  
  geom_col(position = "dodge") + 
  
  facet_wrap(~ customer_type)+
  
  labs(x = "Months", y = "Total Number of Rides", title = "Visualization:3", 
       subtitle = "Usage per Month in a year", tag = "GoogleCapstone", 
       caption = "Author: Samsil Arifeen", fill = "Type_of_Membership") + 
  
  scale_y_continuous(breaks = c(50000, 100000, 150000, 200000, 250000, 300000, 350000), 
                     labels = c("50K", "100K", "150K", "200K", "250K", "300K", "350K"))+ 
  
  theme_ipsum()+
  
  theme(
    plot.title= element_text(size = 20, color = "orangered", face = "bold", family = "Tahoma"),
    
    plot.subtitle = element_text(size = 12, color = "green4", face = "bold", family = "Tahoma"),
    
    plot.caption= element_text(size = 12, color = "blue", face = "bold"),
    
    plot.tag = element_text(size = 12, color = "blue", face = "bold"),
    
    axis.title.x = element_text(size = 18, color = "orangered", face = "bold", angle = 0),
    
    axis.title.y = element_text(size = 18, color = "orangered", face = "bold", angle = 90),
    
    axis.text.x= element_text(family = "Tahoma", face = "bold", colour = "green4", size=12, angle = 90),
    
    axis.text.y = element_text(family = "Tahoma", face = "bold", colour = "green4", size = 12),
    
    legend.title=element_text(color="orangered",face="bold",size=18),
    
    legend.text=element_text(face = "bold", color = "green4", size = 16)
  )

ggsave("viz3.png", width = 16, height = 6, 
       units = "in", scale = 1, 
       dpi = 600, limitsize = TRUE)

#The summer months bring more riders in total but casual rider usage is nearly 
#nonexistent in the winter months. There are multiple factors which contribute to 
#this result but annual members still use the service at a good rate in those months. 
#reff: Analysis-10

#Visualization:4 average rides by each customer type every day per week 
#-----------------------------------------------------------------------

trip_clean_final %>%   
  
  group_by(customer_type,week_day) %>%  
  
  summarise(average_duration = mean(ride_length)) %>% 
  
  ggplot(aes(week_day, average_duration, fill = customer_type)) + 
  
  geom_col(position = "dodge") + 
  
  facet_wrap(~customer_type)+
  
  labs(x= "Week days", y= "Average Number of Rides", title = "Vizualisation:4", 
       subtitle = "Average Rides per week days", tag = "GoogleCapstone", caption = "Author: Samsil Arifeen",
       fill = "Type_of_Membership") + 
  
  theme_ipsum()+
  
  theme(
    plot.title= element_text(size = 20, color = "orangered", face = "bold", family = "Tahoma"),
    
    plot.subtitle = element_text(size = 12, color = "green4", face = "bold", family = "Tahoma"),
    
    plot.caption= element_text(size = 12, color = "blue", face = "bold"),
    
    plot.tag = element_text(size = 12, color = "blue", face = "bold"),
    
    axis.title.x = element_text(size = 18, color = "orangered", face = "bold", angle = 0),
    
    axis.title.y = element_text(size = 18, color = "orangered", face = "bold", angle = 90),
    
    axis.text.x= element_text(family = "Tahoma", face = "bold", colour = "green4", size=12, angle = 90),
    
    axis.text.y = element_text(family = "Tahoma", face = "bold", colour = "green4", size = 12),
    
    legend.title=element_text(color="orangered",face="bold",size=18),
    
    legend.text=element_text(face = "bold", color = "green4", size = 16)
  )

ggsave("viz4.png", width = 16, height = 6, 
       units = "in", scale = 1, 
       dpi = 600, limitsize = TRUE)

# The average ride length of casual riders is considerably longer than those of members 
#and peak on Saturday and Sunday. Annual members bike a near constant length regardless 
#of day of week. in short, member riders have a continuos everyday usage.
#reff: Analysis-7/9


#Visualization:5 average rides by each customer type every month in a year
#-----------------------------------------------------------------------
trip_clean_final %>%   
  
  group_by(customer_type, month)%>%  
  
  summarise(average_duration = mean(ride_length)) %>% 
  
  ggplot(aes(month,average_duration, fill = customer_type)) + 
  
  geom_col(position = "dodge") + 
  
  facet_wrap(~customer_type)+
  
  labs(x= "Months", y= "Average Number of Rides", title = "Visualization:5", 
       subtitle = "Average_Rides per month in a year", caption = "Author: Samsil Arifeen",
       tag = "GoogleCapstone", fill = "Type_of_Membership") + 
  
  theme_ipsum()+
  
  theme(
    plot.title= element_text(size = 20, color = "orangered", face = "bold", family = "Tahoma"),
    
    plot.subtitle = element_text(size = 12, color = "green4", face = "bold", family = "Tahoma"),
    
    plot.caption= element_text(size = 12, color = "blue", face = "bold"),
    
    plot.tag = element_text(size = 12, color = "blue", face = "bold"),
    
    axis.title.x = element_text(size = 18, color = "orangered", face = "bold", angle = 0),
    
    axis.title.y = element_text(size = 18, color = "orangered", face = "bold", angle = 90),
    
    axis.text.x= element_text(family = "Tahoma", face = "bold", colour = "green4", size=12, angle = 90),
    
    axis.text.y = element_text(family = "Tahoma", face = "bold", colour = "green4", size = 12),
    
    legend.title=element_text(color="orangered",face="bold",size=18),
    
    legend.text=element_text(face = "bold", color = "green4", size = 16)
  )

ggsave("viz5.png", width = 16, height = 6, 
       units = "in", scale = 1, 
       dpi = 600, limitsize = TRUE)

# The average ride length by casual riders is still considerably longer than members 
#even broken by month. #reff: Analysis-7/9


#visualization:6 looking at breakdown of bike types rented by customer types
#---------------------------------------------------------------------------
trip_clean_final %>%    
  
  ggplot(aes(bike_type, fill = customer_type)) + 
  
  geom_bar(position = "dodge") + 
  
  facet_wrap(~customer_type, ncol=3)+
  
  labs(x= 'Bike Type', y='Number of Rides', title='Visualization:6', 
       subtitle = "Bike Type Breakdown", tag = "GoogleCapstone", caption = "Author: Samsil Arifeen",
       fill = 'Type_of_Membership') + 
  
  scale_y_continuous(breaks = c(500000, 1000000, 1500000), 
                     labels = c("500K", "1Mil", "1.5Mil"))+
  
  theme_linedraw()+
  
  theme(
    plot.title= element_text(size = 20, color = "orangered", face = "bold", family = "Tahoma"),
    
    plot.subtitle = element_text(size = 12, color = "green4", face = "bold", family = "Tahoma"),
    
    plot.caption= element_text(size = 12, color = "blue", face = "bold"),
    
    plot.tag = element_text(size = 12, color = "blue", face = "bold"),
    
    axis.title.x = element_text(size = 18, color = "orangered", face = "bold", angle = 0),
    
    axis.title.y = element_text(size = 18, color = "orangered", face = "bold", angle = 90),
    
    axis.text.x= element_text(family = "Tahoma", face = "bold", colour = "green4", size=12, angle = 90),
    
    axis.text.y = element_text(family = "Tahoma", face = "bold", colour = "green4", size = 12),
    
    legend.title=element_text(color="orangered",face="bold",size=18),
    
    legend.text=element_text(face = "bold", color = "green4", size = 16)
  )

ggsave("viz6.png", width = 16, height = 6, 
       units = "in", scale = 1, 
       dpi = 600, limitsize = TRUE)

#The bike type breakdown shows members use classic bikes much more than casual members.
#The electric bike use is nearly identical but casual riders are more willing to use 
#docked bikes.

#visualization:7
# The total amount of time ridden by casual riders is greater than by member riders
#---------------------------------------------------------------------------

trip_clean_final %>%    
  
  group_by(customer_type) %>% 
  
  summarise(ride_total = sum(ride_length)) %>% 
  
  ggplot(aes(customer_type, ride_total, fill = customer_type))+
  
  geom_col(alpha = 0.5, width = 0.5,position = "dodge")+
  
  labs(title = "Visualization:7", subtitle = "Total Duration(million minutes) by customer type",
       x = "Customer type", y = "Total duration", tag = "GoogleCapstone", 
       caption = "Author: Samsil Arifeen", fill = "Type_of_Membership")+
  
  theme_ipsum()+
  
  scale_y_continuous(breaks = c(10000000, 20000000, 30000000, 40000000, 50000000, 60000000),
                     labels = c("10Mil", "20Mil", "30Mil", "40Mil", "50Mil", "60Mil"))+
  coord_flip()+
  
  theme(
    plot.title= element_text(size = 20, color = "orangered", face = "bold", family = "Tahoma"),
    
    plot.subtitle = element_text(size = 12, color = "green4", face = "bold", family = "Tahoma"),
    
    plot.caption= element_text(size = 12, color = "blue", face = "bold"),
    
    plot.tag = element_text(size = 12, color = "blue", face = "bold"),
    
    axis.title.x = element_text(size = 18, color = "orangered", face = "bold", angle = 0),
    
    axis.title.y = element_text(size = 18, color = "orangered", face = "bold", angle = 90),
    
    axis.text.x= element_text(family = "Tahoma", face = "bold", colour = "green4", size=12),
    
    axis.text.y = element_text(family = "Tahoma", face = "bold", colour = "green4", size = 12),
    
    legend.title=element_text(color="orangered",face="bold",size=18),
    
    legend.text=element_text(face = "bold", color = "green4", size = 16)
  )
ggsave("viz7.png", width = 16, height = 4, 
       units = "in", scale = 1, 
       dpi = 600, limitsize = TRUE)

#visualization:8
#bar graph that shows weekly frequency distribution of the member and casual customers 
#with bike types. For this I organized days in order from Monday to Sunday. 
#Then applied geom_bar and fill with rideable type.
#---------------------------------------------------------------------------
trip_clean_final %>% 
  
  ggplot()+
  
  geom_bar(aes(week_day, fill = bike_type))+
  
  facet_wrap(~customer_type)+
  
  labs(title = 'Visualization:8', subtitle = "Bike Type Breakdown:1 (Weekly frequency distribution)",
       x = "Week Days", y = "Bike usage Frequency", tag = "GoogleCapstone", 
       caption = "Author: Samsil Arifeen", fill = "Type of Bikes")+
  
  scale_y_continuous(breaks = c(50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 550000), 
                     labels = c("50K", "100K", "150K", "200K", "250K", "300K", "350K", "400K", "550K"))+
  
  theme_ipsum()+
  
  theme(
    plot.title= element_text(size = 20, color = "orangered", face = "bold", family = "Tahoma"),
    
    plot.subtitle = element_text(size = 12, color = "green4", face = "bold", family = "Tahoma"),
    
    plot.caption= element_text(size = 12, color = "blue", face = "bold"),
    
    plot.tag = element_text(size = 12, color = "blue", face = "bold"),
    
    axis.title.x = element_text(size = 18, color = "orangered", face = "bold", angle = 0),
    
    axis.title.y = element_text(size = 18, color = "orangered", face = "bold", angle = 90),
    
    axis.text.x= element_text(family = "Tahoma", face = "bold", colour = "green4", size=12, angle = 90),
    
    axis.text.y = element_text(family = "Tahoma", face = "bold", colour = "green4", size = 12),
    
    legend.title=element_text(color="orangered",face="bold",size=18),
    
    legend.text=element_text(face = "bold", color = "green4", size = 14)
  )

ggsave("viz8.png", width = 16, height = 8, 
       units = "in", scale = 1, 
       dpi = 600, limitsize = TRUE)


#Visualization:9
#bar graph that shows Monthly frequency distribution of the member and casual customers 
#with bike types in a year period.

trip_clean_final %>% 
  ggplot()+
  
  geom_bar(aes(month, fill = bike_type))+
  
  facet_wrap(~customer_type)+
  
  labs(title ="Visualization:9", 
       subtitle = "Bike type breakdown:2 (Monthly frequency distribution)",
       x = "12 Months", y = "Usage Frequency", tag = "GoogleCapstone", 
       caption = "Author: Samsil Arifeen", fill = "Bike Type")+
  
  scale_y_continuous(breaks = c(50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 550000), 
                     labels = c("50K", "100K", "150K", "200K", "250K", "300K", "350K", "400K", "550K"))+
  theme_ipsum()+
  theme(
    plot.title= element_text(size = 20, color = "orangered", face = "bold", family = "Tahoma"),
    
    plot.subtitle = element_text(size = 12, color = "green4", face = "bold", family = "Tahoma"),
    
    plot.caption= element_text(size = 12, color = "blue", face = "bold"),
    
    plot.tag = element_text(size = 12, color = "blue", face = "bold"),
    
    axis.title.x = element_text(size = 18, color = "orangered", face = "bold", angle = 0),
    
    axis.title.y = element_text(size = 18, color = "orangered", face = "bold", angle = 90),
    
    axis.text.x= element_text(family = "Tahoma", face = "bold", hjust=1, colour = "green4", size=12, angle = 90),
    
    axis.text.y = element_text(family = "Tahoma", face = "bold", colour = "green4", size = 12),
    
    legend.title=element_text(color="orangered",face="bold",size=18),
    
    legend.text=element_text(face = "bold", color = "green4", size = 14)
  )

ggsave("viz9.png", width = 16, height = 8, 
       units = "in", scale = 1, 
       dpi = 600, limitsize = TRUE)

#Visualization:10

trip_clean_final %>% 
  
  filter(ride_length<100) %>% 
  
  ggplot(aes(ride_length))+
  
  geom_histogram(binwidth = 3, fill = "green4", color="#e9ecef", alpha=0.9)+
  
  facet_wrap(~customer_type)+
  
  labs(title ="Visualization:10", 
       subtitle = "Ride duration behavior for customer type ",
       x = "Duration(min)", y = "Ride Count", tag = "GoogleCapstone", caption = "Author: Samsil Arifeen")+
  
  scale_y_continuous(breaks = c(50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 500000, 550000), 
                     labels = c("50K", "100K", "150K", "200K", "250K", "300K", "350K", "400K", "500K", "550K"))+
  
  theme_ipsum()+
  
  theme(
    plot.title= element_text(size = 20, color = "orangered", face = "bold", family = "Tahoma"),
    
    plot.subtitle = element_text(size = 12, color = "green4", face = "bold", family = "Tahoma"),
    
    plot.caption= element_text(size = 12, color = "blue", face = "bold"),
    
    plot.tag = element_text(size = 12, color = "blue", face = "bold"),
    
    axis.title.x = element_text(size = 18, color = "orangered", face = "bold", angle = 0),
    
    axis.title.y = element_text(size = 18, color = "orangered", face = "bold", angle = 90),
    
    axis.text.x= element_text(family = "Tahoma", face = "bold", hjust=1, colour = "green4", size=12),
    
    axis.text.y = element_text(family = "Tahoma", face = "bold", colour = "green4", size = 12),
  )

ggsave("viz10.png", width = 16, height = 8, 
       units = "in", scale = 1, 
       dpi = 600, limitsize = TRUE)

#Visualization:11

trip_clean_final %>% 
  
  ggplot(aes(time))+
  geom_histogram(binwidth = 10, color="green4", alpha=0.5, position = "dodge")+
  
  facet_wrap(~customer_type)+
  
  labs(title ="Visualization:11", subtitle = "Start time behavior for customer type ",
       x = "Hour Started", y = "Ride Count", tag = "GoogleCapstone", 
       caption = "Author: Samsil Arifeen")+
  scale_x_datetime(date_breaks = "1 hour",
                   date_labels = "%H:%M", expand = c(0,0))+
  
  scale_y_continuous(breaks = c(0, 2000, 4000, 6000), 
                     labels = c("0", "2K", "4K", "6K"))+
  
  theme_ipsum()+
  theme(
    plot.title= element_text(size = 20, color = "orangered", face = "bold", family = "Tahoma"),
    
    plot.subtitle = element_text(size = 12, color = "green4", face = "bold", family = "Tahoma"),
    
    plot.caption= element_text(size = 12, color = "blue", face = "bold"),
    
    plot.tag = element_text(size = 12, color = "blue", face = "bold"),
    
    axis.title.x = element_text(size = 18, color = "orangered", face = "bold", angle = 0),
    
    axis.title.y = element_text(size = 18, color = "orangered", face = "bold", angle = 90),
    
    axis.text.x= element_text(family = "Tahoma", face = "bold", hjust=1, colour = "green4", size=12, angle = 90),
    
    axis.text.y = element_text(family = "Tahoma", face = "bold", colour = "green4", size = 12),
  )

ggsave("viz11.png", width = 16, height = 8, 
       units = "in", scale = 1, 
       dpi = 600, limitsize = TRUE)


