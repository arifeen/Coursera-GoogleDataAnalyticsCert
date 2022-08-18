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
install.packages("ggthemes")

#load packages

library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(readr)
library(psych)
library(hrbrthemes)
library(ggplot2)
library(ggthemes)

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
  
  geom_col(position = "dodge", width = 0.5) + 
  
  #facet_wrap(~customer_type, scales = "free")+
  
  labs(x="Week Days", y = "Ride Count", 
       title = "Visualization:1", subtitle = "Rides per Day of Week", 
       fill = 'Type_of_Membership', tag = "GoogleCapstone:Samsil Arifeen",
       caption = "Ride count for each Week Day broken down by Customer Type. The color shows details about Customer Type.") +
  
  scale_y_continuous(breaks = c(50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 550000), 
                     labels = c("50K", "100K", "150K", "200K", "250K", "300K", "350K", "400K", "550K"))+
  
  theme_ipsum()+
  
  theme(
    plot.title= element_text(size = 20, color = "orangered", face = "bold", family = "Tahoma"),
    
    plot.subtitle = element_text(size = 12, color = "green4", face = "bold", family = "Tahoma"),
    
    plot.caption= element_text(size = 12, color = "blue", face = "bold", hjust = 0.5),
    
    plot.tag = element_text(size = 12, color = "blue", face = "bold"),
    
    axis.title.x = element_text(size = 18, color = "orangered", face = "bold", angle = 0, hjust = 0.5),
    
    axis.title.y = element_text(size = 18, color = "orangered", face = "bold", angle = 90,hjust = 0.5),
    
    axis.text.x= element_text(family = "Tahoma", face = "bold", colour = "green4", size=12, angle = 90),
    
    axis.text.y = element_text(family = "Tahoma", face = "bold", colour = "green4", size = 12),
    
    legend.title=element_text(color="orangered",face="bold",size=18),
    
    legend.text=element_text(face = "bold", color = "green4", size = 16),
    
    panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "black"), 
    
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "black")
  )

ggsave("viz1.png", width = 16, height = 8, 
       units = "in", scale = 1, 
       dpi = 600, limitsize = TRUE)


#visualization:2  #Looking at usage over a 24 hour/day period by each customer type
#---------------------------------------------------------------------------
trip_clean_final %>%    
  
  group_by(customer_type, time) %>% 
  
  summarise(total_rides = n()) %>% 
  
  rename(Type_of_Membership = customer_type) %>% 
  
  ggplot(aes(time,total_rides, color = Type_of_Membership, group = Type_of_Membership)) +
  
  scale_x_datetime(date_breaks = "1 hour",
                   date_labels = "%H:%M", expand = c(0,0)) +
  
  geom_smooth(linetype = 'solid', alpha = 0.5, size = 1)+
  
  labs(title = "Visualization:2", subtitle = "Usage Throughout the Day (24Hr)",
       x = "24Hr Time Period", y = "Total Rides", tag = "GoogleCapstone", 
       caption = "The trend of total rides for 24Hr-Time Period. Color shows details about Customer Type.")+
  
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
    
    legend.text=element_text(face = "bold", color = "green4", size = 16),
    
    panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "black"), 
    
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "black")
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
  
  geom_col(position = "dodge", width = 0.5) + 
  
  facet_wrap(~ customer_type)+
  
  labs(x = "Months", y = "Total Number of Rides", title = "Visualization:3", 
       subtitle = "Usage per Month in a year", tag = "GoogleCapstone", 
       caption = "Total rides for each Month broken down by Customer Type.
       Color shows details about Customer Type. The view is filtered on Month, which keeps one year.", fill = "Type_of_Membership") + 
  
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
    
    legend.text=element_text(face = "bold", color = "green4", size = 16),
    
    panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "black"), 
    
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "black")
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
  
  geom_col(position = "dodge", width = 0.5) + 
  
  facet_wrap(~customer_type)+
  
  labs(x= "Week days", y= "Average Number of Rides", title = "Vizualisation:4", 
       subtitle = "Average Rides per week days", tag = "GoogleCapstone", 
       caption = "Average of Ride Length for each Week Day broken down by Customer Type.  
       Color shows details about Customer Type.",
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
    
    legend.text=element_text(face = "bold", color = "green4", size = 16),
    
    panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "black"), 
    
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "black")
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
  
  geom_col(position = "dodge",  width = 0.5) + 
  
  facet_wrap(~customer_type)+
  
  labs(x= "Months", y= "Average Number of Rides", title = "Visualization:5", 
       subtitle = "Average_Rides per month in a year", 
       caption = "Average of Ride Length for each Month broken down by Customer Type.  
       Color shows details about Customer Type.",
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
    
    legend.text=element_text(face = "bold", color = "green4", size = 16),
    
    panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "black"), 
    
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "black")
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
  
  geom_bar(position = "dodge", width = 0.75) + 
  
  facet_wrap(~customer_type, ncol=3)+
  
  labs(x= 'Bike Type', y='Number of Rides', title='Visualization:6', 
       subtitle = "Bike Type Breakdown", tag = "GoogleCapstone", 
       caption = "Ride count for each Bike Type broken down by Customer Type.Color shows details about Bike Type.  
       The Customer Type filter keeps casual and member.",
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
    
    legend.text=element_text(face = "bold", color = "green4", size = 16),
    
    panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "black"), 
    
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "black")
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
       caption = "Sum of Ride Length for each Customer Type.  
       Color shows details about Customer Type.", fill = "Type_of_Membership")+
  
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
  
  labs(title = 'Visualization:8', subtitle = "Bike Type Breakdown:(Weekly frequency distribution)",
       x = "Week Days", y = "Bike usage Frequency", tag = "GoogleCapstone", 
       caption = "Count of Bike Type for each Customer Type broken down by Week Day.  
        Color shows details about Bike Type.", fill = "Type of Bikes")+
  
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
    
    legend.text=element_text(face = "bold", color = "green4", size = 14),
    
    panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "black"), 
    
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "black")
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
       subtitle = "Bike type breakdown:(Monthly frequency distribution)",
       x = "12 Months", y = "Usage Frequency", tag = "GoogleCapstone", 
       caption = "Bike usage frequency for each Customer Type broken down by month.  
        Color shows details about Bike Type.", fill = "Bike Type")+
  
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
    
    legend.text=element_text(face = "bold", color = "green4", size = 14),
    
    panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "black"), 
    
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "black")
  )

ggsave("viz9.png", width = 16, height = 8, 
       units = "in", scale = 1, 
       dpi = 600, limitsize = TRUE)

#Visualization:10

trip_clean_final %>% 
  
  filter(ride_length<100) %>% 
  
  ggplot(aes(ride_length))+
  
  geom_histogram(binwidth = 3, fill = "green4", color="#e9ecef", alpha=0.9)+
  
  facet_wrap(~customer_type, scales = 'free')+
  
  labs(title ="Visualization:10", 
       subtitle = "Ride duration Density for customer type ",
       x = "Ride Duration(min)", y = "Ride Count", tag = "GoogleCapstone", 
       caption = "The trend of Ride Count by customer type.  
        Color shows details about Customer Type.  
       The marks are labeled by % of Total Count of Ride Count.")+
  
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
    
    panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "black"), 
    
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "black")
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
       caption = "The trend of count of Ride Length for Time Hour.  
         Color shows details about Customer Type.")+
  scale_x_datetime(date_breaks = "2 hour",
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
    
    panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "black"), 
    
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "black")
  )

ggsave("viz11.png", width = 16, height = 8, 
       units = "in", scale = 1, 
       dpi = 600, limitsize = TRUE)






