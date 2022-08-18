---
title: "Google Data Analytics Capstone Project - Cyclistic"
author: "Samsil Arifeen"
date: "2022-03-22"
output:
  html_document: 
    toc: yes
    theme: cosmo
    toc_depth: 5
    highlight: espresso
    fig_height: 4
    fig_caption: yes
    number_sections: yes
    keep_md: yes
    df_print: tibble
  pdf_document: default
---

<style>
body {text-align: justify}
</style>


![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/google-data-analytics-professional-certificate.1.png){width=250px height=250px}

# **Introduction**

**This is a case study that i conducted to solve for Google data analytics certifications Course Capstone Project: Case Study 1 "Cyclistic". This is my first ever case study on data analytics as well. It involves a fictional bike share company based in Chicago, USA. However the company wants to know the different users' behavior while using the services available. The management believes that doing this, they will be able to start a new marketing strategy that will be key for future growth of the company. As I have learned from the Google Data Analytics program, I will follow the steps of the data analysis process: ask, prepare, process, analyze, share and act. However, since act step is for executives to decide, I will not cover that step in this solution.** 

# **Scenario**

**Cyclistic operates a fleet of more than 5,800 bicycles which can be accessed from over 600 docking stations across the city. Bikes can be borrowed from one docking station, ridden, then returned to any docking stations. Over the years marketing campaigns have been broad and targeted a cross-section of potential users. Data analysis has shown that riders with an annual membership are more profitable than casual riders. Lily Moreno, the director of marketing, wants to implement a new marketing strategy in order to convert casual riders into annual members. She believes that with the right campaign there is a very good chance of such conversions between the user types. There are also user-friendly bike options include such as electric bikes, classic bikes and docked bikes. It  makes Cyclistic services more inclusive to people. Lily has tasked the marketing analytics team to analyze past user data of one year to find trends and habits of Cyclistic’s users to help create this marketing campaign. The marketing analyst team would like to know:**

* **how annual members and casual riders differ**

* **why casual riders would buy a membership**

* **how Cyclistic can use digital media to influence casual riders to become members.** 

**Here i have to analyze the Cyclistic historical bike trip data to identify trends in the usage of bikes by casual and member riders.**

# **Phase 1: Ask**

#### **Business objective**

**To increase profitability by converting casual riders to annual members via a targeted marketing campaign.**

#### **Business task**

**I as an analyst have been tasked with answering this question:**

* How do **annual members** and **casual riders** use Cyclistic bikes differently? The behavioral differences between annual members and casual riders. 
* Why a **casual rider** would buy Cyclistic **annual memberships**.
* How digital media can influence **casual riders** to becoming **annual members**. 

**The marketing analytics team tasked me with using past user data to find the behavioral differences between annual members and casual riders and report their findings.**

#### **Stakeholders**
**The stakeholders in this project include:**

* **Lily Moreno, Director of Marketing at Cyclistic, who is responsible for implementing the marketing campaigns at Cyclistic.**

* **The Cyclistic marketing analytics team. This team is responsible for collecting, analyzing and reporting data to be used in marketing campaigns. I am the junior analyst in this team.**

* **The Cyclistic executive team. This team makes the final decision on the recommended marketing plan. They are notoriously detail-oriented.**

# **Phase 2: Prepare**

#### **Where is Data located**

**The data used for this analysis were obtained from the Motivate, a company employed by the City of Chicago to collect data on bike share usage. Here is the [link](https://divvy-tripdata.s3.amazonaws.com/index.html) for the datasets that i have used in this case study.**

#### **How is the Data Organized?**

**The data is organized in monthly csv files. The most recent twelve months of data (December, 2020 – November,2021) were used for this project. The files consist of 13 columns containing information related to ride id, ridership type, ride time, start location and end location and geographic coordinates, etc.**

#### **Credibility of the Data**

**The [data](https://divvy-tripdata.s3.amazonaws.com/index.html) is collected directly by Motivate, Inc., the company that runs the Cyclistic Bike Share program for the City of Chicago. The data is comprehensive and consistent as it consists of data for all the rides taken by users and it is not just a sample of the data. The data is current as it had been released monthly. The City of Chicago makes the data available to the public.**

#### **Licensing, privacy, security, and accessibility**

**This [data](https://divvy-tripdata.s3.amazonaws.com/index.html) has been stripped of all identifying information. This ensures privacy, but it limits the extent of the possible analysis. There is not enough data to determine if casual riders are repeat-riders or if casual riders are residents of the Chicago area. The data is released under this [license](https://ride.divvybikes.com/data-license-agreement).**

#### **Ability of Data to answer Business Questions**

In order to answer the business question, How do **annual members** and **casual riders** use Cyclistic bikes differently,The behavioral differences between annual members and casual riders, the available dataset is sufficient enough. After a detail observation about the variables, it is clearly depicted that casual riders pay for individual or daily rides whereas members riders purchase annual subscriptions. This information is very important in order to determine the behavioral differences between the two groups. 

#### **Challenges with the data**

* **There are some problems with the data as well. Most of the the problems (duplicate records, missing fields, etc.) can be dealt with by data cleaning.**

* **There was 12 csv files for 12 months/year. After combining all the files, it became 1.2 Gb in total size. It was alright until it became larger than my laptop's RAM. At first i tried to dealt it with diskframe function, but as it took too long for me to learn and apply i have decided to go by segments. In that case, i did the data cleaning and then removed all the variables that i am not going to work with at this moment. Then i wrote it down in a csv file in the hard disk and imported it again for analysis and preparing visualization.**   

# **Phase 3: Data Process**

#### **What tools are you choosing and why?**

**For this project I choose RStudio Desktop in order to prepare, process, clean, analyze and  create the visualizations. The data set was too large to be processed in Ms Excel, google spreadsheets and RStudio Cloud.**

#### **Review of Data**

**In order to get an overview, the data was reviewed in terms of understanding of the concent of variables, data formats and data integrity.** 

**Data review involved the following:**

* **Checking column names across all the 12 original files.**
* **Checking for missing values.**
* **Checking of white spaces.**
* **Checking of duplicate records.**
* **Other data anomalies.** 

**However the review of the data revealed several problems:**

* **Duplicate record of ID numbers.**
* **Records with missing start or end station name.**
* **Records with very short or very long ride duration.**
* **Records for trips starting or ending at an administrative station (repair or testing station).**

**All 12 files were combined into one data set after initial review was completed.The final data set consisted of 5479096 rows with 13 columns of character and numeric data. This matched the number of records in all 12 monthly data files.** 
#### **Setting up environment**


```r
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
```

#### **Collect Data(Data wrangling)**
 
 * **At this point, i had to upload data files into new vectors and we will run it only once to combine out data frames into one big dataset.**
 

```r
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
```
 
**Data validation**

**Check column names to ensure we can join all the data. Compare column names for each of the files. While the names don't have to be in the same order but they do need to match perfectly before we can use a command to join them into one file.->**


```r
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
#---------------------------------------------------------------------------
```

**Data validation**

**Calculate the total number of records in all twelve monthly files. It is 5479096 rows and 13 columns in total.->** 


```r
sum(nrow(december_2020) + nrow(january_2021) + nrow(february_2021) 
    + nrow(march_2021) + nrow(april_2021) + nrow(may_2021) 
    + nrow(june_2021) + nrow(july_2021) + nrow(august_2021)
    + nrow(september_2021) + nrow(october_2021) + nrow(november_2021))
```

**Aggregate and observe Data**

**Aggregate monthly data frames into one data frame.->** 


```r
trip_final <- rbind(december_2020,january_2021,february_2021,march_2021,april_2021,
                    may_2021,june_2021,july_2021,august_2021,september_2021,october_2021,november_2021)
```

**Write the newly combined data file into a new file and save it to hard disk.->**


```r
write.csv(trip_final,file = "trip_final.csv",row.names = FALSE)
```

**Needed to restart R session and upload the trip_final.csv file by read_csv function.->** 


```r
trip_final <- read_csv("trip_final.csv")
options(future.globals.maxSize = Inf)
```

**Data Validation->**


```r
str(trip_final)
view(head(trip_final))
view(tail(trip_final))
dim(trip_final)
summary(trip_final)
names(trip_final)
```

# **Phase 3: Data Cleaning**

**We need to start data cleaning now.** 

**Remove rows with NA values.->**


```r
colSums(is.na(trip_final))
```

**Remove 5% of missing values and save into a new data frame.->**


```r
trip_clean_final <- trip_final[complete.cases(trip_final), ]
```

**Remove any duplicates from the new data frame.->**


```r
trip_clean_final <- distinct(trip_clean_final)
```

**By filtering, data with started_at greater than ended_at will be removed.->**


```r
trip_clean_final <- trip_clean_final %>% 
  filter(trip_clean_final$started_at < trip_clean_final$ended_at)
```

**Change a few column names for better understanding.->**


```r
trip_clean_final <- rename(trip_clean_final, customer_type = member_casual,bike_type = rideable_type)
trip_clean_final <- rename(trip_clean_final, start_time = started_at)
```

**Remove empty, NA and missing data.->**


```r
drop_na(trip_clean_final)
remove_empty(trip_clean_final)
remove_missing(trip_clean_final)
```

**Create additional columns for Date, Month, Day, Year, day of the Week from the started_at column.This allows for more granular analysis of the data by date/day/month.->** 


```r
trip_clean_final$date <- as.Date(trip_clean_final$start_time)
trip_clean_final$week_day <- format(as.Date(trip_clean_final$date), "%A")
trip_clean_final$month <- format(as.Date(trip_clean_final$date), "%b_%y")
trip_clean_final$year<-format(trip_clean_final$date,"%Y")
```

**Add a new column named time.->**


```r
trip_clean_final$time <- format(trip_clean_final$start_time, format = "%H:%M")
```
  
**Change format for the time column for later use.->**


```r
trip_clean_final$time <- as.POSIXct(trip_clean_final$time, format = "%H:%M") 
```

**Create a column for duration of rides calculated from start and end time of rides.->**


```r
trip_clean_final$ride_length <- difftime(trip_clean_final$ended_at,trip_clean_final$start_time, units = "mins")
```

**Filter out data that we will not be using for this analysis.->**


```r
trip_clean_final <- trip_clean_final %>% 
  select(bike_type, customer_type, month, year, time, start_time, week_day, ride_length)
```

**Get rid of too long rides as rides should be limited to 1 day or 1440 minutes or 24Hr(cyclistic considers these bikes are stolen).->**


```r
trip_clean_final <- trip_clean_final[!trip_clean_final$ride_length>1440,] 
```

**Get rid of negative rides.->**


```r
trip_clean_final <- trip_clean_final[!trip_clean_final$ride_length<5,] 
```

**Write the csv file again in the disk and restart R session for analysis.->**


```r
write.csv(trip_clean_final,file = "trip_clean_final.csv",row.names = FALSE)
```

# **Phase 4: Data analysis**

**Data validation->**

* **Upload the trip_clean.csv file on-board and check data validation if everything is ok.**


```r
trip_clean_final <- read_csv("trip_clean_final.csv")

str(trip_clean_final)
names(trip_clean_final)
```

**Sorting Month and week days in order.->**

* **Sort the week days and month in order for future analysis process.**


```r
trip_clean_final$month <- ordered(trip_clean_final$month,
                                  levels=c("Dec_20", "Jan_21", "Feb_21", "Mar_21", 
                                           "Apr_21", "May_21", "Jun_21", "Jul_21", 
                                           "Aug_21", "Sep_21", "Oct_21", "Nov_21"))

trip_clean_final$week_day <- ordered(trip_clean_final$week_day, 
                                     levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                                              "Friday", "Saturday"))
```

**Analysis-1: min, max, median, and average ride lengths.->**


```r
view(describe(trip_clean_final$ride_length, fast=TRUE))
```

**Analysis-2: Total number of customers by membership details.->**


```r
view(table(trip_clean_final$customer_type))
```

**Analysis-3: Total rides for each customer type in minutes.->**


```r
view(setNames(aggregate(ride_length ~ customer_type, trip_clean_final, sum), 
              c("customer_type", " total_ride_length(mins)")))
```

**Analysis-4: Show differences between members and casual riders in terms of length of ride (mean, median, maximum and minimum).->**


```r
view(trip_clean_final %>% 
       group_by(customer_type) %>% 
       summarise(min_length_minutes = min(ride_length), max_length_minutes = max(ride_length), 
                 median_length_minutes = median(ride_length), mean_length_minutes = mean(ride_length)))
```

**Analysis-5: Average ride_length for users by day_of_week and Number of total rides by day_of_week.->**


```r
view(trip_clean_final %>% 
       group_by(week_day) %>% 
       summarize(Avg_length = mean(ride_length),
                 number_of_rides = n())
     )
```

**Analysis-6: Number of average rides by month.->**


```r
view(trip_clean_final %>% 
       group_by(month) %>% 
       summarize(Avg_length = mean(ride_length),
                 number_of_rides = n())
)
```
 
**Analysis-7:Average ride length comparison by each week day according to each customer type.->**


```r
view(aggregate(trip_clean_final$ride_length ~ trip_clean_final$customer_type + 
            trip_clean_final$week_day, FUN = mean))
```

**Analysis-8:Average ride length comparison by each month  according to each customer type.->**


```r
view(aggregate(trip_clean_final$ride_length ~ trip_clean_final$customer_type + 
                 trip_clean_final$month, FUN = mean))
```

**Analysis-9:Analyze rider length data by customer type and weekday.->**


```r
view(
trip_clean_final %>% 
  group_by(customer_type, week_day) %>% 
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length),
            median_duration = median(ride_length),
            max_duration = max(ride_length),
            min_duration = min(ride_length)) 
    )
```

**Analysis-10:Analyze rider length data by customer type and month.->**


```r
view(
  trip_clean_final %>% 
    group_by(customer_type, month) %>% 
    summarize(number_of_rides = n(),
              average_duration = mean(ride_length),
              median_duration = median(ride_length),
              max_duration = max(ride_length),
              min_duration = min(ride_length)) 
)
```

**Write the cleaned file for future visualization.->**

write.csv(trip_clean_final,file = "trip_clean_final_tableau.csv",row.names = FALSE)

# **Phase 5: Data findings with visualizations**


[Tableau link for the Dashboard](https://public.tableau.com/views/GoogleCapstone-Cyclistic/Dashboard1?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link)

[Tableau link for the Data story-telling](https://public.tableau.com/views/GoogleCapstone-Cyclistic/Story1?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link)


### **Visualization:1->**

**This is the first visualization which shows total rides per day in a week by each customer type. The number of  rides per day of week indicates that casual riders peak on the Saturday and Sunday. While members remain steady whole weak accept Tuesday and Wednesday in particular where a slight peak can be seen. This indicates members mainly use the bikes for their regular commutes while casual riders for leisure on weekends.** 

**R-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz1.png)

**Tableau-viz** 
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz1tab.png)

### **Visualization:2->**

**The demand for bike usage in a 24 hour span shows that usage by annual members peak during rush hour which indicates many members may use the bikes for commute to and from work especially with the steep drop after the peak at 6pm. Casual riders are not as volatile as there is a steady increase throughout the day with a steady decrease after the peak at 6pm. This plot also supports the previous one and here we can assume that annual members prefer bikes for there workplace transportation mostly.**  

**R-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz2.png)

**Tableau-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz2tab.png)

### **Visualization:3->** 

**Now, lets see what is the usage status in terms of monthly basis for bike usage. The summer months bring more riders in both types where casual usage is a bit higher. On the other hand, casual rider usage is nearly nonexistent in the winter months than the annual members. There may be  multiple factors which can contribute to this result but annual members still use the service at a good rate in those months as well. So we can assume that annual members use bikes in a more steady through out the year.** 

**R-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz3.png)

**Tableau-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz3tab.png)

### **Visualization:4->** 

**On my fourth plot, i tried to observe the average number of rides in a week. The average ride length of casual riders is considerably longer than those of members and with a peak on Saturday and Sunday. Annual members ride length is near constant length regardless of day of week. So we can assume that annual members use bikes in a more steady way through out the week as well.**

**R-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz4.png)

**Tableau-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz4tab.png)

### **Visualization:5->** 

**The average ride length by casual riders are still considerably longer than members even  when broken by month also. Casual usage got higher average usage rate within the middle months as well. On the other hand, members users' average usage remains lower than casuals but with steady usage through out the year as well.**

**R-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz5.png)

**Tableau-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz5tab.png)

### **Visualization:6->**  

**The bike type breakdown shows members use classic bikes more than casual members.The electric bike usage is nearly identical but casual riders are more willing to use docked bikes than the member users. Here it wasn't clear about the definition of 'Docked Bike' while studying the data sets, but it is evident that it is not a preferable or popular bike service for annual members at all. Both  group prefer classic bikes on top of the other two types.**

**R-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz6.png)

**Tableau-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz6tab.png)

### **Visualization:7->** 

**This plot shows us clearly that the total amount of time ridden by casual riders are greater than that of member riders. Although we have seen in our previous plots that members usage is more steady both weekly and yearly basis, but casuals use the bike for more duration than members.**

**R-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz7.png)
**Tableau-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz7tab.png)

### **Visualization:8->**

**This breakdown bar graph shows weekly frequency distribution of the member and casual customers with bike types. I had organized week days in order from Sunday to Saturday. However, this plot shows us few observations about members and casuals as well. Firstly, members usage are almost similar throughout the week. We can infer that members are mostly working people. Secondly, casual usage is slow for weekdays but weekends are very popular especially Saturday. Thirdly, Docked bike is the most unpopular for both members and casuals as we observed before as well. Fourthly, docked bike is not at all popular within annual members throughout the weekly usage session.**

**R-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz8.png)

**Tableau-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz8tab.png)

### **Visualization:9->**

**This breakdown bar graph that shows monthly frequency distribution of the member and casual customers with bike types. I had organized months in order from December,2020 till November, 2021 as per my data sets for one year. However, this plot shows us a few observations also about members and casuals. Casual usage is higher than the members specially within the middle months. Both user group mostly choose classic bike than other two types all over the year. However, it is evident that member usage is higher even in the winter seasons and they also maintains a steady usage frequency all over the year than the casuals. It also supports the assumption that docked bike is not popular at all within the members through out the year as well.** 

**R-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz9.png)

**Tableau-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz9tab.png)

### **Visualization:10->**

**Now let's observe ride duration behavior for member and casuals by a histogram and we will observe the density in terms of usage behavior as well. For this I have filtered the duration times to less than 100 minutes for a better density. Only observation here is that members tend to take short trips than casuals. Or casual users' trip duration is longer than members. On the other hand, we can also say that member users' ride count is higher within shorter duration.**

**R-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz10.png)

**Tableau-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz10tab.png)

### **Visualization:11->**

**This visualization is a histogram using data from the 'time' column that we have created right after the data cleaning in order to investigate whether there was a difference within the start time for trips or rides between casual and member customer type. There was noticeable higher number of start time for member-users in early morning hours between 7am till 8am specifically.This also support the observation that members are using the bikes mostly for commute to work. On the other hand, there is another high pick noticeable for members again later in the evening that is 5pm to 7pm to be specific. Which indicates end of work day. However, for casuals, it seems that their bike usage is mostly during the later part of the day and afternoon. Here a noticeably higher number of rides being started by casual users between 1pm to 2pm followed by the highest pick at 4pm. In that case we can assume that casual users mostly use the bikes for a leisure.**

**R-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz11.png)

**Tableau-viz**
![](C:/Users/Arifeen/Downloads/Cyclistic_tableau/viz11tab.png)

Options Used to Create Forecasts

Time series:	Hour of Time
Measures:	Count of Ride Length

Forecast forward:	11 hours (11 PM - 9 AM)
Forecast based on:	12 AM - 10 PM
Ignore last:	1 hour (11 PM)
Seasonal pattern:	None (Not enough data to search for a seasonal pattern recurring every 24 Hours)



# **Phase 6: ACT**

### **Key Takeaways->**

* **Annual members mainly use the bikes for regular commutes while casual users for leisure on weekends or summer months in a year.**

* **Annual Members bike usage is more steady through out a day of weeks or even in months of a year than casual users bike usage.**

* **Both casual and annual members prefer classic bikes than other two types of bikes.**

* **Annual members mainly use classic bikes and rarely use docked bikes but casual riders are more open to riding all kinds of bikes.**

* **Casual users ride nearly 50% longer than the annual members in terms of ride duration in total.**

* **Casual riders do not use the service during the winter months as much as annual members.**

### **Recommendations->** 

* **To reach the most riders, marketing should be targeted for the busiest casual rider days (Friday, Saturday and Sunday), busiest hours (afternoon) and the most popular months (June, July and August).**

* **Run promotions for annual memberships during the winter months to boost sales and try to convert casual riders into annual members.**

* **Decrease the price of single-fare and full-day passes from Monday through Friday to boost casual ridership during the work hour.**

* **Single-fare and full-day passes' sale can be increased on Saturday and Sunday to entice customers to convert from casual ridership into annual membership.**


### **Additional analysis for future**

* **Age and gender - This data  is missing as these could show the demographics of the existing customers which could be used to target others in similar groups.**

* **Marital status - This data could be used for promotional use to target entire families and not just individuals as well.**

* **Pricing of plans - This missing data can be used to optimize the cost benefit analysis for existing customers and potential new ones.**

* **Income range - This data could be used for targeting potential customers in certain income ranges and/or neighborhoods.**


[Tableau link for the Dashboard](https://public.tableau.com/views/GoogleCapstone-Cyclistic/Dashboard1?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link)

[Tableau link for the Data story-telling](https://public.tableau.com/views/GoogleCapstone-Cyclistic/Story1?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link)

**------------------------------------------------------------------------------------------------------End of Case Study----------------------------------------------------------------**

