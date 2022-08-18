library(tidyverse)

iris
view(iris)
my_data <- as_tibble(iris)

#identify if there are duplicate values in a particular column 
#according to that column name exists
my_data1 <- duplicated(my_data$Sepal.Length)
view(my_data1)

#removes duplicate rows from a dataset based on one/more column
my_data[!duplicated(my_data$Sepal.Length), ]

#OR
my_data %>% distinct(Sepal.Length, .keep_all = TRUE) 

# OR bellow code for the number of rows after removing the duplicates
length(unique(my_data$Sepal.Length))

#based on two columns or variables
my_data %>% distinct(Sepal.Length,Petal.Width, .keep_all = TRUE)

#OR......with one/more variables.
distinct(my_data,Sepal.Length) %>% 
  count()

# for removing duplicate rows from entire datasets.(without column)
which(duplicated(my_data))
my_data1 <- my_data[!duplicated(my_data),]

#check if there are any duplicates
length(distinct(my_data,Sepal.Length)) == nrow(my_data)
length(unique(my_data$Sepal.Length)) == nrow(my_data)




