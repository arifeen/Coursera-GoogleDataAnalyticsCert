set.seed(11022021)

# Variables
ans <- sample(c("Yes", "No", "N/A"),
              size = 100, replace = TRUE,
              prob = c(0.4, 0.35, 0.25))
gen <- sample(c("Male", "Female"),
              size = 100, replace = TRUE)

# Change the levels of the variable
# so "Yes" appears first in the legend
ans <- factor(ans, levels = c("Yes", "No", "N/A"))

# Data frame
data <- data.frame(answer = ans,
                   gender = gen)
# install.packages("dplyr")
# install.packages("scales")
library(dplyr)

# Data transformation
df <- data %>% 
  group_by(answer) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>% 
  mutate(labels = scales::percent(perc))

# install.packages("ggplot2")
library(ggplot2)

ggplot(df, aes(x = "", y = perc, fill = answer)) +
  geom_col() +
  coord_polar(theta = "y") 


ggplot(df, aes(x = "", y = perc, fill = answer)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") 


ggplot(df, aes(x = "", y = perc, fill = answer)) +
  geom_col() +
  geom_label(aes(label = labels),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y") 


ggplot(df, aes(x = "", y = perc, fill = answer)) +
  geom_col(color = "black") +
  geom_label(aes(label = labels), color = c(1, "white", "white"),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Answer")) +
  scale_fill_viridis_d() +
  coord_polar(theta = "y") + 
  theme_void()

#visualization:7  #Number of Trips by rider type on a pie chart
#---------------------------------------------------------------------------

#data transformation.
viz_pie<-trip_clean_final %>% 
  group_by(customer_type) %>% 
  count() %>%  
  ungroup() %>% 
  mutate(perc=`n`/sum(`n`)) %>% 
  arrange(perc) %>% 
  mutate(labels=scales::percent(perc)) %>% 
  #pass the column containing the relative frequency to y and fill by the categorical variable 
  #representing groups.
  ggplot(aes("", perc, fill = customer_type)) +
  geom_col() +
  coord_polar(theta = "y",start = 0)+ 
  #The labels column allows you to add the labels with percentages. 
  #now we are adding them with geom_text.
  # geom_text(aes(label = labels),
  #          position = position_stack(vjust = 0.5)) +
  #coord_polar(theta = "y") 
  #An alternative to geom_text is geom_label. Note that we had to
  #override the aesthetic of the legend with show.legend = FALSE,
  #or a letter will appear over the legend fill colors.
  # geom_label(aes(label = labels),
  #           position = position_stack(vjust = 0.5),
  #          show.legend = FALSE)
#The pie charts can be customized in several ways. 
#we can customize the legend, the colors or the themes. 
#In the following  we removed the default theme with theme_void. 
geom_label(aes(label = labels), color = c("red", "blue"),
           position = position_stack(vjust = 0.5),
           show.legend = FALSE) +
  guides(fill = guide_legend(title = "customer_type")) +
  scale_fill_viridis_d() + 
  theme_void()
viz_pie

