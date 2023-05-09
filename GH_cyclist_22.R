# Set working directory to find files. Under session tab, choose set working directory and our file is on the desktop.

# Install and load all packages for this project.

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("geosphere")
install.packages("gridExtra")
install.packages("tidyr")
install.packages("scales")

library(tidyverse)
library(ggplot2)
library(lubridate)
library(geosphere)
library(gridExtra)
library(tidyr)
library(scales)

# Load 12 months of data for cleaning and analysis.

Jan <- read.csv("Jan_trips.csv")
Feb <- read.csv("Feb_trips.csv")
Mar <- read.csv("Mar_trips.csv")
Apr <- read.csv("Apr_trips.csv")
May <- read.csv("May_trips.csv")
Jun <- read.csv("Jun_trips.csv")
Jul <- read.csv("Jul_trips.csv")
Aug <- read.csv("Aug_trips.csv") 
Sep <- read.csv("Sep_trips.csv")
Oct <- read.csv("Oct_trips.csv")
Nov <- read.csv("Nov_trips.csv")
Dec <- read.csv("Dec_trips.csv")

# Check data 

summary(Apr)
head(Jul)

# Create Vector to combine annual data
data <- rbind(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)

# Clean data and drop empty rows.

ncol(data)
summary(data)
data <- drop_na(data)
summary(data) # see how many rows were dropped 5,858.
  

# Make new columns with date, month, year, day of week.

data$date <- as.Date(data$started_at) 
data$month <- format(as.Date(data$date), "%m")
data$day <- format(as.Date(data$date), "%d")
data$year <- format(as.Date(data$date), "%Y")
data$day_of_week <- format(as.Date(data$date), "%A")

# Check the new columns.

summary(data)


# Convert started at and ended at to dates with lubridate.

data$started_at <- ymd_hms(data$started_at)
data$ended_at <- ymd_hms(data$ended_at)

# Now that started at and ended at are formatted as date-time, calculate ride duration in minutes. 
## Also, round decimal two places to clean up numbers. 

data$ride_length <- round(difftime(data$ended_at, data$started_at, units= "mins"), 2)


# Calculate the distance of each ride in kilometers.

data$ride_distance <- distGeo(matrix(c(data$start_lng, data$start_lat), ncol = 2), matrix(c(data$end_lng, data$end_lat), ncol = 2))
data$ride_distance <- data$ride_distance/1000
data$ride_distance <- round(data$ride_distance, 2)


#Calculate kilometers per hour for each ride.

data$ride_speed = c(data$ride_distance)/as.numeric(c(data$ride_length), units="hours")
data$ride_speed <- round(data$ride_speed, 2)



# Check for duplicates.

any(duplicated(data))



# Arrange ride_distance in ascending order and inspect the data.

clean_rides <- data %>%
  arrange(ride_length, ride_distance)

# We can see that there are trips with negative ride_length. Make a subset of data excluding rides under 1 minute and 0 km distance traveled.
# Remove trips that were less than 60 seconds. 

clean_rides <- subset(clean_rides, ride_length > 1 & ride_distance > 0)

# Upon further inspection of the data, many of the longest trips have start and end stations as bases and warehouses.
# We will remove these assuming that they are for maintenance. Let's add that to the subset.


clean_rides <- subset(clean_rides, ride_length > 1 & ride_distance > 0 
       & !(start_station_name %in% c("Base - 2132 W Hubbard Warehouse", "Base - 2132 W Hubbard", "Charge")) 
       & end_station_name != "Hubbard Bike-checking (LBS-WH-TEST)" & end_station_name != "Base - 2132 W Hubbard Warehouse")






# Let's make some data frames for calculations.



# Calculate the total number of rides and by user type. 


rides_summary <- clean_rides %>% 
  group_by(member_casual) %>% 
  summarise(total_rides = n()) %>% 
  ungroup() %>% 
  mutate(member_casual = if_else(is.na(member_casual), "Total", member_casual)) %>% 
  bind_rows(data.frame(member_casual = "Total", total_rides = nrow(clean_rides)))


user_rides <- clean_rides %>%
  group_by(member_casual) %>%
  summarise(total_rides = n())
  
user_rides <- user_rides %>%
  mutate(percent = round(total_rides/sum(total_rides) * 100, 2))






# Month Calculations

# Calculate the total numbers of rides each month. 


total_monthly_rides <- clean_rides %>%
  group_by(month) %>%
  summarise(rides = n())



# Calculate the total rides and average ride time and length for user type by month. 

user_type_monthly_calc <- clean_rides %>% 
  group_by(month, member_casual) %>% 
  summarise(number_of_rides = n(),
            total_ride_time = sum(ride_length),
            average_ride_time = mean(ride_length),
            average_ride_distance = mean(ride_distance))




# We need to calculate what percent of monthly rides were by casual and member users.


user_type_monthly_data <- clean_rides %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()) %>%
  group_by(month) %>% 
  mutate(percentage = case_when(
    member_casual == "casual" ~ round(number_of_rides / sum(number_of_rides) * 100, 0),
    member_casual == "member" ~ round(number_of_rides / sum(number_of_rides) * 100, 0)
  ))





# Calculate the total average distance and ride length for member and casual.

user_type_average <- clean_rides %>% 
  group_by(member_casual) %>% 
  summarise(average_ride_time = mean(ride_length),
            average_distance = mean(ride_distance))



# Calculate the average ride distance for each month by user type.

average_monthly_distance_user <- clean_rides %>%
  group_by(month, member_casual) %>%
  summarise(average_distance = mean(ride_distance))
















# Let's make some calculations for daily data.

# Calculate the number of rides, total ride time and average ride time for user type each day of the week.
## First put days in the correct order. 

clean_rides$day_of_week <- ordered(clean_rides$day_of_week, 
                                    levels=c("Sunday", "Monday", "Tuesday", 
                                             "Wednesday", "Thursday", "Friday", "Saturday"))


# Make a table of total rides by user type for each day of the week. 


total_daily_rides <- clean_rides %>%
  mutate(day_of_week = factor(day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n()) %>%
  group_by(day_of_week) %>%
  mutate(percent = round(number_of_rides / sum(number_of_rides) * 100))







# Daily bike trip calculations by user type. # Used in ggplot below

daily_rides_user_type <- clean_rides %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(),
            total_ride_time = sum(ride_length),
            average_ride_time = mean(ride_length),
            average_distance = mean(ride_distance))



# Calculate the average ride distance for each day of the week by user type. 

average_daily_distance_user <- clean_rides %>%
  group_by(day_of_week, member_casual) %>%
  summarise(average_distance = mean(ride_distance))





















# Let's make some calculations for bike types. 


# Calculate the total number of rides for each bike type. 

total_rideable_type <- clean_rides %>%
  group_by(rideable_type) %>%
  summarise(number_of_rides = n (),
            total_ride_length = sum(ride_length),
            average_ride_length = mean(ride_length),
            total_distance = sum(ride_distance),
            average_distance = mean(ride_distance))



#Make a data frame with the number of trips with each bike type by user type.


user_type_ride_total <- clean_rides %>% 
  group_by(rideable_type, member_casual) %>% 
  summarise(number_of_rides = n())




# Let's look at the total ride time and average ride time of each user type. 

user_type_ride_type <- clean_rides %>% 
  group_by(rideable_type, member_casual) %>% 
  summarise(number_of_rides = n(),
            total_ride_time = sum(ride_length),
            average_ride_time = mean(ride_length)) %>%
mutate(percentage = case_when(
  member_casual == "casual" ~ round(number_of_rides / sum(number_of_rides) * 100, 0),
  member_casual == "member" ~ round(number_of_rides / sum(number_of_rides) * 100, 0)
))



# We need to change the order of the bike types for our plot later on.

# Create a named vector of the desired order
levels_order <- c("classic_bike", "electric_bike", "docked_bike")

# Reorder the levels of the rideable_type factor variable
user_type_ride_type$rideable_type <- factor(user_type_ride_type$rideable_type, levels = levels_order)




# Make a table for monthly bike types by user type. 

user_ride_type_monthly <- clean_rides %>% 
  group_by(rideable_type, member_casual, month) %>% 
  summarise(number_of_rides = n(),
            total_ride_time = sum(ride_length),
            average_ride_time = mean(ride_length))




# Make a table for daily bike type by user type. 

user_ride_type_day <- clean_rides %>% 
  group_by(rideable_type, member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(),
            total_ride_time = sum(ride_length),
            average_ride_time = mean(ride_length))


















# Calculate the number of rides starting during each hour over 24 hours. 

rides_by_hour <- clean_rides %>%
  filter(!is.na(started_at)) %>%
  group_by(hour = hour(started_at)) %>%
  summarise(count = n()) %>%
  ungroup() 



# Then let's calculate the number of rides every hour by user type to compare rider use. 

rides_by_hour_user_type <- clean_rides %>%
  filter(!is.na(started_at)) %>%
  group_by(hour = hour(started_at), member_casual) %>%
  summarise(count = n()) %>%
  ungroup()



  



















# Let's make some data frames for most used stations. 

# See top 10 most used start stations. 

top_start_station <- clean_rides %>%
  filter(!is.na(start_station_name)) %>%
  filter(start_station_name != "") %>%
  group_by(start_station_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)


# Calculate top 10 most used start stations by user type. 

top_station_user_type <- clean_rides %>%
  filter(!is.na(start_station_name)) %>%
  filter(start_station_name != "") %>%
  group_by(start_station_name, member_casual) %>%
  summarise(count = n()) %>%
  group_by(start_station_name) %>%
  mutate(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  top_n(10, total_count) %>%
  select(start_station_name, member_casual, count, total_count) %>% 
  head(20)



# We need to calculate the percentage of rides by member and casual users for our plot.

top_station_user_type <- clean_rides %>%
  filter(!is.na(start_station_name)) %>%
  filter(start_station_name != "") %>%
  group_by(start_station_name, member_casual) %>%
  summarise(count = n()) %>%
  group_by(start_station_name) %>%
  mutate(total_count = sum(count),
         percent_user_type = if_else(member_casual == "member", count/total_count * 100, count/total_count * 100)) %>%
  mutate(percent_user_type = round(percent_user_type)) %>%
  arrange(desc(total_count)) %>%
  top_n(10, total_count) %>%
  select(start_station_name, member_casual, count, total_count, percent_user_type) %>% 
  head(20)







# Now let's see top 10 most used start stations by month and user type. 

top_station_user_type_mo <- clean_rides %>%
  filter(!is.na(start_station_name)) %>%
  filter(start_station_name != "") %>%
  group_by(start_station_name, member_casual, month) %>%
  summarise(count = n()) %>%
  group_by(start_station_name) %>%
  mutate(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  top_n(10, total_count) %>%
  select(start_station_name, member_casual, month, count, total_count) %>%
  head(240)
  

# Calculate the top 10 ending stations. 

top_end_station <- clean_rides %>%
  filter(!is.na(end_station_name)) %>%
  filter(end_station_name != "") %>%
  group_by(end_station_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)


# Calculate top 10 most used end stations by user type. 

top_end_station_user_type <- clean_rides %>%
  filter(!is.na(end_station_name)) %>%
  filter(end_station_name != "") %>%
  group_by(end_station_name, member_casual) %>%
  summarise(count = n()) %>%
  group_by(end_station_name) %>%
  mutate(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  top_n(10, total_count) %>%
  select(end_station_name, member_casual, count, total_count) %>% 
  head(20)



# Calculate the percentage of rides by user type for each station. 

top_end_station_user_type <- clean_rides %>%
  filter(!is.na(end_station_name)) %>%
  filter(end_station_name != "") %>%
  group_by(end_station_name, member_casual) %>%
  summarise(count = n()) %>%
  group_by(end_station_name) %>%
  mutate(total_count = sum(count),
         percent_user_type = if_else(member_casual == "member", count/total_count * 100, count/total_count * 100)) %>%
  mutate(percent_user_type = round(percent_user_type)) %>%
  arrange(desc(total_count)) %>%
  top_n(10, total_count) %>%
  select(end_station_name, member_casual, count, total_count, percent_user_type) %>% 
  head(20)











# Now let's see top 10 most used end stations by month and user type. 

top_end_station_user_type_mo <- clean_rides %>%
  filter(!is.na(end_station_name)) %>%
  filter(end_station_name != "") %>%
  group_by(end_station_name, member_casual, month) %>%
  summarise(count = n()) %>%
  group_by(end_station_name) %>%
  mutate(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  top_n(10, total_count) %>%
  select(end_station_name, member_casual, month, count, total_count) %>%
  head(240)



 


























# Phase 4: Analyze 


# Plot 1
# Start by plotting the top 10 most used start stations.


clean_rides %>%
  filter(!is.na(start_station_name)) %>%
  filter(start_station_name != "") %>%
  group_by(start_station_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10) %>%
  mutate(start_station_name = fct_reorder(start_station_name, count)) %>%
  ggplot(aes(start_station_name, count, fill = count)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = comma(count), hjust = 1), show.legend = FALSE) +
  scale_fill_gradient(low = "lightblue", high = "#0044C0") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  labs(x = "Start Station Name",
       y = "Ride Count",
       title = "Total Rides for Top 10 Starting Stations",
       caption = "Data Source: Motivate International Inc") +
  coord_flip() +
  theme(plot.caption.position = "plot", plot.caption = element_text(color = "gray"))




# Plot 2
# Now let's plot the top 10 most used end stations. 

clean_rides %>%
  filter(!is.na(end_station_name)) %>%
  filter(end_station_name != "") %>%
  group_by(end_station_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10) %>%
  mutate(end_station_name = fct_reorder(end_station_name, count)) %>%
  ggplot(aes(end_station_name, count, fill = count)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = comma(count), hjust = 1), show.legend = FALSE) +
  scale_fill_gradient(low = "lightblue", high = "#0044C0") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  labs(x = "End Station Name",
       y = "Ride Count",
       title = "Total Rides for Top 10 Ending Stations",
       caption = "Data Source: Motivate International Inc") +
  coord_flip()+
  theme(plot.caption.position = "plot", plot.caption = element_text(color = "gray"))




# Define custom function to format y-axis labels with suffixes. We'll use this for multiple plots.
format_labels <- function(x) {
  ifelse(x >= 1e6, paste0(round(x/1e6, 1), "M"),
         ifelse(x >= 1e3, paste0(round(x/1e3, 1), "K"),
                format(x, big.mark = ","))
  )
}




# Plot 3

# Plot the top 10 most used stations by user type. 

ggplot(top_station_user_type, aes(x = reorder(start_station_name, total_count),  y = count, fill = member_casual)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("casual" = "#00BA38", "member" = "#F8766D"),
                    name = "User Type") +
  scale_y_continuous(label = format_labels) +
  theme_bw() +
  labs(x = "Start Station Name",
       y = "Number of Rides",
       title = "Top 10 Starting Stations by User type",
       caption = "Data Source: Motivate International Inc") +
  theme(axis.text.x = element_text(vjust = 1, hjust = 1)) +
  theme(plot.caption.position = "plot", plot.caption = element_text(color = "gray")) +
  coord_flip()





# Let's add labels with the percentage of rides by user type for each stations. 
  
  ggplot(top_station_user_type, aes(x = reorder(start_station_name, total_count), 
                                    y = count, fill = member_casual)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("casual" = "#00BA38", "member" = "#F8766D"), name = "User Type") +
  scale_y_continuous(label = format_labels) +
  theme_bw() +
  labs(x = "Start Station Name", y = "Number of Rides", 
       title = "Top 10 Starting Stations by User type", 
       caption = "Data Source: Motivate International Inc") +
  theme(axis.text.x = element_text(vjust = 1, hjust = 1)) +
  theme(plot.caption.position = "plot", plot.caption = element_text(color = "gray")) +
  coord_flip() +
  geom_text(aes(label = paste0(percent_user_type, "%"), y = count), color = "black", 
            size = 3, position = position_stack(vjust = 0.5))















# Plot 4

# Let's plot the top 10 most used end stations by user type. 

ggplot(top_end_station_user_type, aes(x = reorder(end_station_name, total_count),  
                                      y = count, fill = member_casual)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("casual" = "#00BA38", "member" = "#F8766D"),
                    name = "User Type") +
  scale_y_continuous(label = format_labels) +
  theme_bw() +
  labs(x = "End Station Name",
       y = "Number of Rides",
       title = "Top 10 Ending Stations by User type",
       caption = "Data Source: Motivate International Inc") +
  theme(axis.text.x = element_text(vjust = 1, hjust = 1)) +
  theme(plot.caption.position = "plot", plot.caption = element_text(color = "gray")) +
  coord_flip()




# Add labels of percentage of rides by user type for each station. 

ggplot(top_end_station_user_type, aes(x = reorder(end_station_name, total_count),  
                                      y = count, fill = member_casual)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("casual" = "#00BA38", "member" = "#F8766D"),
                    name = "User Type") +
  scale_y_continuous(label = format_labels) +
  theme_bw() +
  labs(x = "End Station Name",
       y = "Number of Rides",
       title = "Top 10 Ending Stations by User type",
       caption = "Data Source: Motivate International Inc") +
  theme(axis.text.x = element_text(vjust = 1, hjust = 1)) +
  theme(plot.caption.position = "plot", plot.caption = element_text(color = "gray")) +
  coord_flip() +
  geom_text(aes(label = paste0(percent_user_type, "%"), y = count), 
            color = "black", size = 3, position = position_stack(vjust = 0.5))



















# Make a pie chart showing the total number of rides by member and casual users. 


pie_chart <- ggplot(user_rides, aes(x = "", y = total_rides, fill = member_casual)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("casual" = "#00BA38", "member" = "#F8766D"),
                    name = "User Type",
                    labels = c(paste0("Casual (", comma(sum(user_rides$total_rides[user_rides$member_casual == "casual"])), ")"),
                               paste0("Member (", comma(sum(user_rides$total_rides[user_rides$member_casual == "member"])), ")"))) +
  labs(title = "Total User Type Rides", caption = "Data Source: Motivate International Inc") +
  theme_void()

# Add labels for the percentage of each slice
pie_chart +
  geom_text(aes(label = paste0(round((total_rides/sum(total_rides))*100), "%")),
            position = position_stack(vjust = 0.5), size = 5, fontface = "bold") +
  theme(legend.position = "bottom",
        plot.caption.position = "plot",
        plot.caption = element_text(color = "gray", hjust = 1.1, vjust = 1))



























# Plot 5

# Plot a pie chart to show the total number of rides for each bike type. 


# Filter the data to only include the three bike types of interest.
rideable_data <- total_rideable_type %>%
  filter(rideable_type %in% c("electric_bike", "docked_bike", "classic_bike"))

# Replace underscores with spaces in bike types label.
rideable_data$rideable_type <- str_replace_all(rideable_data$rideable_type, "_", " ")


# Bike type colors.
bike_colors <- c("classic bike" = "#00C19F", "docked bike" = "#9100B1", "electric bike" = "#D7EF16")

# Create the pie chart
ggplot(rideable_data, aes(x="", y=number_of_rides, fill=rideable_type)) +
  geom_bar(stat="identity", width = 1) +
  coord_polar(theta="y") +
  labs(title = "Total Number of Rides by Bike Type",
       fill = "Bike Type",
       caption = "Data Source: Motivate International Inc") +
  scale_fill_manual(values = bike_colors) + 
  theme_void() +
  theme(legend.position = "bottom",
        plot.caption.position = "plot",
        plot.caption = element_text(color = "gray", hjust = 3.8, vjust = 1)) +
  geom_text(aes(x=1.35, 
                label = comma(number_of_rides)),
            color = "black",
            size = 4,
            fontface = "bold",
            position = position_stack(vjust = 0.5)) + 
  guides(fill = guide_legend(title = "Bike Type"))




  


# Let's compare bike type preferences of members and casual users.






# Plot 6

# Plot the stacked bar chart

ggplot(user_type_ride_total, aes(x=member_casual, y=number_of_rides, fill=rideable_type)) +
  geom_bar(stat="identity", position="stack") +
  scale_y_continuous(labels = format_labels,
                       breaks = seq(0, 3500000, 1000000),
                       limit = c(0, 3500000)) +
    labs(title="Number of Rides by User Type and Bike Type",
         x="Member Type",
         y="Number of Rides",
         fill="Bike Type",
         caption = "Data Source: Motivate International Inc") +
    theme_light() + # makes black outline and white background
  theme(plot.caption.position = "plot") + #pushes caption to bottom right corner
  scale_fill_manual(values = c("#00C19F", "#9100B1", "#D7EF16"),
                    labels = c("classic bike", "docked bike", "electric bike"))+
  theme(plot.caption.position = "plot", plot.caption = element_text(color = "gray"))












#Plot 7


# Create a named vector of the desired order
levels_order <- c("classic_bike", "electric_bike", "docked_bike")

# Reorder the levels of the rideable_type factor variable
user_type_ride_type$rideable_type <- factor(user_type_ride_type$rideable_type, levels = levels_order)

# Filter the data frame by member and casual
member_data <- user_type_ride_type %>% filter(member_casual == "member")
casual_data <- user_type_ride_type %>% filter(member_casual == "casual")



# Create pie chart for member data
ggplot(member_data, aes(x="", y=number_of_rides, fill=rideable_type)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values = c("#00C19F", "#D7EF16"),
                    labels = c("classic bike", "electric bike")) +
  coord_polar("y", start=0) +
  labs(title = "Total Rides by Bike Type for Members",
       fill="Bike Type",
       caption = "Data Source: Motivate International Inc") +
  geom_text(aes(label = comma(number_of_rides)), 
            position=position_stack(vjust=0.5),
            vjust=0.5, size=4, 
            fontface="bold") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.caption.position = "plot",
        plot.caption = element_text(color = "gray", hjust = 3.8, vjust = 1))






# Plot 8
# Create a pie chart for casual data.

ggplot(casual_data, aes(x = "", y = number_of_rides, fill = rideable_type)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = c("#00C19F", "#D7EF16", "#9100B1"),
                    labels = c("classic bike", "electric bike", "docked bike")) +
  coord_polar("y", start=0) +
  geom_text(aes(label = comma(number_of_rides)), 
            position=position_stack(vjust=0.5),
            vjust=0.5, size=4, 
            fontface="bold") +
  labs(title = "Total Rides by Bike Type for Casual Users",
       fill = "Bike Type",
       caption = "Data Source: Motivate International Inc") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.caption.position = "plot",
        plot.caption = element_text(color = "gray", hjust = 3.8, vjust = 1))




















# Plot 9

# Plot the total number of rides per month.

# First we need to convert months from factor to numeric.
# Convert month to numeric.


total_monthly_rides$month <- as.numeric(total_monthly_rides$month)

#check that the conversions are correct.

sapply(total_monthly_rides, class)


# Create a vector of month names
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


# Generate 12 evenly spaced breaks between the min and max values of x.
breaks <- seq(min(total_monthly_rides$month), max(total_monthly_rides$month), length.out = 12)

labels <- month.abb[1:12] # Used to abbreviate months with the first 3 letters. 


# Create the plot.

ggplot(total_monthly_rides, aes(x = month, y = rides)) +
  geom_col(color = "black", fill = "darkgreen") +
  scale_x_continuous(breaks = breaks, labels = labels) +
  scale_y_continuous(limits = c(0, 800000), 
                     breaks = seq(0, 800000, 150000),
                     labels =  format_labels) +
  labs(title = "Total Monthly Rides", x = "Month", y = "Number of Rides", 
       caption = "Data Source: Motivate International Inc") +
  theme_bw () +
  theme(plot.caption.position = "plot", plot.caption = element_text(color = "gray"))













# Plot 10

# Create a plot showing monthly rides by rider type. 

# First convert the months column to factor data type. 
user_type_monthly_data$month <- as.factor(user_type_monthly_data$month)

sapply(user_type_monthly_data, class)


# Then, we need to convert numbers from scientific notation to digits and replace zeros with "k".

options(scipen = 999)


# Now we can plot the variables. 




ggplot(user_type_monthly_data, aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("casual" = "#00BA38", "member" = "#F8766D"),
                    name =  "User Type",
                    labels = c("Casual", "Member")) +
  scale_x_discrete(labels = labels) +
  scale_y_continuous(labels = format_labels, 
                     limits = c(0, 450000), 
                     breaks = seq(0, 450000, 100000)) +
  theme_bw() +
  labs(title = "Total Monthly Rides by User Type", 
       x = "Month", y = "Number of Rides", 
       caption = "Data Source: Motivate International Inc") +
  theme(plot.caption.position = "plot", plot.caption = element_text(color = "gray")) +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_dodge(width = 1), 
            vjust = -0.4, 
            size = 2.8)






  
  
  
  


  
  
  
  






# Plot 11

# Let's plot the total number of rides by user type for each day of the week.

# First put days in order starting with Sunday. 
total_daily_rides$day_of_week <- factor(total_daily_rides$day_of_week, 
                                        levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


ggplot(total_daily_rides, aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_bar(stat = "identity", position =  "dodge") +
  scale_fill_manual(values = c("casual" = "#00BA38", "member" = "#F8766D"),
                    name =  "User Type",
                    labels = c("Casual", "Member")) +
  scale_y_continuous(labels = format_labels) +
  theme_bw() +
  labs(title = "Total Daily Rides by User Type", 
       x = "Day of the Week", y = "Total Number of Rides",
       caption = "Data Source: Motivate International Inc") +
  theme(plot.caption.position = "plot", plot.caption = element_text(color = "gray")) 




ggplot(total_daily_rides, aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_bar(stat = "identity", position =  "dodge") +
  scale_fill_manual(values = c("casual" = "#00BA38", "member" = "#F8766D"),
                    name =  "User Type",
                    labels = c("Casual", "Member")) +
  scale_y_continuous(labels = format_labels, limits = c(0, 550000),
                     breaks = seq(0, 5500000, 100000)) +
  theme_bw() +
  labs(title = "Total Daily Rides by User Type", 
       x = "Day of the Week", y = "Total Number of Rides",
       caption = "Data Source: Motivate International Inc") +
  theme(plot.caption.position = "plot", plot.caption = element_text(color = "gray")) +
  geom_text(aes(label = paste0(percent, "%")), 
            position = position_dodge(width = 1),
            vjust = -0.5,
            size = 4)














# Plot 12

# Plot the number of rides by each hour to see peak times.  


ggplot(rides_by_hour, aes(x = hour, y = count)) +
  geom_line(color = "blue", linewidth = 1.5) +
  scale_x_continuous(breaks = 0:23, limits = c(0, 23)) +
  scale_y_continuous(labels = format_labels,
                     limits = c(0, 600000),
                     breaks = seq(0, 600000, 100000)) +
  theme_bw() +
  labs(title = "Total Rides by Hour", 
       x = "Hour", y = "Total Number of Rides",
       caption = "Data Source: Motivate International Inc")+
  theme(plot.caption.position = "plot", plot.caption = element_text(color = "gray"))










# Plot 13
# Plot the number of rides each hour and compare user types and peak times of each user type. 


ggplot(rides_by_hour_user_type, aes(x = hour, y = count, color = member_casual)) +
  geom_line(size = 1.5) +
  scale_x_continuous(breaks = 0:23, limits = c(0, 23)) +
  scale_y_continuous(labels = format_labels,
                     limits = c(0, 400000),
                     breaks = seq(0, 400000, 100000)) +
  scale_color_manual(values = c("casual" = "#00BA38", "member" = "#F8766D"),
                     name =  "User Type",
                     labels = c("Casual", "Member")) +
  theme_bw() +
  labs(title = "Total User Type Rides by Hour", 
       x = "Hour", y = "Total Number of Rides",
       caption = "Data Source: Motivate International Inc",) +
  theme(plot.caption.position = "plot", plot.caption = element_text(color = "gray"))







user_type_monthly_calc$month <- as.factor(user_type_monthly_calc$month)



# Plot 14
# Let's plot the average ride length by user type for each month of the year.

ggplot(user_type_monthly_calc, aes(x = month, y = average_ride_time, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("casual" = "#00BA38", "member" = "#F8766D"),
                    name =  "User Type",
                    labels = c("Casual", "Member")) +
  scale_x_discrete(labels = labels) +
  theme_bw() +
  labs(title = "User Type Average Ride Time by Month", 
       x = "Month", y = "Average Ride Time (minutes)",
       caption = "Data Source: Motivate International Inc") +
  theme(plot.caption.position = "plot", plot.caption = element_text(color = "gray"))












# Plot 15

# To compare average ride time, plot average daily ride time by user type. 


ggplot(daily_rides_user_type, aes(x = day_of_week, y = average_ride_time, fill = member_casual)) +
  geom_bar(stat = "identity", position =  "dodge") +
  scale_fill_manual(values = c("casual" = "#00BA38", "member" = "#F8766D"),
                    name =  "User Type",
                    labels = c("Casual", "Member")) +
  labs(title = "User Type Average Ride time by Day", 
       x = "Day of the Week", y = "Average Ride Time (minutes)",
       caption = "Data Source: Motivate International Inc") +
  theme_bw() +
  theme(plot.caption.position = "plot", plot.caption = element_text(color = "gray"))














# Plot 16


# Plot the average ride distance by month for each user type. 

ggplot(average_monthly_distance_user, aes(x = month, y = average_distance, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("casual" = "#00BA38", "member" = "#F8766D"),
                    name =  "User Type",
                    labels = c("Casual", "Member")) +
  scale_x_discrete(labels = labels) +
  scale_y_continuous(limits = c(0, 3),
                     breaks = seq(0, 3, 0.5)) +
  labs(title = "User Type Average Distance by Month", 
       x = "Month", y = "Average Ride Distance (km)",
       caption = "Data Source: Motivate International Inc") +
  theme_bw() +
  theme(plot.caption.position = "plot", plot.caption = element_text(color = "gray"))


  








# Plot 17


  # Plot the average ride distance by day of the week for each user type. 

# First put days in order starting with Sunday. 
average_daily_distance_user$day_of_week <- factor(average_daily_distance_user$day_of_week, 
                                        levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  
ggplot(average_daily_distance_user, aes(x = day_of_week, y = average_distance, fill = member_casual)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("casual" = "#00BA38", "member" = "#F8766D"),
                      name =  "User Type",
                      labels = c("Casual", "Member")) +
    scale_y_continuous(limits = c(0, 3),
                       breaks = seq(0, 3, 0.5)) +
  labs(title = "User Type Average Ride Distance by Day", 
       x = "Day of the Week", y = "Average Ride Distance (km)",
       caption = "Data Source: Motivate International Inc") +
  theme_bw() +
  theme(plot.caption.position = "plot", plot.caption = element_text(color = "gray"))















# Project data says there are 692 stations, actual data shows 1655 start station names.
# 1306 start station ids.
# We need to decide what station data to use for the heat map, 1600 is too much. 
# 692 doesn't really seems logical. or we could choose number that shows stations with significant rides.
# Then suggest in analysis of removing less popular stations. 



# Let's use the stations with 500< annual rides 


# Make calculations and tables for Tableau heat maps

# First we need a table of all stations and count number of trips started and number of trips ended. 

# Here's a data frame for all rides started at each station

rides_started_station <- clean_rides %>%
  filter(!is.na(start_station_name)) %>%
  filter(start_station_name != "") %>%
  group_by(start_station_name) %>% 
  summarise(total_rides = n_distinct(ride_id),
            start_lat = first(start_lat),
            start_lng = first(start_lng)) %>%
  arrange(desc(total_rides)) %>%
  head(554)





# Check to make sure there are no duplicates.

any(duplicated(rides_per_station))


# Let's make one for total rides ending at each station.

rides_ended_station <- clean_rides %>%
  filter(!is.na(end_station_name)) %>%
  filter(end_station_name != "") %>%
  group_by(end_station_name) %>% 
  summarise(total_rides = n_distinct(ride_id),
            end_lat = first(end_lat),
            end_lng = first(end_lng)) %>%
  arrange(desc(total_rides)) %>%
  head(541)





# Create a data frame to total rides started and ended at each station by members. 


member_rides_started <- clean_rides %>%
  filter(!is.na(start_station_name)) %>%
  filter(start_station_name != "") %>%
  group_by(start_station_name, member_casual) %>% 
  summarise(total_rides = n_distinct(ride_id),
            startlat = first(start_lat),
            startlng = first(start_lng)) %>%
  filter(member_casual == "member") %>%
  arrange(desc(total_rides)) %>%
  head(504)






member_rides_ended <- clean_rides %>%
  filter(!is.na(end_station_name)) %>%
  filter(end_station_name != "") %>%
  group_by(end_station_name, member_casual) %>% 
  summarise(total_rides = n_distinct(ride_id),
            end_lat = first(end_lat),
            end_lng = first(end_lng)) %>%
  filter(member_casual == "member") %>%
  arrange(desc(total_rides)) %>%
  head(497)






# Now create the same for casual users. 


casual_rides_started <- clean_rides %>%
  filter(!is.na(start_station_name)) %>%
  filter(start_station_name != "") %>%
  group_by(start_station_name, member_casual) %>% 
  summarise(total_rides = n_distinct(ride_id),
            startlat = first(start_lat),
            startlng = first(start_lng)) %>%
  filter(member_casual == "casual") %>%
  arrange(desc(total_rides)) %>%
  head(470)





casual_rides_ended <- clean_rides %>%
  filter(!is.na(end_station_name)) %>%
  filter(end_station_name != "") %>%
  group_by(end_station_name, member_casual) %>% 
  summarise(total_rides = n_distinct(ride_id),
            end_lat = first(end_lat),
            end_lng = first(end_lng)) %>%
  filter(member_casual == "casual") %>%
  arrange(desc(total_rides)) %>%
  head(459)






write.csv(rides_started_station, "rides_started_station.csv")
write.csv(rides_ended_station, "rides_ended_station.csv")


write.csv(member_rides_started, "member_rides_started.csv")
write.csv(member_rides_ended, "member_rides_ended.csv")


write.csv(casual_rides_started, "casual_rides_started.csv")
write.csv(casual_rides_ended, "casual_rides_ended.csv")





