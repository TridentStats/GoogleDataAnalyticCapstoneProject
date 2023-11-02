# Created 10/29/23 by Matt Glover for Google Data Analytics Capstone Project
# This file combines R code I wrote with code provide by Coursera for the bike
# rideshare Capstone project.

# Load necessary packages
library(dplyr)
library(ggplot2)
library(qqplotr)
library(car)
library(lubridate)
library(gridExtra)
library(tidyr)

# ------

# Part 1: Initial Data Transformation Process

# Sets working directory to sub-folder within project folder:
setwd("CyclisticTripData")

# Creates list of all files in above sub-folder (All files are CSVs):
csv_FileList <- list.files(pattern = "\\.csv$")

# Use "lapply()" function to apply "read.csv()" function over "csv_FileList"
# vector. The final output is "dfList", which is a list of matrices:
dfList <- lapply(csv_FileList, read.csv)

# "Creates "FinalDf" data frame, which should combine the 12 original
# CSV files into a single data frame that still has all observations in sequential order:
all_trips <- do.call(bind_rows, dfList)

# Converts two necessary columns from "Char" to "Datetime" values:
all_trips$started_at <- as.POSIXct(all_trips$started_at, format="%Y-%m-%d %H:%M:%S", tz= "America/Chicago")
all_trips$ended_at <- as.POSIXct(all_trips$ended_at, format="%Y-%m-%d %H:%M:%S", tz= "America/Chicago")

# ------

# Part 2: Data Inspection

# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

sum(all_trips$start_station_name == '')

# Count the number of unique values in the 'ride_id' column to confirm that
# each ride (i.e. row) has a unique ride-id value
length(unique(all_trips$ride_id))

# ------

# Part 3: Data Transformation

# Remove unnecessary columns
all_trips_v2 <- all_trips %>%  
  select(-c(ride_id))

# Creates a new interval that is the time elapsed between "started_at" and "ended_at"
all_trips_v2$time_elapsed <- difftime(all_trips_v2$ended_at, all_trips_v2$started_at, units = "secs")

# Creates column that is double values for "time_elapsed"
all_trips_v2$secs_elapsed <- as.numeric(gsub("[^0-9.-]", "", all_trips_v2$time_elapsed))

# Remove all rows where 'secs_elapsed' < 180 or > 28800, and where 'rideable_type' = 'docked_bike'
all_trips_v3 <- all_trips_v2[!(all_trips_v2$rideable_type == "docked_bike" | all_trips_v2$secs_elapsed<180 | all_trips_v2$secs_elapsed>28800),]

# ------

# Part 4: Exploratory Data Analysis

# Descriptive analysis on ride_length (all figures in seconds)
summary(all_trips_v3$secs_elapsed)

# Compare members and casual users
aggregate(all_trips_v3$secs_elapsed ~ all_trips_v3$member_casual, FUN = mean)
aggregate(all_trips_v3$secs_elapsed ~ all_trips_v3$member_casual, FUN = median)
aggregate(all_trips_v3$secs_elapsed ~ all_trips_v3$member_casual, FUN = max)
aggregate(all_trips_v3$secs_elapsed ~ all_trips_v3$member_casual, FUN = min)

# Extracts day of week from each observation, based on an observation's
# "started_at" value. 1 = Sunday, 7 = Saturday.
all_trips_v3$day_of_week <- wday(all_trips_v3$started_at, label = FALSE, week_start = 7)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v3$secs_elapsed ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(secs_elapsed)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Create new column that has month and hour each ride begins
all_trips_v3$startMonth_asInt <- month(all_trips_v3$started_at)
all_trips_v3$startHour_asInt <- hour(all_trips_v3$started_at)

# Calculate relative frequencies for casual riders and member riders
prop.table(table(all_trips_v3$member_casual))

# Calculates conditional relative frequencies for rideable_type
prop.table(table(all_trips_v3$member_casual, all_trips_v3$rideable_type), margin = 1)

# Calculates conditional relative frequencies for day_of_week
prop.table(table(all_trips_v3$member_casual, all_trips_v3$day_of_week), margin = 1)

# Calculates average and median ride length for each group
aggregate(all_trips_v3$secs_elapsed ~ all_trips_v3$member_casual, FUN = mean)
aggregate(all_trips_v3$secs_elapsed ~ all_trips_v3$member_casual, FUN = median)

# Calculates conditional relative frequencies of 'startMonth_asInt' for each group
prop.table(table(all_trips_v3$member_casual, all_trips_v3$startMonth_asInt), margin = 1)

# Calculates conditional relative frequencies of 'startHour_asInt' for each group
prop.table(table(all_trips_v3$member_casual, all_trips_v3$startHour_asInt), margin = 1)

# ----

# Part 5: Boolean Variable Between Group Comparisons

# Below code derives boolean variables from numeric variables for easier analysis

# Creates "is_weekend" boolean in "all_trips_v3" df
all_trips_v3 <- all_trips_v3 %>%
  mutate(is_weekend = day_of_week %in% c(1, 7))

# "is_summer" boolean in "all_trips_v3" df
summer_start_date <- format(as.POSIXct("2023-06-07 00:00:00", tz= "America/Chicago"), format = "%m-%d %H:%M:%S %Z")
summer_end_date <- format(as.POSIXct("2022-08-22 00:00:00", tz= "America/Chicago"), format = "%m-%d %H:%M:%S %Z")
all_trips_v3 <- all_trips_v3 %>%
  mutate(is_summer = format(started_at, format = "%m-%d %H:%M:%S %Z") >= summer_start_date & format(started_at, format = "%m-%d %H:%M:%S %Z") <= summer_end_date)

# "is_work_hours" boolean in "all_trips_v3" df
start_time <- format(as.POSIXct("2023-06-07 09:00:00", tz= "America/Chicago"), format = "%H:%M:%S")
end_time <- format(as.POSIXct("2023-06-07 17:00:00", tz= "America/Chicago"), format = "%H:%M:%S")
all_trips_v3 <- all_trips_v3 %>%
  mutate(is_work_hours = weekdays(started_at) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") &
           format(started_at, format = "%H:%M:%S") >= start_time & format(started_at, format = "%H:%M:%S") <= end_time)

# Creates conditional probability tables for all four boolean variables

# Calculates conditional relative frequencies for "rideable_type"
prop.table(table(all_trips_v3$member_casual, all_trips_v3$rideable_type), margin = 1)

# Calculates conditional relative frequencies for "is_weekend"
prop.table(table(all_trips_v3$member_casual, all_trips_v3$is_weekend), margin = 1)

# Calculates conditional relative frequencies for "is_summer"
prop.table(table(all_trips_v3$member_casual, all_trips_v3$is_summer), margin = 1)

# Calculates conditional relative frequencies for "is_work_hours"
prop.table(table(all_trips_v3$member_casual, all_trips_v3$is_work_hours), margin = 1)

# Examine differences in ride frequencies between groups during commuter hours.
# Setting "is_rush_hours" Boolean variable as 7 AM to 9 AM and 4 PM to 6 PM
morning_start_time <- format(as.POSIXct("2023-06-07 07:00:00", tz= "America/Chicago"), format = "%H:%M:%S")
morning_end_time <- format(as.POSIXct("2023-06-07 09:00:00", tz= "America/Chicago"), format = "%H:%M:%S")
evening_start_time <- format(as.POSIXct("2023-06-07 16:00:00", tz= "America/Chicago"), format = "%H:%M:%S")
evening_end_time <- format(as.POSIXct("2023-06-07 18:00:00", tz= "America/Chicago"), format = "%H:%M:%S")

all_trips_v3 <- all_trips_v3 %>%
  mutate(is_rush_hours = weekdays(started_at) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") &
           (format(started_at, format = "%H:%M:%S") >= morning_start_time & format(started_at, format = "%H:%M:%S") <= morning_end_time)
         | format(started_at, format = "%H:%M:%S") >= evening_start_time & format(started_at, format = "%H:%M:%S") <= evening_end_time)

# ----

# Part 6: Statistical Tests
# Create a contingency tables, perform ch-squared test, and display test results for
# all 5 boolean variables being compared between two groups

# Chi squared tests
rideable_type_contingency_table <- table(all_trips_v3$rideable_type, all_trips_v3$member_casual)
rideable_type_chi_squared_result <- chisq.test(rideable_type_contingency_table)
print(rideable_type_chi_squared_result)

is_weekend_contingency_table <- table(all_trips_v3$is_weekend, all_trips_v3$member_casual)
is_weekend_chi_squared_result <- chisq.test(is_weekend_contingency_table)
print(is_weekend_chi_squared_result)

is_summer_contingency_table <- table(all_trips_v3$is_summer, all_trips_v3$member_casual)
is_summer_chi_squared_result <- chisq.test(is_summer_contingency_table)
print(is_summer_chi_squared_result)

is_work_hours_contingency_table <- table(all_trips_v3$is_work_hours, all_trips_v3$member_casual)
is_work_hours_chi_squared_result <- chisq.test(is_work_hours_contingency_table)
print(is_work_hours_chi_squared_result)

is_rush_hours_contingency_table <- table(all_trips_v3$is_rush_hours, all_trips_v3$member_casual)
is_rush_hours_chi_squared_result <- chisq.test(is_rush_hours_contingency_table)
print(is_rush_hours_chi_squared_result)

# ----

# Part 7: Create and Export Database for Tableau
# Creates a new database that lists addresses by order of difference between
# two groups in "started at" frequency.

start_station_freq_rounded <- all_trips_v3 %>%
  group_by(member_casual, start_station_name, start_lat, start_lng) %>%
  summarise(Frequency = n()) %>%
  ungroup()

start_station_freq_diff <- start_station_freq_rounded %>%
  pivot_wider(names_from = member_casual, values_from = Frequency, values_fill = 0) %>%
  mutate(Diff = casual - member) %>% mutate(Sum = casual + member)

# Rounds coordinant values to 3 decimal places, to prevent unnecessary
# disaggregation.
all_trips_v3$start_lat <- round(all_trips_v3$start_lat, 3)
all_trips_v3$start_lng <- round(all_trips_v3$start_lng, 3)
all_trips_v3$end_lat <- round(all_trips_v3$end_lat, 3)
all_trips_v3$end_lng <- round(all_trips_v3$end_lng, 3)

# Exports 'start_station_freq_diff' data frame as CSV file for analysis in
# Google Sheets, and then to Tableau

casual_data <- start_station_freq_diff[order(start_station_freq_diff$casual, decreasing = TRUE), ]
casual_subset <- casual_data[1:100, ]
write.csv(casual_subset, "casual_subset.csv", row.names = TRUE)

member_data <- start_station_freq_diff[order(start_station_freq_diff$member, decreasing = TRUE), ]
member_subset <- member_data[1:100, ]
write.csv(member_subset, "member_subset.csv", row.names = TRUE)

diff_data <- start_station_freq_diff[order(start_station_freq_diff$Diff, decreasing = TRUE), ]
diff_subset <- diff_data[1:100, ]
write.csv(diff_subset, "diff_subset.csv", row.names = TRUE)
