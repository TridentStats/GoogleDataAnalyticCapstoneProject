# Created Tue. 10/03/23 by Matt Glover
# Code for final Visuals of Cyclistic Capstone Project are kept in this file

# ----

# Exploratory Data Visualizations
# Using data visualizations to examine differences between two groups

# Visualize the number of rides per weekday by rider type
all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(secs_elapsed)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Create a visualization of average ride duration per weekday by rider type
all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(secs_elapsed)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

# ----

# Visual for group sizes
summary_df <- all_trips_v3 %>% 
  group_by(member_casual) %>% 
  summarize(
    absolute_frequency = n(),
    relative_frequency = n() / nrow(all_trips_v3)
  )

ggplot(summary_df, aes(x = factor(member_casual), y = relative_frequency, fill = factor(member_casual))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste("Abs: ", absolute_frequency, "\nRel: ", sprintf("%.2f", relative_frequency))),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Comparison of Group Sizes (Absolute & Relative)",
       x = "Ride Type",
       y = "Relative Frequency") +
  guides(fill = FALSE) + 
  theme_minimal()

# ----

# Visualizes the number of rides per weekday by rider type
all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(secs_elapsed)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# ----

# Bar graph to compare bike type preferences between groups.
abs_frequency_table <- as.data.frame(table(all_trips_v3$member_casual, all_trips_v3$rideable_type), margin = 1)
relative_frequency_table <- as.data.frame(prop.table(table(all_trips_v3$member_casual, all_trips_v3$rideable_type), margin = 1))
comb_frequency_table <- cbind(abs_frequency_table, Rel_freq = relative_frequency_table$Freq)

ggplot(comb_frequency_table, aes(fill=Var2, y=Rel_freq, x=Var1)) + 
  geom_bar(position='dodge', stat='identity') +
  geom_text(aes(label = paste(sprintf("%.2f", Rel_freq))),
            position = position_dodge(0.9)) +
  labs(title = "Comparison of Bike Preferences (Relative)",
       x = "Ride Type",
       y = "Relative Frequency",
       fill = "Bike Type") +
  theme_minimal()

# ----

# Bar graph to compare weekend ride relative frequency between groups
abs_frequency_table <- as.data.frame(table(all_trips_v3$member_casual, all_trips_v3$is_weekend), margin = 1)
relative_frequency_table <- as.data.frame(prop.table(table(all_trips_v3$member_casual, all_trips_v3$is_weekend), margin = 1))
comb_frequency_table <- cbind(abs_frequency_table, Rel_freq = relative_frequency_table$Freq)

ggplot(comb_frequency_table, aes(fill=Var2, y=Rel_freq, x=Var1)) + 
  geom_bar(position='dodge', stat='identity') +
  geom_text(aes(label = paste(sprintf("%.2f", Rel_freq))),
            position = position_dodge(0.9)) +
  labs(title = "Comparison of Weekend Ride Frequencies",
       x = "Ride Type",
       y = "Relative Frequency",
       fill = "Weekday Range") +
  scale_fill_manual(values = c("TRUE" = "#F8766D", "FALSE" = "#00BFC4"),
                    labels = c("Mon. - Fri.", "Sat. - Sun.")) +
  theme_minimal()

# ----
# Bar graph to compare summer ride relative frequency between groups.
abs_frequency_table <- as.data.frame(table(all_trips_v3$member_casual, all_trips_v3$is_summer), margin = 1)
relative_frequency_table <- as.data.frame(prop.table(table(all_trips_v3$member_casual, all_trips_v3$is_summer), margin = 1))
comb_frequency_table <- cbind(abs_frequency_table, Rel_freq = relative_frequency_table$Freq)

ggplot(comb_frequency_table, aes(fill=Var2, y=Rel_freq, x=Var1)) + 
  geom_bar(position='dodge', stat='identity') +
  geom_text(aes(label = paste(sprintf("%.2f", Rel_freq))),
            position = position_dodge(0.9)) +
  labs(title = "Comparison of Summer Ride Frequencies",
       x = "Ride Type",
       y = "Relative Frequency",
       fill = "Season") +
  scale_fill_manual(values = c("TRUE" = "#F8766D", "FALSE" = "#00BFC4"),
                    labels = c("Not Summer", "Summer")) +
  theme_minimal()

# ----

# Bar graph to compare ride frequency during working hours (9 AM to 5 PM)
abs_frequency_table <- as.data.frame(table(all_trips_v3$member_casual, all_trips_v3$is_work_hours), margin = 1)
relative_frequency_table <- as.data.frame(prop.table(table(all_trips_v3$member_casual, all_trips_v3$is_work_hours), margin = 1))
comb_frequency_table <- cbind(abs_frequency_table, Rel_freq = relative_frequency_table$Freq)

ggplot(comb_frequency_table, aes(fill=Var2, y=Rel_freq, x=Var1)) + 
  geom_bar(position='dodge', stat='identity') +
  geom_text(aes(label = paste(sprintf("%.2f", Rel_freq))),
            position = position_dodge(0.9)) +
  labs(title = "Comparison of Ride Frequencies during Work Hours",
       x = "Ride Type",
       y = "Relative Frequency",
       fill = "Time of Day") +
  scale_fill_manual(values = c("TRUE" = "#F8766D", "FALSE" = "#00BFC4"),
                    labels = c("Non-Work Hours", "Work Hours")) +
  theme_minimal()

# ----

# Bar graph to compare ride frequency during rush hours (7 AM to 9 AM and 5 PM to 7 PM)

abs_frequency_table <- as.data.frame(table(all_trips_v3$member_casual, all_trips_v3$is_rush_hours), margin = 1)
relative_frequency_table <- as.data.frame(prop.table(table(all_trips_v3$member_casual, all_trips_v3$is_rush_hours), margin = 1))
comb_frequency_table <- cbind(abs_frequency_table, Rel_freq = relative_frequency_table$Freq)

ggplot(comb_frequency_table, aes(fill=Var2, y=Rel_freq, x=Var1)) + 
  geom_bar(position='dodge', stat='identity') +
  geom_text(aes(label = paste(sprintf("%.2f", Rel_freq))),
            position = position_dodge(0.9)) +
  labs(title = "Comparison of Ride Frequencies during Rush Hours",
       x = "Ride Type",
       y = "Relative Frequency",
       fill = "Time of Day") +
  scale_fill_manual(values = c("TRUE" = "#F8766D", "FALSE" = "#00BFC4"),
                    labels = c("Non-Rush Hours", "Rush Hours")) +
  theme_minimal()
