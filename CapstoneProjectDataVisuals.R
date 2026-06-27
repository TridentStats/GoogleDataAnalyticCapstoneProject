# Created Sat 06/20/2026
# Code for final Visuals of Cyclistic Capstone Project are kept in this file
# Data visual code must be ran AFTER main 'finalCapstoneProjectCode.R' file

# ----

# Create data visualization for absolute and relative size of each group
# (Figure A in r)

summary_df <- all_trips_v3 %>% 
  group_by(member_casual) %>% 
  summarize(
    absolute_frequency = n(),
    relative_frequency = n() / nrow(all_trips_v3)
  )

ggplot(summary_df, aes(x = factor(member_casual), y = relative_frequency, fill = factor(member_casual))) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = paste0(
      format(absolute_frequency, big.mark = ","),   # adds the thousands comma
      " rides (",
      round(relative_frequency * 100),              # converts 0.38 → 38
      "%)"
    )),
    position = position_stack(vjust = 0.5),
    size = 5
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),  # converts 0.2 → 20%
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(title = "Comparison of Group Sizes (Absolute & Relative)",
       x = "Ride Type",
       y = "Relative Frequency (%)") +
  guides(fill = FALSE) + 
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 12, color = 'black')
  )

ggsave("abs_group_diffs.png", width = 8, height = 5, dpi = 300)

# ----

# Create data visualization for in-group relative frequency of 'rideable_type'
# binary variable (Figure B in report)

abs_frequency_table <- as.data.frame(table(all_trips_v3$member_casual, all_trips_v3$rideable_type), margin = 1)
relative_frequency_table <- as.data.frame(prop.table(table(all_trips_v3$member_casual, all_trips_v3$rideable_type), margin = 1))
comb_frequency_table <- cbind(abs_frequency_table, Rel_freq = relative_frequency_table$Freq)

ggplot(comb_frequency_table, aes(fill=Var2, y=Rel_freq, x=Var1)) + 
  geom_bar(position='dodge', stat='identity') +
  geom_text(
    aes(label = paste0(round(Rel_freq * 100), "%")),  # converts 0.61 → 61%
    position = position_dodge(0.9),
    vjust = -0.4
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.1))              # headroom so top labels don't clip
  ) +
  labs(title = "Comparison of Bike Preferences (Relative)",
       x = "Ride Type",
       y = "Relative Frequency (%)",
       fill = "Bike Type") +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 12, color = 'black')
  )

ggsave("bike_type_group_diffs.png", width = 8, height = 5, dpi = 300)
  
# ----
# Step 1: Create function to find difference between groups for each variable

diff_for_variable <- function(data, var_name, label) {
  rel_freq_table <- as.data.frame(
    prop.table(table(data$member_casual, data[[var_name]]), margin = 1)
  )
  
  casual_rate <- rel_freq_table %>%
    filter(Var1 == "casual", Var2 == "TRUE") %>%
    pull(Freq)
  
  member_rate <- rel_freq_table %>%
    filter(Var1 == "member", Var2 == "TRUE") %>%
    pull(Freq)
  
  data.frame(
    variable = label,
    difference = abs(casual_rate - member_rate)
  )
}

# ----
# Step 2: Build one row per binary variable, then add bike-type
# preference separately since it isn't a TRUE/FALSE column.

binary_vars <- bind_rows(
  diff_for_variable(all_trips_v3, "is_weekend",    "Weekend ride"),
  diff_for_variable(all_trips_v3, "is_summer",     "Summer ride"),
  diff_for_variable(all_trips_v3, "is_work_hours", "Work-hour ride"),
  diff_for_variable(all_trips_v3, "is_rush_hours", "Rush-hour ride")
)

bike_pref_table <- as.data.frame(
  prop.table(table(all_trips_v3$member_casual, all_trips_v3$rideable_type), margin = 1)
)
bike_pref_diff <- bike_pref_table %>%
  filter(Var2 == "electric_bike") %>%
  summarize(difference = abs(diff(Freq))) %>%
  pull(difference)

diff_summary <- bind_rows(
  binary_vars,
  data.frame(variable = "Electric-bike preference", difference = bike_pref_diff)
) %>%
  arrange(difference) %>%
  mutate(variable = factor(variable, levels = variable))  # locks the sort order for the plot

# ----
# Step 3: Plot as a horizontal ranked bar chart

ggplot(diff_summary, aes(x = variable, y = difference)) +
  geom_col(fill = '#00BFC4', width = 0.6) +
  geom_text(
    aes(label = percent(difference, accuracy = 1)),
    hjust = -0.15,
    size = 4,
    color = "black"
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(diff_summary$difference) * 1.25),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Difference in Relative Frequency: Casual vs. Member Riders",
    subtitle = "Larger difference = stronger signal for targeted advertising",
    x = NULL,
    y = "Absolute difference in relative frequency"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 1.8),
    plot.subtitle = element_text(color = "black"),
    panel.grid.major.x = element_line(color = 'white'),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 12, color = 'black'),
    panel.border = element_blank(),
    plot.background  = element_rect(fill = "grey92", color = NA)
  )

# Save for report
ggsave("ranked_differences_chart.png", width = 8, height = 5, dpi = 300)

