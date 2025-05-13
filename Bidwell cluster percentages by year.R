rm(list = ls())
library(ggplot2)
library(dplyr)
library(lubridate)

datafile <- read.csv("clean_datafile.csv")
# Add year column to datafile
datafile <- datafile %>%
  mutate(year = year(served_date))

# Total visitors per year (all locations)
total_visitors <- datafile %>%
  filter(year != 2024) %>%
  group_by(year) %>%
  summarise(total_count = n())

# Visitors to Bidwell Riverside Center per year
bidwell_visitors <- datafile %>%
  filter(location == "Bidwell Riverside Center", year != 2024) %>%
  group_by(year) %>%
  summarise(bidwell_count = n())

# Join and calculate percent
bidwell_datafile <- left_join(bidwell_visitors, total_visitors, by = "year") %>%
  mutate(percent = bidwell_count / total_count * 100)

# Get max percent for y-axis scaling
max_y <- max(bidwell_datafile$percent, na.rm = TRUE)

# Plot
ggplot(bidwell_datafile, aes(x = year, y = percent)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            vjust = -1, size = 3.5) +
  labs(title = "Percent of Total Visitors Who Visited Bidwell by Year",
       x = "Year",
       y = "Visitor Percent") +
  scale_y_continuous(
    limits = c(0, max_y + 5),
    expand = c(0, 0)
  ) +
  theme_minimal()


#Percentages of Pre Cluster 7 and Post Cluster 11
#Various levels of homeless/unhoused
summary_df <- datafile %>%
  filter(!is.na(served_date), location == "Bidwell Riverside Center") %>%
  mutate(year = year(served_date),
         meets_condition = housing %in% c("Homeless", "Other") & homeless %in% c("Literally Homeless", "Imminently Homeless", "Unstably Housed") & income_source == "Unemployed") %>%
  group_by(year) %>%
  summarise(
    total = n(),
    count_with_characteristics = sum(meets_condition, na.rm = TRUE),
    percent = (count_with_characteristics / total) * 100
  )

# Get max percent for y-axis scaling
max_y <- max(summary_df$percent, na.rm = TRUE)

ggplot(summary_df, aes(x = year, y = percent)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            vjust = -1, size = 3.5) +
  scale_y_continuous(limits = c(0, max_y + 5), expand = c(0, 0)) +
  labs(title = "Percent of Bidwell Population by Year",
       x = "Year",
       y = "Percent") +
  theme_minimal()

#Percentages of Pre Cluster 1 and Post Cluster 19
#hispanic/latino, pre-k younger, unknown and other race
summary_df <- datafile %>%
  filter(!is.na(served_date), location == "Bidwell Riverside Center") %>%
  mutate(year = year(served_date),
         meets_condition = ethnicity == "Hispanic or Latino" & education == "Pre-K and younger (Currently)") %>%
  group_by(year) %>%
  summarise(
    total = n(),
    count_with_characteristics = sum(meets_condition, na.rm = TRUE),
    percent = (count_with_characteristics / total) * 100
  )

# Get max percent for y-axis scaling
max_y <- max(summary_df$percent, na.rm = TRUE)

ggplot(summary_df, aes(x = year, y = percent)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            vjust = -1, size = 3.5) +
  scale_y_continuous(limits = c(0, max_y + 5), expand = c(0, 0)) +
  labs(title = "Percent of Bidwell Population by Year",
       x = "Year",
       y = "Percent") +
  theme_minimal()

#Percentage of Pre Cluster 11 and Post Cluster 18
#black/african american minors
summary_df <- datafile %>%
  filter(!is.na(served_date), location == "Bidwell Riverside Center") %>%
  mutate(year = year(served_date),
         meets_condition = race == "Black/African American" & education  %in% c("Pre-K and younger (Currently)", "K-8 (Currently)") & age == "Minor" & snap_household == "Y") %>%
  group_by(year) %>%
  summarise(
    total = n(),
    count_with_characteristics = sum(meets_condition, na.rm = TRUE),
    percent = (count_with_characteristics / total) * 100
  )

# Get max percent for y-axis scaling
max_y <- max(summary_df$percent, na.rm = TRUE)

ggplot(summary_df, aes(x = year, y = percent)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            vjust = -1, size = 3.5) +
  scale_y_continuous(limits = c(0, max_y + 5), expand = c(0, 0)) +
  labs(title = "Percent of Bidwell Population by Year",
       x = "Year",
       y = "Percent") +
  theme_minimal()
