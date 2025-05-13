rm(list = ls())
library(ggplot2)

#reading in clustering results
pre <- read.csv("pre_covid_bidwell_full.csv")
post <- read.csv("post_covid_bidwell_full.csv")

#Pre 7 and Post 11
condition1 <- pre$housing %in% c("Homeless", "Other") & pre$homeless %in% c("Literally Homeless", "Imminently Homeless", "Unstably Housed") & pre$income_source == "Unemployed"

condition2 <-  post$housing %in% c("Homeless", "Other") & post$homeless %in% c("Literally Homeless", "Imminently Homeless", "Unstably Housed") & post$income_source == "Unemployed" 

pct1 <- mean(condition1) * 100
pct2 <- mean(condition2) * 100

#Percentage of the condition in each cluster
comparison_df <- data.frame(
  Table = factor(c("Pre-Covid Population", "Post-Covid Population"),
                 levels = c("Pre-Covid Population", "Post-Covid Population")),
  Percentage = c(pct1, pct2))

ggplot(comparison_df, aes(x = Table, y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), vjust = -0.5) +
  ylim(0, 5) +
  labs(title = "Percent of Total Visitors",
       y = "Percentage",
       x = "") +
  theme_minimal()

#Pre 1 and Post 19
condition1 <- pre$race %in% c("Other", "Unknown") & pre$ethnicity == "Hispanic or Latino" & pre$education == "Pre-K and younger (Currently)"

condition2 <-  post$race %in% c("Other", "Unknown") & post$ethnicity == "Hispanic or Latino" & post$education == "Pre-K and younger (Currently)"

pct1 <- mean(condition1) * 100
pct2 <- mean(condition2) * 100

#Percentage of the condition in each cluster
comparison_df <- data.frame(
  Table = factor(c("Pre-Covid Population", "Post-Covid Population"),
                 levels = c("Pre-Covid Population", "Post-Covid Population")),
  Percentage = c(pct1, pct2))

ggplot(comparison_df, aes(x = Table, y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), vjust = -0.5) +
  ylim(0, 5) +
  labs(title = "Percent of Total Visitors",
       y = "Percentage",
       x = "") +
  theme_minimal()

#Pre 11 and Post 18
condition1 <- pre$education %in% c("Pre-K and younger (Currently)", "K-8 (Currently)") & pre$age == "Minor" & pre$income_source == "Other" & pre$snap_household == "Y" & pre$race == "Black/African American"

condition2 <-  post$education %in% c("Pre-K and younger (Currently)", "K-8 (Currently)") & post$age == "Minor" & post$income_source == "Other" & post$snap_household == "Y" & post$race == "Black/African American"

pct1 <- mean(condition1) * 100
pct2 <- mean(condition2) * 100

#Percentage of the condition in each cluster
comparison_df <- data.frame(
  Table = factor(c("Pre-Covid Population", "Post-Covid Population"),
                 levels = c("Pre-Covid Population", "Post-Covid Population")),
  Percentage = c(pct1, pct2))

ggplot(comparison_df, aes(x = Table, y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), vjust = -0.5) +
  ylim(0, 5) +
  labs(title = "Percent of Total Visitors",
       y = "Percentage",
       x = "") +
  theme_minimal()

#Pre 2 and Post 13
condition1 <- pre$income_source %in% c("Child Support", "FIP (Family Investment Program)", "Disability", "Stay at Home Parent", "Social Security") & pre$snap_household == "Y" & pre$housing_type == "Five or More Unit Apartments" & pre$gender == "Woman (girl)"

condition2 <- post$income_source %in% c("Child Support", "FIP (Family Investment Program)", "Disability", "Stay at Home Parent", "Social Security") & post$snap_household == "Y" & post$housing_type == "Five or More Unit Apartments" & post$gender == "Woman (girl)"

pct1 <- mean(condition1) * 100
pct2 <- mean(condition2) * 100

#Percentage of the condition in each cluster
comparison_df <- data.frame(
  Table = factor(c("Pre-Covid Population", "Post-Covid Population"),
                 levels = c("Pre-Covid Population", "Post-Covid Population")),
  Percentage = c(pct1, pct2))

ggplot(comparison_df, aes(x = Table, y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), vjust = -0.5) +
  ylim(0, 5) +
  labs(title = "Percent of Total Visitors",
       y = "Percentage",
       x = "") +
  theme_minimal()

#Pre 3 and Post 8
condition1 <- pre$education == "No Schooling"

condition2 <- post$education == "No Schooling"

pct1 <- mean(condition1) * 100
pct2 <- mean(condition2) * 100

#Percentage of the condition in each cluster
comparison_df <- data.frame(
  Table = factor(c("Pre-Covid Population", "Post-Covid Population"),
                 levels = c("Pre-Covid Population", "Post-Covid Population")),
  Percentage = c(pct1, pct2))

ggplot(comparison_df, aes(x = Table, y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), vjust = -0.5) +
  ylim(0, 5) +
  labs(title = "Percent of Total Visitors",
       y = "Percentage",
       x = "") +
  theme_minimal()
