rm(list = ls())
library(tidyr)
library(dplyr)

pre <- read.csv("data/pre_covid_urbandale_clustered.csv")
post <- read.csv("data/post_covid_urbandale_clustered.csv")

pre <- pre %>% select(-Cluster, -Unnamed..0)
post <- post %>% select(-Cluster, -Unnamed..0)

pre$Time <- "Pre-COVID"
post$Time <- "Post-COVID"

combined <- bind_rows(pre, post)

combined <- combined %>% mutate(across(everything(), as.factor))

long_df <- combined %>%
  pivot_longer(cols = -Time, names_to = "Variable", values_to = "Category")

summary <- long_df %>%
  group_by(Time, Variable, Category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Time, Variable) %>%
  mutate(Proportion = Count / sum(Count)) %>%
  ungroup()

library(ggplot2)

selected_vars <- c("race", "ethnicity", "fed_poverty_level", "income_source", "snap_household", "family_type")

summary$Time <- factor(summary$Time, levels = c("Pre-COVID", "Post-COVID"))

ggplot(filter(summary, Variable %in% selected_vars),
       aes(x = Category, y = Proportion, fill = Time)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Variable, scales = "free_x", ncol = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Urbandale: Pre vs Post-COVID Overview of Key Variables",
       x = NULL, y = "Proportion")
