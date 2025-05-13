rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)

# Load data
pre_covid <- read.csv("data/pre_covid_urbandale_clustered.csv") %>%
  select(-Unnamed..0)
post_covid <- read.csv("data/post_covid_urbandale_clustered.csv") %>%
  select(-Unnamed..0)

# Exclude Cluster and convert remaining columns to factors
pre_covid_fct <- pre_covid %>%
  select(-Cluster) %>%
  mutate(across(everything(), as.factor))

# Pivot, summarize counts and proportions
overall_makeup <- pre_covid_fct %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable, Value) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Variable) %>%
  mutate(Proportion = Count / sum(Count)) %>%
  ungroup()

ggplot(overall_makeup, aes(x = Value, y = Proportion)) +
  geom_col(fill = "#4E79A7") +
  facet_wrap(~ Variable, scales = "free_x", ncol = 4) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Overall Makeup of Pre-COVID Data",
       x = NULL, y = "Proportion")

# Now calculate proportions within each cluster
cluster_summary <- pre_covid %>%
  mutate(across(everything(), as.factor)) %>%
  pivot_longer(cols = -Cluster, names_to = "Variable", values_to = "Value") %>%
  group_by(Cluster, Variable, Value) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Cluster, Variable) %>%
  mutate(Cluster_Prop = Count / sum(Count)) %>%
  ungroup()

# Join with overall proportions
cluster_vs_total <- left_join(cluster_summary, overall_makeup,
                              by = c("Variable", "Value")) %>%
  mutate(Diff = Cluster_Prop - Proportion)