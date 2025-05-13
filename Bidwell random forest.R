rm(list = ls())
library(dplyr)
library(ranger)

# Load data
pre_covid <- read.csv("pre_covid_bidwell_full.csv") %>%
  select(-X)
post_covid <- read.csv("post_covid_bidwell_full.csv") %>%
  select(-X)

# Ensure Cluster is a factor
pre_covid$Cluster <- as.factor(pre_covid$Cluster)
post_covid$Cluster <- as.factor(post_covid$Cluster)

# Separate predictors and target
X_pre <- pre_covid %>% select(-Cluster)
y_pre <- pre_covid$Cluster

X_post <- post_covid %>% select(-Cluster)
y_post <- post_covid$Cluster

rf_model <- ranger(
  formula = Cluster ~ .,
  data = pre_covid,
  num.trees = 500,
  probability = FALSE
)

post_pred <- predict(rf_model, data = X_post)$predictions

comparison <- data.frame(
  Actual_Post_COVID = y_post,
  Predicted_Pre_COVID = post_pred
)

conf_matrix <- table(comparison$Actual_Post_COVID, comparison$Predicted_Pre_COVID)

best_matches <- apply(conf_matrix, 1, function(x) names(which.max(x)))
best_matches

conf_matrix_prop <- prop.table(conf_matrix, 1)  # Row-wise percentages
round(conf_matrix_prop, 2)

best_matches2 <- apply(conf_matrix_prop, 1, function(row) {
  pre_cluster <- which.max(row) - 1  # subtract 1 to match pre-COVID cluster labels starting from 0
  match_percent <- round(max(row) * 100, 1)
  paste0("Pre-COVID Cluster ", pre_cluster, " (", match_percent, "%)")
})

best_matches2

# Step 3: For each post-COVID cluster (row), find max match % and corresponding pre-COVID cluster
match_summary <- apply(conf_matrix_prop, 1, function(row) {
  pre_cluster <- as.integer(names(row)[which.max(row)])
  percent_match <- round(max(row) * 100, 1)
  c("Best_Pre_COVID" = pre_cluster, "Match_Percent" = percent_match)
})

# Step 4: Convert to a data frame for display
match_df <- as.data.frame(t(match_summary))
match_df$Post_COVID_Cluster <- rownames(match_df)
match_df <- match_df %>% select(Post_COVID_Cluster, everything())
match_df$Best_Pre_COVID <- as.integer(match_df$Best_Pre_COVID)
match_df$Match_Percent <- as.numeric(match_df$Match_Percent)



subset_clusters <- pre_covid %>%
  filter(Cluster %in% c(7, 10)) %>%
  mutate(Cluster = factor(Cluster))

library(tidyr)
compare_cats <- subset_clusters %>%
  pivot_longer(-Cluster, names_to = "Feature", values_to = "Value") %>%
  group_by(Cluster, Feature, Value) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Cluster, Feature) %>%
  mutate(Percent = round(100 * Count / sum(Count), 1)) %>%
  ungroup()

library(ggplot2)

ggplot(compare_cats, aes(x = Value, y = Percent, fill = Cluster)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Feature, scales = "free_x") +
  labs(title = "Cluster 7 vs 10: Feature Distributions",
       x = "Category", y = "Percent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#All pre covid
cluster_summary <- pre_covid %>%
  pivot_longer(-Cluster, names_to = "Feature", values_to = "Value") %>%
  group_by(Cluster, Feature, Value) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Cluster, Feature) %>%
  mutate(Percent = round(100 * Count / sum(Count), 1)) %>%
  arrange(Cluster, Feature, desc(Percent)) %>%
  ungroup()

top_feature_values <- cluster_summary %>%
  group_by(Cluster, Feature) %>%
  slice_max(order_by = Percent, n = 1, with_ties = FALSE) %>%
  ungroup()

report_table <- top_feature_values %>%
  unite("Top_Value", Value, Percent, sep = " (", remove = FALSE) %>%
  mutate(Top_Value = paste0(Top_Value, "%)")) %>%
  select(Cluster, Feature, Top_Value) %>%
  pivot_wider(names_from = Feature, values_from = Top_Value)

print(report_table)

library(gt)

report_table %>%
  gt() %>%
  tab_header(
    title = "Post-COVID Cluster Profiles",
    subtitle = "Top Category by Feature for Each Cluster"
  )


# Step 1: Summarize proportions per cluster-feature-value
post_cluster_summary <- post_covid %>%
  pivot_longer(-Cluster, names_to = "Feature", values_to = "Value") %>%
  group_by(Cluster, Feature, Value) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Cluster, Feature) %>%
  mutate(Percent = round(100 * Count / sum(Count), 1)) %>%
  arrange(Cluster, Feature, desc(Percent)) %>%
  ungroup()

# Step 2: Keep only the top category per feature per cluster
top_post_feature_values <- post_cluster_summary %>%
  group_by(Cluster, Feature) %>%
  slice_max(order_by = Percent, n = 1, with_ties = FALSE) %>%
  ungroup()

# Step 3: Format into wide table
post_report_table <- top_post_feature_values %>%
  unite("Top_Value", Value, Percent, sep = " (", remove = FALSE) %>%
  mutate(Top_Value = paste0(Top_Value, "%)")) %>%
  select(Cluster, Feature, Top_Value) %>%
  pivot_wider(names_from = Feature, values_from = Top_Value)

# Step 4: Print nicely
post_report_table %>%
  gt() %>%
  tab_header(
    title = "Post-COVID Cluster Profiles",
    subtitle = "Top Category by Feature for Each Cluster"
  )
