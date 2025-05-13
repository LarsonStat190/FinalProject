rm(list = ls())
library(tidyverse)
library(lubridate)
library(ggplot2)

#reading in clustering results
pre <- read.csv("pre_covid_bidwell_full.csv")
post <- read.csv("post_covid_bidwell_full.csv")

#Cluster Sizes
#Pre-Covid
pre_size <- pre %>%
  group_by(Cluster) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count/sum(count))*100)

#Post-Covid
post_size <- post %>%
  group_by(Cluster) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count/sum(count))*100)

#Population Percentages of individual values
#Pre-Covid 
pre_population <- pre %>%
  pivot_longer(cols = -c(X, Cluster), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable, Value) %>%
  summarise(Pop_Count = n(), .groups = "drop") %>%
  mutate(Pop_Percentage = round(Pop_Count / sum(Pop_Count) * 100, 2)) %>%
  unite("Variable_Value", Variable, Value, sep = ": ") %>%
  filter(Pop_Percentage != 0)

#Post-Covid
post_population <- post %>%
  pivot_longer(cols = -c(X, Cluster), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable, Value) %>%
  summarise(Pop_Count = n(), .groups = "drop") %>%
  mutate(Pop_Percentage = round(Pop_Count / sum(Pop_Count) * 100, 2)) %>%
  unite("Variable_Value", Variable, Value, sep = ": ") %>%
  filter(Pop_Percentage != 0)

#Percentages of every value for each cluster divided by the populations percentage
#The percentage of the value present in the population is used to prevent values with a strong presence in the population from being over represented in the cluster
#Pre-Covid
pre_cluster_table <- pre %>%
  mutate(Cluster = as.character(Cluster)) %>% 
  pivot_longer(cols = -c(X, Cluster), names_to = "Variable", values_to = "Value") %>% 
  group_by(Cluster, Variable, Value) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Cluster, Variable) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2)) %>%
  ungroup() %>%
  unite("Variable_Value", Variable, Value, sep = ": ") %>%
  left_join(pre_population, by = "Variable_Value") %>%
  mutate(Relative_Percentage = round(Percentage/Pop_Percentage,2)) %>%
  mutate(Cluster = as.numeric(Cluster)) %>%
  select(Variable_Value, Cluster, Relative_Percentage) %>%
  group_by(Variable_Value, Cluster) %>%
  pivot_wider(names_from = Cluster, values_from = Relative_Percentage)

#Pre-Covid
post_cluster_table <- post %>%
  mutate(Cluster = as.character(Cluster)) %>% 
  pivot_longer(cols = -c(X, Cluster), names_to = "Variable", values_to = "Value") %>% 
  group_by(Cluster, Variable, Value) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Cluster, Variable) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2)) %>%
  ungroup() %>%
  unite("Variable_Value", Variable, Value, sep = ": ") %>%
  left_join(pre_population, by = "Variable_Value") %>%
  mutate(Relative_Percentage = round(Percentage/Pop_Percentage,2)) %>%
  mutate(Cluster = as.numeric(Cluster)) %>%
  select(Variable_Value, Cluster, Relative_Percentage) %>%
  group_by(Variable_Value, Cluster) %>%
  pivot_wider(names_from = Cluster, values_from = Relative_Percentage)

#Intermediary step comparing pre and post
post_long <- post_cluster_table %>%
  pivot_longer(cols = -Variable_Value, names_to = "Cluster", values_to = "Value") %>%
  filter(!is.na(Value))

pre_long <- pre_cluster_table %>%
  pivot_longer(cols = -Variable_Value, names_to = "Cluster", values_to = "Value") %>%
  filter(!is.na(Value))

# Create all combinations of cluster pairs and display their weighted jaccard similarity score
# Each score is weighted by the values calculated in the pre and post cluster table
jaccard_table <- expand_grid(Cluster.post = unique(post_long$Cluster),
                          Cluster.pre = unique(pre_long$Cluster)) %>%
  left_join(post_long, by = c("Cluster.post" = "Cluster")) %>%
  rename(Value.post = Value) %>%
  left_join(pre_long, by = c("Cluster.pre" = "Cluster", "Variable_Value")) %>%
  rename(Value.pre = Value) %>%
  replace_na(list(Value.post = 0, Value.pre = 0)) %>%
  group_by(Cluster.post, Cluster.pre) %>%
  summarise(
    Weighted_Jaccard = sum(pmin(Value.post, Value.pre)) / sum(pmax(Value.post, Value.pre)),
    .groups = "drop"
  ) %>%
  arrange(desc(Weighted_Jaccard))


#Comparing Similar Clusters
# 2 Pre and 13 Post
# Weighted Jaccard: .6994698

pre_cluster_2 <- pre_cluster_table %>%
  select(Variable_Value, `2`) %>% 
  rename(Pre_Cluster_2 = `2`) 
  
post_cluster_13 <- post_cluster_table %>%
  select(Variable_Value, `13`) %>%
  rename(Post_Cluster_13 = `13`)

#Table showing most popular values in each cluster
combined_table_2_13 <- full_join(pre_cluster_2, post_cluster_13, by = "Variable_Value") %>%
  filter(!is.na(Pre_Cluster_2) | !is.na(Post_Cluster_13)) %>%
  filter(Pre_Cluster_2 >= 20, Post_Cluster_13 >= 20) %>%
  mutate(average = (Pre_Cluster_2 + Post_Cluster_13) / 2) %>%
  arrange(desc(average))

# 7 Pre and 11 Post
# Weighted Jaccard: .6853933

pre_cluster_7 <- pre_cluster_table %>%
  select(Variable_Value, `7`) %>% 
  rename(Pre_Cluster_7 = `7`)

post_cluster_11 <- post_cluster_table %>%
  select(Variable_Value, `11`) %>%
  rename(Post_Cluster_11 = `11`)

#Table showing most popular values in each cluster
combined_table_7_11 <- full_join(pre_cluster_7, post_cluster_11, by = "Variable_Value") %>%
  filter(!is.na(Pre_Cluster_7) | !is.na(Post_Cluster_11)) %>% 
  filter(Pre_Cluster_7 >= 20, Post_Cluster_11 >=20) %>%
  mutate(average = (Pre_Cluster_7 + Post_Cluster_11) / 2) %>%
  arrange(desc(average))

# 3 Pre and 8 Post
# Weighted Jaccard: .6716755

pre_cluster_3 <- pre_cluster_table %>%
  select(Variable_Value, `3`) %>% 
  rename(Pre_Cluster_3 = `3`) 
  
post_cluster_8 <- post_cluster_table %>%
  select(Variable_Value, `8`) %>%
  rename(Post_Cluster_8 = `8`)

#Table showing most popular values in each cluster
combined_table_3_8 <- full_join(pre_cluster_3, post_cluster_8, by = "Variable_Value") %>%
  filter(!is.na(Pre_Cluster_3) | !is.na(Post_Cluster_8)) %>% 
  filter(Pre_Cluster_3 >= 20, Post_Cluster_8 >=20) %>%
  mutate(average = (Pre_Cluster_3 + Post_Cluster_8) / 2) %>%
  arrange(desc(average))

# 1 Pre and 19 Post
# Weighted Jaccard: 

pre_cluster_1 <- pre_cluster_table %>%
  select(Variable_Value, `1`) %>% 
  rename(Pre_Cluster_1 = `1`) 

post_cluster_19 <- post_cluster_table %>%
  select(Variable_Value, `19`) %>%
  rename(Post_Cluster_19 = `19`)

#Table showing most popular values in each cluster
combined_table_1_19 <- full_join(pre_cluster_1, post_cluster_19, by = "Variable_Value") %>%
  filter(!is.na(Pre_Cluster_1) | !is.na(Post_Cluster_19)) %>% 
  filter(Pre_Cluster_1 >= 20, Post_Cluster_19 >=20) %>%
  mutate(average = (Pre_Cluster_1 + Post_Cluster_19) / 2) %>%
  arrange(desc(average))

# 11 Pre and 18 Post
# Weighted Jaccard: 

pre_cluster_11 <- pre_cluster_table %>%
  select(Variable_Value, `11`) %>% 
  rename(Pre_Cluster_11 = `11`) 

post_cluster_18 <- post_cluster_table %>%
  select(Variable_Value, `18`) %>%
  rename(Post_Cluster_18 = `18`)

#Table showing most popular values in each cluster
combined_table_11_18 <- full_join(pre_cluster_11, post_cluster_18, by = "Variable_Value") %>%
  filter(!is.na(Pre_Cluster_11) | !is.na(Post_Cluster_18)) %>% 
  filter(Pre_Cluster_11 >= 20, Post_Cluster_18 >=20) %>%
  mutate(average = (Pre_Cluster_11 + Post_Cluster_18) / 2) %>%
  arrange(desc(average))