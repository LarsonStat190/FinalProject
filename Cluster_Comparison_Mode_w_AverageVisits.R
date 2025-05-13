rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)

# Helper to get mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Function to get cluster profile excluding unwanted columns
get_cluster_profile <- function(data, cluster_col_name, cluster_id, exclude_cols) {
  cluster_data <- data[data[[cluster_col_name]] == cluster_id, ]
  cluster_size <- nrow(cluster_data)  # Count the number of rows in the cluster
  compare_vars <- setdiff(names(cluster_data), exclude_cols)
  # Convert factors to characters for comparison
  cluster_data_char <- cluster_data[, compare_vars, drop = FALSE]
  cluster_data_char[] <- lapply(cluster_data_char, as.character)
  profile <- sapply(cluster_data_char, get_mode)
  return(list(profile = profile, size = cluster_size))
}

# Load datasets
pre_data <- read.csv("pre_covid_polk_clustered.csv", stringsAsFactors = TRUE)
post_data <- read.csv("post_covid_polk_clustered.csv", stringsAsFactors = TRUE)
pre_full <- read.csv("pre_covid_polk_full.csv", stringsAsFactors = TRUE)
post_full <- read.csv("post_covid_polk_full.csv", stringsAsFactors = TRUE)

pre_total_visits <- table(pre_full$Cluster)
post_total_visits <- table(post_full$Cluster)


# Columns to exclude from comparison
exclude_cols <- c("Cluster", "service_name", "X", "zip", "Unnamed..0")

# Validate presence of Cluster
if (!"Cluster" %in% colnames(pre_data) || !"Cluster" %in% colnames(post_data)) {
  stop("Both datasets must contain a 'Cluster' column.")
}

# Identify variables used in comparison
comparison_vars <- setdiff(intersect(names(pre_data), names(post_data)), exclude_cols)
cat("Variables used in cluster similarity comparison:\n")
print(comparison_vars)

# Get cluster IDs
pre_clusters <- sort(unique(pre_data$Cluster))
post_clusters <- sort(unique(post_data$Cluster))

# Get cluster profiles
pre_profiles <- lapply(pre_clusters, function(c) get_cluster_profile(pre_data, "Cluster", c, exclude_cols))
post_profiles <- lapply(post_clusters, function(c) get_cluster_profile(post_data, "Cluster", c, exclude_cols))

# Calculate total visits per cluster
pre_ave_visits <- sapply(pre_clusters, function(c) {
  denom <- sum(pre_data$Cluster == c, na.rm = TRUE)
  if (denom > 0) {
    num <- sum(pre_data$visits[pre_data$Cluster == c], na.rm = TRUE)
    num / denom
  } else {
    NA
  }
})

post_ave_visits <- sapply(post_clusters, function(c) {
  denom <- sum(post_data$Cluster == c, na.rm = TRUE)
  if (denom > 0) {
    num <- sum(post_data$visits[post_data$Cluster == c], na.rm = TRUE)
    num / denom
  } else {
    NA
  }
})

# Create similarity matrix
similarity_matrix <- matrix(0, nrow = length(pre_clusters), ncol = length(post_clusters),
                            dimnames = list(pre_clusters, post_clusters))

# Store cluster details (size, percent, mode match, and average visits)
cluster_details <- list()

for (i in seq_along(pre_profiles)) {
  for (j in seq_along(post_profiles)) {
    similarity_matrix[i, j] <- sum(pre_profiles[[i]]$profile == post_profiles[[j]]$profile) / length(pre_profiles[[i]]$profile)
    
    common_modes <- names(pre_profiles[[i]]$profile)[pre_profiles[[i]]$profile == post_profiles[[j]]$profile]
    common_values <- paste(pre_profiles[[i]]$profile[pre_profiles[[i]]$profile == post_profiles[[j]]$profile], collapse = ", ")
    
    key <- paste0("pre_", pre_clusters[i], "_post_", post_clusters[j])
    
    cluster_details[[key]] <- list(
      pre_size = pre_profiles[[i]]$size,
      pre_percent = pre_profiles[[i]]$size / nrow(pre_data),
      post_size = post_profiles[[j]]$size,
      post_percent = post_profiles[[j]]$size / nrow(post_data),
      common_modes = paste(common_modes, collapse = ", "),
      common_values = common_values,
      pre_avg_visits = pre_ave_visits[as.character(pre_clusters[i])],
      post_avg_visits = post_ave_visits[as.character(post_clusters[j])],
      pre_total_visits = pre_total_visits[as.character(pre_clusters[i])],
      post_total_visits = post_total_visits[as.character(post_clusters[j])]
    )
    
  }
}

# Get top matches
match_results <- as.data.frame(as.table(similarity_matrix)) %>%
  rename(Pre_COVID_Cluster = Var1, Post_COVID_Cluster = Var2, Similarity = Freq) %>%
  arrange(desc(Similarity)) %>%
  head(400)

match_results <- match_results %>%
  rowwise() %>%
  mutate(
    key = paste0("pre_", Pre_COVID_Cluster, "_post_", Post_COVID_Cluster),
    Pre_COVID_Cluster_Size = if (key %in% names(cluster_details)) cluster_details[[key]]$pre_size else NA_integer_,
    Pre_COVID_Percent = if (key %in% names(cluster_details)) cluster_details[[key]]$pre_percent else NA_real_,
    Post_COVID_Cluster_Size = if (key %in% names(cluster_details)) cluster_details[[key]]$post_size else NA_integer_,
    Post_COVID_Percent = if (key %in% names(cluster_details)) cluster_details[[key]]$post_percent else NA_real_,
    Common_Modes = if (key %in% names(cluster_details)) cluster_details[[key]]$common_modes else NA_character_,
    Mode_Values = if (key %in% names(cluster_details)) cluster_details[[key]]$common_values else NA_character_,
    Pre_COVID_Total_Visits = if (key %in% names(cluster_details)) cluster_details[[key]]$pre_total_visits else NA_integer_,
    Post_COVID_Total_Visits = if (key %in% names(cluster_details)) cluster_details[[key]]$post_total_visits else NA_integer_,
    Pre_COVID_Ave_Visits = if (!is.na(Pre_COVID_Total_Visits) && !is.na(Pre_COVID_Cluster_Size) && Pre_COVID_Cluster_Size > 0)
      Pre_COVID_Total_Visits / Pre_COVID_Cluster_Size else NA_real_,
    Post_COVID_Ave_Visits = if (!is.na(Post_COVID_Total_Visits) && !is.na(Post_COVID_Cluster_Size) && Post_COVID_Cluster_Size > 0)
      Post_COVID_Total_Visits / Post_COVID_Cluster_Size else NA_real_
  ) %>%
  ungroup() %>%
  select(-key)


# Export match results
write.csv(match_results, "cluster_similarity_matches.csv", row.names = FALSE)

# Heatmap
similarity_df <- as.data.frame(as.table(similarity_matrix))
names(similarity_df) <- c("Pre_COVID_Cluster", "Post_COVID_Cluster", "Similarity")

ggplot(similarity_df, aes(x = as.factor(Post_COVID_Cluster), y = as.factor(Pre_COVID_Cluster), fill = Similarity)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = c("white", "#a6c8ff", "#004080"),
    values = scales::rescale(c(0, 0.4, 1)),
    name = "Similarity"
  ) +
  labs(
    title = "Similarity Between Pre- and Post-COVID Clusters",
    x = "Post-COVID Clusters",
    y = "Pre-COVID Clusters"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
