rm(list = ls())
library(ranger)
###---post polk----
post_covid_polk_train_clustered <- read.csv("post_covid_polk_clustered.csv")
post_covid_polk_test <- read.csv("post_covid_polk_visit.csv")

post_covid_polk_train_clustered <- subset(post_covid_polk_train_clustered, select=-c(Unnamed..0))
post_covid_polk_test <- subset(post_covid_polk_test, select=-c(X))

col_names <- names(post_covid_polk_train_clustered)
post_covid_polk_train_clustered[,col_names] <- lapply(post_covid_polk_train_clustered[,col_names] , factor)

col_names <- names(post_covid_polk_test)
post_covid_polk_test[,col_names] <- lapply(post_covid_polk_test[,col_names] , factor)



# number of features
n_features <- length(setdiff(names(post_covid_polk_train_clustered), "Cluster"))

# train a default random forest model
post_covid_polk_train_rf1 <- ranger(
  Cluster ~ ., 
  data = post_covid_polk_train_clustered,
  mtry = 4,
  num.trees = 500,
  classification = TRUE,
  replace = FALSE,
  keep.inbag = TRUE,
  seed = 123
)
(default_rmse <- sqrt(post_covid_polk_train_rf1$prediction.error))

Results <- predict(post_covid_polk_train_rf1, post_covid_polk_test)
post_covid_polk_test$Cluster <- Results$predictions
post_covid_polk_test$Cluster <- as.factor(post_covid_polk_test$Cluster)

post_covid_polk_full <- post_covid_polk_test

write.csv(post_covid_polk_full, "post_covid_polk_full.csv")
write.csv(post_covid_polk_test, "post_covid_bidwell_polk_clustered.csv")

###--pre polk----

pre_covid_polk_train_clustered <- read.csv("pre_covid_polk_clustered.csv")
pre_covid_polk_test <- read.csv("pre_covid_polk_visit.csv")

pre_covid_polk_train_clustered <- subset(pre_covid_polk_train_clustered, select=-c(Unnamed..0))
pre_covid_polk_test <- subset(pre_covid_polk_test, select=-c(X))

col_names <- names(pre_covid_polk_train_clustered)
pre_covid_polk_train_clustered[,col_names] <- lapply(pre_covid_polk_train_clustered[,col_names] , factor)

col_names <- names(pre_covid_polk_test)
pre_covid_polk_test[,col_names] <- lapply(pre_covid_polk_test[,col_names] , factor)



# number of features
n_features <- length(setdiff(names(pre_covid_polk_train_clustered), "Cluster"))

# train a default random forest model
pre_covid_polk_train_rf1 <- ranger(
  Cluster ~ ., 
  data = pre_covid_polk_train_clustered,
  mtry = 4,
  num.trees = 500,
  classification = TRUE,
  replace = FALSE,
  keep.inbag = TRUE,
  seed = 123
)
(default_rmse <- sqrt(pre_covid_polk_train_rf1$prediction.error))

Results <- predict(pre_covid_polk_train_rf1, pre_covid_polk_test)
pre_covid_polk_test$Cluster <- Results$predictions
pre_covid_polk_test$Cluster <- as.factor(pre_covid_polk_test$Cluster)

pre_covid_polk_full <- pre_covid_polk_test

write.csv(pre_covid_polk_full, "pre_covid_polk_full.csv")
write.csv(pre_covid_polk_test, "pre_covid_bidwell_polk_clustered.csv")
