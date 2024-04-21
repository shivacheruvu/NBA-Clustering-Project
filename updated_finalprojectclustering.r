set.seed(5533)

library(tidyverse)
library(rpart)
library(rpart.plot)
library(caTools)
library(dplyr)
library(nnet)


# Data Prep
game_data = read.csv("/Users/shivacheruvu/Documents/GitHub/NBA-Clustering-Project/game_data.txt")

game_data

# Filters out players with team = TOT
# Isolates players who have played in over 20 games
# Converts null to zero (in 3pt, free throw (to league average for all players), etc)
player_data_processed <- game_data %>% 
  filter(Tm != "TOT") %>% 
  filter(G > 20) %>% 
  select(-Player.additional) %>% 
  mutate(X3P. = ifelse(X3PA == 0, 0, X3P.),
         FT. = ifelse(FTA == 0, mean(FT., na.rm = T), FT.)) %>% 
  mutate(FG = FG / MP,
         FGA = FGA / MP,
         X3P = X3P / MP,
         X3PA = X3PA / MP,
         X2P = X2P / MP,
         X2PA = X2PA / MP,
         FT = FT / MP,
         FTA = FTA / MP,
         ORB = ORB / MP,
         DRB = DRB / MP,
         TRB = TRB / MP,
         AST = AST / MP,
         STL = STL / MP,
         BLK = BLK / MP,
         TOV = TOV / MP,
         PF = PF / MP,
         PTS = PTS / MP)
# Modifying to minutes per game

# Remove non-numerical data and other variables
player_data_cluster = player_data_processed %>% select(-Rk, -Player,-Pos,-Tm,-G,-GS,-MP,-Age) %>% scale() %>% as.data.frame()

# Hierarchical Clustering
distance = dist(player_data_cluster, method = "euclidean")
clusterintensity = hclust(distance, method = "ward.D2")

plot(clusterintensity)

# Cut into Clusters
playerclusters = cutree(clusterintensity, k = 5)
playerclusters

# Cross table
y2 = t(player_data_processed$Pos)
table(playerclusters, y2)


# K means
kmc = kmeans(player_data_cluster, centers = 5, nstart = 20)
table(kmc$cluster, y2)

# Scree Plot
metrics = data.frame(
  matrix(nrow=0, ncol = 2,
         dimnames = list( c() , c("k", "within_cluster_sums"))))
for (i in 2:10) {
  pos_kmc <- kmeans(playervector, centers = i,nstart = 10)
  metrics[i,"k"] = i
  metrics[i,"within_cluster_sums"] = pos_kmc$tot.withinss
}
print(metrics)

ggplot(data = metrics, aes(x = k, y= within_cluster_sums)) +
  geom_point(size = 4, shape = 20, col = "blue") +
  geom_line() +
  ggtitle("Scree Plot") +
  theme_bw() +
  theme(text = element_text(size =25))

# "elbow" is at 3 or 4 clusters. Try clustering with those? 
# 3 Clusters
# Hierarchical
playerclusters = cutree(clusterintensity, k = 3)
playerclusters
y2 = t(player_data_processed$Pos)
table(playerclusters, y2)


# K means
kmc = kmeans(player_data_cluster, centers = 3, nstart = 20)
table(kmc$cluster, y2)

# 4 Clusters
# Hierarchical
playerclusters = cutree(clusterintensity, k = 4)
playerclusters
y2 = t(player_data_processed$Pos)
table(playerclusters, y2)


# K means
kmc = kmeans(player_data_cluster, centers = 4, nstart = 20)
table(kmc$cluster, y2)


# Clustering Using Only Variables from Decision Tree
treedata = subset(player_data_processed, select = c('TRB', 'AST'))

# 5 Clusters
distance = dist(treedata, method = "euclidean")
clusterintensity = hclust(distance, method = "ward.D2")

plot(clusterintensity)

playerclusters = cutree(clusterintensity, k = 5)
y2 = t(player_data_processed$Pos)
table(playerclusters, y2)

# K means
kmc = kmeans(treedata, centers = 5, nstart = 20)
table(kmc$cluster, y2)

# 4 Clusters
# Hierarchical
playerclusters = cutree(clusterintensity, k = 4)
y2 = t(player_data_processed$Pos)
table(playerclusters, y2)

# K means
kmc = kmeans(treedata, centers = 4, nstart = 20)
table(kmc$cluster, y2)

# 3 Clusters
# Hierarchical
playerclusters = cutree(clusterintensity, k = 3)
y2 = t(player_data_processed$Pos)
table(playerclusters, y2)

# K means
kmc = kmeans(treedata, centers = 3, nstart = 20)
table(kmc$cluster, y2)

# Logistic Regression Model to predict player's position
game_data <- read.csv('/Users/shivacheruvu/Documents/GitHub/NBA-Clustering-Project/game_data.txt', sep=",")
game_data$Pos <- as.factor(game_data$Pos)  # Convert position to a factor

# Selecting a subset of predictors for the logistic model
multinom_model <- multinom(Pos ~ Age + G + GS + MP + FG + FGA, data = game_data)

# Summary of the logistic regression model
summary(multinom_model)

# Predicting the positions using the model
predicted_positions <- predict(multinom_model, newdata = game_data, type = "class")
confusion_matrix <- table(game_data$Pos, predicted_positions)

# Accuracy: Overall, how often is the classifier correct?
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Precision, Recall, and F1 for each class
precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
f1_score <- 2 * precision * recall / (precision + recall)

# Mean Precision, Recall, and F1 Score for the model
mean_precision <- mean(precision, na.rm = TRUE)
mean_recall <- mean(recall, na.rm = TRUE)
mean_f1_score <- mean(f1_score, na.rm = TRUE)

# Printing the metrics
cat("Accuracy:", accuracy, "\n")
cat("Mean Precision:", mean_precision, "\n")
cat("Mean Recall:", mean_recall, "\n")
cat("Mean F1 Score:", mean_f1_score, "\n")

