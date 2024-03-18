set.seed(5533)

library(tidyverse)
library(rpart)
library(rpart.plot)
library(caTools)
library(dplyr)

# Data Prep
game_data = read.csv("C:/Users/mylmi/OneDrive/Documents/School/Fall2023/IE5533/game_data.txt")

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
