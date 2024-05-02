##### set up & functions #####
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caTools)
library(mclust)
library(kernlab)
library(ggplot2)
library(gghighlight)
library(randomForest)
library(naivebayes)
library(neuralnet)


set.seed(5523)


##### data prep #####

game_data <- read.csv("game_data.txt")

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

##### k-means clustering #####
player_train_prep <- player_data_processed %>% 
  select(-Rk,
         -Player,
         -Pos, 
         -Tm,
         -G,
         -GS,
         -MP,
         -Age) %>%
  scale() %>% 
  as.data.frame()

player_kmeans <- kmeans(player_train_prep, centers = 5, nstart = 100)
table(player_data_processed$Pos, player_kmeans$cluster)

# scree plot
wss <- 0

for (i in 1:15) {
  km.out <- kmeans(player_train_prep, centers = i, nstart=20)
  wss[i] <- km.out$tot.withinss
  
}

plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")


##### hierarchical clustering #####

distances <- dist(player_train_prep, method = "euclidean")
player_hclust <- hclust(distances, method = "complete")
plot(player_hclust)

player_hclust_clust = cutree(player_hclust, k = 5)
table(player_data_processed$Pos, player_hclust_clust)

##### Spectral clustering #####

player_specc <- specc(x=as.matrix(player_train_prep), centers=5, kernel='rbfdot') 
table(player_data_processed$Pos, player_specc@.Data)


##### GMM #####

player_gmm <- Mclust(player_train_prep, G=1:5)
table(player_data_processed$Pos, player_gmm$classification)

##### baseline mode #####

pos_count <- table(player_data_processed$Pos)
print(pos_count)

pos_prop <- table(player_data_processed$Pos) / nrow(player_data_processed)
print(pos_prop)

simple_baseline_accuracy <- max(pos_prop)
print(simple_baseline_accuracy) #SG

metrics <- data.frame(matrix(nrow = 0, 
                             ncol = 1,
                             dimnames = list( c(),
                                              c("Accuracy"))))

metrics["Simple Baseline",
        "Accuracy"] <-  simple_baseline_accuracy

print(metrics)



##### spliting data #####

set.seed(5533)
spl = sample.split(player_data_processed$Pos, SplitRatio = 0.75)
player_train = subset(player_data_processed, spl==TRUE)
player_test = subset(player_data_processed, spl==FALSE)


##### trees #####

players_tree = rpart(Pos ~ FG + FGA + FG. +
                       X3P + X3PA + X3P. +
                       X2P + X2PA + X2P. + 
                       eFG. +
                       FT + FTA + FT. +
                       ORB + DRB + TRB + 
                       AST + STL + BLK + 
                       TOV + PF + PTS,
                     data = player_train,
                     method = "class",
                     cp = 0.025)

prp(players_tree)

# make predictions

predict_tree_test <- predict(players_tree,
                             newdata = player_test,
                             type = "class")


tree_cm <- table(player_test$Pos, predict_tree_test)

print(tree_cm) # largest error in the misclassification of SF as SG

metrics["CART",
        "Accuracy"] <- sum(diag(tree_cm)) / sum(tree_cm)

print(metrics)



##### random forests #####

players_forest <- randomForest(factor(Pos) ~ FG + FGA + FG. +
                                 X3P + X3PA + X3P. +
                                 X2P + X2PA + X2P. + 
                                 eFG. +
                                 FT + FTA + FT. +
                                 ORB + DRB + TRB + 
                                 AST + STL + BLK + 
                                 TOV + PF + PTS,
                               data = player_train, 
                               nodesize = 25, 
                               ntree = 200)


predict_forest <- predict(players_forest, newdata = player_test)

cm_forest <- table(player_test$Pos, predict_forest)
cm_forest

metrics["Random Forest",
        "Accuracy"] <- sum(diag(cm_forest)) / sum(cm_forest)

print(metrics)


###### Naive Bayes #####

prediction_matix <- as.matrix(subset(player_train, select = -c(Rk, Player, Pos, Tm, Age, G, GS, MP)))

naive_bayes <- multinomial_naive_bayes(prediction_matix, player_train$Pos)

test_matrix <- as.matrix(subset(player_test, select = -c(Rk, Player, Pos, Tm, Age, G, GS, MP)))

predict_nb <- predict(naive_bayes, newdata = test_matrix)

cm_nb <- table(player_test$Pos, predict_nb)
cm_nb

metrics["Naive Bayes",
        "Accuracy"] <- sum(diag(cm_nb)) / sum(cm_nb)

print(metrics)

