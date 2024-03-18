##### set up & functions #####
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caTools)
library(randomForest)


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
         PTS = PTS / MP) #scaling has no major effect on results


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

## looking SF classified as SG
SF_as_SG <- player_test %>% 
  mutate(pred_pos = predict_forest) %>% 
  filter(Pos == "SF", 
         pred_pos == "SG") %>% 
  summarise_if(is.numeric, mean)

SF_as_SF <- player_test %>% 
  mutate(pred_pos = predict_forest) %>% 
  filter(Pos == "SF", 
         pred_pos == "SF") %>% 
  summarise_if(is.numeric, mean)

differences <- SF_as_SF - SF_as_SG # no significance differences between these groups



##### custom penalty function ######
penalty_matrix = matrix(c(0, 1, 2, 3, 4,
                          1, 0, 1, 2, 3,
                          2, 1, 0, 1, 2,
                          3, 2, 1, 0, 1,
                          4, 3, 2, 1, 0),
                        byrow=TRUE,
                        nrow=5)

players_tree_penalty_loss = rpart(Pos ~ FG + FGA + FG. +
                       X3P + X3PA + X3P. +
                       X2P + X2PA + X2P. + 
                       eFG. +
                       FT + FTA + FT. +
                       ORB + DRB + TRB + 
                       AST + STL + BLK + 
                       TOV + PF + PTS,
                     data = player_train,
                     method = "class",
                     cp = 0.025,
                     parms=list(loss = penalty_matrix))

prp(players_tree_penalty_loss)

# make predictions

predict_tree_pl_test <- predict(players_tree_penalty_loss,
                             newdata = player_test,
                             type = "class")


tree_pl_cm <- table(player_test$Pos, predict_tree_pl_test)

print(tree_pl_cm)

metrics["CART PL",
        "Accuracy"] <- sum(diag(tree_pl_cm)) / sum(tree_pl_cm)

print(metrics)


##### k-means clustering #####
player_train_kmeans_prep <- player_train %>% 
  select(-Age,
         -Rk,
         -Player,
         -Pos, 
         -Tm,
         -G,
         -GS,
         -MP)

player_kmeans <- kmeans(player_train_kmeans_prep, centers = 5, nstart = 100)
table(player_train$Pos, player_kmeans$cluster)


## k means with only TRB and AST
player_train_kmeans_prep_ta <- player_train_kmeans_prep %>% 
  select(TRB, AST)

player_kmeans_ta <- kmeans(player_train_kmeans_prep_ta, centers = 3, nstart = 100)
table(player_train$Pos, player_kmeans_ta$cluster)

##### hierarchical clustering #####

distances <- dist(player_train_kmeans_prep, method = "euclidean")
player_hclust <- hclust(distances, method = "complete")
plot(player_hclust)

player_hclust_clust = cutree(player_hclust, k = 5)
table(player_train$Pos, player_hclust_clust)


## hierarchical with only TRB and AST
distances_ta <- dist(player_train_kmeans_prep_ta, method = "euclidean")
player_hclust_ta <- hclust(distances_ta, method = "complete")
plot(player_hclust_ta)

player_hclust_clust_ta = cutree(player_hclust, k = 3)
table(player_train$Pos, player_hclust_clust_ta)



