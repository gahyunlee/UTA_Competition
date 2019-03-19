library(data.table)
library(mclust)
library(klaR)
library(tidyverse)
library(psycho)
library(plot3D)
library(ca)
library(graphics)
library(utils)
library(caret)
library(e1071)
library(gains)
library(fastDummies)

setwd("C:/Users/esnxwng/Desktop/UTA Pier 1/P1ProductData_UTA2019")

initial_data <- fread('P1ProductData_UTA2019.csv')

str(initial_data)

#making sure variables are in the correct format

initial_data$SKU <- as.factor(initial_data$SKU)
initial_data$ClassID <- as.factor(initial_data$ClassID)
initial_data$LifeCycleName <- as.factor(initial_data$LifeCycleName)

#finding for names of columns which contain missing values
colnames(initial_data)[colSums(is.na(initial_data)) > 0]

#These are not dirty values but rather they are items without any depth
blank_depth <- initial_data[is.na(initial_data$Depth),]
initial_data$Depth[is.na(initial_data$Depth)] <- 0

#Using character counts to find null values because of the underlying data structure
initial_data$color_length <- nchar(initial_data$Color)
initial_data$season_length <- nchar(initial_data$Season)
initial_data$material_length <- nchar(initial_data$Material)
initial_data$finish_length <- nchar(initial_data$FinishName)

#Running for loops to find characters counts of 0 and filling with None Value
for (row in 1:nrow(initial_data)) {
  if (initial_data[row, 'color_length'] == 0) {
    initial_data[row, 'Color'] <- 'None'
      }
}

for (row in 1:nrow(initial_data)) {
  if (initial_data[row, 'season_length'] == 0) {
    initial_data[row, 'Season'] <- 'None'
  }
}

for (row in 1:nrow(initial_data)) {
  if (initial_data[row, 'season_length'] == 0) {
    initial_data[row, 'Is_Season'] <- TRUE
  }
  
  else{
    initial_data[row, 'Is_Season'] <- FALSE
  }
}

for (row in 1:nrow(initial_data)) {
  if (initial_data[row, 'material_length'] == 0) {
    initial_data[row, 'Material'] <- 'None'
  }
}

for (row in 1:nrow(initial_data)) {
  if (initial_data[row, 'finish_length'] == 0) {
    initial_data[row, 'FinishName'] <- 'None'
  }
}

for (row in 1:nrow(initial_data)) {
  if (initial_data[row, 'CountOfBigTransactions'] != 0) {
    initial_data[row, 'BigTransactions'] <- TRUE
  }
  else{
    initial_data[row, 'BigTransactions'] <- FALSE
  }
}

#Replacing null values in online_flag variable with False label
initial_data$`online-flag`[is.na(initial_data$`online-flag`)] <- 'FALSE'

#Splitting SizeorUtility variable into 2 seperate variables
initial_data$Size <- sapply(strsplit(as.character(initial_data$SizeOrUtility), "-"), "[", 1)
initial_data$Size[is.na(initial_data$Size)] <- 'None'
initial_data$Utility <- sapply(strsplit(as.character(initial_data$SizeOrUtility), "-"), "[", 2)
initial_data$Utility[is.na(initial_data$Utility)] <- 'None'

#Reformating the word DACOR to DECOR
initial_data$Utility <- gsub("DÉCOR", 'DECOR', initial_data$Utility)

#Creating standardized variables for Height, Weight, Depth and Width
initial_data$std_weight <- initial_data$Weight %>% standardize()
initial_data$std_height <- initial_data$Height %>% standardize()
initial_data$std_width <- initial_data$Width %>% standardize()
initial_data$std_depth <- initial_data$Depth %>% standardize()

#Dropping columns
initial_data <- initial_data[,-c(13, 17, 19, 20:23)]

#k-means clustering with elbow test - on normal weights, heights, depth and width
set.seed(777)

k.max <- 20
wss <- sapply(1:k.max, function(k){kmeans(initial_data[,5:8], k, nstart=50)$tot.withinss})
wss
plot(1:k.max, wss, type='b', pch=19, frame=FALSE,
     xlab='Number of clusters - K',
     ylab='Total within-clusters sum of squares')


actual_cluster <- kmeans(initial_data[, 5:8], 13, nstart=50)
actual_cluster$centers
# 
# d_clust <- Mclust(as.matrix(initial_data[,5:8]), G=1:20, modelNames = mclust.options("emModelNames"))
# d_clust$BIC

#Inserting K-Means Clustering
initial_data$size_cluster <- actual_cluster$cluster

#PCA plot
norm_size_data <- initial_data[,5:8]
norm_pca <- prcomp(norm_size_data, center = FALSE, scale. = FALSE)
print(norm_pca)

target_pca <- initial_data$size_cluster

x <- norm_pca$x[,1]
y <- norm_pca$x[,2]
z <- norm_pca$x[,3]
scatter3D(x, y, z, colvar = target_pca, pch = 18, bty = 'u', colkey = FALSE,
          main = 'Cluster for Sizes', xlab = 'PCA 1', ylab = 'PCA 2', zlab = 'PCA 3',
          col.panel = 'steelblue', expand = 0.7, col.grid= 'darkblue')

#k-means clustering with elbow test - on standardized weights, heights, depth and width
set.seed(777)

initial_data[is.na(initial_data$std_width)]

k.max <- 20
wss_std <- sapply(1:k.max, function(k){kmeans(initial_data[,21:24], k, nstart=50)$tot.withinss})
wss_std

plot(1:k.max, wss_std, type='b', pch=19, frame=FALSE,
     xlab='Number of clusters - K',
     ylab='Total within-clusters sum of squares')


std_cluster <- kmeans(initial_data[, 21:24], 14, nstart=50)
std_cluster

# d_std_clust <- Mclust(as.matrix(initial_data[,20:23]), G=1:20, modelNames = mclust.options("emModelNames"))
# d_std_clust$BIC

#Inserting K-Means Clustering
initial_data$size_std_cluster <- std_cluster$cluster

#PCA plot
std_size_data <- initial_data[,21:24]
std_pca <- prcomp(std_size_data, center = TRUE, scale. = TRUE)
print(std_pca)

target_std_pca <- initial_data$size_std_cluster

x <- std_pca$x[,1]
y <- std_pca$x[,2]
z <- std_pca$x[,3]
scatter3D(x, y, z, colvar = target_pca, pch = 18, bty = 'u', colkey = FALSE,
          main = 'Cluster for Sizes - Standardized', xlab = 'PCA 1', ylab = 'PCA 2', zlab = 'PCA 3',
          col.panel = 'steelblue', expand = 0.7, col.grid= 'darkblue')

#k-modes clustering with elbow test - on FinishName, Color, Material and Country of Origin

k.max <- 20
mss <- sapply(1:k.max, 
              function(k){set.seed(777)
                sum(kmodes(initial_data[,c(10,11,13,14)], k, iter.max = 50 ,weighted = FALSE)$withindiff)})
mss

plot(1:k.max, mss, type='b', pch=19, frame=FALSE,
     xlab='Number of clusters - Modes',
     ylab='Total within-clusters simple-matching distance')

mode_cluster <- kmodes(initial_data[,c(10,11,13,14)], 18 ,iter.max = 50, weighted = FALSE ) 
sum(mode_cluster$withindiff)
initial_data$man_cluster <- mode_cluster$cluster

#Correspondence Analysis
mytable <- with(initial_data, table(initial_data$size_std_cluster, initial_data$man_cluster))

fit <- ca(mytable)
print(fit)

plot(fit)
plot(fit, mass = TRUE, contrib = "absolute", map =
       "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map

#Inspecting number of Values
season_count <- table(initial_data$Season)
season_count

#Visualizing 2 clusters types - Easter
y <- initial_data$man_cluster[initial_data$Season == 'EASTER']
x <- initial_data$size_std_cluster[initial_data$Season == 'EASTER']
z <- initial_data$Quartile[initial_data$Season == 'EASTER']
target_value <- as.factor(initial_data$Season)
test <- target_value[target_value == 'EASTER']
target_value <- as.numeric(test)

scatter3D(x, y, z, colvar = target_value, pch = 18, vbty = 'u', colkey = FALSE,
          main = 'Observing Groupings - Easter', xlab = 'Size Cluster', ylab = 'Mode Cluster', zlab = 'Quartile',
          col.panel = 'steelblue', expand = 0.7, col.grid= 'darkblue')

#Visualizing 2 clusters types - CHRISTMAS
y <- initial_data$man_cluster[initial_data$Season == 'HOLIDAY']
x <- initial_data$size_std_cluster[initial_data$Season == 'HOLIDAY']
z <- initial_data$Quartile[initial_data$Season == 'HOLIDAY']
target_value <- as.factor(initial_data$Season)
test <- target_value[target_value == 'HOLIDAY']
target_value <- as.numeric(test)

scatter3D(x, y, z, colvar = target_value, pch = 18, vbty = 'u', colkey = FALSE,
          main = 'Observing Groupings - Holiday', xlab = 'Size Cluster', ylab = 'Mode Cluster', zlab = 'Quartile',
          col.panel = 'steelblue', expand = 0.7, col.grid= 'darkblue')

#Visualizing 2 clusters types - OUTDOOR
y <- initial_data$man_cluster[initial_data$Season == 'OUTDOOR']
x <- initial_data$size_std_cluster[initial_data$Season == 'OUTDOOR']
z <- initial_data$Quartile[initial_data$Season == 'OUTDOOR']
target_value <- as.factor(initial_data$Season)
test <- target_value[target_value == 'OUTDOOR']
target_value <- as.numeric(test)

scatter3D(x, y, z, colvar = target_value, pch = 18, vbty = 'u', colkey = FALSE,
          main = 'Observing Groupings - Outdoor', xlab = 'Size Cluster', ylab = 'Mode Cluster', zlab = 'Quartile',
          col.panel = 'steelblue', expand = 0.7, col.grid= 'darkblue')

#Visualizing 2 clusters types - HARVEST
y <- initial_data$man_cluster[initial_data$Season == 'HARVEST']
x <- initial_data$size_std_cluster[initial_data$Season == 'HARVEST']
z <- initial_data$Quartile[initial_data$Season == 'HARVEST']
target_value <- as.factor(initial_data$Season)
test <- target_value[target_value == 'HARVEST']
target_value <- as.numeric(test)

scatter3D(x, y, z, colvar = target_value, pch = 18, vbty = 'u', colkey = FALSE,
          main = 'Observing Groupings - Harvest', xlab = 'Size Cluster', ylab = 'Mode Cluster', zlab = 'Quartile',
          col.panel = 'steelblue', expand = 0.7, col.grid= 'darkblue')

#Visualizing 2 clusters types - TOP SELLERS
y <- initial_data$man_cluster[initial_data$Quartile == 4]
x <- initial_data$size_std_cluster[initial_data$Quartile == 4]
z <- initial_data$Quartile[initial_data$Quartile == 4]
target_value <- as.factor(initial_data$Season[initial_data$Quartile == 4])
target_value <- as.numeric(target_value)

scatter3D(x, y, z, colvar = target_value, pch = 18, vbty = 'u', colkey = FALSE,
          main = 'Observing Groupings - Harvest', xlab = 'Size Cluster', ylab = 'Mode Cluster', zlab = 'Quartile',
          col.panel = 'steelblue', expand = 0.7, col.grid= 'darkblue')

#Configuring dataset
initial_data$FinishName <- as.factor(initial_data$FinishName)
initial_data$Color <- as.factor(initial_data$Color)
initial_data$Season <- as.factor(initial_data$Season)
initial_data$Material <- as.factor(initial_data$Material)
initial_data$CountryOfOrigin <- as.factor(initial_data$CountryOfOrigin)
initial_data$Quartile <- as.factor(initial_data$Quartile)
initial_data$`online-flag` <- as.factor(initial_data$`online-flag`)
initial_data$Size <- as.factor(initial_data$Size)
initial_data$Utility <- as.factor(initial_data$Utility)
initial_data$size_cluster <- as.factor(initial_data$size_cluster)
initial_data$size_std_cluster <- as.factor(initial_data$size_std_cluster)
initial_data$man_cluster <- as.factor(initial_data$man_cluster)
initial_data$BigTransactions <- as.factor(initial_data$BigTransactions)

#Selecting variables for Naives Bayes Classifier
nb_data <- initial_data[,c(9:18, 26:27)]

#Seperating dataset into Test & Train
set.seed(777)
train.index <- createDataPartition(nb_data$Quartile, p=0.8, list=FALSE)
train.df <- nb_data[train.index,]
test.df <- nb_data[-train.index,]

#Performing Naives-Bayes Classifier
initial_data_nb <- naiveBayes(Quartile ~ ., data = train.df)
initial_data_nb

#Observing probabilities of Season against Quartile Sales
prop.table(table(train.df$Quartile, train.df$Is_Season), margin = 1)

#Probabilities
pred.prob <- predict(initial_data_nb, test.df, type = 'raw')

# class membership
pred.class <- predict(initial_data_nb, test.df)

# Data frame with actual and predicted values
df <- data.frame(test.df$Quartile, pred.class, pred.prob)

# Confusion Matrices _ test data
confusionMatrix(pred.class, test.df$Quartile)

#Sensitivity is how good the model is at predicting the class of interest
#Specificity is how good the model is at ruling out the class of no interest

### Lift Chart - For Quartile 1
gain <- gains(ifelse(test.df$Quartile==1,1,0), pred.prob[,1], groups=100)

plot(c(0,gain$cume.pct.of.total*sum(test.df$Quartile==1))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="Lift Chart - For Quartile 1", type="l", col = "firebrick2")
lines(c(0,sum(test.df$Quartile==1))~c(0, dim(test.df)[1]), lty=2, col = "blue")

### Lift Chart - For Quartile 2
gain <- gains(ifelse(test.df$Quartile==2,1,0), pred.prob[,2], groups=100)

plot(c(0,gain$cume.pct.of.total*sum(test.df$Quartile==2))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="Lift Chart - For Quartile 2", type="l", col = "firebrick2")
lines(c(0,sum(test.df$Quartile==2))~c(0, dim(test.df)[1]), lty=2, col = "blue")

### Lift Chart - For Quartile 3
gain <- gains(ifelse(test.df$Quartile==3,1,0), pred.prob[,3], groups=100)

plot(c(0,gain$cume.pct.of.total*sum(test.df$Quartile==3))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="Lift Chart - For Quartile 3", type="l", col = "firebrick2")
lines(c(0,sum(test.df$Quartile==3))~c(0, dim(test.df)[1]), lty=2, col = "blue")

### Lift Chart - For Quartile 4
gain <- gains(ifelse(test.df$Quartile==4,1,0), pred.prob[,4], groups=100)

plot(c(0,gain$cume.pct.of.total*sum(test.df$Quartile==4))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="Lift Chart - For Quartile 4", type="l", col = "firebrick2")
lines(c(0,sum(test.df$Quartile==4))~c(0, dim(test.df)[1]), lty=2, col = "blue")

#blue line is the naives baseline - it is the average of randomness

#Selecting variables for Support Vector Machine
svm_data <- initial_data[,c(3,9:24,26,27)]
svm_data$CountOfBigTransactions <- svm_data$CountOfBigTransactions %>% standardize()

target_cols <- svm_data$Quartile
numeric_cols <- svm_data[,c(1, 14:17)]
categorical_cols <- svm_data[,c(2:7, 9:11, 18, 19)]

categorical_cols <- dummy_cols(categorical_cols)
categorical_cols <- categorical_cols[,-c(1:11)]

svm_data <- cbind(target_cols, numeric_cols, categorical_cols)

#Seperating dataset into Test & Train
set.seed(777)
train.index <- createDataPartition(svm_data$target_cols, p=0.8, list=FALSE)
train.df <- svm_data[train.index,]
test.df <- svm_data[-train.index,]

#Performing Support Vector Machines 
svm1 <- svm(target_cols~., data=train.df)
summary(svm1)

# generate predicted values based on test data
pred1 <- predict(svm1, test.df)

# confusion matrix
table(pred1, test.df$target_cols)
#Predicted VS Actual

# misclassification error
sum(diag(table(pred1, test.df$target_cols)))/sum(table(pred1, test.df$target_cols))

write.csv(initial_data, "initial_data.csv")