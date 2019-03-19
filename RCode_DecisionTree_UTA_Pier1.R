library(rpart)
library(rpart.plot)

setwd("C:/Users/esnxwng/Desktop/UTA Pier 1")

pier1.dt <- read.csv("nb_data.csv")

pier1.dt <- pier1.dt[,-c(1)]

str(pier1.dt)

pier1.dt$Quartile <- as.factor(pier1.dt$Quartile)
pier1.dt$size_std_cluster <- as.factor(pier1.dt$size_std_cluster)
pier1.dt$man_cluster <- as.factor(pier1.dt$man_cluster)

table(pier1.dt$FinishName)

pier1.dt$FinishName <- as.character(pier1.dt$FinishName)

for (row in 1:nrow(pier1.dt)) {
  if (pier1.dt[row, 'FinishName'] != 'None') {
    pier1.dt[row, 'FinishName'] <- 'Finish'
  }
}

table(pier1.dt$Color)

pier1.dt$Color <- as.character(pier1.dt$Color)
pier1.dt$Color <- sapply(strsplit(as.character(pier1.dt$Color), "-"), "[", 1)
pier1.dt$Color <- gsub(" ", '', pier1.dt$Color)

for (row in 1:nrow(pier1.dt)) {
  if (pier1.dt[row, 'Color'] != 'Blue' && pier1.dt[row, 'Color'] != 'Brown' && pier1.dt[row, 'Color'] != 'Gold' && pier1.dt[row, 'Color'] != 'Gray' && pier1.dt[row, 'Color'] != 'Green' && pier1.dt[row, 'Color'] != 'Ivory' && pier1.dt[row, 'Color'] != 'Multi' && pier1.dt[row, 'Color'] != 'Natural' && pier1.dt[row, 'Color'] != 'Red' && pier1.dt[row, 'Color'] != 'Silver' && pier1.dt[row, 'Color'] != 'White') {
    pier1.dt[row, 'Color'] <- 'Others'
  }
}

table(pier1.dt$Season)

pier1.dt$Season <- as.character(pier1.dt$Season)

for (row in 1:nrow(pier1.dt)) {
  if (pier1.dt[row, 'Season'] != 'None') {
    pier1.dt[row, 'Season'] <- 'Season'
  }
}

table(pier1.dt$Material)

pier1.dt$Material <- as.character(pier1.dt$Material)
pier1.dt$Material <- sapply(strsplit(as.character(pier1.dt$Material), " - "), "[", 1)
pier1.dt$Material <- sapply(strsplit(as.character(pier1.dt$Material), " -"), "[", 1)

for (row in 1:nrow(pier1.dt)) {
  if (pier1.dt[row, 'Material'] != 'CERAMIC' && pier1.dt[row, 'Material'] != 'FABRIC' && pier1.dt[row, 'Material'] != 'GLASS' && pier1.dt[row, 'Material'] != 'METAL' && pier1.dt[row, 'Material'] != 'WOOD' && pier1.dt[row, 'Material'] != 'None') {
    pier1.dt[row, 'Material'] <- 'Others'
  }
}

table(pier1.dt$CountryOfOrigin)
pier1.dt$CountryOfOrigin <- as.character(pier1.dt$CountryOfOrigin)
pier1.dt$CountryOfOrigin <- sapply(strsplit(as.character(pier1.dt$CountryOfOrigin), ","), "[", 1)

for (row in 1:nrow(pier1.dt)) {
  if (pier1.dt[row, 'CountryOfOrigin'] != 'China' && pier1.dt[row, 'CountryOfOrigin'] != 'India') {
    pier1.dt[row, 'CountryOfOrigin'] <- 'Others'
  }
}

pier1.dt <- pier1.dt[,-c(9, 11, 13)]

str(pier1.dt)

pier1.dt$FinishName <- as.factor(pier1.dt$FinishName)
pier1.dt$Color <- as.factor(pier1.dt$Color)
pier1.dt$Season <- as.factor(pier1.dt$Season)
pier1.dt$Material <- as.factor(pier1.dt$Material)
pier1.dt$CountryOfOrigin <- as.factor(pier1.dt$CountryOfOrigin)

pier1.nb <- pier1.dt[-c(9)]

d_tree <- rpart(Quartile~., data = pier1.nb, method = 'class')
rpart.plot(d_tree, extra = 106, type = 5, tweak = 1.05)

#Plotting for First Quartile
pier1.dt$Quartile <- as.character(pier1.dt$Quartile)
pier1.nq.first <- pier1.dt[which(pier1.dt$Quartile == '1'),]
pier1.nq.first <- pier1.nq.first[,-c(7)]

d_tree_nq_first <- rpart(BigTransactions~., data = pier1.nq.first, method = 'class')
rpart.plot(d_tree_nq_first, extra = 106, type = 5, tweak = 1.05)

#Plotting for Second Quartile
pier1.nq.second <- pier1.dt[which(pier1.dt$Quartile == '2'),]
pier1.nq.second <- pier1.nq.second[,-c(7)]

d_tree_nq_second <- rpart(BigTransactions~., data = pier1.nq.second, method = 'class')
rpart.plot(d_tree_nq_second, extra = 106, type = 5, tweak = 1.05)

#Plotting for Third Quartile
pier1.nq.third <- pier1.dt[which(pier1.dt$Quartile == '3'),]
pier1.nq.third <- pier1.nq.third[,-c(7)]

d_tree_nq_third <- rpart(BigTransactions~., data = pier1.nq.third, method = 'class')
rpart.plot(d_tree_nq_third, extra = 106, type = 5, tweak = 1.05)

#Plotting for Fourth Quartile
pier1.nq.fourth <- pier1.dt[which(pier1.dt$Quartile == '4'),]
pier1.nq.fourth <- pier1.nq.fourth[,-c(7)]

d_tree_nq_fourth <- rpart(BigTransactions~., data = pier1.nq.fourth, method = 'class')
rpart.plot(d_tree_nq_fourth, extra = 106, type = 5, tweak = 1.05)