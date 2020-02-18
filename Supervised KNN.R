setwd("C:\\Users\\hahas\\Desktop\\BA\\Hands-on\\6. Supervised KNN")
iris <- read.csv("iris.csv")
iris <- iris[,-1]
View(iris)

# Normalization
nor <- function(x){
  (x-min(x)) / (max(x)-min(x))
}
iris.norm<- as.data.frame(lapply(iris[,c(1,2,3,4)],nor))
View(iris.norm)

# Partition
set.seed(1)
trainIds<-sample(1:dim(iris.norm)[1],0.6*dim(iris.norm)[1])
traindata <- iris.norm[trainIds,]
validata <- iris.norm[-trainIds,] 

# Partition2 for kNN
iris.category <- iris[trainIds,5]
iris.category2 <- iris[-trainIds,5]

# modeling (KNN)
library(class) 
pr <- knn(traindata,validata,cl=iris.category,k=13)
table(pr,iris.category2)
