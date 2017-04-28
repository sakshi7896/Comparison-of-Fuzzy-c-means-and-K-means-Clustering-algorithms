#install.packages("scatterplot3d")
#library("scatterplot3d")
library(e1071)

ptm <- proc.time()
iris <- read.csv("IRIS-fuzzy-anomaly.csv", header=FALSE)
for (i in 1:7)
{
  result <- cmeans(iris, 3, i,verbose=TRUE,dist="euclidean", method="cmeans",m=2)
  print(result)
  x11(); 
  plot(iris[,1], iris[,2], col=result$cluster)
  points(result$centers[,c(1,2)], col=1:3, pch=8, cex=2)
  result$membership[1:3,]
}
#1           2         3
#[1,] 0.001072018 0.002304389 0.9966236
#[2,] 0.007498458 0.016651044 0.9758505
#[3,] 0.006414909 0.013760502 0.9798246
#table(iris$Species, result$cluster)

proc.time() - ptm
#1  2  3
#setosa      0  0 50
#versicolor  3 47  0
#virginica  37 13  0
##############################################################




