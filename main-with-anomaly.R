source("my_kmeans_library.R")
source("my_kmeans.R")

cat("main start ")
IRIS <- read.csv("IRIS-anomaly.csv", header=FALSE)
par(mar=c(1,1,1,1))
x11()
plot(IRIS[,c(3,4)], col= IRIS$V5, main="original plot", pch=20, cex=2)
IRISts=IRIS[,1:4]
labels=matrix(nrow=nrow(IRISts),ncol=1)
IRISts=cbind(IRISts,labels)
max_iter=7
#A. For K=3. Also show the initial cluster, intermediate cluster(any 2) and the final cluster formed.
ptm <- proc.time()
K=3
x11() # for plotting in different window
par(mfrow=c(2,4))
J=k_means(IRISts,K,max_iter)
x11()
proc.time() - ptm
#B. Find the optimal value of K (ranging from 2 to 12) for which the error is minimized and plot the graph showing the error curve obtained on different values of K.
all_J=c()
for(j in 2:12)
{
  par(mfrow=c(2,4))
  tmp=k_means(IRISts,j,max_iter) # plot with my implementation of kmeans
  
  op=kmeans(IRIS[,1:4],j,iter.max = 7,nstart=5)  # plot with inbuilt kmeans
  all_J=c(all_J,op$tot.withinss)              
}
par(mfrow=c(1,1))
x11()
plot(c(2:12),all_J,type="o",xlab="K",ylab="distortion function")
cat(" main end")