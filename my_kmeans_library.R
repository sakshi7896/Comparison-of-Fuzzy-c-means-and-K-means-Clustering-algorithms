choosecluster <- function(x,centroids) 
{
  d=c()                           #list of dist of points wrt to clusters'centroids
  #cat("centroids passed",View(centroids))  
  for( i in 1:nrow(x))
  {
    dist=c() 
    for(j in 1:nrow(centroids))
    {
      dist<-c(dist,calcdist(x[i,],centroids[j,]))
      #cat("dist",dist,"\n")
      #cat("cluster no",j)
    }
    x[i,5]=which.min(dist)
    d<-c(d,(min(dist))^2)    #SSE : sum of square errors
  }
   #cat("end choose cluster ")  
   return(list(x,sum(d)))
}  
    
calcdist <- function(x,y) 
{
  #cat("x[1:8]=",View(x),"y[1:8]=",View(y[1:8]))
  #cat("x[1:8]-y[1:8]=")
  #x[1:8]-y[1:8]
  #cat("\nx[1:8]-y[1:8]^2=")
  #(x[1:8]-y[1:8])^2
  #cat("entered calcdist",sqrt(rowSums((x[1:8]-y[1:8])^2)),"\n")
  return(sqrt(rowSums((x[1:4]-y[1:4])^2)))
}

calcnewcentroids<- function(x,K)
{
  #cat("entered calcnewcentroids")
  
  newcentroids=matrix(nrow=K,ncol=5)
  
  for(i in 1:K)
  {
    temp=x[x[,5]==i,]  
    A=c()
    for( j in 1:ncol(temp))
    { 
      A<-c(A,mean(temp[,j]))
    }
    
    
    newcentroids[i,]=A    
  }
  #cat("end newcentroid")
  return(newcentroids)
}