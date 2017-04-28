k_means <- function(x,K,max_iter) 
{
    #1. choose initial clusters
        initialcentroids=x[sample(nrow(x),K),]
    newcentroids=initialcentroids
    Jprev=0;
    for( cnt in 1:max_iter)
    {
      #2. choose cluster for each point
    
    t=choosecluster(x,newcentroids) 
    x=t[[1]]
    J=t[[2]]  # distortion function's value
    #cat("\ndistortion value=",J,"\n")
    #3. calc new centroids
    
    newcentroids=calcnewcentroids(x,K) #ignore the last column
    
    #4. 
    plot(x[,c(3,4)], col =(x[,5] +1) , main= paste(c("K-Means step ",cnt," result with ",K," clusters"), collapse = " "), pch=20, cex=2)
    points(newcentroids[,3],newcentroids[,4],cex=2,pch=23,col="black",bg="black")
    
    if(abs(Jprev-J)<0.05)
      break
    
    Jprev=J
  }  
  plot(x[,c(3,4)], col =(x[,5] +1) , main=paste(c("final K-Means result with ",K," clusters"),collapse=" "), pch=20, cex=2)
  points(newcentroids[,3],newcentroids[,4],cex=2,pch=23,col="black",bg="black")
  cat("\ndistortion value=",J,"with K=",K,"\n")
  return(J)
}

