fsRF <- function(mts, max.lag, show.progress = TRUE) {
  k<-ncol(mts)
  res<-matrix(0, k*max.lag, k)
  res <- fsNames(res, mts, max.lag)
  all_features <- data.frame()
  for (i in 1:k){
    dat <- composeYX(mts, i, max.lag)
    features<-c()
    rf <- randomForest::randomForest(dat[,-1],dat[,1],importance = T)
    df <- as.data.frame(randomForest::importance(rf))
    df<-df[df$`%IncMSE`>0,]
    df$rn<-rownames(df)
    df$s <- i
    res[df$rn, i]<-df$`%IncMSE`
    if (show.progress) svMisc::progress(100*i/k)
  }
  return (res)
}
