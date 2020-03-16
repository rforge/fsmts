fsRF.run <- function(mts, maxLag, mode="system-wide") {
  k<-ncol(mts)
  res<-matrix(0, k*maxLag, k)
  res <- fsNames(res, mts, maxLag)
  all_features <- data.frame()
  for (i in 1:k){
    dat <- composeYX(mts, i, maxLag)
    features<-c()
    rf <- randomForest::randomForest(dat[,-1],dat[,1],importance = T)
    if (mode=="system-wide"){
      df <- as.data.frame(randomForest::importance(rf))
      df<-df[df[,1]>0,]
      df$rn<-rownames(df)
      df$s <- i
      #features<-importance(rf)%>%as.data.frame%>%rownames_to_column("rn")%>%mutate(IncMSE=`%IncMSE`)%>%filter(IncMSE>0)
      #all_features <- bind_rows(all_features, features%>%mutate(s=i))
      all_features <- rbind(all_features, df)
    }else{
      df <- as.data.frame(randomForest::importance(rf))
      df<-df[df[,1]>0,]
      df$rn<-rownames(df)
      df$s <- i
      #features<-importance(rf)%>%as.data.frame%>%rownames_to_column%>%mutate(IncMSE=`%IncMSE`)%>%filter(IncMSE>0)
      res[df$rn, i]<-df$`%IncMSE`
    }
  }
  if (mode=="system-wide"){
    #all_features<-all_features%>%filter(IncMSE>0)
    for (i in 1:nrow(all_features)){
      res[all_features[i,]$rn, all_features[i,]$s]<-all_features[i,]$`%IncMSE`
    }
  }
  return (res)
}

#' @export
fsRF <- list(
  name="Random-forest",
  run = fsRF.run,
  functions = c(),
  packages = c('randomForest')
)
