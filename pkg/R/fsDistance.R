fsDistance.run <- function(mts, maxLag, shortest, step=1) {
  n <- ncol(mts)
  lagMatrix <- round(shortest / step)
  lagMatrix[is.infinite(lagMatrix)]<-0
  res<-data.frame()
  for (i in 1:maxLag){
    res<-rbind(res,ifelse(lagMatrix==i,1,0))
  }
  res <- fsNames(as.matrix(res), mts, maxLag)
  return (res)
}
fsDistance <- list(
  name="Distance-based feature selection",
  run = fsDistance.run,
  functions = c(),
  packages = c()
)
