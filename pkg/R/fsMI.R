fsMI <- function(mts, max.lag, show.progress = TRUE) {
  k<-ncol(mts)
  res<-matrix(0, k*max.lag, k)
  res <- fsNames(res, mts, max.lag)
  for (i in 1:k){
    dat <- composeYX(mts, i, max.lag)
    mi <- mpmi::cminjk(dat)[1,-1]
    res[,i] <- mi
    if (show.progress) svMisc::progress(100*i/k)
  }
  return (res)
}
