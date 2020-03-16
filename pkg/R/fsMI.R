fsMI.run <- function(mts, maxLag) {
  k<-ncol(mts)
  res<-matrix(0, k*maxLag, k)
  res <- fsNames(res, mts, maxLag)
  for (i in 1:k){
    dat <- composeYX(mts, i, maxLag)
    mi <- mpmi::cminjk(dat)[1,-1]
    res[,i] <- mi
  }
  return (res)
}

#' @export
fsMI <- list(
  name="Mutual information",
  run = fsMI.run,
  functions = c(),
  packages = c('mpmi')
)
