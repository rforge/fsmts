fsIndependent.run <- function(mts, maxLag, rho) {
  k<-ncol(mts)
  res<-data.frame()
  for (l in 1:maxLag){
    res<-rbind(res,diag(k))
  }
  r<-as.matrix(res)
  r <- fsNames(r, mts, maxLag)
  return (r)
}

#' @export
fsIndependent <- list(
  name="Independent time series",
  run = fsIndependent.run,
  functions = c(),
  packages = c()
)
