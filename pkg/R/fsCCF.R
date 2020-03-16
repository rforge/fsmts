fsCCF.run <- function(mts, maxLag) {
  n <- ncol(mts)
  res <- matrix(0, n*maxLag, n)
  for (i in 1:n){
    for (j in i:n){
        corVals <- stats::ccf(mts[,i],mts[,j], lag.max=maxLag, plot=F)
        res[j+(0:(maxLag-1))*n, i] <- rev(corVals$acf[1:maxLag])
        res[i+(0:(maxLag-1))*n, j] <- corVals$acf[(maxLag+2):(2*maxLag+1)]
    }
  }
  res <- fsNames(res, mts, maxLag)
  return (res)
}

#' @export
fsCCF <- list(
  name="Cross-correlation based feature selection",
  run = fsCCF.run,
  functions = c(),
  packages = c()
)
