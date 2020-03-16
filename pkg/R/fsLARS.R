fsLARS.run <- function(mts, maxLag) {
  k<-ncol(mts)
  res<-matrix(0, k*maxLag, k)
  res<-fsNames(res, mts, maxLag)
  for (i in 1:k){
    dat <- composeYX(mts, i, maxLag)
    reg <- lars(dat[,-1],dat[,1],type="lar")
    coef1 <- reg$beta
    s1 <- apply(abs(coef1), 1, sum)
    s1 <- s1/max(s1)
    s1<-s1[-1]
    s2 <- apply(abs(coef1), 2, function(c)sum(c!=0))
    sn<-names(sort(s2,  decreasing = T))
    res[sn,i] <- 1-as.vector(s1)
  }
  return (res)
}

fsLARS <- list(
  name="Least-angle regression",
  run = fsLARS.run,
  functions = c(),
  packages = c('lars')
)
