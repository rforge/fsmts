fsGLASSO.run <- function(mts, maxLag, rho) {
  k<-ncol(mts)
  res<-matrix(0, k*maxLag, k)
  for (i in 1:k){
    dat <- composeYX(mts, i, maxLag)
    dat.cov<-cor(dat)
    gl<-glasso(dat.cov, rho=rho)
    links<-gl$w[1,-1]
    res[,i] <- links
  }
  return (res)
}

fsGLASSO <- list(
  name="Local graphical LASSO (correlation matrix)",
  run = fsGLASSO.run,
  functions = c(),
  packages = c('glasso')
)
