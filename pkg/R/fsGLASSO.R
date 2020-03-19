fsGLASSO <- function(mts, max.lag, rho, absolute = TRUE, show.progress = TRUE) {
  k<-ncol(mts)
  res<-matrix(0, k*max.lag, k)
  for (i in 1:k){
    dat <- composeYX(mts, i, max.lag)
    dat.cov<-stats::cor(dat)
    gl<-glasso::glasso(dat.cov, rho=rho, penalize.diagonal=FALSE)
    links<-gl$wi[1,-1]
    res[,i] <- links
    if (show.progress) svMisc::progress(100*i/k)
  }
  if (absolute) res <- abs(res)
  return (res)
}
