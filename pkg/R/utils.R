fsNames <- function(res, mts, maxLag){
  n <- ncol(mts)
  vars<-colnames(mts)
  if (is.null(vars)) vars <- paste0("V",1:n)
  colnames(res)<-vars
  rnames<-c()
  for (k in 1:maxLag){
    rnames <- append(rnames, paste0(vars,".l",k))
  }
  rownames(res)<-rnames
  return (res)
}

#' @export
fs.sparsity <- function(m){
  return(length(m[m>0])/(ncol(m)*nrow(m)))
}

#' @export
fs.match <- function(m1, m2){
  if (nrow(m1)==nrow(m2) && ncol(m1)==ncol(m2) ){
    return(sum(m1==m2)/(ncol(m1)*nrow(m1)))
  }
  return(0)
}

composeYX <- function(mts, i, maxLag){
  y <- mts[,i]
  x <- mts
  cnames<-colnames(mts)[i]
  dat<-NULL
  for (l in 1:maxLag){
    if(is.null(dat)){
      dat <- x[-c(1:l),]
    }else{
      dat <- cbind(dat[-nrow(dat),],x[-c(1:l),])
    }
    cnames <- c(cnames,paste0(colnames(x),".l",l))
  }
  dat <- cbind(y[-((length(y)-maxLag+1):length(y))],dat)
  colnames(dat)<-cnames
  return(dat)
}

matrixInvRank <-function(m){
  max <- ncol(m)*nrow(m)
  res<-matrix(max-rank(m)+1, ncol=ncol(m))
  colnames(res) <- colnames(m)
  rownames(res) <- rownames(m)
  return(res)
}

#' @export
ensembleFS <- function(ms, threshold){
  res<-NULL
  for (m in ms){
    if (is.null(res)){
      res <- matrixInvRank(m)
    } else{
      res <- res + matrixInvRank(m)
    }
  }
  th <- stats::quantile(res, threshold)
  res <- ifelse(res>th,0,1)
  return(res)
}
#' @export
cutoff <- function(m, threshold){
  m<-matrixInvRank(m)
  th <- stats::quantile(m, threshold)
  res <- ifelse(m>th,0,1)
  return(res)
}

#' @export
fs.match.in <- function(est, real){
  if (nrow(est)==nrow(real) && ncol(est)==ncol(real)){
    index <- sum(est+real==2)/sum(real==1)
    return(index)
  }
  return(0)
}
