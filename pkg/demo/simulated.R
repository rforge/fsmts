if (!require(sparsevar)) install.packages("sparsevar")
require(sparsevar)

k <- 20
genM <- function(k, sparsity_lag = 0.2){
  nonzero <- round(sparsity_lag*k*k)
  m = diag(k)*runif(k*k, min = 0, max=0.1)
  size <- k
  samples <- nonzero
  vals <- sample.int(size ^ 2, samples)
  pair<-cbind(vals %/% size + 1, vals %% size+1)
  pair[pair>k]<-k
  m[pair] <- runif(nonzero, min = -0.1, max=0.1)
  return(m)
}
set.seed(9)
pars <- list(
  l1 = genM(k),
  l2 = genM(k),
  l3 = genM(k)
)
res<-data.frame()
for (i in pars){
  res <-rbind(res,i)
}
mreal<-as.matrix(res)
image(abs(mreal))

mts.sim <- simulateVAR(N=k, p=3, nobs=200,fixedMat=pars)
data <- mts.sim$series
colnames(data)<-paste0("V",1:ncol(data))

data <- scale(data)


m1<-fsIndependent$run(data, maxLag=3)
image(m1)

m2<-fsCCF$run(data, maxLag=3)
image(abs(m2))

shortest <- matrix(rexp(k*k, rate = 0.2), nrow=k)
shortest <- shortest-diag(k)*shortest
m3<-fsDistance$run(data, maxLag=3, shortest = shortest, step = 1)
image(abs(m3))

m4<-fsGLASSO$run(data, maxLag=3, rho = 0.05)
image(abs(m4))

m5<-fsLARS$run(data, maxLag=3)
image(abs(m5))

m7<-fsRF$run(data, maxLag=3, mode="system-wide")
image(abs(m7))

m8<-fsRF$run(data, maxLag=3, mode="independent")
image(abs(m8))

m9<-fsMI$run(data, maxLag=3)
image(abs(m9))

mE <- ensembleFS(list(m1,m2,m3,m4,m5,m7,m8,m9), threshold = 0.2)
image(mE)

mR <- cutoff(abs(mreal), 0.2)
image(mR)


fs.sparsity(mR)
fs.sparsity(m2)
fs.sparsity(m3)
fs.sparsity(m8)

th<-0.2
fs.match(mR, cutoff(m1,th))
fs.match(mR, cutoff(m2,th))
fs.match(mR, cutoff(m3,th))
fs.match(mR, cutoff(m4,th))
fs.match(mR, cutoff(m6,th))
fs.match(mR, cutoff(m7,th))
fs.match(mR, cutoff(m8,th))
fs.match(mR, cutoff(m9,th))
fs.match(mR, mE)

fs.match.in(cutoff(m1,th), mR)
fs.match.in(cutoff(m2,th), mR)
fs.match.in(cutoff(m3,th), mR)
fs.match.in(cutoff(m4,th), mR)
fs.match.in(cutoff(m6,th), mR)
fs.match.in(cutoff(m7,th), mR)
fs.match.in(cutoff(m8,th), mR)
fs.match.in(cutoff(m9,th), mR)
fs.match.in(mE, mR)
