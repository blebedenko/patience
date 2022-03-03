library(tictoc)
library(tidyverse)
library(simsalapar)
library(parallel)
library(filenamer)
library(PPQ)
library(pbapply)
library(doSNOW)


# make the grid DOUBLE CHECK PARAMETERS
eta <- 1
mu <- 1
gamma <- 40
lambda_0 <- 10
theta <- 2.5
PARAMS <- c(gamma=gamma,lambda_0=lambda_0,theta=theta)
(rho <- (gamma + lambda_0) * eta / (s*mu))
spans <-  c(0.99,0.99,0.5)
grid.sizes <- c(36,36,9)
grid <- makeParGrid(params = PARAMS,spans = spans,grid.sizes = grid.sizes)

iterations <- nrow(grid)
# Average likelihood - make tables with the negative log likelihood ----
# set wd to s = 5

## no. servers = 5


## compute the likelihood for that grid ----

nservers <- 5
setwd("~/patience/Extras/average_likelihood/gamma = 40 lambda = 10 theta = 1.5/realizations for s=5")

L <- pblapply(dir(),read.csv)

cl <- makeCluster(detectCores()-1)
registerDoSNOW(cl)

tic()
ans <- foreach(i = 1:iterations,.combine = cbind,.packages = "patience") %dopar% {
  negLL <- meanLogLikelihoodFromList(gamma = grid$gamma[i], lambda_0 = grid$lambda_0[i] ,theta = grid$theta[i],res_list = L)
  c(gamma = grid$gamma[i], lambda_0 = grid$lambda_0[i] ,theta = grid$theta[i], negLogLik = negLL)
}
toc()
stopCluster(cl)
ans <- t(ans)
## write the results
tit <- paste0("big grid results for s=",nservers,".csv")
write.csv(ans,tit,row.names = FALSE)
mles <- sapply(L,mleBoris,PARAMS = PARAMS)
mles <- data.frame(t(mles))
write.csv(mles,paste0("mles for s=",nservers,".csv"),row.names = FALSE)

rm(L)
rm(ans)


nservers <- 10
setwd("~/patience/Extras/average_likelihood/gamma = 40 lambda = 10 theta = 1.5/realizations for s=10")
L <- pblapply(dir(),read.csv)
cl <- makeCluster(detectCores()-1)
registerDoSNOW(cl)
pb <- txtProgressBar(max = iterations, style = 2)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
tic()
ans <- foreach(i = 1:iterations,.combine = cbind,.packages = "patience") %dopar% {
  negLL <- meanLogLikelihoodFromList(gamma = grid$gamma[i], lambda_0 = grid$lambda_0[i] ,theta = grid$theta[i],res_list = L)
  c(gamma = grid$gamma[i], lambda_0 = grid$lambda_0[i] ,theta = grid$theta[i], negLogLik = negLL)
}
toc()
close(pb)
stopCluster(cl)
ans <- t(ans)
## write the results
tit <- paste0("big grid results for s=",nservers,".csv")
write.csv(ans,tit,row.names = FALSE)
mles <- sapply(L,mleBoris,PARAMS = PARAMS)
mles <- data.frame(t(mles))
write.csv(mles,paste0("mles for s=",nservers,".csv"),row.names = FALSE)

rm(L)
rm(ans)


nservers <- 20
setwd("~/patience/Extras/average_likelihood/gamma = 40 lambda = 10 theta = 1.5/realizations for s=20")#L <- pblapply(dir()[1:10],read.csv)
L <- pblapply(dir(),read.csv)
cl <- makeCluster(detectCores()-1)
registerDoSNOW(cl)
pb <- txtProgressBar(max = iterations, style = 2)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
tic()
ans <- foreach(i = 1:iterations,.combine = cbind,.packages = "patience") %dopar% {
  negLL <- meanLogLikelihoodFromList(gamma = grid$gamma[i], lambda_0 = grid$lambda_0[i] ,theta = grid$theta[i],res_list = L)
  c(gamma = grid$gamma[i], lambda_0 = grid$lambda_0[i] ,theta = grid$theta[i], negLogLik = negLL)
}
toc()
close(pb)
stopCluster(cl)
ans <- t(ans)
## write the results
tit <- paste0("big grid results for s=",nservers,".csv")
write.csv(ans,tit,row.names = FALSE)
mles <- sapply(L,mleBoris,PARAMS = PARAMS)
mles <- data.frame(t(mles))
write.csv(mles,paste0("mles for s=",nservers,".csv"),row.names = FALSE)

rm(L)
rm(ans)
