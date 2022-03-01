library(tictoc)
library(tidyverse)
library(simsalapar)
library(parallel)
library(filenamer)
library(PPQ)
library(pbapply)
library(doSNOW)

# Average likelihood - make tables with the negative log likelihood ----
# set wd to s = 5

## no. servers = 5

s <- 5
L <- readAWXFiles()

## compute the grid -----

eta <- 1
mu <- 1
gamma <- 10
lambda_0 <- 10
theta <- 2.5
PARAMS <- c(gamma=gamma,lambda_0=lambda_0,theta=theta)
(rho <- (gamma + lambda_0) * eta / (s*mu))
spans <-  c(0.99,0.99,0.5)
grid.sizes <- c(36,36,9)
grid <- makeParGrid(params = PARAMS,spans = spans,grid.sizes = grid.sizes)

## compute the likelihood for that grid ----

nservers <- 5

L <- pblapply(dir(),read.csv)
cl <- makeCluster(detectCores()-1)
registerDoSNOW(cl)
iterations <- 100# nrow(grid)
pb <- txtProgressBar(max = iterations, style = 2)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
tic()
ans <- foreach(i = 1:iterations,.combine = cbind,.packages = "PPQ") %dopar% {
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
rm(L)
rm(ans)
