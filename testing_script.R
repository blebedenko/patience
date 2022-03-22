# testing file
library(tictoc)
library(tidyverse)
library(simsalapar)
library(parallel)
library(patience)
library(pbapply)
library(doParallel)





# Estimation --------------------------------------------------------------
library(tictoc)
library(tidyverse)
library(simsalapar)
library(parallel)
library(patience)
library(pbapply)
library(doParallel)
setwd("~/patience/results/c2 - Copy/n=10^4/")
gamma <- 40
lambda_0 <- 10
theta <- 2.5
PARAMS <- c(gamma,lambda_0,theta)
grid <- makeParGrid(params = PARAMS,
                    spans = c(0.8,0.8,0.8),
                    grid.sizes = c(25,25,10))


estimateALL.parallel(grid = grid,PARAMS = PARAMS)

likGridSummary("C2")
mleALL(PARAMS = PARAMS,scenario = "C2")

a
eta <- 1
mu <- 1
s <- 1
# EB (Job size epxectation)eta/mu
n <- 5e4
gamma <- 4
lambda_0 <- 10
theta <- 2.5
(rho <- (gamma + lambda_0 / 2) * eta / (s * mu))

# Simulation --------------------------------------------------------------


PARAMS <- c(gamma,lambda_0,theta)

R <- resSimCosine(n = 5,
                  gamma = gamma,
                  lambda_0 = lambda_0,
                  theta = theta,
                  s = s,
                  eta = eta,
                  mu = mu)



patience::makeAWXDirectories()


library(tictoc)
library(tidyverse)
library(parallel)
library(patience)
library(pbapply)
library(doParallel)


# Parallel ---------
#cl <- makeCluster(length(SS)) # a core for each server number
cl <- makeCluster(8)
registerDoParallel(cl)
gamma <- 40
lambda_0 <- 10
theta <- 2.5
PARAMS <- c(gamma,lambda_0,theta)
grid <- makeParGrid(params = PARAMS,
                    spans = c(0.8,0.8,0.8),
                    grid.sizes = c(25,25,10))

setwd("~/patience/results/c2 - Copy/n=10^4")
estimateALL.parallel(grid = grid, PARAMS = PARAMS)
stopCluster(cl)
