# testing file
library(tictoc)
library(tidyverse)
library(simsalapar)
library(parallel)
library(patience)
library(pbapply)
library(doParallel)
L <- makeAWXDirectories()



eta <- 1
mu <- 1
s <- 1
# EB (Job size epxectation)eta/mu
n <- 1e5
gamma <- 40
lambda_0 <- 10
theta <- 2.5
(rho <- (gamma + lambda_0 / 2) * eta / (s * mu))
PARAMS <- c(gamma,lambda_0,theta)
R <- resSimBIG(n_thousands = n %/% 1000L,
               gamma = gamma,
               lambda_0 = lambda_0,
               theta = theta,
               s = s,
               eta = eta,
               mu = mu
               )



grid <- makeParGrid(params = PARAMS,
                    spans = c(0.7,0.7,0.5),grid.sizes = c(25,25,9))

setwd("~/patience/results/C2 n=10^5")
estimateALL(grid = grid,PARAMS = PARAMS)


**

