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
setwd("~/patience/results/C3/dummy_small")
gamma <- 1
lambda_0 <- 2
theta <- 1
PARAMS <- c(gamma,lambda_0,theta)
grid <- makeParGrid(params = PARAMS,
                    spans = c(0.8,0.8,0.8),
                    grid.sizes = c(25,25,10))

estimateALL(grid = grid,PARAMS = PARAMS)
likGridSummary("C3")
mleALL(PARAMS = PARAMS,scenario = "C3")

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
