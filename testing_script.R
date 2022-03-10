# testing file
library(tictoc)
library(tidyverse)
library(simsalapar)
library(parallel)
library(filenamer)
library(patience)
library(pbapply)
library(doParallel)


# Simulation --------------------------------------------------------------


eta <- 1
mu <- 1
s <- 6
# EB (Job size epxectation)eta/mu
n <- 2000
gamma <- 1
lambda_0 <- 10
theta <- 1
(rho <- (gamma + lambda_0/2) * eta / (s*mu))

PARAMS <- c(gamma = gamma,lambda_0=lambda_0,theta = theta)

tic()
RES <- resSimCosine(n=n,gamma = gamma,lambda_0 = lambda_0,theta = theta,s = s,eta = eta,mu = mu)
toc()
AWX <-RES2AWX(RES)

y <- RES$Q.trans
y[y==0]
