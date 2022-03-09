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
n <- 20000
gamma <- 1
lambda_0 <- 10
theta <- 1
(rho <- (gamma + lambda_0/2) * eta / (s*mu))

PARAMS <- c(gamma = gamma,lambda_0=lambda_0,theta = theta)

tic()
RES <- resSimCosine(n=n,gamma = gamma,lambda_0 = lambda_0,theta = theta,s = s,eta = eta,mu = mu)
toc()
dat <- data.frame(A=RES$Aj,W=RES$Wj,X=RES$Xj)

patience::negLogLikelihoodMean.KnownArrival(theta = 1:10,params = c(gamma,lambda_0),dati = dat)

curve(negLogLikelihoodMean.KnownArrival(theta.vec = x, params = PARAMS[1:2],dati = dat),from = 0,to = 5)
mle <- optimize(negLogLikelihoodMean.KnownArrival,interval = c(0,5),params = PARAMS[1:2],dati = dat)
mle


