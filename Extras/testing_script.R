# testing file
library(tictoc)
library(tidyverse)
library(simsalapar)
library(parallel)
library(filenamer)
library(patience)
library(pbapply)



# Simulation --------------------------------------------------------------


tic()
eta <- 1
mu <- 1
s <- 15
# EB (Job size epxectation)eta/mu
n <- 50000
gamma <- 50
lambda_0 <- 10
theta <- 2.5
(rho <- (gamma + lambda_0) * eta / (s*mu))

PARAMS <- c(gamma = gamma,lambda_0=lambda_0,theta = theta)

tic()
RES <- resSimCosine(n=n,gamma = gamma,lambda_0 = lambda_0,theta = theta,s = s,eta = eta,mu = mu)
toc()
dat <- data.frame(A=RES$Aj,W=RES$Wj,X=RES$Xj)



opt <-
  optim(PARAMS, # note that PARAMS is temporary
        fn = negLogLikelihoodMean,
        lower = PARAMS/100,
        upper = PARAMS*100,
        method = "L-BFGS-B",
        gr = gradNegLogLikelihoodMean,
        dati = dat)
opt
opt$par
RES$Pl


grid_opt <- NMOF::gridSearch(negLogLikelihoodMean,
                 lower = PARAMS/2,
                 upper = PARAMS*2,
                 n = 20,

                dati = dat
                 )
grid_opt$minlevels

dfoptim::nmkb(PARAMS, # note that PARAMS is temporary
              fn = negLogLikelihoodFull,
              lower = PARAMS/2,
              upper = PARAMS*2,RES=RES)

minqa::bobyqa(PARAMS, # note that PARAMS is temporary
              fn = negLogLikelihoodFull,
              lower = PARAMS/2,
              upper = PARAMS*2,RES=RES,)

grid_opt$minlevels



# make simulation files ----

library(filenamer)
library(PPQ)
setwd("simulation_results")
#makeSimFilesAWX(N_files = 3600, n_obs = 10000, gamma = 10, lambda_0 = 10, theta = 2.5, s = 10, eta = 1, mu = 1)


# the i-th likelihood function
ithLL <- function(dati,gamma,lambda_0,theta){
  A <- dati$A
  W <- dati$W
  X <- dati$X
  A_i = A[-1]
  A_tilde <-c(0,cumsum(A))
  A_tilde <- A_tilde[-length(A_tilde)]
  A_tilde_i = cumsum(A_i)
  W_i = W[-1]
  w_i = W[-length(W)]
  x_i = X[-length(X)]

  logLikelihood <-
    log(gamma/2 + lambda_0 + (gamma*cos(2*pi*A_tilde_i))/2) +
    log(exp(-W_i*theta)) +
    (gamma*exp(-theta*(w_i + x_i))*(2*pi*sin(2*pi*A_tilde_i) + theta*cos(2*pi*A_tilde_i) - 2*pi*sin(2*pi*(A_i + A_tilde_i))*exp(A_i*theta) - theta*exp(A_i*theta)*cos(2*pi*(A_i + A_tilde_i))))/
    (2*(4*pi^2 + theta^2)) - (lambda_0*exp(-theta*(w_i + x_i))*(exp(A_i*theta) - 1))/theta - (gamma*exp(-theta*(w_i + x_i))*(exp(A_i*theta) - 1))/(2*theta)

  return(-mean(logLikelihood))
}

library(pbapply)
L <- pblapply(dir()[1:10], read.csv)
mapply(ithLL, L, gamma = 10, lambda_0 = 10, theta = 2.5)



dati <- L[[2]]
mleBoris(dati,PARAMS)
tic()
mles <- sapply(L,mleBoris,PARAMS = PARAMS)
mles <- data.frame(t(mles))
write.csv(mles,paste0("mles for s=",nservers,".csv"),row.names = FALSE)
toc()
