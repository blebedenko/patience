# testing file
library(tictoc)
library(tidyverse)
library(simsalapar)
library(parallel)
library(filenamer)
library(patience)
library(pbapply)
library(doParallel)




# Installation ------------------------------------------------------------

devtools::install_github("blebedenko/patience")
library(patience)
library(tidyverse)
library(parallel)
library(doParallel)

patience::makeAWXDirectories()
# Simulation --------------------------------------------------------------


eta <- 1
mu <- 1
s <- 1
# EB (Job size epxectation)eta/mu
n <- 10
gamma <- 1
lambda_0 <- 10
theta <- 1
(rho <- (gamma + lambda_0/2) * eta / (s*mu))

PARAMS <- c(gamma = gamma,lambda_0=lambda_0,theta = theta)

tic()
RES <- resSimCosine(n=n,gamma = gamma,lambda_0 = lambda_0,theta = theta,s = s,eta = eta,mu = mu)
toc()

AWX <-RES2AWX(RES)
undebug(resSimCosine.Initial)
RES2 <- resSimCosine.Initial(initial_RES = RES,n=n,gamma = gamma,lambda_0 = lambda_0,theta = theta,s = s,eta = eta,mu = mu)
RES2
RES$Q.trans
RES2$Q.trans
# Data Generation ---------------------------------------------------------
dir()

eta <- 1
mu <- 1
s <- 6
# EB (Job size epxectation)eta/mu
n <- 2000
gamma <- 1
lambda_0 <- 10
theta <- 1
(rho <- (gamma + lambda_0/2) * eta / (s*mu))

makeSimFilesAWX(dir_path =  "./s=1/",
                n_cores = 4,
                N_files = 10,
                n_obs = 100,
                s = 1,
                gamma = gamma,
                lambda_0 = lambda_0,
                theta = theta,
                eta = eta,
                mu = mu
                )

makeSimFilesAWX(dir_path =  "./s=2/",
                n_cores = 4,
                N_files = 10,
                n_obs = 100,
                s = 2,
                gamma = gamma,
                lambda_0 = lambda_0,
                theta = theta,
                eta = eta,
                mu = mu
)
