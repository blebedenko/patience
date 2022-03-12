# testing file
library(tictoc)
library(tidyverse)
library(simsalapar)
library(parallel)
library(patience)
library(pbapply)
library(doParallel)
patience::makeSimFilesAWX



# Installation ------------------------------------------------------------

# devtools::install_github("blebedenko/patience")
library(patience)
library(tidyverse)
library(parallel)
library(pbapply)
library(doParallel)
library(tictoc)
Sys.time()
makeAWXDirectories()
4# Simulation --------------------------------------------------------------


eta <- 1
mu <- 1
s <- 1
# EB (Job size epxectation)eta/mu
n <- 1e5
gamma <- 1
lambda_0 <- 10
theta <- 1
(rho <- (gamma + lambda_0/2) * eta / (s*mu))

r3 <- resSimBIG(n_total = 1e5,gamma = gamma,lambda_0 = lambda_0,theta = theta,s = s,eta = eta,
                mu = mu) %>% tik

r4 <- patience::resSimBIG(n_total = 1e5,gamma = gamma,lambda_0 = lambda_0,theta = theta,s = s,eta = eta,
                mu = mu) %>% tik

