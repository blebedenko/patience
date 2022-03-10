# launch filemaking script
# set wd in a directory where you want subdirectories for each s
library(tictoc)
library(tidyverse)
library(simsalapar)
library(parallel)
library(patience)
library(pbapply)
setwd("~/patience/Extras/average_likelihood/results/gamma = 1 lambda_0 = 2 theta = 1")# lambda = 10 gamma = 40 theta = 1.5
Target_files <- 1000 # number of realization per folder

N_files <- 10 # number of files per iteration

eta <- 1
mu <- 1
# EB (Job size epxectation)eta/mu
n <- 100000
gamma <- 1 # smaller  periodic input
lambda_0 <- 2
theta <-  1
PARAMS <-c(gamma=gamma,lambda_0=lambda_0,theta=theta)


# s = 1 -------------------------------------------------------------------

s <- 1
(rho <-   (0:2*gamma/2 + lambda_0) / (s*eta/mu))
path <-  paste0("./realizations for s=",s)
dir.create(path=path)
setwd(path)
# the generation function:
tic()
while (length(dir()) < Target_files)
  makeSimFilesAWX(N_files = N_files,
                  n_obs = n,
                  gamma = gamma,
                  lambda_0 = lambda_0,
                  theta = theta,
                  s = s,
                  eta = eta,
                  mu = mu)
toc()
rm(s)
setwd("..") #one folder up


# s = 2 -------------------------------------------------------------------


s <- 2
(rho <-   (0:2*gamma/2 + lambda_0) / (s*eta/mu))

path <-  paste0("./realizations for s=",s)
dir.create(path=path)
setwd(path)
# the generation function:
while (length(dir()) < Target_files)
  makeSimFilesAWX(N_files = N_files,
                  n_obs = n,
                  gamma = gamma,
                  lambda_0 = lambda_0,
                  theta = theta,
                  s = s,
                  eta = eta,
                  mu = mu)

rm(s)
setwd("..") #one folder up


# s = 3 -------------------------------------------------------------------


s <- 3
(rho <-   (0:2*gamma/2 + lambda_0) / (s*eta/mu))

path <-  paste0("./realizations for s=",s)
dir.create(path=path)
setwd(path)
# the generation function:
while (length(dir()) < Target_files)
  makeSimFilesAWX(N_files = N_files,
                  n_obs = n,
                  gamma = gamma,
                  lambda_0 = lambda_0,
                  theta = theta,
                  s = s,
                  eta = eta,
                  mu = mu)

rm(s)
setwd("..") #one folder up



# s = 5 -------------------------------------------------------------------


s <- 5
(rho <-   (0:2*gamma/2 + lambda_0) / (s*eta/mu))

path <-  paste0("./realizations for s=",s)
dir.create(path=path)
setwd(path)
# the generation function:
while (length(dir()) < Target_files)
  makeSimFilesAWX(N_files = N_files,
                  n_obs = n,
                  gamma = gamma,
                  lambda_0 = lambda_0,
                  theta = theta,
                  s = s,
                  eta = eta,
                  mu = mu)

rm(s)
setwd("..") #one folder up
