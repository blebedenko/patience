# launch filemaking script
# set wd in a directory where you want subdirectories for each s
library(tictoc)
library(tidyverse)
library(simsalapar)
library(parallel)
library(patience)
library(pbapply)

# lambda = 10 gamma = 40 theta = 1.5
Target_files <- 2000 # number of realization per folder

N_files <- 10 # number of files per iteration


eta <- 1
mu <- 1
# EB (Job size epxectation)eta/mu
n <- 100000
gamma <- 10 # bigger periodic input
lambda_0 <- 10
theta <- 2.5
PARAMS <-c(gamma=gamma,lambda_0=lambda_0,theta=theta)


# s = 5 -------------------------------------------------------------------

s <- 3
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


# s = 10 -------------------------------------------------------------------


s <- 10

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


# s = 20 -------------------------------------------------------------------


s <- 20

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
