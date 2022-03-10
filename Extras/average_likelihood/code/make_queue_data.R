# launch filemaking script
# set wd in a directory where you want subdirectories for each s
library(tictoc)
library(tidyverse)
library(simsalapar)
library(parallel)
library(patience)
library(pbapply)
<<<<<<< HEAD
setwd("~/patience/Extras/average_likelihood/results/gamma = 1 lambda_0 = 2 theta = 1")# lambda = 10 gamma = 40 theta = 1.5
Target_files <- 1000 # number of realization per folder
=======

# lambda = 10 gamma = 40 theta = 1.5
Target_files <- 500 # number of realization per folder
>>>>>>> ec5635570904e47e6f626a25dcbcc9ec78830570

N_files <- 60 # number of files per iteration

eta <- 1
mu <- 1
# EB (Job size epxectation)eta/mu
n <- 100000
<<<<<<< HEAD
gamma <- 1 # smaller  periodic input
lambda_0 <- 2
theta <-  1
=======
gamma <- 1 # bigger periodic input
lambda_0 <- 2
theta <- 1
>>>>>>> ec5635570904e47e6f626a25dcbcc9ec78830570
PARAMS <-c(gamma=gamma,lambda_0=lambda_0,theta=theta)


# s = 1 -------------------------------------------------------------------

s <- 1
<<<<<<< HEAD
(rho <-   (0:2*gamma/2 + lambda_0) / (s*eta/mu))
=======
>>>>>>> ec5635570904e47e6f626a25dcbcc9ec78830570
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


<<<<<<< HEAD
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
=======


# s = 2 -------------------------------------------------------------------
>>>>>>> ec5635570904e47e6f626a25dcbcc9ec78830570

s <- 2
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

# s=3 ---------------------------------------------------------------------


s <- 1
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


<<<<<<< HEAD

=======
>>>>>>> ec5635570904e47e6f626a25dcbcc9ec78830570
# s = 5 -------------------------------------------------------------------


s <- 5
<<<<<<< HEAD
(rho <-   (0:2*gamma/2 + lambda_0) / (s*eta/mu))
=======
>>>>>>> ec5635570904e47e6f626a25dcbcc9ec78830570

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

