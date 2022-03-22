# testing file
library(tictoc)
library(tidyverse)
library(simsalapar)
library(parallel)
library(patience)
library(pbapply)
library(doParallel)



eta <- 1
mu <- 1
s <- 1
# EB (Job size epxectation)eta/mu
n <- 5e4
gamma <- 4
lambda_0 <- 10
theta <- 2.5
(rho <- (gamma + lambda_0 / 2) * eta / (s * mu))

PARAMS <- c(gamma,lambda_0,theta)

R <- resSimCosine(n = 5,
               gamma = gamma,
               lambda_0 = lambda_0,
               theta = theta,
               s = s,
               eta = eta,
               mu = mu
)
(A <- R$Aj)
(A_tilde <- cumsum(R$Aj))
(IT <- R$IT.times)
(Qt <- R$Q.trans)

cumsum(A)
cumsum(IT)
Qt
pltQueueAtArrivals(A,Qt,IT)
pltOneCycle(RES = R,start_time = 1)
length(IT)
length(Qt)

R$Xj#n
R$Wj#n
R$Qj#n
R$
R$Q.trans

R <- resSimBIG(n_thousands = n %/% 1000L,
               gamma = gamma,
               lambda_0 = lambda_0,
               theta = theta,
               s = s,
               eta = eta,
               mu = mu
               )


# Estimation --------------------------------------------------------------

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
mleALL(PARAMS = PARAMS)
