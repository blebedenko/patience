# testing file
library(tictoc)
library(tidyverse)
library(simsalapar)
library(parallel)
library(patience)
library(pbapply)
library(doParallel)
patience::makeAWXDirectories()



eta <- 1
mu <- 1
s <- 1
# EB (Job size epxectation)eta/mu
n <- 1e5
gamma <- 1
lambda_0 <- 2
theta <- 1
(rho <- (gamma + lambda_0 / 2) * eta / (s * mu))
PARAMS <- c(gamma,lambda_0,theta)
r3 <-
  resSimCosine2(n_obs = 1e3,
                gamma = gamma,
                lambda_0 = lambda_0,
                theta = theta,
                s = s,
                eta = eta,
                mu = mu
  ) %>% tik

E0 <- r3

grid <- makeParGrid(params = PARAMS,spans = c(0.5,0.5,0.5),grid.sizes = c(20,20,5))
paths <- dir(path = "patience/results/C3/n=50K/realizations for s=1",full.names = T)[1:20]
paths <- getwd()
R <- pblapply(X = paths,FUN = gridFromFilePath,grid=grid)
gridFromFilePath(path = paths[1],grid = grid,csv = T)



setwd("~/patience/results/C3/n=50K/realizations for s=3")
estimateFolder(params = PARAMS)
fcgvb  rdfxc B NGY U,G HVBN ZXSD§WQ                         66666666;setwd("~/patience/results/C3/n=50K/realizations for s=5")
estimateFolder(params = PARAMS)



dir() %>% grepl("lik_grid", x = .) -> liks
L <- pblapply(dir()[liks],read.csv)
f <- function(td) td %>% pull(negLogLik) # get the likelihoods
negLogLik <- sapply(L, f)
ave_lik <- rowMeans(negLogLik)
grid$aveLogLik <- ave_lik
grid[which.min(grid$aveLogLik), ]



# Getting the mean likelihood ---------------------------------------------

dir()
setwd("~/patience/results/C3/n=50K/realizations for s=1")
name <- "ave_lik_s=1.csv"
al <- getAverageLikGrid()
write.csv(al,file = name,row.names = F)
setwd("~/patience/results/C3/n=50K/realizations for s=2")
name <- "ave_lik_s=2.csv"
al <- getAverageLikGrid()
write.csv(al,file = name,row.names = F)
setwd("~/patience/results/C3/n=50K/realizations for s=3")
name <- "ave_lik_s=3.csv"
al <- getAverageLikGrid()
write.csv(al,file = name,row.names = F)
setwd("~/patience/results/C3/n=50K/realizations for s=5")
name <- "ave_lik_s=5.csv"
al <- getAverageLikGrid()
write.csv(al,file = name,row.names = F)


