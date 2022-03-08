# Run this file to set up the folder estimation
# including package install etc.
# script to get average likelihood from a the current directory
# provided the parameters are correct
library(devtools)
library(tidyverse)
library(doParallel)
library(tictoc)

devtools::install_github("blebedenko/patience")
library(patience)

n.cores <- -Inf # change to correct number of cores on each machine

# Case 1 ---------

gamma <- 10
lambda_0 <- 10
theta <- 2.5
spans <-  c(0.7,0.7,0.5)
grid.sizes <- c(25,25,5)
PARAMS <- c(gamma=gamma,lambda_0=lambda_0,theta=theta)
grid <- makeParGrid(params = PARAMS,spans = spans,grid.sizes = grid.sizes)



## Small n -----

setwd("###")
PATHS <- dir()

cl <- makeCluster(n.cores)
registerDoParallel(cl)



tic()
ANS_lik <-
  foreach (i = 1:length(PATHS),.packages = c("patience","dplyr"),
           .verbose = T) %dopar% {

             path <- PATHS[i]
             curr_grid_with_logLik <- gridFromFilePath(path = path, grid = grid)

             curr_grid_with_logLik

           }
toc()
stopCluster(cl)


averageLogLik <-
  sapply(ANS_lik, function(d1) d1 %>% pull (negLogLik)) %>%
  rowMeans()

average_for_folder <- data.frame(grid, averageLogLik)
write.csv(average_for_folder,"average_likelihood_grid.csv")



# MLE's



cl <- makeCluster(n.cores)
registerDoParallel(cl)
tic()
ANS_mle <-
  foreach (i = 1:length(PATHS),.packages = c("patience","dplyr"),
           .verbose = T,.combine = "rbind") %dopar% {


             path <- PATHS[i]
             gamma <- 10
             lambda_0 <- 10
             theta <- 2.5

             PARAMS <- c(gamma=gamma,lambda_0=lambda_0,theta=theta)
             mle <- mleFromFilePath(path = path)
             mle
           }
toc()
stopCluster(cl)


mle_folder <- ANS_mle %>% as.data.frame()
write.csv(mle_folder,"MLE.csv",row.names = FALSE)


## Big n --------------

# Case 2 -------

## Small n -----

## Big n -----
