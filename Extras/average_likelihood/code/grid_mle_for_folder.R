# script to get average likelihood from a the current directory
# provided the parameters are correct
library(patience)
library(tidyverse)
library(doParallel)
library(tictoc)
gamma <- 10
lambda_0 <- 10
theta <- 2.5
spans <-  c(0.7,0.7,0.5)
grid.sizes <- c(25,25,5)
PARAMS <- c(gamma=gamma,lambda_0=lambda_0,theta=theta)
grid <- makeParGrid(params = PARAMS,spans = spans,grid.sizes = grid.sizes)


# Functions that work on a filepath ---------------------------------------


gridFromFilePath <- function(path, grid){
  dati <- read.csv(path)
  a <- Sys.time()
  negLogLik <- evaluateGridFromRealization(dati = dati,grid = grid)
  b <- Sys.time()
  timediff <- as.numeric(b-a)
  units <- attributes(b-a)$units
  cat("Start @:",a,"Stop @:",b,"time diff. of", timediff, units,"\n")
  res <- grid
  res$negLogLik <- negLogLik
  return(res)


}

mleFromFilePath <- function(path){
  dati <- read.csv(path)

  boris <- mleBoris(dati = dati,PARAMS = PARAMS)
  liron <- mleLironThetaLambda(dati = dati)
  mles <- c(boris,liron)
  mles

}


# The computation ---------------------------------------------------------

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/simulations/C1/n=10^5/s=3")
PATHS <- dir()

## The filepath functions are called in parallel ---------------------------

### average likelihood grid ----
cl <- makeCluster(8)
registerDoParallel(cl)

tic()
ANS_lik <-
foreach (i = 1:length(PATHS),.packages = c("patience","dplyr"),
         .verbose = T) %dopar% {

  path <- PATHS[i]
  # gamma <- 10
  # lambda_0 <- 10
  # theta <- 2.5
  #
  # spans <-  c(0.7,0.7,0.5)
  # grid.sizes <- c(25,25,5)
  # PARAMS <- c(gamma=gamma,lambda_0=lambda_0,theta=theta)
  #
  # grid <- makeParGrid(params = PARAMS,spans = spans,grid.sizes = grid.sizes)

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


### MLE's ----

cl <- makeCluster(8)
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
