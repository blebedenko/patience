
# Step 1: generate datasets -----------------------------------------------

# parameters supplied from source script
eta <- 1
mu <- 1
s <- 20
# EB (Job size epxectation)eta/mu
n <- 1000
gamma <- 10
lambda_0 <- 10
theta <- 2.5
PARAMS <-c(gamma=gamma,lambda_0=lambda_0,theta=theta)
(rho <- (gamma + lambda_0) * eta / (s*mu))
# the generation function:
#makeSimFilesAWX(N_files = 100, n_obs = 10000, gamma = 10, lambda_0 = 10, theta = 2.5, s = 10, eta = 1, mu = 1)




# Step 2: Compute likelihood ------------------------------------------------------

#' Compute log-likelihood from a dataset A,W,X
#'
#'
#' @param dati dataframe with columns A,W,X
#' @param gamma
#' @param lambda_0
#' @param theta
#' @details this auxiliary function is used within `meanLogLikelihoodFromList`.
#' @return
#' @export
#'
#' @examples
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


#' Average log likelihood from a list with A,W,X dataframes
#' @param gamma
#' @param lambda_0
#' @param theta
#' @param res_list a list where each element is a dataframe with columns A,W,X
#'
#' @return the average likelihood for the parameter values provided
#' @export
#'
#' @examples
#' # use with vectorize:
#' VmLLVec <- Vectorize(meanLogLikelihoodFromList,vectorize.args = c("gamma","lambda_0","theta"))
#'


meanLogLikelihoodFromList <- function(gamma,lambda_0,theta,res_list){
  mean(mapply(ithLL, res_list, gamma = gamma, lambda_0 = lambda_0, theta = theta))

}







#' #' Make Parameter Grid for testing
#' #'
#' #' @param params named list of the parameters (gamma,lambda_0,theta)
#' #' @param spans A number in (0,1) that derermines how far from the true
#' parameter value the grid ranges.
#' #' @param grid.size How many breaks per sequence.
#' #'
#' #' @return A dataframe with the parameter grids in the columns gamma,lambda_0,theta
#' #' @export
#' #'
#' #' @examples
#'

makeParGrid <- function(params,span,grid.size){

  grid.values <- mapply(seq,
                        from = params * (1 - spans),
                        to = params * (1 + spans),
                        length.out = grid.size) %>%
    as.data.frame()


  names(grid.values) <- c("gamma", "lambda_0", "theta")

  grid <- expand.grid(grid.values$gamma,grid.values$lambda_0,grid.values$theta)
  names(grid) <- c("gamma", "lambda_0", "theta")
  return(grid)
}

grid <- makeParGrid(params = PARAMS,spans = .4,grid.size = 10)

# read the simulation data
L <- pblapply(dir(),read.csv)
library(doSNOW)
cl <- makeCluster(detectCores()-2)
registerDoSNOW(cl)
#iterations <- nrow(grid)
iterations <- 100
pb <- txtProgressBar(max = iterations, style = 2)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
tic()
ans <- foreach(i = 1:iterations,.combine = cbind) %dopar% {
  negLL <- meanLogLikelihoodFromList(gamma = grid$gamma[i], lambda_0 = grid$lambda_0[i] ,theta = grid$theta[i],res_list = L)
  c(gamma = grid$gamma[i], lambda_0 = grid$lambda_0[i] ,theta = grid$theta[i], negLogLik = negLL)
}
close(pb)
stopCluster(cl)
toc()
t(ans)
