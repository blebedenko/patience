# testing file
library(tictoc)
library(tidyverse)
library(simsalapar)
library(parallel)
library(patience)
library(pbapply)
library(doParallel)





# Estimation --------------------------------------------------------------
library(tictoc)
library(tidyverse)
library(simsalapar)
library(parallel)
library(patience)
library(pbapply)
library(doParallel)
setwd("~/patience/results/c2 - Copy/n=10^4/")
gamma <- 40
lambda_0 <- 10
theta <- 2.5
PARAMS <- c(gamma, lambda_0, theta)
grid <- makeParGrid(
  params = PARAMS,
  spans = c(0.8, 0.8, 0.8),
  grid.sizes = c(25, 25, 10)
)


estimateALL.parallel(grid = grid, PARAMS = PARAMS)

likGridSummary("C2")
mleALL(PARAMS = PARAMS, scenario = "C2")















# Simujlatioin ------------------------------------------------------------


eta <- 1
mu <- 1
s <- 5
# EB (Job size epxectation)eta/mu
n <- 5e4
gamma <- 10
lambda_0 <- 10
theta <- 2.5
(rho <- (gamma + lambda_0 / 2) * eta / (s * mu))



PARAMS <- params <- c(gamma, lambda_0, theta)

R <- resSimCosine(
  n = 5000,
  gamma = gamma,
  lambda_0 = lambda_0,
  theta = theta,
  s = s,
  eta = eta,
  mu = mu
  )
AWX <- RES2AWX(R)
mle(AWX = RES2AWX(R),params = PARAMS)
debug(pltLik3D)
pltLik3D(params = params,AWX = AWX,known = "theta",grid_size = 30)

ave_neg_likelihoodMean.KnownGamma()
ave_neg_likelihoodMean.KnownGamma()
twoParameterLikelihood <- function(params, which_known) {
  if (which_known == "gamma") {
    gamma <- params[1]
    likFun <- function(lambda_0, theta, AWX) {
      return(ave_neg_likelihoodMean.KnownGamma(
        lambda0_theta = c(lambda_0, theta),
        gamma = gamma,
        AWX = AWX
      ))

    }
  }
  if (which_known == "lambda_0") {
    lambda_0 <- params[2]
    likFun <- function(gamma, theta, AWX) {
      return(
        ave_neg_likelihoodMean.KnownLambda0(
          gamma_theta = c(gamma, theta),
          lambda_0 = lambda_0,
          AWX = AWX
        )
      )
    }
  }
  if (which_known == "theta") {
    theta <- params[3]
    likFun <- function(lambda_0, gamma, AWX) {
      return(ave_neg_likelihoodMean.KnownTheta(
        lambda0_gamma = c(lambda_0, gamma),
        theta = theta,
        AWX = AWX
      ))
    }

  }
  return(likFun)
}

A <- RES2AWX(R)

twoParamGrid <-
  function(params,
           which_known,
           spans = rep(0.8, 3),
           grid.size = 25) {
    gamma <- params[1]
    lambda_0 <- params[2]
    theta <- params[3]

    gamma.seq <- seq(
      from = params["gamma"] * (1 - spans[1]),
      to = params["gamma"] * (1 + spans[1]),
      length.out = grid.sizes[1]
    )

    lambda_0.seq <- seq(
      from = params["lambda_0"] * (1 - spans[2]),
      to = params["lambda_0"] * (1 + spans[2]),
      length.out = grid.sizes[2]
    )


    theta.seq <- seq(
      from = params["theta"] * (1 - spans[3]),
      to = params["theta"] * (1 + spans[3]),
      length.out = grid.sizes[3]
    )


    if (which_known == "gamma") {
      grid <- expand.grid(lambda_0.seq, theta.seq)
      names(grid) <- c("lambda_0", "theta")

    }
  }
if (which_known == "lambda_0") {
  grid <- expand.grid(gamma.seq,  theta.seq)
  names(grid) <- c("gamma",  "theta")
}
if (which_known == "theta") {
  grid <- expand.grid(gamma.seq, lambda_0.seq, theta.seq)
  names(grid) <- c("gamma", "lambda_0", "theta")
}

library(tictoc)
library(tidyverse)
library(parallel)
library(patience)
library(pbapply)
library(doParallel)


# Parallel ---------
#cl <- makeCluster(length(SS)) # a core for each server number
cl <- makeCluster(8)
registerDoParallel(cl)
gamma <- 40
lambda_0 <- 10
theta <- 2.5
PARAMS <- c(gamma, lambda_0, theta)
grid <- makeParGrid(
  params = PARAMS,
  spans = c(0.8, 0.8, 0.8),
  grid.sizes = c(25, 25, 10)
)

estimateALL.parallel(grid = grid, PARAMS = PARAMS)
stopCluster(cl)




gamma <- 1
lambda_0 <- 2
theta <- 1
PARAMS <- c(gamma, lambda_0, theta)
grid <- makeParGrid(
  params = PARAMS,
  spans = c(0.8, 0.8, 0.8),
  grid.sizes = c(25, 25, 10)
)






eta <- 1
mu <- 1
s <- 10
# EB (Job size epxectation)eta/mu
n <- 5e4
gamma <- 10
lambda_0 <- 10
theta <- 2.5
(rho <- (gamma + lambda_0 / 2) * eta / (s * mu))



PARAMS <- c(gamma, lambda_0, theta)

R <- resSimCosine(
  n = n,
  gamma = gamma,
  lambda_0 = lambda_0,
  theta = theta,
  s = s,
  eta = eta,
  mu = mu
)

AWX = RES2AWX(R)
ll <- negLogLikFactory(
  gamma = 1,
  lambda_0 = 2,
  theta = 3,
  AWX = AWX
)
ll2 <- partial(ll, theta = 1)
ll2(1, 2)

p1 <- negLogLikFactory(1, 2, 1, AWX, "theta")
# create grid matrices
G       <- seq(1, 20, length.out = 50)
L       <- seq(1, 20, length.out = 50)
M       <- mesh(G, L)
phi     <- M$x
theta   <- M$y

x <- phi
y <- theta
z <- p1(phi, theta)
z <- matrix(z, 50, 50)
# full colored image
plot3D::perspbox(
  x,
  y,
  z,
  theta = 120,
  xlim = range(G),
  bty = "f",
  ticktype = "detailed"
)
surf3D(
  x,
  y,
  z,
  colvar = z,
  colkey = T,
  shade = 0.1,
  box = F,
  theta = 120,
  contour = T,
  xlab = "x",
  add = T
)
? surf3D
dd <- data.frame(x = as.vector(x), y = as.vector(y), z = as.vector(z))
names(dd) <- c("X", "Y", "Z")
plot_ly(
  x =  ~ X,
  y = ~ Y,
  z =  ~ Z,
  data = dd,
  type = "mesh3d",
  contour = list(
    show = TRUE,
    color = "#001",
    width = 5
  ),
  opacity = 0.5,
  showlegend = TRUE
)


pltLik3D <- function(params, AWX, known, grid_size = 30) {
  names(params) <- c("gamma", "lambda_0", "theta")
  gamma <- params[1]
  lambda_0 <- params[2]
  theta <- params[3]
  negL <-
    negLogLikFactory(
      gamma = gamma,
      lambda_0 = lambda_0,
      theta = theta,
      AWX = AWX,
      known = known
    )
  # parameter values:
  Gam       <- seq(gamma / 10, gamma * 1.5, length.out = grid_size)
  Lam       <-
    seq(lambda_0 / 20, lambda_0 *1.5, length.out = grid_size)
  The <- seq(theta / 10, theta * 1.5, length.out = grid_size)
  if (known == "gamma") {
    # likelihood for lambda_0 and theta
    M <- mesh(Lam,The)
    x_vals <- M$x %>% as.vector()
    y_vals <- M$y %>% as.vector()
    z_vals <- negL(x_vals, y_vals)
    # plotly axis names
    axx <- list(title = "lambda_0")
    axy <- list(title = "theta")


  }

  if (known == "lambda_0") {
    # likelihood for gamma and theta
    M <- mesh(Gam,The)
    x_vals <- M$x %>% as.vector()
    y_vals <- M$y %>% as.vector()
    z_vals <- negL(x_vals, y_vals)
    # plotly axis names
    axx <- list(title = "gamma")
    axy <- list(title = "theta")
  }

  if (known == "theta") {
    # likelihood for gamma and lambda_0
    M <- mesh(Gam,Lam)
    x_vals <- M$x %>% as.vector()
    y_vals <- M$y %>% as.vector()
    z_vals <- negL(x_vals, y_vals)
    # plotly axis names
    axx <- list(title = "gamma")
    axy <- list(title = "lambda_0")
  }

  # plotly z-axis:
  axz <- list(title = "neg. log-likelihood")
  dd <- data.frame(X=x,Y=y,negLik=z)

  lik_plot <-
  plot_ly(
    x =  ~ dd$X,
    y = ~ dd$Y,
    z =  ~ negLik,
    data = dd,
    type = "mesh3d",
    # contour = list(
    #   show = TRUE,
    #   # color = "#001",
    #   width = 5
    # ),
intensity =  ~negLik
  ) %>%
    layout(scene = list(xaxis = axx, yaxis = axy, zaxis = axz),
           title = list(text = paste0("Likelihood when ", known, " is known")))


  lik_plot
}
debug(pltLik3D)
pltLik3D(params ,AWX ,known = "gamma")
pltLik3D(params = PARAMS,AWX = AWX,known = "lambda_0")
pltLik3D(params = PARAMS,AWX = AWX,known = "theta")


