# Simulation --------------------------------------------------------------


#' Rate function factory
#'
#' @param gamma
#' @param lambda_0
#'
#' @return A function with argument t that computes the arrival rate
#' @export
#'
#' @examples
#' rate <- RATE(5,10)
#' rate(1:3)
RATE <- function(gamma,lambda_0){
  return(function(t) lambda_0 + (gamma/2) * (1 + cos(2*pi*t)))
}




#' Next Arrival of inhomogeneous Poisson process
#'
#' Generates the next arrival time (not inter-arrival!) of a nonhomogeneous Poisson process.
#' @param current_time The current time (a non-negative real number)
#' @param gamma Parameter of the cosine arrival
#' @param lambda_0 Parameter of the cosine arrival
#' @return The time of the next arrival.
#'
#' @export
nextArrivalCosine <- function(current_time, gamma, lambda_0){
  lambda_sup <- gamma + lambda_0 # highest rate in the cosine model
  rate <- RATE(gamma = gamma, lambda_0 = lambda_0)
  arrived <- FALSE
  while (!arrived) {
    u1 <- runif(1)
    current_time <- current_time - (1/lambda_sup) * log(u1) # generate next arrival from Pois(sup_lambda)
    u2 <- runif(1)
    arrived <- u2 <= rate(current_time) / lambda_sup
  }
  return(current_time)
}



#' Customer's job size and patience
#' Provides with a realization of Y~Exp(theta) and B~Gamma(eta,mu)
#' @param theta The parameter of the exponential patience.
#' @param eta Shapae parameter of the job size.
#' @param mu rate parameter of the job size.
#'
#' @return A list with elements 'Patience' and 'Jobsize'
#' @export
#'
#' @examples
customerExp <- function(theta,eta,mu){

  B <-  rgamma(1, shape = eta, rate = mu) #Job sizes
  Y <- rexp(1,theta)
  return(list(Patience=Y,Jobsize=B))
}



#' Liron's Virtual Waiting function
#'
#' @param Res.service the vector of residual service times
#' @param s the number of servers
#' @export
VW <- function(Res.service,s){
  Q.length <- length(Res.service) #Number in the system
  if(Q.length < s){virtual.wait <- 0}else
  {
    D.times <- rep(NA,Q.length+1)
    Vw <- rep(NA,Q.length+1)
    D.times[1:s] <- Res.service[1:s]
    Vw[1:s] <- rep(0,s) #VW for customers in service
    for(i in (s+1):(Q.length+1))
    {
      D.i <- sort(D.times[1:i]) #Sorted departures of customers ahead of i
      Vw[i] <- D.i[i-s] #Virtual waiting time for position i
      if(i <= Q.length){D.times[i] <- Res.service[i]+Vw[i]} #Departure times
    }
    virtual.wait <- Vw[Q.length+1]
  }
  return(virtual.wait)
}

#' Atomic Simulation for periodic arrivals (cosine model)
#' @param gamma The periodic arrival rate maximum amplitude
#' @param lambda_0 The constant arrival rate
#' @param theta Parameter of the exponential patience
#' @param eta Shape parameter of job size.
#' @param mu Rate parameter of job size.
#' @param s Number of servers.
#' @param n Number of samples to generate.
#' @return A list with the estimates
#' @export
#' @examples
resSimCosine <- function(n, gamma, lambda_0, theta, s, eta, mu){
  #find the supremum of the arrival function
  lambda_sup <- lambda_0 + gamma
  lambdaFunction <-   RATE(gamma = gamma, lambda_0 = lambda_0)
  #Running simulation variables
  klok <- 0
  n.counter <- 0 #Observation counter
  Q.length <- 0 #Queue length (including service)
  virtual.wait <- 0 #Virtual waiting time process
  m <- 0 #Total customer counter
  #A <- rexp(1,lambda) #First arrival time
  A <- nextArrivalCosine(klok[length(klok)],  gamma, lambda_0)
  A <- A - klok[length(klok)]
  klok <- c(klok, klok[length(klok)] + A)
  trans.last <- A #Counter of time since last transition
  time.last <- A #Counter of time since last admission event
  Res.service <- numeric(0) #Vector of residual service times

  #Output vectors:
  Wj <- rep(NA,n) #Vector of workloads before jumps
  Xj <- rep(NA,n) #Vector of workload jumps
  Aj <- rep(NA,n) #Vector of effective inter-arrival times
  Qj <- rep(NA,n) #Vector of queue lengths at arrival times
  IPj <- A #Vector of idle periods
  Yj <- rep(NA,n) # Vector of patience values of customers that join
  Q.trans <- 0 #Vector of queue lengths at transitions
  IT.times <- numeric(0) #Vector of inter-transition times
  Pl <- 1 #Proportion of lost customers
  Nl <- 0 # number of lost customers

  while(n.counter < n+1)
  {
    m <- m+1
    customer <- customerExp(theta = theta, eta = eta, mu = mu)
    #Generate patience and job size:
    B <- customer$Jobsize
    Y <- customer$Patience
    if(virtual.wait <= Y) #New customer if patience is high enough
    {

      n.counter <- n.counter+1 #Count observation
      Res.service <- c(Res.service,B) #Add job to residual service times vector
      Q.length <- length(Res.service) #Queue length
      Q.trans <- c(Q.trans,Q.length) #Add current queue length
      #Add new observations:
      Wj[n.counter] <- virtual.wait
      Aj[n.counter] <- time.last
      Qj[n.counter] <- Q.length-1 #Queue length (excluding new arrival)
      Yj[n.counter] <- Y # patience of the customer arriving
      IT.times <- c(IT.times,trans.last) #Update transition time
      trans.last <- 0 #Reset transition time
      time.last <- 0 #Reset last arrival time
      Pl <- m*Pl/(m+1) #Update loss proportion
    }else { Pl <- m*Pl/(m+1)+1/(m+1)
    Nl <- Nl + 1}

    #Update system until next arrival event
    #A <- rexp(1,lambda) #Next arrival time
    A <- nextArrivalCosine(klok[length(klok)],  gamma, lambda_0)
    A <- A - klok[length(klok)]
    klok <- c(klok, klok[length(klok)] + A)
    time.last <- time.last+A #Add arrival time to effective arrival time

    #Departure and residual service times of customers in the system:
    Q.length <- length(Res.service) #Queue length
    D.times <- rep(NA,Q.length) #Departure times
    Vw <- rep(NA,Q.length) #Virtual waiting times
    for(i in 1:Q.length)
    {
      if(i <= s)
      {
        Vw[i] <- 0 #No virtual waiting time
        D.times[i] <- Res.service[i] #Departure time is the residual service time
        Res.service[i] <- max(Res.service[i]-A,0) #Update residual service time
      }else
      {
        D.i <- sort(D.times[1:i]) #Sorted departures of customers ahead of i
        Vw[i] <- D.i[i-s] #Time of service start for customer i
        D.times[i] <- Res.service[i]+Vw[i] #Departure time
        serv.i <- max(0,A-Vw[i]) #Service obtained before next arrival
        Res.service[i] <- max(Res.service[i]-serv.i,0) #New residual service
      }
    }
    #Jump of virtual waiting time:
    if(virtual.wait <= Y)
    {
      if(Q.length < s)
      {
        Xj[n.counter] <- 0
      }else
      {
        Xj[n.counter] <- sort(D.times)[Q.length+1-s]-virtual.wait
      }
    }
    #Update residual service times:
    Res.service <- Res.service[!(Res.service == 0)] #Remove completed services

    #Update transition times and queue lengths:
    D.before <- which(D.times <= A) #Departing customers before next arrival
    if(length(D.before) > 0)
    {
      T.d <- sort(D.times[D.before]) #Sorted departure times
      for(i in 1:length(D.before))
      {
        Q.trans <- c(Q.trans,Q.length-i) #Update queue length at departures
        if(i ==1)
        {
          trans.last <- trans.last+T.d[1] #Update time since last transition
          IT.times <- c(IT.times,trans.last) #Departure transition time
          trans.last <- 0 #Reset transition time
        }else
        {
          trans.last <- trans.last+T.d[i]-T.d[i-1] #Update time since last transition
          IT.times <- c(IT.times,trans.last) #Departure transition time
          trans.last <- 0 #Reset transition time
        }
        if(Q.trans[length(Q.trans)] == 0){IPj <- cbind(IPj,A-T.d[length(T.d)])} #Add idle time observation
      }
      trans.last <- A-T.d[i] #Update remaining time until next arrival
    }else if (length(D.before) == 0 )
    {
      trans.last <- trans.last+A #Update timer since least transition with new arrival
    }
    virtual.wait <- VW(Res.service,s) #Update virtual waiting time

  }
  # progress bar
  if (m%%1000 == 0 )
    cat("* ")
  RES <- list(Aj=Aj,Xj=Xj,Wj=Wj,Qj=Qj,
              IPj=IPj,Q.trans=Q.trans,IT.times=IT.times,
              Yj = Yj, klok = klok, Pl = Pl, Nl = Nl)

  return(RES)
}



filenamer <- function(n_obs, gamma, lambda_0, theta, s, eta, mu){
  name <- (paste0("AWX_",
                  "s=",s,
                  "n=",n_obs,
                  "gamma=",gamma,
                  "lambda_0=",lambda_0,
                  "theta=",theta,
                  "eta=",eta,
                  "mu=", mu))
  name <- paste0(name,
                 as.numeric(Sys.time()),
                 ".csv") # name with timestamp and extension
  return(name)
}
filenamer(n_obs = 1000,
          gamma = 19,
          lambda_0 = 10,
          theta = 2.5,
          s = 5,
          eta = 1,
          mu = 1)

#' Function to make many files with simulation results
#'
#' @param path Absolute path of the folder to create the files in (will be set to wd)
#' @param N_files Desired number of files to be created in the working directory
#' @param n_obs Number of effective arrivals per file. Defaults to 10,000. Do not use less than 5000.
#' @param gamma The periodic arrival rate maximum amplitude
#' @param lambda_0 The constant arrival rate
#' @param theta Parameter of the exponential patience
#' @param eta Shape parameter of job size.
#' @param mu Rate parameter of job size.
#' @param s Number of servers.
#' @details The filenames have the parameter values encoded alongside a timestamp,
#' which is meant for protection against overwriting in case of repeated calls to this function.
#' Also note that if files take less than a second to generate, they will not be written.
#' @return
#' @export
#'
#' @examples
makeSimFilesAWX <- function(path,
                            n.cores = parallel::detectCores() - 2,
                            N_files,
                            n_obs,
                            gamma,
                            lambda_0,
                            theta,
                            s,
                            eta,
                            mu){
  cl <- makeCluster(n.cores) # make a parallel cluster
  doParallel::registerDoParallel(cl=cl)
  foreach::foreach(i= 1:N_files,
                   .combine = c,
                   .packages = "patience") %dopar% {
                     RES <- resSimCosine(n=n_obs,
                                         gamma = gamma,
                                         lambda_0 = lambda_0,
                                         theta = theta,
                                         s = s,
                                         eta = eta,
                                         mu = mu)
                     A <- RES$A
                     W <- RES$Wj
                     X <- RES$Xj
                     dat <- data.frame(A=A,W=W,X=X)
                     name <- filenamer(n_obs = n_obs,
                                       gamma = gamma,
                                       lambda_0 = lambda_0,
                                       theta = theta,
                                       s = s,
                                       eta = eta,
                                       mu = mu)
                     write.csv(dat,file = name,row.names = FALSE)
                   }
  stopCluster(cl)
}


# Plotting  ---------------------------------------------------------------



pltQueueLengthArrivals <- function(RES, n_customers = 500, pch = 4){
  A <- RES$Aj # start from 0
  W <- RES$Wj
  Q.trans <- RES$Q.trans
  IT.times <- RES$IT.times
  Transitions <- c(0,cumsum(IT.times))
  A_tilde <-c(0,cumsum(A))
  A_tilde <- A_tilde[-length(A_tilde)]

  # scale everyting:
  # A_tilde <- scale(A_tilde)
  # W <- scale(W)

  # scaled lambdaFucntion just for this plot

  last_transition <- which.max(Transitions[Transitions<=A_tilde[n_customers]])
  # advanced plot -----------------------------------------------------------
  plot(Transitions[1:last_transition],Q.trans[1:last_transition],
       type='s',
       lwd=0.5,
       xlab = "time",
       ylab = "No. customers",
       col = "darkgreen",
       main = "Queue length and arrivals")
  stripchart(A_tilde,at=.01, add=T,pch = pch)
  last_transition <- which.max(Transitions[Transitions <= max(A_tilde)] )
  # normalized the queue length by the maximum

  # twoord.plot(lx=c(0,Transitions[1:(n_customers-1)]), ly = Q.trans[1:n_customers],
  #             rx = c(0,A_tilde[1:(n_customers-1)]), ry = W[1:n_customers],
  #             rylim =
  #               type = c("s","s"),
  #             lty = 1:2)
  legend(x = "topleft",
         legend = c("No. Customers","Waiting time","Effective arrival"),
         col = c("black","darkgreen"),
         lty = c(1,1,NA),
         lwd = c(2,2,1),
         pch = c(NA,NA,pch))

}


pltQueueByHour <- function(RES, n_quantiles = 4){
  A <- RES$Aj
  W <- RES$Wj
  Q.trans <- RES$Q.trans
  IT.times <- RES$IT.times
  Transitions <- c(0,cumsum(IT.times))
  A_tilde <-c(0,cumsum(A))
  A_tilde <- A_tilde[-length(A_tilde)]
  X <- RES$Xj
  Q <- RES$Qj
  Pl <- RES$Pl
  Y <- RES$Yj
  (lambda.eff <- 1/mean(A))

  nt <- length(Transitions)
  # Little's Law:
  # a <- (mean(W)+input$eta/input$mu)*lambda.eff  #Arrival rate times sojourn times
  # b <- sum(Q.trans[1:nt]*IT.times[1:nt])/sum(IT.times[1:nt]) #Empirical mean queue length
  # a <- round(a,3)
  # b <- round(b,3)

  IT.hours <- IT.times * (24)

  Y_quantized <- dvmisc::quant_groups(Y,groups = n_quantiles)
  Transitions.hours <- cumsum(IT.hours)
  dat <- tibble::tibble(time = Transitions.hours, queue =Q.trans[-1])
  dat <- dat %>% mutate(h =trunc(time %% 24))
  average_customers_hourly <- dat %>% group_by(h) %>%
    summarise(m=mean(queue))
  h <- trunc(24 * (A_tilde - trunc(A_tilde)))
  dat_customers <- tibble(h,patience_class = Y_quantized)
  hourly_customers <- dat_customers %>%  group_by(h,patience_class) %>%
    tally() %>%
    ungroup()
  total_per_hour <-   hourly_customers %>%
    group_by(h) %>%
    summarize(total_hour = sum(n))
  hourly_customers %>%
    ggplot() +
    aes(x=h,y=n,fill=patience_class) +
    geom_col() +
    ylab("No. of customers") +
    xlab("Hour") +
    theme(axis.text = element_text(size=20),axis.title = element_text(size=20)) +
    ggtitle("Queue length by hour") +
    theme_bw()


}



pltQueueByHourPerc <- function(RES, n_quantiles = 4){
  A <- RES$Aj
  W <- RES$Wj
  Q.trans <- RES$Q.trans
  IT.times <- RES$IT.times
  Transitions <- c(0,cumsum(IT.times))
  A_tilde <-c(0,cumsum(A))
  A_tilde <- A_tilde[-length(A_tilde)]
  X <- RES$Xj
  Q <- RES$Qj
  Pl <- RES$Pl
  Y <- RES$Yj
  A <- RES$Aj
  (lambda.eff <- 1/mean(A))

  nt <- length(Transitions)
  # Little's Law:
  # a <- (mean(W)+input$eta/input$mu)*lambda.eff  #Arrival rate times sojourn times
  # b <- sum(Q.trans[1:nt]*IT.times[1:nt])/sum(IT.times[1:nt]) #Empirical mean queue length
  # a <- round(a,3)
  # b <- round(b,3)

  IT.seconds <- IT.times * (24)

  Y_quantized <- dvmisc::quant_groups(Y,n_quantiles)

  Transitions.seconds <- cumsum(IT.seconds)
  dat <- tibble::tibble(time = Transitions.seconds, queue =Q.trans[-1])
  dat <- dat %>% mutate(h =trunc(time %% 24))
  average_customers_hourly <- dat %>% group_by(h) %>%
    summarise(m=mean(queue))

  h <- trunc(24 * (A_tilde - trunc(A_tilde)))
  dat_customers <- tibble(h,patience_class = Y_quantized)
  hourly_customers <- dat_customers %>%  group_by(h,patience_class) %>%
    tally() %>%
    ungroup()
  total_per_hour <-   hourly_customers %>%
    group_by(h) %>%
    summarize(total_hour = sum(n))
  hourly_customers %>% inner_join(total_per_hour,by = "h") %>%
    mutate(p = n/total_hour) %>%
    ggplot() +
    aes(x=h,y=p,fill=patience_class) +
    geom_col() +
    ylab("Average no. customers") +
    xlab("Hour") +
    theme(axis.text = element_text(size=20),axis.title = element_text(size=20)) +
    scale_y_continuous(labels = scales::percent_format()) +
    ggtitle("No. of customers by hour", "partioned inot quantile groups") +
    theme_bw()



}


plotLogLikelihoodAlpha <- function(RES,theta,lambda_bar){
  RES <<- RES # create in this env.
  theta <<- theta
  lambda_bar <<-lambda_bar
  W <- RES$Wj
  Q.trans <- RES$Q.trans
  IT.times <- RES$IT.times
  Transitions <- c(0,cumsum(IT.times))
  A <- RES$A
  A_tilde <-c(0,cumsum(A))
  A_tilde <- A_tilde[-length(A_tilde)]
  X <- RES$Xj
  Q <- RES$Qj
  Pl <- RES$Pl
  Y <- RES$Yj
  f <- Vectorize(logLikelihoodAlpha,vectorize.args = "alpha")
  plot(f,from = 0, to = 1,ylab = "log-likelihood",
       xlab = expression(alpha),
       main = paste(c("mle of alpha :", round(findAlphaMLE(RES,theta=theta,lambda_bar = lambda_bar),3))))

}




# Estimation --------------------------------------------------------------

## Full estimation (gamma, lambda_0, theta) ---------


#' (negative) mean log-likelihood for the sinusoidal arrivals model
#' The mean is returned instead of the sum - should help gradient based optimizers
#' @param params A vector with values (gamma,lambda_0, theta).
#' @param dati compact simulation results for memory economy (A,W,X).
#' @return The negative log-likelihood at the point provided.
#' @export
#'
negLogLikelihoodMean<- function(params,dati){

  gamma <- params[1]
  lambda_0 <- params[2]
  theta <- params[3]

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

  # elements of the log-likelihood
  l_i <-
    log(gamma/2+lambda_0+(gamma*cos(A_tilde_i*pi*2))/2)+
    log(exp(-W_i*theta))+
    (gamma*exp(-theta*(w_i+x_i))*(pi*sin(A_tilde_i*pi*2)*2+theta*cos(A_tilde_i*pi*2)-pi*sin(pi*(A_i+A_tilde_i)*2)*exp(A_i*theta)*2-theta*exp(A_i*theta)*cos(pi*(A_i+A_tilde_i)*2)))/(pi^2*8+theta^2*2)-(lambda_0*exp(-theta*(w_i+x_i))*(exp(A_i*theta)-1))/theta-(gamma*exp(-theta*(w_i+x_i))*(exp(A_i*theta)-1))/(theta*2)

  # return the negative mean
  negMean <- -mean(l_i)
  return(negMean)

}








gradNegLogLikelihoodMean <- function(params,dati){

  gamma <- params[1]
  lambda_0 <- params[2]
  theta <- params[3]

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



  # derivative by gamma:
  dl_gamma <-
    (cos(A_tilde_i*pi*2)/2+1/2) /
    (gamma/2+lambda_0+(gamma*cos(A_tilde_i*pi*2))/2)-(exp(-theta*(w_i+x_i))*(exp(A_i*theta)-1))/(theta*2)+(exp(-theta*(w_i+x_i))*(pi*sin(A_tilde_i*pi*2)*2+theta*cos(A_tilde_i*pi*2)-pi*sin(pi*(A_i+A_tilde_i)*2)*exp(A_i*theta)*2-theta*exp(A_i*theta)*cos(pi*(A_i+A_tilde_i)*2)))/(pi^2*8+theta^2*2)

  #derivative by lambda_0:
  dl_lambda_0 <-
    1/(gamma/2+lambda_0+(gamma*cos(A_tilde_i*pi*2))/2)-(exp(-theta*(w_i+x_i))*(exp(A_i*theta)-1))/theta

  # derivative by theta:
  dl_theta <-
    -W_i + lambda_0*1/theta^2*exp(-theta*(w_i+x_i))*(exp(A_i*theta)-1)-(gamma*exp(-theta*(w_i+x_i))*(-cos(A_tilde_i*pi*2)+exp(A_i*theta)*cos(pi*(A_i+A_tilde_i)*2)+A_i*pi*sin(pi*(A_i+A_tilde_i)*2)*exp(A_i*theta)*2+A_i*theta*exp(A_i*theta)*cos(pi*(A_i+A_tilde_i)*2)))/(pi^2*8+theta^2*2)+(gamma*1/theta^2*exp(-theta*(w_i+x_i))*(exp(A_i*theta)-1))/2+(gamma*exp(-theta*(w_i+x_i))*(w_i+x_i)*(exp(A_i*theta)-1))/(theta*2)-(gamma*exp(-theta*(w_i+x_i))*(w_i+x_i)*(pi*sin(A_tilde_i*pi*2)*2+theta*cos(A_tilde_i*pi*2)-pi*sin(pi*(A_i+A_tilde_i)*2)*exp(A_i*theta)*2-theta*exp(A_i*theta)*cos(pi*(A_i+A_tilde_i)*2)))/(pi^2*8+theta^2*2)+(lambda_0*exp(-theta*(w_i+x_i))*(w_i+x_i)*(exp(A_i*theta)-1))/theta-gamma*theta*exp(-theta*(w_i+x_i))*1/(pi^2*4+theta^2)^2*(pi*sin(A_tilde_i*pi*2)*2+theta*cos(A_tilde_i*pi*2)-pi*sin(pi*(A_i+A_tilde_i)*2)*exp(A_i*theta)*2-theta*exp(A_i*theta)*cos(pi*(A_i+A_tilde_i)*2))-(A_i*gamma*exp(A_i*theta)*exp(-theta*(w_i+x_i)))/(theta*2)-(A_i*lambda_0*exp(A_i*theta)*exp(-theta*(w_i+x_i)))/theta
  # return the negative of the gradient elements' mean
  negativeGradientMean <-  - c(mean(dl_gamma), mean(dl_lambda_0), mean(dl_theta))


  return(negativeGradientMean)


}


#' MLE for the cosine arrival + exponential patience model
#'
#' @param RES
#' @param PARAMS (temporary) True values of parameters to use a starting points
#' @param type type of log-likelihood to be optimized - sum ("s") or mean ("m")
#' @return gradient vector of the negative log-likelihood
#' @export
#'
#' @examples
mleBoris <- function(dati, PARAMS){

  opt <-
    optim(PARAMS, # note that PARAMS is temporary
          fn = negLogLikelihoodMean,
          lower = PARAMS/10,
          upper = PARAMS*10,
          method = "L-BFGS-B",
          gr = gradNegLogLikelihoodMean,
          dati = dati)


  ans <- opt$par
  names(ans) <- c("gamma","lambda_0","theta")
  return(ans)
}

## Known Arrival rate ------


#' (negative) mean log-likelihood for known sinusoidal arrivals
#' The mean is returned instead of the sum - should help gradient based optimizers
#' @param theta a vector of theta values
#' @param params A vector with values (gamma,lambda_0)
#' @param dati compact simulation results for memory economy (A,W,X).
#' @return The negative log-likelihood at the point provided.
#' @export
#'
negLogLikelihoodMean.KnownArrival<- function(theta.vec,params,dati){

  gamma <- params[1]
  lambda_0 <- params[2]

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

  negMean <- numeric(length(theta.vec))
  for (k in 1:length(theta.vec)){
    theta <- theta.vec[k]
    l_i <-
      log(exp(-W_i*theta))+
      (gamma*exp(-theta*(w_i+x_i))*(pi*sin(A_tilde_i*pi*2)*2+theta*cos(A_tilde_i*pi*2)-pi*sin(pi*(A_i+A_tilde_i)*2)*exp(A_i*theta)*2-theta*exp(A_i*theta)*cos(pi*(A_i+A_tilde_i)*2)))/(pi^2*8+theta^2*2)-(lambda_0*exp(-theta*(w_i+x_i))*(exp(A_i*theta)-1))/theta-(gamma*exp(-theta*(w_i+x_i))*(exp(A_i*theta)-1))/(theta*2)
    negMean[k] <- -mean(l_i)

  }
  # elements of the log-likelihood

  # return the negative mean
  return(negMean)

}

## Liron's estimator -------------------------------------------------------


#Lambda MLE given an estimator for theta (exponential patience)
lambda.MLE <- function(theta.hat,A,W,X){
  n <- length(W)
  a <- exp(-theta.hat*W[2:n])-exp(-theta.hat*(W[1:(n-1)]+X[1:(n-1)]))
  b <- theta.hat*pmax(rep(0,n-1),A[2:n]-W[1:(n-1)]-X[1:(n-1)])
  lambda.hat <- n*theta.hat/sum(a+b)
  return(lambda.hat)
}

mleLironThetaLambda<- function(dati,acc=1e-4){

  A <- dati$A
  W <- dati$W
  X <- dati$X
  n <- length(W)
  Theta <- c(0,10^3) #Search range
  d <- acc*2
  #Bisection search for optimal theta
  while(abs(d)>acc)
  {
    theta.hat <- mean(Theta)
    lambda.hat <- lambda.MLE(theta.hat,A,W,X) #Lambda mle for given theta
    a <- (1+theta.hat*W[2:n])*exp(-theta.hat*W[2:n])-(1+theta.hat*(W[1:(n-1)]+X[1:(n-1)]))*exp(-theta.hat*(W[1:(n-1)]+X[1:(n-1)]))
    d <- mean(W[2:n])-lambda.hat*mean(a)/(theta.hat^2)
    #Update value:
    if(d > acc){Theta[2] <- theta.hat}
    if(d < -acc){Theta[1] <- theta.hat}
  }
  return(c("theta.hat"=theta.hat,"lambda.hat"=lambda.hat))
}


# Parallel Simulation -----------------------------------------------------

#' atomic realization for parallel simulation study
#'
#' Atomic Simulation for periodic arrivals (cosine model)
#' @param n Number of samples (effective arrivals) to generate.
#' @param lambda_0 The constant arrival rate
#' @param gamma The periodic arrival rate maximum amplitude
#' @param theta Parameter of the exponential patience
#' @param eta Shape parameter of job size.
#' @param mu Rate parameter of job size.
#' @param s Number of servers.

#'
#' @return A vector with Boris's etimates and Liron's etsimates
#' @export
#'
#' @examples
atomicSim <- function(n,lambda_0, gamma,theta, eta =1, mu = 1, s ){
  # temporary: use true parameters as starting values
  PARAMS <- c(gamma = gamma,lambda_0=lambda_0,theta = theta)
  # generate the sample
  RES <- resSimCosine(n=n,gamma = gamma,lambda_0 = lambda_0,theta = theta,s = s,eta = eta,mu = mu)
  boris <- mleBoris(RES,PARAMS)
  liron <- mleLironThetaLambda(RES)
  ans <- as.numeric(c(boris,liron))
  names(ans) <- c("B_gamma", "B_lambda", "B_theta", "L_theta", "L_lambda")
  return(ans)
}


# Average likelihood ------------------------------------------------------

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

  params <- c(gamma,lambda_0,theta)
  negMean <- negLogLikelihoodMean(params = params, dati = dati)
  return(negMean)
}


#' Average log likelihood from many files
#'
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

#' Make parameter grid for the average likelihood computation
#'
#' @param params named list of the parameters (gamma,lambda_0,theta)
#' @param spans vector of length 3 which determines the span of each axis in the
#' grid. Values must be in the interval [0,1). See details.
#' @param grid.sizes an integer vector of length 1 or 3. In the former case, all
#' axes have the same number of points, and in the latter each parameter is
#' matched to the corresponding element of `grid.sizes`.
#'
#' @return
#' @export
#'
#' @examples
makeParGrid <- function(params,spans,grid.sizes){

  if (!(length(spans) %in% c(1,3)) || !(length(grid.sizes %in% c(1,3))))
    stop("Wrong length of spans/grid.sizes. Must be of lengths either 1 or 3.")
  if (any(spans >= 1 | spans <=0))
    stop("All parameters must be positive. Please correct the value of spans.")
  if (length(params) != 3)
    stop("Invalid number of parameters, must be three - gamma, lambda_0, theta.")
  # the central parameter values
  names(params) <- c("gamma", "lambda_0", "theta")

  gamma.seq <- seq(from = params["gamma"]* (1 - spans[1]),
                   to = params["gamma"]* (1 + spans[1]),
                   length.out = grid.sizes[1])

  lambda_0.seq <- seq(from = params[ "lambda_0"]* (1 - spans[2]),
                      to = params[ "lambda_0"]* (1 + spans[2]),
                      length.out = grid.sizes[2])


  theta.seq <- seq(from = params[ "theta"]* (1 - spans[3]),
                   to = params[ "theta"]* (1 + spans[3]),
                   length.out = grid.sizes[3])


  grid <- expand.grid(gamma.seq,lambda_0.seq,theta.seq)
  names(grid) <- c("gamma", "lambda_0", "theta")
  return(grid)
}


#' Title Read all simulation files in a folder
#'
#' @param path The folder path. Defaults to the current working directory.
#'
#' @return a list with elemets comprising the AWX tagbles.
#' @export
#'
#' @examples L <- readAWXFiles()
readAWXFiles <- function(path = getwd()){
  L <- pblapply(dir(path = path), read.csv)
  return(L)
}



#' Evaluate the log likelihood function over a grid of parameter values
#'
#' @param dati a dataset A,W,X
#' @param grid a grid of parameters
#'
#' @return
#' @export
#'
#' @examples

evaluateGridFromRealization <- function(dati, grid){
  # prepare the data:
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
  # the columns of this dataframe will contain the repeating values in the loglikelihood
  tdf <- data.frame(A_i,A_tilde_i,W_i,w_i,x_i)
  # columns that are independent of the parameters
  tdf <- tdf %>%
    mutate(
      s3 = 2 * pi * (A_i + A_tilde_i),
      s4 = cos(2 * pi * A_tilde_i),
      s5 = sin(2 * pi * A_tilde_i)

    )
  # create a local function that computes the likelihood for one parameter value

  getNegMeanLogLik <- function(gamma, lambda_0, theta){

    tdf %>%
      # create the shortcut notation columns:
      mutate(s1 = exp(-theta * (w_i + x_i)),
             s2 = exp(theta * A_i) - 1,
             s3 = 2 * pi * (A_i + A_tilde_i),
             s4 = cos(2 * pi * A_tilde_i),
             s5 = sin(2 * pi * A_tilde_i),
             s6 = s2 + 1) %>%
      # compute each row of the expression for l_i in the PDF
      mutate(row1 = log(gamma/2 + lambda_0 + (gamma/2) * s4),
             row2 = W_i * theta,
             row3 = gamma * s1 *
               (2 * pi * s5 + theta * s4 - 2 * pi * sin(s3) * s6 - theta * s6 * cos(s3)) /
               (2 * (theta ^ 2 + 4 * pi ^ 2)),
             row4 = lambda_0 * s1 * s2 / theta,
             row5 = gamma * s1 * s2 / (2 * theta)) %>%
      summarize( - mean( row1 - row2 + row3 - row4 - row5)) %>%
      pull()
  }

  # apply the function to the parameter grid
  ans <- mapply(getNegMeanLogLik,
                gamma = grid$gamma,
                lambda_0 = grid$lambda_0,
                theta = grid$theta)

  return(ans)


}



#' Evaluate a parameter grid's likelihood from AWX file
#'
#' @param path a AWX.csv file path
#' @param grid output of makeParGrid()
#'
#' @return
#' @export
#'
#' @examples
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




#' Evaluate MLE's (Boris and Liron) from AWX file
#'
#' @param path a AWX.csv file path
#' @param grid output of makeParGrid()
#'
#' @return Length 5 vector - 3 Boris + 2 Liron
#' @export
#'
#' @examples
mleFromFilePath <- function(path){
  dati <- read.csv(path)

  boris <- mleBoris(dati = dati,PARAMS = PARAMS)
  liron <- mleLironThetaLambda(dati = dati)
  mles <- c(boris,liron)
  mles

}





