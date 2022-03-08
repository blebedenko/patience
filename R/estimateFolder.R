### ESTIMATE EVERY THING FROM AWX FOLDER


# Case 1 ------------------------------------------------------------------

SS <- c(3,5,10,20)
gamma <- 10
lambda_0 <- 10
theta <- 2.5
spans <-  c(0.7,0.7,0.5)
grid.sizes <- c(25,25,5)
PARAMS <- c(gamma=gamma,lambda_0=lambda_0,theta=theta)
grid <- makeParGrid(params = PARAMS,spans = spans,grid.sizes = grid.sizes)



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
             mle <- mleFromFilePath(path = path)
             mle
           }
toc()
stopCluster(cl)


mle_folder <- ANS_mle %>% as.data.frame()
write.csv(mle_folder,"MLE.csv",row.names = FALSE)


setwd("~/Library/Mobile Documents/com~apple~CloudDocs/simulations/C1/n=10^5/s=3")
PATHS <- dir()


