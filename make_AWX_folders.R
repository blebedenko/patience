# this is the file which accepts the parameters for the simulation
# creates a directory and sources make_AWX_files_source.R
library(patience)


makeAWXDirectories <- function(){
  setwd(svDialogs::dlg_dir()$res) # set the directory to where pointed
  n_cores <- as.numeric(readline(prompt = "How many cores to use? "))
  n_obs <- as.numeric(readline(prompt = "Input a value for n (sample size): "))
  N_files <- as.numeric(readline(prompt = "How many files? "))
  lambda_0 <- as.numeric(readline(prompt = "Input a value for lambda_0: "))
  gamma <- as.numeric(readline(prompt = "Input a value for gamma: "))
  theta <- as.numeric(readline(prompt = "Input a value for theta: "))
  eta <- as.numeric(readline(prompt = "Input a value for eta: "))
  mu <- as.numeric(readline(prompt = "Input a value for mu: "))
  prompt <- "enter numbers of servers for the experiments (space-separated) \n"
  s_values <- as.integer(strsplit(readline(prompt), " ")[[1]])
  rate <- RATE(gamma = gamma, lambda_0 = lambda_0)
  min_rate <- lambda_0
  max_rate <- lambda_0 + gamma # not divided by two!
  ave_rate <- lambda_0 + gamma / 2
  rates <- c("minimum" = min_rate, "average" = ave_rate, "maximum" = max_rate)
  offered_loads <- foreach(s = s_values,.combine = rbind) %do% {
    rhos <- c( rates["minimum"] * eta / (s * mu),
               rates["average"] * eta / (s * mu),
               rates["maximum"] * eta / (s * mu))

  }

  all_params <- c(lambda_0,gamma,theta,eta,mu)
  param_names <- c("lambda_0","gamma","theta","eta","mu")
  param_message <- paste(param_names, " = ", all_params, "\n",collapse = "")
  server_message <- paste0("no. of servers: ", paste0(s_values,collapse = ","))
  obs_message <- paste0("no. observations: ", n_obs,"\n",
                        "no. of files: ", N_files)
  user_message <- paste0(param_message,"\n",server_message,"\n",obs_message)
  user_confirm <- svDialogs::dlg_message(message = user_message,type = "okcancel")$res
  user_confirm <- user_confirm == "ok"

  # message about offered loads:
  offered_loads <- data.frame(offered_loads)

  cat("The offered loads at the parameters you suggest are:\n")
  rownames(offered_loads) <- paste0("s=",s_values)
  print(offered_loads)
  final_confirm <- svDialogs::dlg_message(type = "okcancel")$res
  final_confirm <- final_confirm == "ok"
  if(!final_confirm)
    break
  if(final_confirm)
  {

    for (s in s_values){
      curr_n_servers <- s
      curr_dirname <- paste0("realizations for s=",curr_n_servers,"/")
      # dir.create(path = curr_dirname)
      makeSimFilesAWX(dir_path =  curr_dirname,
                      n_cores = n_cores,
                      N_files = N_files,
                      n_obs = n_obs ,
                      s = s,
                      gamma = gamma,
                      lambda_0 = lambda_0,
                      theta = theta,
                      eta = eta,
                      mu = mu)


      cat("done with s = ", s,"...","\n")
    }

  }


}
#debug(makeAWXDirectories)
makeAWXDirectories()
