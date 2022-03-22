# run inside a folder with the mixed likelihoods
likGridSummary <- function(scenario_name){
filenames <- dir()
s_values <- filenames %>% str_extract("\\d+") %>% as.numeric() %>% unique()
files_by_s <- split(filenames, s_values)

res_list <- list()
for(i in 1:length(s_values)){
  curr_s_liks <- files_by_s[[s_values[i]]]
  curr_s_liks <- lapply(curr_s_liks, read.csv)

  A <- purrr::reduce(curr_s_liks,
                     left_join,
                     by = c("gamma","lambda_0","theta"))

  ave_neg_lik <- A %>% select(contains("neg_lik")) %>% rowMeans()

  dat_s_lik <- A %>%
    select(gamma,lambda_0,theta) %>%
    bind_cols(ave_neg_lik = ave_neg_lik) %>%
    bind_cols(s = s_values[i])

  res_list[[i]] <- dat_s_lik
  names(res_list)[i] <- paste0("s=",s_values[i])
}

# a table with the likelihoods from each file:
# in long format which will be convenient for shiny
lik_long <- purrr::reduce(res_list, rbind)
name_for_summary <- paste0(scenario_name,"_likelihood_averages.csv")
write.csv(lik_long, name_for_summary,row.names = FALSE)
}




p1 <-
  plot_ly(
    x =  ~ gamma,
    y =  ~ lambda_0,
    z =  ~ ave_neg_lik,
    split = factor(dat_lik$theta),
    type = "mesh3d",
    data = ,
    contour = list(
      show = TRUE,
      color = "#001",
      width = 5
    ),
    opacity = 0.5,
    showlegend = TRUE,
  #   transforms = list(
  #     list(
  #       type = 'groupby',
  #       groups = dat_lik$theta)
  # )
  )
p1
p1 %>% layout(showlegend = TRUE)
