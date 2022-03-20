for( curr_folder in dir()){
  setwd(curr_folder)
  likelihoods <- dir()[dir() %>% grepl(pattern = "lik_grid", x = .)]
  L <- lapply(likelihoods,
              function(filename) read.csv(filename) %>% select(-1))

  for (i in 1:length(L)){
    L[[i]] %>% pull(negLogLik)
  }
  setwd("..")
}

A <- purrr::reduce(L,
                   left_join,
                   by = c("gamma","lambda_0","theta"))
dim(A)
# the likelihoods
liks <- A %>% select(starts_with("neg"))
ave_neg_lik <- rowMeans(liks)

dat_lik <-
  A %>% select(gamma,lambda_0,theta) %>%
  bind_cols(ave_neg_lik = ave_neg_lik)

write.csv(dat_lik,"C2_big_ave_lik.csv",row.names = FALSE)

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
