library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(GGally)
library(plot3D)
library(psych)
source("~/patience/R/functions.R")
G_C1 <- read.csv("C1_likelihood_averages.csv")
G_C1$s <- G_C1$s * 10
G_C2 <- read.csv("C2_likelihood_averages.csv")
G_C2$s <- G_C2$s * 10 # mistake fix
G_C3 <- read.csv("C3_likelihood_averages.csv")
M_C2 <- read.csv("MLE_scenario_C2.csv")


# UI ----
ui <- navbarPage("Periodic arrivals of impatient customers",
                 mainPanel(tabsetPanel(
                   tabPanel(
                     title = "Introduction",
                     icon = icon("book"),

                     h2("Welcome to the app"),
                     h4(
                       "This app contains all the recent results from my work.
                                You can switch to different views using the tabs above."
                     ),
                     h5("List of contents:"),
                     HTML(
                       "<ul><li> Simulation - simulate a realization</li>
                                 <li>MLE performance - examined in various conditions</li>
                                 <li>Average likelihood - study shape of the likelihood function</li>
                                 </ul>"
                     )


                   ),


                   tabPanel(
                     ## Simulation tab ----------------------------------------------------------

                     title = "Simulation",
                     icon = icon("dice"),
                     fluidPage(
                       fluidRow(
                         inputPanel(
                           helpText('number of effective arrivals'),
                           numericInput(
                             inputId = "n_obs",
                             label = "n",
                             value = 1000,
                             min = 100,
                             max = 100000,
                             step = 100
                           ),
                           helpText("Job size shape parameter"),
                           numericInput(
                             inputId = "mu",
                             label = HTML("&mu;"),
                             value = 1,
                             min = 0.1,
                             max = 10,
                             step = 0.1
                           ),
                           helpText("Job size rate parameter"),
                           numericInput(
                             inputId = "eta",
                             label =  HTML("&eta;"),
                             value = 1,
                             min = 0.1,
                             max = 10,
                             step = 0.1
                           ),
                           helpText("Number of servers"),
                           numericInput(
                             inputId = "s",
                             label = "s",
                             value = 5,
                             min = 1,
                             max = 100,
                             step = 1L
                           )
                         ),
                         inputPanel(
                           helpText('Periodic rate ("AC")'),

                           sliderInput(
                             inputId = "gamma",
                             label = HTML("&gamma;"),
                             value = 10,
                             min = 0.1,
                             max = 100,
                             step = 0.1
                           ),
                           helpText('Constant rate ("DC")'),
                           sliderInput(
                             inputId = "lambda_0",
                             label = HTML("&lambda;0"),
                             value = 10,
                             min = 0.1,
                             max = 100,
                             step = 0.1
                           ),
                           helpText('Patience parameter'),
                           numericInput(
                             inputId = "theta",
                             label = HTML("&theta;"),
                             value = 1,
                             min = 0.1,
                             max = 10,
                             step = 0.1
                           )
                         )

                       ),
                       fluidRow(
                         column(
                           width = 4,
                           helpText("Click the button to make a realization"),
                           actionButton("simulation_go", "Generate Dataset!", icon = icon('play')),
                           helpText("Offered workload for selected parameters:"),
                           tableOutput("simulation_workload")
                         ),
                         column(width = 8,
                                plotOutput("simulation_plot_rate"))
                       ),

                       ### sim. estimates -----
                       fluidRow(
                         inputPanel(
                           checkboxGroupInput(
                             "simulation_estimators",
                             "Choose estimators",
                             choiceNames = c(
                               "Boris",
                               "Liron",
                               "Known Arrival",
                               "Known gamma",
                               "Known lambda_0",
                               "Known theta"
                             ),
                             choiceValues = c("boris", "liron", "KAr", "KG", "KL", "KT"),
                             selected = c("boris", "liron")
                           )
                         ),
                         tableOutput("simulation_estimates")
                       ),
                       fluidRow(
                         column(
                           4,
                           tableOutput("simulation_estimates_boris"),
                           tableOutput("simulation_estimates_liron")
                         ),
                         column(
                           8,
                           tableOutput("simulation_estimates_known_arrival"),
                           tableOutput("simulation_estimates_known_gamma"),
                           tableOutput("simulation_estimates_known_lambda_0"),
                           tableOutput("simulation_estimates_known_theta")
                         )
                       ),


                       ### sim. plots --------

                       fluidRow(
                         h5("Plot of two-parameter likelihood:"),
                         inputPanel(
                           radioButtons(
                             inputId = "simulation_known",
                             label = "Choose known parameter",
                             choices = c("lambda_0", "gamma", "theta")
                           )
                         ),
                         plotlyOutput("simulation_known_likelihood")

                       ),
                       fluidRow(
                         column(width = 4,       plotOutput("simulation_plot_queue")),
                         column(width = 4,  plotOutput("simulation_plot_customers_patience")),
                         column(width = 4,  plotOutput("simulation_plot_hourly_queue"))
                       )
                     )
                   )   ,

                   ## Performance tab -------------------------
                   tabPanel(
                     title = "MLE performance",
                     icon = icon("dashboard"),

                     h3("simulation study"),
                     h4("Estimator performance"),

                     inputPanel(
                       checkboxInput(inputId = "performance_remove_boundary",
                                     label = "remove boundary solutions"),
                       radioButtons(
                         "performance_scenario",
                         label = "select scenario:",
                         choices = paste0("C", 1:3)
                       ),
                       radioButtons(
                         inputId = "performance_s",
                         label = "no. servers",
                         choices = 1:3
                       ),
                       checkboxGroupInput(
                         "performance_cols",
                         "Choose estimators:",
                         choiceNames = names(M_C2) %>% setdiff(c("boundary", "s")),

                         choiceValues =
                           names(M_C2) %>% setdiff(c("boundary", "s")),
                         selected = names(M_C2)[1:2]
                       )
                     ),



                     plotOutput("performance_pairs")

                   ),

                   tabPanel(
                     title = "Average Likelihood",
                     icon = icon('chart-area'),
                     ## Likelihood tab --------------
                     inputPanel(
                       ### scenario:
                       radioButtons(
                         "likelihood_scenario",
                         label = "select scenario:",
                         choices = paste0("C", 1:3)
                       ),

                       ### no. of servers (updated by scenario)
                       radioButtons(
                         inputId = "likelihood_s",
                         label = "no. servers",
                         choices = 1:3
                       )

                     ),

                     plotlyOutput("likelihood_average_plot",height = "800px",width = "100%")


                   )




                 )))








# Server ----

server <- function(input, output, session) {
  ## Simulation ----
  RES <- eventReactive(input$simulation_go, {
    eta <- input$eta %>% as.integer()
    mu <- input$mu %>% as.integer()
    nservers <- input$s %>% as.integer()
    n_obs <- input$n_obs %>% as.integer()
    gamma <- input$gamma %>% as.integer()
    lambda_0 <- input$lambda_0 %>% as.integer()
    theta <- input$theta %>% as.integer()
    PARAMS <- c(gamma, lambda_0, theta)

    RES <- resSimCosine(
      n = n_obs,
      gamma = input$gamma,
      lambda_0 = input$lambda_0,
      theta = input$theta,
      s = input$s,
      eta = input$eta,
      mu = input$mu
    )
    RES
  })

  AWX <- reactive({
    data.frame(A = RES()$Aj,
               W = RES()$Wj,
               X = RES()$Xj)
  })

  PARAMS <- reactive({
    gamma <- input$gamma %>% as.numeric()
    lambda_0 <- input$lambda_0 %>% as.numeric()
    theta <- input$theta %>% as.numeric()
    PARAMS <- c("gamma" = gamma,
                "lambda_0" = lambda_0,
                "theta" = theta)
    PARAMS
  })

  observe(head(AWX()))


  output$PARAMS <- renderTable({
    #data.frame(gamma = input$gamma, lambda_0 = input$lambda_0, theta = input$theta)
    as.data.frame(PARAMS(), row.names = names(PARAMS()))
  })


  output$simulation_workload <- renderTable({
    gamma <- input$gamma
    lambda_0 <- input$lambda_0
    eta <- input$eta
    mu <- input$mu
    s <- input$s
    min_rate <- lambda_0
    max_rate <- lambda_0 + gamma # not divided by two!
    ave_rate <- lambda_0 + gamma / 2
    rates <-
      c("minimum" = min_rate,
        "average" = ave_rate,
        "maximum" = max_rate)

    rhos <- c(rates["minimum"] * eta / (s * mu),
              rates["average"] * eta / (s * mu),
              rates["maximum"] * eta / (s * mu))

    data.frame(t(rhos))
  })

  output$simulation_rho_text <- renderText({
    gamma <- input$gamma
    lambda_0 <- input$lambda_0
    eta <- input$eta
    mu <- input$mu
    s <- input$s
    min_rate <- lambda_0
    max_rate <- lambda_0 + gamma # not divided by two!
    ave_rate <- lambda_0 + gamma / 2
    rates <-
      c("minimum" = min_rate,
        "average" = ave_rate,
        "maximum" = max_rate)

    rhos <- c(rates["minimum"] * eta / (s * mu),
              rates["average"] * eta / (s * mu),
              rates["maximum"] * eta / (s * mu))
    rhos <- round(rhos, 3)
    paste0(
      "The minimum offered workload is ",
      rhos[1],
      "\n",
      "The average offered load is ",
      rhos[2],
      "\n",
      "The maximum offered load is ",
      rhos[3]
    )
  })
  ### known likelihoods plots ----

  output$simulation_likelihood_known_arrival <- renderPlot({
    curve(
      negLogLikelihoodMean.KnownArrival(
        theta.vec = x,
        params = c(input$gamma, input$lambda_0),
        AWX = AWX()
      ),
      from = input$theta / 4,
      to = input$theta * 4,
      ylab = "-LogLik",
      xlab = expression(theta)
    )
  })

  output$simulation_likelihood_known_gamma <- renderPlotly({
    AWX <- AWX()
    params <- PARAMS()
    p1 <- pltLik3D(params = PARAMS(),
                   AWX = AWX(),
                   known = "gamma")
    p1

  })

  output$simulation_likelihood_known_lambda_0 <- renderPlotly({
    AWX <- AWX()
    params <- PARAMS()
    p1 <- pltLik3D(params = PARAMS(),
                   AWX = AWX,
                   known = "lambda_0")
    p1
  })
  output$simulation_likelihood_known_theta <- renderPlotly({
    pltLik3D(params = PARAMS(),
             AWX = AWX(),
             known = "theta")
  })
  output$simulation_known_likelihood <- renderPlotly({
    pltLik3D(
      params = PARAMS(),
      AWX = AWX(),
      known = input$simulation_known
    )
  })


  #### queue statistics plots -----

  output$simulation_plot_rate <- renderPlot({
    pltRate(gamma = input$gamma, lambda_0 = input$lambda_0)
    abline(h = input$lambda_0 + input$gamma / 2, col  = "red")
  }, height = 300)
  output$simulation_plot_queue <- renderPlot({
    pltQueueLengthArrivals(RES(), n_customers = 100)
  })

  output$simulation_plot_customers_patience <- renderPlot({
    pltQueueByHour(RES())
  })

  output$simulation_plot_hourly_queue <- renderPlot({
    pltQueueByHourPerc(RES())
  })








  ### Point Estimation ----


  estimates <- reactive({
    mle(AWX = AWX(), params = PARAMS()) %>% rownames_to_column(var = "estimator")
  })

  estimates_table <-
    reactive({
      estimators_regex <-
        paste0(input$simulation_estimators, collapse = "|") # whuich estimators selected
      estimates_data <- estimates()
      estimates_data[estimates_data$estimator %>% str_which(estimators_regex),]
    })

  output$simulation_estimates <- renderTable({
    estimates_table()
  })

  # output$simulation_estimates_boris <- renderTable({
  #   estimates() %>% filter(str_detect(estimator,"boris"))
  # },
  # width = "20%", spacing = "l",rownames = FALSE,bordered = T,align = "c")
  #
  # output$simulation_estimates_liron <- renderTable({
  #   estimates() %>% filter(str_detect(estimator,"liron"))
  # },
  # width = "15%", spacing = "l",rownames = FALSE,bordered = T,align = "c")
  #
  # output$simulation_estimates_known_arrival <- renderTable({
  #   estimates() %>% filter(str_detect(estimator,"KAr"))
  # },
  # width = "15%", spacing = "l",rownames = FALSE,bordered = T,align = "c")
  #
  #
  # output$simulation_estimates_known_gamma <- renderTable({
  #   estimates() %>% filter(str_detect(estimator,"KG"))
  # },
  # width = "15%", spacing = "l",rownames = FALSE,bordered = T,align = "c")
  #
  # output$simulation_estimates_known_lambda_0 <- renderTable({
  #   estimates() %>% filter(str_detect(estimator,"KL"))
  # },
  # width = "15%", spacing = "l",rownames = FALSE,bordered = T,align = "c")
  #
  # output$simulation_estimates_known_theta <- renderTable({
  #   estimates() %>% filter(str_detect(estimator,"KT"))
  # },
  # width = "15%", spacing = "l",rownames = FALSE,bordered = T,align = "c")








  ## Likelihood ------------------------------------------------------------

  observeEvent(input$likelihood_scenario, {
    updateRadioButtons(inputId = "likelihood_s",
                       choices = serversScenario(input$likelihood_scenario))
  })
  # current likelihood average data
  curr_dat_lik <- reactive({
    if (input$likelihood_scenario == "C1")
      dat_lik <- G_C1
    if (input$likelihood_scenario == "C2")
      dat_lik <- G_C2
    if (input$likelihood_scenario == "C3")
      dat_lik <- G_C3

    dat_lik %>%
      filter(s == input$likelihood_s)

  })



  output$likelihood_average_plot <- renderPlotly({
    dat_lik <- curr_dat_lik()
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 40,
      pad = 10
    )
    plot_title <-
      paste0(
        "Average likelihood for ",
        input$likelihood_s,
        " servers in scenario ",
        input$likelihood_scenario
      )

    p1 <-
      plot_ly(
        x =  ~ gamma,
        y =  ~ lambda_0,
        z =  ~ ave_neg_lik,
        split = factor(dat_lik$theta),
        type = "mesh3d",
        data = dat_lik,
        contour = list(
          show = TRUE,
          color = "#001",
          width = 5
        ),
        opacity = 0.5,
        showlegend = TRUE,
        name = paste0("theta.hat = ", round(dat_lik$theta, 3))
      ) %>%
      plotly::layout(title = plot_title, margin = m)

    p1
  })


  ## Performance ----

  observeEvent(input$performance_scenario, {
    updateRadioButtons(inputId = "performance_s",
                       choices = serversScenario(input$performance_scenario))
  })
  ### current mles:
  curr_dat_mle <- reactive({
    if (input$performance_scenario == "C2")
      dat_mle <- M_C2
    if (input$performance_scenario == "C3")
      dat_mle <- M_C3


    if (input$performance_remove_boundary)
      dat_mle <- dat_mle %>% filter(!boundary)

    dat_mle %>%
      filter(s == input$performance_s)


  })

  output$performance_pairs <- renderPlot({
    dat_mle <- curr_dat_mle()
    dat_mle %>%
      select(input$performance_cols) %>%
      GGally::ggpairs()
  })




  output$performance_scatter <- renderPlot({
    dat_mle <- curr_dat_mle()
    dat_mle %>% filter(s < 30) %>%
      ggplot() +
      aes(x = theta,
          y = theta.hat,
          color = factor(s)) +
      geom_point()

    dat_mle %>%
      group_by(s) %>%
      summarise(across(
        c(gamma, lambda_0, theta, theta.hat, lambda.hat),
        .fns = list(mean)
      ))

  })



}

shinyApp(ui, server)







