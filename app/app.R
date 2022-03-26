#### App for everything
library(shiny)
library(plotly)
library(tidyverse)
source("functions_for_shiny.R")

# Data --------------------------------------------------------------------

#dat_lik <- read.csv('C2_big_likelihood.csv')

dat_mle <- read.csv("MLE_scenario_C2.csv" )

dum <- read.csv("C2_likelihood_averages.csv" )
# Functions ---------------------------------------------------------------

# no. of servers from scenario
serversScenario <- function(scenario) {
  SS <- switch(
    scenario,
    "C1" = 1:4 * 10,
    "C2" = 1:4 * 10,
    "C3" = 1:5

  )
  return(SS)
}
# UI ----------------------------------------------------------------------

ui <-
  navbarPage(
    windowTitle = "Periodical Arrivals and Impatient Customers",
    title = "Periodical Arrivals and Impatient Customers",

    ## Simulations ----
    tabPanel(
      title = "Simulate",
      h4(
        "Choose parameters, then press the button to generate a realization"
      ),

      ### inputs for simulation ----

      inputPanel(
        numericInput(
          inputId = "n_obs",
          label = "n",
          value = 1000,
          min = 100,
          max = 100000,
          step = 100
        ),
        numericInput(
          inputId = "mu",
          label = "mu",
          value = 1,
          min = 0.1,
          max = 10,
          step = 0.1
        ),
        numericInput(
          inputId = "eta",
          label = "eta",
          value = 1,
          min = 0.1,
          max = 10,
          step = 0.1
        ),
        numericInput(
          inputId = "s",
          label = "s",
          value = 5,
          min = 1,
          max = 100,
          step = 1L
        ),
        numericInput(
          inputId = "gamma",
          label = "gamma",
          value = 10,
          min = 0.1,
          max = 100,
          step = 0.1
        ),
        numericInput(
          inputId = "lambda_0",
          label = "lambda_0",
          value = 10,
          min = 0.1,
          max = 100,
          step = 0.1
        ),
        numericInput(
          inputId = "theta",
          label = "theta",
          value = 1,
          min = 0.1,
          max = 10,
          step = 0.1
        ),
        tableOutput("simulation_workload")
      ),
      actionButton("simulation_go", "Simulate", icon = icon('play')),

      ### plots for the simulation -----
      fluidRow(
        column(
          width = 6,
          plotOutput("simulation_plot_queue"),
          plotOutput("simulation_plot_customers_patience")

        ),
        column(
          width = 6,
          plotOutput("simulation_plot_hourly_queue"),
          tableOutput("simulation_estimates")
        )

      ),
      plotOutput("simulation_known_likelihood")

    ),

    ## Likelihood panel ----
    tabPanel(
      title = "Average Likelihood",

      h4("Choose a scenario to plot the average likelihood function"),
      h5("For each scenario, select number of servers"),
      ### inputs for likelihood ----
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
      #plotlyOutput("likelihood_average_plot"),
      plotlyOutput("likelihood_dummy_plot")
    )
    ,
    ## Performance panel ----
    tabPanel(
      "Estimator performance",
      tableOutput("performance_summary"),
      plotOutput("performance_scatter")
    ),

    ## Known parameters panel

    tabPanel(title = "Known parameters"),
    h1("Estimation with known parameters")



  )



# Server ------------------------------------------------------------------


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
  }, caption = "offered load as a function of time")
  ### Plots ----


  output$simulation_plot_queue <- renderPlot({
    pltQueueLengthArrivals(RES(), n_customers = 100)
  })

  output$simulation_plot_customers_patience <- renderPlot({
    pltQueueByHour(RES())
  })

  output$simulation_plot_hourly_queue <- renderPlot({
    pltQueueByHourPerc(RES())
  })


  output$simulation_known_likelihood <- renderPlot({
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

  ### Point Estimation ----

  output$simulation_estimates <- renderTable({
    AWX <-  AWX()
    boris <-
      mleBoris(AWX = AWX(), PARAMS = PARAMS()) # gamma, lambda_0, theta
    liron <-
      mleLironThetaLambda(AWX = AWX()) # theta.hat, "lambda".hat
    params <- PARAMS()
    params <- params[-3]
    arrival_known  <-  mleKnownArrival(AWX, params = params)
    true_params <-
      PARAMS() # true values of gamma, lambda_0, theta
    liron_tab <- c(NA, liron[2], liron[1])
    estimates <-
      data.frame(
        true = true_params,
        boris = boris,
        liron = liron_tab,
        known_arrival = c(NA, NA, arrival_known)
      )
    estimates
  },
  width = "100%", spacing = "l")

  ## Likelihood ----

  observeEvent(input$likelihood_scenario, {
    updateRadioButtons(inputId = "likelihood_s",
                       choices = serversScenario(input$likelihood_scenario))
  })

  observe(input$likelihood_s)


  currDat <- reactive({
    dat_lik
  })


  output$likelihood_average_plot <- renderPlotly({
    p1 <-
      plot_ly(
        x =  ~ gamma,
        y =  ~ lambda_0,
        z =  ~ ave_neg_lik,
        split = factor(dat_lik$theta),
        type = "mesh3d",
        data = currDat(),
        contour = list(
          show = TRUE,
          color = "#001",
          width = 5
        ),
        opacity = 0.5,
        showlegend = TRUE
      )
    p1
  })
  dumDat <- reactive({
    dum %>%
      filter(s == input$likelihood_s)
  })

  output$likelihood_dummy_plot <- renderPlotly({
    dat <- dumDat()
    p1 <-
      plot_ly(
        x =  ~ gamma,
        y =  ~ lambda_0,
        z =  ~ ave_neg_lik,
        split = factor(dat$theta),
        type = "mesh3d",
        data = dat,
        contour = list(
          show = TRUE,
          color = "#001",
          width = 5
        ),
        opacity = 0.5,
        showlegend = TRUE
      )
    p1
  })


  ## Performance ----

  output$performance_summary <- renderTable({

  })

  output$performance_scatter <- renderPlot({
    dat_mle %>%
      filter(gamma != trunc(gamma), theta == 0.25) %>%
      ggplot() +
      aes(x = gamma , y = lambda_0, col = factor(s)) +
      geom_point()

  })



}

shinyApp(ui = ui, server = server)
