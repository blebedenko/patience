## app.R ##

library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(filenamer)
library(patience)

# read average likelihood data:
d3 <- read_csv("C3_lik_grid_n=50000.csv")
# Shiny -------------------------------------------------------------------




## UI ----------------------------------------------------------------------



ui <- shinydashboard::dashboardPage(
  ### Header ----
  dashboardHeader(title = "tit"),
  ### Sidebar ----
  dashboardSidebar(
    title = 'Set parameters and press "Simulate"',

    ### inputs regarding the simulation ----
    actionButton("go", "Simulate", icon = icon('play')),
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
    )



  ),
  ## Body ----
  dashboardBody(
    fluidRow(
    ### Plots -----
    box(
      title = "Arrivals and queue length",
      collapsible = TRUE,
      plotOutput("plot_queue")

    ),
    box(
      title = "Arrivals by patience",
      collapsible = TRUE,
      plotOutput("plot_customers_patience")
    ),
    box(
      title = "Effective Arrivals' patience distribution by time",
      collapsible = TRUE,
      plotOutput("plot_hourly_queue")
    ),
    box(title = "Likelihood (known arrival rate)",collapsible = TRUE,
        h4("The likelihoods are averages (instead of sums)"),
        plotOutput("likelihood")
  ),
  ### Estimation ----
  fluidRow(
    box(
      title = "Parameter estimation",
      collapsible = TRUE,
      h4("True parameter values:"),
      tableOutput("true_parameters"),
      h4("Boris (all parameters uknown):"),
      tableOutput("estimates_Boris"),
      hr(),
      h4("Boris (arrival rate function known):"),
      tableOutput("estimates_Known_Arrival"),
      hr(),
      h4("Liron:"),
      tableOutput("estimates_Liron")
    ),
    box(title = "Queue statistics",
        "what")
  ),
  ### Average likelihood ------
  fluidRow(
    column(
      h3("Average likelihood function"),width = 12
    ),
    box(title = "Case 3",

        radioButtons(inputId = "case",label = "Which simulation case?",choices = c("C1","C2","C3"))),
numericInput(inputId = "ave_s",label = "no. servers",value = 1,min = 1,max = 100,step = 1L),
        plotOutput("plotLikelihood_C3")
  ))
)
)

# Server ------------------------------------------------------------------


server <- function(input, output) {
  # generate queue data on button press:

  RES <- eventReactive(input$go, {
    eta <- input$eta %>% as.integer()
    mu <- input$mu %>% as.integer()
    nservers <- input$s %>% as.integer()
    n_obs <- input$n_obs %>% as.integer()
    gamma <- input$gamma %>% as.integer()
    lambda_0 <- input$lambda_0 %>% as.integer()
    theta <- input$theta %>% as.integer()
    PARAMS <- c(gamma, lambda_0, theta)
    # eta <- 1 %>% as.numeric()
    # mu <- 1 %>% as.numeric()
    # nservers <- 5 %>% as.numeric()
    # n.obs <- 1000 %>% as.numeric()
    # gamma <- 10 %>% as.numeric()
    # lambda_0 <- 10 %>% as.numeric()
    # theta <- 1 %>% as.numeric()

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

  ## Plots ----
  output$plot_queue <- renderPlot({
    pltQueueLengthArrivals(RES(), n_customers = 100)
  })

  output$plot_customers_patience <- renderPlot({
    pltQueueByHour(RES())
  })

  output$plot_hourly_queue <- renderPlot({
    pltQueueByHourPerc(RES())
  })

  ## Estimation ----
  output$true_parameters <- renderTable({
    gamma <- input$gamma %>% as.integer()
    lambda_0 <- input$lambda_0 %>% as.integer()
    theta <- input$theta %>% as.integer()
    PARAMS <- c(gamma, lambda_0, theta)
    t(data.frame(PARAMS()))
  })
  output$estimates_Boris <- renderTable({
    AWX <-  AWX()
    boris <- mleBoris(AWX = AWX(), PARAMS = PARAMS())
    data.frame(gamma_B = boris[1],
               lambda0_B = boris[2],
               theta_B = boris[3])
  })

  output$estimates_Liron <- renderTable({
    liron <- mleLironThetaLambda(AWX = AWX())

    data.frame(theta_L = liron[1],
               lambda_L = liron[2])

  })

  output$estimates_Known_Arrival <- renderTable({
    params <- PARAMS()
    params <- params[-3]
    data.frame(theta_Known = mleKnownArrival(AWX(), params = params))
  })


  output$likelihood <- renderPlot({
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

  ## Average Likelihood -----


  output$plotLikelihood_C3 <- renderPlotly({
    dat <- d3 %>% select(1:3, contains(as.character(input$ave_s)))
    p1 <- plot_ly(x=~gamma,y=~lambda_0,z=~nLL_s1,type="mesh3d",data =d3,split = factor(d3$theta ),
                  contour = list(show = TRUE,color = "#001",width = 5), opacity = 0.5)
    p1
  }
  )




}



# Run ---------------------------------------------------------------------


shinyApp(ui, server)
