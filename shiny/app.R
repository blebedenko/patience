## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(filenamer)
library(patience)

# Shiny -------------------------------------------------------------------




## UI ----------------------------------------------------------------------



ui <- dashboardPage(
  ### Header ----
  dashboardHeader(title = "tit"),
  ### Sidebar ----
  dashboardSidebar(title = "Simulation parameters",

                   ### inputs regarding the simulation ----
                   numericInput(inputId = "n_obs",
                                label = "n",
                                value = 1000,
                                min = 100,
                                max = 100000,
                                step = 100),
                   numericInput(inputId = "mu",
                                label = "mu",
                                value = 1,
                                min = 0.1,
                                max = 10,
                                step = 0.1),
                   numericInput(inputId = "eta",
                                label = "eta",
                                value = 1,
                                min = 0.1,
                                max = 10,
                                step = 0.1),
                   numericInput(inputId = "s",
                                label = "s",
                                value = 5,
                                min = 1,
                                max = 100,
                                step = 1L),
                   numericInput(inputId = "gamma",
                                label = "gamma",
                                value = 10,
                                min = 0.1,
                                max = 100,
                                step = 0.1),
                   numericInput(inputId = "lambda_0",
                                label = "lambda_0",
                                value = 10,
                                min = 0.1,
                                max = 100,
                                step = 0.1),
                   numericInput(inputId = "theta",
                                label = "theta",
                                value = 1,
                                min = 0.1,
                                max = 10,
                                step = 0.1)



  ),
  ## Body ----
  dashboardBody(fluidRow(
    ### Plots -----
    box(title = "The sim",
        tableOutput("PARAMS"),
        plotOutput("plot1"),
        plotOutput("plot_rate")
    ),
    box(title = "The data",
        plotOutput("plot_res")
    )
  ))
)

# Server ------------------------------------------------------------------


server <- function(input, output) {

  # geenrate queue data:

  RES <- reactive({

    # eta <- input$eta %>% as.numeric()
    # mu <- input$mu %>% as.numeric()
    # nservers <- input$s %>% as.numeric()
    # n.obs <- input$n.obs %>% as.numeric()
    # gamma <- input$gamma %>% as.numeric()
    # lambda_0 <- input$lambda_0 %>% as.numeric()
    # theta <- input$theta %>% as.numeric()
    # PARAMS <- c(gamma,lambda_0,theta)
    eta <- 1 %>% as.numeric()
    mu <- 1 %>% as.numeric()
    nservers <- 5 %>% as.numeric()
    n_obs <- 1000 %>% as.numeric()
    gamma <- 10 %>% as.numeric()
    lambda_0 <- 10 %>% as.numeric()
    theta <- 1 %>% as.numeric()
    PARAMS <- c(gamma,lambda_0,theta)

    RES <- resSimCosine(n = n_obs,gamma = gamma,lambda_0 = lambda_0,theta = theta,s = nservers,eta = eta,mu = mu)
    RES$Aj
  })

  output$PARAMS <- renderTable({
    data.frame(gamma = input$gamma, lambda_0 = input$lambda_0, theta = input$theta)
  })

  output$AWX <- renderTable(data.frame(1))

  output$plot_res <- renderPlot({plot(RES())})
  # output$plot1 <- renderPlot({
  #   RES1 <- RES()
  #   hist(RES1$Aj)
  #     })




}


shinyApp(ui, server)
#
# mu <- 1
# s <- 6
# # EB (Job size epxectation)eta/mu
# n <- 20000
# gamma <- 1
# lambda_0 <- 10
# theta <- 1
# (rho <- (gamma + lambda_0/2) * eta / (s*mu))
