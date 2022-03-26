## ui.R ##
library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(GGally)
library(plot3D)
source("functions.R")
G_C2 <- read.csv("C2_likelihood_averages.csv")
G_C2$s <- G_C2$s * 10 # mistake fix
G_C3 <- read.csv("C3_likelihood_averages.csv")
M_C2 <- read.csv("MLE_scenario_C2.csv")


ui <- dashboardPage(
  header = dashboardHeader(title = "Impatient customers"),


  # Sidebar ----
  sidebar = dashboardSidebar(sidebarMenu(
    menuItem(
      "Simulation",
      tabName = "simulation",
      icon = icon("dice")
    ),
    actionButton("simulation_go", "Generate Dataset!", icon = icon('play')),
    menuItem(
      "Performance",
      tabName = "performance",
      icon = icon("dashboard")
    ),
    menuItem(
      "Average Likelihood",
      tabName = "likelihood",
      icon = icon("chart-area")
    )
  )),
  # Body ----
  body =  dashboardBody(tabItems(
    ## Simulation ----
    tabItem(
      tabName = "simulation",
      ### parameter setting -----
      fluidRow(
box(collapsible = TRUE, title = "Simulation parameters",width = 4,
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
        ))),
box(collapsible = T,title = "Model parameters: arrival & patience",width = 4,
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


      )),
          title = "Rate function plot",

      plotOutput("simulation_plot_rate"),

     verbatimTextOutput("simulation_rho_text"),

      )),
### simulation plots-----

fluidRow(
  box(collapsible = TRUE, title = HTML("Likelihood - known &lambda;0 and &gamma;"),
  ),
  box(collapsible = TRUE, title = HTML("Likelihood - known &lambda;0"),
      #plotlyOutput("simulation_likelihood_known_lambda_0",width = "800px",height = "800px")
  ),
  box(collapsible = TRUE, title = HTML("Likelihood - known &gamma;"),
  ),
  box(collapsible = TRUE, title = HTML("Likelihood - known &theta;"),
  )
),

box(collapsible = TRUE, title = "Queue plots",
    plotOutput("simulation_plot_queue"),
    plotOutput("simulation_plot_customers_patience"),
    plotOutput("simulation_plot_hourly_queue")
      ),


),
      ## Performance ----
      tabItem(tabName = "performance",
              h2("Estimator performance"),
              box(collapsible = TRUE,
                  inputPanel(
                checkboxGroupInput("performance_cols", "Choose estimators:",
                                   choiceNames = names(M_C2),

                                   choiceValues =
                                     names(M_C2),
                                   selected = names(M_C2)[1:2])
                  )
              ),
              box(width = 4,
                  inputPanel(
                  checkboxInput(inputId = "performance_remove_boundary",
                                label = "remove boundary solutions"),
                  radioButtons(
                    "performance_scenario",
                    label = "select scenario:",
                    choices = paste0("C", 2:3)
                  ),
                  radioButtons(
                    inputId = "performance_s",
                    label = "no. servers",
                    choices = 1:3
                  )
                  ))
              ,

              plotOutput("performance_pairs"),

              ),

      ## Likelihood ----
      tabItem(tabName = "likelihood",
              h2("Average likelihood"),
              inputPanel(
                ### scenario:
                radioButtons(
                  "likelihood_scenario",
                  label = "select scenario:",
                  choices = paste0("C", 2:3)
                ),

                ### no. of servers (updated by scenario)
                radioButtons(
                  inputId = "likelihood_s",
                  label = "no. servers",
                  choices = 1:3
                )



              ),
              box(
              plotlyOutput("likelihood_average_plot",
                           height = "800px",
                           width = "1200px"),
              collapsible = TRUE
              )
    )
  )
)
)




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


    output$PARAMS <- renderTable({
      #data.frame(gamma = input$gamma, lambda_0 = input$lambda_0, theta = input$theta)
      as.data.frame(PARAMS(), row.names = names(PARAMS()))
    })


    output$simulation_workload <- renderText({
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
      rhos <- round(rhos,3)
      paste0("The minimum offered workload is ", rhos[1],"\n",
             "The average offered load is ", rhos[2],"\n",
             "The maximum offered load is ", rhos[3])
          } )
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
      )})

      output$simulation_likelihood_known_gamma <- renderPlotly({
        gamma <- input$gamma
        lambda_0 <- input$lambda_0
        theta <- input$theta
        AWX <- AWX()
        known <- "gamma"
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

          # likelihood for lambda_0 and theta
          M <- plot3D::mesh(Lam,The)
          x_vals <- M$x %>% as.vector()
          y_vals <- M$y %>% as.vector()
          z_vals <- negL(x_vals, y_vals)
          # plotly axis names
          axx <- list(title = "lambda_0")
          axy <- list(title = "theta")
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

              )

      output$simulation_likelihood_known_lambda_0 <- renderPlotly({
        AWX <- AWX()
        params <- PARAMS()
        p1 <- pltLik3D(params = PARAMS,AWX = AWX,known = "theta")
        p1
      })
      output$simulation_likelihood_known_theta <- renderPlotly({
        pltLik3D(params = PARAMS(),AWX = AWX(),known = "theta")
      })


    #### queue statistics plots -----

    output$simulation_plot_rate <- renderPlot({
      pltRate(gamma = input$gamma,lambda_0 = input$lambda_0)
      abline(h = input$lambda_0 + input$gamma / 2, col  = "red")
    })
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

    output$simulation_estimates <- renderTable({
      estimates <- mle(AWX = AWX(),params = PARAMS())

      estimates
    },
    width = "100%", spacing = "l")

    ## Likelihood ------------------------------------------------------------

    observeEvent(input$likelihood_scenario, {
      updateRadioButtons(inputId = "likelihood_s",
                         choices = serversScenario(input$likelihood_scenario))
    })
      # current likelihood average data
      curr_dat_lik <- reactive({

      if(input$likelihood_scenario == "C2")
        dat_lik <- G_C2
      if(input$likelihood_scenario == "C3")
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
      plot_title <- paste0("Average likelihood for ", input$likelihood_s, " servers in scenario ",
                           input$likelihood_scenario)

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
          name = paste0("theta.hat = ",round(dat_lik$theta,3))) %>%
            plotly::layout(title = plot_title,margin = m)

      p1
    })


    ## Performance ----

    observeEvent(input$performance_scenario, {
      updateRadioButtons(inputId = "performance_s",
                         choices = serversScenario(input$performance_scenario))
    })
    ### current mles:
    curr_dat_mle <- reactive({

      if(input$likelihood_scenario == "C2")
        dat_mle <- M_C2
      if(input$likelihood_scenario == "C3")
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
        aes(x = theta, y = theta.hat, color = factor(s))+
        geom_point()

      dat_mle %>%
        group_by(s) %>%
        summarise(across(c(gamma,lambda_0,theta,theta.hat,lambda.hat),.fns = list(mean)))

    })



  }


  shinyApp(ui, server)
