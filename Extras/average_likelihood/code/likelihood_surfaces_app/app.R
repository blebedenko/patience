library(shiny)
library(plotly)
library(GGally)
d5 <- read.csv("big grid results for s=5.csv" )
mle5 <- read.csv("mles for s=5.csv")
mle10 <- read.csv("mles for s=10.csv")
mle20 <- read.csv("mles for s=20.csv")

liron5 <- read.csv("Liron mles for s=5.csv")
liron10 <- read.csv("Liron mles for s=10.csv")
liron20 <- read.csv("Liron mles for s=20.csv")
# d10 <- read.csv("big grid results for s=10.csv" )
# d20 <- read.csv( "big grid results for s=20.csv"  )

ui <- fluidPage(
  titlePanel("Average likelihood from 2000 datasets of n=20001"),

  inputPanel(
    radioButtons("which_plot",
                 "Which scenarios to plot? s = 5,10,20",
                 choiceNames = c("s=5","s=10","s=20"),
                 choiceValues = c(5L,10L,20L))

  ),
  fluidRow(
    h4("Distrubution and correlation of the realized MLE's"),
    h5("In the optimization scheme - the parameters are assumed to lie in"),
  withMathJax(h5("$$[\\gamma \\cdot 0.1, \\gamma \\cdot 10]\\times [\\lambda_0 \\cdot 0.1, \\lambda_0 \\cdot 10] \\times [\\theta \\cdot 0.1, \\theta \\cdot 10]$$")),
  column(
    h4("Boris:"),
    plotOutput('mle_plot'),
         width = 6),
  column(
    h4("Liron:"),
      plotOutput("Liron_plot"),
         width = 6)
  ),
  fluidRow(
      h5("performance of MLE:"),
    withMathJax(h5("$$MAE(\\theta) = 100\\% \\cdot E\\ \\Big[ \\frac{|\\hat{\\theta} - \\theta |}{\\theta}\\Big]  $$")),
    tableOutput("performance"),
    h5('"Liron" estimates the parameters:'),
    withMathJax(h5("$$ \\lambda' = \\lambda_0 + \\frac{\\gamma}{2} = 30 \\quad
                   \\theta' = \\theta = 2.5$$ ")),
    tableOutput("Liron_performance")
  ),
  fluidRow(
    withMathJax(h4("$$\\lambda(t) = \\lambda_0 + \\frac{\\gamma}{2}(cos2\\pi t + 1)$$")),
    withMathJax(h4("true values: $$\\lambda_0 = 10, \\gamma = 40, \\theta = 2.5, E(B)=1$$")),
    withMathJax(h5("$$\\text{Patience:}\\quad Y \\sim Exp(\\theta)$$"))
  ),
  inputPanel(
    radioButtons("theta_value", label = "Choose a value for theta",
                 choiceNames =  as.character(1.25 + 0:8 * 0.3125),
                 choiceValues =  1.25 + 0:8 * 0.3125)
  ),
  fluidRow(
    h3("The average likelihood function (currently only) for  s=5"),
    textOutput('s_title'),
    h4("The MLE's, given the current value of theta:"),
    tableOutput('mle'),
    plotlyOutput('plotLikelihood',width = "100%"),
    h4("red dot shows the minimum")
  )


)



server <- function(input, output) {

  PARAMS <- c(gamma = 40, lambda_0 = 10, theta = 2.5)
  d5R <- reactive({d5 %>% filter(theta<= input$theta_value, theta == input$theta_value)})
  d10R <- reactive({d10 %>% filter(theta<= input$theta_value, theta == input$theta_value)})
  d10R <- reactive({d10 %>% filter(theta<= input$theta_value, theta == input$theta_value)})
  currDat <- reactive({
    if(input$which_plot == 5)
      Dat <- d5R()
    # if(input$which_plot == 10)
    #   Dat <- d10R()
    # if(input$which_plot == 20)
    #   Dat <- d10R()

    Dat

  })

  output$plotLikelihood <- renderPlotly({
    dat <-currDat()
    p1 <- plot_ly(x=~gamma,y=~lambda_0,z=~negLogLik,type="mesh3d",data =dat,split = factor(dat$theta ),
                  contour = list(show = TRUE,color = "#001",width = 5), opacity = 0.5)
    mle <- dat[which.min(dat$negLogLik),1:4]

    p1 %>%
      add_trace(data = mle,
                type = "scatter3d",mode="markers",
                x=~gamma,y=~lambda_0,z=~negLogLik,
                marker = list(color="red"))

  }
  )
  output$s_title <- renderText({
    paste0("s = ", input$which_plot)
  })
  output$mle <- renderTable({
    dat <-currDat()
    dat %>% arrange(negLogLik) %>% head(1)
  },digits = 5)

  mleDat <- reactive({
    if(input$which_plot == 5L)
      Dat <- mle5
    if(input$which_plot == 10L)
      Dat <- mle10
    if(input$which_plot == 20L)
      Dat <- mle20

    Dat

  })

  LironDat <- reactive({
    if(input$which_plot == 5L)
      Dat <- liron5
    if(input$which_plot == 10L)
      Dat <- liron10
    if(input$which_plot == 20L)
      Dat <- liron20

    Dat

  })

  output$mle_plot <- renderPlot({
    GGally::ggpairs(mleDat())

  })
  # pointwise estimation performance
  output$performance <- renderTable({

    currMLE <- mleDat()
    # matrix of the true parameter values
    pars <- matrix(rep(PARAMS,nrow(currMLE)),ncol = 3, byrow = T)

    bias <- colMeans(currMLE - pars)
    SD <- apply(currMLE, 2, sd)
    RMSE <- sqrt(colMeans((currMLE - pars)^2))
    MAE <- colMeans(abs((currMLE - pars))/pars) * 100

    data.frame(bias,SD,RMSE,MAE)

  },rownames = T)

  output$Liron_plot <- renderPlot({
    liron <- LironDat()
    GGally::ggpairs(liron)
  })

  output$Liron_performance <- renderTable({
    liron <- LironDat()
    pars <- c(2.5,30)
    pars <- matrix(rep(pars,nrow(liron)),ncol = 2, byrow = T)

    bias <- colMeans(liron - pars)
    SD <- apply(liron, 2, sd)
    RMSE <- sqrt(colMeans((liron - pars)^2))
    MAE <- colMeans(abs((liron - pars))/pars) * 100
    data.frame(bias,SD,RMSE,MAE)


  },rownames = T)


}
#server <- function(input, output){rnorm(1)}
shinyApp(ui,server)
