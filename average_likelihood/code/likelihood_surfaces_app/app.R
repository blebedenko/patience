library(shiny)
library(plotly)

d5 <- read.csv("big grid results for s=5.csv" )
d10 <- read.csv("big grid results for s=10.csv" )
d10 <- read.csv( "big grid results for s=20.csv"  )
#d10 <- read.csv( "results for s=10.csv" )

ui <- fluidPage(
  titlePanel("Average likelihood from 2000 datasets of n=20001"),

  inputPanel(
    radioButtons("which_plot",
                 "Which scenarios to plot? s = 10,20,30",
                 choiceNames = c("s=5","s=10","s=20"),
                 choiceValues = c(5L,10L,20L)),
    radioButtons("theta_value", label = "Choose a value for theta",
                 choiceNames =  as.character(1.25 + 0:8 * 0.3125),
                 choiceValues =  1.25 + 0:8 * 0.3125)
  ),

  fluidRow(
    withMathJax(h4("$$\\lambda(t) = \\lambda_0 + \\frac{\\gamma}{2}(cos2\\pi t + 1)$$")),
    withMathJax(h4("true values: $$\\lambda_0 = 10, \\gamma = 40, \\theta = 2.5, E(B)=1$$")),
    h4("exponential patience"),
    textOutput('s_title'),
    h4("The MLE's, given the current value of theta:"),
    tableOutput('mle'),
    plotlyOutput('plotLikelihood',width = "100%")
  )
)



server <- function(input, output) {


  d5R <- reactive({d5 %>% filter(theta<= input$theta_value, theta == input$theta_value)})
  d10R <- reactive({d10 %>% filter(theta<= input$theta_value, theta == input$theta_value)})
  d10R <- reactive({d10 %>% filter(theta<= input$theta_value, theta == input$theta_value)})
  currDat <- reactive({
    if(input$which_plot == 5)
      Dat <- d5R()
    if(input$which_plot == 10)
      Dat <- d10R()
    if(input$which_plot == 20)
      Dat <- d10R()

    Dat

  })

  output$plotLikelihood <- renderPlotly({
    dat <-currDat()
    p1 <- plot_ly(x=~gamma,y=~lambda_0,z=~negLogLik,type="mesh3d",data =dat,split = factor(dat$theta ),
                  contour = list(show = TRUE,color = "#001",width = 5), opacity = 0.5)
    p1
  }
  )
  output$s_title <- renderText({
    paste0("s = ", input$which_plot)
  })
  output$mle <- renderTable({
    dat <-currDat()
    dat %>% arrange(negLogLik) %>% head(1)
  },digits = 5)


}
#server <- function(input, output){rnorm(1)}
shinyApp(ui,server)
