#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
setwd("~/patience/no_dash")

d3 <- read.csv("C3_lik_grid_n=50000.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plotLikelihood_C3")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


    ## Average Likelihood -----


    output$plotLikelihood_C3 <- plotly::renderPlotly({
        plot_ly(
            x  = d3$gamma,
            y = d3$lambda_0,
            z = d3$nLL_s1 ,
            split = factor(d3$theta),
            type = "mesh3d"
        )


    })
}

# Run the application
shinyApp(ui = ui, server = server)
