# app for logistic map tinkering

# plotting beta densities for different 'alpha', 'beta' values
library(shiny)
library(bslib)
library(ggplot2)

# Define UI ----
ui <- page_sidebar(
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    sliderInput(
      inputId = "x1",
      label = "initial value",
      min = 0.01,
      max = 0.99,
      value = 0.1
    ),
    sliderInput(
      inputId = "T",
      label = "timesteps",
      min = 10,
      max = 100,
      value = 30
    ),
    sliderInput(
      inputId = "r",
      label = "parameter 'r'",
      min = 1,
      max = 4.5,
      value = 1
    ),
  ),
  # Output: Plots
  plotOutput(outputId = "plots")
)

# Define server logic ----
server <- function(input, output) {
  output$plots <- renderPlot({
    T <- input$T
    x1 <- input$x1
    r <- input$r
    
    x <- rep(NA, T)
    x[1] <- x1
    for (t in 1:(T-1)) {
      x[t+1] = r*x[t]*(1-x[t])
    }
    

    ggplot(data.frame( time = 1:T, x = x ), aes(x=time, y=x)) +
    geom_point(size = 2, colour = "navyblue") +
    geom_line(alpha = 0.1, size = 1, colour = "navyblue") +
    coord_cartesian(ylim = c(0,1)) +
    theme_classic() +
    theme(
      aspect.ratio = 0.7
    ) 
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

