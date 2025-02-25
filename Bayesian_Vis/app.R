library(shiny)
library(ggplot2)
library(LearnBayes)

ui <- fluidPage(
  titlePanel("Beta-Binomial Posterior Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("alpha", "Alpha:", min = 1, max = 20, value = 2.5, step = 0.5),
      sliderInput("beta", "Beta:", min = 1, max = 50, value = 10, step = 0.5),
      sliderInput("n", "Sample Size (n):", min = 1, max = 500, value = 50, step = 1),
      sliderInput("y", "Number of Successes (y):", min = 0, max = 500, value = 7, step = 1)
    ),
    
    mainPanel(
      plotOutput("posteriorPlot")
    )
  )
)

server <- function(input, output) {
  
  output$posteriorPlot <- renderPlot({
    alpha <- input$alpha
    beta <- input$beta
    n <- input$n
    y <- input$y
    
    pi <- seq(0.005, 0.995, length = 1000)
    
    prior <- dbeta(pi, alpha, beta)
    likelihood <- dbinom(y, size = n, prob = pi)
    
    posterior <- dbeta(pi, alpha + y, beta + n - y)
    
    data <- data.frame(
      pi = pi,
      Prior = prior,
      Likelihood = likelihood,
      Posterior = posterior
    )
    
    ggplot(data, aes(x = pi)) +
      geom_line(aes(y = Prior, color = "Prior"), size = 1) +
      geom_line(aes(y = Posterior, color = "Posterior"), size = 1) +
      labs(
        title = "Prior and Posterior Distributions",
        x = expression(pi),
        y = "Density",
        color = "Distribution"
      ) +
      theme_minimal() +
      scale_color_manual(values = c("Prior" = "red", "Posterior" = "blue"))
  })
}

shinyApp(ui = ui, server = server)
