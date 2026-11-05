library(UsingR); data(galton); library(ggplot2); library(reshape2)

longGalton <- melt(galton, measure.vars = c("child", "parent"))
g <- ggplot(longGalton, aes(x = value)) + 
        geom_histogram(aes(y = after_stat(density), fill = variable), binwidth = 1, color = "black") + 
        geom_density(linewidth = 2)
g <- g +
        facet_grid(. ~ variable)
g

install.packages("manipulate")

library(manipulate)

myHist <- function(mu){
        g <- ggplot(galton, aes(x = child))
        g <- g + geom_histogram(fill = "salmon",
                        binwidth=1, aes(y = ..density..), color = "black")
        g <- g + geom_density(size = 2)
        g <- g + geom_vline(xintercept = mu, size = 2)
        mse <- round(mean((galton$child - mu)^2), 3)
        g <- g + labs(title = paste('mu = ', mu, ' MSE = ', mse))
        g
}

manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))


library(ggplot2)
library(manipulate)
library(UsingR) # contains the 'galton' dataset

myHist <- function(mu) {
  mse <- round(mean((galton$child - mu)^2), 3)
  
  ggplot(galton, aes(x = child)) +
    geom_histogram(aes(y = after_stat(density)), fill = "salmon", color = "black", binwidth = 1) +
    geom_density(linewidth = 1.5) +
    geom_vline(xintercept = mu, color = "red", linewidth = 1.5, linetype = "dashed") +
    labs(
      title = paste0("Mean (mu): ", mu, " | MSE: ", mse),
      x = "Child Height (inches)",
      y = "Density"
    ) +
    theme_minimal()
}

manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

library(shiny)
library(ggplot2)
library(UsingR) # dataset

ui <- fluidPage(
  titlePanel("Galton Height Mean & MSE"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("mu", "Mean Estimate (mu):", min = 62, max = 74, value = 68, step = 0.5)
    ),
    mainPanel(
      plotOutput("histPlot")
    )
  )
)

server <- function(input, output) {
  output$histPlot <- renderPlot({
    mse <- round(mean((galton$child - input$mu)^2), 3)
    
    ggplot(galton, aes(x = child)) +
      geom_histogram(aes(y = after_stat(density)), fill = "salmon", color = "black", binwidth = 1) +
      geom_density(linewidth = 1.5) +
      geom_vline(xintercept = input$mu, color = "red", linewidth = 1.5, linetype = "dashed") +
      labs(title = paste0("mu = ", input$mu, " | MSE = ", mse)) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

p = seq(0 , 1, length = 1000)
y = p * (1 - p)
plot(p, y, type = "l", lwd = 3, frame = FALSE)
