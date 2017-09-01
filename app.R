library(shiny)
# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Parameters"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      sliderInput("I", "Investment:",
                  min = 0, max = 0.5,
                  value = 0.13),
      
      # Input: Decimal interval with step value ----
      sliderInput("Delta", "Depreciation:",
                  min = 0, max = 0.005,
                  value = 0.004),   #, step = 0.1),
      
      # Input: Specification of range within an interval ----
      sliderInput("Sigma", "Variance of Health shocks:",
                  min = 0, max = 1,
                  value = 0.4),
      
      # Input: Custom currency format for with basic animation ----
      sliderInput("Alpha", "Aging:",
                  min = 0.9, max = 1.5,
                  value = 1.3,
                  animate = TRUE),
      
      # Input: Animation with custom interval (in ms) ----
      # to control speed, plus looping
      sliderInput("Mu", "Initial Conditions:",
                  min = 0, max = 2, value = 1,
                  step = 0.1)
                  #animate =
                   # animationOptions(interval = 300, loop = TRUE))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "Plot")

      
    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  
  output$Plot <- renderPlot({
    
    data.frame(
      Name = c("I",
               "D",
               "S",
               "A",
               "M"),
      Value = as.character(c(input$I,
                             input$Delta,
                             paste(input$Sigma, collapse = " "),
                             input$Alpha,
                             input$Mu)),
      stringsAsFactors = FALSE)
    
    #Y <- logMR(I=0.13,delta=0.0056,sigma_e=0.53,alpha=1.32,mu_H=0.9)
    Y <- logMR(I=input$I,delta=input$Delta,sigma_e=input$Sigma,alpha=input$Alpha,mu_H=input$Mu)
    
    data<-read.csv(file="MR_FRA1870F_i.csv")
    data<-log(data/100)
    
    plot(Y, xlab= "Age", ylab="Ln of Mortality Rate")
    lines(data)
    
    
  })
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
   
    
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
    

  })
  
}

# Create Shiny app ----
shinyApp(ui, server)