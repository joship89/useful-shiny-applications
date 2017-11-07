# To enable authentication in shiny server, user needs to update the subscription to
# a standard plan or higher. The following is a way to embed password authentication using the
# validate() function.

library(shiny)

# UI for an application to generate a plot using Old Failthful Geyser Data:
ui <- fluidPage(
  passwordInput("password", "Password:"),
  actionButton("go", "Go"),
  
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      

      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Server Logic:
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      
# Use Validate() function to test the condition that the supplied password is correct.
# Error message "Incorrect Password" is signaled which stops execution


     validate(
       need(input$password == 1234, "Incorrect Password")
     )
     
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

