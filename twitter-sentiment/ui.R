library(shiny)

ui <- fluidPage(
  textInput("handle", "Search Tweets:"),
  sliderInput("maxTweets","Number of recent tweets to use for analysis:",min=5,max=1500, value = 5),
  downloadButton("download", "Download File"),
  dataTableOutput("table")
)
