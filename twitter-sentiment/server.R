# library(ROAuth)
# library(twitteR)

server <- function(input, output) {

  api.key <- "Myg215fWKaN6Fh1L0wK1uIeSr"
  api.secret <- "iHSpA9MGypR0pvgdVZ6vDOVXalMCCGWFZs8AU1q8djzfuIUa11"
  access.token <- "116723960-DtdqkWGzn1O9CIlCcA8T1imUJzZvJSgRvIrkCfay"
  access.token.secret <- "JAh0Rf70iRE90t8Aa54k01GBDhSUNl0vdrkazrUjpBnKU"
  
  setup_twitter_oauth(api.key, api.secret, access.token, access.token.secret)
  
  
  output$table <- renderDataTable({
     TweetFrame<-function(searchTerm, maxTweets)
     {
     
       twtList <- searchTwitter(searchTerm,n=maxTweets)
       twtList1 <- do.call("rbind",lapply(twtList,as.data.frame))
       twtList1$text <- iconv(twtList1$text, 'UTF-8', 'ASCII') #WILL THIS SOLVE THE UTF ENCODING PROBLEM: http://lists.hexdump.org/pipermail/twitter-users-hexdump.org/2013-May/000335.html
       return(twtList1)
       
     }
     entity1 <- reactive({
       entity1 <- TweetFrame(input$handle, input$maxTweets)
       })
    output$table <- renderDataTable({tab<-entity1()[1]})
    output$download <- downloadHandler(filename = function() {paste(input$handle, '.csv', sep='')},
                                        content = function(file){
                                          write.csv(entity1(), file)
                                        })
  })
}

