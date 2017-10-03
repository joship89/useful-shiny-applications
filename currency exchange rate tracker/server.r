library(ROAuth)
library(twitteR)
library(syuzhet)
library(streamR)
library(rtweet)


server <- function(input, output) {

  consumer.key <- "qFceZASMHsZ5RiLNTgjR1V1Tb"
  consumer.secret <- "6Ab0LtLOS2qZgEqOm7qFAxpglnI8rrK4cD9mU825hxBPwvDF9M"
  access.token <- "116723960-DtdqkWGzn1O9CIlCcA8T1imUJzZvJSgRvIrkCfay"
  access.token.secret <- "JAh0Rf70iRE90t8Aa54k01GBDhSUNl0vdrkazrUjpBnKU"
  
  setup_twitter_oauth(consumer_key = consumer.key, consumer_secret = consumer.secret, access_token = access.token, access_secret = access.token.secret)
  
  output$table <- renderDataTable({
     TweetFrame<-function(searchTerm, maxTweets)
     {
       twtList <- searchTwitter(searchTerm,n=maxTweets)
       twtList1 <- do.call("rbind",lapply(twtList,as.data.frame))
       twtList1$text <- iconv(twtList1$text, 'UTF-8', 'ASCII') #WILL THIS SOLVE THE UTF ENCODING PROBLEM: http://lists.hexdump.org/pipermail/twitter-users-hexdump.org/2013-May/000335.html
       twtList1$sentiment <- get_sentiment(twtList1$text, method = "syuzhet")
       twtList1 <- twtList1[!(is.na(twtList1$text)),]
       return(twtList1)
       
     }
     
     entity1 <- reactive({
       entity1 <- TweetFrame(input$handle, input$maxTweets)
       })
    output$table <- renderDataTable({
      tab<-entity1()[,c(1,20)]
      })
    
    output$download <- downloadHandler(filename = function() {paste(input$handle, '.csv', sep='')},
                                        content = function(file){
                                          write.csv(entity1(), file)
                                        })
  })
}