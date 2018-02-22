  ##10/18/2017 Sentiment Analysis Mini Project
  
  #install the package
  
  pkg <- c("tm","wordcloud","readr", "dplyr", "tidytext", "ggplot2", "RColorBrewer", "sentimentr", "syuzhet",
  "shiny", 'DT', "shinythemes", "tidyr")
 
  install.packages("pkg")
  

  #Load the package
  
  library(tm)
  library(wordcloud)
  library(readr)
  library(dplyr)
  library(tidytext)
  library(ggplot2)
  library(RColorBrewer)
  library(syuzhet)
  library(shiny)
  library(DT)
  library(shinythemes)
  library(tidyr)
  library(sentimentr)
  
 
  #Shinyapp Structure
  
  ui <- shinyUI(navbarPage("Haiyun_NLP", theme = shinytheme("united"),
                           tabPanel("Normalized Review Table",
                                    sidebarLayout(
                                      sidebarPanel( 
                                        #User can choose columns to display
                                        checkboxGroupInput("show_vars", "Columns in normalized review table to show:",
                                                           names(normReviewtable), selected = names(normReviewtable))),                 
                                      mainPanel(DT::dataTableOutput("normReviewtable")),
                                      position = c("left", "right")
                                    )
                           ),
                           tabPanel("Review Visualization",
                                    tabsetPanel(
                                      tabPanel("Term Count Tables",fluidRow(                  
                                        column(8,offset=1,h3(strong("Observations:")),
                                               tags$p(h4("In the term count table for all reviews, stopwords are kept. Stopwords are words that most requently appear.")),                                              
                                               tags$p(h4("In the term count table for normalized reviews, stopwords are removed. Noun, adjectives and verb are words that most requently appear.")),                                             
                                               column(9,offset=1,h3(strong("Two term count tables for all of the reviews and normalized reviews:"))),
                                               column(9,offset=1,
                                                      #Term count table for all of the reviews
                                                      DT::dataTableOutput("termcount1"),
                                                      hr()),
                                               column(9,offset=1,
                                                      #Term count table for normalized reviews
                                                      DT::dataTableOutput("termcount2"))
                                        ))),
                                      tabPanel("Word clouds",
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   sliderInput("max",
                                                               "Number of Most Frequently Occurring Words:",
                                                               min = 100,  max = 400,  value = 200)),
                                                 mainPanel(
                                                   tabsetPanel(type = "tabs",
                                                               tabPanel("Word Cloud for All Reviews",plotOutput("allreviewcloud",width = "650px",height = "650px")),
                                                               tabPanel("Word Cloud for Normalized Reviews",plotOutput("normreviewcloud",width = "500px",height = "500px")),
                                                               tags$strong(h3("Observation:")),
                                                               tags$p(h4("When the wordcloud shows most frequent words appearing in the reviews without removing the stopwords, the most frequent")),
                                                               tags$p(h4("words are: and, the, this,for. These words do not convey any meaningful insights. On the other hand, when the wordcloud displays ")),
                                                               tags$p(h4("most frequent words appearing in the reviews after removing the stopwords, individual words such as: good, like, great, coffee, flavor,")),
                                                               tags$p(h4("roduct, taste, etc become the most frequently appearing words. These words convey a positive attitude and satisfaction 
                                                                         from the customers. In addition, products such as coffee,tea,food are most discussed words by the consumers."))                                                             
                                                               )))
                                                   ))),                                                                        
                           tabPanel("Sentiment Analysis",
                                    tabsetPanel(
                                      tabPanel("Top 6 Most Reviewed Products",
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   sliderInput("top",
                                                               "Number of Most-Reviewed Products:",
                                                               min = 5,  max = 15,  value = 6)),                    
                                                 mainPanel("Top N Most Reviewed Products",
                                                           DT::dataTableOutput("top6Reviews"),
                                                           hr(),
                                                           plotOutput("topreviewbar",width = "500px",height = "600px")),
                                                 position = c("left", "right")
                                               )
                                      ),
                                      tabPanel("Relationship between AFINN and Rating",fluidRow(
                                        column(12,tags$strong(h3("Observation:")),
                                               tags$p(h4("The scatter plot displays all of the AFINN score and actual score from the reviews of top six most reviewed products.")),
                                               tags$p(h4("The acutual scores of the products do not display accurrately by the calcualted AFINN score for reviews.")),
                                               tags$p(h4(paste("The correlation between the AFINN score and product rating is", round(cor(top6Sentiment$UserRating,top6Sentiment$AFINN),2),", which shows they have weak correlation.")))),
                                        column(11, offset = .5,
                                               plotOutput("top6relationship"))
                                      )),
                                      tabPanel("N-gram with AFINN", fluidRow(
                                        column(10,tags$strong(h3("Observation:")),
                                               tags$p(h4("The chart displays that the amount of effect the negating word plays on sentiment analysis with Unigram.")),
                                               tags$p(h4("The bigram consider negating words which reverse the meaning. However, N-gram with AFINN method does not significantly improve the accuracy of the result"))),
                                        br(), 											 
                                        column(11, offset = 3,plotOutput("AFINNbigram",width = "550px",height = "900px"))
                                      )
                                      ))),
                           tabPanel("Exploration",
                                    tabsetPanel(
                                      tabPanel("Unigram with Bing", fluidRow(
                                        column(8,tags$strong(h3("Observation:")),
                                               tags$p(h4("Using Bing method to do unigram shows better result than the one with AFINN method. The results generated from Bing method are less 
                                                         spreading out than the ones generated from the AFINN method and are more closer to the actual rating from the customers. Even though the Bing 
                                                         method does not improve much the accuracy of predicting, the lexicon is better than AFINN when predicting the scores."))),                                             
                                        column(11, offset = .5,plotOutput("Bingsentence"))
                                               )),
                                      tabPanel("Conclusion",fluidRow(
                                        column(10,tags$p(h4("One of the possible way to improve sentiment analysis is to do it with N-gram. Unigram only consider individual words and ")),
                                               tags$p(h4("ignore the negation word which will make the whole meaning of the phrase complete different from the meaning of a single word. 
                                                         The negation makes the positive and negative words opposite meaning. The possible way to improve sentiment analysis is to use Ngram method.")),
                                               tags$p(h4("While doing the sentiment analysis with unigram methods, lexicons from syuzhet method and Hu & Liu (2004) and sentimentr packages are used to 
                                                         compared with Bing method, but sentimentr package takes long time to process large files, so they are dropped."))
                                               )))
                                               )),
                           tabPanel("About",tags$strong(h2("Sentiment Analysis on Amazon Product Reviews")),
                                    tags$p(h4("This Shiny App is to do sentiment analysis for Amazon Product
                                              reivews using methods wordcloud and Bigrams. The App also compare")),
                                    tags$p(h4("the AFINN score and Bing score generated from the Reviews with the actual 
                                              rating by the customers. The data set involves 250,000 product reviews from different consumers and reviews scores.")),
                                    tags$br(),
                                    tags$p(h4("Author: Haiyun Chen")))
                                    )
                           )
  
  
  
  server <- function(input,output,session) {

  	 #Load the data
  
  reviews <- read.csv(file.choose(),stringsAsFactors = FALSE)
  reviews <- as.data.frame(reviews)
  AFINN <- read.table(file.choose(),stringsAsFactors = FALSE)
  colnames(AFINN) <- c("word","score")
  
  #Normalize the review
  normalizereviw <- Corpus(VectorSource(reviews$Text))
  corpusforindividual <- tm_map(normalizereviw,content_transformer(tolower))
  corpusforindividual <- tm_map(corpusforindividual,removePunctuation)
  corpusforindividual <- tm_map(corpusforindividual,removeWords,stopwords("english"))
  normalizedcolumn <- as.data.frame(corpusforindividual$content)
  
  #Normalized review table
  normReviewtable <- cbind.data.frame(reviews$Id,reviews$ProductId,reviews$UserId,reviews$Text,normalizedcolumn)
  colnames(normReviewtable) <- c("Id","ProductId","UserId","Reviews","NormalizedReviews")
  
  ##Wordcloud for all reviews
  #Transform the reviews to corpus
  reviewtext <- paste(reviews$Text,collapse = " ")
  reviewsource <- VectorSource(reviewtext)
  corpus <- Corpus(reviewsource)
  
  #Covert all reviews from uppercase to lowercase
  corpus <- tm_map(corpus,content_transformer(tolower))
  #Remove punctuation
  corpus <- tm_map(corpus,removePunctuation)
  dtm <- DocumentTermMatrix(corpus)
  dtm2 <- as.matrix(dtm)
  termfrequency <- colSums(dtm2)
  termfrequency <- sort(termfrequency,decreasing = TRUE)
  
  #Term count table for all reviews
  words <- names(termfrequency)
  words1 <- as.data.frame(words)
  termfrequency1<- as.data.frame(termfrequency)
  termcount <- as.data.frame(cbind(words1,termfrequency1)[,1:2])
  colnames(termcount) <- c("Word","Frequency")
  
  
  ##Wordcloud for normalized reviews
  #Remove stop words to make normalized reviews
  corpus1 <- tm_map(corpus,removeWords,stopwords("english"))
  dtm3 <- DocumentTermMatrix(corpus1)
  dtm4 <- as.matrix(dtm3)
  frequency <- colSums(dtm4)
  frequency <- sort(frequency,decreasing = TRUE)
  
  #Term count table for normalized reviews
  words2 <- names(frequency)
  words3 <- as.data.frame(words2)
  frequency1<- as.data.frame(frequency)
  termcountnomalized <- as.data.frame(cbind(words3,frequency1)[,1:2])
  colnames(termcountnomalized) <- c("Word","Frequency")
  
  ##Build AFINN score for each review
  #Build table for normalized review
  Reviewtable <- cbind.data.frame(reviews$Id,reviews$ProductId,reviews$Text)
  colnames(Reviewtable) <- c("Id","ProductId","Reviews")
  Reviewtable <- Reviewtable %>% mutate_if(is.factor, as.character)
  
  
  #Display reviews one word per row according to their id identified as group
  tidyReview <- Reviewtable %>%
    unnest_tokens(word, Reviews)
  
  #Get AFINN score for each review
  afinn <- tidyReview %>% 
    inner_join(AFINN) %>% 
    group_by(Id,ProductId) %>% 
    summarise(sentiment = sum(score)) 
  
  #Assign average AFINN score for each product
  productAfinn <- afinn %>%
    group_by(ProductId) %>%
    summarise(ProductAverageAfinn = round(mean(sentiment),2))
  
  #Get number of user each product
  NumberofUser <- Reviewtable %>% 
    group_by(ProductId) %>%
    summarise(NumberofUser = n())  
  
  #Build the table for question 8
  productScore <- NumberofUser %>%
    inner_join(productAfinn) %>%
    arrange(desc(NumberofUser))
  productScore <- as.data.frame(productScore)
  
  #Table for Top 6 most reviewed products
  topReviews <- productScore[1:6,1:3]
  top6sentiment <- ggplot(topReviews,aes(reorder(topReviews$ProductId,topReviews$ProductAverageAfinn),topReviews$ProductAverageAfinn)) + 
    geom_bar(stat = "identity", fill="red", alpha=0.7) + 
    xlab("Most Reviewed Products") + 
    ylab("Product Average AFINN Score")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 10))+
    coord_flip()
  
  #Sentiment score and rating for the top 6 products
  ReviewSentiment <- topReviews %>%
    inner_join(reviews) 
  ReviewSentiment <- ReviewSentiment %>%
    inner_join(afinn)
  
  top6Sentiment <- ReviewSentiment[,c(1,9,13)]
  colnames(top6Sentiment) <- c("productId","UserRating","AFINN")
  
  ##Ngram Analysis
  #Break all of the reviews into sentences
  
  reviews$Text <- as.character(reviews$Text)
  reviewsentence <- sentimentr::get_sentences(reviews$Text)
  
  ##Get sentiment score for reviews sentences with different methods
  
  #AFINN Bigram method
  
  review_bigrams <- Reviewtable %>%
    unnest_tokens(text,Reviews,token="ngrams",n=2)
  colnames(review_bigrams) <- c("Id","ProductId","word")
  
  bigramseperated <- review_bigrams %>%
    separate(word,c("word1","word2"),sep = " ")
  
  #Counting the frequency of the bigrams
  bigramcount <- bigramseperated %>% 
    count(word1, word2, sort = TRUE)
  
  #filter the two words with negation word at first
  negationwords <- c("not","no","won't","without", "don't","can't")
  
  #Calculate the total sentiment score of words following a negating word
  negationcontribute <- bigramcount %>%
    filter(word1 %in% negationwords) %>%
    inner_join(AFINN, by = c(word2 = "word")) %>%
    count(word1,word2, score, sort = TRUE) %>%
    mutate(contribution = score * nn) %>%
    group_by(word1) %>%
    top_n(5, abs(contribution)) %>%
    slice(1:15) %>%
    ungroup() %>%
    mutate(word2 = reorder(paste(word2, word1, sep = "__"), contribution))
  
  #Bing method
  bingscore1 <- list()
  for (i in 1:length(reviewsentence)) {
    bingscore1[[i]] <- get_sentiment(reviewsentence[[i]], method="bing")
  }
  bingscore <- data.frame(sapply(bingscore1,sum))
  colnames(bingscore) <- "bingscore"
  
  #Combine two types of sentiment scores and reviews
  reviewscore <- reviews[,c(1,2,7)]
  bingsentimentscore <- cbind(reviewscore,bingscore)
  bingsentimentscore <- bingsentimentscore %>%
    inner_join(topReviews) %>%
    select(ProductId,Score,bingscore)
  

    normReviewtable2 = normReviewtable[sample(nrow(normReviewtable), 2000), ]
    output$normReviewtable <- DT::renderDataTable({
      DT::datatable(normReviewtable2[, input$show_vars, drop = FALSE])
    })
    #Two term count tables
    
    output$termcount1 <- DT::renderDataTable({
      DT::datatable(termcount, options = list(pageLength = 10))
    })
    
    output$termcount2 <- DT::renderDataTable({
      DT::datatable(termcountnomalized, options = list(pageLength = 10))
    })
    
    #Ploting two word clouds
    # Make the wordcloud drawing predictable during a session
    
    output$allreviewcloud <- renderPlot({
      wordcloud(termcount$Word,termcount$Frequency,
                max.words = input$max,
                random.order = FALSE, 
                scale=c(3,1),                  
                colors=brewer.pal(8, "Dark2"),
                rot.per=.3)
    })
    
    
    output$normreviewcloud <- renderPlot({
      wordcloud(termcountnomalized$Word,termcountnomalized$Frequency,
                max.words = input$max,
                random.order = FALSE, 
                scale=c(3,0.5),                  
                colors=brewer.pal(6, "Dark2"),
                rot.per=.3)
    })
    
    #Table of number of users who review the product and the average AFINN score for the product
    
    output$top6Reviews <- DT::renderDataTable({
      DT::datatable(productScore[1:input$top,], options = list(pageLength = 10))
    })
    
    #Display a bar chart top 6 most reviewed products and their average score
    
    output$topreviewbar <- renderPlot({
      ggplot(productScore[1:input$top,],aes(reorder(ProductId,ProductAverageAfinn),ProductAverageAfinn)) + 
        geom_bar(stat = "identity", fill="red", alpha=0.7) + 
        xlab("Most Reviewed Products") + 
        ylab("Product Average AFINN Score")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 10),axis.title = element_text(color = "orange",size = 15))+
        coord_flip()
    })
    
    #scatter plots with sentiment score per review and Rating for top 6 most reviewed products 
    
    output$top6relationship <- renderPlot({
      ggplot(top6Sentiment,aes(x=AFINN,y=UserRating, group = top6Sentiment$productId))+
        geom_point(aes(colour=factor(productId)),alpha=0.5,size=3.5)+
        facet_wrap(~top6Sentiment$productId)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 10),axis.text.y = element_text(size = 10),
              axis.title = element_text(color = "orange",size = 15))+
        xlab("AFINN Score")+
        ylab("Users' Product Rating")
    })
    
    
    ##visualize the consumer rating AND AFINN score 
    
    output$AFINNbigram <- renderPlot({
      ggplot(negationcontribute,(aes(word2, contribution, fill = contribution > 0))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ word1, scales = "free", nrow = 3) +
        scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
        xlab("Words preceded by a negation") +
        ylab("The Sum of the Sentiment score by Words") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        coord_flip()
    })
    
    #visualize the consumer rating AND Bing score 
    
    output$Bingsentence <- renderPlot({
      ggplot(bingsentimentscore,aes(bingscore,Score))+
        geom_point(aes(colour=factor(ProductId)),alpha=0.5,size=3.5)+
        scale_x_continuous(limit = c(-20, 60))+
        facet_wrap(~bingsentimentscore$ProductId)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 10),axis.text.y = element_text(size = 10),
              axis.title = element_text(color = "orange",size = 15))+
        xlab("Bing Score")+
        ylab("Users' Product Rating")
    })
  }
  
  # Return a Shiny app object
  shinyApp(ui = ui, server = server)
