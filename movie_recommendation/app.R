#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(reshape)
library(ROAuth)
library(twitteR)
library(shiny)
library(shinythemes)

# Define UI for application
ui <- fluidPage(
   theme = shinytheme("superhero"),
   br(),
   fluidRow(width=12,
    column(width=8,offset=2,
      titlePanel("Movie Recommendation Using Twitter Profile Extraction")
      )
    ),
   fluidRow(width = 12,
    sidebarLayout(
      column(width = 4,br(),br(),
      sidebarPanel(
        width = 12,
        textInput("twitter_handle",h4("Twitter Handle"),placeholder = "Enter Twitter handle..."),
        sliderInput(inputId = "no_of_tweets", label = h4("Select number of tweets"),min = 1, max = 3200, value = 0, round = TRUE),
        submitButton("Submit"),
        tags$head(tags$style(type="text/css", "
             #loadmessage {
                             position: fixed;
                             top: 0px;
                             left: 0px;
                             width: 100%;
                             padding: 7px 0px 7px 0px;
                             text-align: center;
                             font-weight: bold;
                             font-size: 100%;
                             color: #000000;
                             background-color: #FFFF00;
                             z-index: 105;
                             }
                             ")),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Loading...",id="loadmessage"))
      )
    ),
     column(width = 8,br(),
      mainPanel(
        width = 12,
        navbarPage(selected="Recommendation",position = "static-top",theme="superhero",br(),
        #tabsetPanel(type = "tabs",
                    tabPanel(title="Recommendation",
        h3("Recommended movies for you"),br(),
        h4("1. Hollywood movies"),br(),
        dataTableOutput("movie_list"),br(),
        h4("2. Bollywood movies"),br(),
        dataTableOutput("bolly_movies_list"),br(),
        h3("Trending movies for you"),br(),
        dataTableOutput("trending_movies"),br(),
        h3("Trending Movies"),br(),
        img(src="Black Panther .jpg",width="200px",height="200px"),
        img(src="padmavat.jpg",width="200px",height="200px"),
        img(src="Padman.jpg",width="200px",height="200px"),
        img(src="Paddington 2.jpg",width="200px",height="200px"),
        img(src="Strong.jpg",width="200px",height="200px"),
        img(src="Darkest hour.jpg",width="200px",height="200px"),
        img(src="Jumanji 2.jpg",width="200px",height="200px"),
        img(src="Skyscraper.jpg",width="200px",height="200px"),
        img(src="Tiger Zinda Hai .jpg",width="200px",height="200px"),
        img(src="The Greatest Showman.jpg",width="200px",height="200px")
                    ),
                    tabPanel(title="Tweet Sentiment",
                             h3("Sentiment score for each tweet"),
                             dataTableOutput("sentiment")
                             ),
                    tabPanel(title="Genre weight",
                             h3("Genre weight"),
                             dataTableOutput("genre_weight"))
        )
       )
     )
    )
  )
)

server <- function(input, output) {
  consumer_key <- ""
  consumer_secret <- ""
  access_token <- ""
  access_secret <- ""
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
  token <- get("oauth_token", twitteR:::oauth_cache)
  token$cache()
  observe({
    output$movie_list <- renderDataTable({
      user <- input$twitter_handle
      tw1 <- userTimeline(user,n = input$no_of_tweets)
      df <- do.call("rbind", lapply(tw1, as.data.frame))
      df <- twListToDF(tw1)
      df$text <- sapply(df$text, function(row) iconv(row, "latin1", "ASCII", sub=""))
      df$text = gsub("(f|ht)tp(s?)://t.co/[a-z,A-Z,0-9]*", "", df$text)
      sample <- df$text
      pos_words = scan('D:/Project/positive-words.txt', what='character', comment.char=';')
      neg_words = scan('D:/Project/negative-words.txt', what='character', comment.char=';')
      
      #For adding extra words in the list  
      wordDatabase<-function()
      {
        pos_words<<-c(pos_words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader', 'power', 'powerful', 'latest')
        neg_words<<-c(neg_words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not')
      }
      
      score.sentiment = function(sentences, pos_words, neg_words, .progress='none')
      {
        require(plyr)
        require(stringr)
        list=lapply(sentences, function(sentence, pos_words, neg_words)
        {
          sentence = gsub('[[:punct:]]',' ',sentence)
          sentence = gsub('[[:cntrl:]]','',sentence)
          sentence = gsub('\\d+','',sentence)  #removes decimal number
          sentence = gsub('\n','',sentence)    #removes new lines
          sentence = tolower(sentence)
          word.list = str_split(sentence, '\\s+')
          words = unlist(word.list)  #changes a list to character vector
          pos_matches = match(words, pos_words)
          neg_matches = match(words, neg_words)
          pos_matches = !is.na(pos_matches)
          neg_matches = !is.na(neg_matches)
          pp = sum(pos_matches)
          nn = sum(neg_matches)
          score = sum(pos_matches) - sum(neg_matches)
          list1 = c(score, pp, nn)
          return (list1)
        }, pos_words, neg_words)
        score_new = lapply(list, `[[`, 1)
        pp1 = lapply(list, `[[`, 2)
        nn1 = lapply(list, `[[`, 3)
        scores.df = data.frame(score = score_new, text=sentences)
        positive.df = data.frame(Positive = pp1, text=sentences)
        negative.df = data.frame(Negative = nn1, text=sentences)
        list_df = list(scores.df, positive.df, negative.df)
        return(list_df)
      }
      
      result = score.sentiment(sample, pos_words, neg_words)
    
      test1=result[[1]]
      test2=result[[2]]
      test3=result[[3]]
      
      #Creating three different data frames for Score, Positive and Negative
      #Removing text column from data frame
      test1$text=NULL
      test2$text=NULL
      test3$text=NULL
      #Storing the first row(Containing the sentiment scores) in variable q
      q1=test1[1,]
      q2=test2[1,]
      q3=test3[1,]
      qq1=melt(q1, ,var='Score')
      qq2=melt(q2, ,var='Positive')
      qq3=melt(q3, ,var='Negative') 
      qq1['Score'] = NULL
      qq2['Positive'] = NULL
      qq3['Negative'] = NULL
      #Creating data frame
      table1 = data.frame(Text=result[[1]]$text, Score=qq1)
      table2 = data.frame(Text=result[[2]]$text, Score=qq2)
      table3 = data.frame(Text=result[[3]]$text, Score=qq3)
      
      #Merging three data frames into one
      table_final=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)
      
      rows <- nrow(table_final)
      tweets_index <- c()
      for(i in 1:rows) {
        if(table_final[i,"Score"]>=0) {
          tweets_index <- c(tweets_index, i) 
        }
      }
      stweet <- sample[tweets_index]
      
      tweet_words <- strsplit(sample," ")
      tweet_words_list <- unlist(tweet_words)
      genrelist <- read.csv(file="genrelist.csv",header = TRUE,sep = ",")
      genrelist <- genrelist[,-1]
      
      action_words <- genrelist[,1]
      action_words <- setdiff(action_words,"")
      animation_words <- genrelist[,2]
      animation_words <- setdiff(animation_words,"")
      mystery_words <- genrelist[,3]
      mystery_words <- setdiff(mystery_words,"")
      comedy_words <- genrelist[,4]
      comedy_words <- setdiff(comedy_words,"")
      drama_words <- genrelist[,5]
      drama_words <- setdiff(drama_words,"")
      family_words <- genrelist[,6]
      family_words <- setdiff(family_words,"")
      horror_words <- genrelist[,7]
      horror_words <- setdiff(horror_words,"")
      romance_words <- genrelist[,8]
      romance_words <- setdiff(romance_words,"")
      thriller_words <- genrelist[,9]
      thriller_words <- setdiff(thriller_words,"")
      adventure_words <- genrelist[,10]
      adventure_words <- setdiff(adventure_words,"")
      scifi_words <- genrelist[,11]
      scifi_words <- setdiff(scifi_words,"")
      
      weight <- vector()
      word_name <- vector()
      
      for(i in 1:length(action_words)) {
        ind <- grep(action_words[i],stweet)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(action_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(action_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,action_words[i])
        }
      }
      
      for(i in 1:length(animation_words)) {
        ind <- grep(animation_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(animation_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(animation_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,animation_words[i])
        }
      }
      for(i in 1:length(mystery_words)) {
        ind <- grep(mystery_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(mystery_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(mystery_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,mystery_words[i])
        }
      }
      for(i in 1:length(comedy_words)) {
        ind <- grep(comedy_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(comedy_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(comedy_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,comedy_words[i])
        }
      }
      for(i in 1:length(drama_words)) {
        ind <- grep(drama_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(drama_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(drama_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,drama_words[i])
        }
      }
      for(i in 1:length(family_words)) {
        ind <- grep(family_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(family_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(family_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,family_words[i])
        }
      }
      for(i in 1:length(horror_words)) {
        ind <- grep(horror_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(horror_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(horror_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,horror_words[i])
        }
      }
      for(i in 1:length(romance_words)) {
        ind <- grep(romance_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(romance_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(romance_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,romance_words[i])
        }
      }
      for(i in 1:length(thriller_words)) {
        ind <- grep(thriller_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(thriller_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(thriller_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,thriller_words[i])
        }
      }
      for(i in 1:length(adventure_words)) {
        ind <- grep(adventure_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(adventure_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(adventure_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,adventure_words[i])
        }
      }
      for(i in 1:length(scifi_words)) {
        ind <- grep(scifi_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(scifi_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(scifi_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,scifi_words[i])
        }
      }
      tweet_weight <- data.frame(word_name,weight,stringsAsFactors = FALSE)
      
      genrelist <- read.csv(file="genrelist.csv",header = TRUE,sep=",")
      genrelist <- genrelist[,-1]
      action_words_list <- as.character(genrelist[,1])
      action_words_list <- setdiff(action_words_list,"")
      animation_words_list <- as.character(genrelist[,2])
      animation_words_list <- setdiff(animation_words_list,"")
      mystery_words_list <- as.character(genrelist[,3])
      mystery_words_list <- setdiff(mystery_words_list,"")
      comedy_words_list <- as.character(genrelist[,4])
      comedy_words_list <- setdiff(comedy_words_list,"")
      drama_words_list <- as.character(genrelist[,5])
      drama_words_list <- setdiff(drama_words_list,"")
      family_words_list <- as.character(genrelist[,6])
      family_words_list <- setdiff(family_words_list,"")
      horror_words_list <- as.character(genrelist[,7])
      horror_words_list <- setdiff(horror_words_list,"")
      romance_words_list <- as.character(genrelist[,8])
      romance_words_list <- setdiff(romance_words_list,"")
      thriller_words_list <- as.character(genrelist[,9])
      thriller_words_list <- setdiff(thriller_words_list,"")
      adventure_words_list <- as.character(genrelist[,10])
      adventure_words_list <- setdiff(adventure_words_list,"")
      scifi_words_list <- as.character(genrelist[,11])
      scifi_words_list <- setdiff(scifi_words_list,"")
      
      weight1 <- vector(length=length(action_words_list))
      actionwords_weight <- data.frame(action_words_list,weight1,stringsAsFactors = FALSE)
      weight2 <- vector(length=length(animation_words_list))
      animationwords_weight <- data.frame(animation_words_list,weight2,stringsAsFactors = FALSE)
      weight3 <- vector(length=length(mystery_words_list))
      mysterywords_weight <- data.frame(mystery_words_list,weight3,stringsAsFactors = FALSE)
      weight4 <- vector(length=length(comedy_words_list))
      comedywords_weight <- data.frame(comedy_words_list,weight4,stringsAsFactors = FALSE)
      weight5 <- vector(length=length(drama_words_list))
      dramawords_weight <- data.frame(drama_words_list,weight5,stringsAsFactors = FALSE)
      weight6 <- vector(length=length(family_words_list))
      familywords_weight <- data.frame(family_words_list,weight6,stringsAsFactors = FALSE)
      weight7 <- vector(length=length(horror_words_list))
      horrorwords_weight <- data.frame(horror_words_list,weight7,stringsAsFactors = FALSE)
      weight8 <- vector(length=length(romance_words_list))
      romancewords_weight <- data.frame(romance_words_list,weight8,stringsAsFactors = FALSE)
      weight9 <- vector(length=length(thriller_words_list))
      thrillerwords_weight <- data.frame(thriller_words_list,weight9,stringsAsFactors = FALSE)
      weight10 <- vector(length=length(adventure_words_list))
      adventurewords_weight <- data.frame(adventure_words_list,weight10,stringsAsFactors = FALSE)
      weight11 <- vector(length=length(scifi_words_list))
      scifiwords_weight <- data.frame(scifi_words_list,weight11,stringsAsFactors = FALSE)
      
      tf=log10(1+1)
      idf=log10(11/1)
      tf_idf <- tf*idf
      
      for(i in 1:nrow(actionwords_weight)) {
        actionwords_weight[i,2]<-tf_idf
      }
      for(i in 1:nrow(animationwords_weight)) {
        animationwords_weight[i,2]<-tf_idf
      }
      for(i in 1:nrow(mysterywords_weight)) {
        mysterywords_weight[i,2]<-tf_idf
      }
      for(i in 1:nrow(comedywords_weight)) {
        comedywords_weight[i,2]<-tf_idf
      }
      for(i in 1:nrow(dramawords_weight)) {
        dramawords_weight[i,2]<-tf_idf
      }
      for(i in 1:nrow(familywords_weight)) {
        familywords_weight[i,2]<-tf_idf
      }
      for(i in 1:nrow(horrorwords_weight)) {
        horrorwords_weight[i,2]<-tf_idf
      }
      for(i in 1:nrow(romancewords_weight)) {
        romancewords_weight[i,2]<-tf_idf
      }
      for(i in 1:nrow(thrillerwords_weight)) {
        thrillerwords_weight[i,2]<-tf_idf
      }
      for(i in 1:nrow(adventurewords_weight)) {
        adventurewords_weight[i,2]<-tf_idf
      } 
      for(i in 1:nrow(scifiwords_weight)) {
        scifiwords_weight[i,2]<-tf_idf
      }
      
      movie_matrix <- read.csv(file="movie_matrix.csv",header = TRUE,sep = ",")
      movie_matrix <- movie_matrix[,-1]
      movie_name <- as.character(movie_matrix[,"Movie_Title"])
      
      genre <- c("Action","Animation","Mystery","Comedy","Drama","Family","Horror","Romance","Thriller","Adventure","Sci-Fi")
      weight<-vector(length=length(genre))
      genrematrix <- data.frame(genre,weight,stringsAsFactors = FALSE)
      
      no_words<-nrow(tweet_weight)
      if(no_words==0) {
        for(i in 1:length(genre)) {
          genrematrix[i,2] <- 0
        }
      }
      if(nrow(tweet_weight)!=0) {
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],action_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            genrematrix[1,2] <- genrematrix[1,2]+(tweet_weight[i,2]*actionwords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],animation_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            genrematrix[2,2] <- genrematrix[2,2]+(tweet_weight[i,2]*animationwords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],mystery_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[3,2] <- genrematrix[3,2]+(tweet_weight[i,2]*mysterywords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],comedy_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[4,2] <- genrematrix[4,2]+(tweet_weight[i,2]*comedywords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],drama_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[5,2] <- genrematrix[5,2]+(tweet_weight[i,2]*dramawords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],family_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[6,2] <- genrematrix[6,2]+(tweet_weight[i,2]*familywords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],horror_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[7,2] <- genrematrix[7,2]+(tweet_weight[i,2]*horrorwords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],romance_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[8,2] <- genrematrix[8,2]+(tweet_weight[i,2]*romancewords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],thriller_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[9,2] <- genrematrix[9,2]+(tweet_weight[i,2]*thrillerwords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],adventure_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[10,2] <- genrematrix[10,2]+(tweet_weight[i,2]*adventurewords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],scifi_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[11,2] <- genrematrix[11,2]+(tweet_weight[i,2]*scifiwords_weight[ind[1],2])
          }
        }
      }
      moviedb <- read.csv(file="movie.csv",header = TRUE,sep=",",stringsAsFactors = FALSE)
      moviedb <- moviedb[,-1]
      imdb_rating <- moviedb[,"imdb_score"]
      movie_weight <- vector(length=length(movie_name))
      multiply_result_matrix <- data.frame(movie_name,movie_weight,imdb_rating,stringsAsFactors = FALSE)
      for(i in 1:nrow(multiply_result_matrix)) {
        m <- 0
        for(j in 2:12) {
          m <- m + (movie_matrix[i,j]*genrematrix[j-1,2])
        }
        multiply_result_matrix[i,2] <- m
      }
      #sort matrix in decreasing order of movie weight
      multiply_result_matrix <- multiply_result_matrix[order(multiply_result_matrix$movie_weight,decreasing = TRUE),]
      sort_data <- read.csv(file="sort_swap.csv",header = TRUE,stringsAsFactors = FALSE)
      sort_data <- sort_data[,-1]
      for(i in 1:nrow(multiply_result_matrix)) {
        if(identical(multiply_result_matrix[i,2],0)) {
          break;
        }
        else {
          if(identical(multiply_result_matrix[i,2],multiply_result_matrix[i+1,2])) {
            for(j in i+1:nrow(multiply_result_matrix)) {
              if(identical(multiply_result_matrix[i,2],multiply_result_matrix[j,2])) {
                k=j
              }
              else {
                break;
              }
            }
            sort_data[i:k,] <- multiply_result_matrix[i:k,]
            sort_data <- sort_data[order(sort_data$imdb_rating,decreasing = TRUE),]
            multiply_result_matrix[i:k,] <- sort_data[i:k,]
          }
        }
      }
      names(multiply_result_matrix)[1] <- "Movie"
      names(multiply_result_matrix)[2] <- "Weight"
      names(multiply_result_matrix)[3] <- "IMDB Rating"
      multiply_result_matrix[1:25,]
    })
    output$bolly_movies_list <- renderDataTable({
      user <- input$twitter_handle
      tw1 <- userTimeline(user,n = input$no_of_tweets)
      df <- do.call("rbind", lapply(tw1, as.data.frame))
      df <- twListToDF(tw1)
      df$text <- sapply(df$text, function(row) iconv(row, "latin1", "ASCII", sub=""))
      df$text = gsub("(f|ht)tp(s?)://t.co/[a-z,A-Z,0-9]*", "", df$text)
      sample <- df$text
      library(reshape)
      library(twitteR)
      pos_words = scan('D:/Project/positive-words.txt', what='character', comment.char=';')
      neg_words = scan('D:/Project/negative-words.txt', what='character', comment.char=';')
      
      #For adding extra words in the list  
      wordDatabase<-function()
      {
        pos_words<<-c(pos_words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader', 'power', 'powerful', 'latest')
        neg_words<<-c(neg_words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not')
      }
      
      score.sentiment = function(sentences, pos_words, neg_words, .progress='none')
      {
        require(plyr)
        require(stringr)
        list=lapply(sentences, function(sentence, pos_words, neg_words)
        {
          sentence = gsub('[[:punct:]]',' ',sentence)
          sentence = gsub('[[:cntrl:]]','',sentence)
          sentence = gsub('\\d+','',sentence)  #removes decimal number
          sentence = gsub('\n','',sentence)    #removes new lines
          sentence = tolower(sentence)
          word.list = str_split(sentence, '\\s+')
          words = unlist(word.list)  #changes a list to character vector
          pos_matches = match(words, pos_words)
          neg_matches = match(words, neg_words)
          pos_matches = !is.na(pos_matches)
          neg_matches = !is.na(neg_matches)
          pp = sum(pos_matches)
          nn = sum(neg_matches)
          score = sum(pos_matches) - sum(neg_matches)
          list1 = c(score, pp, nn)
          return (list1)
        }, pos_words, neg_words)
        score_new = lapply(list, `[[`, 1)
        pp1 = lapply(list, `[[`, 2)
        nn1 = lapply(list, `[[`, 3)
        scores.df = data.frame(score = score_new, text=sentences)
        positive.df = data.frame(Positive = pp1, text=sentences)
        negative.df = data.frame(Negative = nn1, text=sentences)
        list_df = list(scores.df, positive.df, negative.df)
        return(list_df)
      }
      
      result = score.sentiment(sample, pos_words, neg_words)
      
      test1=result[[1]]
      test2=result[[2]]
      test3=result[[3]]
      
      #Creating three different data frames for Score, Positive and Negative
      #Removing text column from data frame
      test1$text=NULL
      test2$text=NULL
      test3$text=NULL
      #Storing the first row(Containing the sentiment scores) in variable q
      q1=test1[1,]
      q2=test2[1,]
      q3=test3[1,]
      qq1=melt(q1, ,var='Score')
      qq2=melt(q2, ,var='Positive')
      qq3=melt(q3, ,var='Negative') 
      qq1['Score'] = NULL
      qq2['Positive'] = NULL
      qq3['Negative'] = NULL
      #Creating data frame
      table1 = data.frame(Text=result[[1]]$text, Score=qq1)
      table2 = data.frame(Text=result[[2]]$text, Score=qq2)
      table3 = data.frame(Text=result[[3]]$text, Score=qq3)
      
      #Merging three data frames into one
      table_final=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)
      
      rows <- nrow(table_final)
      tweets_index <- c()
      for(i in 1:rows) {
        if(table_final[i,"Score"]>=0) {
          tweets_index <- c(tweets_index, i) 
        }
      }
      stweet <- sample[tweets_index]
      
      tweet_words <- strsplit(sample," ")
      tweet_words_list <- unlist(tweet_words)
      genrelist <- read.csv(file="genrelist.csv",header = TRUE,sep = ",")
      genrelist <- genrelist[,-1]
      
      action_words <- genrelist[,1]
      action_words <- setdiff(action_words,"")
      animation_words <- genrelist[,2]
      animation_words <- setdiff(animation_words,"")
      mystery_words <- genrelist[,3]
      mystery_words <- setdiff(mystery_words,"")
      comedy_words <- genrelist[,4]
      comedy_words <- setdiff(comedy_words,"")
      drama_words <- genrelist[,5]
      drama_words <- setdiff(drama_words,"")
      family_words <- genrelist[,6]
      family_words <- setdiff(family_words,"")
      horror_words <- genrelist[,7]
      horror_words <- setdiff(horror_words,"")
      romance_words <- genrelist[,8]
      romance_words <- setdiff(romance_words,"")
      thriller_words <- genrelist[,9]
      thriller_words <- setdiff(thriller_words,"")
      adventure_words <- genrelist[,10]
      adventure_words <- setdiff(adventure_words,"")
      scifi_words <- genrelist[,11]
      scifi_words <- setdiff(scifi_words,"")
      
      weight <- vector()
      word_name <- vector()
      
      for(i in 1:length(action_words)) {
        ind <- grep(action_words[i],stweet)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(action_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(action_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,action_words[i])
        }
      }
      
      for(i in 1:length(animation_words)) {
        ind <- grep(animation_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(animation_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(animation_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,animation_words[i])
        }
      }
      for(i in 1:length(mystery_words)) {
        ind <- grep(mystery_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(mystery_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(mystery_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,mystery_words[i])
        }
      }
      for(i in 1:length(comedy_words)) {
        ind <- grep(comedy_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(comedy_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(comedy_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,comedy_words[i])
        }
      }
      for(i in 1:length(drama_words)) {
        ind <- grep(drama_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(drama_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(drama_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,drama_words[i])
        }
      }
      for(i in 1:length(family_words)) {
        ind <- grep(family_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(family_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(family_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,family_words[i])
        }
      }
      for(i in 1:length(horror_words)) {
        ind <- grep(horror_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(horror_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(horror_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,horror_words[i])
        }
      }
      for(i in 1:length(romance_words)) {
        ind <- grep(romance_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(romance_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(romance_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,romance_words[i])
        }
      }
      for(i in 1:length(thriller_words)) {
        ind <- grep(thriller_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(thriller_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(thriller_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,thriller_words[i])
        }
      }
      for(i in 1:length(adventure_words)) {
        ind <- grep(adventure_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(adventure_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(adventure_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,adventure_words[i])
        }
      }
      for(i in 1:length(scifi_words)) {
        ind <- grep(scifi_words[i],sample)
        ind_count <- length(ind)
        if(ind_count>0) {
          total_tweets <- length(stweet)
          #term_freq <- length(grep(scifi_words[i],tweet_words_list))
          #word_count <- length(unlist(tweet_words[ind]))
          #tf <- (log10(1+term_freq))
          stweet_words <- str_split(stweet[ind]," ")
          totalwords <- unlist(stweet_words)
          tf <- length(grep(scifi_words[i],totalwords))/length(totalwords)
          idf <- log10(total_tweets/ind_count)
          weight <- c(weight,tf*idf)
          word_name <- c(word_name,scifi_words[i])
        }
      }
      tweet_weight <- data.frame(word_name,weight,stringsAsFactors = FALSE)
      
      genrelist <- read.csv(file="genrelist.csv",header = TRUE,sep=",")
      genrelist <- genrelist[,-1]
      action_words_list <- as.character(genrelist[,1])
      action_words_list <- setdiff(action_words_list,"")
      animation_words_list <- as.character(genrelist[,2])
      animation_words_list <- setdiff(animation_words_list,"")
      mystery_words_list <- as.character(genrelist[,3])
      mystery_words_list <- setdiff(mystery_words_list,"")
      comedy_words_list <- as.character(genrelist[,4])
      comedy_words_list <- setdiff(comedy_words_list,"")
      drama_words_list <- as.character(genrelist[,5])
      drama_words_list <- setdiff(drama_words_list,"")
      family_words_list <- as.character(genrelist[,6])
      family_words_list <- setdiff(family_words_list,"")
      horror_words_list <- as.character(genrelist[,7])
      horror_words_list <- setdiff(horror_words_list,"")
      romance_words_list <- as.character(genrelist[,8])
      romance_words_list <- setdiff(romance_words_list,"")
      thriller_words_list <- as.character(genrelist[,9])
      thriller_words_list <- setdiff(thriller_words_list,"")
      adventure_words_list <- as.character(genrelist[,10])
      adventure_words_list <- setdiff(adventure_words_list,"")
      scifi_words_list <- as.character(genrelist[,11])
      scifi_words_list <- setdiff(scifi_words_list,"")
      
      weight1 <- vector(length=length(action_words_list))
      actionwords_weight <- data.frame(action_words_list,weight1,stringsAsFactors = FALSE)
      weight2 <- vector(length=length(animation_words_list))
      animationwords_weight <- data.frame(animation_words_list,weight2,stringsAsFactors = FALSE)
      weight3 <- vector(length=length(mystery_words_list))
      mysterywords_weight <- data.frame(mystery_words_list,weight3,stringsAsFactors = FALSE)
      weight4 <- vector(length=length(comedy_words_list))
      comedywords_weight <- data.frame(comedy_words_list,weight4,stringsAsFactors = FALSE)
      weight5 <- vector(length=length(drama_words_list))
      dramawords_weight <- data.frame(drama_words_list,weight5,stringsAsFactors = FALSE)
      weight6 <- vector(length=length(family_words_list))
      familywords_weight <- data.frame(family_words_list,weight6,stringsAsFactors = FALSE)
      weight7 <- vector(length=length(horror_words_list))
      horrorwords_weight <- data.frame(horror_words_list,weight7,stringsAsFactors = FALSE)
      weight8 <- vector(length=length(romance_words_list))
      romancewords_weight <- data.frame(romance_words_list,weight8,stringsAsFactors = FALSE)
      weight9 <- vector(length=length(thriller_words_list))
      thrillerwords_weight <- data.frame(thriller_words_list,weight9,stringsAsFactors = FALSE)
      weight10 <- vector(length=length(adventure_words_list))
      adventurewords_weight <- data.frame(adventure_words_list,weight10,stringsAsFactors = FALSE)
      weight11 <- vector(length=length(scifi_words_list))
      scifiwords_weight <- data.frame(scifi_words_list,weight11,stringsAsFactors = FALSE)
      
      tf=log10(1+1)
      idf=log10(11/1)
      tf_idf <- tf*idf
      
      for(i in 1:nrow(actionwords_weight)) {
        actionwords_weight[i,2]<-tf_idf
      }
      for(i in 1:nrow(animationwords_weight)) {
        animationwords_weight[i,2]<-tf_idf
      }
      for(i in 1:nrow(mysterywords_weight)) {
        mysterywords_weight[i,2]<-tf_idf
      }
      for(i in 1:nrow(comedywords_weight)) {
        comedywords_weight[i,2]<-tf_idf
      }
      for(i in 1:nrow(dramawords_weight)) {
        dramawords_weight[i,2]<-tf_idf
      }
      for(i in 1:nrow(familywords_weight)) {
        familywords_weight[i,2]<-tf_idf
      }
      for(i in 1:nrow(horrorwords_weight)) {
        horrorwords_weight[i,2]<-tf_idf
      }
      for(i in 1:nrow(romancewords_weight)) {
        romancewords_weight[i,2]<-tf_idf
      }
      for(i in 1:nrow(thrillerwords_weight)) {
        thrillerwords_weight[i,2]<-tf_idf
      }
      for(i in 1:nrow(adventurewords_weight)) {
        adventurewords_weight[i,2]<-tf_idf
      } 
      for(i in 1:nrow(scifiwords_weight)) {
        scifiwords_weight[i,2]<-tf_idf
      }
      genre <- c("Action","Animation","Mystery","Comedy","Drama","Family","Horror","Romance","Thriller","Adventure","Sci-Fi")
      weight<-vector(length=length(genre))
      genrematrix <- data.frame(genre,weight,stringsAsFactors = FALSE)
      
      no_words<-nrow(tweet_weight)
      if(no_words==0) {
        for(i in 1:length(genre)) {
          genrematrix[i,2] <- 0
        }
      }
      if(nrow(tweet_weight)!=0) {
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],action_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[1,2] <- genrematrix[1,2]+(tweet_weight[i,2]*actionwords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],animation_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[2,2] <- genrematrix[2,2]+(tweet_weight[i,2]*animationwords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],mystery_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[3,2] <- genrematrix[3,2]+(tweet_weight[i,2]*mysterywords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],comedy_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[4,2] <- genrematrix[4,2]+(tweet_weight[i,2]*comedywords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],drama_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[5,2] <- genrematrix[5,2]+(tweet_weight[i,2]*dramawords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],family_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[6,2] <- genrematrix[6,2]+(tweet_weight[i,2]*familywords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],horror_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[7,2] <- genrematrix[7,2]+(tweet_weight[i,2]*horrorwords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],romance_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[8,2] <- genrematrix[8,2]+(tweet_weight[i,2]*romancewords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],thriller_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[9,2] <- genrematrix[9,2]+(tweet_weight[i,2]*thrillerwords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],adventure_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[10,2] <- genrematrix[10,2]+(tweet_weight[i,2]*adventurewords_weight[ind[1],2])
          }
        }
        for(i in 1:nrow(tweet_weight)) {
          ind <- grep(word_name[i],scifi_words_list,fixed=TRUE)
          ind_length <- length(ind)
          if(ind_length!=0) {
            
            genrematrix[11,2] <- genrematrix[11,2]+(tweet_weight[i,2]*scifiwords_weight[ind[1],2])
          }
        }
      }
      d <- read.csv(file="bollywood_matrix.csv",header = TRUE,sep=",")
      d <- d[,-1]
      movie_name <- as.character(d[,1]) 
      movie_matrix <- read.csv(file="bollywood_matrix.csv",header = TRUE,sep=",")
      movie_matrix <- movie_matrix[,-1]
      movie_weight <- vector(length=length(movie_name))
      multiply_result_matrix <- data.frame(movie_name,movie_weight,stringsAsFactors = FALSE)
      for(i in 1:nrow(multiply_result_matrix)) {
        m <- 0
        for(j in 2:12) {
          m <- m + (movie_matrix[i,j]*genrematrix[j-1,2])
        }
        multiply_result_matrix[i,2] <- m
      }
      #sort matrix in decreasing order of movie weight
      multiply_result_matrix <- multiply_result_matrix[order(multiply_result_matrix$movie_weight,decreasing = TRUE),]
      names(multiply_result_matrix)[1] <- "Movie"
      names(multiply_result_matrix)[2] <- "Weight"
      multiply_result_matrix[1:25,]
    })
  })
  output$sentiment <- renderDataTable({
    user <- input$twitter_handle
    tw1 <- userTimeline(user,n = input$no_of_tweets)
    df <- do.call("rbind", lapply(tw1, as.data.frame))
    df <- twListToDF(tw1)
    df$text <- sapply(df$text, function(row) iconv(row, "latin1", "ASCII", sub=""))
    df$text = gsub("(f|ht)tp(s?)://t.co/[a-z,A-Z,0-9]*", "", df$text)
    sample <- df$text
    library(reshape)
    library(twitteR)
    pos_words = scan('D:/Project/positive-words.txt', what='character', comment.char=';')
    neg_words = scan('D:/Project/negative-words.txt', what='character', comment.char=';')
    
    #For adding extra words in the list  
    wordDatabase<-function()
    {
      pos_words<<-c(pos_words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader', 'power', 'powerful', 'latest')
      neg_words<<-c(neg_words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not')
    }
    
    score.sentiment = function(sentences, pos_words, neg_words, .progress='none')
    {
      require(plyr)
      require(stringr)
      list=lapply(sentences, function(sentence, pos_words, neg_words)
      {
        sentence = gsub('[[:punct:]]',' ',sentence)
        sentence = gsub('[[:cntrl:]]','',sentence)
        sentence = gsub('\\d+','',sentence)  #removes decimal number
        sentence = gsub('\n','',sentence)    #removes new lines
        sentence = tolower(sentence)
        word.list = str_split(sentence, '\\s+')
        words = unlist(word.list)  #changes a list to character vector
        pos_matches = match(words, pos_words)
        neg_matches = match(words, neg_words)
        pos_matches = !is.na(pos_matches)
        neg_matches = !is.na(neg_matches)
        pp = sum(pos_matches)
        nn = sum(neg_matches)
        score = sum(pos_matches) - sum(neg_matches)
        list1 = c(score, pp, nn)
        return (list1)
      }, pos_words, neg_words)
      score_new = lapply(list, `[[`, 1)
      pp1 = lapply(list, `[[`, 2)
      nn1 = lapply(list, `[[`, 3)
      scores.df = data.frame(score = score_new, text=sentences)
      positive.df = data.frame(Positive = pp1, text=sentences)
      negative.df = data.frame(Negative = nn1, text=sentences)
      list_df = list(scores.df, positive.df, negative.df)
      return(list_df)
    }
    
    result = score.sentiment(sample, pos_words, neg_words)
    
    test1=result[[1]]
    test2=result[[2]]
    test3=result[[3]]
    
    #Creating three different data frames for Score, Positive and Negative
    #Removing text column from data frame
    test1$text=NULL
    test2$text=NULL
    test3$text=NULL
    #Storing the first row(Containing the sentiment scores) in variable q
    q1=test1[1,]
    q2=test2[1,]
    q3=test3[1,]
    qq1=melt(q1, ,var='Score')
    qq2=melt(q2, ,var='Positive')
    qq3=melt(q3, ,var='Negative') 
    qq1['Score'] = NULL
    qq2['Positive'] = NULL
    qq3['Negative'] = NULL
    #Creating data frame
    table1 = data.frame(Text=result[[1]]$text, Score=qq1)
    table2 = data.frame(Text=result[[2]]$text, Score=qq2)
    table3 = data.frame(Text=result[[3]]$text, Score=qq3)
    
    #Merging three data frames into one
    table_final=data.frame(Tweet=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)
    table_final
  })
  output$genre_weight <- renderDataTable({
    user <- input$twitter_handle
    tw1 <- userTimeline(user,n = input$no_of_tweets)
    df <- do.call("rbind", lapply(tw1, as.data.frame))
    df <- twListToDF(tw1)
    df$text <- sapply(df$text, function(row) iconv(row, "latin1", "ASCII", sub=""))
    df$text = gsub("(f|ht)tp(s?)://t.co/[a-z,A-Z,0-9]*", "", df$text)
    sample <- df$text
    library(reshape)
    library(twitteR)
    pos_words = scan('D:/Project/positive-words.txt', what='character', comment.char=';')
    neg_words = scan('D:/Project/negative-words.txt', what='character', comment.char=';')
    
    #For adding extra words in the list  
    wordDatabase<-function()
    {
      pos_words<<-c(pos_words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader', 'power', 'powerful', 'latest')
      neg_words<<-c(neg_words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not')
    }
    
    score.sentiment = function(sentences, pos_words, neg_words, .progress='none')
    {
      require(plyr)
      require(stringr)
      list=lapply(sentences, function(sentence, pos_words, neg_words)
      {
        sentence = gsub('[[:punct:]]',' ',sentence)
        sentence = gsub('[[:cntrl:]]','',sentence)
        sentence = gsub('\\d+','',sentence)  #removes decimal number
        sentence = gsub('\n','',sentence)    #removes new lines
        sentence = tolower(sentence)
        word.list = str_split(sentence, '\\s+')
        words = unlist(word.list)  #changes a list to character vector
        pos_matches = match(words, pos_words)
        neg_matches = match(words, neg_words)
        pos_matches = !is.na(pos_matches)
        neg_matches = !is.na(neg_matches)
        pp = sum(pos_matches)
        nn = sum(neg_matches)
        score = sum(pos_matches) - sum(neg_matches)
        list1 = c(score, pp, nn)
        return (list1)
      }, pos_words, neg_words)
      score_new = lapply(list, `[[`, 1)
      pp1 = lapply(list, `[[`, 2)
      nn1 = lapply(list, `[[`, 3)
      scores.df = data.frame(score = score_new, text=sentences)
      positive.df = data.frame(Positive = pp1, text=sentences)
      negative.df = data.frame(Negative = nn1, text=sentences)
      list_df = list(scores.df, positive.df, negative.df)
      return(list_df)
    }
    
    result = score.sentiment(sample, pos_words, neg_words)
    
    test1=result[[1]]
    test2=result[[2]]
    test3=result[[3]]
    
    #Creating three different data frames for Score, Positive and Negative
    #Removing text column from data frame
    test1$text=NULL
    test2$text=NULL
    test3$text=NULL
    #Storing the first row(Containing the sentiment scores) in variable q
    q1=test1[1,]
    q2=test2[1,]
    q3=test3[1,]
    qq1=melt(q1, ,var='Score')
    qq2=melt(q2, ,var='Positive')
    qq3=melt(q3, ,var='Negative') 
    qq1['Score'] = NULL
    qq2['Positive'] = NULL
    qq3['Negative'] = NULL
    #Creating data frame
    table1 = data.frame(Text=result[[1]]$text, Score=qq1)
    table2 = data.frame(Text=result[[2]]$text, Score=qq2)
    table3 = data.frame(Text=result[[3]]$text, Score=qq3)
    
    #Merging three data frames into one
    table_final=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)
    
    rows <- nrow(table_final)
    tweets_index <- c()
    for(i in 1:rows) {
      if(table_final[i,"Score"]>=0) {
        tweets_index <- c(tweets_index, i) 
      }
    }
    stweet <- sample[tweets_index]
    
    tweet_words <- strsplit(sample," ")
    tweet_words_list <- unlist(tweet_words)
    genrelist <- read.csv(file="genrelist.csv",header = TRUE,sep = ",")
    genrelist <- genrelist[,-1]
    
    action_words <- genrelist[,1]
    action_words <- setdiff(action_words,"")
    animation_words <- genrelist[,2]
    animation_words <- setdiff(animation_words,"")
    mystery_words <- genrelist[,3]
    mystery_words <- setdiff(mystery_words,"")
    comedy_words <- genrelist[,4]
    comedy_words <- setdiff(comedy_words,"")
    drama_words <- genrelist[,5]
    drama_words <- setdiff(drama_words,"")
    family_words <- genrelist[,6]
    family_words <- setdiff(family_words,"")
    horror_words <- genrelist[,7]
    horror_words <- setdiff(horror_words,"")
    romance_words <- genrelist[,8]
    romance_words <- setdiff(romance_words,"")
    thriller_words <- genrelist[,9]
    thriller_words <- setdiff(thriller_words,"")
    adventure_words <- genrelist[,10]
    adventure_words <- setdiff(adventure_words,"")
    scifi_words <- genrelist[,11]
    scifi_words <- setdiff(scifi_words,"")
    
    weight <- vector()
    word_name <- vector()
    
    for(i in 1:length(action_words)) {
      ind <- grep(action_words[i],stweet)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(action_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(action_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,action_words[i])
      }
    }
    
    for(i in 1:length(animation_words)) {
      ind <- grep(animation_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(animation_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(animation_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,animation_words[i])
      }
    }
    for(i in 1:length(mystery_words)) {
      ind <- grep(mystery_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(mystery_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(mystery_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,mystery_words[i])
      }
    }
    for(i in 1:length(comedy_words)) {
      ind <- grep(comedy_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(comedy_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(comedy_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,comedy_words[i])
      }
    }
    for(i in 1:length(drama_words)) {
      ind <- grep(drama_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(drama_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(drama_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,drama_words[i])
      }
    }
    for(i in 1:length(family_words)) {
      ind <- grep(family_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(family_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(family_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,family_words[i])
      }
    }
    for(i in 1:length(horror_words)) {
      ind <- grep(horror_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(horror_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(horror_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,horror_words[i])
      }
    }
    for(i in 1:length(romance_words)) {
      ind <- grep(romance_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(romance_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(romance_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,romance_words[i])
      }
    }
    for(i in 1:length(thriller_words)) {
      ind <- grep(thriller_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(thriller_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(thriller_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,thriller_words[i])
      }
    }
    for(i in 1:length(adventure_words)) {
      ind <- grep(adventure_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(adventure_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(adventure_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,adventure_words[i])
      }
    }
    for(i in 1:length(scifi_words)) {
      ind <- grep(scifi_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(scifi_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(scifi_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,scifi_words[i])
      }
    }
    tweet_weight <- data.frame(word_name,weight,stringsAsFactors = FALSE)
    
    genrelist <- read.csv(file="genrelist.csv",header = TRUE,sep=",")
    genrelist <- genrelist[,-1]
    action_words_list <- as.character(genrelist[,1])
    action_words_list <- setdiff(action_words_list,"")
    animation_words_list <- as.character(genrelist[,2])
    animation_words_list <- setdiff(animation_words_list,"")
    mystery_words_list <- as.character(genrelist[,3])
    mystery_words_list <- setdiff(mystery_words_list,"")
    comedy_words_list <- as.character(genrelist[,4])
    comedy_words_list <- setdiff(comedy_words_list,"")
    drama_words_list <- as.character(genrelist[,5])
    drama_words_list <- setdiff(drama_words_list,"")
    family_words_list <- as.character(genrelist[,6])
    family_words_list <- setdiff(family_words_list,"")
    horror_words_list <- as.character(genrelist[,7])
    horror_words_list <- setdiff(horror_words_list,"")
    romance_words_list <- as.character(genrelist[,8])
    romance_words_list <- setdiff(romance_words_list,"")
    thriller_words_list <- as.character(genrelist[,9])
    thriller_words_list <- setdiff(thriller_words_list,"")
    adventure_words_list <- as.character(genrelist[,10])
    adventure_words_list <- setdiff(adventure_words_list,"")
    scifi_words_list <- as.character(genrelist[,11])
    scifi_words_list <- setdiff(scifi_words_list,"")
    
    weight1 <- vector(length=length(action_words_list))
    actionwords_weight <- data.frame(action_words_list,weight1,stringsAsFactors = FALSE)
    weight2 <- vector(length=length(animation_words_list))
    animationwords_weight <- data.frame(animation_words_list,weight2,stringsAsFactors = FALSE)
    weight3 <- vector(length=length(mystery_words_list))
    mysterywords_weight <- data.frame(mystery_words_list,weight3,stringsAsFactors = FALSE)
    weight4 <- vector(length=length(comedy_words_list))
    comedywords_weight <- data.frame(comedy_words_list,weight4,stringsAsFactors = FALSE)
    weight5 <- vector(length=length(drama_words_list))
    dramawords_weight <- data.frame(drama_words_list,weight5,stringsAsFactors = FALSE)
    weight6 <- vector(length=length(family_words_list))
    familywords_weight <- data.frame(family_words_list,weight6,stringsAsFactors = FALSE)
    weight7 <- vector(length=length(horror_words_list))
    horrorwords_weight <- data.frame(horror_words_list,weight7,stringsAsFactors = FALSE)
    weight8 <- vector(length=length(romance_words_list))
    romancewords_weight <- data.frame(romance_words_list,weight8,stringsAsFactors = FALSE)
    weight9 <- vector(length=length(thriller_words_list))
    thrillerwords_weight <- data.frame(thriller_words_list,weight9,stringsAsFactors = FALSE)
    weight10 <- vector(length=length(adventure_words_list))
    adventurewords_weight <- data.frame(adventure_words_list,weight10,stringsAsFactors = FALSE)
    weight11 <- vector(length=length(scifi_words_list))
    scifiwords_weight <- data.frame(scifi_words_list,weight11,stringsAsFactors = FALSE)
    
    tf=log10(1+1)
    idf=log10(11/1)
    tf_idf <- tf*idf
    
    for(i in 1:nrow(actionwords_weight)) {
      actionwords_weight[i,2]<-tf_idf
    }
    for(i in 1:nrow(animationwords_weight)) {
      animationwords_weight[i,2]<-tf_idf
    }
    for(i in 1:nrow(mysterywords_weight)) {
      mysterywords_weight[i,2]<-tf_idf
    }
    for(i in 1:nrow(comedywords_weight)) {
      comedywords_weight[i,2]<-tf_idf
    }
    for(i in 1:nrow(dramawords_weight)) {
      dramawords_weight[i,2]<-tf_idf
    }
    for(i in 1:nrow(familywords_weight)) {
      familywords_weight[i,2]<-tf_idf
    }
    for(i in 1:nrow(horrorwords_weight)) {
      horrorwords_weight[i,2]<-tf_idf
    }
    for(i in 1:nrow(romancewords_weight)) {
      romancewords_weight[i,2]<-tf_idf
    }
    for(i in 1:nrow(thrillerwords_weight)) {
      thrillerwords_weight[i,2]<-tf_idf
    }
    for(i in 1:nrow(adventurewords_weight)) {
      adventurewords_weight[i,2]<-tf_idf
    } 
    for(i in 1:nrow(scifiwords_weight)) {
      scifiwords_weight[i,2]<-tf_idf
    }
    
    movie_matrix <- read.csv(file="movie_matrix.csv",header = TRUE,sep = ",")
    movie_matrix <- movie_matrix[,-1]
    movie_name <- as.character(movie_matrix[,"Movie_Title"])
    
    genre <- c("Action","Animation","Mystery","Comedy","Drama","Family","Horror","Romance","Thriller","Adventure","Sci-Fi")
    weight<-vector(length=length(genre))
    genrematrix <- data.frame(genre,weight,stringsAsFactors = FALSE)
    
    no_words<-nrow(tweet_weight)
    if(no_words==0) {
      for(i in 1:length(genre)) {
        genrematrix[i,2] <- 0
      }
    }
    if(nrow(tweet_weight)!=0) {
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],action_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          genrematrix[1,2] <- genrematrix[1,2]+(tweet_weight[i,2]*actionwords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],animation_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          genrematrix[2,2] <- genrematrix[2,2]+(tweet_weight[i,2]*animationwords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],mystery_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          
          genrematrix[3,2] <- genrematrix[3,2]+(tweet_weight[i,2]*mysterywords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],comedy_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          
          genrematrix[4,2] <- genrematrix[4,2]+(tweet_weight[i,2]*comedywords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],drama_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          
          genrematrix[5,2] <- genrematrix[5,2]+(tweet_weight[i,2]*dramawords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],family_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          
          genrematrix[6,2] <- genrematrix[6,2]+(tweet_weight[i,2]*familywords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],horror_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          
          genrematrix[7,2] <- genrematrix[7,2]+(tweet_weight[i,2]*horrorwords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],romance_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          
          genrematrix[8,2] <- genrematrix[8,2]+(tweet_weight[i,2]*romancewords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],thriller_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          
          genrematrix[9,2] <- genrematrix[9,2]+(tweet_weight[i,2]*thrillerwords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],adventure_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          
          genrematrix[10,2] <- genrematrix[10,2]+(tweet_weight[i,2]*adventurewords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],scifi_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          
          genrematrix[11,2] <- genrematrix[11,2]+(tweet_weight[i,2]*scifiwords_weight[ind[1],2])
        }
      }
    }
    genrematrix <- genrematrix[order(genrematrix$weight,decreasing = TRUE),]
    genrematrix
  })
  output$trending_movies <- renderDataTable({
    user <- input$twitter_handle
    tw1 <- userTimeline(user,n = input$no_of_tweets)
    df <- do.call("rbind", lapply(tw1, as.data.frame))
    df <- twListToDF(tw1)
    df$text <- sapply(df$text, function(row) iconv(row, "latin1", "ASCII", sub=""))
    df$text = gsub("(f|ht)tp(s?)://t.co/[a-z,A-Z,0-9]*", "", df$text)
    sample <- df$text
    library(reshape)
    library(twitteR)
    pos_words = scan('D:/Project/positive-words.txt', what='character', comment.char=';')
    neg_words = scan('D:/Project/negative-words.txt', what='character', comment.char=';')
    
    #For adding extra words in the list  
    wordDatabase<-function()
    {
      pos_words<<-c(pos_words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader', 'power', 'powerful', 'latest')
      neg_words<<-c(neg_words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not')
    }
    
    score.sentiment = function(sentences, pos_words, neg_words, .progress='none')
    {
      require(plyr)
      require(stringr)
      list=lapply(sentences, function(sentence, pos_words, neg_words)
      {
        sentence = gsub('[[:punct:]]',' ',sentence)
        sentence = gsub('[[:cntrl:]]','',sentence)
        sentence = gsub('\\d+','',sentence)  #removes decimal number
        sentence = gsub('\n','',sentence)    #removes new lines
        sentence = tolower(sentence)
        word.list = str_split(sentence, '\\s+')
        words = unlist(word.list)  #changes a list to character vector
        pos_matches = match(words, pos_words)
        neg_matches = match(words, neg_words)
        pos_matches = !is.na(pos_matches)
        neg_matches = !is.na(neg_matches)
        pp = sum(pos_matches)
        nn = sum(neg_matches)
        score = sum(pos_matches) - sum(neg_matches)
        list1 = c(score, pp, nn)
        return (list1)
      }, pos_words, neg_words)
      score_new = lapply(list, `[[`, 1)
      pp1 = lapply(list, `[[`, 2)
      nn1 = lapply(list, `[[`, 3)
      scores.df = data.frame(score = score_new, text=sentences)
      positive.df = data.frame(Positive = pp1, text=sentences)
      negative.df = data.frame(Negative = nn1, text=sentences)
      list_df = list(scores.df, positive.df, negative.df)
      return(list_df)
    }
    
    result = score.sentiment(sample, pos_words, neg_words)
    
    test1=result[[1]]
    test2=result[[2]]
    test3=result[[3]]
    
    #Creating three different data frames for Score, Positive and Negative
    #Removing text column from data frame
    test1$text=NULL
    test2$text=NULL
    test3$text=NULL
    #Storing the first row(Containing the sentiment scores) in variable q
    q1=test1[1,]
    q2=test2[1,]
    q3=test3[1,]
    qq1=melt(q1, ,var='Score')
    qq2=melt(q2, ,var='Positive')
    qq3=melt(q3, ,var='Negative') 
    qq1['Score'] = NULL
    qq2['Positive'] = NULL
    qq3['Negative'] = NULL
    #Creating data frame
    table1 = data.frame(Text=result[[1]]$text, Score=qq1)
    table2 = data.frame(Text=result[[2]]$text, Score=qq2)
    table3 = data.frame(Text=result[[3]]$text, Score=qq3)
    
    #Merging three data frames into one
    table_final=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)
    
    rows <- nrow(table_final)
    tweets_index <- c()
    for(i in 1:rows) {
      if(table_final[i,"Score"]>=0) {
        tweets_index <- c(tweets_index, i) 
      }
    }
    stweet <- sample[tweets_index]
    
    tweet_words <- strsplit(sample," ")
    tweet_words_list <- unlist(tweet_words)
    genrelist <- read.csv(file="genrelist.csv",header = TRUE,sep = ",")
    genrelist <- genrelist[,-1]
    
    action_words <- genrelist[,1]
    action_words <- setdiff(action_words,"")
    animation_words <- genrelist[,2]
    animation_words <- setdiff(animation_words,"")
    mystery_words <- genrelist[,3]
    mystery_words <- setdiff(mystery_words,"")
    comedy_words <- genrelist[,4]
    comedy_words <- setdiff(comedy_words,"")
    drama_words <- genrelist[,5]
    drama_words <- setdiff(drama_words,"")
    family_words <- genrelist[,6]
    family_words <- setdiff(family_words,"")
    horror_words <- genrelist[,7]
    horror_words <- setdiff(horror_words,"")
    romance_words <- genrelist[,8]
    romance_words <- setdiff(romance_words,"")
    thriller_words <- genrelist[,9]
    thriller_words <- setdiff(thriller_words,"")
    adventure_words <- genrelist[,10]
    adventure_words <- setdiff(adventure_words,"")
    scifi_words <- genrelist[,11]
    scifi_words <- setdiff(scifi_words,"")
    
    weight <- vector()
    word_name <- vector()
    
    for(i in 1:length(action_words)) {
      ind <- grep(action_words[i],stweet)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(action_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(action_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,action_words[i])
      }
    }
    
    for(i in 1:length(animation_words)) {
      ind <- grep(animation_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(animation_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(animation_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,animation_words[i])
      }
    }
    for(i in 1:length(mystery_words)) {
      ind <- grep(mystery_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(mystery_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(mystery_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,mystery_words[i])
      }
    }
    for(i in 1:length(comedy_words)) {
      ind <- grep(comedy_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(comedy_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(comedy_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,comedy_words[i])
      }
    }
    for(i in 1:length(drama_words)) {
      ind <- grep(drama_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(drama_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(drama_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,drama_words[i])
      }
    }
    for(i in 1:length(family_words)) {
      ind <- grep(family_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(family_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(family_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,family_words[i])
      }
    }
    for(i in 1:length(horror_words)) {
      ind <- grep(horror_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(horror_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(horror_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,horror_words[i])
      }
    }
    for(i in 1:length(romance_words)) {
      ind <- grep(romance_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(romance_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(romance_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,romance_words[i])
      }
    }
    for(i in 1:length(thriller_words)) {
      ind <- grep(thriller_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(thriller_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(thriller_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,thriller_words[i])
      }
    }
    for(i in 1:length(adventure_words)) {
      ind <- grep(adventure_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(adventure_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(adventure_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,adventure_words[i])
      }
    }
    for(i in 1:length(scifi_words)) {
      ind <- grep(scifi_words[i],sample)
      ind_count <- length(ind)
      if(ind_count>0) {
        total_tweets <- length(stweet)
        #term_freq <- length(grep(scifi_words[i],tweet_words_list))
        #word_count <- length(unlist(tweet_words[ind]))
        #tf <- (log10(1+term_freq))
        stweet_words <- str_split(stweet[ind]," ")
        totalwords <- unlist(stweet_words)
        tf <- length(grep(scifi_words[i],totalwords))/length(totalwords)
        idf <- log10(total_tweets/ind_count)
        weight <- c(weight,tf*idf)
        word_name <- c(word_name,scifi_words[i])
      }
    }
    tweet_weight <- data.frame(word_name,weight,stringsAsFactors = FALSE)
    
    genrelist <- read.csv(file="genrelist.csv",header = TRUE,sep=",")
    genrelist <- genrelist[,-1]
    action_words_list <- as.character(genrelist[,1])
    action_words_list <- setdiff(action_words_list,"")
    animation_words_list <- as.character(genrelist[,2])
    animation_words_list <- setdiff(animation_words_list,"")
    mystery_words_list <- as.character(genrelist[,3])
    mystery_words_list <- setdiff(mystery_words_list,"")
    comedy_words_list <- as.character(genrelist[,4])
    comedy_words_list <- setdiff(comedy_words_list,"")
    drama_words_list <- as.character(genrelist[,5])
    drama_words_list <- setdiff(drama_words_list,"")
    family_words_list <- as.character(genrelist[,6])
    family_words_list <- setdiff(family_words_list,"")
    horror_words_list <- as.character(genrelist[,7])
    horror_words_list <- setdiff(horror_words_list,"")
    romance_words_list <- as.character(genrelist[,8])
    romance_words_list <- setdiff(romance_words_list,"")
    thriller_words_list <- as.character(genrelist[,9])
    thriller_words_list <- setdiff(thriller_words_list,"")
    adventure_words_list <- as.character(genrelist[,10])
    adventure_words_list <- setdiff(adventure_words_list,"")
    scifi_words_list <- as.character(genrelist[,11])
    scifi_words_list <- setdiff(scifi_words_list,"")
    
    weight1 <- vector(length=length(action_words_list))
    actionwords_weight <- data.frame(action_words_list,weight1,stringsAsFactors = FALSE)
    weight2 <- vector(length=length(animation_words_list))
    animationwords_weight <- data.frame(animation_words_list,weight2,stringsAsFactors = FALSE)
    weight3 <- vector(length=length(mystery_words_list))
    mysterywords_weight <- data.frame(mystery_words_list,weight3,stringsAsFactors = FALSE)
    weight4 <- vector(length=length(comedy_words_list))
    comedywords_weight <- data.frame(comedy_words_list,weight4,stringsAsFactors = FALSE)
    weight5 <- vector(length=length(drama_words_list))
    dramawords_weight <- data.frame(drama_words_list,weight5,stringsAsFactors = FALSE)
    weight6 <- vector(length=length(family_words_list))
    familywords_weight <- data.frame(family_words_list,weight6,stringsAsFactors = FALSE)
    weight7 <- vector(length=length(horror_words_list))
    horrorwords_weight <- data.frame(horror_words_list,weight7,stringsAsFactors = FALSE)
    weight8 <- vector(length=length(romance_words_list))
    romancewords_weight <- data.frame(romance_words_list,weight8,stringsAsFactors = FALSE)
    weight9 <- vector(length=length(thriller_words_list))
    thrillerwords_weight <- data.frame(thriller_words_list,weight9,stringsAsFactors = FALSE)
    weight10 <- vector(length=length(adventure_words_list))
    adventurewords_weight <- data.frame(adventure_words_list,weight10,stringsAsFactors = FALSE)
    weight11 <- vector(length=length(scifi_words_list))
    scifiwords_weight <- data.frame(scifi_words_list,weight11,stringsAsFactors = FALSE)
    
    tf=log10(1+1)
    idf=log10(11/1)
    tf_idf <- tf*idf
    
    for(i in 1:nrow(actionwords_weight)) {
      actionwords_weight[i,2]<-tf_idf
    }
    for(i in 1:nrow(animationwords_weight)) {
      animationwords_weight[i,2]<-tf_idf
    }
    for(i in 1:nrow(mysterywords_weight)) {
      mysterywords_weight[i,2]<-tf_idf
    }
    for(i in 1:nrow(comedywords_weight)) {
      comedywords_weight[i,2]<-tf_idf
    }
    for(i in 1:nrow(dramawords_weight)) {
      dramawords_weight[i,2]<-tf_idf
    }
    for(i in 1:nrow(familywords_weight)) {
      familywords_weight[i,2]<-tf_idf
    }
    for(i in 1:nrow(horrorwords_weight)) {
      horrorwords_weight[i,2]<-tf_idf
    }
    for(i in 1:nrow(romancewords_weight)) {
      romancewords_weight[i,2]<-tf_idf
    }
    for(i in 1:nrow(thrillerwords_weight)) {
      thrillerwords_weight[i,2]<-tf_idf
    }
    for(i in 1:nrow(adventurewords_weight)) {
      adventurewords_weight[i,2]<-tf_idf
    } 
    for(i in 1:nrow(scifiwords_weight)) {
      scifiwords_weight[i,2]<-tf_idf
    }
    
    genre <- c("Action","Animation","Mystery","Comedy","Drama","Family","Horror","Romance","Thriller","Adventure","Sci-Fi")
    weight<-vector(length=length(genre))
    genrematrix <- data.frame(genre,weight,stringsAsFactors = FALSE)
    
    no_words<-nrow(tweet_weight)
    if(no_words==0) {
      for(i in 1:length(genre)) {
        genrematrix[i,2] <- 0
      }
    }
    if(nrow(tweet_weight)!=0) {
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],action_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          genrematrix[1,2] <- genrematrix[1,2]+(tweet_weight[i,2]*actionwords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],animation_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          genrematrix[2,2] <- genrematrix[2,2]+(tweet_weight[i,2]*animationwords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],mystery_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          
          genrematrix[3,2] <- genrematrix[3,2]+(tweet_weight[i,2]*mysterywords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],comedy_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          
          genrematrix[4,2] <- genrematrix[4,2]+(tweet_weight[i,2]*comedywords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],drama_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          
          genrematrix[5,2] <- genrematrix[5,2]+(tweet_weight[i,2]*dramawords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],family_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          
          genrematrix[6,2] <- genrematrix[6,2]+(tweet_weight[i,2]*familywords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],horror_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          
          genrematrix[7,2] <- genrematrix[7,2]+(tweet_weight[i,2]*horrorwords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],romance_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          
          genrematrix[8,2] <- genrematrix[8,2]+(tweet_weight[i,2]*romancewords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],thriller_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          
          genrematrix[9,2] <- genrematrix[9,2]+(tweet_weight[i,2]*thrillerwords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],adventure_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          
          genrematrix[10,2] <- genrematrix[10,2]+(tweet_weight[i,2]*adventurewords_weight[ind[1],2])
        }
      }
      for(i in 1:nrow(tweet_weight)) {
        ind <- grep(word_name[i],scifi_words_list,fixed=TRUE)
        ind_length <- length(ind)
        if(ind_length!=0) {
          
          genrematrix[11,2] <- genrematrix[11,2]+(tweet_weight[i,2]*scifiwords_weight[ind[1],2])
        }
      }
    }
    trend_moviematrix <- read.csv(file="trending_movies_matrix.csv",header = TRUE,sep=",",stringsAsFactors = FALSE)
    movie_name <- as.character(trend_moviematrix[,1])
    movie_weight <- vector(length=length(movie_name))
    multiply_result_matrix <- data.frame(movie_name,movie_weight,stringsAsFactors = FALSE)
    for(i in 1:nrow(multiply_result_matrix)) {
      m <- 0
      for(j in 2:12) {
        m <- m + (trend_moviematrix[i,j]*genrematrix[j-1,2])
      }
      multiply_result_matrix[i,2] <- m
    }
    #sort matrix in decreasing order of movie weight
    multiply_result_matrix <- multiply_result_matrix[order(multiply_result_matrix$movie_weight,decreasing = TRUE),]
    names(multiply_result_matrix)[1] <- "Movie"
    names(multiply_result_matrix)[2] <- "Weight"
    multiply_result_matrix[,]
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

