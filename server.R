library(shiny)
library("twitteR")
library("tm")
library(wordcloud)
library(wordcloud2)
library(DT)
library(plyr); library(dplyr); library("tidyverse")
CONSUMER_KEY <- 
CONSUMER_SECRET <- 
access_token <- 
access_secret <- 
setup_twitter_oauth(CONSUMER_KEY, CONSUMER_SECRET, access_token, access_secret)


getTweetsFromWord <- function(word) {
    tweets <- searchTwitter(word,n=1500)
    tweets.df <<- ldply(tweets, function(t) t$toDataFrame())
    write.csv(tweets.df, file = paste("data/", word,".csv"), row.names = FALSE)
	browser()
}

getTweetsFromWordCsv <- function(word) {
    tweets.df <<- read_csv(paste("data/", word, ".csv"))
}

load.tweets <- function(word) {
	if(!file.exists(paste("data/", word, ".csv"))) {
        getTweetsFromWord(word)
    } else {
        getTweetsFromWordCsv(word)
    }
}

#docsCorpus <- stri_trans_general(docsCorpus,"Latin-ASCII")
limpiar <- function(x){
	x <- gsub("http[[:alnum:]]*",'', x)
	x <- gsub('http\\S+\\s*', '', x) ## Remove URLs
	x <- gsub('\\b+RT', '', x) ## Remove RT
	x <- gsub('#\\S+', '', x) ## Remove Hashtags
	x <- gsub('@\\S+', '', x) ## Remove Mentions
	x <- gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
	x <- gsub("\\d", '', x) ## Remove Controls and special characters
	x <- gsub('[[:punct:]]', '', x) ## Remove Punctuations
	x <- gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
	x <- gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
	x <- gsub(' +',' ',x) ## Remove extra whitespaces
	x <- iconv(x, to='ASCII//TRANSLIT') # Remove accent and special letter
	return(x)
}

getCorpus <- function(text) {
	return(tm_map(Corpus(VectorSource(text)), function(x) iconv(enc2utf8(x), sub = "byte")))
}

cleanCorpus <- function(docsCorpus) {
	# quito las mayúsculas
	docsCorpus <- tm_map(docsCorpus, tolower)
	docsCorpus <- tm_map(docsCorpus, limpiar)
	docsCorpus <- tm_map(docsCorpus, removeWords, stopwords("spanish"))
	indeseadas <- c("rt", "tcovqnuaejl") # algunas palabras que no me interesan y que me aparecen en el análisis
	docsCorpus <- tm_map(docsCorpus, removeWords, indeseadas)
	return(docsCorpus)
}

getDtm <- function(corpus, sparse=NULL) {
	dtm <- DocumentTermMatrix(corpus)
	if(!is.null(sparse)) {
		dtm <- removeTermMatrix(dtm, sparse)
	}
	return(dtm)
}

getFreqFromDtm <- function(dtm) {
	freq <- as.data.frame(colSums(as.matrix(dtm)))
	freq <- mutate(freq,termino= rownames(freq))
	colnames(freq) <- c("frecuencia","termino")
	cols <- c("termino", "frecuencia")
	freq <- freq[cols]
	return(freq)
}


getWordcloudFromFreq <- function(freq) {
	set.seed(142)
	# Distintas paletas de colores
	brewer.pal(6, "Dark2")
	brewer.pal(9,"YlGnBu")
	wc1 <- wordcloud(freq$termino, freq$frecuencia, 
	min.freq=500, max.words=50, scale=c(3, .01), 
	colors=brewer.pal(6, "Dark2"))
	return(wc1)
}

getWordcloud <- function() {
	corpus <- getCorpus(tweets.df$text)
	corpus <- cleanCorpus(corpus)
	dtm <- getDtm(corpus)
	freq <- getFreqFromDtm(dtm)
	return(getWordcloudFromFreq(freq))
}

getTweets <- function() {
  # tweets <- searchTwitter(word,n=1500)
  # Convertimos a data.frame
	tweets.df.2 <- select(tweets.df, text, screenName)
	return(tweets.df.2)
}

getTopDevices <- function(top) {
	# review when top > dim

	grouped.tweets.df <- group_by(tweets.df, statusSource)
	grouped.tweets.df <- select(grouped.tweets.df, statusSource)
 	n.tweets.device <- unique(dplyr::mutate(grouped.tweets.df,number=n()))
	# row.names(n.tweets.device) <- n.tweets.device[1]
	top_n(n.tweets.device, 5)
	n.tweets.device <- n.tweets.device[1:top,1:2]
	return(barplot(n.tweets.device$number, main="Device/Tweets", xlab="Device", las=2))
}

getMostTweeters <- function() {
	tweets.df.2 <- select(tweets.df, screenName)
	grouped.tweets <- group_by(tweets.df.2, screenName)
	n.tweets.user <- arrange(unique(dplyr::mutate(grouped.tweets, number=n())), desc(number))
	return(n.tweets.user)
}

getTopTweeters <- function() {
	# TODO: this is another function to switch with most tweeters
	tweets.df.2 <- select(tweets.df, screenName, favoriteCount, retweetCount)
	return(tweets.df.2 %>%
			group_by(screenName) %>%
			summarise_at(vars(favoriteCount, retweetCount),
			list(favs = sum))
			%>%
			arrange(desc(favoriteCount_favs), desc(retweetCount_favs))
			%>%
			dplyr::rename(favs = favoriteCount_favs, retweets = retweetCount_favs))
}

# Define server logic ----
server <- function(input, output) {
	
	observeEvent(input$submit, {
			load.tweets(input$text)

		output$table <- DT::renderDataTable({
			DT::datatable(getTweets(),options = list(
			pageLength = 3,
			lengthMenu = c(3, 5, 10, 20)
			))
		})

		output$table <- DT::renderDataTable({
			DT::datatable(getTweets(),options = list(
			pageLength = 3,
			lengthMenu = c(3, 5, 10, 20)
			))
		})

		output$topDevices <- renderPlot({
			getTopDevices(input$topDevices)
		})


		output$users <- DT::renderDataTable({
			DT::datatable(getTopTweeters(), options = list(
			pageLength = 5,
			lengthMenu = c(5, 10, 20, 25)
			))
		})

		output$topWords <- renderPlot({
			getWordcloud()
		})
	})
	
}