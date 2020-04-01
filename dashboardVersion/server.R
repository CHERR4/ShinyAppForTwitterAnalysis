library("twitteR")
library("plyr")
library("tidyverse")
library("tidytext")
library("tm")
library("wordcloud")
library("ggplot2")
library("tidyr")
library("reshape2")



CONSUMER_KEY <- 'NV6XI9LgMbION0RJsBpTzKtzJ'
CONSUMER_SECRET <- 'sPemSetJ8mPqQuxD1iSueB707TaIDjC5NwNAkK64exDrVs77Gs'
access_token <- '2488412620-6UF4rbzupzdlxQU5I6ydbRXPcp2P7gdzZQoaxco'
access_secret <- 'F3ltkyqteUC0TMRv75yylp8ipoLXvzv83CZplJs1DZgbO'
setup_twitter_oauth(CONSUMER_KEY, CONSUMER_SECRET, access_token, access_secret)


getTweetsFromWord <- function(word) {
    tweets <- searchTwitter(word,n=1500, lang = "en")
    tweets.df <<- ldply(tweets, function(t) t$toDataFrame())
    write.csv(tweets.df, file = paste("data/", word,".csv"), row.names = FALSE)
}

getTweetsFromWordCsv <- function(word) {
    tweets.df <<- read_csv(paste("data/", word, ".csv"))
}

load.tweets <- function(word) {
	if(!file.exists(paste("data/", word, ".csv"))) {
        getTweetsFromWord(word)
    } else {
        getTweetsFromWordCsv(word)
        #getTweetsFromWord(word)
    }
}

getTweets <- function() {
    # tweets <- searchTwitter(word,n=1500)
    # Convertimos a data.frame
	return(select(tweets.df, text, screenName))
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
	docsCorpus <- tm_map(docsCorpus, removeWords, stopwords("english"))
	indeseadas <- c("rt") # algunas palabras que no me interesan y que me aparecen en el análisis
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
	min.freq=50, max.words=50, scale=c(8, .01), 
	colors=brewer.pal(6, "Dark2"))
	return(wc1)
}

getWordcloud <- function() {
	arranged.tweets <- tweets.df %>%
  		arrange(desc(retweetCount))
	word.tokens <<- arranged.tweets %>%
        select(text) %>%
        mutate(tweet = row_number()) %>%
        unnest_tokens(word, text)
	return(	word.tokens %>%
    	inner_join(get_sentiments("bing")) %>%
  		count(word, sentiment, sort = TRUE) %>%
  		acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  		comparison.cloud(colors = c("red", "green"),
                   max.words = 100))
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
			summarise_at(vars(retweetCount),
			list(rt = sum))
			%>%
			arrange(desc(rt)))
}

getWordTibble <- function() {
	word.tibble <<- tweets.df %>%
	mutate(ntweet = row_number()) %>%
	select(ntweet, text) %>% 
	unnest_tokens(word, text) %>%
	anti_join(get_stopwords())
}

getSentimentPlot <- function() {
	arranged.tweets <- tweets.df %>%
  		arrange(desc(retweetCount))
	word.tokens <<- arranged.tweets %>%
        select(text) %>%
        mutate(tweet = row_number()) %>%
        unnest_tokens(word, text)
	tweets.sentiment <- word.tokens %>%
		inner_join(get_sentiments("bing")) %>%
		count(tweet, sentiment) %>%
		spread(sentiment, n, fill = 0) %>%
		mutate(sentiment = positive - negative)
	ggplot(tweets.sentiment, aes(tweet, sentiment)) +
  		geom_col(show.legend = FALSE)
}

server <- function(input, output) { 

    load.tweets("coronavirus")

    output$all.tweets <- DT::renderDataTable({
        DT::datatable(getTweets(),options = list(
        pageLength = 5  ,
        lengthMenu = c(5, 10, 20, 25)
        ))
    })

    output$most.tweeters <- DT::renderDataTable({
        DT::datatable(getMostTweeters(),options = list(
        pageLength = 5,
        lengthMenu = c(5, 10, 20, 25)
        ))
    })

    output$top.tweeters <- DT::renderDataTable({
        DT::datatable(getTopTweeters(),options = list(
        pageLength = 5,
        lengthMenu = c(5, 10, 20, 25)
        ))
    })

    output$top.words <- renderPlot({
        getWordcloud()
    })

	output$sentiments <- renderPlot({
        getSentimentPlot()
    })
}