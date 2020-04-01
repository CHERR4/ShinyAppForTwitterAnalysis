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

getTopTweets <- function() {
	return(tweets.df %>%
	select(text, screenName, retweetCount, favoriteCount) %>%
	arrange(desc(retweetCount)))
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
	return(ggplot(tweets.sentiment, aes(tweet, sentiment)) +
  		geom_col(show.legend = FALSE))
}

# TODO: fix duplicated code to get tweets.sentiments and word.tokens
getTopWordBySentimentPlot <- function() {
	arranged.tweets <- tweets.df %>%
  		arrange(desc(retweetCount))
	word.count <- arranged.tweets %>%
					select(text) %>%
					unnest_tokens(word,text) %>%
					inner_join(get_sentiments("bing")) %>%
					count(word, sentiment) %>%
					top_n(10, n)
	ggplot(word.count, aes(x = word, y = n)) +
	geom_col(show.legend = FALSE, aes(colour = sentiment, fill = sentiment))
}

server <- function(input, output) { 
	observeEvent(input$submit, {
		load.tweets(input$text)

		output$all.tweets <- DT::renderDataTable({
			DT::datatable(getTweets(),options = list(
			pageLength = 5  ,
			lengthMenu = c(5, 10, 20, 25)
			))
		})

		output$top.tweets <- DT::renderDataTable({
			DT::datatable(getTopTweets(),options = list(
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
			pageLength = 10,
			lengthMenu = c(5, 10, 20, 25)
			))
		})

		output$wordcloud <- renderPlot({
			getWordcloud()
		})

		output$sentiments.by.tweet <- renderPlot({
			getSentimentPlot()
		})

		output$top.words <- renderPlot({
			getTopWordBySentimentPlot()
		})
	})
}