library("rtweet")
library("httpuv")
library("plyr")
library("tidyverse")
library("tidytext")
library("tm")
library("wordcloud")
library("ggplot2")
library("tidyr")
library("reshape2")
library("lubridate")	
library("igraph")
library("ggraph")
# library("topicmodels")


load.tweets <- function(word) {
	if(!file.exists(paste("data/", word, ".csv"))) {
		tweets.tibble <<- search_tweets(word, n = 1500, include_rts = FALSE, lang = "en")
		tweets.tibble <<- tweets.tibble %>% select_if(~!is.list(.))
    	write.csv(as.data.frame(tweets.tibble), file = paste("data/", word,".csv"), row.names = FALSE)
    } else {
		tweets.tibble <<- read_csv(paste("data/", word, ".csv"))
    }
}

load.user <- function(user.text) {
	user.name <<- user.text
	if(!file.exists(paste("data/user.", user.name, ".csv"))){
		tweets.tibble <<- get_timelines(user.name, n = 1000)
		tweets.tibble <<- tweets.tibble %>% filter(!is_retweet)
		tweets.tibble <<- tweets.tibble %>% select_if(~!is.list(.))
		write.csv(as.data.frame(tweets.tibble), file = paste("data/user.", user.name, ".csv"), row.names = FALSE)
	} else {
		tweets.tibble <<- read_csv(paste("data/user.", user.name, ".csv"))
	}
	friends.id <- get_friends(user.name)
	followed <<- lookup_users(friends.id$user_id)%>%
		select(screen_name, user_id)
	followers.id <- get_followers(user.name)
	followers <<- lookup_users(followers.id$user_id)%>%
		select(screen_name, user_id)
}

getTweetsUser <- function() {
	return(tweets.tibble %>%
	select(text, screen_name, retweet_count, favorite_count))
}

getLikedTweets <- function() {
	return(get_favorites(user.name))
}

getTopTweetsUser <- function() {
	return(tweets.tibble %>%
	select(text, screen_name, retweet_count, favorite_count) %>%
	arrange(desc(retweet_count), desc(favorite_count)))
}

getDontFollowYou <- function() {
	return(anti_join(followed, followers))
}

getYouDontFollow <- function() {
	return(anti_join(followers, followed))
}

getMutuals <- function() {
	return(inner_join(followers, followed))
}

getTweets <- function() {
	return(select(tweets.tibble, text, screen_name))
}

getTopTweets <- function() {
	return(tweets.tibble %>%
	select(text, screen_name, retweet_count, favorite_count) %>%
	arrange(desc(retweet_count)))
}

getWordcloud <- function() {
	arranged.tweets <- tweets.tibble %>%
  		arrange(desc(retweet_count))
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

getWordcloudUser <- function() {
	word.tokens <<- tweets.tibble %>%
        select(text) %>%
        mutate(tweet = row_number()) %>%
        unnest_tokens(word, text)
	stopwords <- c("https", "t.co", "http")
	stopwords <- append(stopwords, followed$screen_name)
	stopwords <- append(stopwords, followers$screen_name)
	return(	word.tokens %>%
		anti_join(get_stopwords("es", "snowball")) %>%
		filter(!(word %in% stopwords)) %>%	
  		count(word) %>%
  		with(wordcloud(word, n, max.words = 100)))
}

getMostTweeters <- function() {
	tweets.tibble.2 <- select(tweets.tibble, screen_name)
	grouped.tweets <- group_by(tweets.tibble.2, screen_name)
	n.tweets.tibble <- arrange(unique(dplyr::mutate(grouped.tweets, number=n())), desc(number))
	return(n.tweets.tibble)
}

getTopTweeters <- function() {
	tweets.tibble.2 <- select(tweets.tibble, screen_name, favorite_count, retweet_count)
	return(tweets.tibble.2 %>%
			group_by(screen_name) %>%
			summarise_at(vars(retweet_count),
			list(rt = sum))
			%>%
			arrange(desc(rt)))
}

getWordTibble <- function() {
	word.tibble <<- tweets.tibble %>%
	mutate(ntweet = row_number()) %>%
	select(ntweet, text) %>% 
	unnest_tokens(word, text) %>%
	anti_join(get_stopwords())
}

getSentimentPlot <- function() {
	arranged.tweets <- tweets.tibble %>%
  		arrange(desc(retweet_count))
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
	arranged.tweets <- tweets.tibble %>%
  		arrange(desc(retweet_count))
	word.count <- arranged.tweets %>%
					select(text) %>%
					unnest_tokens(word,text) %>%
					inner_join(get_sentiments("bing")) %>%
					count(word, sentiment) %>%
					top_n(10, n)
	ggplot(word.count, aes(x = word, y = n)) +
	geom_col(show.legend = FALSE, aes(colour = sentiment, fill = sentiment))
}

getTweetsPerWeek <- function() {
	tweets.tibble %>%
		ts_plot("7 days") +
		theme_minimal() +
		theme(plot.title = element_text(face = "bold")) +
		labs(
			x = NULL, y = NULL,
			title = paste("Frequency of tweets from", user.name),
			subtitle = "Twitter status (tweet) counts aggregated using 1 week intervals"
		)
}

getTweetsPerHour <- function() {
	tweets.hour <- tweets.tibble %>%
					transmute(hour(created_at)) %>%
					rename("hour" = "hour(created_at)")
	return(ggplot(tweets.hour, aes(x = hour)) +
	geom_bar(stat = "count", fill = "blue") +
	theme_minimal())
}

getTopCorrelatedWords <- function(tweets) {
	stopwords <- c("https", "t.co", "http")
	word.bigrams <- tweets %>% 
		select(text) %>%
		unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
		separate(bigram, c("word1", "word2", sep = " ")) %>%
		filter(!word1 %in% union(stop_words$word, stopwords)) %>%
		filter(!word2 %in% union(stop_words$word, stopwords)) %>%
		count(word1, word2, sort = TRUE) %>% 
		filter(n > quantile(n, .98)) %>%
  		graph_from_data_frame()

	set.seed(2017)

	return(ggraph(word.bigrams, layout = "fr") +
		geom_edge_link() +
		geom_node_point() +
		geom_node_text(aes(label = name), vjust = 1, hjust = 1))
}

getNumberOfTopicsPlot <- function() {
	return(number_topics(DataFrame = tweets.tibble, 
		num_cores = 2L, 
		min_clusters = 2, 
		max_clusters = 12, 
		skip = 1, 
		set_seed = 1234))
}


server <- function(input, output) { 
	observeEvent(input$topic, {
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

		output$number.of.topics <- renderPlot({
			getNumberOfTopicsPlot()
		})
	})

	observeEvent(input$user.submit, {
		load.user(input$user)

		output$all.tweets.user <- DT::renderDataTable({
		 	DT::datatable(getTweetsUser(),options = list(
		 	pageLength = 5  ,
		 	lengthMenu = c(5, 10, 20, 25)
		 	))
		 })

		output$top.tweets.user <- DT::renderDataTable({
		  	DT::datatable(getTopTweetsUser(),options = list(
		  	pageLength = 5  ,
		  	lengthMenu = c(5, 10, 20, 25)
		  	))
		})

		output$followers <- DT::renderDataTable({
			DT::datatable(followers,options = list(
		 	pageLength = 5,
		 	lengthMenu = c(5, 10, 20, 25)
		 	))
		})

		output$followed <- DT::renderDataTable({
			DT::datatable(followed,options = list(
		 	pageLength = 5,
		 	lengthMenu = c(5, 10, 20, 25)
		 	))
		})

		output$dont.follow.you <- DT::renderDataTable({
		 	DT::datatable(getDontFollowYou(),options = list(
		 	pageLength = 5,
		 	lengthMenu = c(5, 10, 20, 25)
		 	))
		})

		output$you.dont.follow <- DT::renderDataTable({
		 	DT::datatable(getYouDontFollow(),options = list(
		 	pageLength = 5,
		 	lengthMenu = c(5, 10, 20, 25)
		 	))
		})

		output$mutuals <- DT::renderDataTable({
		 	DT::datatable(getMutuals(),options = list(
		 	pageLength = 5,
		 	lengthMenu = c(5, 10, 20, 25)
		 	))
		})

		output$top.words.user <- renderPlot({
			getWordcloudUser()
		})

		output$tweets.per.week <- renderPlot({
			getTweetsPerWeek()
		})

		output$tweets.per.hour <- renderPlot({
			getTweetsPerHour()
		})

		output$top.correlated.word <- renderPlot({
			getTopCorrelatedWords(tweets.tibble)
		})

		output$top.correlated.liked.word <- renderPlot({
			getTopCorrelatedWords(getLikedTweets())
		})

		output$no.active.following <- DT::renderDataTable({
		 	DT::datatable(getMutuals(),options = list(
		 	pageLength = 5,
		 	lengthMenu = c(5, 10, 20, 25)
		 	))
		})
	})
}