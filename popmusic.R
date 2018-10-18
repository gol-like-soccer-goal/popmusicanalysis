###SETUP###
require("tidyverse")
require("tidytext")
require("wordcloud")
require("reshape2")
require("topicmodels")

#set working directory
setwd("/Users/gai7612/OneDrive/Documents/Data Science Project/pop music analysis")

#import data
lyrics <- read_csv("billboard_lyrics_1964-2015.csv")

#add decade column
lyrics <- lyrics %>%
	mutate(decade =
				ifelse(lyrics$Year %in% 1965:1969, "1960's",
				ifelse(lyrics$Year %in% 1970:1979, "1970's",
				ifelse(lyrics$Year %in% 1980:1999, "1980's",
				ifelse(lyrics$Year %in% 1990:1999, "1990's",
				ifelse(lyrics$Year %in% 2000:2009, "2000's",
				ifelse(lyrics$Year %in% 2010:2015, "2010's",
				"NA"
				)))))))
				
#clean and tokenize data
lyrics.tidy <- lyrics %>%
	na.omit() %>%
	unnest_tokens(word, Lyrics)

#word cloud of top 100 words
lyrics.tidy %>%
	anti_join(stop_words) %>%
	count(word) %>%
	with(wordcloud(word, n, max.words = 200, color = "hotpink"))

###SENTIMENT ANALYSIS###
#sentiments using Bing
lyrics.sentiment <- lyrics.tidy %>%
	inner_join(get_sentiments("bing")) %>%
	count(Song, Year, decade, sentiment) %>%
	spread(sentiment, n, fill = 0) %>%
	mutate(sentiment = positive - negative) #%>%
	#filter(Year == 2014 | Year == 2013 | Year == 2012 | Year == 2011 | Year == 2010) 

#Visualize sentiment by song separated by decade
ggplot(lyrics.sentiment, aes(Song, sentiment, fill = decade)) + geom_col(show.legend = FALSE) + facet_wrap(~decade, ncol = 2, scales ="free_x") + theme(axis.text.x = element_blank()) 

#Visualize mean sentiment score by decade
ggplot(lyrics.sentiment, aes(decade, sentiment, fill = decade)) + stat_summary(fun.y = "mean", geom = "bar")

#Count and visualize how many Bing sentiment words are in lyrics data
bing.word.counts <- lyrics.tidy %>%
	inner_join(get_sentiments("bing")) %>%
	count(word, sentiment, sort = TRUE) %>%
	ungroup() %>%
	group_by(sentiment)  %>%
	top_n(30)  %>%
	ungroup() %>%
	mutate(word = reorder(word, n)) %>%
	ggplot(aes(word, n, fill = sentiment)) + 
	geom_col(show.legend = FALSE) +
	facet_wrap(~sentiment, scales="free_y") +
	labs(y = "contribution to sentiment", x = NULL) + coord_flip()

bing.word.counts

#Word cloud of positive and negative words
lyrics.tidy %>%
	inner_join(get_sentiments("bing")) %>%
	count(word, sentiment, sort = TRUE) %>%
	acast(word ~ sentiment, value.var = "n", fill = 0 ) %>%
	comparison.cloud(colors = c("purple1" , "royalblue"))

#sentiments using NRC
lyrics.sentiment.NRC <- lyrics.tidy %>%
	inner_join(get_sentiments("nrc")) %>%
	count(Song, Year, decade, sentiment) %>%
	spread(sentiment, n, fill = 0) %>%
	mutate(sentiment = positive - negative)

#Visualize NRC sentiment by song separated by decade
ggplot(lyrics.sentiment.NRC, aes(Song, sentiment, fill = decade)) + geom_col(show.legend = FALSE) + facet_wrap(~decade, ncol = 2, scales ="free_x") + theme(axis.text.x = element_blank()) 

#Visualize NRC mean sentiment score by decade
ggplot(lyrics.sentiment.NRC, aes(decade, sentiment, fill = decade)) + stat_summary(fun.y = "mean", geom = "bar")

#Count and visualize how many NRC sentiment words are in lyrics data
nrc.word.counts <- lyrics.tidy %>%
	inner_join(get_sentiments("nrc")) %>%
	count(word, sentiment, sort = TRUE) %>%
	ungroup() %>%
	group_by(sentiment)  %>%
	top_n(10)  %>%
	ungroup() %>%
	mutate(word = reorder(word, n)) %>%
	ggplot(aes(word, n, fill = sentiment)) + 
	geom_col(show.legend = FALSE) +
	facet_wrap(~sentiment, scales="free_y") +
	labs(y = "contribution to sentiment", x = NULL) + coord_flip()

nrc.word.counts

#NRC sentiments by categories by decade
nrc.sub <- lyrics.tidy %>%
	inner_join(get_sentiments("nrc")) %>%
	filter(!sentiment %in% c("positive" , "negative"))

decade.sent.nrc <- nrc.sub %>%
	group_by(decade, sentiment) %>%
	count(decade, sentiment) %>%
	select(decade, sentiment, sentiment.decade.count = n)

total.sent.nrc <- nrc.sub %>%
	count(decade) %>%
	select(decade, decade.total = n)

nrc.decade.plot <- decade.sent.nrc %>%
	mutate_if(is.factor, as.character) %>%
	inner_join(total.sent.nrc, by = "decade") %>%
	mutate(percent = sentiment.decade.count / decade.total * 100) %>%
	ungroup() %>%
	arrange(decade, percent) %>%
	#mutate(ordered = paste(decade, percent) %>%
	#forcats::fct_inorder())
	mutate(row = row_number())

nrc.decade.plot  %>%
	ggplot(aes(row, percent, fill=factor(decade))) + geom_col(show.legend = FALSE) + facet_wrap(~decade, scales ="free_y", drop = TRUE) + scale_x_continuous(breaks = nrc.decade.plot$row, labels = nrc.decade.plot$sentiment ) + coord_flip()  

###NGRAMS### 

lyrics.trigrams <- lyrics %>%
	mutate_if(is.factor, as.character) %>%
	unnest_tokens(bigram, Lyrics, token ="ngrams" , n = 3) %>%
	separate(bigram, c("word1", "word2", "word3"), sep = " ") %>%
	filter(!word1 %in% stop_words$word) %>%
	filter(!word2 %in% stop_words$word) %>%
	filter(!word3 %in% stop_words$word) %>%
	filter(word1 != word2) %>%
	filter(word2 != word3) %>%
	unite(bigram, word1, word2, word3, sep= " ") %>%
	inner_join(lyrics) %>%
	count(bigram, decade, sort = TRUE) %>%
	group_by(decade) %>%
	top_n(10, n) %>%
	ungroup() %>%
	arrange(decade, n) %>%
	mutate(row = row_number())

lyrics.trigrams %>%
	ggplot(aes(row, n, fill = factor(decade))) + geom_col(show.legend= FALSE) + facet_wrap(~decade, scales = "free_y") + scale_x_continuous(breaks = lyrics.trigrams$row, labels = lyrics.trigrams$bigram) + coord_flip()
	
	
###TOPIC ANALYSIS###	

#pre-process and filter data for topic analyses
lyrics.filter <- lyrics.tidy %>%
	anti_join(stop_words) %>%
	distinct() %>%
	filter(nchar(word) >3)

#Create document term matrix
lyrics.dtm <- lyrics.filter %>%
	count(Song, word, sort = TRUE) %>%
	cast_dtm(Song, word, n)

#LDA
lyrics.lda <- LDA(lyrics.dtm, k = 15, control = list(seed = 1234))
lyrics.tidy.topics <- tidy(lyrics.lda, matrix = "beta")

lyrics.top.terms <- lyrics.tidy.topics %>%
	mutate_if(is.factor, as.character) %>%
	group_by(topic) %>%
	top_n(10, beta) %>%
	ungroup() %>%
	arrange(beta) %>%
	mutate(ordered = paste(topic, beta) %>%
	forcats::fct_inorder())

lyrics.top.terms %>%
	ggplot(aes(x=ordered, y=beta, fill = factor(topic))) +
	geom_col(show.legend = FALSE) +
	facet_wrap(~ topic, scales = "free", drop = TRUE) +
	scale_x_discrete(labels = setNames(lyrics.top.terms$term, lyrics.top.terms$ordered)) +
	coord_flip()


	

	
	