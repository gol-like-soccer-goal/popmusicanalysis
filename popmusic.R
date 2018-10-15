###SETUP###
require("tidyverse")
require("tidytext")

#set working directory
#setwd("")

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

###SENTIMENT ANALYSIS###
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

#pre-process and filter data for topic analyses
lyrics.filter <- lyrics.tidy %>%
	anti_join(stop_words) %>%
	distinct() %>%
	filter(nchar(word) >3)

lyrics.count <- lyrics.filter %>%
	
