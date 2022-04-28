install.packages('pdftools')
library(pdftools)
devtools::install_github("brooke-watson/BRRR")
library(BRRR)
install.packages('tidytext')
library(tidytext)
install.packages('tidyverse')
library(tidyverse)
install.packages('corpus')
library(corpus)
install.packages('wordcloud')
library(wordcloud)
install.packages('widyr')
library(widyr)
install.packages('ggraph')
library(ggraph)
install.packages('igraph')
library(igraph)
skrrrahh(0)
install.packages('textdata')
library(textdata)
install.packages('tm')
library(tm)

# readin in le data
my.data <- data.frame(pdf_text("https://github.com/matthew-danna/justice-research/raw/main/Aggregated%20Tweets.pdf"))
# adding sequence column
my.data$ID <- seq.int(nrow(my.data))
# column names
names(my.data) <- c("text", "page")
#text 2 characters
my.data$text <- as.character(my.data$text)
# tokenize text
my.words <- my.data %>% unnest_tokens(word, text)
my.words$word.ID <- seq.int(nrow(my.words))
# stem words
my.stems <- text_tokens(my.words$word, stemmer = "en")
my.stems <- as.data.frame(unlist(my.stems))
my.stems$word.ID <- seq.int(nrow(my.stems))
names(my.stems) <- c("stem.word", "word.ID")
# 8. join stemmed and original
my.stem.words <- my.words %>% inner_join(my.stems)
names(my.stem.words) <- c("page", "original.word", "word.ID", "word")

# create stopwords
stop_words$word.ID <- seq.int(nrow(stop_words))
stops.stemmed <- text_tokens(stop_words$word, stemmer = "en")
stops.stemmed <- as.data.frame(unlist(stops.stemmed))
stops.stemmed$word.ID <- seq.int(nrow(stops.stemmed))
stops.stemmed$lexicon <- "Stemmed Stopword"
names(stops.stemmed) <- c("word", "word.ID", "lexicon")
all.stops <- rbind(stop_words, stops.stemmed)
# removes the stopwords
my.clean.words.no.stops <- my.stem.words %>% anti_join(stop_words, by = "word")
# remove the numbers
tmp.clean.words <- my.clean.words.no.stops[grep("^[[:digit:]]", my.clean.words.no.stops$word), ]
my.clean.words <- my.clean.words.no.stops %>% anti_join(tmp.clean.words, by = "word")

# sentiment analysis
my.afinn <- my.clean.words %>% inner_join(get_sentiments("afinn"))
my.bing <- my.clean.words %>% inner_join(get_sentiments("bing"))
my.nrc <- my.clean.words %>% inner_join(get_sentiments("nrc"))
my.sentiment <- my.bing # replace with the sentiment of your choice

# word counts
counts.my.clean.words <- my.clean.words %>% count(word, sort = TRUE)
counts.my.sentiment <- my.sentiment %>% count(word, sort = TRUE)
counts.sentiments <- my.sentiment %>% count(sentiment, sort = TRUE)
counts.words.sentiments <- my.sentiment %>% count(word, sentiment, sort = TRUE)
top.word.sentiments <- subset(counts.words.sentiments, n >= quantile(counts.words.sentiments$n, 0.75))

#plz god do the thing
write.csv(counts.words.sentiments,"C:\\Users\\rubyc\\downloads\\tweetsentiments1.csv", row.names = FALSE)


# bar graph
ggplot(top.word.sentiments, aes(x=word, y=n, fill=n)) + geom_bar(stat = "identity") + coord_flip()

# wordclouds
wordcloud(my.clean.words$original.word)

# word pairs
wordpairs.my.words <- my.clean.words %>% pairwise_count(word, page, sort = TRUE)
wordpairs.top <- subset(wordpairs.my.words, n >= quantile(wordpairs.my.words$n, 0.99)) # this examines the top 99.999 percentile of pairs

# network charts
set.seed(611)
pairs.plot <- wordpairs.my.words %>%
  filter(n >= 25) %>% # update as you see fit
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "steelblue") +
  ggtitle("Word Pairs!") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
pairs.plot


