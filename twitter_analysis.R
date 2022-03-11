library(tidytext)
library(tidyverse)
library(lubridate)
library(textdata)

library(RColorBrewer)
library(wordcloud)
library(wordcloud2)

library(topicmodels)
library(tm)

#import twitter data
tweet<-read_csv("12_10_2021.csv")

tweet<-
  tweet%>%
  mutate(
    timestamp=ymd_hms(created_at),
    day_of_week=wday(timestamp),
    id=as_factor(row_number())
  )

#wday(ymd(080101), label = TRUE, abbr = FALSE)
#wday(ymd(080101), label = TRUE, abbr = TRUE)

#explore wday() function

remove_reg <- "&amp;|&lt;|&gt;|\\d+\\w*\\d*|#\\w+|[^\x01-\x7F]|[[:punct:]]|https\\S*"
# &amp = @
# &lt;= <
# &gt; >


#removing retweets characters
tidy_tweets <- tweet %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) 


#unnesting tweets 
tidy_tweets<-tidy_tweets%>%
  unnest_tokens(word, text, token = "tweets") 

# Remove mentions, urls, emojis, numbers, punctuations, etc.
tidy_tweets<-tidy_tweets%>%
  filter(
    str_detect(word, "[a-z]")#keep only character-words
    )%>%
  anti_join((stop_words))

head(tidy_tweets$word)
#!!!!
#if you want to use stem of the words (e.g. egg instead of eggs) you can use SnowballC::wordStem() option

#install.packages("SnowballC")
#tidy_tweets_stemmed<-tidy_tweets%>% mutate(word=SnowballC::wordStem(word))
#head(tidy_tweets_stemmed$word)

#sentiment
bing<-get_sentiments("bing")
nrc<-get_sentiments("nrc")

selected_tweets_tidy<-tidy_tweets%>%select(timestamp, day_of_week, location, word, user_id)

selected_tweets_tidy<-selected_tweets_tidy%>%
  inner_join(bing)%>%
  rename(sentiment_bing=sentiment)%>%
inner_join(nrc)%>%
  rename(sentiment_nrc=sentiment)


selected_tweets_tidy%>%
  count(sentiment_nrc, sort=TRUE)

selected_tweets_tidy%>%
  count(sentiment_bing, sort=TRUE)

selected_tweets_tidy%>%
  group_by(user_id)%>%
  count(sentiment_nrc, sort=TRUE)

#wordcloud

tidy_tweets%>%count(word) %>%
  with(wordcloud(word, n, max.words = 100))

#more wordclouds with wordcloud2 package! - it is awesome!
# https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html
#install.packages("wordcloud2")
#library(wordcloud2)

tidy_tweets_wordcloud<-tidy_tweets%>%count(word) %>%
  select(word, n)%>%
  filter(!word=="coffee") #too many occurencies!!!!
  
  
wordcloud2(tidy_tweets_wordcloud,color = "random-light", backgroundColor = "black")

letterCloud(tidy_tweets_wordcloud, word = "COFFEE", size = 2)

#---------------------
#topic modeling

#create a document term frequency based on how often words are
tweets_dtm<-tidy_tweets%>%
  count(id, word)%>%
  cast_dtm( #converts our data to a special object for R = document term frequency matrix
    document=id,
    term=word,
    value=n,
    weighting=tm::weightTf
    )

tweets_dtm

#to speed up processing let's remove those words that are VERY rare
#install.packages("tm")
#sparse=rare
tweets_dtm_trim<-tm::removeSparseTerms(tweets_dtm, sparse=.99)
rowTotals <- apply(tweets_dtm_trim, 1, sum) #Find the sum of words in each Document
tweets_dtm_trim <- tweets_dtm_trim[rowTotals> 0, ] 

tweets_dtm_trim

#LDA for 5 topics
tweets_lda<-LDA(tweets_dtm_trim, k=5, control=list(seed=1234))

tweet_topics<-tidy(tweets_lda, matrix="beta")

#let's look at them with 10 top words for each topic
tweet_top_terms <- tweet_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

tweet_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


#OPTIONAL
#if you want a "scientific" way to see how many topics you need to use
#you can use ldatuning package

install.packages("ldatuning") #select no for compilation if asked
library(ldatuning)

#it will take TIME!
topic_n <- FindTopicsNumber(dtm = tweets_dtm_trim , topics = seq(from = 2, to = 30, by = 1), 
                            metrics = "Griffiths2004", 
                            method = "Gibbs",
                            control = list(seed = 123),
                            mc.cores = 4L,
                            verbose = TRUE)

FindTopicsNumber_plot(topic_n)
#it works the same way as "elbow method" for cluster analysis from bco6008
#if you have not done bco6008- just skip this part! 
#once are happy with the number of topics - 
#you can rerun the LDA with the number of topics you identified from ldatuning
