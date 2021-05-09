library(foreign)
library(psych) 
library(NLP) 
library(tm) 
library(ldatuning)
library(slam)
library(lsa)
library(NLP)
library(tm)
library ('SnowballC')

library("wordcloud")
library("RColorBrewer")
library("topicmodels")



text_film <- read.csv(file.choose(), header = TRUE, encoding="UTF-8", stringsAsFactors=FALSE)  # file.choose()
text_film<-na.omit(text_film)

text_film$Type <- as.factor(text_film$Type)  #change it into factors
class(text_film$Type) #wanna to know what's the type of the "type"
#the type have been changed into the factors
levels(text_film$Type) # what the each factors means?
# 1:"Best Art Direction"   2:"Best Film"        3:"Best Visual Effects"

text_BADR <- text_film[which(text_film$Type=='Best Film'), ]

M_text <- subset(text_BADR)
M_text_1 <- VectorSource(M_text$Comment_Content)

M_text_2 <- SimpleCorpus(M_text_1)

MT <- tm_map(M_text_2, content_transformer(tolower))
MT <- tm_map(MT, removeNumbers)
MT <- tm_map(MT, removeWords, stopwords("english"))
MT <- tm_map(MT, removeWords, c("YOUR OWN STOP WORDS"))
MT <- tm_map(MT, removePunctuation)
MT <- tm_map(MT, stripWhitespace)

inspect(MT)


#--------------------

MT_dtm <- DocumentTermMatrix(MT, control = list(removePunctuation = TRUE, stopwords=TRUE)) 
MT_dtm

inspect(MT_dtm) 


# TF
term_freq_MT <- colSums(as.matrix(MT_dtm)) 


# TF-IDF
MT_dtm_tfidf <- DocumentTermMatrix(MT, control = list(weighting = weightTfIdf)) # DTM is for TF-IDF calculation 
MT_dtm_tfidf2 = removeSparseTerms(MT_dtm_tfidf, 0.99)
print(MT_dtm_tfidf2) 
write.csv(as.data.frame(sort(colSums(as.matrix(MT_dtm_tfidf2)), decreasing=TRUE)), file="tweets_lib_dtm_tfidf.csv")

#  topic modeling with LDA-----
#install.packages("topicmodels")
library(topicmodels)

rowTotals <- apply(MT_dtm , 1, sum)  #Find the sum of words in each Document
MT_dtm.new   <- MT_dtm[rowTotals> 0, ] #remove all docs without words



# k4 - 4 topics, 10 term 
MT_4topics <- LDA(MT_dtm.new , k = 6, method = "Gibbs", control = list(iter=2000, seed = 2000)) # find k topics
MT_4topics_10words <- terms(MT_4topics, 10) # get top 10 words of every topic
(MT_4topics_10words <- apply(MT_4topics_10words, MARGIN = 2, paste, collapse = ", ")) 




#------


library(ldatuning)
library(slam)

result <- FindTopicsNumber(
  MT_dtm.new,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
) 

FindTopicsNumber_plot(result)






##----------words cloud
dtm <- TermDocumentMatrix(MT)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq=v)
head(d,30)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per=0.0,
          color=brewer.pal(8,"Dark2"))    


##----sentiment analysis
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(foreign)



text_film <- read.csv(file.choose(), header = TRUE, encoding="UTF-8", stringsAsFactors=FALSE)  # file.choose()
text_film<-na.omit(text_film)
text_BADR <- text_film[which(text_film$Type=='Best Visual Effects'), ]
write.csv(text_BADR, file="Best Visual Effects.csv")



script<-read.csv(file.choose(), stringsAsFactors=FALSE)
tidy_script <- script %>%unnest_tokens(word, text)



tidy_script %>%
  inner_join(get_sentiments("nrc")) %>%
  arrange(line) %>%
  head(10)


tidy_script %>%
  inner_join(get_sentiments("nrc")) %>%
  count(line, sentiment) %>%
  mutate(index = line %/% 5) %>%  
  arrange(index) %>%
  head(10)

tidy_script %>%
  inner_join(get_sentiments("nrc")) %>%
  count(line, sentiment) %>%
  mutate(index = line %/% 5) %>%
  ggplot(aes(x=index, y=n, color=sentiment)) %>%
  + geom_col()


tidy_script %>%
  inner_join(get_sentiments("nrc")) %>%
  count(line, sentiment) %>%
  mutate(index = line %/% 5) %>%
  ggplot(aes(x=index, y=n, color=sentiment)) %>%
  + geom_col() %>%
  + facet_wrap(~sentiment, ncol=3)




