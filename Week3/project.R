library(readr)
library(dplyr)
library(tidyr)
library(stringr)

library(tm)
library(qdap)
library(ggplot2)

data <- scan("Week3/data/cnn_2019.txt", what = character(), encoding = "UTF-8")
data_source <- VectorSource(data)

data_corpus <- Corpus(data_source) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(content_transformer(function(x) gsub("[[:punct:]]", " ", x))) %>% #remove punctuation
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, c(stopwords("en"), "said", "says", "also", "new", "will", "year", "two", "can", "may", "s", ""))


# stem_corpus <- tm_map(data_corpus, stemDocument)
# aljazeera_corpus <- tm_map(stem_corpus, stemCompletion, dictionary = aljazeera_dict)

cnn_tdm <- TermDocumentMatrix(data_corpus)

cnn_m <- as.matrix(cnn_tdm)
dim(cnn_m)
cnn_m[1:10, 1:20]

term_frequency <- rowSums(cnn_m)
term_frequency <- sort(term_frequency, decreasing = T)

View(term_frequency[1:15])

#graph with ggplot
barplot(term_frequency[1:10], 
        xlab = "Top 10 Words", 
        ylab = "Frequency",
        col = grey.colors(10), las = 2)

#wordcloud
cnn_freqs <- data.frame(term = names(term_frequency), 
                        num = term_frequency)
brown_bg <- brewer.pal(10, "BrBG")
brown_bg <- brown_bg[-(5:6)]
wordcloud::wordcloud(cnn_freqs$term, cnn_freqs$num,
                     max.words = 100, colors = brown_bg)
#word association
president_association <- findAssocs(cnn_tdm, "president", 0.1)

china_association <- findAssocs(cnn_tdm, "china", 0.1)

government_association <- findAssocs(cnn_tdm, "government", 0.1)

hk_association <- findAssocs(cnn_tdm, "hong", 0.1)

#using tfidf
tfidf_tdm <- TermDocumentMatrix(data_corpus, 
                                control = list(weighting = weightTfIdf))
tfidf_m <- as.matrix(tfidf_tdm)
dim(tfidf_m)
tfidf_m[50:60, 1:20]

term_frequency <- rowSums(tfidf_m)
term_frequency <- sort(term_frequency, decreasing = T)

term_frequency[1:30]
