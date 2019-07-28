setwd("C:/Users/fqasd/git/repository/Data-Science-Programming")

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

library(tm)
library(qdap)


data <-  read_csv("Week2/aljazeera2.csv")
data <- na.omit(data)



#data_2019 <- unite(data_2019, "merged_text", c("title", "summary", "content"), sep=' ')


merged_data <- data %>% 
  mutate(year=str_sub(date, -4, -1)) %>%
  select(-date) %>%
  unite("merged_text", c("title", "summary", "content")) %>%
  group_by(year) %>%
  summarise(all_text=paste0(merged_text, collapse = " "))

# better way?...(create folder)
cat(merged_data$all_text[1], file = "Week2/aljazeera_data/text_2019.txt")
cat(merged_data$all_text[2], file = "Week2/aljazeera_data/text_2018.txt")
cat(merged_data$all_text[3], file = "Week2/aljazeera_data/text_2017.txt")
cat(merged_data$all_text[4], file = "Week2/aljazeera_data/text_2016.txt")
cat(merged_data$all_text[5], file = "Week2/aljazeera_data/text_2015.txt")
cat(merged_data$all_text[6], file = "Week2/aljazeera_data/text_2014.txt")


# txt <- system.file("texts", "txt", package = 'tm')
aljazeera_corpus <- VCorpus(DirSource("./Week2/aljazeera_data", encoding = "UTF-8")) %>%
  # tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(content_transformer(bracketX)) %>%
  tm_map(content_transformer(function(x) gsub("http[[:alnum:]]*", " ", x))) %>% #remove url
  tm_map(content_transformer(function(x) gsub("[[:punct:]]", " ", x))) %>% #remove punctuation
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, c(stopwords("en"), "taiwan",))

# stem_corpus <- tm_map(aljazeera_corpus, stemDocument)
# aljazeera_corpus <- tm_map(stem_corpus, stemCompletion, dictionary = aljazeera_dict)



aljazeera_tdm <- TermDocumentMatrix(stem_corpus)
print(aljazeera_tdm)

aljazeera_m <- as.matrix(aljazeera_tdm)
dim(aljazeera_m)
View(aljazeera_m)

findFreqTerms(aljazeera_tdm, 500)
findAssocs(aljazeera_tdm, "china", 0.9)

#TF-IDF

