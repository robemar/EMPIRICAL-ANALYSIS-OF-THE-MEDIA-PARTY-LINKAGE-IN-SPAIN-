
rm(list = ls())
setwd("D:/Master Denmark/3rd Semester/Political Data Science/Final Exam/True Final Exam/1. Downloading")

#install.packages("topicmodels")
#install.packages("ggplot2")
#install.packages("R.matlab")

library(quanteda)
library(stringr)
library(topicmodels) 
library(ggplot2)
library(R.matlab)

# load article data
load("D:/Master Denmark/3rd Semester/Political Data Science/Final Exam/True Final Exam/1. Downloading/Rajoy(ElPais)clean.RData")


# Drop articles missing text (don't work?)
article.data <- textclean[!is.na(textclean$full_text), ]


# Create a date
article.data$date <- as.Date(article.data$file.date, "%Y-%m-%d")

# Manually drop articles with missing text
newdata <- article.data[ -c(24),]



# 'tokenize' our full_text
quanteda_options("language_stemmer" = "spanish")
texts <- gsub(":", " ", newdata$full_text, fixed = T)

texts <- tokens(texts, what = "word",
                remove_numbers = T,
                remove_punct = T,
                remove_symbols = T,
                remove_separators = T,
                remove_hyphens = T,
                remove_url = T,
                verbose = T)

texts <- tokens_tolower(texts)
texts <- tokens_remove(texts, stopwords("spanish"))
texts <- tokens_wordstem(texts)
texts <- tokens_remove(texts, stopwords("spanish"))


# Get dfm from tokens
txt.mat <- dfm(texts)


# Check recurrent features in dfm
topfeatures(txt.mat)

# Keep words appearing in multiple documents
txt.mat <- dfm_trim(txt.mat, min_termfreq = 4)

# Filter words with one letter
txt.mat <- txt.mat[, str_length(colnames(txt.mat)) > 2]

# Filter out some undesired terms and signs on the html 
txt.mat <- txt.mat[, !grepl("[[:digit:]]+px", colnames(txt.mat))]
txt.mat <- txt.mat[, !grepl(".", colnames(txt.mat), fixed = T)]
txt.mat <- txt.mat[, !grepl("_", colnames(txt.mat), fixed = T)]
txt.mat <- txt.mat[, !grepl("@", colnames(txt.mat), fixed = T)]
txt.mat <- txt.mat[, !grepl("#", colnames(txt.mat), fixed = T)]


# Remove words
to.remove <- c("http", "href", "www", "el", "la", "con", "https")


txt.mat <- dfm_remove(txt.mat, to.remove)


# Make a "tf_idf-weighted" dfm
ti <- dfm_tfidf(txt.mat)

# Select from main dfm using its top features
txt.mat <- dfm_keep(txt.mat, names(topfeatures(ti, n = 1000)))


# Transform the dfm to topic model "dtm" using quanteda
dtm <- convert(txt.mat, to = "topicmodels")

# Execute LDA using 4 topics
lda <- LDA(dtm, k = 4)

# Check content of topics
terms(lda, 25)


# Sample Test Data with 15 topics
set.seed(61218)
select <- sample(1:nrow(dtm), size = 100)
test <- dtm[select, ]
train <- dtm[!(1:nrow(dtm) %in% select), ]

n.tops <- 2:15
metrics <- data.frame(topics = n.tops,
                      perplexity = NA)

for(i in n.tops) { 
  print(i)
  est <- LDA(train, k = i)
  metrics[(i - 1), "perplexity"] <- perplexity(est, newdata = test)
}

save(metrics, file = "ElPaisRajoy_perplexity.RData")
load("ElPaisRajoy_perplexity.RData")


qplot(data = metrics, x = topics, y = perplexity, geom = "line",
      xlab = "Number of topics",
      ylab = "Perplexity on test data for El Pais") + theme_bw()


# Execute lda with 12 topics (optimal)
lda <- LDA(dtm, k = 12)
save(lda, file = "ElPaisRajoyOptTop.RData")

# Output
terms(lda, 10)


# Introduce topics into newdata
newdata$topic <- topics(lda)

# simple frequency
qplot(data = newdata, x = topic, geom = "bar")

# Create labels in consonance with optimal topics
newdata$topic_lab <- factor(newdata$topic,
                        levels = 1:12,
                        labels = c("Jailing of Catalan Politicians", "Territorial Conflict", "Corruption", "Ministry of Finance", "Spanish Government", "Community of Madrid", 
                                   "European Economy", "European Politics", "Catalan Government", "Spanish Politics", "Spanish Economy",  
                                   "Budget Vote"))

# Frequency plot with specific topcis
qplot(data = newdata, x = topic_lab, 
      geom = "bar", xlab = "", 
      ylab = "Frequency", main = "Rajoy's Executive topic frequency ElPais") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

terms (lda, 40)

save(newdata, file="RajoyElPaistopics.RData")
load("RajoyElPaistopics.RData")