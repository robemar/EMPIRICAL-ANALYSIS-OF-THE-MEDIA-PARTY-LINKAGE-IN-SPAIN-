

rm(list = ls())
setwd("D:/Master Denmark/3rd Semester/Political Data Science/Final Exam/True Final Exam/1. Downloading")

library(rvest)
library(data.table)
library(stringr)
library(RCurl)


# create main archive
if(!dir.exists("ElPaisRajoy")) dir.create("ElPaisRajoy")

# create sub-archive for search results
archive.folder <- "./ElMundoSanchez/fulltext/"
if(!dir.exists(archive.folder)) dir.create(archive.folder)

load("D:/Master Denmark/3rd Semester/Political Data Science/Final Exam/True Final Exam/1. Downloading/ElMundoSanchezFinal.RData")

link.list <- finaldata$fulltextlink


##Download all texts


#Collecting full text from articles (using gsub to clean the text from non-wanted objects, such as links to twitter, facebook or even to other articles)
full_text <- rep("",nrow(finaldata))
for(i in 1:nrow(finaldata)){
  try({
    text <- read_html(as.character(finaldata$fulltextlink[i])) %>%
      html_nodes("p") %>% 
      html_text(trim = T) 
    full_text[i] = paste(text, collapse = " ")
  })
}

full_text <- gsub('Send  Facebook(.*)dw\\.com/p/\\w+','',full_text)

#Creating new row with full text
finaldata$full_text <- full_text

#put out the texts without content
finaldata <- finaldata %>% distinct(full_text, .keep_all = TRUE)

#In all el mundo articles there was the concern that the website self-scrolls towards the next article inside the same link, but at the end our code works perfectly avoiding it


View(full_text)

save(finaldata, file = "ElMundoSanchezText.RData")
