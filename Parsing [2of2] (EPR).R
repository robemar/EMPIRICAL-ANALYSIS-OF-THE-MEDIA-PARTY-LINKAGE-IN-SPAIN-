

rm(list = ls())
setwd("D:/Master Denmark/3rd Semester/Political Data Science/Final Exam/True Final Exam/1. Downloading")

library(rvest)
library(data.table)
library(stringr)
library(RCurl)


# create main archive
if(!dir.exists("ElPaisRajoy")) dir.create("ElPaisRajoy")

# create sub-archive for search results
archive.folder <- "./ElPaisRajoy/fulltext/"
if(!dir.exists(archive.folder)) dir.create(archive.folder)

load("D:/Master Denmark/3rd Semester/Political Data Science/Final Exam/True Final Exam/1. Downloading/ElPaisRajoyFinal.RData")

link.list <- finaldata$fulltextlink


##

#Organize URLs
finaldata$fulltextlink <- paste0("https://www.elpais.com",finaldata$fulltextlink)

#Collecting full text from articles (using gsub to clean the text from non-wanted objects, such as links to twitter, facebook or even to other articles)
full_text <- rep("",nrow(finaldata))
for(i in 1:nrow(finaldata)){
  try({
    text <- read_html(as.character(finaldata$fulltextlink[i])) %>%
      html_nodes(".articulo-cuerpo p") %>% 
      html_text(trim = T) 
    full_text[i] = paste(text, collapse = " ")
  })
}

full_text <- gsub('Send  Facebook(.*)dw\\.com/p/\\w+','',full_text)


#when you run this code in el pais articles a lot of errors will pop-up. This wasn't intentional on the beginning but after understanding -
#- how it works is an added functionality extremely useful to filter relevant texts, as in the El Pais website there are "collaborations" with -
#- other new sites and each error in this code represents an article that isn't 100% a article made from the main "El País" newspaper or -
#- have an added and undesired functionallity. (e.g. articles from the catalan version of the "El Pais")


#Creating new row with full text
finaldata$full_text <- full_text

#there are multiple errors once you run the analysis, this errors are the articles that are from other newspapers or just aren't in spanish, so doesn't affect the results.


View(full_text)

save(finaldata, file = "ElPaisRajoyText.RData")
