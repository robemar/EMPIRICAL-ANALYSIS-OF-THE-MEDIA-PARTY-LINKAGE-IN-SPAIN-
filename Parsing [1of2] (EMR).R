

rm(list = ls())
setwd("D:/Master Denmark/3rd Semester/Political Data Science/Final Exam/True Final Exam/1. Downloading")

library(rvest)
library(data.table)
library(stringr)

#define archive folder
archive.folder <- "./ElMundoRajoy/"

#list all the downloaded files
file.list <- list.files(archive.folder)

#empty list of dataframes
dfs <- list()


#loop over the files present on our archive folder
for (file in file.list) {
  
  #get date and time of the day for each file
  file.date <- str_extract(file, "[[:digit:]]*-[[:digit:]]*-[[:digit:]]*[[:alpha:]]")
  
  #print status
  print(paste("Working on:", file.date))
  
  #load html source code from archive
  load(paste0(archive.folder, file))
  
  #save "xml_document" 
  p <- read_html(to.save)
  
  #get titles
  fulltexttitle <- p %>% html_nodes(".container header a") %>% html_text(trim = TRUE)
  
  find.hits <- grepl("Rajoy|Mariano|Moncloa|gobierno|ejecutivo", fulltexttitle, ignore.case = T)
  
  fulltexttitle <- fulltexttitle[find.hits]
  
  #get link for the full article
  fulltextlink <- p %>% html_nodes(".container header a") %>% html_attr("href")
  fulltextlink <- fulltextlink[find.hits]
  
  if (length(fulltexttitle) == 0) next
  
  #create number for each page
  iteration <- (1 + length(dfs))
  
  #creating a dataframe
  dfs[[iteration]] <- data.frame(
    fulltexttitle=fulltexttitle, 
    fulltextlink=fulltextlink,
    iteration=iteration, 
    file.date=file.date, 
    stringsAsFactors = F)
  
}


dataframe <- as.data.frame(rbindlist(dfs))

View(dataframe)

# save dataframe
save(dataframe, file = "ElMundoRajoy.RData")


#install quanteda
#install.packages("quanteda")
#install.packages("tidyverse")
library(quanteda)
library(tidyverse)

#break, no need to run it every time, just load the dataframe

load("D:/Master Denmark/3rd semester/Political Data Science/Final Exam/True Final Exam/1. Downloading/ElMundoRajoy.RData")
data.frame(dataframe$fulltexttitle)

library(dplyr)
dataclean <- dataframe %>% distinct(fulltexttitle, .keep_all = TRUE)

View(dataclean)

#keyword analysis (1st round)
kwic(dataclean$fulltexttitle, "Rajoy", window = 3, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "Mariano", window = 3, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "Moncloa", window = 3, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "moncloa", window = 3, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "Gobierno", window = 3, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "gobierno", window = 2, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "ejecutivo", window = 3, valuetype = "fixed")

#Unconclusive validity of articles using only kwic, manually checked
#(212, 247, 174)

#Deleting unrelated obvservations
finaldata <- dataclean[ -c(15, 23, 27, 58, 51, 55, 73, 88, 95, 105, 115, 123, 128, 132, 130, 134, 136, 155, 163, 166, 175, 177, 210, 196, 218, 244, 259, 263, 268, 272, 275, 282, 303, 311, 317, 11, 13, 36), ]

# save dataframe
save(finaldata, file = "ElMundoRajoyFinal.RData")


