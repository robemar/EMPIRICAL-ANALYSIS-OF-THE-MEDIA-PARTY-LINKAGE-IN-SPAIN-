

rm(list = ls())
setwd("D:/Master Denmark/3rd Semester/Political Data Science/Final Exam/True Final Exam/1. Downloading")

library(rvest)
library(data.table)
library(stringr)

#define archive folder
archive.folder <- "./ElMundoSanchez/"

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
  
  #save 'xml_document' 
  p <- read_html(to.save)
  
  #get titles
  fulltexttitle <- p %>% html_nodes(".container header a") %>% html_text(trim = TRUE)
  
  find.hits <- grepl("Sanchez|Pedro|Moncloa|gobierno|ejecutivo", fulltexttitle, ignore.case = T)
  
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
save(dataframe, file = "ElMundoSanchez.RData")


#install quanteda
#install.packages("quanteda")
#install.packages("tidyverse")
library(quanteda)
library(tidyverse)

#break, no need to run it every time, just load the dataframe

load("D:/Master Denmark/3rd semester/Political Data Science/Final Exam/True Final Exam/1. Downloading/ElMundoSanchez.RData")
data.frame(dataframe$fulltexttitle)

library(dplyr)
dataclean <- dataframe %>% distinct(fulltexttitle, .keep_all = TRUE)

View(dataclean)

#keyword analysis (1st roudn)
kwic(dataclean$fulltexttitle, "Sánchez", window = 3, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "Pedro", window = 3, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "Moncloa", window = 3, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "moncloa", window = 3, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "Gobierno", window = 3, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "gobierno", window = 2, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "ejecutivo", window = 3, valuetype = "fixed")


#Deleting unrelated obvservations
finaldat <- dataclean[ -c(43, 48, 51, 63, 69, 233, 237, 254, 258, 282, 301, 305, 314, 317, 320, 345, 419, 449, 452, 453, 459, 464, 166, 170, 319, 8, 33, 46, 65, 85, 86, 95, 111, 140, 157, 199, 201, 217, 257, 260, 309, 222, 382, 384), ]


#keyword analysis second round (and double check)

kwic(finaldat$fulltexttitle, "Pedro", window = 3, valuetype = "fixed")
kwic(finaldat$fulltexttitle, "Gobierno", window = 2, valuetype = "fixed")


finalda <- finaldat[ -c(284, 259), ]


#final data control done manually on the articles not visualized by kwic
finaldata <- finalda[ -c(268, 343, 361, 367, 409, 421, 435, 441, 442, 454), ]


#save dataframe
save(finaldata, file = "ElMundoSanchezFinal.RData")


