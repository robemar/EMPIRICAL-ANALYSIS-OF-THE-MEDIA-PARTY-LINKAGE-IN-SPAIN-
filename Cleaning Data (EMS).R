
rm(list = ls())
setwd("D:/Master Denmark/3rd Semester/Political Data Science/Final Exam/True Final Exam/1. Downloading")

library(dplyr)


#load parsing results
load("D:/Master Denmark/3rd semester/Political Data Science/Final Exam/True Final Exam/1. Downloading/ElMundoSanchezText.RData")

#clean parsing results


#repeated articles (with different titles)
textclean <- finaldata %>% distinct(full_text, .keep_all = TRUE)

#articles with errors, we write "a" as it is one of the most common letters in words 
find.hits <- grepl("a", textclean$full_text, ignore.case = T)

fulltexttext <- textclean$full_text[find.hits]

save(fulltexttext, file = "Sanchez(ElMundo).RData")  #just in case
save(textclean, file = "Sanchez(ElMundo)clean.RData")

