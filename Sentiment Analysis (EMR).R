#sentient analysis
#run without clearing the data from the tokenization and topic analysis file


library(quanteda)

load("D:/Master Denmark/3rd Semester/Political Data Science/Final Exam/True Final Exam/1. Downloading/RajoyElMundotopics.RData")


#Load sentient dictionary of postive and negative words 
library(RCurl)

x <- getURL("https://raw.githubusercontent.com/JoseCardonaFigueroa/sentiment-analysis-spanish/master/data/subjectivity.csv")
dictio <- read.csv(text = x, as.is = T)

View(dictio)

pos.words <- dictio$termino[dictio$polaridad=="positive"]
neg.words <- dictio$termino[dictio$polaridad=="negative"]

#check validity of the classification
sample(pos.words,10)
sample(neg.words, 10)
#nice

#create corpus in case of need
twcorpus <- corpus(newdata$full_text)


#dictionary creation
mydict <- dictionary(list(negative = neg.words,
                positive = pos.words))

#process running a sentiment classification of articles and classify them on sentiment cluster

txt.mat.2 <- dfm_lookup(txt.mat, mydict, exclusive = T, valuetype = "fixed", verbose = T)

text.df <- convert(txt.mat.2, to = "data.frame")

text.df$sentiment <- with(text.df, positive - negative)

for (i in 1:nrow(text.df)){
  if(text.df$sentiment[i]>=2){
    text.df$score[i] = "Positive"
  } else if (text.df$sentiment[i]<=-2){
    text.df$score[i] = "Negative"
  } else if (text.df$sentiment[i] >-2 & text.df$sentiment[i] < 2){
    text.df$score[i] = "Neutral"}
}

#save everything in case of errors handling the code 
save(text.df, file="itsworking(EMR).RData")

newdata$sentiment <- text.df$sentiment
newdata$score <- text.df$score

library(ggplot2)

#create sentiment graphs on the specific newspaper
qplot(data = newdata, x = score, 
      geom = "bar", xlab = "", 
      ylab = "Frequency", main = "Rajoy's Executive teatment ElMundo") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))




#create a topic sentient graph
ggplot(newdata, aes(x=topic_lab, y=sentiment, label=topic_lab)) + 
  geom_bar(stat='identity', aes(fill=score), width=.5)  +
  scale_fill_manual(name="Sentiment", 
                    labels = c("Negative", "Neutral", "Positive"), 
                    values = c("Negative"="#f8766d", "Neutral"="#ffff00", "Positive"="#00ba38")) + 
  labs(subtitle="Articles from El Mundo", 
       title= "Topic Sentiment of Rajoy's Executive") + 
  coord_flip()


save(newdata, file="SentientAnalysis(EMR).RData")
load("SentientAnalysis(EMR).RData")

#!!!Data originally expected to be included on the final paper but due to word limitations decided at the last moment not to be added!!!#start treatment of data for time series

#select month aggregation for proper visualization of results
#month<- strftime(newdata$date, "%m")
#year<- strftime(newdata$date, "%Y")
#finaldate <- paste0(year, "-", month, "-", "01")
#newdata$finaldate <- finaldate

#install.packages("lubricate")

#library(lubridate)

#newdata$finaldate <- as.Date(newdata$finaldate, "%Y-%m-%d")


#First Time Series (daily)

#ggplot(newdata, aes(x=date)) + 
#  geom_line(aes(y=sentiment)) + 
 # labs(title="Time Series Chart", 
  #     subtitle="Returns Percentage from 'Economics' Dataset", 
   #    caption="Source: Economics", 
    #   y="sentiment")

#Second Time Series (monthly)

# labels and breaks for X axis text
#lbls <- paste0(month.abb[month(newdata$date)], " ", lubridate::year(newdata$date))
#brks <- newdata$finaldate

#newdata$lblsa <- lbls

# plot
#ggplot(newdata, aes(x=finaldate)) + 
 # geom_line(aes(y=sentiment)) + 
  #labs(title="Monthly Time Series", 
   #    subtitle="Returns Percentage from Economics Dataset", 
    #   caption="Source: Economics", 
     #  y="sentiment") +  # title and caption
  #scale_x_date(labels = lbls, 
   #            breaks = brks) +  # change to monthly ticks and labels
  #theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
   #     panel.grid.minor = element_blank())  # turn off minor grid

#numerical values for the Time Series (monthly)

#aggregate( sentiment ~ finaldate , newdata , mean )

#save(newdata, file="Visualization(EMR).RData")

#dummy variables for regression

newdata$elpais <- 0
newdata$elmundo <- 1
newdata$Rajoy <- 1
newdata$Sanchez <- 0

dataEMR <- newdata

save(dataEMR, file="Complete(EMR).RData")
