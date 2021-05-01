setwd("D:/Master Denmark/3rd Semester/Political Data Science/Final Exam/True Final Exam/1. Downloading")

#load all the datasets to one
load("Complete(EMR).RData")
load("Complete(EMS).RData")
load("Complete(EPR).RData")
load("Complete(EPS).RData")

#unite all datasets
Fulldata <- rbind(dataEMR, dataEMS, dataEPR, dataEPS)

FulldataRajoy <- rbind(dataEMR, dataEPR)
FulldataSanchez <- rbind(dataEMS, dataEPS)


#run first regression analysis for Rajoy
fit1 <- lm(sentiment ~ elpais, data=FulldataRajoy)
summary(fit1)
confint(fit1, level=0.95) # Confidence Intervalss for model parameters 


#run second regression analysis for Sanchez
fit2 <- lm(sentiment ~ elpais, data=FulldataSanchez)
summary(fit2)
confint(fit2, level=0.95) # Confidence Intervals for model parameters 
