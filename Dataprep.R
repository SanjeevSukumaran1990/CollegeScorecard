
library(readr)
library(RSQLite)
library(ggplot2)
library(gridExtra)


## Read the code from sqllite file
db <- dbConnect(dbDriver("SQLite"), "F:/Data Minimg/college-scorecard-release-2015-09-23-15-08-57/output/database.sqlite")
db
sample <- dbGetQuery(db, "SELECT COSTT4_A AverageCostOfAttendance,
       md_earn_wne_p10 MedianEarnings,ZIP FROM Scorecard WHERE Year='2011'")
View(sample)
#data preparation
sample<-sample[complete.cases(sample),]
View(sample)
sample <- sample[as.numeric(as.character(sample$MedianEarnings)) > 0,]
sort(sample$ZIP)
View(sample)
t<-sample$AverageCostOfAttendance
d<-sample$MedianEarnings
roi<-d/t
sample$roi<-roi
min(sample$roi)
max(sample$roi)
write.csv(sample, file = "zip1.csv")




