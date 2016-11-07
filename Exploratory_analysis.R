## Exploratory Analysis
#Utilized SAS inbuild zipcode file to extract the zipcode along with status that the area belonging to particular zipcode is a rural or urban area
#Then combined that with the dataset to perform t-test.This was done to see whether rural area has better ROI or urban
final_1$status<-NULL
final_1$status<-ifelse(final_1$MSA>0,"URBAN","RURAL")
View(final_1)
final_1<-final_1[complete.cases(final_1),]
final_1 <- final_1[as.numeric(as.character(final_1$MSA)) > 0,]
View(final_1)
table(final_1$status)
s<-t.test(final_1$roi~final_1$status)
s

#contunuing with Exploratory Analysis
### top 10 colleges with best roi for year 2011

roi <- dbGetQuery(db, "SELECT COSTT4_A AverageCostOfAttendance,
       md_earn_wne_p10 MedianEarnings,INSTNM FROM Scorecard WHERE Year='2011' AND PREDDEG='Predominantly bachelor''s-degree granting' AND PCIP14>0 AND PCIP49=0")
View(roi)
roi$roi<-NULL
roi<-roi[roi$MedianEarnings>55000,]
roi$roi<-roi$MedianEarnings/roi$AverageCostOfAttendance
roi<-roi[complete.cases(roi),]
roi<-roi[order(-roi$roi),]
roi$AverageCostOfAttendance<-NULL
roi$MedianEarnings<-NULL
roi<-roi[1:10,]
View(roi)


##top 10 colleges having highest median pay

salary <- dbGetQuery(db, "SELECT 
       md_earn_wne_p10 MedianEarnings,INSTNM FROM Scorecard WHERE Year='2011' AND PREDDEG='Predominantly bachelor''s-degree granting' AND PCIP14>0 AND PCIP49=0")
View(salary)

salary<-salary[complete.cases(salary),]
salary1<-salary[order(-salary$MedianEarnings),]
salary1<-salary1[1:10,]
View(salary1)# REMOVED MEDICAL of them were from medical


##top 10 colleges with lowest Admission rate

admin<-salary <- dbGetQuery(db, "SELECT 
       ADM_RATE_ALL,INSTNM FROM Scorecard WHERE Year='2013' AND PREDDEG='Predominantly bachelor''s-degree granting' AND PCIP14>0")
View(admin)

admin<-admin[complete.cases(admin),]
admin<-admin[order(admin$ADM_RATE_ALL),]
admin<-admin[1:10,]
View(admin)


## latest roi calculations
salary <- dbGetQuery(db, "SELECT 
       md_earn_wne_p6 MedianEarnings,INSTNM FROM Scorecard WHERE Year='2011' AND PREDDEG='Predominantly bachelor''s-degree granting' AND PCIP14>0")
View(salary)

salary<-salary[complete.cases(salary),]
salary1<-salary[order(-salary$MedianEarnings),]
salary1<-salary1[1:10,]
View(salary1)# REMOVED MEDICAL of them were from medical

##statewise distribution of universities.

db<- dbConnect(dbDriver("SQLite"), "F:/Data Minimg/college-scorecard-release-2015-09-23-15-08-57/output/database.sqlite")
db
ch<- dbGetQuery(db, "SELECT STABBR  FROM Scorecard  WHERE Year='2013' AND PREDDEG='Predominantly bachelor''s-degree granting' AND PCIP14>0")
View(ch)
ch$count<-1
san<-aggregate(ch$count,by=list(c(ch$STABBR)),FUN=sum)
san
san<-san[order(-san$x),]
View(san[1:10,])


##Colleges with highest SAT SCORE

ch<- dbGetQuery(db, "SELECT SATVRMID,INSTNM FROM Scorecard  WHERE Year='2013' AND PREDDEG='Predominantly bachelor''s-degree granting' AND PCIP14>0")
View(ch)
ch<-ch[complete.cases(ch),]
ch<-ch[order(-ch$SATVRMID),]
ch[1:10,]

ch1<- dbGetQuery(db, "SELECT SATMTMID ,INSTNM FROM Scorecard  WHERE Year='2013' AND PREDDEG='Predominantly bachelor''s-degree granting' AND PCIP14>0")
View(ch1)
ch1<-ch1[complete.cases(ch1),]
ch1<-ch1[order(-ch1$SATMTMID),]
View(ch1[1:10,])


## top 10 most expensive colleges
exp<- dbGetQuery(db, "SELECT COSTT4_A,INSTNM FROM Scorecard  WHERE Year='2013' AND PREDDEG='Predominantly bachelor''s-degree granting' AND PCIP14>0")
View(exp)
exp<-exp[complete.cases(exp),]
exp<-exp[order(-exp$COSTT4_A),]
View(exp[1:10,])

##which fields are most popular
exp<-dbGetQuery(db,"SELECT UGDS,PCIP01,PCIP03,PCIP04,PCIP05,PCIP09,PCIP10,PCIP11,PCIP12,PCIP13,PCIP14,
                    PCIP15,PCIP16,PCIP19,PCIP22,PCIP23,PCIP24,PCIP25,PCIP26,PCIP27,PCIP29,PCIP41,PCIP42,PCIP45,PCIP51,PCIP52,PCIP54 FROM Scorecard  WHERE Year='2013'")
View(exp)

sum<-sum(exp$UGDS)
exp$PCIP01<-exp$PCIP01*exp$UGDS
exp$PCIP03<-exp$PCIP03*exp$UGDS
exp$PCIP04<-exp$PCIP04*exp$UGDS
exp$PCIP05<-exp$PCIP05*exp$UGDS
exp$PCIP09<-exp$PCIP09*exp$UGDS
exp$PCIP10<-exp$PCIP10*exp$UGDS
exp$PCIP11<-exp$PCIP11*exp$UGDS
exp$PCIP12<-exp$PCIP12*exp$UGDS
exp$PCIP13<-exp$PCIP13*exp$UGDS
exp$PCIP14<-exp$PCIP14*exp$UGDS
exp$PCIP15<-exp$PCIP15*exp$UGDS
exp$PCIP16<-exp$PCIP16*exp$UGDS
exp$PCIP19<-exp$PCIP19*exp$UGDS
exp$PCIP22<-exp$PCIP22*exp$UGDS
exp$PCIP23<-exp$PCIP23*exp$UGDS
exp$PCIP24<-exp$PCIP24*exp$UGDS
exp$PCIP25<-exp$PCIP25*exp$UGDS
exp$PCIP26<-exp$PCIP26*exp$UGDS
exp$PCIP27<-exp$PCIP27*exp$UGDS
exp$PCIP29<-exp$PCIP29*exp$UGDS
exp$PCIP41<-exp$PCIP41*exp$UGDS
exp$PCIP42<-exp$PCIP42*exp$UGDS
exp$PCIP45<-exp$PCIP45*exp$UGDS
exp$PCIP51<-exp$PCIP51*exp$UGDS
exp$PCIP52<-exp$PCIP52*exp$UGDS
exp$PCIP54<-exp$PCIP54*exp$UGDS
exp<-exp[complete.cases(exp),]
View(exp)
fields<-c("Agriculture","Natural resource","Architecture","Cultural studies","Communication","Support Services","Computer Services",
      "Personal Service","Education","Engineering"," Engineering Technologies","Foreign Languages", "Family and Consumer Sciences",
        "Legal Professions","English Language","Liberal Arts and Sciences","Library Science","Biological and Biomedical Sciences",
          "Mathematics and Statistics","Military Technologies","Science Technologies","Psychology","Social Sciences",
           " Health Professions ","Business","History")
Popularity<-c((sum(exp$PCIP01)/sum),(sum(exp$PCIP03)/sum),(sum(exp$PCIP04)/sum),(sum(exp$PCIP05)/sum),(sum(exp$PCIP09)/sum),
     (sum(exp$PCIP10)/sum),(sum(exp$PCIP11)/sum),(sum(exp$PCIP12)/sum),(sum(exp$PCIP13)/sum),(sum(exp$PCIP14)/sum),
     (sum(exp$PCIP15)/sum),(sum(exp$PCIP16)/sum),(sum(exp$PCIP19)/sum),(sum(exp$PCIP22)/sum),(sum(exp$PCIP23)/sum),
     (sum(exp$PCIP24)/sum),(sum(exp$PCIP25)/sum),(sum(exp$PCIP26)/sum),(sum(exp$PCIP27)/sum),(sum(exp$PCIP29)/sum),
     (sum(exp$PCIP41)/sum),(sum(exp$PCIP42)/sum),(sum(exp$PCIP45)/sum),(sum(exp$PCIP51)/sum),(sum(exp$PCIP52)/sum),
     (sum(exp$PCIP54)/sum))
Popularity<-round(Popularity*100,2)
fields1<-data.frame(fields,Popularity)
fields1
View(fields)
fields1<-fields1[order(-fields1$Popularity),]
fields1
View(fields1[1:10,])
