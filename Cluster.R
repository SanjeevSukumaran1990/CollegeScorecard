## Cluster analysis to find the types of universities in US
types<- dbGetQuery(db, "SELECT UGDS
                        ADM_RATE_ALL, SATVRMID, SATMTMID,
                        COSTT4_A,
                        UGDS_WHITE,
                        UGDS_BLACK,
                        UGDS_HISP,
                        UGDS_ASIAN,
                        PCTPELL,
                        C150_4,
                        md_earn_wne_p6,
                        RPY_5YR_RT,
                        PCIP14,PCIP51
                        FROM Scorecard  WHERE Year='2011' AND PREDDEG='Predominantly bachelor''s-degree granting' AND PCIP14>0")

types<- na.omit(types) # listwise deletion of missing
types<- scale(types) # standardize variables
View(types)

# Determine number of clusters
wss <- (nrow(types)-1)*sum(apply(types,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(types, 
                                     centers=i,nstart=25, iter.max=1000)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


fit <- kmeans(types, 5) # 5 cluster solution
View(fit)
# get cluster means 
aggregate(types,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(types, fit$cluster)
fit <- kmeans(types, 5)
#plot cluster
library(cluster) 
clusplot(types, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
View(fit$cluster)
types$cluster<-NULL
new<-data.frame(types,fit$cluster)
View(new)
cluster1<-NULL
cluster1<-subset(fit,fit$cluster='1L')
s<-table(fit$cluster)
