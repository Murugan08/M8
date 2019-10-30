#Author: Murugan
#Data:10/02/19
#Progess: Still working..

#London Crime data simple analysis
data1 <- london_crime_by_lsoa

model1 <- lm(log(value)~year, data=data)
model1
summary(model1)

nrow(data1)

max(data1$borough) #Highest crime commited is at westminster, London.
newdata <- subset(data1, borough='Westminster',
                  select=major_category:minor_category)
max(newdata$major_category)#Westminster ->Highest Major Category-> Violence Against the Person
max(newdata$minor_category)#Westminster -> Highest Minor Category-> Wounding/GBH

newdata1 <- subset(data1, borough='Ealing',
                   select=major_category:minor_category)
max(newdata1$major_category)
#Violence Against the Person
max(newdata1$minor_category)
#Wounding/GBH
##############################################################
#Short Analaysis on Time series data
#Mergeddata -> bitcoin , oil, exchange rate gold, sp
bitcoinnew <- fread('mergeddata.csv')#Bitcoin+Gold+Oil
summary(bitcoinnew)
head(bitcoinnew)

bitcoinnew$date <- as.Date(bitcoinnew$date)
bitcoinnew$date

ts.plot(bitcoinnew$bitcoin, gpars = list(xlab = "day" , ylab = "bitcoin"))
ggplot(bitcoinnew, aes(x=date,bitcoin)) + geom_line()

#Linear model between the data variables
model1<-lm(bitcoin~sp+gold+oil+user,data=bitcoinnew)
summary(model1)
coeftest(model1,vcov.=vcovHC)
model2<-lm(log(bitcoin)~log(sp)+log(gold)+log(oil)+log(user),data=bitcoinnew)
summary(model2)
#To find relation between oil,gold prices
rep.kpss <- function(series,maxdiffs=5,crit.val=0.05) {
  x <- series
  diffs <- 0
  for(i in 1:maxdiffs) {
    pval <- kpss.test(x,null="Level")$p.value
    if(pval >= crit.val) 
      return(c(diffs,0,pval))
    pval <- kpss.test(x,null="Trend")$p.value
    if(pval >= crit.val) 
      return(c(diffs,1,pval))
    x <- diff(x)
    diffs <- diffs + 1
  }
  return(FALSE)
}

rep.kpss(bitcoinnew$bitcoin)
rep.kpss(bitcoinnew$sp)
rep.kpss(bitcoinnew$gold)
rep.kpss(bitcoinnew$user)
rep.kpss(bitcoinnew$oil)
#gold prices are not related with the bitcoin price 
#oil prices are not related with the bitcoin price 

model2 <- lm(diff(bitcoin)~diff(sp)+diff(gold)+diff(user)+diff(oil), data = bitcoinnew)
summary(model2)
coeftest(model2,vcov.=vcovHC)
#gold prices are not related with the bitcoin price 
#oil prices are not related with the bitcoin price 
#euro prices are not related with the bitcoin price
#it seems that relationship exist between bitcoin and sp



