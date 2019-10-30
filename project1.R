#Author: Murugan
#Data:10/02/19
#Progess: Still working..

#Short analysis on crime rates in London

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
