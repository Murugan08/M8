n <- 500
set.seed(75080)

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 50
y   <- -100*z+ 1100 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt1 <- data.table('id'=1:500,'sat'=y,'income'=x,'group'=rep(1,n))

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 80
y   <- -80*z+ 1200 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt2 <- data.table('id'=501:1000,'sat'=y,'income'=x,'group'=rep(2,n))

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 30
y   <- -120*z+ 1000 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt3 <- data.table('id'=1001:1500,'sat'=y,'income'=x,'group'=rep(3,n))

dtable <- merge(dt1    ,dt2, all=TRUE)
dtable <- merge(dtable ,dt3, all=TRUE)

ggplot(dtable,aes(x=income,y=sat,color=as.factor(group)))+geom_point()

#(1)
#Data generating process refers to the methods used by researchers to generate data from 
#a sampled data source in a qualitative study.The data generating process is done using three groups here. Those are lower,middle,higher income groups , Within every group,SAT score is negatively related to income level. 
#but in Between groups, the effect might  be different, which is  not  obvious from  data generating process.

#(2)
pooled <- lm(sat~income,data=dtable)
within <- lm(sat~income+group-1,data=dtable)
model1 <- lm(sat~income,data=dtable[group==1])
model2 <- lm(sat~income,data=dtable[group==2])
model3 <- lm(sat~income,data=dtable[group==3])


summary(pooled)
# For every $1000 increase in annual income of the family, the 
# SAT scores increase by 2.79
summary(within)
# For every $1000 increase in income of the family, the SAT scores decrease by 
# 20.173 
summary(model1)
# Every $1000 increase in income reduces the SAT scores by 19.85
summary(model2)
# Every $1000 increase in income reduces the SAT scores by 16.32 
summary(model3)
# Every $1000 increase in income reduces the SAT scores by 24.027

#(3)
tree <- ctree(sat~income,data=dtable)
plot(tree)
tree <- ctree(sat~group,data=dtable)
plot(tree)
tree <- ctree(sat~income+group,data=dtable)
plot(tree)
#Splitting based on income makes no sense here. The income variable have a continuous relationship with SAT ,so the trees are very different and uncontrollable here. 
#better tree here is based on the group variable.

#(4)
glmtree(sat~income|group,data=dtable)
plot(glmtree(sat~income|group,data=dtable))
# glmtree is perfect model as this is the data generating process, linear models based on splits defined by the three groups.
# splitting on the group variable and including income in linear modeling process is critical here.


#(5)
auto.kmeans <- function(data,maxclu=10,seed=1,nstart=10) {
  wss <- rep(NA,maxclu)
  for (i in 1:maxclu) { 
    set.seed(seed)
    model <- kmeans(data,centers=i,nstart=nstart)
    wss[i] <- model$tot.withinss
  }
  plot(1:maxclu,	wss, type="b", 
       xlab="Number of Clusters",
       ylab="Aggregate Within Group SS")
}
auto.kmeans(dtable)
set.seed(1)
model <- kmeans(dtable,centers=3,nstart=10)
model$centers

#optimal number of groups are 3.

#(6)

auto.hclust <- function(data,model=hclust(dist(data)),maxclu=10) {
  wss <- rep(NA,maxclu)
  r <- ncol(data)+1
  for(i in 1:maxclu) {
    groups <- cutree(model,i)
    means <- data[order(groups),lapply(.SD,mean),by=groups]
    means <- means[,2:r]
    demeaned <- data - means[groups]
    wss[i] <- sum(rowSums(demeaned^2))
  }
  plot(1:maxclu,	wss, type="b", 
       xlab="Number of Clusters",
       ylab="Aggregate Within Group SS")
}
data <- dtable
auto.hclust(data)
auto.hclust(data,model=hclust(dist(data),method="complete"))
auto.hclust(data,model=hclust(dist(data),method="average"))
auto.hclust(data,model=hclust(dist(data),method="median"))
auto.hclust(data,model=hclust(dist(data),method="ward.D"))
#Hierarchical clustering gives a better fit to the model.
#(7)


pooled <- lm(sat~income,data=dtable)
within <- lm(sat~income+groupsk-1,data=dtable)
model1 <- lm(sat~income,data=dtable[groupsk==1])
model2 <- lm(sat~income,data=dtable[groupsk==2])
model3 <- lm(sat~income,data=dtable[groupsk==3])

summary(pooled)
# For every $1000 increase in annual income of the family, the 
# SAT scores increase by 2.79

summary(within)
# For every $1000 increase in income of the family, the SAT scores decrease by 
# 11.918

summary(model1)
# For every $1000 increase in income reduces the SAT scores by 15.38 

summary(model2)
# For every $1000 increase in income reduces the SAT scores by 6.57 

summary(model3)
# For every $1000 increase in income reduces the SAT scores by 15.38


#(8)

auto.kmeans <- function(data,maxclu=10,seed=1,nstart=10) {
  wss <- rep(NA,maxclu)
  for (i in 1:maxclu) { 
    set.seed(seed)
    model <- kmeans(data,centers=i,nstart=nstart)
    wss[i] <- model$tot.withinss
  }
  plot(1:maxclu,	wss, type="b", 
       xlab="Number of Clusters",
       ylab="Aggregate Within Group SS")
}
auto.kmeans(dtable$income)
# Yes, we can find out the relationship from the data generating process. It gives us 3 clusters

#(9)

wss <- rep(NA,10)
for(i in 1:10)
  wss[i] <- kmeans(dtable%>%select(sat,income)%>%scale,i,nstart=10)$tot.withinss
kmeansplot(wss)

kmeans(dtable%>%select(income,sat)%>%scale,3)$centers
dtable$kmean_311 <- as.factor(kmeans(dtable%>%select(income,sat)%>%scale,3,nstart=10)$cluster)
table(dtable$kmean_311,dtable$group)

summary(modeli1 <- lm(sat~income+kmean_311-1,data=dtable))

#After including the scaling, we can find the negative relationship. 
#Caluclating identification rate for kmeans (from the matrix table) : (371+500+330)/1500=80% W
