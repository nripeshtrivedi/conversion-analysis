library(gWidgets)
library(RMySQL)
#library(DBI)
library(dbConnect)
library(sqldf)
library(foreach)
library(doParallel)
library(ROCR)
plotROC <- function(truth, predicted, ...){
  pred <- prediction(abs(predicted), truth)    
  perf <- performance(pred,"tpr","fpr")
  
  plot(perf, ...)
}

library(randomForest)
mydb=dbConnect(MySQL(),user='root',password='',dbname='7cot-fall15',host='localhost')

# For Members
activity <- fetch(dbSendQuery(mydb,'select actDate, action, actCount, actUserID from actionlogging'),n = -1)
activity$action <- as.factor(activity$action)
activity$actDate <- as.numeric(as.POSIXct(activity$actDate,'%Y-%m-%d'))

logged_members <- members$signupDate >= as.numeric(as.POSIXct("2013-5-05 00:00:00 EST")) & 
  members$signupDate <= as.numeric(as.POSIXct("2014-10-16 00:00:00 EST")) 

m <- members[logged_members,]
m <- m[(which(is.na(m$signupDate) == FALSE)),]
m <- m[(which(is.na(m$lastLogin) == FALSE)),]
m <- m[(which(is.na(m$signupDistressLevel) == FALSE)),]


## Separate users into 'active' and 'inactive' classes. 
### Active: A user who has signed up for at least two weeks and has made at least two actions on the site in the last month
##### 2 week time span:
is_active <- rep(F,length(m[,1]))
last_act_time <- max(activity$actDate)
last_month <- last_act_time - (60*60*24*7*4) #4 weeks
actions <- sqldf(paste("select action, sum(actCount) as count, actUserId from activity where ",
                       "action = 'convMessage' or action = 'forumPost' or action = 'convRequestGeneral' or action = 'convRequestPersonal' ",
                       "and actDate >= ",last_month," group by actUserID"),drv="SQLite")  

for(i in 1:length(is_active)){
  two_weeks <- m$signupDate[i] + 60*60*24*7*2
  if(two_weeks < last_act_time){
    j <- which(actions$actUserID == m$listID[i]) 
    is_active[i] <- length(j) == 1 && actions$count[j] > 1
  }
}

actions <- sqldf(paste("select action, sum(actCount) as count, actUserId, actDate from activity group by action, actUserID"),drv="SQLite")  

#we build this table in parallel because of all the lookups on the action table required
registerDoParallel(makeCluster(8)) ##Specfiy number of clusters to use here
two_week_activity <- foreach(i = 1:length(m$listID),.combine=rbind) %dopar% {
  
  
  a <- actions[actions$actDate <= m$signupDate[i] + 60*60*24*7*2 &
                 actions$actUserID == m$listID[i],]
  
  data.frame(
    memID = m$listID[i], 
    active=is_active[i],
    activedays=activeDays[m$listID[i]],
    signupDate=m$signupDate[i],
    end_week_two=m$signupDate[i] + 60*60*24*7*2,#2 weeks
    numMsgUser=sum(a[a$action=="convMessage",]$count),
    numForumPosts=sum(a[a$action=="forumPost",]$count),
    acctLogins=sum(a[a$action=="acctLogin",]$count),
    pageViewsApp=sum(a[a$action=="pageViewApp",]$count),
    convReq=sum(a[a$action=="convRequestGeneral" | a$action=="convRequestPersonal",]$count),
    pageViewsWeb=sum(a[a$action=="paveViewWeb",]$count)
  )
}

two_week_activity$gender=m$gender
cv<-two_week_activity

two_week_activity$active <- as.factor(two_week_activity$active)
s <- sample(1:nrow(two_week_activity), nrow(two_week_activity)*0.66, replace=F)
#0.2105373
train <- two_week_activity[s,]
test <- two_week_activity[-s,]
test <- na.omit(test)

cols <- c("active","numMsgUser","numForumPosts","acctLogins","convReq"
          ,"pageViewsApp","pageViewsWeb","activedays")
fit <- randomForest(active ~ ., 
                    data=train[,cols], importance=TRUE,ntree=1000,na.action=na.omit,do.trace=T
                    ,replace=T)
fit

rf.pr = predict(fit,newdata=test[,cols])

results <- data.frame(pred = predict(fit,newdata=test[,cols]),
                      obs = test$active)
table(results)
plot(performance(prediction(predict(fit,test[,cols],typ="prob")[,2], test$active), 'tpr', 'fpr'),lwd=3,col="red")
abline(0,1,col="grey")
l<-prediction(predict(fit,test[,cols],typ="prob")[,2], test$active)
performance(l,"auc")@y.values[[1]]
# Default
# F 726 30 // T 516 2252
# 2800/4200
#
# 3500/3500
# F 726 30 // T 461 2178
# 4200/2800
# F 769 88 // T 473 2194
# 5250/750
# F 721 27 // T 521 2255
#obs
#pred    FALSE TRUE
#FALSE  1025  140
#TRUE    593 2888
i <- varImpPlot(fit)
data <- as.data.frame(cbind(rownames(i),round(i[,"MeanDecreaseAccuracy"],1)))
colnames(data) <- c("Parameters","IncMSE")
data$MSE <- as.numeric(as.character(data$IncMSE))
data <- data[order(data$MSE),]
data$Parameters <- factor(data$Parameters, levels=data$Parameters[order(data$MSE)])
ggplot(data, aes(MSE,Parameters)) + geom_point(cex=4) + theme_bw() +
  theme(axis.text.x = element_text(colour = 'black', angle = 60, size = 18, hjust = 0.5, vjust = 0.5),
        axis.title.x= element_text(colour= 'black', size = 20), 
        axis.text.y = element_text(colour = 'black', size = 18), 
        axis.title.y = element_blank(),
        strip.text.y = element_text(size = 11, hjust = 0.5, vjust = 0.5, face = 'bold'),
        legend.position="none") + labs(x = "Mean Decrease Accuracy (%)") #+ scale_x_continuous(breaks=seq(0,30,5))
remove(data,i,m)

k <- matrix(nrow = 500,ncol=500,data = rep(0,250000))
for(i in 1:500){
  cat(i,'\n')
  for(j in 1:500){
    w <- union(which(two_week_activity$numMsgUser <= i), which(two_week_activity$convReq <= j));
    a <- two_week_activity$active; 
    k[i,j] <- length(which(a[w] == "TRUE"))/length(w)
  }
}
colnames(k) <- 1:500
rownames(k) <- 1:500
k <- as.data.frame(as.table(k),stringsAsFactors = F)
col <- colorRampPalette(c("red","blue"))
par(mar=c(5.1,4.1,2.1,2.1))
plot(k$active[which(k$convReq==1)],typ="l",ylim=c(0.15,0.21),xlim=c(0,125),lwd=3,col=col(11)[1],xlab="\u2264 Messages Sent As New Member",ylab="% Of New Members That Are Now Active")
lines(k$active[which(k$convReq==50)],typ="l",lwd=2,col=col(11)[11])
lines(k$active[which(k$convReq==20)],typ="l",lwd=2,col=col(11)[10])
lines(k$active[which(k$convReq==15)],typ="l",lwd=2,col=col(11)[9])
lines(k$active[which(k$convReq==10)],typ="l",lwd=2,col=col(11)[8])
lines(k$active[which(k$convReq==8)],typ="l",lwd=3,col=col(11)[7])
lines(k$active[which(k$convReq==6)],typ="l",lwd=3,col=col(11)[6])
lines(k$active[which(k$convReq==5)],typ="l",lwd=3,col=col(11)[5])
lines(k$active[which(k$convReq==4)],typ="l",lwd=3,col=col(11)[4])
lines(k$active[which(k$convReq==3)],typ="l",lwd=3,col=col(11)[3])
lines(k$active[which(k$convReq==2)],typ="l",lwd=3,col=col(11)[2])
legend(x = 60,y=0.19,legend = c("\u2264 1","\u2264 2","\u2264 3","\u2264 4","\u2264 5","\u2264 6","\u2264 8","\u2264 10","\u2264 15","\u2264 20","\u2264 50 Coversation Requests"),pch=16,col=col(11),bty="n")

#6x5
#plot(k,xlab="Number of Messages Sent", ylab="% of `active' users sending < x initial messages", typ="l",lwd=6,col="blue",xlim=c(1,200))
#remove(k),col=col(1)
