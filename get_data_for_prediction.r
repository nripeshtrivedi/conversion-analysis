library(gWidgets)
library(RMySQL)
library(DBI)
library(dbConnect)
library(sqldf)
library(foreach)
library(doParallel)
library(ROCR)
library(verification)
library(changepoint)

plotROC <- function(truth, predicted, ...){
  pred <- prediction(abs(predicted), truth)    
  perf <- performance(pred,"tpr","fpr")
  
  plot(perf, ...)
}

library(randomForest)
mydb=dbConnect(MySQL(),user='root',password='',dbname='7cot-fall15',host='localhost')

# For Members
activity <- fetch(dbSendQuery(mydb,'select actDate, action, actCount, actUserID from actionLogging'),n = -1)
a <- fetch(dbSendQuery(mydb,'select actDate, action, actCount, actUserID from actionLogging'),n = -1)
activity$action <- as.factor(activity$action)
activity$actDate <- as.numeric(as.POSIXct(activity$actDate,'%Y-%m-%d'))
a$actDate <- as.numeric(as.POSIXct(activity$actDate,'%Y-%m-%d'))
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


listID1<-0
active1<-0
numconversation1<-0
signupDate1<-0
end_week_two1<-0
numMsgUser1<-0
onlineswitch1<-0
currentLevel1<-0
xpPoints1<-0
email1<-0
gender1<-0
cheersPoints1<-0
forumUpvotes1<-0
activedays1<-0
compassionHearts1<-0
coins1<-0
numLogins<-0
lastLogin<-0
pageViewsWeb<-0
groupSupportMsgs1<-0
AccountLive1<-0
ProfileCompleteU1<-0
trainingCompleteU1<-0
Rating1<-0
visitedChat1<-0
visitedForum1<-0
numLogins1<-0
pageViewsWeb1<-0
convReq1<-0
ListID<-0
Active<-0
Numconversation<-0
SignupDate<-0
end_week_two1<-0
NumMsgUser<-0
Onlineswitch<-0
CurrentLevel<-0
XpPoints<-0
Email<-0
CheersPoints<-0
ForumUpvotes<-0
Activedays<-0
CompassionHearts<-0
Coins<-0
GroupSupportMsgs<-0
Accountlive<-0
Profilecompleteu<-0
TrainingcompleteU<-0
L_Rating<-0
VisitedChat<-0
VisitedForum<-0
NumLogins<-0
PageViewsWeb<-0
ConvReq<-0 
#we build this table in parallel because of all the lookups on the action table required
registerDoParallel(makeCluster(8)) ##Specfiy number of clusters to use here
 for(i in 1:length(m$listID)) 
{
    ListID[i] <- m$listID[i] 
    Active[i]=is_active[i]
    Numconversation[i]=m$numconversation[i]
    SignupDate[i]=m$signupDate[i]
    end_week_two1[i]=(m$signupDate[i] + 60*60*24*7*2)
    NumMsgUser[i]=m$numMsgUser[i]
    Onlineswitch[i]=m$onlineswitch[i]
    CurrentLevel[i]=m$currentLevel[i]
    XpPoints[i]=m$xpPoints[i]
    Email[i]=m$email[i]
    CheersPoints[i]=m$cheersPoints[i]
    ForumUpvotes[i]=m$forumUpvotes[i]
    Activedays[i]=m$activedays[i]
    CompassionHearts[i]=m$compassionHearts[i]
    Coins[i]=m$coins[i]
    GroupSupportMsgs[i]=m$groupSupportMsgs[i]
    Accountlive[i]=m$AccountLive[i]
    Profilecompleteu[i]=m$ProfileCompleteU[i]
    TrainingcompleteU[i]=m$trainingCompleteU[i]
    L_Rating[i]=m$Rating[i]
    VisitedChat[i]=m$visitedChat[i]
    VisitedForum[i]=m$VisitedForum[i]
    NumLogins[i]=m$numLogins[i]
    PageViewsWeb[i]=m$pageViewsWeb[i]
    ConvReq[i]=m$convReq[i]
}
performance(l,"auc")@y.values[[1]]
df = data.frame(  ListID,
                  Active,
                  Numconversation,
                  SignupDate,
                end_week_two1,
                NumMsgUser,
                Onlineswitch,
                CurrentLevel,
                XpPoints,
                Email,
                CheersPoints,
                ForumUpvotes,
                Activedays,
                CompassionHearts,
                Coins,
                GroupSupportMsgs,
                Accountlive,
                Profilecompleteu,
                TrainingcompleteU,
                L_Rating,
                VisitedChat,
                VisitedForum,
                NumLogins,
                PageViewsWeb,
                ConvReq)  

df$Active <- as.factor(df$Active)
s <- sample(1:nrow(df), nrow(df)*0.66, replace=F)
#0.2105373
train <- df[s,]
test <- df[-s,]
test <- na.omit(test)
train <- na.omit(train)
performance(l,"auc")@y.values[[1]]

cols <- c(
          "Active",
          "Numconversation",
          "SignupDate",
          "NumMsgUser",
          "Onlineswitch",
          "CurrentLevel",
          "XpPoints",
          "Email",
          "CheersPoints",
          "ForumUpvotes",
          "Activedays",
          "CompassionHearts",
          "Coins",
          "GroupSupportMsgs",
          "Accountlive",
          "Profilecompleteu",
          "TrainingcompleteU",
          "L_Rating",
          "VisitedChat",
          "VisitedForum",
          "NumLogins",
          "PageViewsWeb",
          "ConvReq")
fit <- randomForest(Active ~ ., 
                    data=train[,cols], importance=TRUE,ntree=1000,na.action=na.omit,do.trace=T,replace=T)
fit

rf.pr = predict(fit,newdata=test[,cols])

results <- data.frame(pred = predict(fit,newdata=test[,cols]),
                      obs = test$Active)
table(results)

plot(performance(prediction(predict(fit,test[,cols],typ="prob")[,2], test$Active), 'tpr', 'fpr'),lwd=3,col="red")
l<-prediction(predict(fit,test[,cols],typ="prob")[,2], test$active1)

p<-performance(prediction(predict(fit,test[,cols],typ="prob")[,2], test$active1), 'tpr', 'fpr','auc',fpr.stop=0.5,measure="tpr",x.measure="fpr")
performance(l,"auc")@y.values[[1]]
abline(0,1,col="black")
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
performance(l,"auc")@y.values[[1]]
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
    w <- union(which(df$numMsgUser <= i), which(df$ConvReq <= j));
    a <- df$Active; 
    k[i,j] <- length(which(a[w] == 1))/length(w)
  }
}
colnames(k) <- 1:500
rownames(k) <- 1:500
k <- as.data.frame(as.table(k),stringsAsFactors = F)
col <- colorRampPalette(c("red","blue"))
par(mar=c(5.1,4.1,2.1,2.1))
plot(k$Var2[which(k$Var2==1)],typ="l",ylim=c(0.15,0.21),xlim=c(0,125),lwd=3,col=col(11)[1],xlab="\u2264 Messages Sent As New Member",ylab="% Of New Members That Are Now Active")
 lines(k$Active[which(k$ConvReq>=50)],typ="l",lwd=2,col=col(11)[11])
 lines(k$Active[which(k$ConvReq==20)],typ="l",lwd=2,col=col(11)[10])
 lines(k$Active[which(k$ConvReq==15)],typ="l",lwd=2,col=col(11)[9])
 lines(k$Active[which(k$ConvReq==10)],typ="l",lwd=2,col=col(11)[8])
lines(k$Active[which(k$ConvReq==8)],typ="l",lwd=3,col=col(11)[7])
lines(k$Active[which(k$ConvReq==6)],typ="l",lwd=3,col=col(11)[6])
lines(k$Active[which(k$ConvReq==5)],typ="l",lwd=3,col=col(11)[5])
 lines(k$Active[which(k$ConvReq==4)],typ="l",lwd=3,col=col(11)[4])
lines(k$Active[which(k$ConvReq==3)],typ="l",lwd=3,col=col(11)[3])
lines(k$Active[which(k$ConvReq==2)],typ="l",lwd=3,col=col(11)[2])
legend(x = 60,y=0.19,legend = c("\u2264 1","\u2264 2","\u2264 3","\u2264 4","\u2264 5","\u2264 6","\u2264 8","\u2264 10","\u2264 15","\u2264 20","\u2264 50 Coversation Requests"),pch=16,col=col(11),bty="n")

#6x5
#plot(k,xlab="Number of Messages Sent", ylab="% of `active' users sending < x initial messages", typ="l",lwd=6,col="blue",xlim=c(1,200))
#remove(k),col=col(1)
