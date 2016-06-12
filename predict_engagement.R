library(tree)
library(randomForest)
library(gWidgets)
library(DBI)
library(ggplot2)
library(plyr)
library(miscTools)


if(!exists("members")){
  ## Get data from MySQL
  source('../get_tables_from_db.r')
}

## Filter away data where signupdate and lastlogin is NA
members <- members[!duplicated(members$listID),]
members <- unique(members)
if("signupDateU" %in% colnames(members)) { 
  members <- rename(members,c("signupDateU"="signupDate","lastLoginU"="lastLogin"))
}
members <- members[(which(is.na(members$signupDate) == FALSE)),]
members <- members[(which(is.na(members$lastLogin) == FALSE)),]
members <- members[(which(is.na(members$currentLevel) == FALSE)),]
members$lastCheckTime <- NULL

##Get activity of users in the last 4 days
source("get_data_for_prediction.r")
cols <- c("coins","groupSupportMsgs","forumUpvotes","cheersPoints","compassionHearts", "numconversation",
          "signupDate","lastLogin",
          "currentLevel","xpPoints",
          "numForumPosts","message_rate", "activedays","gender","email","onlineswitch","ProfileCompleteU","AccountLive")
member<-members[,cols]


s <- sample(1:length(member[,1]), length(member[,1])*0.75, replace=F)
train <- member[s,]
test <- member[setdiff(1:length(member[,1]),s),]
test <- na.omit(test)

fit <- randomForest(message_rate ~ ., data=train[,cols], importance=TRUE,ntree=400,na.action=na.omit,do.trace=T)
fit

importance(fit)
#so, how good is the model at predicting a user's conversation rate?
rf.pr = predict(fit,newdata=test[,cols])

par(mar=c(4.1,4.1,1.1,1.1))
plot(rf.pr,test$message_rate,log="xy",xlim=c(0.01,100),ylim=c(0.01,100),col="blue",xlab="Predicted Rate", ylab="Actual Rate",cex=0.6,cex.axis=1)
abline(0,1,col="red",lwd=3)

qqplot(rf.pr,test$message_rate,plot.it = T,xlim=c(0,150),ylim=c(0,150),xlab="Predicted Quantile",ylab="Actual Quantile",col="blue",pch=5,cex=0.8,cex.axis=1)
abline(0,1,col="red",lwd=3)
RMSE <- (sum((rf.pr-test$message_rate)^2)/length(test$message_rate))^(1/2)
RMSE
r2 <- rSquared(test$message_rate, test$message_rate - rf.pr)

attribute(#what are the importance of the factors we considered? 
i <- varImpPlot(fit)
data <- as.data.frame(cbind(rownames(i),round(i[,"%IncMSE"],1)))
data$Parameters
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
        legend.position="none") + labs(x = "% Increase in MSE") + scale_x_continuous(breaks=seq(0,45,5))
remove(data,i)
