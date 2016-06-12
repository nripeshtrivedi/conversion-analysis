library(tree)
library(randomForest)
library(gWidgets)
library(DBI)
library(ggplot2)
library(plyr)

if(!exists("members")){
  ## Get data from MySQL
  source('get_tables_from_db.r')
}

## Filter away data where signupdate and lastlogin is NA
members <- members[!duplicated(members$memID),]
members <- unique(members)
m <- members[logged_members,]
m <- m[(which(is.na(m$signupDate) == FALSE)),]
m <- m[(which(is.na(m$lastLogin) == FALSE)),]
m <- m[(which(is.na(m$signupDistressLevel) == FALSE)),]


s <- sample(1:length(m[,1]), length(m[,1])*0.75, replace=F)
train <- m[s,]
test <- m[-s,]
test <- na.omit(test)
cols <- c("coins","groupSupportMsgs","forumUpvotes","growthPoints","compassionHearts",
          "signupDate","lastLogin","numForumPosts",
          "signupDistressLevel","numLogins","convRequests","forumViews","helpViews",
          "pageViewsApp","pageViewsWeb","message_rate")
fit <- randomForest(message_rate ~ ., 
                    data=train[,cols], importance=TRUE,ntree=1000,na.action=na.omit,do.trace=T)
fit

importance(fit)
#so, how good is the model at predicting a user's messaging rate?
rf.pr = predict(fit,newdata=test[,cols])

par(mar=c(4.1,4.1,1.1,1.1))
plot(rf.pr,test$message_rate,log="xy",xlim=c(0.01,100),ylim=c(0.01,100),col="blue",xlab="Predicted Rate", ylab="Actual Rate",cex=0.6,cex.axis=1)
abline(0,1,col="red",lwd=3)

qqplot(rf.pr,test$message_rate,plot.it = T,xlim=c(0,100),ylim=c(0,100),xlab="Predicted Quantile",ylab="Actual Quantile",col="blue",pch=5,cex=0.8,cex.axis=1)
abline(0,1,col="red",lwd=3)
RMSE <- (sum((rf.pr-test$message_rate)^2)/length(test$message_rate))^(1/2)
RMSE

#what are the importance of the factors we considered? 
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
        legend.position="none") + labs(x = "% Increase in MSE") + scale_x_continuous(breaks=seq(0,30,5))
remove(data,i,m)