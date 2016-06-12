library(ggplot2)
library(scales)
source('../multiplot.r')

## Filter away data where signupdate and lastlogin is NA
members <- members[!duplicated(members$memID),]
members <- unique(members)
m <- members[logged_members,]
m <- m[(which(is.na(m$signupDate) == FALSE)),]
m <- m[(which(is.na(m$lastLogin) == FALSE)),]
m <- m[(which(is.na(m$signupDistressLevel) == FALSE)),]



s <- sample(1:length(m[,1]), length(m[,1])*0.75, replace=F)
train <- m[s,]
test <- m[setdiff(1:length(m[,1]),s),]
test <- na.omit(test)
cols <- c("coins","groupSupportMsgs","forumUpvotes","growthPoints","compassionHearts",
          "signupDate","lastLogin",
          "signupDistressLevel","numMsgUser","numLogins","convRequests","forumViews","helpViews",
          "pageViewsApp","pageViewsWeb","message_rate")

##ggplot theme: 
t <- theme(axis.text.x = element_blank(),
           axis.title.x=element_blank(), 
           axis.text.y = element_text(colour = 'black', size = 12), 
           axis.title.y = element_blank(),
           strip.text.y = element_text(size = 16, hjust = 0.5, vjust = 0.5, face = 'bold'),
           title = element_text(size=34),
           legend.position="none")
tyn <- theme(axis.text.x = element_blank(),
             axis.title.x=element_blank(), 
             axis.text.y = element_blank(),
             axis.title.y = element_blank(),
             strip.text.y = element_blank(),
             title = element_text(size=34),
             legend.position="none")

c <- ggplot(m[which(m$coins > 0 & m$message_rate > 0),],aes(x=coins, y=message_rate)) + geom_point(cex=3) +  geom_smooth(method="lm",col="red",cex=4)+  scale_y_continuous(limits=c(0.005,128),trans=log2_trans()) + scale_x_log10() +
  theme_bw() + t + ggtitle("Number of coins")

g <- ggplot(m[which(m$groupSupportMsgs > 0 & m$message_rate > 0),],aes(x=groupSupportMsgs, y=message_rate)) + geom_point(cex=3) +  geom_smooth(method="lm",col="red",cex=4)+  scale_y_continuous(limits=c(0.005,128),trans=log2_trans()) + scale_x_continuous(trans=log2_trans()) +  theme_bw() + 
  t + ggtitle("Number of group messages")

gr <- ggplot(m[which(m$growthPoints > 0 & m$message_rate > 0),],aes(x=growthPoints, y=message_rate)) + geom_point(cex=3) +  geom_smooth(method="lm",col="red",cex=4)+  scale_y_continuous(limits=c(0.005,128),trans=log2_trans()) + scale_x_continuous(trans=log2_trans()) +  theme_bw() + 
  tyn + ggtitle("Growth points")

ch <- ggplot(m[which(m$compassionHearts > 0 & m$message_rate > 0),],aes(x=compassionHearts, y=message_rate)) + geom_point(cex=3) +  geom_smooth(method="lm",col="red",cex=4)+  scale_y_continuous(limits=c(0.005,128),trans=log2_trans()) + scale_x_continuous(trans=log2_trans()) +  theme_bw() + 
  tyn + ggtitle("Compassion hearts")

su <- ggplot(m[which(m$signupDate > 0 & m$message_rate > 0),],aes(x=signupDate, y=message_rate)) + geom_point(cex=3) +  geom_smooth(method="lm",col="red",cex=4)+  scale_y_continuous(limits=c(0.005,128),trans=log2_trans()) + scale_x_continuous(trans=log2_trans()) +  theme_bw() + 
  tyn + ggtitle("Signup Date")

login <- ggplot(m[which(m$lastLogin > 0 & m$message_rate > 0),],aes(x=lastLogin, y=message_rate)) + geom_point(cex=3) +  geom_smooth(method="lm",col="red",cex=4)+  scale_y_continuous(limits=c(0.005,128),trans=log2_trans()) + scale_x_continuous(trans=log2_trans(),limits=c(1.402E09,1.416E09)) +  theme_bw() +
  tyn + ggtitle("Last login")

d <- ggplot(m[which(m$signupDistressLevel > 0 & m$message_rate > 0),],aes(x=signupDistressLevel, y=message_rate)) + geom_point(cex=3) +  geom_smooth(method="lm",col="red",cex=4)+  scale_y_continuous(limits=c(0.005,128),trans=log2_trans()) + scale_x_continuous(trans=log2_trans()) +  theme_bw() + 
  tyn + ggtitle("Distress level")

conv <- ggplot(m[which(m$convRequests > 0 & m$message_rate > 0),],aes(x=convRequests, y=message_rate)) + geom_point(cex=3) +  geom_smooth(method="lm",col="red",cex=4)+  geom_smooth(method="lm",col="red",cex=4) +  scale_y_continuous(limits=c(0.005,128),trans=log2_trans()) + scale_x_continuous(trans=log2_trans()) +  theme_bw() + 
  tyn + ggtitle("Conversation requests sent")

nlogin <- ggplot(m[which(m$numLogins > 0 & m$message_rate > 0),],aes(x=numLogins, y=message_rate)) + geom_point(cex=3) +  geom_smooth(method="lm",col="red",cex=4)+  geom_smooth(method="lm",col="red",cex=4) +  scale_y_continuous(limits=c(0.005,128),trans=log2_trans()) + scale_x_continuous(trans=log2_trans()) +  theme_bw() + 
  tyn + ggtitle("Number of Logins")

f <- ggplot(m[which(m$forumUpvotes > 0 & m$message_rate > 0),],aes(x=forumUpvotes, y=message_rate)) + geom_point(cex=3) +  geom_smooth(method="lm",col="red",cex=4)+  scale_y_continuous(limits=c(0.005,128),trans=log2_trans()) + scale_x_continuous(trans=log2_trans()) +  theme_bw() + 
  tyn + ggtitle("Number of forum upvotes")

fview <- ggplot(m[which(m$forumViews > 0 & m$message_rate > 0),],aes(x=forumViews, y=message_rate)) + geom_point(cex=3) +  geom_smooth(method="lm",col="red",cex=4)+  scale_y_continuous(limits=c(0.005,128),trans=log2_trans()) + scale_x_continuous(trans=log2_trans()) +  theme_bw() + 
  tyn + ggtitle("Number of forum views")

numPosts <- ggplot(m[which(m$numForumPosts > 0 & m$message_rate > 0),],aes(x=numForumPosts, y=message_rate)) + geom_point(cex=3) +  geom_smooth(method="lm",col="red",cex=4)+  scale_y_continuous(limits=c(0.005,128),trans=log2_trans()) + scale_x_continuous(trans=log2_trans()) +  theme_bw() +  
  tyn + ggtitle("Forum posts")
plot(numPosts)

aview <- ggplot(m[which(m$helpViews > 0 & m$message_rate > 0),],aes(x=helpViews, y=message_rate)) + geom_point(cex=3) +  geom_smooth(method="lm",col="red",cex=4)+  scale_y_continuous(limits=c(0.005,128),trans=log2_trans()) + scale_x_continuous(trans=log2_trans()) +  theme_bw() + 
  tyn + ggtitle("Number of help article views")

pvapp <- ggplot(m[which(m$pageViewsApp > 0 & m$message_rate > 0),],aes(x=pageViewsApp, y=message_rate)) + geom_point(cex=3) +  geom_smooth(method="lm",col="red",cex=4)+  scale_y_continuous(limits=c(0.005,128),trans=log2_trans()) + scale_x_continuous(trans=log2_trans()) +  theme_bw() + 
  tyn + ggtitle("Number of Pages viewed (iOS)")

pvweb <- ggplot(m[which(m$pageViewsWeb > 0 & m$message_rate > 0),],aes(x=pageViewsWeb, y=message_rate)) + geom_point(cex=3) +  geom_smooth(method="lm",col="red",cex=4)+  scale_y_continuous(limits=c(0.005,128),trans=log2_trans()) + scale_x_continuous(trans=log2_trans()) +  theme_bw() + 
  tyn + ggtitle("Number of pages viewed (web)")

#64x15 inches
pdf("2col.pdf", width=64, height=13)
multiplot(c,g,gr,ch,su,login,d,conv,nlogin,numPosts,f,fview,aview,pvapp,pvweb,cols=8)
dev.off()
#128x15 inches
pdf("scatter.pdf",width=64,height=13)
multiplot(c,g,gr,ch,su,login,d,conv,nlogin,numPosts,f,fview,aview,pvapp,pvweb,cols=8)
dev.off()
png("scatter.png",width = 6400,height=1300)
multiplot(c,g,gr,ch,su,login,d,conv,nlogin,numPosts,f,fview,aview,pvapp,pvweb,cols=8)
dev.off()
remove(c,g,gr,ch,su,login,d,conv,nlogin,numPosts,f,fview,aview,pvapp,pvweb,s,cols,t,tyn)
remove(m)

