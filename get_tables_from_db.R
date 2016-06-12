library(gWidgets)
library(RMySQL)
library(DBI)
library(dbConnect)

mydb=dbConnect(MySQL(),user='nripesh',password='rFceha@!95',dbname='7cot-fall15',host='130.108.85.104')


# Build the members table with the attributes for evaluating user engagement

#'"coins","groupSupportMsgs","forumUpvotes","growthPoints ","compassionHearts","signupDate","lastLogin",
#"signupDistressLevel",
members <- fetch(dbSendQuery(mydb,'select listID, coins, groupSupportMsgs,forumUpvotes,cheersPoints,xpPoints, compassionHearts,signupDateU as signupDate, lastLoginU as lastLogin, currentLevel from usersListeners'),n=-1)
actual_members<- fetch(dbSendQuery(mydb,'select memID, signupDateU as signupDate from usersMembers'),n=-1)
actual_guests<- fetch(dbSendQuery(mydb,'select guestID, guestCreatedDate as signupDate from usersGuests'),n=-1)
numconversation <- fetch(dbSendQuery(mydb,'select listID , numConversations from usersListeners group by listID'),n=-1)
for(i in 1:length(actual_members))
{
  if(members$listID[i] %in% actual_members$memID)
  {}
if(actual_members$signupDate<=meb)
}
isin <- members$listID %in% numconversation$listID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]) nmu[i] <- sum(numconversation$numConversations[which(numconversation$listID == members$listID[i])]) }
members$numconversation <- nmu
remove(nmu,i,isin)
gender <- fetch(dbSendQuery(mydb,'select listID , gender from usersListeners group by listID'),n=-1)
isin <- members$listID %in% gender$listID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]){ if((gender$gender[which(gender$listID == members$listID[i])])=='F'){ nmu[i] <- 1 }
else
  nmu[i] <- 0
}}
members$gender <- nmu
remove(nmu,i,isin)
gender <- fetch(dbSendQuery(mydb,'select listID , gender from usersListeners group by listID'),n=-1)
isin <- members$listID %in% gender$listID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]){ if((gender$gender[which(gender$listID == members$listID[i])])=='F'){ nmu[i] <- 1 }
                                       else
                                         nmu[i] <- 0
}}
members$gender <- nmu
remove(nmu,i,isin)
email <- fetch(dbSendQuery(mydb,'select listID , messageEmail from usersListeners group by listID'),n=-1)
isin <- members$listID %in% email$listID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]){ if((email$messageEmail[which(email$listID == members$listID[i])])=='never'){ nmu[i] <- 0 }
                                       else if(email$messageEmail[which(email$listID == members$listID[i])]=='weekly')
                                         nmu[i] <- 1
else if(email$messageEmail[which(email$listID == members$listID[i])]=='daily')
nmu[i] <- 2
else if(email$messageEmail[which(email$listID == members$listID[i])]=='immediately')
nmu[i] <- 3
else 
nmu[i] <- -1
}}
members$email <- nmu
remove(nmu,i,isin)
logged_members <- members$signupDate >= as.numeric(as.POSIXct("2014-05-07 00:00:00 EST")) & 
  members$signupDate <= as.numeric(as.POSIXct("2014-11-18 00:00:00 EST")) 

Activedays <- fetch(dbSendQuery(mydb,'select actDate, actUserID as userID, (count(DISTINCT actDate)) as numForumPosts from actionLogging group by actUserID'),n=-1)
isin <- members$listID %in% Activedays$userID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]) nmu[i] <- sum(Activedays$numForumPosts[which(Activedays$userID == members$listID[i])]) }
members$activedays <- nmu
remove(nmu,isin)
activeDays<-0
activeDays <- rep(0,53593)
isin <- m$listID %in% activity$actUserID
Activedays$actDate<-as.numeric(as.POSIXct(Activedays$actDate,'%Y-%m-%d'))
for(i in 1:length(m$listID)) 
  {
  if(isin[i])
  {
  if(activity$actDate[i] <= (m$signupDate[i] + 60*60*24*7*2) )
  {
    activeDays[activity$actUserID[i]]=activeDays[activity$actUserID[i]]+1
  print( Activedays[i,3])
  }
  }
}
  



onlineswitch <- fetch(dbSendQuery(mydb,'select listID as userID, onlineSwitch from usersListeners group by listID'),n=-1)
isin <- members$listID %in% onlineswitch$userID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]) nmu[i] <- sum(onlineswitch$onlineSwitch[which(onlineswitch$userID == members$listID[i])]) }
members$onlineswitch <- nmu
remove(nmu,isin)

#"numMsgUser",
clink <- fetch(dbSendQuery(mydb,'select userID, sum(numMsgUser) as numMsgUser from conversationLink where userType="l" group by userID'),n=-1)
isin <- members$listID %in% clink$userID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]) nmu[i] <- clink$numMsgUser[which(clink$userID == members$listID[i])] }
members$numMsgUser <- nmu
remove(nmu,isin,clink)

#numForumPosts
forumpost <- fetch(dbSendQuery(mydb,'select actDate, actUserID as userID, sum(actCount) as numForumPosts from actionLogging where action="forumPost" group by actUserID,actDate'),n=-1)
isin <- members$listID %in% forumpost$userID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]) nmu[i] <- sum(forumpost$numForumPosts[which(forumpost$userID == members$listID[i])]) }
members$numForumPosts <- nmu
remove(nmu,i,isin)

#acctLogins
logins <- fetch(dbSendQuery(mydb,'select actDate, actUserID as userID, sum(actCount) as numForumPosts from actionLogging where action="acctLogin" group by actUserID,actDate'),n=-1)
isin <- members$listID %in% logins$userID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]) nmu[i] <- sum(logins$numForumPosts[which(logins$userID == members$listID[i])]) }
members$numLogins <- nmu
remove(nmu,isin)

#Conversation Requests ('convRequestGeneral','convRequestPersonal')
convReq <- fetch(dbSendQuery(mydb,'select actDate, actUserID as userID, sum(actCount) as convRequests from actionLogging where action="convRequestGeneral" or action="convRequestPersonal" group by actUserID,actDate'),n=-1)
isin <- members$listID %in% convReq$userID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]) nmu[i] <- sum(convReq$convRequests[which(convReq$userID == members$listID[i])]) }
members$convRequests <- nmu
remove(nmu,isin)

#''forumView',
forumViews <- fetch(dbSendQuery(mydb,'select actDate, actUserID as userID, sum(actCount) as views from actionLogging where action="forumView" group by actUserID,actDate'),n=-1)
isin <- members$listID %in% forumViews$userID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]) nmu[i] <- sum(forumViews$views[which(forumViews$userID == members$listID[i])]) }
members$forumViews <- nmu
remove(nmu,isin)

#''helpGuideView',
helpViews <- fetch(dbSendQuery(mydb,'select actDate, actUserID as userID, sum(actCount) as views from actionLogging where action="helpGuideView" group by actUserID,actDate'),n=-1)
isin <- members$listID %in% helpViews$userID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]) nmu[i] <- sum(helpViews$views[which(helpViews$userID == members$listID[i])]) }
members$helpViews <- nmu
remove(nmu,isin)

#''pageviewApp',
pageViewsApp <- fetch(dbSendQuery(mydb,'select actDate, actUserID as userID, sum(actCount) as views from actionLogging where action="pageviewApp" group by actUserID,actDate'),n=-1)
isin <- members$listID %in% pageViewsApp$userID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]) nmu[i] <- sum(pageViewsApp$views[which(pageViewsApp$userID == members$listID[i])]) }
members$pageViewsApp <- nmu
remove(nmu,isin)

#'pageviewWeb'
pageViewsWeb <- fetch(dbSendQuery(mydb,'select actDate, actUserID as userID, sum(actCount) as views from actionLogging where action="pageviewWeb" group by actUserID,actDate'),n=-1)
isin <- members$listID %in% pageViewsWeb$userID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]) nmu[i] <- sum(pageViewsWeb$views[which(pageViewsWeb$userID == members$listID[i])]) }
members$pageViewsWeb <- nmu
remove(nmu,isin)
OfflineMessages <- fetch(dbSendQuery(mydb,'select listID, offlineMsgCount from usersListeners group by listID'),n=-1)
isin <- members$listID %in% onlineswitch$listID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]) nmu[i] <- sum(OfflineMessages$offlineMsgCount[which(offlineMsgCount$listID == members$listID[i])]) }
members$offlineMsgCount <- nmu
AccountLive <- fetch(dbSendQuery(mydb,'select listID, AccountLive from usersListeners group by listID'),n=-1)
isin <- members$listID %in% AccountLive$listID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]) nmu[i] <- sum(AccountLive$AccountLive[which(AccountLive$listID == members$listID[i])]) }
members$AccountLive <- nmu
remove(nmu,isin)
ProfileCompleteU <- fetch(dbSendQuery(mydb,'select listID, ProfileCompleteU from usersListeners group by listID'),n=-1)
isin <- members$listID %in% ProfileCompleteU$listID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]) {nmu[i]<-ProfileCompleteU$ProfileCompleteU[which(ProfileCompleteU$listID == members$listID[i])]} 
}
members$ProfileCompleteU <- nmu
remove(nmu,isin)

trainingCompleteU <- fetch(dbSendQuery(mydb,'select listID, trainingCompleteU from usersListeners group by listID'),n=-1)
isin <- members$listID %in% trainingCompleteU$listID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]) {if (!is.na(trainingCompleteU$trainingCompleteU[which(trainingCompleteU$listID == members$listID[i])])){nmu[i]<-trainingCompleteU$trainingCompleteU[which(trainingCompleteU$listID == members$listID[i])]} 
else
{
  nmu[i]<-0
}
}
}
members$trainingCompleteU <- nmu
remove(nmu,isin)
Rating <- fetch(dbSendQuery(mydb,'select listID, R1, R2, R3, R4 from ListenerRatings '),n=-1)
isin <- members$listID %in% Rating$listID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]) {n1 <- sum(Rating$R1[which(Rating$listID == members$listID[i])]) 
                                       n2 <- sum(Rating$R2[which(Rating$listID == members$listID[i])])
                                       n3 <- sum(Rating$R3[which(Rating$listID == members$listID[i])])
                                       n4 <- sum(Rating$R4[which(Rating$listID == members$listID[i])])
                                       nmu[i]<- n1+n2+n3+n4;

}
                                      
}
for(i in 1:length(nmu))
{
  if(is.na(nmu[i]))
    nmu[i]<-0
}
members$Rating <- nmu
remove(nmu,isin)
VisitedChat <- fetch(dbSendQuery(mydb,'select listID, visitedChat from usersListeners '),n=-1)
isin <- members$listID %in% VisitedChat$listID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]) nmu[i] <- sum(VisitedChat$visitedChat[which(VisitedChat$listID == members$listID[i])]) }
members$visitedChat <- nmu
remove(nmu,isin)
VisitedForum <- fetch(dbSendQuery(mydb,'select listID, visitedForum from usersListeners '),n=-1)
isin <- members$listID %in% VisitedForum$listID
nmu <- rep(0,length(isin))
for(i in 1:length(isin)){ if(isin[i]) nmu[i] <- sum(VisitedForum$visitedForum[which(VisitedForum$listID == members$listID[i])]) }
members$VisitedForum <- nmu
remove(nmu,isin)
#"conversation_rate"
date_diff <- as.numeric(
  as.Date(as.character("10/16/2015"), format="%m/%d/%Y") -
    as.Date(as.character("5/7/2013"), format="%m/%d/%Y")
)
members$message_rate <- members$numMsgUser/date_diff

remove(date_diff,i)