k<-which(activity$actUserID==312562)
for(i in 1:length(k))
{
  if((activity[k[i],]$action=='convRequestGeneral')||(activity[k[i],]$action=='convRequestPersonal'))
  {
    
    mark_date<-(as.POSIXct(activity[k[i],]$actDate,origin = "1970-01-01"))
    mark_date<-as.Date(mark_date)
    print(mark_date)
  }
}
k<-which(Conversation_IDs$userID==312562)
for(i in 1:length(k))
{
  mark_date<-(as.POSIXct(Conversation_IDs[k[i],]$reqDate,origin = "1970-01-01"))
  mark_date<-as.Date(mark_date)
  print(mark_date)
}
