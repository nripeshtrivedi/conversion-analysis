

  
  mydb=dbConnect(MySQL(),user='nripesh',password='rFceha@!95',dbname='7cot-fall15',host='130.108.85.104')
  IDS <- fetch(dbSendQuery(mydb,'select memID, listID from actionUsers'),n = -1)
  IDs <- na.omit(IDS)
  isin <- IDs$listID %in% activity$actUserID
  nmu<-0
  nmu$listID <-0
  nmu$action<-0
  nmu$actCount<-0
  nmu$actDate<-0
  isin <- activity$actUserID %in% IDs$listID 
  c<-0
  for(i in 1:length(isin)){ if (isin[i])
      {
    nmu$listID[c]<-activity$actUserID[i]
    nmu$action[c]<-activity$action[i]
    nmu$actCount[c]<-activity$actCount[i]
    nmu$actDate[c]<-activity$actDate[i]
  c<-c+1  
  }
  
  }
  isin <- IDs$memID %in% activity$actUserID
  nmum<-0
  nmum$memID <-0
  nmum$action<-0
  nmum$actCount<-0
  nmum$actDate<-0
  isin <- activity$actUserID %in% IDs$memID 
  c<-0
  for(i in 1:length(isin)){ if (isin[i]) #making a data frame of activities of members that were also listners
  {
    nmum$memID[c]<-activity$actUserID[i]
    nmum$action[c]<-activity$action[i]
    nmum$actCount[c]<-activity$actCount[i]
    nmum$actDate[c]<-activity$actDate[i]
    c<-c+1  
  }
  
  }
 
  
  c<-0
  k<-1
  order<-0
  new_list<-unique(alist_id) #genrating individual order of members
  for(i in 1:length(new_list))
  {
    tmp_id<-new_list[i]

   c<-length(which(nmum$memID==tmp_id))
    print(i)
    order[k]<-c
    k<-k+1
  }
  }

  df1<-data.frame(new_list,order)
  names(df1)<-c("number","order")
  row_sub = apply(df1, 1, function(row) all(row >=11 ))
  df2<-df1[row_sub,]
  newdata <- df2 [order(df2$order),]
 
  listi<-0 # making list of members with substantial run and conversion to listner during the same
  u<-0
  b<-0
  z<-0
  for(i in 1:length(newdata$number))
  {
    number<- newdata$number[i]
    date<-0
    ct<-0
    c<-1
    u<-0
    b<-0
    isin<-IDs$memID %in% number
    j<-which(isin==TRUE)
    lisid<-IDs[j,]$listID
    isin<-members$listID %in% lisid
    j<-which(isin==TRUE)
    if(length(j)==0)
    {
      next
    }
    mark<-members$signupDate[j]
    if(length(mark)==0)
      next
    for(k in 1:length(nmum$memID))
    {
      
      if(number==nmum$memID[k]){
        date[c]<-nmum$actDate[k]
        ct[c]<-nmum$actCount[k]
        c<-c+1
        print(k)
      }
    }
    for(l in 1:length(date))
    {
      if(mark<date[l])
      {
        b=1
      }
      if(mark>date[l])
      {
        u=1
      }
      if((b==1)&&(u==1))
      {
        listi[z]<-i
        z<-z+1
        print("z")
        print("i")
        print(i)
        b<-0
        u<-0
        break
      }
    }
  }
  
  #--------------------------------------------------------------------------------------
  
  number<- newdata$number[12]
  date<-0
  ct<-0
  c<-1
  i<-0
  for(i in 1:length(nmum$memID))
  {
   
    if(number==nmum$memID[i]){
      date[c]<-nmum$actDate[i]
      ct[c]<-nmum$actCount[i]
      c<-c+1
      print(i)
    }
  }
  isin<-IDs$memID %in% number
  j<-which(isin==TRUE)
  lisid<-IDs[j,]$listID
  isin<-members$listID %in% lisid
  j<-which(isin==TRUE)
  date<- (as.POSIXct(date,origin = "1970-01-01"))
  date<-as.Date(date)
  mark_date<- (as.POSIXct(members$signupDate[j],origin = "1970-01-01"))
  mark_date<-as.Date(mark_date)
b<-data.frame(ct, date)
  names(b) <- c("number","times")
  print(mark_date)
 

