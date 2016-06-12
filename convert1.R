
listz<-rep(0,length(listi))
listx<-rep(0,length(listi))
rightafter<-0
for(q in 1:length(listi))
{
  rightafter<-0
  burst<-0
  excecuted<-0
  p<-listi[q]
  number<-newdata$number[p]
  date<-0
  ct<-0
  c<-1
  for(i in 1:length(nmum$memID))
  {
    
    if(number==nmum$memID[i]){
      date[c]<-nmum$actDate[i]
      ct[c]<-nmum$actCount[i]
      c<-c+1
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
  min_date<-min(date)
  n_of_weeks<-(max(date)-min_date)/7
  n_int<-ceiling(n_of_weeks)
  plot_n<-n_int
  token<-0
  start<-min((date))
  n_start<-start+7
  
  c<-1
  for(i in 1:length(date))
  {
    if((b[i,2]<=(n_start))&&(b[i,2]>=(start)))
    {
      token[i]<-c
    }
    else
    {
      while(b[i,2]>n_start)
      {
        c<-c+1
        start<-n_start
        n_start<-n_start+7
        
      }
      token[i]<-c
    }
  }
  count<-rep(0,200)
  for(i in 1:length(token))
  {
    
    count[token[i]]<-count[token[i]]+ct[i]
  }
  
  
  count<-count[1:n_int]
  
  numberr<-1:n_int
  c_week<-(mark_date-min(date))/7
  if(c_week<1)
  {
    list[q]<--1
    next
  }
  if(length(count)>2)
  {
    m.pm<-cpt.mean(count,penalty='Manual', pen.value='(0.25)*log(n)',class=TRUE,test.stat="CUSUM",method = "AMOC")
    plot(m.pm,xlab="Time", ylabcp="Overall Act.")
    abline(v=c_week,lty=1,col="blue")
    excecuted<-1
  }
  MAX<-max(count)
  threshhold<- (0.15)*(MAX)
  
  if(((length(cpts(m.pm)))!=0))
  {
            if((excecuted==1))
            {
              for(i in 1:length(count))
              {
                if((count[i]>threshhold)&&(i<=c_week))
                {
                  burst<-1
                  if((i==(cpts(m.pm)))||((i+1)==(cpts(m.pm))))
                  {
                    rightafter<-1
                    
    }
  }
}
if((rightafter==0)&&(burst==1))
{
  listx[q]<-1
  print(q)
}
}
}
}
list_2<-which(listx==1) #list of numbers with changes, not rightafter burst


