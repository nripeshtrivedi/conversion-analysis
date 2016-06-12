# calculting the fraction of the time between change and conversion and the total time the user is active

library(ggplot2)
library(grid)
library(gridExtra)
library(changepoint)
fraction<-0
frac_list<-rep(0, length(listi))
list<-0
k<-1
for(q in 1:length(listi))
{
  p<-listi[q]
  sum1<-0
  sum2<-0
  number<-newdata$number[p]
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
    }
  }
  sum1<-sum(ct)
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
  
  par(mfrow = c(1, 2))
  par(mar=c(2.9,3,0,2.8)+1)
  min_date<-min(date)
  a_n_of_weeks<-(max(date)-min_date)/7
  n_int<-ceiling(n_of_weeks)
  
  plot_n<-n_int
  token<-0
  plots<-list()
  count_plot<-1
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
        print(n_start)
        
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
  b1<-data.frame(count, numberr)
  names(b1) <- c("number","times")
  c_week<-(mark_date-min(date))/7
  if(length(count)>2)
  {
    
    
    m.pm<-cpt.mean(count,penalty='Manual', pen.value='0.25*log(n)',class=TRUE,test.stat="CUSUM",method = "AMOC")
    plot(m.pm,xlab="Time", ylab="Overall Act.")
    abline(v=c_week,lty=1,col="blue")
  }
  
  rm(date,ct)
  
  #activity 2
  number<- newdata$number[p]
  date<-0
  ct<-0
  c<-1
  i<-0
  for(i in 1:length(actid2))
  {
    
    if(number==actid2[i]){
      date[c]<-date2[i]
      ct[c]<-count_activity2[i]
      c<-c+1
      
    }
  }
  sum2<-sum2+sum(ct)
  isin<-IDs$memID %in% number
  j<-which(isin==TRUE)
  lisid<-IDs[j,]$listID
  isin<-members$listID %in% lisid
  j<-which(isin==TRUE)
  date<- (as.POSIXct(date,origin = "1970-01-01"))
  date<-as.Date(date)
  b<-data.frame(ct, date)
  names(b) <- c("number","times")
  
  if(length(date)>1)
  {
    n_of_weeks<-(max(date)-min_date)/7
    n_int<-ceiling(n_of_weeks)
    token<-0
    start<-min((date))
    n_start<-start+7
    c<-1
    tmpd<-min_date
    while(min(date)>tmpd+6)
    {
      tmpd<-tmpd+7
      c<-c+1
    }
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
          print(n_start)
          
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
    b1<-data.frame(count, numberr)
    names(b1) <- c("number","times")
    if(length(count)>2)
    {
      m.pm<-cpt.mean(count,penalty='Manual', pen.value='0.25*log(n)',class=TRUE,test.stat="CUSUM",method = "AMOC")
      if(length(cpts(m.pm))>0)
      {
      frac_list[q]<-(as.numeric(c_week)-cpts(m.pm))/(as.numeric(a_n_of_weeks))
      print("x")
      list[k]<-q
      k<-k+1
      }
      plot(m.pm,xlab="Time", ylab="convMessage")
      abline(v=c_week,lty=1,col="blue")
    }
    
  }

}
  