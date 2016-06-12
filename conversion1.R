library(ggplot2)
library(grid)
library(gridExtra)
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

par(mfrow = c(4, 2))
par(mar=c(2.9,3,0,2.8)+1)
min_date<-min(date)
n_of_weeks<-(max(date)-min_date)/7
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

  
m.pm<-cpt.mean(count,penalty='Manual', pen.value='0.5*log(n)',class=TRUE,test.stat="CUSUM",method = "AMOC")
plot(m.pm,xlab="Time", ylab="Overall Act.")
abline(v=c_week,lty=1,col="blue")
}

rm(date,ct)
#activity 1 
number<- newdata$number[p]
date<-0
ct<-0
c<-1
i<-0
for(i in 1:length(actid1))
{
  
  if(number==actid1[i]){
    date[c]<-date1[i]
    ct[c]<-count_activity1[i]
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
while((min(date)>tmpd+6))
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
m.pm<-cpt.mean(count,penalty='Manual', pen.value='0.5*log(n)',class=TRUE,test.stat="CUSUM",method = "AMOC")
plot(m.pm,xlab="Time", ylab="Acc. login")
abline(v=c_week,lty=1,col="blue")
}
rm(date,ct)
}
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
m.pm<-cpt.mean(count,penalty='Manual', pen.value='0.5*log(n)',class=TRUE,test.stat="CUSUM",method = "AMOC")
plot(m.pm,xlab="Time", ylab="convMessage")
abline(v=c_week,lty=1,col="blue")
}

}
#activity 3
number<- newdata$number[p]
date<-0
ct<-0
c<-1
for(i in 1:length(actid3))
{
  
  if(number==actid3[i]){
    date[c]<-date3[i]
    ct[c]<-count_activity3[i]
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
      
      
    }
    token[i]<-c
  }
}
c<-1
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
m.pm<-cpt.mean(count,penalty='Manual', pen.value='0.5*log(n)',class=TRUE,test.stat="CUSUM",method = "AMOC")
plot(m.pm,xlab="Time", ylab="convReqpers.")
abline(v=c_week,lty=1,col="blue")
}

}
#activity 4
number<- newdata$number[p]
date<-0
ct<-0
c<-1
i<-0
for(i in 1:length(actid4))
{
  
  if(number==actid4[i]){
    date[c]<-date4[i]
    ct[c]<-count_activity4[i]
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
     
      
    }
    token[i]<-c
  }
}
c<-1
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
m.pm<-cpt.mean(count,penalty='Manual', pen.value='0.5*log(n)',class=TRUE,test.stat="CUSUM",method = "AMOC")
plot(m.pm,xlab="Time", ylab="convReqGen.")
abline(v=c_week,lty=1,col="blue")
}

}
#activity5
number<- newdata$number[p]
date<-0
ct<-0
c<-1
i<-0
for(i in 1:length(actid5))
{
  
  if(number==actid5[i]){
    date[c]<-date5[i]
    ct[c]<-count_activity5[i]
    c<-c+1
    print(i)
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
m.pm<-cpt.mean(count,penalty='Manual', pen.value='0.5*log(n)',class=TRUE,test.stat="CUSUM",method = "AMOC")
plot(m.pm,xlab="Time", ylab="forumPost")
abline(v=c_week,lty=1,col="blue")
}

}
#activity6
number<- newdata$number[p]
date<-0
ct<-0
c<-1
for(i in 1:length(actid6))
{
  
  if(number==actid6[i]){
    date[c]<-date6[i]
    ct[c]<-count_activity6[i]
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
m.pm<-cpt.mean(count,penalty='Manual', pen.value='0.5*log(n)',class=TRUE,test.stat="CUSUM",method = "AMOC")
plot(m.pm,xlab="Time", ylab="helpGuideView")
abline(v=c_week,lty=1,col="blue")
}

}

#activity 7
number<- newdata$number[p]
date<-0
ct<-0
c<-1
for(i in 1:length(actid7))
{
  
  if(number==actid7[i]){
    date[c]<-date7[i]
    ct[c]<-count_activity7[i]
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
m.pm<-cpt.mean(count,penalty='Manual', pen.value='0.5*log(n)',class=TRUE,test.stat="CUSUM",method = "AMOC")
plot(m.pm,xlab="Time", ylab="forumView")
abline(v=c_week,lty=1,col="blue")
}

}

#activity8
number<- newdata$number[p]
date<-0
ct<-0
c<-1
for(i in 1:length(actid8))
{
  
  if(number==actid8[i]){
    date[c]<-date8[i]
    ct[c]<-count_activity8[i]
    c<-c+1
    print(i)
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
m.pm<-cpt.mean(count,penalty='Manual', pen.value='0.5*log(n)',class=TRUE,test.stat="CUSUM",method = "AMOC")
plot(m.pm,xlab="Time", ylab="pageviewWeb")
abline(v=c_week,lty=1,col="blue")
}

}
#activity9
number<- newdata$number[p]
date<-0
ct<-0
c<-1
for(i in 1:length(actid9))
{
  
  if(number==actid9[i]){
    date[c]<-date9[i]
    ct[c]<-count_activity9[i]
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
m.pm<-cpt.mean(count,penalty='Manual', pen.value='0.5*log(n)',class=TRUE,test.stat="CUSUM",method = "AMOC")
plot(m.pm,xlab="Time", ylab="pageviewApp")
abline(v=c_week,lty=1,col="blue")

}


}
print(sum1)
print(sum2)
name<-paste(c(p,".pdf"), collapse = " ")
testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}
testit(5.7)
windows()
testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}
testit(5.7)
dev.copy2pdf(file=name,width=10, height=8,onefile=FALSE)
dev.off()



}

