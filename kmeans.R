library(rgl)
list_distress<-rep(0,length(list))
list_d<-0
listd<-0
c<-1
for(q in 1:length(list))
{
  p<-listi[list[q]]
  number<-newdata$number[p]
  l<-which(distresslevel$memID==number)
  if(length(l)>0)
  {
  list_distress[q]<-distresslevel$signupDistressLevel[l]
  list_d[c]<-p
  listd[c]<-list[q]
  c<-c+1
  }
}
c<-1
list_final<-0
listfinal<-0
for(q in 1:length(listd))
{
  if(RankList[listd[q]]>0)
  {
    p<-list_d[q]
    list_final[c]<-p
    listfinal[c]<-listd[q]
    c<-c+1
  }
}
for(j in 1:length(listfinal))
{
  p<-listfinal[j]
  print(RankList[[p]])
}
list_distress_final<-0
ranks_final<-0
frac_final_list<-0
list_distress_final_1<-0
ranks_final_1<-0
frac_final_list_1<-0
c1<-1
c2<-1
store_fit1<-0
store_fit<-0
for(q in 1:length(listfinal))
{
  p<-list_final[q]
  p1<-listfinal[q]
  number<-newdata$number[p]
  l<-which(distresslevel$memID==number)
  if(frac_list[p1]>0)
  {
  list_distress_final[c1]<-distresslevel$signupDistressLevel[l]
  ranks_final[c1]<-RankList[p1]
  frac_final_list[c1]<-frac_list[p1]
  c1<-c1+1
  store_fit[c1]<-p1
  }
  else
  {
    list_distress_final_1[c2]<-distresslevel$signupDistressLevel[l]
    ranks_final_1[c2]<-RankList[p1]
    frac_final_list_1[c2]<-frac_list[p1]
    c2<-c2+1
    store_fit1[c2]<-p1
  }
}
ranks<-unlist(ranks_final)
mydata<-data.frame(list_distress_final, ranks, frac_final_list)
ranks_1<-unlist(ranks_final_1)
mydata_1<-data.frame(list_distress_final_1, ranks_1, frac_final_list_1)
data.matrix(mydata)
data.matrix(mydata_1)
fit <- kmeans(mydata, 2)
fit1 <- kmeans(mydata_1, 2)
par(mar = rep(2, 4))
plot(mydata, col=fit$cluster)

set.seed(123)
k.max <- 15 # Maximal number of clusters
wss <- sapply(1:k.max, 
              function(k){kmeans(mydata, k, nstart=10 )$tot.withinss})

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)

fitall<-0


for(q in 1:length(list_final))
{
  
  p<-list_final[q]
  print(newdata$number[p])
}
  
