###- Running the Markov chain occupancy model -###

#-Call over all functions-#
source("MarkovInclTPMmaker.R")

#-Preparing data-#
prep()

#-Subset the data (open bracket at own risk)-#
{##- Sort data into archetypal sets: s_n single household , c_n couple household , f_n family household h-house f-flat/room o-homeowner m-mortgage r-rent -##
  
  ## Single households split by tenure and accom
  s_1h<- tuscom[(tuscom$DVHsize==1 & tuscom$Accom==1) & (tuscom$ddayw==1),]
  s_1f<- tuscom[(tuscom$DVHsize==1 & tuscom$Accom>1) & (tuscom$ddayw==1),]
  s_1o<- tuscom[(tuscom$DVHsize==1 & tuscom$Tenure==1) & (tuscom$ddayw==1),]
  s_1m<- tuscom[(tuscom$DVHsize==1 & tuscom$Tenure==2) & (tuscom$ddayw==1),]
  s_1r<- tuscom[(tuscom$DVHsize==1 & tuscom$Tenure>3) & (tuscom$ddayw==1),]
  
  sss_1h<- tuscom[(tuscom$DVHsize==1 & tuscom$Accom==1) & (tuscom$ddayw>1),]
  sss_1f<- tuscom[(tuscom$DVHsize==1 & tuscom$Accom>1) & (tuscom$ddayw>1),]
  sss_1o<- tuscom[(tuscom$DVHsize==1 & tuscom$Tenure==1) & (tuscom$ddayw>1),]
  sss_1m<- tuscom[(tuscom$DVHsize==1 & tuscom$Tenure==2) & (tuscom$ddayw>1),]
  sss_1r<- tuscom[(tuscom$DVHsize==1 & tuscom$Tenure>3) & (tuscom$ddayw>1),]
  
  s_2h<- tuscom[(tuscom$DVHsize==2 & tuscom$Accom==1 & tuscom$NumChild==0) & (tuscom$ddayw==1),]
  s_2f<- tuscom[(tuscom$DVHsize==2 & tuscom$Accom>1 & tuscom$NumChild==0) & (tuscom$ddayw==1),]
  s_2o<- tuscom[(tuscom$DVHsize==2 & tuscom$Tenure==1 & tuscom$NumChild==0) & (tuscom$ddayw==1),]
  s_2m<- tuscom[(tuscom$DVHsize==2 & tuscom$Tenure==2 & tuscom$NumChild==0) & (tuscom$ddayw==1),]
  s_2r<- tuscom[(tuscom$DVHsize==2 & tuscom$Tenure>3 & tuscom$NumChild==0) & (tuscom$ddayw==1),]
  
  sss_2h<- tuscom[(tuscom$DVHsize==2 & tuscom$Accom==1 & tuscom$NumChild==0) & (tuscom$ddayw>1),]
  sss_2f<- tuscom[(tuscom$DVHsize==2 & tuscom$Accom>1 & tuscom$NumChild==0) & (tuscom$ddayw>1),]
  sss_2o<- tuscom[(tuscom$DVHsize==2 & tuscom$Tenure==1 & tuscom$NumChild==0) & (tuscom$ddayw>1),]
  sss_2m<- tuscom[(tuscom$DVHsize==2 & tuscom$Tenure==2 & tuscom$NumChild==0) & (tuscom$ddayw>1),]
  sss_2r<- tuscom[(tuscom$DVHsize==2 & tuscom$Tenure>3 & tuscom$NumChild==0) & (tuscom$ddayw>1),]
  
  s_3h<- tuscom[(tuscom$DVHsize==3 & tuscom$Accom==1 & tuscom$NumChild==0) & (tuscom$ddayw==1),]
  s_3f<- tuscom[(tuscom$DVHsize==3 & tuscom$Accom>1 & tuscom$NumChild==0) & (tuscom$ddayw==1),]
  s_3o<- tuscom[(tuscom$DVHsize==3 & tuscom$Tenure==1 & tuscom$NumChild==0) & (tuscom$ddayw==1),]
  s_3m<- tuscom[(tuscom$DVHsize==3 & tuscom$Tenure==2 & tuscom$NumChild==0) & (tuscom$ddayw==1),]
  s_3r<- tuscom[(tuscom$DVHsize==3 & tuscom$Tenure>3 & tuscom$NumChild==0) & (tuscom$ddayw==1),]
  
  sss_3h<- tuscom[(tuscom$DVHsize==3 & tuscom$Accom==1 & tuscom$NumChild==0) & (tuscom$ddayw>1),]
  sss_3f<- tuscom[(tuscom$DVHsize==3 & tuscom$Accom>1 & tuscom$NumChild==0) & (tuscom$ddayw>1),]
  sss_3o<- tuscom[(tuscom$DVHsize==3 & tuscom$Tenure==1 & tuscom$NumChild==0) & (tuscom$ddayw>1),]
  sss_3m<- tuscom[(tuscom$DVHsize==3 & tuscom$Tenure==2 & tuscom$NumChild==0) & (tuscom$ddayw>1),]
  sss_3r<- tuscom[(tuscom$DVHsize==3 & tuscom$Tenure>3 & tuscom$NumChild==0) & (tuscom$ddayw>1),]
  
  s_4o<- tuscom[(tuscom$DVHsize==4 & tuscom$Tenure==1 & tuscom$NumChild==0) & (tuscom$ddayw==1),]
  s_4m<- tuscom[(tuscom$DVHsize==4 & tuscom$Tenure==2 & tuscom$NumChild==0) & (tuscom$ddayw==1),]
  s_4r<- tuscom[(tuscom$DVHsize==4 & tuscom$Tenure>3 & tuscom$NumChild==0) & (tuscom$ddayw==1),]
  
  sss_4o<- tuscom[(tuscom$DVHsize==4 & tuscom$Tenure==1 & tuscom$NumChild==0) & (tuscom$ddayw>1),]
  sss_4m<- tuscom[(tuscom$DVHsize==4 & tuscom$Tenure==2 & tuscom$NumChild==0) & (tuscom$ddayw>1),]
  sss_4r<- tuscom[(tuscom$DVHsize==4 & tuscom$Tenure>3 & tuscom$NumChild==0) & (tuscom$ddayw>1),]
  
  ## Complete sets of single households
  s_1<- tuscom[(tuscom$DVHsize==1 & tuscom$NumChild==0),]
  s_2<- tuscom[(tuscom$DVHsize==2 & tuscom$NumChild==0),]
  s_3<- tuscom[(tuscom$DVHsize==3 & tuscom$NumChild==0),]
  s_4<- tuscom[(tuscom$DVHsize==4 & tuscom$NumChild==0),]
  s_5<- tuscom[(tuscom$DVHsize==5 & tuscom$NumChild==0),]  # Possibly too small a subset #
  
  ## Couples households
  c_0<- tuscom[((tuscom$NumCPart>0 | tuscom$NumMPart>0) & tuscom$DVHsize==2) & (tuscom$ddayw==1),]
  c_0h<- tuscom[((tuscom$NumCPart>0 | tuscom$NumMPart>0) & tuscom$DVHsize==2 & tuscom$Accom==1) & (tuscom$ddayw==1),]
  c_0f<- tuscom[((tuscom$NumCPart>0 | tuscom$NumMPart>0) & tuscom$DVHsize==2 & tuscom$Accom>1) & (tuscom$ddayw==1),]
  c_0o<- tuscom[((tuscom$NumCPart>0 | tuscom$NumMPart>0) & tuscom$DVHsize==2 & tuscom$Tenure==1) & (tuscom$ddayw==1),]
  c_0m<- tuscom[((tuscom$NumCPart>0 | tuscom$NumMPart>0) & tuscom$DVHsize==2 & tuscom$Tenure==2) & (tuscom$ddayw==1),]
  c_0r<- tuscom[((tuscom$NumCPart>0 | tuscom$NumMPart>0) & tuscom$DVHsize==2 & tuscom$Tenure>3) & (tuscom$ddayw==1),]
  
  ssc_0<- tuscom[((tuscom$NumCPart>0 | tuscom$NumMPart>0) & tuscom$DVHsize==2) & (tuscom$ddayw>1),]
  ssc_0h<- tuscom[((tuscom$NumCPart>0 | tuscom$NumMPart>0) & tuscom$DVHsize==2 & tuscom$Accom==1) & (tuscom$ddayw>1),]
  ssc_0f<- tuscom[((tuscom$NumCPart>0 | tuscom$NumMPart>0) & tuscom$DVHsize==2 & tuscom$Accom>1) & (tuscom$ddayw>1),]
  ssc_0o<- tuscom[((tuscom$NumCPart>0 | tuscom$NumMPart>0) & tuscom$DVHsize==2 & tuscom$Tenure==1) & (tuscom$ddayw>1),]
  ssc_0m<- tuscom[((tuscom$NumCPart>0 | tuscom$NumMPart>0) & tuscom$DVHsize==2 & tuscom$Tenure==2) & (tuscom$ddayw>1),]
  ssc_0r<- tuscom[((tuscom$NumCPart>0 | tuscom$NumMPart>0) & tuscom$DVHsize==2 & tuscom$Tenure>3) & (tuscom$ddayw>1),]
  
  ## Family households split by tenure and accom
  f_3o<- tuscom[(tuscom$DVHsize==3 & tuscom$Tenure==1 & tuscom$NumChild>0) & (tuscom$ddayw==1),]   # Possibly extremely small subset #
  f_3m<- tuscom[(tuscom$DVHsize==3 & tuscom$Tenure==2 & tuscom$NumChild>0) & (tuscom$ddayw==1),]
  f_3r<- tuscom[(tuscom$DVHsize==3 & tuscom$Tenure>3 & tuscom$NumChild>0) & (tuscom$ddayw==1),]
  
  ssf_3o<- tuscom[(tuscom$DVHsize==3 & tuscom$Tenure==1 & tuscom$NumChild>0) & (tuscom$ddayw>1),]   # Possibly extremely small subset #
  ssf_3m<- tuscom[(tuscom$DVHsize==3 & tuscom$Tenure==2 & tuscom$NumChild>0) & (tuscom$ddayw>1),]
  ssf_3r<- tuscom[(tuscom$DVHsize==3 & tuscom$Tenure>3 & tuscom$NumChild>0) & (tuscom$ddayw>1),]
  
  f_4o<- tuscom[(tuscom$DVHsize==4 & tuscom$Tenure==1 & tuscom$NumChild>0) & (tuscom$ddayw==1),]   # Possibly small subset #
  f_4m<- tuscom[(tuscom$DVHsize==4 & tuscom$Tenure==2 & tuscom$NumChild>0) & (tuscom$ddayw==1),]
  f_4r<- tuscom[(tuscom$DVHsize==4 & tuscom$Tenure>3 & tuscom$NumChild>0) & (tuscom$ddayw==1),]
  
  ssf_4o<- tuscom[(tuscom$DVHsize==4 & tuscom$Tenure==1 & tuscom$NumChild>0) & (tuscom$ddayw>1),]   # Possibly small subset #
  ssf_4m<- tuscom[(tuscom$DVHsize==4 & tuscom$Tenure==2 & tuscom$NumChild>0) & (tuscom$ddayw>1),]
  ssf_4r<- tuscom[(tuscom$DVHsize==4 & tuscom$Tenure>3 & tuscom$NumChild>0) & (tuscom$ddayw>1),]
  
  f_5o<- tuscom[(tuscom$DVHsize==5 & tuscom$Tenure==1 & tuscom$NumChild>0) & (tuscom$ddayw==1),]   # Possibly small subset #
  f_5m<- tuscom[(tuscom$DVHsize==5 & tuscom$Tenure==2 & tuscom$NumChild>0) & (tuscom$ddayw==1),]
  f_5r<- tuscom[(tuscom$DVHsize==5 & tuscom$Tenure>3 & tuscom$NumChild>0) & (tuscom$ddayw==1),]
  
  ssf_5o<- tuscom[(tuscom$DVHsize==5 & tuscom$Tenure==1 & tuscom$NumChild>0) & (tuscom$ddayw>1),]   # Possibly small subset #
  ssf_5m<- tuscom[(tuscom$DVHsize==5 & tuscom$Tenure==2 & tuscom$NumChild>0) & (tuscom$ddayw>1),]
  ssf_5r<- tuscom[(tuscom$DVHsize==5 & tuscom$Tenure>3 & tuscom$NumChild>0) & (tuscom$ddayw>1),]
  
  ## Complete sets of family households
  f_2<- tuscom[(tuscom$DVHsize==2 & tuscom$NumChild>0),]   # Single parent household #
  f_3<- tuscom[(tuscom$DVHsize==3 & tuscom$NumChild>0),]
  f_4<- tuscom[(tuscom$DVHsize==4 & tuscom$NumChild>0),]
  f_5<- tuscom[(tuscom$DVHsize==5 & tuscom$NumChild>0),]
  f_6<- tuscom[(tuscom$DVHsize==6 & tuscom$NumChild>0),]
}

#-Produce 10000 occupant profiles based on user input-#
cdstringc<-c("c_0h","c_0f","c_0o","c_0m","c_0r","ssc_0h","ssc_0f","ssc_0o","ssc_0m","ssc_0r")

cdstrings1<-c("s_1h","s_1f","s_1o","s_1m","s_1r","sss_1h","sss_1f","sss_1o","sss_1m","sss_1r")
cdstrings2<-c("s_2h","s_2f","s_2o","s_2m","s_2r","sss_2h","sss_2f","sss_2o","sss_2m","sss_2r")
cdstrings3<-c("s_3h","s_3f","s_3o","s_3m","s_3r","sss_3h","sss_3f","sss_3o","sss_3m","sss_3r")
cdstrings4<-c("s_4o","s_4m","s_4r","sss_4o","sss_4m","sss_4r")

cdstringf3<-c("f_3o","f_3m","f_3r","ssf_3o","ssf_3m","ssf_3r")
cdstringf4<-c("f_4o","f_4m","f_4r","ssf_4o","ssf_4m","ssf_4r")
cdstringf5<-c("f_5o","f_5m","f_5r","ssf_5o","ssf_5m","ssf_5r")
cdstring6<-c("s_2","f_4","c_0")


f_4allset<-lapply(cdstring,Andrey)
c_allset<-lapply(cdstringc,Andrey)
s_allset1<-lapply(cdstrings1,Andrey)
s_allset2<-lapply(cdstrings2,Andrey)
s_allset3<-lapply(cdstrings3,Andrey)
s_allset4<-lapply(cdstrings4,Andrey)

f_allset3<-lapply(cdstringf3,Andrey)
f_allset4<-lapply(cdstringf4,Andrey)
f_allset5<-lapply(cdstringf5,Andrey)

testset<-lapply(cdstring6,Andrey)

###- Presenting results and clustering -###

boc<-do.call("cbind",s_allset3)
boc[boc<2]<-0
boc[boc>1]<-1
boc<-boc[seq(0,145,2),]
bocmean<-rowMeans(boc)
boc<-data.frame(t(boc))
boc[is.na(boc)]<-0
qplot(x=c(1:144),y=bocmean,geom="line",xlab = "10min time steps",ylab = "Average Occupancy",ylim = c(0,1),xlim = c(0,140),main = "3-Person Singles' Household")+theme(plot.title = element_text(hjust = 0.5))

boc<-do.call("cbind",f_allset4)
boc[boc<2]<-0
boc[boc>1]<-1
boc<-boc[seq(0,145,2),]
bocmean<-rowMeans(boc)
boc<-data.frame(t(boc))
boc[is.na(boc)]<-0
boc<-boc[,-c(60:72)]
qplot(x=c(1:62),y=bocmean[1:62],geom="line",xlab = "20min time steps",ylab = "Average Occupancy",ylim = c(0,1),xlim = c(0,60),main = "4-Person Family Household")+theme(plot.title = element_text(hjust = 0.5))

boc<-do.call("cbind",c_allset)
boc[boc<2]<-0
boc[boc>1]<-1
boc<-boc[seq(0,145,2),]
bocmean<-rowMeans(boc)
boc<-data.frame(t(boc))
boc[is.na(boc)]<-0
boc<-boc[,-c(66:72)]
qplot(x=c(1:65),y=bocmean[1:65],geom="line",xlab = "20min time steps",ylab = "Average Occupancy",ylim = c(0,1),xlim = c(0,60),main = "Couples' Household")+theme(plot.title = element_text(hjust = 0.5))


exm1<-c_0set[[1]]
exm2<-c_0set[[2]]

boc<-data.frame(t(boco))

boc[boc<2]<-0
boc[boc>1]<-1
boco[boco<2]<-0
boco[boco>1]<-1

#- Optional Dimensionality decrease -#
boc<-boc[-145,]
boc<-boc[seq(0,145,3),]
corboc<-cor(boc[,1:72])
rboc<-findCorrelation(corboc, cutoff = 0.9, verbose = FALSE, names = FALSE)
boc<-boc[,-rboc]


#- Viewing various Results and Plots -#

boco<-f_allset4[[1]]
bocm<-f_allset4[[2]]
bocr<-f_allset4[[3]]

bocmean<-rowMeans(boc)
bocomean<-rowMeans(boco)
bocmmean<-rowMeans(bocm)
bocrmean<-rowMeans(bocr)

bocof<-mapply(as.factor,boco)
bocmf<-mapply(as.factor,bocm)
bocrf<-mapply(as.factor,bocr)

qplot(x=c(1:48),y=bocmean,geom="line")
plot(bocomean,type="l",xlab="Time Step",ylab = "f_4 owners' occupancy")
plot(bocmmean,type="l",xlab="Time Step",ylab = "f_4 mortgaged occupancy")
plot(bocrmean,type="l",xlab="Time Step",ylab = "f_4 renters' occupancy")

pie(c(475516,491026,444321,29137),labels = c("Out","Asleep","Home","Cooking"),main="Owners' time use")
pie(c(468171,498393,444038,29398),labels = c("Out","Asleep","Home","Cooking"),main="Mortgaged time use")
pie(c(460652,493524,455060,30764),labels = c("Out","Asleep","Home","Cooking"),main="Renters' time use")

bocof<-mapply(as.factor,boco)

#- K-means clustering tests -#

kclust2<-kmeans(boc,2)
kclust3<-kmeans(boc,3)
kclust4<-kmeans(boc,4,iter.max=50)
kclust5<-kmeans(boc,5,iter.max=50)
kclust6<-kmeans(boc,6,iter.max=50)
kclust7<-kmeans(boc,7,iter.max=50)
kclust8<-kmeans(boc,8,iter.max=50)
kclust9<-kmeans(boc,9,iter.max=50)
kclust10<-kmeans(boc,10,iter.max=50)
kclust11<-kmeans(boc,11,iter.max=50)
kclust12<-kmeans(boc,12,iter.max=50)
kclust13<-kmeans(boc,13,iter.max=50)
kclust14<-kmeans(boc,14,iter.max=50)
kclust15<-kmeans(boc,15,iter.max=50)
kclust16<-kmeans(boc,16,iter.max=50)
kclust17<-kmeans(boc,17,iter.max=50)
kclust18<-kmeans(boc,18,iter.max=50)
kclust19<-kmeans(boc,19,iter.max=50)
kclust20<-kmeans(boc,20,iter.max=50)
kclust21<-kmeans(boc,21,iter.max=50)
kclust22<-kmeans(boc,22,iter.max=50)

clusts<-list(kclust2,kclust3,kclust4,kclust5,kclust6,kclust7,kclust8,kclust9,kclust10,kclust11,kclust12,kclust13,kclust14,kclust15,kclust16,kclust17,kclust18,kclust19,kclust20,kclust21,kclust22)
withins<-c(kclust2$tot.withinss,kclust3$tot.withinss,kclust4$tot.withinss,kclust5$tot.withinss,kclust6$tot.withinss,kclust7$tot.withinss,kclust8$tot.withinss,kclust9$tot.withinss,kclust10$tot.withinss,kclust11$tot.withinss,kclust12$tot.withinss,kclust13$tot.withinss,kclust14$tot.withinss,kclust15$tot.withinss)

kmeansAIC = function(fit){
  
  m = ncol(fit$centers)
  n = length(fit$cluster)
  k = nrow(fit$centers)
  D = fit$tot.withinss
  return(D + 2*m*k)
}

kmeansAIC(kclust2)
AICs<-sapply(clusts,kmeansAIC)

plot(withins)
plot(AICs)

qplot(x=c(1:14),y=withins,xlab = "k, Amount of clusters",ylab = "Within cluster SS",main = "Couples' Household Clustering")+theme(plot.title = element_text(hjust = 0.5))
qplot(x=c(1:14),y=AICs[1:14],xlab = "k, Amount of clusters",ylab = "AIC",main = "Couples' Household Clustering")+theme(plot.title = element_text(hjust = 0.5))


##- Plot an average of each cluster -##
#example 6#
bocc<-cbind(boc,kclust5$cluster)
c61<-bocc[bocc$`kclust5$cluster`==1,]
c62<-bocc[bocc$`kclust5$cluster`==2,]
c63<-bocc[bocc$`kclust5$cluster`==3,]
c64<-bocc[bocc$`kclust5$cluster`==4,]
c65<-bocc[bocc$`kclust5$cluster`==5,]
c66<-bocc[bocc$`kclust6$cluster`==6,]

m61[] <- lapply(c61, coercetonumeric)# This may not be useful

m61<-as.vector(sapply(c61,mean))
m62<-as.vector(sapply(c62,mean))
m63<-as.vector(sapply(c63,mean))
m64<-as.vector(sapply(c64,mean))
m65<-as.vector(sapply(c65,mean))
m66<-as.vector(sapply(c66,mean))

qplot(x=c(1:65),y=m61[1:65],ylab="Estimated occupancy",xlab="20 min intervals",geom = "line",ylim = c(0,1))
qplot(x=c(1:65),y=m62[1:65],ylab="Estimated occupancy",xlab="20 min intervals",geom = "line",ylim = c(0,1))
qplot(x=c(1:65),y=m63[1:65],ylab="Estimated occupancy",xlab="20 min intervals",geom = "line",ylim = c(0,1))
qplot(x=c(1:65),y=m64[1:65],ylab="Estimated occupancy",xlab="20 min intervals",geom = "line",ylim = c(0,1))
qplot(x=c(1:65),y=m65[1:65],ylab="Estimated occupancy",xlab="20 min intervals",geom = "line",ylim = c(0,1))
plot(m66[1:57],ylab="Estimated occupancy",xlab="10 min intervals",type="l",lwd=2,ylim=c(0,1))




