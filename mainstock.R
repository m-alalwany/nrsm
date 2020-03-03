#################################################
#####- Main script to make synthetic stock -#####
#################################################

##- Ensures Numbers are not printed in engineering form -##

options(scipen=999)

#####- Call all functions and files -#####

source('E:/localstockcode/stockmember.R')
source("E:/localstockcode/MarkovInclTPMmaker.R")
load("E:/localstockcode/svmmods.rdata")

#####- Prep for Occ model -#####
prep()
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

#####- Define size of batch and simulation parameters -#####

ssize<- 500
Model <- (1:ssize)
Vars=c(1,1,1,1,1,1,1,1,1,1)


#####- Synthetic Stock Starts- make log file -#####

setwd("E:/localstockcode/run4")
file.create("log.txt")
LogWriter <- file("log.txt")

#####- Creates the latin hypercube -#####

HC <<- randomLHS(ssize, 10)

#####- Runs the script initially -#####
cl<-makeCluster(1)

Results<- parLapply(cl,Model,Synrun,Vars=Vars)

Results <- lapply(Model, Synrun, Vars=Vars)

multirun<-function(run){
  wdname<-paste("E:/localstockcode/run",run,sep = "")
  dir.create(wdname)
  setwd(wdname)
  file.create("log.txt")
  LogWriter <- file("log.txt")
  ssize<- 50
  Model <- (1:ssize)
  Vars=c(1,1,1,1,1,1,1,1,1,1)
  HC <<- randomLHS(ssize, 10)
  HC <<- randomLHS(ssize, 10)
  HC <<- randomLHS(ssize, 10)
  HC <<- randomLHS(ssize, 10)
  Results <- lapply(Model, Synrun, Vars=Vars)
  return(Results)
  
}

mresults<-sapply(c(5:20),multirun)


 ##- Tidy up steps -##
##- Redifines some of the variables for the next runs -##
Results <- unlist(Results)
StockMean <- mean(Results)
StockMedian <- median(Results)
StockVar <- var(Results)
Model <- ssize

##- Writes averages to log file -##
write(c("Averages:", as.character(StockMean), as.character(StockMedian)), file="log.txt", append=TRUE, sep="\n")

#-INSERT BOUNDARY CONDITION SCRIPT-#

#####- END -#####

resav<<-cumsum(results)/seq_along(results)
varav<-var(resav)

scann<<-c(1:60)
resav50<-resav[seq(50,3800,by=50)]
scanner<-function(num){
  num2=num+1
  diffs<-(resav50[num]-resav50[num2])/resav50[num]
  return(diffs*100)
}
resdiff<-sapply(scann,scanner)



sscan<-(seq(50,3800,by=50))

getvars<-function(inp){
  return(sqrt((var(results[1:inp]))))
}
scanner2<-function(num){
  num2=num+1
  diffs<-(vars[num]-vars[num2])/vars[num]
  return(diffs*100)
}
vars<-sapply(sscan,getvars)
vdiffs<-sapply(scann,scanner2)

###############
#- Graveyard -#
###############

#- repeat loop until the P-value is outside the boundary -#
p.value=0.5
ssize <- 1
##- New Hypercube for next runs, 500 long just incase -##
HC <- randomLHS(500, 8)

##-Loop-##
repeat{
  Model <- Model + 1
  NewResults <- c(Results, Synrun(Model, Vars, idf))
  NewStockVar <- var(Results)
  stopcond <- t.test(Results, NewResults, ratio = 1,
                     alternative = "two.sided",
                     conf.level = 0.95)
  p.value <- stopcond$p.value
  Results= NewResults
  StockMean <- mean(Results)
  StockMedian <- median(Results)
  write(c("Averages:", as.character(StockMean), as.character(StockMedian)), file="log.txt", append=TRUE, sep="\n")
  
  
  if(p.value > 0.975 || p.value <0.025){break}
}
t500<-c(1:500)
bla1<-sapply(t500,GetEnergySynxo)
setwd("E:/localstockcode/run2")
bla2<-sapply(t500,GetEnergySynxo)
setwd("E:/localstockcode/run3")
bla3<-sapply(t500,GetEnergySynxo)
setwd("E:/localstockcode/run4")
bla4<-sapply(t500,GetEnergySynxo)


