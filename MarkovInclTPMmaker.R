###---Data import and FUNCTIONS!---###

#- Import TUS tab files -#
tusraw<-read.table(file="tuswide.tab",sep="\t",header = TRUE)
hhraw<-read.table(file="hhdata.tab",sep="\t",header = TRUE)

#- Trim down data -#
tusraw<-tusraw[c(1,19,32:751)]
hhraw<-hhraw[c("serial","NumChild","NumCPart","NumMPart","DVHsize","Tenure","Accom","WorkSta_P1","WorkSta_P2","WorkSta_P3","WorkSta_P4","WorkSta_P5","WorkSta_P6")]
tusloc<-data.frame(tusraw)
#-Translating location and activity to binary/simple numeric-#
ishome<-function(time){
  var<-paste("wher_",time,sep="")
  eval(parse(text=paste("tusloc$",var,"[tusloc$",var,"!=11]<<-0",sep="")))
  eval(parse(text=paste("tusloc$",var,"[tusloc$",var,"==11]<<-1",sep="")))
  
}

whatdo<-function(time){
  testvar<-paste("wher_",time,sep="")
  var<-paste("act1_",time,sep="")
  eval(parse(text=paste(
    "tusloc$",var,"[tusloc$",testvar,"==0]<<-0","\n",
    "tusloc$",var,"[tusloc$",var,"==110]<<-1","\n",
    "tusloc$",var,"[tusloc$",var,"==111]<<-1","\n",
    "tusloc$",var,"[tusloc$",var,"==112]<<-1","\n",
    "tusloc$",var,"[tusloc$",var,"==3110]<<-3","\n",
    
    "tusloc$",var,"[((tusloc$",var,"!=1) & (tusloc$",var,"!=3) & (tusloc$",var,"!=0))]<<-2","\n",
    sep="")))
  
}

#-Get probs for start state-#
getstartstate<-function(tdata){
  tot<-length(tdata$act1_1)
  tpm<-vector(length = 4,mode = "numeric")
  tpm[1]<-length(tdata$act1_1[tdata$act1_1==0])/tot
  tpm[2]<-length(tdata$act1_1[tdata$act1_1==1])/tot
  tpm[3]<-length(tdata$act1_1[tdata$act1_1==2])/tot
  tpm[4]<-length(tdata$act1_1[tdata$act1_1==3])/tot
  return(tpm)
}

#-Making the tpm step by step-#
getstateprob<-function(time,tdata){
  var<-paste("act1_",time,sep="")
  eval(parse(text=paste("varcol<-tdata$",var)))
  prevar<-paste("act1_",time-1,sep="")
  eval(parse(text=paste("prevarcol<-tdata$",prevar)))
  tot<-length(varcol)
  tpm<-vector(length = 16,mode = 'numeric')
  tpm[1]<-length(varcol[(varcol==0 & prevarcol==0)])/length(varcol[prevarcol==0])
  tpm[2]<-length(varcol[(varcol==1 & prevarcol==0)])/length(varcol[prevarcol==0])
  tpm[3]<-length(varcol[(varcol==2 & prevarcol==0)])/length(varcol[prevarcol==0])
  tpm[4]<-length(varcol[(varcol==3 & prevarcol==0)])/length(varcol[prevarcol==0])
  
  tpm[5]<-length(varcol[(varcol==0 & prevarcol==1)])/length(varcol[prevarcol==1])
  tpm[6]<-length(varcol[(varcol==1 & prevarcol==1)])/length(varcol[prevarcol==1])
  tpm[7]<-length(varcol[(varcol==2 & prevarcol==1)])/length(varcol[prevarcol==1])
  tpm[8]<-length(varcol[(varcol==3 & prevarcol==1)])/length(varcol[prevarcol==1])
  
  tpm[9]<-length(varcol[(varcol==0 & prevarcol==2)])/length(varcol[prevarcol==2])
  tpm[10]<-length(varcol[(varcol==1 & prevarcol==2)])/length(varcol[prevarcol==2])
  tpm[11]<-length(varcol[(varcol==2 & prevarcol==2)])/length(varcol[prevarcol==2])
  tpm[12]<-length(varcol[(varcol==3 & prevarcol==2)])/length(varcol[prevarcol==2])
  
  tpm[13]<-length(varcol[(varcol==0 & prevarcol==3)])/length(varcol[prevarcol==3])
  tpm[14]<-length(varcol[(varcol==1 & prevarcol==3)])/length(varcol[prevarcol==3])
  tpm[15]<-length(varcol[(varcol==2 & prevarcol==3)])/length(varcol[prevarcol==3])
  tpm[16]<-length(varcol[(varcol==3 & prevarcol==3)])/length(varcol[prevarcol==3])
  return(tpm)
}

#-Calculate next state based on previous one-#
calcnextstate<-function(prevstate,statenum,transprob){
  transprob2<-transprob[transprob$t==statenum,]
  transprob3<-transprob2[transprob2$a==prevstate,]
  nextstateprobs<-as.vector(transprob3$framelesstpm)
  nextstate<-sample(0:3,size=1,replace=TRUE,prob=nextstateprobs)
  return(nextstate)
}

#-Coerce dataframe to numeric values-#
coercetonumeric<-function(x) {
  as.numeric(as.character(x)) 
}

coercetofactor<-function(x) {
  return((as.factor(x))) 
}

#-Loop to create occupancy profile
occprofile<-function(step,tpm){
  sstateprobs<-getstartstate(choosedata)
  sstate<-sample(0:3,size=1,replace=TRUE,prob=sstateprobs)
  occ<-vector(mode="integer",length=144)
  occ[1]<-sstate
  prevstate=occ[1]
  for(i in 1:143){
    statenum=i
    nextstate=calcnextstate(prevstate = prevstate,statenum = statenum, transprob = tpm)
    occ[i+1]=nextstate
    prevstate=nextstate
    rm(nextstate)
  }
  #print(step)
  bckup<-occ[133:144]
  occ<-shift(occ,n=12)
  occ[1:12]<-bckup
  return(occ)
}


###-Producing an entire occupancy profile-###

#-Data prep and manipulation-#

prep<-function(){
  times<-c(1:144)
  
  sapply(times,ishome)
  sapply(times,whatdo)
  
  tuscom<<-merge(hhraw,tusloc,by="serial")
  
  ##- Create TPM, frame for TPM and attaching TPM to frame -##
  tpmframe<<-data.frame(t=integer(length=2288),a=integer(length = 2288),b=integer(length = 2288))
  tpmframe$t<<-rep(1:143,each=16)
  tpmframe$a<<-rep(0:3,143,each=4)
  tpmframe$b<<-seq(0,3)
}


Andrey<-function(cd){
  #-input data from user-#
  #inputline<-readline(prompt="select data:")
  eval(parse(text=paste("choosedata<<-",cd,sep="")))
  
  tpmstep<-c(2:144)
  framelesstpm<-unlist(lapply(tpmstep,getstateprob,tdata=choosedata))
  
  completetpm<-cbind(tpmframe,framelesstpm)
  
  ##- Running many occupant profiles -##
  
  bareoccs<-sapply(c(1:1000),occprofile,tpm=completetpm)
  return(bareoccs)
}

Andreyweek<-function(cd){
  #-input data from user-#
  #inputline<-readline(prompt="select data:")
  eval(parse(text=paste("choosedata<<-",cd,sep="")))
  
  tpmstep<-c(2:144)
  framelesstpm<-unlist(lapply(tpmstep,getstateprob,tdata=choosedata))
  
  completetpm<-cbind(tpmframe,framelesstpm)
  
  ##- Running many occupant profiles -##
  
  bareoccs<-occprofile(1,tpm=completetpm)
  return(bareoccs)
}

Andreyweekend<-function(cd){
  #-input data from user-#
  #inputline<-readline(prompt="select data:")
  eval(parse(text=paste("choosedata<<-ss",cd,sep="")))
  
  tpmstep<-c(2:144)
  framelesstpm<-unlist(lapply(tpmstep,getstateprob,tdata=choosedata))
  
  completetpm<-cbind(tpmframe,framelesstpm)
  
  ##- Running many occupant profiles -##
  
  bareoccs<-occprofile(1,tpm=completetpm)
  return(bareoccs)
}

spliterweek<-function(data){
  eval(parse(text=paste("data<-",data,sep="")))
  week<-data[data$ddayw==1,]
  return(week)
}

spliterweekend<-function(data){
  eval(parse(text=paste("data<-",data,sep="")))
  weekend<<-data[data$ddayw>1,]
  return(weekend)
}

###- End of main script -###





# Spare lines #


