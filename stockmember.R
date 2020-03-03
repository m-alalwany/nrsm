##############################################################
#####- SCRIPT TO CREATE A MEMBER OF THE SYNTHETIC STOCK -#####
##############################################################

Synrun <- function(Model, Vars){

#####- SELECTING IDF -#####  
  
  idfspath<-file.path("E:/localstockcode/idfs")
  idfs<-list.files(path=idfspath,pattern="\\.idf$")  
  idfprobs=c(0.0202 , 0.0202 , 0.0202 , 0.007, 0.02 , 0.005, 0.007 , 0.02 , 0.005 , 0.02, 0.0017, 0.011 , 0.011 , 0.0017 , 0.011 , 0.005 , 0.008 , 0.008 , 0.005, 0.008, 0.032 , 0.032 , 0.032, 0.0014, 0.008 , 0.003 , 0.006 , 0.001 , 0.003 , 0.006 , 0.001 , 0.0024 , 0.001 ,  0.001 , 0.003 , 0.007 , 0.003 , 0.007 , 0.0075 , 0.017 , 0.025 , 0.0075 , 0.017 , 0.025 , 0.016 , 0.009 , 0.016 , 0.004 , 0.004 , 0.09 , 0.07 , 0.017 , 0.0015 , 0.009 , 0.002 , 0.0015 , 0.008 , 0.002 , 0.0015 , 0.008 , 0.007 , 0.009 , 0.01 , 0.005 , 0.04 , 0.016 , 0.001 , 0.01 , 0.025 , 0.005 , 0.0015 , 0.008 , 0.0015 , 0.007 , 0.0015 , 0.0015 , 0.007 , 0.03 , 0.03 , 0.03)
  idfprobs<-idfprobs/sum(idfprobs)
  cumidfprobs<-cumsum(idfprobs)
  idfsample<-findInterval(HC[Model,1],cumidfprobs)+1
  idf<-idfs[idfsample]
  
#####- Deriving typology number and Data import -#####
  
  idflabel<-gsub(".idf","",idf)
  styp<-gsub("[A-Z]","",idflabel)
  styp<-substring(styp,1,2)
  styp<<-as.numeric(gsub("_", "", styp))
  idftable<-paste(idflabel,"table.csv",sep="")  
  source("E:/localstockcode/DataImport.R")
  
#####- PERMEABILITY SAMPLING -#####

  averagessteph<-c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)
  steph<-c(0.00390625, 0.05078125, 0.048177083, 0.063802083, 0.071614583,  0.078125, 0.065104167, 0.05078125,0.032552083,0.022135417,0.005208333,0.002604167,0.001302083,0.00390625,0)
  stephdist<-t(rbind(averagessteph, steph))
  smoothsteph<-spline(averagessteph,steph, n=15*length(averagessteph), method="fmm")
  
  permeabilitysteph<-sample(smoothsteph$x,1,prob = smoothsteph$y,replace = TRUE)
  
  averagespan<-c(1,3,5,7,9,11,13,15,17)
  pan<-c(0.0976, 0.2648, 0.5993, 0.7038, 0.2927, 0.0209, 0.0139, 0.0000 ,0.0070)
  pan<-pan/2
  pandist<-spline(averagespan, pan, n=20*length(averagespan), method="fmm")
  pandist$y[pandist$y<0]<-0
  permeabilitypan<-sample(pandist$x,1,prob = pandist$y,replace = TRUE)
  
  if(styp<12){
    permability<<-permeabilitysteph
  }else{
    permability<<-permeabilitypan
  }
  permability<-mean(permability)
  
#####- MATERIAL DEFINITION -#####

material_name <- c("EPS Expanded Polystyrene (Standard)- thickness 0.1","XPS Extruded Polystyrene  - CO2 Blowing- thickness 0.0795", "! EPS 10 mm- thickness 0.01")
roof_name <- c("! Board insulation", "Min wool quilt,  100 mm- thickness 0.1")

#####- READING IDF FILE -#####

idftoread<-paste(idfspath,"/",idf,sep="")
idf_model <- readLines(idftoread)
idf_model <- gsub("\t","", idf_model)
    
divider1 <- grep("LinearBridgingLayer,", idf_model)
divider2 <- grep("ZoneCapacitanceMultiplier", idf_model)
divider3 <- grep("Schedule:Year, Zone Control Type Sched, Control Type,", idf_model)
divider4 <- grep("Output:Diagnostics, DisplayAdvancedReportVariables;", idf_model)

partA_idf <- idf_model[1:(divider1[1])]
partB_idf <- idf_model[(divider1+1):(divider2)]
partC_idf <- idf_model[(divider2+1):divider3]
partD_idf <- idf_model[(divider3+1):divider4]

#####- PART A - IDF ID AND AREA/VOLUME EXTRACTION -#####

partA_idf[1] <- paste("!"," ", "model_", Model,sep="")

Get_area<-function(line){
  return(as.numeric(substr(gsub(",", "", idf_model[line]), 1, 20)) )
}
Get_area2<-function(line){
  if(substr(gsub(":", "", gsub(",","  ",idf_model[line+3])), 7, 10)=="Wall"){
    return(as.numeric(gsub("[a-z]","",substr(gsub("-", "", gsub("m2","  ",idf_model[line])), 28, 36))))
  }else{return(0)}
}

###- Get zone volumes and surface areas -###

V_line <- grep("Zone volume", idf_model)
areas<-vector()
areas<- sapply(V_line, Get_area)
zonevolume<-sum(unlist(areas))

Get_all_surfaces<-function(inp_table){
  baloon<-readLines(inp_table)
  G_line <- grep(",Gross Wall Area", baloon)
  G_line=G_line[1]
  area=strsplit(baloon[G_line], split=",")
  area=unlist(area)
  area=area[3]
  area<-as.numeric(area)
  R_line <- grep(",Gross Roof Area", baloon)
  rarea=strsplit(baloon[R_line], split=",")
  rarea=unlist(rarea)
  rarea=rarea[3]
  rarea<-as.numeric(rarea)
  return(rarea)
}

##- Exposed walls -##

G_line <- grep("WindExposed,", idf_model)
G_line<-G_line-7
adareas<-vector()
adareas<- lapply(G_line, Get_area2)
adia_area<-sum(unlist(adareas))

##- Areas from table output -##

stockoutputfile<-file.path("E:/localstockcode/idfs",idftable)
wall_area<-Get_all_surfaces(stockoutputfile)
exparea<-as.numeric(wall_area)

##- Compiling areas and volumes -##

exparea50<-mean(adia_area)+exparea
AV<-mean(exparea50/zonevolume)
vol<-zonevolume

#####- PART B - MATERIALS -#####

###- WALLS -###

if(Vars[1]==1) {
  MaterialEdit <- function(name, partB_idf) {
    inst.cdf <- ecdf(inst)
    thickness <<- quantile(inst.cdf, probs=HC[Model,2], names=FALSE )
    wall_insinput<<-thickness/100
    if (thickness<30){wall_insinput<<-0.001}
    B_line <- grep(name, partB_idf)
    partB_idf[B_line+4] <<- paste("    ",as.character(wall_insinput),",",sep="")
    rm(B_line);
  }
  sapply(material_name, MaterialEdit, partB_idf=partB_idf)
}

###- GLAZING -###

B_line <- grep("WindowMaterial:SimpleGlazingSystem,", partB_idf)

  GlazingEdit <- function(x, partB_idf) {
    Uval.cdf <- ecdf(Uval)
    UvalG <<- quantile(Uval.cdf, probs=HC[Model,3], names=FALSE )
    partB_idf[x+2] <<- paste("    ",as.character(UvalG),",",sep="");
    CODERUNNING<<-1
  }
  sapply(B_line, GlazingEdit, partB_idf=partB_idf)


###- SHGC -###

GlazingEditSHGC <- function(x, partB_idf) {
  SHGC.cdf <- ecdf(SHGC)
  solar.SHGC <<- quantile(SHGC.cdf, probs=HC[Model,4], names=FALSE )
  partB_idf[x+3] <<- paste("    ",as.character(solar.SHGC),",",sep="");
}

if(Vars[3]==1) {
  sapply(B_line, GlazingEditSHGC, partB_idf=partB_idf)
}
rm(B_line)

#####- PART C - BUILDING ORIENTATION AND INFILTRATION -#####

###- Orientation -###

C_line <- grep("Building,", partC_idf)

if(Vars[4]==1) {
  orient.cdf <- ecdf(allign)
  orient.syn <<- quantile(orient.cdf, probs=HC[Model,5], names=FALSE ) 
  orient.syn<-round(orient.syn)
  partC_idf[C_line[1]+1] <- paste("    ",as.character(orient.syn),",",sep="");
}

rm(C_line)

###- Infiltration offload to schedules -###

InfEdit <- function(x, partC_idf){
  partC_idf[x+2] <<- paste("    ",as.character("DOMVENTsched,"),sep="")
  partC_idf[x+3] <<- paste("    ",as.character("AirChanges/Hour,"),sep="")
  partC_idf[x+7] <<- paste("    ",as.character("10"),"," ,sep="");
}

C_line <- grep("ZoneInfiltration:DesignFlowRate,", ignore.case = FALSE, partC_idf)
if(Vars[5]==1) {
sapply(C_line, InfEdit, partC_idf=partC_idf)
}

rm(C_line)

#####--  PART D: HEATING SYSTEMS --#####

###- Binary schedules -###

scdbinname <- "bin"
scdbinnamec <- "binc"
scdname.cdf <- ecdf(scdid)
scdname <<- quantile(scdname.cdf, probs=HC[Model,6], names=FALSE )
#hr<<-sample(hrd,1,replace=TRUE )
hi.cdf<-ecdf(hi)
his<<-quantile(hi.cdf, probs=HC[Model,7], names=FALSE )

Schededit <- function(x, partD_idf){
  if(substr(partD_idf[x],2,2)!= " "){
    scdname <<- round(scdname)
    scdprefix <- substr(partD_idf[x],0,67)
    partD_idf[x] <<- paste(scdprefix, "scdn",",c",";",sep="")
  }  
}

Schededitbin <- function(x, partD_idf){
  partD_idf[x+14] <<- paste("bin",",",sep="")
  partD_idf[x+15] <<- paste("binc",",",sep="")
}

##- Placing binary schedules -##

if(Vars[7]==1) {
  D_line <- grep("ThermostatSetpoint:DualSetpoint,", ignore.case = FALSE, partD_idf)
  sapply(D_line, Schededit, partD_idf=partD_idf)
  rm(D_line)
  D_line <- grep("Ideal Loads Air Name", ignore.case = FALSE, partD_idf)
  sapply(D_line, Schededitbin, partD_idf=partD_idf)

}

###-Radiator Sizing-###

D_line <- grep("Ideal Loads Air Name", partD_idf)
if(Vars[7]==1) {
  SizingEdit <- function(x, partD_idf) {
    RadSize <<- 1250
    
    partD_idf[x+10] <<- paste("    ",as.character(RadSize),",",sep="");
  }
  
  
  
  sapply(D_line, SizingEdit, partD_idf=partD_idf)
}
rm(D_line)

#####- WEATHER FILE HANDLING -#####

tempdata<- read.csv("E:/localstockcode/Nottingham_TRY.csv")
tempext<-tempdata$DryT
tempext<-aggregate(tempext,list(0:(length(tempext)-1) %/% 24),mean)
tempext<-tempext$x
tempext<- tempext[1:(length(tempext)-5)]
tempext<-aggregate(tempext,list(0:(length(tempext)-1) %/% 30),mean)
tempext<-tempext$x
setpoint=scdname
setback=14
deltaT<-setpoint-tempext
deltaT[deltaT<0]<-0

windext<-tempdata$WS
windext<-aggregate(windext,list(0:(length(windext)-1) %/% 24),mean)
windext<-windext$x
windext<- windext[1:(length(windext)-5)]
windext<-aggregate(windext,list(0:(length(windext)-1) %/% 30),mean)
wind<-windext$x
wind<-wind*(log(3/0.3)/log(10/0.05))

#####- TEMPERATURE SCHEDULE MAKER -#####

scdstart=c("Schedule:Compact,", "scdn,", "Temperature,")
scdwinter11=c("Through: 29 Feb,", "For: Weekdays WinterDesignDay SummerDesignDay,")
scdwinter12=c( "For: Weekends Holidays CustomDay1 CustomDay2,")
scdsummer1=c("Through: 15 Aug,", "For: Weekdays SummerDesignDay WinterDesignDay,")
scdsummer2=c( "For: Weekends Holidays CustomDay1 CustomDay2,")
scdwinter21=c("Through: 31 Dec,", "For: Weekdays WinterDesignDay,")
scdwinter22=c( "For: Weekends Holidays,")
scdendline=c("For: AllOtherDays,", "Until: 24:00, 0;")

hours=c("Until: 01:00, ", "Until: 02:00, ", "Until: 03:00, ", "Until: 04:00, ", "Until: 05:00, ",  "Until: 06:00, ", "Until: 07:00, ", "Until: 08:00, ", "Until: 09:00, ", "Until: 10:00, ", "Until: 11:00, ", "Until: 12:00,", "Until: 13:00, ", "Until: 14:00, ", "Until: 15:00, ", "Until: 16:00, ", "Until: 17:00, ", "Until: 18:00, ", "Until: 19:00, ", "Until: 20:00, ", "Until: 21:00, ", "Until: 22:00, ", "Until: 23:00, ", "Until: 24:00, ")
scdcounter=c(1:24)

popdist<-ecdf(pop)
poptypdist<-ecdf(poptyp)
hhpop<-quantile(popdist, probs=HC[Model,8], names=FALSE )
hhpoptyp<-quantile(poptypdist, probs=HC[Model,9], names=FALSE )
hhpop[hhpop>5]<-5

cdstringc<-c("c_0o","c_0m","c_0r")
cdstrings1<-c("s_1o","s_1m","s_1r")
cdstrings2<-c("s_2o","s_2m","s_2r")
cdstrings3<-c("s_3o","s_3m","s_3r")
cdstrings4<-c("s_4o","s_4m","s_4r")
cdstringf3<-c("f_3o","f_3m","f_3r")
cdstringf4<-c("f_4o","f_4m","f_4r")
cdstringf5<-c("f_5o","f_5m","f_5r")

whichstring<-switch(hhpop,
                    cdstrings1,
                    switch(hhpoptyp,cdstringc,cdstringc,cdstrings2),
                    switch(hhpoptyp,cdstringf3,cdstringf3,cdstrings3),
                    switch(hhpoptyp,cdstringf4,cdstringf4,cdstrings4),
                    cdstringf5)

occtype<-sample(x = whichstring,size = 1,replace = TRUE,prob = c(0.2,0.4,0.4))

scdcontentswriter2<-function(count){
  occlist<-rep(occtype,hhpop)
  premarkovch<-sapply(occlist,Andreyweekend)
  premarkovch[premarkovch<2]<-0
  markovch<-rowSums(premarkovch)
  markovch[markovch>0]<-setpoint
  markovch[markovch==0]<-setback
  tempweekend<-markovch
  out=c(hours[count], tempweekend[count], ",")
  out=paste(out,collapse="")
  scdcontents2<<-c(scdcontents2,out)
}

scdcontentswriter<-function(count){
  occlist<-rep(occtype,hhpop)
  premarkovch<-sapply(occlist,Andreyweek)
  premarkovch[premarkovch<2]<-0
  markovch<-rowSums(premarkovch)
  markovch[markovch>0]<-setpoint
  markovch[markovch==0]<-setback
  temp<-markovch
  out=c(hours[count], temp[count], ",")
  out=paste(out,collapse="")
  scdcontents<<-c(scdcontents,out)
}

setpoint<-scdname 
scdfinal<-scdstart

#-Winter 1
scdfinal<-c(scdfinal, scdwinter11)

###- Winter 1 Schedule Prep -###
scdcontents=vector(length=0)
scdcontents2=vector(length=0)
lapply(scdcounter, scdcontentswriter) 
write.table(c(scdcontents), file = "scdlog.csv", sep = ",", col.names = FALSE, row.names= FALSE, append=TRUE)

###- Winter 1 Schedule write -###
scdfinal<-c(scdfinal, scdcontents)
rm(scdcontents)
scdcontents=vector(length=0)
scdfinal<-c(scdfinal, scdwinter12)
lapply(scdcounter, scdcontentswriter2)
scdfinal<-c(scdfinal, scdcontents2)
rm(scdcontents2)
scdcontents2=vector(length=0)

#-Summer-#
scdfinal<-c(scdfinal, scdsummer1)

###- Summer Schedule Prep -###
lapply(scdcounter, scdcontentswriter)

###- Summer Schedule Write -###
scdfinal<-c(scdfinal, scdcontents)
rm(scdcontents)
scdcontents=vector(length=0)
scdfinal<-c(scdfinal, scdsummer2)
lapply(scdcounter, scdcontentswriter2)
scdfinal<-c(scdfinal, scdcontents2)
rm(scdcontents2)
scdcontents2=vector(length=0)

#-Winter 2-#
scdfinal<-c(scdfinal, scdwinter21)

###- Winter 2 Schedule Prep -###

lapply(scdcounter, scdcontentswriter) 

###- Winter 2 Schedule write -###
scdfinal<-c(scdfinal, scdcontents)
rm(scdcontents)
scdcontents=vector(length=0)
scdfinal<-c(scdfinal, scdwinter22)
lapply(scdcounter, scdcontentswriter2)
scdfinal<-c(scdfinal, scdcontents2)
rm(scdcontents2)
scdcontents2=vector(length=0)
scdfinal<-c(scdfinal, scdendline)

partC_idf<- append(partC_idf, scdfinal, after=(length(partC_idf)-1))

#####- Roof insulation -#####

if(Vars[8]==1) {
  MaterialEditroof <- function(name, partB_idf) {
    roofins.cdf <- ecdf(rit)
    roof <<- quantile(roofins.cdf, probs=HC[Model,10], names=FALSE ) 
    if (roof>=75 & roof<250){roof<-0.15}
    else if (roof<75 & roof>0){roof<-0.05}
    else if (roof == 0) {roof<-0.000001}
    else if (roof>=250){roof<-0.3}
    B_line <- grep(name, partB_idf)
    partB_idf[B_line+4] <<- paste("   ",as.character(roof),",",sep="")
    rm(B_line)
    }
  sapply(roof_name, MaterialEditroof, partB_idf=partB_idf)
}

#####- INFILTRATION SCHEDULE MAKER -#####

scdstartinf=c("Schedule:Compact,", "DOMVENTsched,", "Any Number,")
scdendlineinf=c("For: AllOtherDays,", "Until: 24:00, 0;")

AA50<-1
fan<-0.4
exparea4<-exparea50
deltaT<-deltaT
vol<-vol
permability<-permability
av<-AV
b<-rnorm(1,mean=0.65,sd=0.08)

infinputs<-data.frame(av,vol,deltaT,wind,permability,b,exparea4,AA50,fan)

if(styp==1|styp==9|styp==13){
  clean_model<-svmflat
  model_used<-"flat"
}else if(styp==2|styp==5|styp==7|styp==10|styp==14){
  clean_model<-svmtmid
  model_used<-"tmid"
}else if(styp==3|styp==11|styp==15){
  clean_model<-svmsemi
  model_used<-"semi"
}else if(styp==4|styp==6|styp==8|styp==12|styp==16){
  clean_model<-svmdet
  model_used<-"det"
}else{clean_model<-svmsemi
model_used<-"emergency"}

baloon<-predict(clean_model, infinputs)
baloon<-(baloon)/10

hoursinf=c("Through: 1/31,", "Through: 2/29,", "Through: 3/31,", "Through: 4/30,", "Through: 5/31,",  "Through: 6/30,", "Through: 7/31,", "Through: 8/31,", "Through: 9/30,", "Through: 10/31,", "Through: 11/30,", "Through: 12/31,")
scdcounter=c(1:12)

scdcontentswriterinf=function(count){
  baloon2<-paste("For: Alldays,", "Until: 24:00,", baloon[count])
  out=c(hoursinf[count], baloon2, ",")
  out=paste(out,collapse="")
  scdcontentsinf<<-c(scdcontentsinf,out)
}

inf.syn<-baloon*10
scdfinalinf<-scdstartinf

scdcontentsinf=vector(length=0)
lapply(scdcounter, scdcontentswriterinf) 

scdfinalinf<-c(scdfinalinf, scdcontentsinf)
scdfinalinf<-c(scdfinalinf, scdendlineinf)

partC_idf<- append(partC_idf, scdfinalinf, after=(length(partC_idf)-1))

#################################################
#####- RECOMPILING IDF AND EPLUS EXECUTION -#####
#################################################

write(c(idf, "Model Number:", Model, as.character(Sys.time()),"Insulation:", wall_insinput,"Orientation:", orient.syn,"Infiltration:", inf.syn, "Heating:", scdname,"Roof Insulation:", roof), file="log.txt", append=TRUE, sep="\n")

##- Recreate model -##

assign(paste("model_", Model,sep=""),c(partA_idf,partB_idf,partC_idf)) 
file.create(paste("model_", Model,".idf",sep=""))
fileConn <- file(paste("model_", Model,".idf",sep=""))
writeLines(c(partA_idf,partB_idf,partC_idf,partD_idf), fileConn)
close(fileConn)
rm(partA_idf,partB_idf,partC_idf,partD_idf)

##- simulate Model -##

system(paste('"D:\\EnergyPlusV8-3-0\\RunEPlus"'," ","model_", Model, " ", "GBR_Nottingham_CIBSE-TRY", sep=""))
csv=readLines(paste("model_",Model,"Table.csv", sep=""))
outputline = grep("Total End Uses", csv)
outputline = csv[outputline]
outputline = gsub(",Total End Uses,","",outputline)
outputline = as.numeric(unlist(strsplit(outputline,",")))
output= outputline[5]

write(c("Output:", as.character(GetEnergySyn(Model))), file="log.txt", append=TRUE, sep="\n")
subt <- sapply(strsplit(idf, ".", fixed=TRUE), "[", 1)
csvline<- c(idf, idflabel, subt, Model, wall_insinput, orient.syn,UvalG, solar.SHGC, scdname, roof,exparea50, vol, permability, mean(inf.syn),occtype, GetEnergySyn(Model))
FF <- as.matrix(t(csvline))
write.table(FF, file = "log.csv", sep = ",", 
            col.names = FALSE, row.names= FALSE, append=TRUE)


return(output)
}  

#############################################################
###- BONUS FUNCTION!- Gets energy usage after simulation -###
#############################################################

GetEnergySyn <- function(Model){
  csv=readLines(paste("Model_",Model,"Table.csv", sep=""))
  outputline = grep("Total Site Energy", csv)
  outputline = csv[outputline]
  outputline = gsub(",Total Site Energy,","",outputline)
  outputline = as.numeric(unlist(strsplit(outputline,",")))
  #outputline = max(outputline)
  uvalline=grep("Construction,Reflectance,U-Factor with Film",csv)
  uvalline=uvalline+1
  uvalline=csv[uvalline]
  uvalline = as.numeric(unlist(strsplit(uvalline,",")))
  uvalline=uvalline[5]
  return(c(outputline,uvalline))
}