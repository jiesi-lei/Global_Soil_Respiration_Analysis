source("0_functions_plot.R")
res.data <- read.csv("02_sampledataset.csv",check.names=FALSE) 
temp.data<- read.csv("temp_extract_result.csv",check.names=FALSE)
precip.data<- read.csv("precip_extract_result.csv",check.names=FALSE)

###--------------------------------------------------------------------
res.data = select(res.data,Record_number,Study_midyear,YearsOfData,
                  Latitude,Longitude,Site2,Study_number,Elevation,
                  Biome,Ecosystem_state,Ecosystem_type,Leaf_habit,
                  MAT,MAP,Meas_method,Partition_method,Stage,
                  Rs_annual,Site.match,Manipulation)
res.data = filter(res.data,Manipulation == "None")
res.data2 = na.omit(res.data[,c('Record_number','Site.match','Study_midyear','YearsOfData')])
#######Temp & Precip
workingdata = filter(res.data2, Site.match %in% temp.data$Site )
workingdata$Site.match = as.character(workingdata$Site)
temp.data$Site.match = as.character(temp.data$Site)
precip.data$Site.match = as.character(precip.data$Site)
attach(workingdata)

Mean.Temp. <- Mean.Temp.30y <- temp.list.all <- c()
Mean.Precip. <- Mean.Precip.30y <- Stdev.Precip.30y <- precip.list.all<-c()
Record_num = c()
i=j=1
for (i in 1:nrow(workingdata)){
  start.p = (Study_midyear[i]-1960)*12-round(12*YearsOfData[i]/2)+2
  end.p = (Study_midyear[i]-1960)*12+round(12*YearsOfData[i]/2)+1
  row.p = which(temp.data$Site == workingdata[i,]$Site.match)
  #temp
  temp.list = as.numeric(temp.data[row.p,c(start.p:end.p)])
  for (j in 1:30){
  temp.list.all[j] = mean(as.numeric(temp.data[row.p,c((12*j+314):(12*j+325))]))
  }
  Mean.Temp.[i] = mean (temp.list)
  Mean.Temp.30y[i]=mean(as.numeric(temp.data[row.p,-c(1:325,686:697)]))

  
  #precip
  precip.list = as.numeric(precip.data[row.p,c(start.p:end.p)])
  Mean.Precip.[i] = sum(precip.list)/(length(temp.list)/12)
  Record_num [i] = Record_number [i]
  for (j in 1:30){
    precip.list.all[j] = mean(as.numeric(precip.data[row.p,c((12*j+314):(12*j+325))]))
  }
  Mean.Precip.30y[i]=mean(as.numeric(precip.data[row.p,-c(1:325,686:697)]))
}
delta.Mean.Temp.= Mean.Temp. - Mean.Temp.30y
delta.Mean.Precip.= 12*(Mean.Precip. - Mean.Precip.30y)
Result3=data.frame(Record_num,Mean.Temp.,delta.Mean.Temp.,Mean.Precip.,delta.Mean.Precip.)
rm(Record_num,Mean.Temp.,Mean.Temp.30y,
   Mean.Precip.,Mean.Precip.30y)
rm(i,start.p,end.p,row.p,temp.list,precip.list)
detach(workingdata)

###merge--------------------
Result = merge(res.data,Result3,by.x = 'Record_number',by.y = 'Record_num',all = TRUE)

Result$MAT[which(is.na(Result$MAT))] <- Result$Mean.Temp.[which(is.na(Result$MAT))]
Result$MAP[which(is.na(Result$MAP))] <- Result$Mean.Precip.[which(is.na(Result$MAP))]
Result$Latitude2 = Result$Latitude
Result$Latitude = abs(Result$Latitude)


write.csv(Result,"03_processed_data.csv",row.names=FALSE)

#before running this section, mannually delete abnormal record(latitude = 59 with ecosystem type "Subtropical") and records belonging to extremely rare ecosystems
#manually add a column "altitude" with altitudes extracted from GPS visulizer, replace NAs with those
#mannually add a column "SOC_stock" with values eatracted from soilgrids
res.data = read.csv(file.choose(),check.names=FALSE)#input dataset with altitude and SOC information
res.data$Elevation[which(is.na(res.data$Elevation))]<-res.data$altitude[which(is.na(res.data$Elevation))]
res.data$Ecosystem_type2 = as.character(res.data$Ecosystem_type)
res.data$Ecosystem_type2[which(res.data$Ecosystem_type2 == 'Deciduous Forest, Evergreen Forest, Mixed Forest')] = "Forest"
working.data <- filter(res.data, !is.na(SOC_stock))
ecosys.med <- aggregate(working.data[,c(26)], list(working.data$Ecosystem_type), median)
for (i in 1:(nrow(res.data))){
  if (is.na(res.data[i,26]) == TRUE)
    res.data[i,26] = ecosys.med[which(ecosys.med$Group.1 == res.data[i,11]),2]
}
summary(res.data$SOC_stock)

#Outliers detection
res.data <- filter(res.data, Meas_method %in% c('IRGA','Gas Chromatography')&Ecosystem_type != 'Agriculture')
res.data <- droplevels(res.data)
dv = c('Rs_annual')
idv = c('Study_midyear','Latitude','Elevation',
        'MAT','MAP','ΔMean.Temp.',
        'ΔMean.Precip.','Biome',
        'Meas_method','Ecosystem_type2','Partition_method','Stage','SOC_stock')
work.data = na.omit(work.data[,c(idv,dv)])
boreal = filter(work.data, Biome =="Boreal" | Biome == 'Arctic')
Temperate = filter(work.data, Biome == "Temperate"|Biome == 'Mediterranean')
Tropical = filter(work.data, Biome == "Tropical" | Biome =="Subtropical")

sp=boxplot(boreal$Rs_annual,boxwex=0.7)
title("Boxplot for outlier detection")
xi=1.1
sd.s=sd(boreal[complete.cases(boreal),]$"Rs_annual")
mn.s=mean(boreal[complete.cases(boreal),]$"Rs_annual")
points(xi,mn.s,col="red",pch=18)
arrows(xi, mn.s - 3*sd.s, xi, mn.s + 3*sd.s, code = 3, col = "pink", angle = 75, length = .1)
text(rep(c(1.05,1.05,0.95,0.95),length=length(sp$out)),labels=sp$out[order(sp$out)],
     sp$out[order(sp$out)]+rep(c(150,-150,150,-150),length=length(sp$out)),col="red")

sp=boxplot(Temperate$Rs_annual,boxwex=0.7)
title("Boxplot for outlier detection")
xi=1.1
sd.s=sd(Temperate[complete.cases(Temperate),]$"Rs_annual")
mn.s=mean(Temperate[complete.cases(Temperate),]$"Rs_annual")
points(xi,mn.s,col="red",pch=18)
arrows(xi, mn.s - 3*sd.s, xi, mn.s + 3*sd.s, code = 3, col = "pink", angle = 75, length = .1)
text(rep(c(1.05,1.05,0.95,0.95),length=length(sp$out)),labels=sp$out[order(sp$out)],
     sp$out[order(sp$out)]+rep(c(150,-150,150,-150),length=length(sp$out)),col="red")

sp=boxplot(Tropical$Rs_annual,boxwex=0.7)
title("Boxplot for outlier detection")
xi=1.1
sd.s=sd(Tropical[complete.cases(Tropical),]$"Rs_annual")
mn.s=mean(Tropical[complete.cases(Tropical),]$"Rs_annual")
points(xi,mn.s,col="red",pch=18)
arrows(xi, mn.s - 3*sd.s, xi, mn.s + 3*sd.s, code = 3, col = "pink", angle = 75, length = .1)
text(rep(c(1.05,1.05,0.95,0.95),length=length(sp$out)),labels=sp$out[order(sp$out)],
     sp$out[order(sp$out)]+rep(c(150,-150,150,-150),length=length(sp$out)),col="red")
#results were collected and represented by "Outlier" column in dataset "03_processed_data_complete_final.csv", with value 1 being outlier.