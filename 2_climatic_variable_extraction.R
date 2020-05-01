source("0_functions_plot.R")
res.data <- read.csv("01_cleandata.csv") 
TEMPpath <- "Terrestial Precipitation and Temperature/air_temp_2017/"
PRECIPpath <- "Terrestial Precipitation and Temperature/precip_2017/"
###--------------------------------------------------------------------

#res.data = select(res.data,Site,Lat,Lon,Latitude,Longitude,Study_midyear)

###-----------------------------------开始处理数???
###主要是检查年份大???2015的情??? # check error in res.data 
#res.data = res.data[-which(res.data$midyear >= 2015),]


###match location
working.data = na.omit(select(res.data,Record_number,Site2,Latitude,Longitude,Study_midyear))
working.data = filter(working.data,working.data$Study_midyear!=1)
yearsrange = c(floor(min(working.data$Study_midyear)-1):2017)

# temp extration
tempdata = read.table(paste0(TEMPpath,'air_temp.',yearsrange[1]))
tempdata$Site = paste(tempdata$V2,tempdata$V1)
Site.num <- Site.match <- Lon <- Lat <- c()
i = 1
for (i in 1:nrow(working.data)){
  row.num = which.min((tempdata$V1-working.data$Longitude[i])^2+(tempdata$V2-working.data$Latitude[i])^2)
  Site.match[i] = tempdata[row.num,16]
  Lon[i] = tempdata[row.num,1]
  Lat[i] = tempdata[row.num,2]
  }

annotation1 = data.frame(working.data$Record_number,Site.match)
tempdata.list = unique(data.frame(Site.match))

for (i in 1:length(yearsrange)){
  tempdata = read.table(paste0(TEMPpath,'air_temp.',yearsrange[i]))
  tempdata$Site = paste(tempdata$V2,tempdata$V1)
  tempdata = filter(tempdata, Site %in% tempdata.list$Site.match)
  colnames(tempdata)[3:14] = c(paste0(rep(yearsrange[i],times=12),'_',c(1:12)))
  tempdata.list = merge(tempdata.list,tempdata[,c(3:14,16)],by.x = 'Site.match', by.y = 'Site')
}
tempdata.list = arrange(tempdata.list, Site.match)
write.csv(tempdata.list,"temp_extract_result.csv",row.names=FALSE) 
rm(tempdata,tempdata.list)
  
#precip extraction
precipdata = read.table(paste0(PRECIPpath,'precip.',yearsrange[1]))
precipdata$Site = paste(precipdata$V2,precipdata$V1)
Site.num <- Site.match <- Lon <- Lat <- c()
i = 1
for (i in 1:nrow(working.data)){
  row.num = which.min((precipdata$V1-working.data$Longitude[i])^2+(precipdata$V2-working.data$Latitude[i])^2)
  Site.match[i] = precipdata[row.num,16]
  Lon[i] = precipdata[row.num,1]
  Lat[i] = precipdata[row.num,2]
}

annotation2 = data.frame(working.data$Record_number,Site.match)
precipdata.list = unique(data.frame(Site.match))

for (i in 1:length(yearsrange)){
  precipdata = read.table(paste0(PRECIPpath,'precip.',yearsrange[i]))
  precipdata$Site = paste(precipdata$V2,precipdata$V1)
  precipdata = filter(precipdata, Site %in% precipdata.list$Site.match)
  colnames(precipdata)[3:14] = c(paste0(rep(yearsrange[i],times=12),'_',c(1:12)))
  precipdata.list = merge(precipdata.list,precipdata[,c(3:14,16)],by.x = 'Site.match', by.y = 'Site')
}
precipdata.list = arrange(precipdata.list, Site.match)
write.csv(precipdata.list,"precip_extract_result.csv",row.names=FALSE) 
rm(precipdata,precipdata.list)

res.data = merge(res.data, annotation1, by.x ='Record_number', by.y = 'working.data.Record_number')
write.csv(res.data,'02_sampledataset.csv',row.names = F)
