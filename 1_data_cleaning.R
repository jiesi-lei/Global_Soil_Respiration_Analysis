source("0_functions_plot.R")
workingpath = getwd()
res.data <- read.delim(file.choose(), sep = ',',stringsAsFactors = FALSE, check.names = FALSE)

#Replace negative values with absolute ones
summary(res.data$Rs_annual)
a = res.data[res.data$Rs_annual<0&!is.na(res.data$Rs_annual),which(colnames(res.data)=='Rs_annual')]
res.data[res.data$Rs_annual<0&!is.na(res.data$Rs_annual),which(colnames(res.data)=='Rs_annual')] = abs(a)

res.data = filter(res.data, !is.na(Rs_annual))
res.data = filter(res.data, !is.na(Latitude))

rs.lost = res.data %$% which(!is.na(Rh_annual)&!is.na(Ra_annual)&is.na(Rs_annual))
rh.lost = res.data %$% which(!is.na(Rs_annual)&!is.na(Ra_annual)&is.na(Rh_annual))
ra.lost = res.data %$% which(!is.na(Rs_annual)&!is.na(Rh_annual)&is.na(Ra_annual))

res.data$Rs_annual[rs.lost] = res.data$Ra_annual[rs.lost] + res.data$Rh_annual[rs.lost]

###加一个site信息和近似经纬度
res.data$Site2 = paste(res.data$Latitude,res.data$Longitude)

###modify the variables
attach(res.data)
leaflabel = Leaf_habit[which(Ecosystem_type == 'Forest')]
forestlabel = Ecosystem_type[which(Ecosystem_type == 'Forest')]
forest_type = paste(leaflabel,forestlabel)
forest_type[which(forest_type == ' Forest')] = 'Others'
res.data$Ecosystem_type = as.character(res.data$Ecosystem_type)
res.data$Ecosystem_type[which(Ecosystem_type == 'Forest')] = forest_type

Methodlabel = as.character(levels(Meas_method))
Meas_method = as.character(Meas_method)
Meas_method[which(Meas_method == 'Gas chromatography')] = "Gas Chromatography"
Meas_method[which(Meas_method == 'Soda lime')] = "Alkali absorption"
Meas_method[which(Meas_method == 'IRGA, static, gradient')] = "IRGA"
res.data$Meas_method = as.factor(Meas_method)

rm(leaflabel,forestlabel,forest_type,Methodlabel)
detach(res.data)

###manaually add the data according to the available references
res.data = filter(res.data, !is.na(Rs_annual))
res.data[which(res.data$Study_number == 4119),'Study_midyear'] = 2005.25
res.data[which(res.data$Study_number == 4119),'YearsOfData'] = 1

#save
res.data = filter(res.data, !is.na(Study_midyear)&!is.na(Site2))
write.csv(res.data,'01_cleandata.csv',row.names=FALSE)



      