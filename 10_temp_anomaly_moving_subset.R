source("0_functions_plot.R")
source("4_statistical_prep.R")
workingpath = getwd()

temp = read.csv('annual.temp.30y.csv', check.names = F)
precip = read.csv('annual.precip.30y.csv',check.names = F)
delta.temp = read.csv('delta.temp.30y.csv', check.names = F)
delta.precip = read.csv('delta.precip.30y.csv', check.names = F)

Lat = rep(temp$work.data.Lat, 30)
Biome = rep(temp$work.data.Biome,30)
Study_midyear = rep(1987:2016, each = nrow(temp))
Temp.annual = as.numeric(unlist(temp[,c(4:33)]))
Precip.annual = as.numeric(unlist(precip[,c(4:33)]))
delta.temp = as.numeric(unlist(delta.temp[,c(4:33)]))
delta.precip = as.numeric(unlist(delta.precip[,c(4:33)]))
climate.data = data.frame(Lat,Biome,Study_midyear,Temp.annual,Precip.annual,delta.temp,delta.precip)

climate.data$Lat.Area = as.character(lapply(climate.data$Lat,labelLat)) %>% as.factor()
climate.data$Year = as.character(lapply(climate.data$Study_midyear,labelYear3)) %>% as.factor()
climate.data$Biome = factor(climate.data$Biome, levels=rev(levels(climate.data$Biome)))
climate.data$delta.precip=climate.data$delta.precip*12 
climate.data = filter(climate.data,!is.na(delta.precip))
idv = c('Temp.annual','Precip.annual','delta.temp','delta.precip')

###Moving subset analysis for latitude windows
cols<-c('#238b45')
mycolors<-colorRampPalette(cols)
rcdata = select(climate.data,Lat,delta.temp,Study_midyear) 
t=q=30;p=1;i=1
ord = c();Lat.Area=c();Slope=c();R.square=c();P.value=c();Num=Mid.Lat = c()
while (t <= max(rcdata$Lat)+p+0.1) {
  i = i+1
  ord = c(ord,i)
  win = filter(rcdata,Lat >= t-q & Lat <t)
  eq= lm_eqn(df=win)
  Lat.Area=c(Lat.Area,paste0((t-q),'-',t))
  Slope=c(Slope,eq[1])
  R.square=c(R.square,eq[2])
  P.value=c(P.value,eq[3])
  Num = c(Num,nrow(win))
  Mid.Lat = c(Mid.Lat,t-(0.5*q))
  t = t+p
}
winresult = data.frame(matrix(nrow = length(ord),ncol = 0))
winresult$Order =ord
winresult$Lat.Area =Lat.Area
winresult$Slope = as.numeric(Slope)
winresult$R.square = as.numeric(R.square)
winresult$P.value = as.numeric(P.value)
winresult$Num = Num
winresult$Mid.Lat = Mid.Lat

winresult$Lat.Area <- factor(winresult$Lat.Area,levels=c(winresult$Lat.Area))#prevent the reorder of levels 
winresult$sig = ''
winresult[winresult$P.value<0.1,which(colnames(winresult)=='sig')] = 'Â·'
winresult[winresult$P.value<0.05,which(colnames(winresult)=='sig')] = '*'
winresult[winresult$P.value<0.001,which(colnames(winresult)=='sig')]= '**'
winresult[winresult$P.value<0.001,which(colnames(winresult)=='sig')]= '***'

#Themeset
TS <- theme(text = element_text(size=6),
            legend.position = "none",
            axis.text.x = element_text(angle=45,size=3, hjust = 1,vjust = 1),
            panel.background=element_blank(),
            panel.border = element_rect(colour = "grey3", fill=NA, size=0.5),
            axis.text.y = element_text(size=6))
star_p = c(as.numeric(Slope)[1:50]+0.0005)
star <- geom_text(aes(x=Lat.Area, y=star_p,label = sig),size=2)
winresult$Num.italic = paste0('italic(',winresult$Num,')')
count <- geom_text(aes(x=Lat.Area, y=0.03,label = Num.italic),size=1,parse = TRUE)
SFM = scale_fill_manual(values = mycolors(nrow(winresult)))
barplot = ggplot(winresult, aes(x=Lat.Area,y=Slope)) + geom_bar(aes(fill=Lat.Area),stat="identity",alpha=0.8)
barplot = barplot+TS+SFM+star+count+ggtitle("(e)")


###before 2000---------------------------------------------
rcdata = select(climate.data,Lat,delta.temp,Study_midyear) 
rcdata = filter(rcdata,Study_midyear < 2000 )
t=q=30;p=1;i=1
ord = c();Lat.Area=c();Slope=c();R.square=c();P.value=c();Num=Mid.Lat = c()
while (t <= max(rcdata$Lat)+p+0.1) {
  i = i+1
  ord = c(ord,i)
  win = filter(rcdata,Lat >= t-q & Lat <t)
  eq= lm_eqn(df=win)
  Lat.Area=c(Lat.Area,paste0((t-q),'-',t))
  Slope=c(Slope,eq[1])
  R.square=c(R.square,eq[2])
  P.value=c(P.value,eq[3])
  Num = c(Num,nrow(win))
  Mid.Lat = c(Mid.Lat,t-(0.5*q))
  t = t+p
}
winresult = data.frame(matrix(nrow = length(ord),ncol = 0))
winresult$Order =ord
winresult$Lat.Area =Lat.Area
winresult$Slope = as.numeric(Slope)
winresult$R.square = as.numeric(R.square)
winresult$P.value = as.numeric(P.value)
winresult$Num = Num
winresult$Mid.Lat = Mid.Lat

winresult$Lat.Area <- factor(winresult$Lat.Area,levels=c(winresult$Lat.Area))#prevent the reorder of levels 
winresult$sig = ''
winresult[winresult$P.value<0.1,which(colnames(winresult)=='sig')] = 'Â·'
winresult[winresult$P.value<0.05,which(colnames(winresult)=='sig')] = '*'
winresult[winresult$P.value<0.001,which(colnames(winresult)=='sig')]= '**'
winresult[winresult$P.value<0.001,which(colnames(winresult)=='sig')]= '***'

#Themeset
TS <- theme(text = element_text(size=6),
            legend.position = "none",
            axis.text.x = element_text(angle=45,size=5, hjust = 1,vjust = 1),
            panel.background=element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            axis.text.y = element_text(size=6))
star_p = c(as.numeric(Slope)[1:50]+0.0002)
star <- geom_text(aes(x=Lat.Area, y=star_p,label = sig),size=1.5)
winresult$Num.italic = paste0('italic(',winresult$Num,')')
count <- geom_text(aes(x=Lat.Area, y=0.034,label = Num.italic),size=0.8,parse = TRUE)
SFM = scale_fill_manual(values = mycolors(nrow(winresult)))
(barplot = ggplot(winresult, aes(x=Lat.Area,y=Slope)) + geom_bar(aes(fill=Lat.Area),stat="identity",alpha=0.8)
          +scale_y_continuous(breaks = c(0.000,0.005,0.010,0.015,0.020,0.025,0.030,0.035)))
barplot = barplot+TS+SFM+star+count+ggtitle("1987-2000")+ xlab("Latitude windows")+ylab(expression('Change rate of delta MAT (¡ãC¡¤'~year^-1~')'))
ggsave(paste0(workingpath,"/Temp.anomaly.change.1987-1999.0614",".pdf"), barplot,width=4.5,height = 2,units = 'in', dpi = 900 )

###After 2000
rcdata = select(climate.data,Lat,delta.temp,Study_midyear) 
rcdata = filter(rcdata,Study_midyear >= 2000 )
t=q=30;p=1;i=1
ord = c();Lat.Area=c();Slope=c();R.square=c();P.value=c();Num=Mid.Lat = c()
while (t <= max(rcdata$Lat)+p+0.1) {
  i = i+1
  ord = c(ord,i)
  win = filter(rcdata,Lat >= t-q & Lat <t)
  eq= lm_eqn(df=win)
  Lat.Area=c(Lat.Area,paste0((t-q),'-',t))
  Slope=c(Slope,eq[1])
  R.square=c(R.square,eq[2])
  P.value=c(P.value,eq[3])
  Num = c(Num,nrow(win))
  Mid.Lat = c(Mid.Lat,t-(0.5*q))
  t = t+p
}
winresult = data.frame(matrix(nrow = length(ord),ncol = 0))
winresult$Order =ord
winresult$Lat.Area =Lat.Area
winresult$Slope = as.numeric(Slope)
winresult$R.square = as.numeric(R.square)
winresult$P.value = as.numeric(P.value)
winresult$Num = Num
winresult$Mid.Lat = Mid.Lat

winresult$Lat.Area <- factor(winresult$Lat.Area,levels=c(winresult$Lat.Area))#prevent the reorder of levels 
winresult$sig = ''
winresult[winresult$P.value<0.1,which(colnames(winresult)=='sig')] = 'Â·'
winresult[winresult$P.value<0.05,which(colnames(winresult)=='sig')] = '*'
winresult[winresult$P.value<0.001,which(colnames(winresult)=='sig')]= '**'
winresult[winresult$P.value<0.001,which(colnames(winresult)=='sig')]= '***'

#Themeset
TS <- theme(text = element_text(size=6),
            legend.position = "none",
            axis.text.x = element_text(angle=45,size=5, hjust = 1,vjust = 1),
            panel.background=element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            axis.text.y = element_text(size=6))
star_p = c(as.numeric(Slope)[1:50]+0.0002)
star <- geom_text(aes(x=Lat.Area, y=star_p,label = sig),size=1.5)
winresult$Num.italic = paste0('italic(',winresult$Num,')')
count <- geom_text(aes(x=Lat.Area, y=0.024,label = Num.italic),size=0.8,parse = TRUE)
SFM = scale_fill_manual(values = mycolors(nrow(winresult)))
(barplot = ggplot(winresult, aes(x=Lat.Area,y=Slope)) + geom_bar(aes(fill=Lat.Area),stat="identity",alpha=0.8)
  +scale_y_continuous(breaks = c(0.000,0.005,0.010,0.015,0.020,0.025,0.030,0.035)))
barplot = barplot+TS+SFM+star+count+ggtitle("2000-2016")+ xlab("Latitude windows")+ylab(expression('Change rate of delta MAT (¡ãC¡¤'~yr^-1~')'))

ggsave(paste0(workingpath,"/Temp.anomaly.change.2000-2016.0614",".pdf"), barplot,width=4.5,height = 2,units = 'in', dpi = 900 )
