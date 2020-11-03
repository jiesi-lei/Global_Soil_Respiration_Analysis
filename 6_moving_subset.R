source("0_functions_plot.R")
source("4_statistical_prep.R")

#OLS estimator===============================================================================
###Moving subset analysis for year windows
mycolors=colorRampPalette(c(brewer.pal(11,"RdGy")))
rcdata = select(work.data,Latitude,Rs_annual,Study_midyear) 
###generate the coefficients of the window
t=q=10;p=1;i=1
ord = c();Lat.Area=c();Slope=c();R.square=c();P.value=c();Num=Mid.Lat = c(); Year.range = c()
while (t <= max(rcdata$Study_midyear - 1987 +p+0.1)) {
  i = i+1
  win = filter(rcdata,Study_midyear -1987 >= (t-q) & (Study_midyear -1987) <t)
  eq= lm_eqn(df=win)
  Year.range=c(Year.range,paste0((1987+t-q),'-',(1987+t-1)))
  Slope=c(Slope,eq[1])
  R.square=c(R.square,eq[2])
  P.value=c(P.value,eq[3])
  Num = c(Num,nrow(win))
  t = t+p
}
winresult = data.frame(Year.range,R.square,Num)
winresult$P.value = as.numeric(P.value)
winresult$Slope = as.numeric(Slope)

winresult$sig = ''
winresult[winresult$P.value<0.1,which(colnames(winresult)=='sig')] = '·'
winresult[winresult$P.value<0.05,which(colnames(winresult)=='sig')] = '*'
winresult[winresult$P.value<0.01,which(colnames(winresult)=='sig')]= '**'
winresult[winresult$P.value<0.001,which(colnames(winresult)=='sig')]= '***'

TS <- theme(legend.position = "none",
            axis.text.x = element_text(angle=45,size=6, hjust = 1,vjust = 1),
            panel.background=element_blank(),
            panel.border = element_rect(colour = "grey3", fill=NA, size=1),
            axis.text.y = element_text(size=6))
star_p = c(as.numeric(Slope)[1:7]+1,as.numeric(Slope)[8:12]-3, as.numeric(Slope)[13:18]+1,as.numeric(Slope)[19:22]+1)
star <- geom_text(aes(x=Year.range, y=star_p,label = sig),size=5)
winresult$Num.italic = paste0('italic(',winresult$Num,')')
count <- geom_text(aes(x=Year.range, y=54,label = Num.italic),size=2,parse = TRUE)
SFM = scale_fill_manual(values = mycolors(nrow(winresult)))
SYC = scale_y_continuous(breaks = c(-15,0,15,30,45), limits = c(-15, 55))
barplot = ggplot(winresult, aes(x=Year.range,y=Slope)) + geom_bar(aes(fill=Year.range),stat="identity")
barplot = barplot+TS+SFM+star+count+SYC+ggtitle("(d)")

ggsave(paste0(workingpath,"/fig1_d",".pdf"), barplot,width=6.5,height = 2,units = 'in', dpi = 900 )

###Moving subset analysis for latitude windows
cols<-c('#ff1236','#519407','#109eec')
mycolors<-colorRampPalette(cols)
work.data1 = filter(work.data, Study_midyear < 2000)
work.data2 = filter(work.data, Study_midyear >= 2000)
rcdata = select(work.data1,Latitude,Rs_annual,Study_midyear) 
t=q=30;p=1;i=1
ord = c();Lat.Area=c();Slope=c();R.square=c();P.value=c();Num=Mid.Lat = c()
while (t <= max(rcdata$Latitude)+p+0.1) {
  i = i+1
  ord = c(ord,i)
  win = filter(rcdata,Latitude >= t-q & Latitude <t)
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
winresult[winresult$P.value<0.1,which(colnames(winresult)=='sig')] = '??'
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
star_p = c(as.numeric(Slope)[1:49]+0.6)#as.numeric(Slope)[1:25]-1.4, as.numeric(Slope)[26:50]+0.6
star <- geom_text(aes(x=Lat.Area, y=star_p,label = sig),size=2)
winresult$Num.italic = paste0('italic(',winresult$Num,')')
count <- geom_text(aes(x=Lat.Area, y=70,label = Num.italic),size=1,parse = TRUE)#13
SFM = scale_fill_manual(values = mycolors(nrow(winresult)))
barplot = ggplot(winresult, aes(x=Lat.Area,y=Slope)) + geom_bar(aes(fill=Lat.Area),stat="identity",alpha=0.8)
(barplot = barplot+TS+SFM+star+count+ggtitle("(c)"))

ggsave(paste0(workingpath,"/fig2_dxxx",".pdf"), barplot,width=4,height = 2,units = 'in', dpi = 900 ) #width = 6.5

##Moving subset analysis for SOC windows
cols<-c('#ff1236','#519407','#109eec')
mycolors<-colorRampPalette(cols) 
rcdata= select(work.data,SOC_stock,Rs_annual,Study_midyear) 
t=q=60;p=10;i=1
ord = c();MAT.Area=c();Slope=c();R.square=c();P.value=c();Num=Mid.Lat = c()
while (t <= 380) {
  i = i+1
  ord = c(ord,i)
  win = filter(rcdata, SOC_stock >=(t-q) & SOC_stock <(t))
  eq= lm_eqn(df=win)
  MAT.Area=c(MAT.Area,paste0((t-q),'-',(t)))
  Slope=c(Slope,eq[1])
  R.square=c(R.square,eq[2])
  P.value=c(P.value,eq[3])
  Num = c(Num,nrow(win))
  Mid.Lat = c(Mid.Lat,t-(0.5*q))
  t = t+p
}
winresult = data.frame(matrix(nrow = length(ord),ncol = 0))
winresult$Order =ord
winresult$MAT.Area =MAT.Area
winresult$Slope = as.numeric(Slope)
winresult$R.square = as.numeric(R.square)
winresult$P.value = as.numeric(P.value)
winresult$Num = Num
winresult$Mid.Lat = Mid.Lat

winresult$MAT.Area <- factor(winresult$MAT.Area,levels=c(winresult$MAT.Area))#prevent the reorder of levels 
winresult$sig = ''
winresult[winresult$P.value<0.1,which(colnames(winresult)=='sig')] = ''
winresult[winresult$P.value<0.05,which(colnames(winresult)=='sig')] = '*'
winresult[winresult$P.value<0.001,which(colnames(winresult)=='sig')]= '**'
winresult[winresult$P.value<0.001,which(colnames(winresult)=='sig')]= '***'

TS <- theme(text = element_text(size=6),
            legend.position = "none",
            axis.text.x = element_text(angle=45,size=3, hjust = 1,vjust = 1),
            panel.background=element_blank(),
            panel.border = element_rect(colour = "grey3", fill=NA, size=0.5),
            axis.text.y = element_text(size=6))
star_p = c(as.numeric(Slope)[1:11]-4.5,as.numeric(Slope)[12:33]+1)
star_p[20:27]=star_p[20:27]-3.5
star <- geom_text(aes(x=MAT.Area, y=star_p,label = sig),size=2)
winresult$Num.italic = paste0('italic(',winresult$Num,')')
count <- geom_text(aes(x=MAT.Area, y=80,label = Num.italic),size=1.7,parse = TRUE)
SFM = scale_fill_manual(values = mycolors(nrow(winresult)))
barplot = ggplot(winresult, aes(x=MAT.Area,y=Slope)) + geom_bar(aes(fill=MAT.Area),stat="identity",alpha=0.8)
barplot = barplot+TS+SFM+star+count+xlab("SOC.Area")+ggtitle("(e)")
barplot

ggsave(paste0(workingpath,"/fig3_c",".pdf"), barplot,width=6.5,height = 2,units = 'in', dpi = 900 )

#Theil-sen estimator
# Theil-Sen regression for moving window analysis-------------------------------
install.packages("mblm")
library("mblm")
###Moving subset analysis for year windows
mycolors=colorRampPalette(c(brewer.pal(11,"RdGy"))[1:5])
rcdata = select(srdb,Latitude,Rs_annual,Study_midyear) 
###generate the coefficients of the window
t=q=10;p=1;i=1
ord = c();Lat.Area=c();Slope=c();R.square=c();P.value=c();Num=Mid.Lat = c(); Year.range = c()
while (t <= max(rcdata$Study_midyear - 1987 +p+0.1)) {
  win = filter(rcdata,Study_midyear -1987 >= (t-q) & (Study_midyear -1987) <t)
  eq= mblm_eqn(df=win)
  Year.range=c(Year.range,paste0((1987+t-q),'-',(1987+t-1)))
  Slope=c(Slope,eq[1])
  P.value=c(P.value,eq[2])
  Num = c(Num,nrow(win))
  t = t+p
}

winresult = data.frame(Year.range,Num)
winresult$P.value = as.numeric(P.value)
winresult$Slope = as.numeric(Slope)

winresult$sig = ''
winresult[winresult$P.value<0.1,which(colnames(winresult)=='sig')] = ''
winresult[winresult$P.value<0.05,which(colnames(winresult)=='sig')] = '*'
winresult[winresult$P.value<0.01,which(colnames(winresult)=='sig')]= '**'
winresult[winresult$P.value<0.001,which(colnames(winresult)=='sig')]= '***'

TS <- theme(legend.position = "none",
            axis.text.x = element_text(angle=45,size=6, hjust = 1,vjust = 1),
            panel.background=element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            axis.text.y = element_text(size=6))
star_p = c(as.numeric(Slope)[1:7]+1,as.numeric(Slope)[8:12]-3, as.numeric(Slope)[13:18]+1,as.numeric(Slope)[19:21]+1)
star <- geom_text(aes(x=Year.range, y=star_p,label = sig),size=5)
winresult$Num.italic = paste0('italic(',winresult$Num,')')
count <- geom_text(aes(x=Year.range, y=54,label = Num.italic),size=2,parse = TRUE)
SFM = scale_fill_manual(values = mycolors(nrow(winresult)))
SYC = scale_y_continuous(breaks = c(-15,0,15,30,45), limits = c(-15, 55))
barplot = ggplot(winresult, aes(x=Year.range,y=Slope)) + geom_bar(aes(fill=Year.range),stat="identity")
barplot = barplot+TS+SFM+star+count+SYC+ggtitle("(a)")
barplot

ggsave("fig1_d_mblm_1025.pdf", barplot,width=6.5,height = 2.6,units = 'in', dpi = 900 )

###Moving subset analysis for latitude windows
cols<-c('#ff1236','#519407','#109eec')
mycolors<-colorRampPalette(cols)
rcdata = select(work.data,Latitude,Rs_annual,Study_midyear) 
t=q=30;p=1;i=1
ord = c();Lat.Area=c();Slope=c();R.square=c();P.value=c();Num=Mid.Lat = c()
while (t <= max(rcdata$Latitude)+p+0.1) {
  i = i+1
  ord = c(ord,i)
  win = filter(rcdata,Latitude >= t-q & Latitude <t)
  eq= mblm_eqn(df=win)
  Lat.Area=c(Lat.Area,paste0((t-q),'-',t))
  Slope=c(Slope,eq[1])
  P.value=c(P.value,eq[2])
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
winresult[winresult$P.value<0.1,which(colnames(winresult)=='sig')] = ''
winresult[winresult$P.value<0.05,which(colnames(winresult)=='sig')] = '*'
winresult[winresult$P.value<0.001,which(colnames(winresult)=='sig')]= '**'
winresult[winresult$P.value<0.001,which(colnames(winresult)=='sig')]= '***'

#Themeset
TS <- theme(element_line(size = 0.3),text = element_text(size=6),
            legend.position = "none",
            axis.text.x = element_text(angle=45,size=3, hjust = 1,vjust = 1),
            panel.background=element_blank(),
            panel.border = element_rect(colour = "grey3", fill=NA, size=0.5),
            axis.text.y = element_text(size=6))
star_p = c(as.numeric(Slope)[1:30]-0.8,as.numeric(Slope)[31:50]+0.6)#as.numeric(Slope)[1:25]-1.4, as.numeric(Slope)[26:50]+0.6
star <- geom_text(aes(x=Lat.Area, y=star_p,label = sig),size=2)
winresult$Num.italic = paste0('italic(',winresult$Num,')')
count <- geom_text(aes(x=Lat.Area, y=70,label = Num.italic),size=1,parse = TRUE)#13
SFM = scale_fill_manual(values = mycolors(nrow(winresult)))
barplot = ggplot(winresult, aes(x=Lat.Area,y=Slope)) + geom_bar(aes(fill=Lat.Area),stat="identity",alpha=0.8)
(barplot = barplot+TS+SFM+star+count+ggtitle("(b)"))

ggsave(paste0(workingpath,"/fig2_dxxx_1025",".pdf"), barplot,width=6.5,height = 2,units = 'in', dpi = 900 ) #width = 6.5

##Moving subset analysis for SOC windows
cols<-c('#ff1236','#519407','#109eec')
mycolors<-colorRampPalette(cols) 
rcdata= select(work.data,SOC_stock,Rs_annual,Study_midyear) 
t=q=60;p=10;i=1
ord = c();MAT.Area=c();Slope=c();R.square=c();P.value=c();Num=Mid.Lat = c()
while (t <= 380) {
  ord = c(ord,i)
  win = filter(rcdata, SOC_stock >=(t-q) & SOC_stock <(t))
  eq= mblm_eqn(df=win)
  MAT.Area=c(MAT.Area,paste0((t-q),'-',(t)))
  Slope=c(Slope,eq[1])
  P.value=c(P.value,eq[2])
  Num = c(Num,nrow(win))
  t = t+p
  i = i+1
}
winresult = data.frame(matrix(nrow = length(ord),ncol = 0))
winresult$Order =ord
winresult$MAT.Area =MAT.Area
winresult$Slope = as.numeric(Slope)
winresult$P.value = as.numeric(P.value)
winresult$Num = Num
winresult$Mid.Lat = Mid.Lat

winresult$MAT.Area <- factor(winresult$MAT.Area,levels=c(winresult$MAT.Area))#prevent the reorder of levels 
winresult$sig = ''
winresult[winresult$P.value<0.1,which(colnames(winresult)=='sig')] = ''
winresult[winresult$P.value<0.05,which(colnames(winresult)=='sig')] = '*'
winresult[winresult$P.value<0.001,which(colnames(winresult)=='sig')]= '**'
winresult[winresult$P.value<0.001,which(colnames(winresult)=='sig')]= '***'

TS <- theme(text = element_text(size=6),
            legend.position = "none",
            axis.text.x = element_text(angle=45,size=5, hjust = 1,vjust = 1),
            panel.background=element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=0.5),
            axis.text.y = element_text(size=6))
star_p = c(as.numeric(Slope)[1:11]-4.5,as.numeric(Slope)[12:33]+1)
star_p[20:27]=star_p[20:27]-3.5
star <- geom_text(aes(x=MAT.Area, y=star_p,label = sig),size=2.1)
winresult$Num.italic = paste0('italic(',winresult$Num,')')
count <- geom_text(aes(x=MAT.Area, y=100,label = Num.italic),size=1.7,parse = TRUE)
SFM = scale_fill_manual(values = mycolors(nrow(winresult)))
barplot = ggplot(winresult, aes(x=MAT.Area,y=Slope)) + geom_bar(aes(fill=MAT.Area),stat="identity",alpha=0.8)
barplot = barplot+TS+SFM+star+count+xlab("SOC.Area")+ggtitle("(c)")
barplot

ggsave(paste0(workingpath,"/fig3_c_1025",".pdf"), barplot,width=6.5,height = 2,units = 'in', dpi = 900 )


#Rs~year in SOC intervals
#0-100
SOC1 = filter(work.data,SOC_stock<100)
summary(lm(Rs_annual~Study_midyear, data = SOC1))
(p <- ggplot(aes_string(x = "Study_midyear", y = "Rs_annual"),data = SOC1) + 
    geom_point(color = '#ff1236',size = 1.6, alpha = 0.3,shape=16,stroke=0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = '#ff1236', size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(a) SOC 0-100")+
    scale_x_continuous(breaks=c(1987,1996,2006,2016),limits =c(1987,2016))+
    scale_y_continuous(breaks = c(0,800,1600,2400,3200), limits = c(0, 3400))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))
ggsave(paste0(workingpath,"/SOC.0-100",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )
#100-180
SOC2 = filter(work.data,SOC_stock>=100 & SOC_stock < 180)
summary(lm(Rs_annual~Study_midyear, data = SOC2))
(p <- ggplot(aes_string(x = "Study_midyear", y = "Rs_annual"),data = SOC2) + 
    geom_point(color = 'Chocolate',size = 1.6, alpha = 0.3,shape=16,stroke=0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = 'Chocolate', size = 1.3) + #绘制回归直线
    theme_few()+
      ylab(NULL)+ xlab(NULL)+ggtitle("(b) SOC 100-180")+
    scale_x_continuous(breaks=c(1987,1996,2006,2016),limits =c(1987,2016.5))+
    scale_y_continuous(breaks = c(0,700,1400,2100,2800), limits = c(0, 2900))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))
ggsave(paste0(workingpath,"/SOC.100-180",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

#180-270
SOC3 = filter(work.data,SOC_stock>=180 & SOC_stock < 270)
summary(lm(Rs_annual~Study_midyear, data = SOC3))
(p <- ggplot(aes_string(x = "Study_midyear", y = "Rs_annual"),data =SOC3) + 
    geom_point(color = '#519407',size = 1.6, alpha = 0.4,shape=16,stroke=0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = 'Darkgreen', size = 1.3, linetype = "dashed") + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(c) SOC 180-270")+
    scale_x_continuous(breaks=c(1987,1996,2006,2016),limits =c(1987,2016.5))+
    scale_y_continuous(breaks = c(0,700,1400,2100,2800), limits = c(0, 2900))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))
ggsave(paste0(workingpath,"/SOC.180-270.0503",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

#270-
SOC4 = filter(work.data,SOC_stock>=270)
summary(lm(Rs_annual~Study_midyear, data = SOC4))
(p <- ggplot(aes_string(x = "Study_midyear", y = "Rs_annual"),data =SOC4) + 
    geom_point(color = '#109eec',size = 1.6, alpha = 0.6,shape=16,stroke=0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = '#109eec', size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(d) SOC 270-")+
    scale_x_continuous(breaks=c(1987,1996,2006,2016),limits =c(1987,2016.5))+
    scale_y_continuous(breaks = c(0,700,1400,2100,2800), limits = c(0, 2900))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))
ggsave(paste0(workingpath,"/SOC.270-",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )


