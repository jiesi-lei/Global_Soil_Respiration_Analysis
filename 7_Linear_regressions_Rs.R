source("0_functions_plot.R")
source("4_statistical_prep.R")
workingpath = paste0(getwd(),"/figures1014")

#Breakpoint detection=============================================================
#piece-wise regression
dt = select(res.data,Latitude,Rs_annual,Study_midyear)
lin.mod <- lm(Rs_annual~Study_midyear, data = dt)
segmented.mod <- segmented(lin.mod)
plot(dt$Study_midyear,dt$Rs_annual, pch=16, ylim=c(0,120))
plot(segmented.mod, add=T,)
segmented.mod

winresult = win_step(dt,var = 'Study_midyear',size = 11)
winresult = win_step(dt,var = 'Study_midyear',size = 10)#Straring from 1996
winresult = win_step(dt,var = 'Study_midyear',size = 9)#Straring from 1995

lin.mod <- lm(Slope~win_edge, data = winresult)
segmented.mod <- segmented(lin.mod)
plot(winresult$win_edge,winresult$Slope, pch=16, ylim=c(0,120))
plot(segmented.mod, add=T,)
segmented.mod

# other method
res <- br.test(winresult$Slope)#Y2000
winresult[res$estimate,"win_edge"]
res <- bu.test(winresult$Slope)#Y2000
winresult[res$estimate,"win_edge"]
res <- snh.test(winresult$Slope)#Y1999
winresult[res$estimate,"win_edge"]

#Test all possible breakpoints
#Year 1994
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <1995 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=1995 )))
#Year 1995
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <1996 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=1996 )))
#Year 1996
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <1997 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=1997 )))
#Year 1997
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <1998 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=1998 )))
#Year 1998
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <1999 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=1999 )))
#Year 1999
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <2000 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=2000 )))
#Year 2000
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <2001 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=2001 )))
#Year 2001
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <2002 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=2002 )))
#Year 2002
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <2003 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=2003 )))
#Year 2003
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <2004 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=2004 )))
#Year 2004
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <2005 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=2005 )))
#Year 2005
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <2006 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=2006 )))
#Year 2006
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <2007 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=2007 )))
#Year 2007
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <2008 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=2008 )))
#Year 2008
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <2009 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=2009 )))
#Year 2009
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <2010 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=2010 )))
#Year 2010
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <2011 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=2011 )))
#Year 2011
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <2012 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=2012 )))
#Year 2012
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <2013 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=2013 )))
#Year 2013
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <2014 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=2014 )))
#Year 2014
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <2015 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=2015 )))
#Year 2015
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear <2016 )))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data,Study_midyear >=2016 )))


#Rs~year in two time periods============================================================
#1987-1999
Period1 = filter(work.data,Study_midyear<2000)
summary(lm(Rs_annual~Study_midyear, data = Period1))
#Period1$Study_midyear = floor(Period1$Study_midyear)
(p <- ggplot(aes_string(x = "Study_midyear", y = "Rs_annual"),data = Period1) + 
        geom_point(color = brewer.pal(11,"RdGy")[2],size = 1, alpha = 0.4) + 
        geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = brewer.pal(11,"RdGy")[2], size = 1.3) + #绘制回归直线
        theme_few()+
        ylab(NULL)+ xlab(NULL)+ggtitle("(a) 1987-1999")+
        scale_x_continuous(breaks = c(1987,1990,1993,1996,1999), limits = c(1987, 1999.5))+
        scale_y_continuous(breaks = c(0,600,1200,1800,2400,3000), limits = c(0, 3000))+
        theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
        theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))

ggsave(paste0(workingpath,"/fig1_a_0503",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

#2000-2016
Period2 = filter(work.data,Study_midyear>=2000)
summary(lm(Rs_annual~Study_midyear, data = Period2))
(p <- ggplot(aes_string(x = "Study_midyear", y = "Rs_annual"),data = Period2) + 
        geom_point(color = brewer.pal(11,"RdGy")[3],size = 1, alpha = 0.2) + 
        geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = "Coral4", size = 1.3, linetype = "dashed") + #绘制回归直线
        theme_few()+
        ylab(NULL)+ xlab(NULL)+ggtitle("(b) 2000-2016")+
        scale_x_continuous(breaks = c(2000,2004,2008,2012,2016), limits = c(2000, 2016.5))+
        scale_y_continuous(breaks = c(0,800,1600,2400,3200), limits = c(0, 3400))+
        theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
        theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))

ggsave(paste0(workingpath,"/fig1_b_0503",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

###Rs~year in different biomes
summary(lm(Rs_annual~Study_midyear, data = Tropical))
(p <- ggplot(aes_string(x = "Study_midyear", y = "Rs_annual"),data = Tropical) + 
    geom_point(color = '#ff1236',size = 1.6, alpha = 0.4,shape=16,stroke=0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = '#ff1236', size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(a) Tropical and subtropical")+
    scale_x_continuous(breaks=c(1987,1996,2006,2016),limits =c(1987,2016))+
    scale_y_continuous(breaks = c(0,800,1600,2400,3200), limits = c(0, 3400))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))

ggsave(paste0(workingpath,"/fig2_a",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

summary(lm(Rs_annual~Study_midyear, data = Temperate))
(p <- ggplot(aes_string(x = "Study_midyear", y = "Rs_annual"),data = Temperate) + 
    geom_point(color = '#519407',size = 1.6, alpha = 0.2,shape=16,stroke=0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = '#519407', size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(b) Temperate")+
    scale_x_continuous(breaks=c(1987,1996,2006,2016),limits =c(1987,2016.5))+
    scale_y_continuous(breaks = c(0,700,1400,2100,2800), limits = c(0, 2900))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))

ggsave(paste0(workingpath,"/fig2_b",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

summary(lm(Rs_annual~Study_midyear, data = boreal))
(p <- ggplot(aes_string(x = "Study_midyear", y = "Rs_annual"),data = boreal) + 
    geom_point(fill = '#109eec',color='black',size = 1.6, alpha = 0.4,shape=21,stroke=0.2) +
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = 'royalblue', size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(c) Boreal and Arctic")+
    scale_x_continuous(breaks=c(1987,1996,2006,2016),limits =c(1987,2016))+
    scale_y_continuous(breaks = c(0,400,800,1200,1600), limits = c(0, 1700))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))

ggsave(paste0(workingpath,"/fig2_c",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

###Global and regional Rs in response to climatic factors
#delta.MAT
summary(lm(Rs_annual~delta.Mean.Temp., data = work.data))
(p <- ggplot(aes_string(x = "delta.Mean.Temp.", y = "Rs_annual"),data = work.data) + 
    geom_point(fill = brewer.pal(11,"RdGy")[2],size = 1.6, alpha = 0.3,shape = 21, color = brewer.pal(11,"RdGy")[2], stroke=0) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = brewer.pal(11,"RdGy")[2], size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(a) Global")+
    scale_x_continuous(limits = c(-5,5.5),breaks = c(-5.0,-2.5,0,2.5,5))+
    scale_y_continuous(breaks = c(0,800,1600,2400,3200), limits = c(0, 3400))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))
ggsave(paste0(workingpath,"/fig3_a_0507_2",".pdf"), p,width=3,height = 2,units = 'in', dpi = 900 ) #width = 2.2

summary(lm(Rs_annual~delta.Mean.Temp., data = Tropical))
(p <- ggplot(aes_string(x = "delta.Mean.Temp.", y = "Rs_annual"),data = Tropical) + 
    geom_point(fill = '#ff1236',size = 2, alpha = 0.5,shape = 21, color = "black",stroke=0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = '#ff1236', size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(a) Tropical")+
    #scale_x_continuous(limits = c(-5,5.5),breaks = c(-5.0,-2.5,0,2.5,5))+
    scale_y_continuous(breaks = c(0,800,1600,2400,3200), limits = c(0, 3400))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))
ggsave(paste0(workingpath,"/figs4_a",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

summary(lm(Rs_annual~delta.Mean.Temp., data = Temperate))
(p <- ggplot(aes_string(x = "delta.Mean.Temp.", y = "Rs_annual"),data = Temperate) + 
    geom_point(fill = '#519407',size = 2, alpha = 0.5, shape = 21, color = "black",stroke=0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = '#519407', size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(b) Temperate")+
    #scale_x_continuous(limits = c(-5,5.5),breaks = c(-5.0,-2.5,0,2.5,5))+
    scale_y_continuous(breaks = c(0,800,1600,2400,3200), limits = c(0, 3400))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))
ggsave(paste0(workingpath,"/figs4_b",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

summary(lm(Rs_annual~delta.Mean.Temp., data = boreal))
(p <- ggplot(aes_string(x = "delta.Mean.Temp.", y = "Rs_annual"),data = boreal) + 
    geom_point(fill = '#109eec',size = 2, alpha = 0.5, shape = 21, color = "black",stroke=0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = '#109eec', size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(c) Boreal and Arctic")+
    #scale_x_continuous(limits = c(-5,5.5),breaks = c(-5.0,-2.5,0,2.5,5))+
    scale_y_continuous(breaks = c(0,400,800,1200,1600), limits = c(0, 1800))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))
ggsave(paste0(workingpath,"/figs4_c",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

#delta MAP
summary(lm(Rs_annual~delta.Mean.Precip., data = work.data))
(p <- ggplot(aes_string(x = "delta.Mean.Precip.", y = "Rs_annual"),data = work.data) + 
    geom_point(fill = brewer.pal(11,"RdGy")[2],size = 1.6, alpha = 0.3,shape = 21, color = brewer.pal(11,"RdGy")[2],stroke=0) +  
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = brewer.pal(11,"RdGy")[2], size = 1.3, linetype = 2) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(b) Global")+
    scale_x_continuous(limits = c(-1100,1100),breaks = c(-1000,-500,0,500,1000))+
    scale_y_continuous(breaks = c(0,800,1600,2400,3200), limits = c(0, 3400))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))
ggsave(paste0(workingpath,"/fig3_b_0507_2",".pdf"), p,width=3,height = 2,units = 'in', dpi = 900 )

summary(lm(Rs_annual~delta.Mean.Precip., data = Tropical))
(p <- ggplot(aes_string(x = "delta.Mean.Precip.", y = "Rs_annual"),data = Tropical) + 
    geom_point(fill = '#FBB4AE',size = 2, alpha = 0.5,shape = 21, color = "black",stroke=0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = 'firebrick3', size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(d) Tropical and subtropical")+
    scale_x_continuous(limits = c(-800,1200),breaks = c(-800,-400,0,400,800,1200))+
    scale_y_continuous(breaks = c(0,800,1600,2400,3200), limits = c(0, 3400))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))
ggsave(paste0(workingpath,"/figs4_d",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

summary(lm(Rs_annual~delta.Mean.Precip., data = Temperate))
(p <- ggplot(aes_string(x = "delta.Mean.Precip.", y = "Rs_annual"),data = Temperate) + 
    geom_point(fill = '#CCEBC5',size = 2, alpha = 0.3, shape = 21, color = "black",stroke=0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = 'forestgreen', size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(e) Temperate")+
    scale_y_continuous(breaks = c(0,800,1600,2400,3200), limits = c(0, 3400))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))
ggsave(paste0(workingpath,"/figs4_e",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

summary(lm(Rs_annual~delta.Mean.Precip., data = boreal))
(p <- ggplot(aes_string(x = "delta.Mean.Precip.", y = "Rs_annual"),data = boreal) + 
    geom_point(fill = '#109eec',size = 2, alpha = 0.5, shape = 21, color = "black",stroke=0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = 'royalblue', size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(f) Boreal and Arctic")+
    #scale_x_continuous(limits = c(-20,25),breaks = c(-20,-10,0,10,20))+
    scale_y_continuous(breaks = c(0,400,800,1200,1600), limits = c(0, 1800))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))
ggsave(paste0(workingpath,"/figs4_f",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

##Rs~year over 1987-2016
work.data$Biome = factor(work.data$Biome,levels =c("Arctic","Boreal","Mediterranean","Temperate","Subtropical","Tropical") )
summary(lm(Rs_annual~Study_midyear, data = work.data))
(p <- ggplot(aes_string(x = "Study_midyear", y = "Rs_annual"),data = work.data) + 
    geom_point(aes(fill = Biome),size = 1.2, alpha = 0.9, shape = 21,stroke = 0.1) + #0.7
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, size = 0.6, color = "darkorange2", data = work.data) + 
    theme_few()+
    ylab(expression(paste('Rs (g C·',m^{-2},')')))+ xlab(NULL)+
    scale_x_continuous(limits = c(1986,2017),breaks = c(1987,1996,2006,2016))+
    expand_limits(y = 0)+        theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in"))+
    scale_fill_manual(values = c("deepskyblue","royalblue","goldenrod","forestgreen","salmon","red2"))+
    #scale_color_manual(values = c("#FB8072"))+
    theme(panel.border = element_rect(color = "black"),legend.position = c(0.01, 1.06),legend.justification = c(0.01,1),legend.title = element_blank(),
          legend.background = element_blank(),text=element_text(size=8),
          legend.key.size = unit(10,"pt")))

ggsave(paste0(workingpath,"/fig_S2_biome_1.2",".pdf"), p,width=3.3,height = 2.2,units = 'in', dpi = 900 )

##Rs~year over 1987-1999 and 2000- in different biomes
Tropical1 = filter(Tropical, Study_midyear < 2000)
Tropical2 = filter(Tropical, Study_midyear >= 2000)
summary(lm(Rs_annual~Study_midyear, data = Tropical1))
summary(lm(Rs_annual~Study_midyear, data = Tropical2))
(p2 <- ggplot(aes(x = Study_midyear, y = Rs_annual ,fill = Year3, color = Year3),shape=16,data = Tropical) + 
    geom_point(size = 1.2, alpha = 0.4)+scale_color_manual(values = c('#ff1236','#ff1236'))+
    scale_fill_manual(values = c('#ff1236','#ff1236'))+
    xlab(NULL)+theme_few()+
    scale_x_continuous(breaks = c(1987,1996,2006,2016),limits =c(1987,2016))+
    scale_y_continuous(breaks = c(0,1000,2000,3000,4000),limits =c(0,4000))+
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, size = 1) + 
    theme(panel.border = element_rect(color = "grey3"),text=element_text(size=6),
          legend.position = "none",
          legend.title = element_blank(),
          legend.background = element_blank()))+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in"))
ggsave(paste0(workingpath,"/tropical_periods",".pdf"), p2,width=3.3,height = 2.2,units = 'in', dpi = 900 )

Temperate1 = filter(Temperate, Study_midyear < 2000)
Temperate2 = filter(Temperate, Study_midyear >= 2000)
summary(lm(Rs_annual~Study_midyear, data = Temperate1))
summary(lm(Rs_annual~Study_midyear, data = Temperate2))
(p2 <- ggplot(aes(x = Study_midyear, y = Rs_annual ,fill = Year3, color = Year3),shape=16,data = Temperate) + 
    geom_point(size = 1.2, alpha = 0.4)+scale_color_manual(values = c('#519407','#519407'))+
    scale_fill_manual(values = c('#519407','#519407'))+
    xlab(NULL)+theme_few()+
    scale_x_continuous(breaks = c(1987,1996,2006,2016),limits =c(1987,2016))+
    scale_y_continuous(breaks = c(0,1000,2000,3000),limits =c(0,3000))+
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, size = 1, linetype = 2) + 
    theme(panel.border = element_rect(color = "grey3"),text=element_text(size=6),
          legend.position = "none",
          legend.title = element_blank(),
          legend.background = element_blank()))+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in"))
ggsave(paste0(workingpath,"/temperate_periods",".pdf"), p2,width=3.3,height = 2.2,units = 'in', dpi = 900 )

Boreal1 = filter(Boreal, Study_midyear < 2000)
Boreal2 = filter(Boreal, Study_midyear >= 2000)
summary(lm(Rs_annual~Study_midyear, data = Boreal1))
summary(lm(Rs_annual~Study_midyear, data = Boreal2))
(p2 <- ggplot(aes(x = Study_midyear, y = Rs_annual ,fill = Year3, color = Year3),shape=16,data = Boreal) + 
    geom_point(size = 1.2, alpha = 0.4)+scale_color_manual(values = c('#109eec','#109eec'))+
    scale_fill_manual(values = c('#109eec','#109eec'))+
    xlab(NULL)+theme_few()+
    scale_x_continuous(breaks = c(1987,1996,2006,2016),limits =c(1987,2016))+
    scale_y_continuous(breaks = c(0,1000,2000),limits =c(0,2000))+
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, size = 1) + 
    theme(panel.border = element_rect(color = "grey3"),text=element_text(size=6),
          legend.position = "none",
          legend.title = element_blank(),
          legend.background = element_blank()))+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in"))
ggsave(paste0(workingpath,"/boreal_periods",".pdf"), p2,width=3.3,height = 2.2,units = 'in', dpi = 900 )


#Rs trends in different ecosystems
work.data$Ecosystem_type = factor(work.data$Ecosystem_type,levels =c("Deciduous Forest","Evergreen Forest","Mixed Forest","Grassland","Shrubland","Wetland","Others") )
summary(lm(Rs_annual~Study_midyear, data = work.data))

summary(lm(Rs_annual~Study_midyear, data = filter(work.data, Ecosystem_type2 == "Forest")))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data, Ecosystem_type == "Deciduous Forest")))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data, Ecosystem_type == "Evergreen Forest")))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data, Ecosystem_type == "Mixed Forest")))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data, Ecosystem_type == "Grassland")))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data, Ecosystem_type == "Shrubland")))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data, Ecosystem_type == "Wetland")))
summary(lm(Rs_annual~Study_midyear, data = filter(work.data, Ecosystem_type == "Others")))

summary(lm(Rs_annual~Study_midyear, data = filter(Period1, Ecosystem_type2 == "Forest")))
summary(lm(Rs_annual~Study_midyear, data = filter(Period1, Ecosystem_type == "Deciduous Forest")))
summary(lm(Rs_annual~Study_midyear, data = filter(Period1, Ecosystem_type == "Evergreen Forest")))
summary(lm(Rs_annual~Study_midyear, data = filter(Period1, Ecosystem_type == "Mixed Forest")))
summary(lm(Rs_annual~Study_midyear, data = filter(Period1, Ecosystem_type == "Grassland")))
summary(lm(Rs_annual~Study_midyear, data = filter(Period1, Ecosystem_type == "Shrubland")))
summary(lm(Rs_annual~Study_midyear, data = filter(Period1, Ecosystem_type == "Wetland")))
summary(lm(Rs_annual~Study_midyear, data = filter(Period1, Ecosystem_type == "Others")))

summary(lm(Rs_annual~Study_midyear, data = filter(Period2, Ecosystem_type2 == "Forest")))
summary(lm(Rs_annual~Study_midyear, data = filter(Period2, Ecosystem_type == "Deciduous Forest")))
summary(lm(Rs_annual~Study_midyear, data = filter(Period2, Ecosystem_type == "Evergreen Forest")))
summary(lm(Rs_annual~Study_midyear, data = filter(Period2, Ecosystem_type == "Mixed Forest")))
summary(lm(Rs_annual~Study_midyear, data = filter(Period2, Ecosystem_type == "Grassland")))
summary(lm(Rs_annual~Study_midyear, data = filter(Period2, Ecosystem_type == "Shrubland")))
summary(lm(Rs_annual~Study_midyear, data = filter(Period2, Ecosystem_type == "Wetland")))
summary(lm(Rs_annual~Study_midyear, data = filter(Period2, Ecosystem_type == "Others")))

(p2 <- ggplot(aes(x = Study_midyear, y = Rs_annual ,fill = Year3, color = Year3),shape=16,data = filter(work.data, Ecosystem_type == "Shrubland")) + 
    geom_point(size = 1.2, alpha = 0.4)+scale_color_manual(values = c('#519407','#519407'))+
    scale_fill_manual(values = c('#519407','#519407'))+
    xlab(NULL)+theme_few()+
    scale_x_continuous(breaks = c(1987,1996,2006,2016),limits =c(1987,2016))+
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, size = 1) + 
    theme(panel.border = element_rect(color = "grey3"),text=element_text(size=6),
          legend.position = "none",
          legend.title = element_blank(),
          legend.background = element_blank()))+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in"))

(p <- ggplot(aes_string(x = "Study_midyear", y = "Rs_annual"),data = work.data) + 
    geom_point(aes(fill = Ecosystem_type),size = 0.9, alpha = 0.6, shape = 21,stroke = 0.1) + #0.7
    geom_smooth(aes(fill = Ecosystem_type, color =Ecosystem_type ),method = 'lm', formula = y ~ x, se = FALSE, size = 0.4, data = work.data) + 
    theme_few()+
    ylab(expression(paste('Rs (g C·',m^{-2},')')))+ xlab(NULL)+
    scale_x_continuous(limits = c(1986,2017),breaks = c(1987,1996,2006,2016))+
    expand_limits(y = 0)+theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in"))+
    scale_fill_manual(values = c("goldenrod","forestgreen","deepskyblue","red3","royalblue","salmon","violet"))+
    scale_color_manual(values = c("goldenrod","forestgreen","deepskyblue","red3","royalblue","salmon","violet"))+
    #scale_color_manual(values = c("#FB8072"))+
    theme(panel.border = element_rect(color = "black"),legend.position = c(0.01, 1.06),legend.justification = c(0.01,1),legend.title = element_blank(),
          legend.background = element_blank(),text=element_text(size=8),
          legend.key.size = unit(10,"pt")))

ggsave(paste0(workingpath,"/fig_S2_ecosystem_1.2",".pdf"), p,width=3.3,height = 2.2,units = 'in', dpi = 900 )


summary(lm(Rs_annual~Study_midyear, data = filter(Boreal, Study_midyear < 2000 )))
summary(lm(Rs_annual~Study_midyear, data = filter(Temperate, Study_midyear < 2000 )))
summary(lm(Rs_annual~Study_midyear, data = filter(Tropical, Study_midyear < 2000 )))
summary(lm(Rs_annual~Study_midyear, data = filter(Boreal, Study_midyear >= 2000 )))
summary(lm(Rs_annual~Study_midyear, data = filter(Temperate, Study_midyear >= 2000 )))
summary(lm(Rs_annual~Study_midyear, data = filter(Tropical, Study_midyear >= 2000 )))
summary(lm(Rs_annual~Study_midyear, data = Boreal))
summary(lm(Rs_annual~Study_midyear, data = Temperate))
summary(lm(Rs_annual~Study_midyear, data = Tropical))

#Density plot summarizing latitudinal scales of different biomes
(ggplot(work.data,aes(x=work.data$Latitude,fill=Biome2, alpha = 0.1))+theme_base()
+geom_density()+scale_fill_manual(values = c('#ff1236','#519407','#109eec'))+theme(legend.position = c(0.15,0.75))
+xlab("Latitude (degrees) "))
ggsave(paste0(workingpath,"/fig_S_new_density",".pdf"),width=8,height = 6,units = 'in', dpi = 900 )

#Robust regression for RS over time
summary(mblm(Rs_annual~Study_midyear, data = work.data))

