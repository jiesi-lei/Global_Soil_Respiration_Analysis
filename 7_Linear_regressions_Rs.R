source("0_functions_plot.R")
source("4_statistical_prep.R")

###Rs~year in different decades
summary(lm(Rs_annual~Study_midyear, data = Dec1))
(p <- ggplot(aes_string(x = "Study_midyear", y = "Rs_annual"),data = Dec1) + 
    geom_point(color = brewer.pal(11,"RdGy")[2],size = 1.3, alpha = 0.4) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = brewer.pal(11,"RdGy")[2], size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(a)1987-1996")+
    scale_x_continuous(breaks = c(1987,1990,1993,1996), limits = c(1987, 1997))+
    scale_y_continuous(breaks = c(0,600,1200,1800,2400), limits = c(0, 2600))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))

ggsave(paste0(workingpath,"fig1_a",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )


summary(lm(Rs_annual~Study_midyear, data = Dec2))
(p <- ggplot(aes_string(x = "Study_midyear", y = "Rs_annual"),data = Dec2) + 
    geom_point(color = brewer.pal(11,"RdGy")[3],size = 1.3, alpha = 0.25) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = brewer.pal(11,"RdGy")[3], size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(b)1997-2006")+
    scale_x_continuous(breaks = c(1997,2000,2003,2006), limits = c(1997, 2007))+
    scale_y_continuous(breaks = c(0,800,1600,2400,3200), limits = c(0, 3400))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))

ggsave(paste0(workingpath,"fig1_b",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )


summary(lm(Rs_annual~Study_midyear, data = Dec3))
(p <- ggplot(aes_string(x = "Study_midyear", y = "Rs_annual"),data = Dec3) + 
    geom_point(color = brewer.pal(11,"RdGy")[4],size = 1.3, alpha = 0.4) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = brewer.pal(11,"RdGy")[4], size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(c)2007-2016")+
    scale_x_continuous(breaks = c(2007,2010,2013,2016), limits = c(2007, 2017))+
    scale_y_continuous(breaks = c(0,800,1600,2400,3200), limits = c(0, 3400))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))

ggsave(paste0(workingpath,"fig1_c",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

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

ggsave(paste0(workingpath,"fig2_a",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

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

ggsave(paste0(workingpath,"fig2_b",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

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

ggsave(paste0(workingpath,"fig2_c",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

###Global and regional Rs in response to climatic factors
#delta.MAT
summary(lm(Rs_annual~delta.Mean.Temp., data = work.data))
(p <- ggplot(aes_string(x = "delta.Mean.Temp.", y = "Rs_annual"),data = work.data) + 
    geom_point(fill = '#bbc6ca',size = 1.6, alpha = 0.2,shape = 21, color = "black",stroke=0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = '#bbc6ca', size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(a) Global")+
    scale_x_continuous(limits = c(-5,5.5),breaks = c(-5.0,-2.5,0,2.5,5))+
    scale_y_continuous(breaks = c(0,800,1600,2400,3200), limits = c(0, 3400))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))
ggsave(paste0(workingpath,"fig3_a",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

summary(lm(Rs_annual~delta.Mean.Temp., data = Tropical))
(p <- ggplot(aes_string(x = "delta.Mean.Temp.", y = "Rs_annual"),data = Tropical) + 
    geom_point(fill = '#FBB4AE',size = 2, alpha = 0.5,shape = 21, color = "black",stroke=0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = '#ff1236', size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(a) Tropical")+
    #scale_x_continuous(limits = c(-5,5.5),breaks = c(-5.0,-2.5,0,2.5,5))+
    scale_y_continuous(breaks = c(0,800,1600,2400,3200), limits = c(0, 3400))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))
ggsave(paste0(workingpath,"figs4_a",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

summary(lm(Rs_annual~delta.Mean.Temp., data = Temperate))
(p <- ggplot(aes_string(x = "delta.Mean.Temp.", y = "Rs_annual"),data = Temperate) + 
    geom_point(fill = '#CCEBC5',size = 2, alpha = 0.5, shape = 21, color = "black",stroke=0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = '#519407', size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(b) Temperate")+
    #scale_x_continuous(limits = c(-5,5.5),breaks = c(-5.0,-2.5,0,2.5,5))+
    scale_y_continuous(breaks = c(0,800,1600,2400,3200), limits = c(0, 3400))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))
ggsave(paste0(workingpath,"figs4_b",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

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
ggsave(paste0(workingpath,"figs4_c",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

#delta MAP
summary(lm(Rs_annual~delta.Mean.Precip., data = work.data))
(p <- ggplot(aes_string(x = "delta.Mean.Precip.", y = "Rs_annual"),data = work.data) + 
    geom_point(fill = '#bbc6ca',size = 2, alpha = 0.4,shape = 21, color = "black",stroke=0.05) +  
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = '#bbc6ca', size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(b) Global")+
    scale_x_continuous(limits = c(-1100,1100),breaks = c(-1000,-500,0,500,1000))+
    scale_y_continuous(breaks = c(0,800,1600,2400,3200), limits = c(0, 3400))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))
ggsave(paste0(workingpath,"fig3_b",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

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
ggsave(paste0(workingpath,"figs4_d",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

summary(lm(Rs_annual~delta.Mean.Precip., data = Temperate))
(p <- ggplot(aes_string(x = "delta.Mean.Precip.", y = "Rs_annual"),data = Temperate) + 
    geom_point(fill = '#CCEBC5',size = 2, alpha = 0.3, shape = 21, color = "black",stroke=0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = 'forestgreen', size = 1.3) + #绘制回归直线
    theme_few()+
    ylab(NULL)+ xlab(NULL)+ggtitle("(e) Temperate")+
    scale_y_continuous(breaks = c(0,800,1600,2400,3200), limits = c(0, 3400))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))
ggsave(paste0(workingpath,"figs4_e",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

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
ggsave(paste0(workingpath,"figs4_f",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

##Rs~year over 1987-2008 and 1987-2016
summary(lm(Rs_annual~Study_midyear, data = work.data))
summary(lm(Rs_annual~Study_midyear, data = Bef2008))
(p <- ggplot(aes_string(x = "Study_midyear", y = "Rs_annual", fill = "Year2", color = "Year2"),data = work.data) + 
    geom_point(size = 1.5, alpha = 0.4, shape = 21,stroke = 0.05,color='black') + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, size = 1.2) + 
    theme_few()+
    ylab(expression(paste('Rh(g C ∙ ',m^{-2},')')))+ xlab(NULL)+
    scale_x_continuous(limits = c(1986,2017),breaks = c(1987,1997,2007,2017))+
    expand_limits(y = 0)+        theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in"))+
    scale_fill_manual(values = c("#d9916d","#8DD3C7"))+
    scale_color_manual(values = c("#FB8072","#8DD3C7"))+
    theme(panel.border = element_rect(color = "grey3"),legend.position = c(0.01, 1),legend.justification = c(0.01,1),legend.title = element_blank(),
          legend.background = element_blank(),text=element_text(size=6),
          legend.key.size = unit(20,"pt")))
p <- p+geom_abline(intercept = -5694.257, slope = 3.243, size = 0.5, color = "black", alpha=0.6)
ggsave(paste0(workingpath,"fig_S2",".pdf"), p,width=3.3,height = 2.2,units = 'in', dpi = 900 )