source("0_functions_plot.R")
res.data <- read.csv("04_processed_data_rhra.csv",check.names=FALSE)#Preprocessing has been done
res.data <- filter(res.data,
                   Study_midyear>=1987,
                   Meas_method %in% c('IRGA','Gas Chromatography') 
                   &Ecosystem_type != 'Agriculture')
workingpath = getwd()
res.data <- droplevels(res.data)
res.data$Year3 = as.character(lapply(res.data$Study_midyear,labelYear3)) %>% as.factor()
Rh.data = filter(res.data,!is.na(Rh_annual) & Outlier_rh ==0)
Ra.data = filter(res.data,!is.na(Ra_annual) & Outlier_ra == 0)

#Multivariate model
#Rh
lmformula = "Rh_annual~I(Study_midyear^2)+Study_midyear*Meas_method+Study_midyear*Partition_method+Study_midyear*Latitude+Study_midyear*Elevation+Study_midyear*Stage+Study_midyear*Ecosystem_type2+Study_midyear*SOC_stock+Study_midyear*Biome+MAT*MAP*Biome+delta.Mean.Temp.*delta.Mean.Precip.*Biome"
multi.lm = lm(lmformula ,data = Rh.data,na.action = 'na.omit')
#full model
full.model.1 <-model.cof(multi.lm) 
full.model.anova = anova(multi.lm)
full.model.1$effect <- rownames(full.model.1)
full.model.anova$effect <- rownames(full.model.anova)
full.model.1 = merge(full.model.anova,full.model.1,by = 'effect',all = TRUE)
write.csv(full.model.1,paste0(workingpath,'/Rh.fullmodel.0526.csv'),row.names=FALSE)
###Stepwise
full.model.1 <-model.cof(multi.lm) 
model.1 <-model.cof(step(multi.lm,direction = "both"))
step.model <- step(multi.lm,direction = "both")
aov.modle.1 <- anova(step(multi.lm,direction = 'both'))
aov.modle.1$effect <- rownames(aov.modle.1)
model.1$effect <- rownames(model.1)
Step.model.1 = merge(aov.modle.1,model.1,by = 'effect',all = TRUE)
write.csv(Step.model.1,paste0(workingpath,'/Rh.stepwise.csv'),row.names=FALSE)


##Ra
lmformula = "Ra_annual~I(Study_midyear^2)+Study_midyear*Meas_method+Study_midyear*Partition_method+Study_midyear*Latitude+Study_midyear*Elevation+Study_midyear*Stage+Study_midyear*Ecosystem_type2+Study_midyear*SOC_stock+Study_midyear*Biome+MAT*MAP*Biome+delta.Mean.Temp.*delta.Mean.Precip.*Biome"
multi.lm = lm(lmformula ,data = Ra.data,na.action = 'na.omit')
###full model
full.model.1 <-model.cof(multi.lm) 
full.model.anova = anova(multi.lm)
full.model.1$effect <- rownames(full.model.1)
full.model.anova$effect <- rownames(full.model.anova)
full.model.1 = merge(full.model.anova,full.model.1,by = 'effect',all = TRUE)
write.csv(full.model.1,paste0(workingpath,'/Ra.fullmodel.0526.csv'),row.names=FALSE)
###Stepwise
full.model.1 <-model.cof(multi.lm) 
model.1 <-model.cof(step(multi.lm,direction = "both"))
step.model <- step(multi.lm,direction = "both")
aov.modle.1 <- anova(step(multi.lm,direction = 'both'))
aov.modle.1$effect <- rownames(aov.modle.1)
model.1$effect <- rownames(model.1)
Step.model.1 = merge(aov.modle.1,model.1,by = 'effect',all = TRUE)
write.csv(Step.model.1,paste0(workingpath,'/Ra.stepwise.csv'),row.names=FALSE)

#Rh~year
summary(lm(Rh_annual~Study_midyear, data = Rh.data))
summary(lm(Rh_annual~Study_midyear, data = filter(Rh.data,Year3 == "1987-1999")))
summary(lm(Rh_annual~Study_midyear, data = filter(Rh.data,Year3 == "2000-2016")))
(p <- ggplot(aes_string(x = "Study_midyear", y = "Rh_annual", fill = "Year3", color = "Year3"),data = Rh.data) + 
    geom_point(size = 1.6, alpha = 0.4,color='black',shape=21,stroke = 0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, size = 0.8) + 
    theme_few()+ggtitle("(a)")+
    ylab(expression(paste('Rh(g C·',m^{-2},')')))+ xlab("Year")+
    scale_x_continuous(limits = c(1987,2016), breaks = c(1987,1991,1996,2001,2006,2011,2016))+
    expand_limits(y = 0)+
    theme(text = element_text(size=6),panel.border = element_rect(color = "grey3"),
          legend.position = c(0.25,0.83), legend.title = element_blank(),legend.key.height = unit(0.1, "in"),
          legend.key.width = unit(0.2,"in"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in"))+
    scale_fill_manual(values = c('darkorchid4','mediumpurple2'))+
    scale_color_manual(values = c('darkorchid4','mediumpurple2')))
    #geom_abline(intercept = -25039.738, slope = 12.726, color = "mediumpurple4"))
ggsave(paste0(workingpath,"/fig_4a_0526_2periods",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )


#Ra~year
summary(lm(Ra_annual~Study_midyear, data = Ra.data))
summary(lm(Ra_annual~Study_midyear, data = filter(Ra.data,Year3 == "1987-1999")))
summary(lm(Ra_annual~Study_midyear, data = filter(Ra.data,Year3 == "2000-2016")))
(p <- ggplot(aes_string(x = "Study_midyear", y = "Ra_annual", fill = "Year3", color = "Year3"),data = Ra.data) + 
    geom_point(size = 1.6,color='black',alpha = 0.4,shape=21,stroke = 0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, size = 0.8) +
    theme_few()+ggtitle("(b)")+
    ylab(expression(paste('Ra(g C·',m^{-2},')')))+ xlab("Year")+
    scale_x_continuous(limits = c(1987,2016), breaks = c(1987,1991,1996,2001,2006,2011,2016))+
    scale_y_continuous(breaks = c(0,700,1400,2100), limits = c(0, 2100))+
    theme(text = element_text(size=6),panel.border = element_rect(color = "grey3"),
          legend.position = c(0.25,0.83), legend.title = element_blank(),legend.key.height = unit(0.1, "in"),
          legend.key.width = unit(0.2,"in"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in"))+
    scale_fill_manual(values = c('darkorchid4','mediumpurple2'))+
    scale_color_manual(values = c('darkorchid4','mediumpurple2')))#+
    #geom_abline(intercept = 1076.4629, slope = -0.3535, color = "mediumpurple4"))
ggsave(paste0(workingpath,"/fig_4b_0526_2periods",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

rh_1 = filter(Rh.data, Study_midyear < 2000)
rh_2 = filter(Rh.data, Study_midyear >= 2000)
ra_1 = filter(Ra.data, Study_midyear < 2000)
ra_2 = filter(Ra.data, Study_midyear >= 2000)
summary(lm(Rh_annual~Study_midyear, rh_1))
summary(lm(Rh_annual~Study_midyear, rh_2))
summary(lm(Ra_annual~Study_midyear, ra_1))
summary(lm(Ra_annual~Study_midyear, ra_2))

#Single period 20201014
###Ra
(p <- ggplot(aes_string(x = "Study_midyear", y = "Ra_annual"),data = Ra.data) + 
        geom_point(size = 1.6,alpha = 0.4, fill = 'darkorchid4', color = 'darkorchid4') + 
        geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, size = 0.8, color = 'darkorchid4', fill = 'darkorchid4') +
        theme_few()+ggtitle("(b)")+
        ylab(expression(paste('Ra(g C·',m^{-2},')')))+ xlab("Year")+
        scale_x_continuous(limits = c(1987,2016), breaks = c(1987,1991,1996,2001,2006,2011,2016))+
        scale_y_continuous(breaks = c(0,700,1400,2100), limits = c(0, 2100))+
        theme(text = element_text(size=6),panel.border = element_rect(color = "grey3"),
              legend.position = c(0.25,0.83), legend.title = element_blank(),legend.key.height = unit(0.1, "in"),
              legend.key.width = unit(0.2,"in"))+
        theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in"))+
        scale_fill_manual(values = c('darkorchid4','mediumpurple2'))+
        scale_color_manual(values = c('darkorchid4','mediumpurple2')))#+
#geom_abline(intercept = 1076.4629, slope = -0.3535, color = "mediumpurple4"))
ggsave(paste0(workingpath,"/fig_4b_0526_2periods",".pdf"), p,width=3,height = 2,units = 'in', dpi = 900 )

#Single period 20201014
(p <- ggplot(aes_string(x = "Study_midyear", y = "Rh_annual"),data = Rh.data) + 
        geom_point(size = 1.6,alpha = 0.4, fill = 'darkorchid4', color = 'darkorchid4') + 
        geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, size = 0.8, color = 'darkorchid4', fill = 'darkorchid4') +
        theme_few()+ggtitle("(b)")+
        ylab(expression(paste('Ra(g C·',m^{-2},')')))+ xlab("Year")+
        scale_x_continuous(limits = c(1987,2016), breaks = c(1987,1991,1996,2001,2006,2011,2016))+
        scale_y_continuous(breaks = c(0,700,1400,2100), limits = c(0, 2100))+
        theme(text = element_text(size=6),panel.border = element_rect(color = "grey3"),
              legend.position = c(0.25,0.83), legend.title = element_blank(),legend.key.height = unit(0.1, "in"),
              legend.key.width = unit(0.2,"in"))+
        theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in"))+
        scale_fill_manual(values = c('darkorchid4','mediumpurple2'))+
        scale_color_manual(values = c('darkorchid4','mediumpurple2')))#+
#geom_abline(intercept = 1076.4629, slope = -0.3535, color = "mediumpurple4"))
ggsave(paste0(workingpath,"/fig_3d",".pdf"), p,width=3,height = 2,units = 'in', dpi = 900 )

library("mblm")
summary(mblm(Ra_annual~Study_midyear, data = Ra.data))
summary(mblm(Rh_annual~Study_midyear, data = Rh.data))
summary(mblm(Rs_annual~Study_midyear, data = work.data))
