source("0_functions_plot.R")
res.data <- read.csv("04_processed_data_rhra.csv",check.names=FALSE)#Preprocessing has been done
res.data <- filter(res.data,
                   Study_midyear>=1987,
                   Meas_method %in% c('IRGA','Gas Chromatography') 
                   &Ecosystem_type != 'Agriculture')

res.data <- droplevels(res.data)
Rh.data = filter(res.data,!is.na(Rh_annual) & Outlier_rh ==0)
Ra.data = filter(res.data,!is.na(Ra_annual) & Outlier_ra == 0)

#Multivariate model
#Rh
lmformula = "Rh_annual~Study_midyear*Meas_method+Study_midyear*Partition_method+Study_midyear*Latitude+Study_midyear*Elevation+Study_midyear*Stage+Study_midyear*Ecosystem_type2+Study_midyear*SOC_stock+Study_midyear*Biome+MAT*MAP*Biome+delta.Mean.Temp.*delta.Mean.Precip.*Biome"
multi.lm = lm(lmformula ,data = Rh.data,na.action = 'na.omit')
#full model
full.model.1 <-model.cof(multi.lm) 
full.model.anova = anova(multi.lm)
full.model.1$effect <- rownames(full.model.1)
full.model.anova$effect <- rownames(full.model.anova)
full.model.1 = merge(full.model.anova,full.model.1,by = 'effect',all = TRUE)
write.csv(full.model.1,paste0(workingpath,'Rh.fullmodel.csv'),row.names=FALSE)


##Ra
lmformula = "Ra_annual~Study_midyear*Meas_method+Study_midyear*Partition_method+Study_midyear*Latitude+Study_midyear*Elevation+Study_midyear*Stage+Study_midyear*Ecosystem_type2+Study_midyear*SOC_stock+Study_midyear*Biome+MAT*MAP*Biome+delta.Mean.Temp.*delta.Mean.Precip.*Biome"
multi.lm = lm(lmformula ,data = Ra.data,na.action = 'na.omit')
###full model
full.model.1 <-model.cof(multi.lm) 
full.model.anova = anova(multi.lm)
full.model.1$effect <- rownames(full.model.1)
full.model.anova$effect <- rownames(full.model.anova)
full.model.1 = merge(full.model.anova,full.model.1,by = 'effect',all = TRUE)
write.csv(full.model.1,paste0(workingpath,'Ra.fullmodel.csv'),row.names=FALSE)

#Rh~year
summary(lm(Rh_annual~Study_midyear, data = Rh.data))
(p <- ggplot(aes_string(x = "Study_midyear", y = "Rh_annual"),data = Rh.data) + 
    geom_point(fill = 'lightcyan2',size = 1.6, alpha = 0.6,color='black',shape=21,stroke = 0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = 'lightcyan3', size = 1.3) + 
    theme_few()+ggtitle("(a)")+
    ylab(expression(paste('Rh(g C ∙ ',m^{-2},')')))+ xlab("Year")+
    scale_x_continuous(limits = c(1987,2016), breaks = c(1987,1991,1996,2001,2006,2011,2016))+
    expand_limits(y = 0)+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))
ggsave(paste0(workingpath,"fig_4a",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )

#Ra~year
summary(lm(Ra_annual~Study_midyear, data = Ra.data))
(p <- ggplot(aes_string(x = "Study_midyear", y = "Ra_annual"),data = Ra.data) + 
    geom_point(fill = 'darkseagreen2',size = 1.6, color='black',alpha = 0.6,shape=21,stroke = 0.05) + 
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE,color = 'darkseagreen4', size = 1.3) +
    theme_few()+ggtitle("(b)")+
    ylab(expression(paste('Ra(g C ∙ ',m^{-2},')')))+ xlab("Year")+
    scale_x_continuous(limits = c(1987,2016), breaks = c(1987,1991,1996,2001,2006,2011,2016))+
    scale_y_continuous(breaks = c(0,700,1400,2100), limits = c(0, 2100))+
    theme(text = element_text(size=8),panel.border = element_rect(color = "grey3"))+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in")))
ggsave(paste0(workingpath,"fig_4b",".pdf"), p,width=2.2,height = 2,units = 'in', dpi = 900 )
