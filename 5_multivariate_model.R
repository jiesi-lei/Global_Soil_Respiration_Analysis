source("0_functions_plot.R")
source("4_statistical_prep.R")
workingpath = getwd()
dir.create(paste0(workingpath,"/ModelTable")) 


lmformula = "Rs_annual~Study_midyear*Meas_method+Study_midyear*Latitude+Study_midyear*Elevation+Study_midyear*Stage+Study_midyear*Ecosystem_type2+Study_midyear*SOC_stock+Study_midyear*Biome+MAT*MAP*Biome+delta.Mean.Temp.*delta.Mean.Precip.*Biome"

###full model
multi.lm = lm(lmformula ,data = work.data,na.action = 'na.omit')
full.model.1 <-model.cof(multi.lm) 
full.model.anova = anova(multi.lm)
full.model.1$effect <- rownames(full.model.1)
full.model.anova$effect <- rownames(full.model.anova)
full.model.1 = merge(full.model.anova,full.model.1,by = 'effect',all = TRUE)
write.csv(full.model.1,paste0(workingpath,'/ModelTable/','Rs.fullmodel.year.csv'),row.names=FALSE)
