source("0_functions_plot.R")
source("4_statistical_prep.R")

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
climate.data$Year = as.character(lapply(climate.data$Study_midyear,labelYear1)) %>% as.factor()
climate.data$Biome = factor(climate.data$Biome, levels=rev(levels(climate.data$Biome)))
climate.data$delta.precip=climate.data$delta.precip*12 
climate.data = filter(climate.data,!is.na(delta.precip))
idv = c('Temp.annual','Precip.annual','delta.temp','delta.precip')

attach(climate.data)
summary(climate.data$Study_midyear)
length(climate.data$Study_midyear)
summary(climate.data$delta.temp)
length(climate.data$delta.temp)

#delta temperature
(p2 <- ggplot(aes(x = Study_midyear, y = delta.temp ,color = Year),shape=16,data = climate.data) + 
    geom_point(size = 1, alpha = 0.2)+scale_color_manual(values = c('#b2e2e2','#66c2a4','#238b45'))+
    ylab(expression(paste('Delta MAT (°C ∙ ',year^{-1},')')))+xlab(NULL)+theme_few()+
    scale_x_continuous(breaks = c(1987,1996,2006,2016))+
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, size = 1,color='darkgrey') + 
    theme(panel.border = element_rect(color = "grey3"),text=element_text(size=6),
          legend.position = "none",
          legend.title = element_blank(),
          legend.background = element_blank()))+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in"))
ggsave(paste0(workingpath,'fig_S3_a',".trend.pdf"), p2,width=3.25,height = 2.6,units = 'in', dpi = 900 )

#delta precipitation
(p2 <- ggplot(aes(x = Study_midyear, y = delta.precip ,color = Year),shape=16,data = climate.data) + 
    geom_point(size = 1, alpha = 0.2)+scale_color_manual(values = c('#b2e2e2','#66c2a4','#238b45'))+
    ylab(expression(paste('Delta MAP (mm ∙ ',year^{-1},')')))+xlab(NULL)+theme_few()+
    scale_x_continuous(breaks = c(1987,1996,2006,2016))+
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, size = 1,color='darkgrey') + 
    theme(panel.border = element_rect(color = "grey3"),text=element_text(size=6),
          legend.position = "none",
          legend.title = element_blank(),
          legend.background = element_blank()))+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"in"))
ggsave(paste0(workingpath,'fig_S3_b',".trend.pdf"), p2,width=3.25,height = 2.6,units = 'in', dpi = 900 )

