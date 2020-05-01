# Template for R analysis script
library('ggplot2')
library('RColorBrewer')
library('dplyr') 
library("Hmisc")
library('magrittr')
library('tidyr')
library('luzlogr')
library('R.utils')
library('assertthat')
library('ggthemes')
#if funciton masked, please run it again

###-----------------------------------
#label year periods
labelLat <- function(x){
  if (abs(x) <= 30){x = "0-30"} 
  else if (abs(x) <= 50){x = "30-50"} 
  else {x = "50-"}
  return(x)}
labelYear1 <- function(x){
  if (x < 1997)
  {x = "1987-1996"} 
  else if (x < 2007){x = "1997-2006"}
  else {x = "2007-2016"}
  return(x)}
labelYear2 <- function(x){
  if (x <= 2008)
  {x = "1960-2008"} 
  #else if (x <= 2006){x = "1997-2006"}
  else {x = "2008-2016"}
  return(x)}
labelYear2 <- function(x){
  if (x <= 2008)
  {x = "1960-2008"} 
  #else if (x <= 2006){x = "1997-2006"}
  else {x = "2008-2016"}
  return(x)}
labelSOC <- function(x){
  if (x <= 120)
  {x = "0-120"} 
  else if (x <= 180){x = "121-180"}
  else if (x <= 320){x = "181-320"}
  else {x = "320-"}
  return(x)}
#Extract model coefficients
###-------------------------------
lm.cof <- function(df,y,x){
  Eq = as.formula(paste0(y,'~',x))
  m <- summary(lm(Eq, df))
  output = as.data.frame(m$coefficients)[-1,]
  output['Total R.squared',1] =m$adj.r.squared
  return(output)
}
model.cof <- function(mdata){
  m <- summary(mdata)
  output = as.data.frame(m$coefficients)[-1,]
  output['Total',c(1:4)] = c('R.squared',m$adj.r.squared,'Counts',length(m$residuals))
  return(output)
}

###annote equation function in plot
###-------------------------------
lm_eqn <- function(df){
  m <- lm(df[,2] ~ df[,3], df);
  eq=c(a = format(coef(m)[2], digits = 2),
       r2 = format(summary(m)$r.squared, digits = 2),
       p = format(summary(m)$coefficients[2,4],digits = 2))
  return(eq)
}
