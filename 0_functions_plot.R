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
library("segmented")
library("trend")
library("mblm")
#if function masked, please run it again

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
labelYear3 <- function(x){
  if (x < 2000)
  {x = "1987-1999"} 
  #else if (x <= 2006){x = "1997-2006"}
  else {x = "2000-2016"}
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
mblm_eqn <- function(df){
  y = df[,2]
  x = df[,3]
  m <- mblm(y ~ x);
  eq=c(a = format(coef(m)[2], digits = 2),
       p = format(summary(m)$coefficients[2,4],digits = 2))
  return(eq)
}
### win scan function, need lm_eqn
win_scan = function(dt,var,step=1,size=10){
  win_left=win_right=min(dt[[var]])
  win_range = Slope = R.square = P.value = Num = win_mid = data_mid = c()
  # num:the counts of data in win
  # data_mid: the mid point of used data in win
  # win_mid: the mid point of win_range
  while(win_right<max(dt[[var]])){
    win_right=win_left+size
    # process windata
    win_data=dt[dt[[var]] >= win_left & dt[[var]] < win_right,] # excluded the data within edge
    win_range=c(win_range,paste0(win_left,'-',win_right))
    win_mid = c(win_mid,(win_left+win_right)/2) %>% as.numeric(.)
    data_mid=c(data_mid,(max(win_data[[var]])+min(win_data[[var]]))/2) %>% as.numeric(.)
    # lm method depend on the lm-result funtion lm_eqn
    eq= lm_eqn(df=win_data)
    Slope = c(Slope,eq[1]) %>% as.numeric(.)
    R.square=c(R.square,eq[2])
    P.value=c(P.value,eq[3])
    Num = c(Num,nrow(win_data))
    # 
    win_left=win_left+step
  }
  winresult = data.frame(win_range,win_mid,data_mid,Slope,R.square,P.value,Num)
}

# win_step. enlarge the win step by step
win_step = function(dt,var,step=1,size=10){
  
  win_left=min(dt[[var]])
  win_right=win_left+size
  win_range = Slope = R.square = P.value = Num = win_mid = data_mid = c()
  # num:the counts of data in win
  # data_mid: the mid point of used data in win
  # win_edge: the right edge of win
  while(win_right<max(dt[[var]])){
    
    # process windata
    win_data=dt[dt[[var]] >= win_left & dt[[var]] <= win_right,] # included the data within edge
    win_range=c(win_range,paste0(win_left,'-',win_right))
    win_edge = c(win_edge,win_right) %>% as.numeric(.)
    data_mid=c(data_mid,(max(win_data[[var]])+min(win_data[[var]]))/2) %>% as.numeric(.)
    # lm method depend on the lm-result funtion lm_eqn
    eq= lm_eqn(df=win_data)
    Slope = c(Slope,eq[1]) %>% as.numeric(.)
    R.square=c(R.square,eq[2])
    P.value=c(P.value,eq[3])
    Num = c(Num,nrow(win_data))
    # 
    #win_left=win_left+step
    win_right=win_right+step
  }
  winresult = data.frame(win_range,win_edge,data_mid,Slope,R.square,P.value,Num)
}

