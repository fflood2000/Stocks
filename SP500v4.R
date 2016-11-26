#
# Stock predictor program --
# Paramater Overview:
# -> stock.list0 = list of stock data input from Yahoo finance (csv files).
#             names(stock.list0) = csv filenames. 
#         ncol = 7 (Open, Close, VHigh, Low, Volume,...), nrow = # years x 365(days)
#         length(stock.list0) = number of stocks
# -> stock.list1 = list0 with date converted to standard format
# -> stock.list2 = 12-member list (delta price, Volume, ...) for days, weeks and months
#             see details in Section-2.
#
#
#
library(lubridate)
#setwd("~/Desktop/Data Science/Projects")
sp.data = read.csv(file="Ind_sp500.csv",header=TRUE,sep=",")
hs.data = read.csv(file="Ind_HengSeng.csv",header=TRUE,sep=",")
dax.data = read.csv(file="Ind_DAX.csv",header=TRUE,sep=",")
n225.data = read.csv(file="Ind_N225.csv",header=TRUE,sep=",")
ssmi.data = read.csv(file="Ind_ssmi.csv",header=TRUE,sep=",")
sti.data = read.csv(file="Ind_sti.csv",header=TRUE,sep=",")
#
stock.list0 = list('sp500'=sp.data,'Nasdaq'=dax.data,'Hangseng'=hs.data,
             'Nekkei'=n225.data,'ssmi'=ssmi.data,
             'sti'=sti.data)
#
#head(sp.data); dim(sp.data); summary(sp.data)
#===================================================
# SECTION-1: Transform data files ( fix dates..)
#--------------------------------------------------
#---Function: Transform Date Format  ----------------
fixdate = function(data){
  tmp = as.Date(data[,1], format="%m/%d/%y")
  bad = (tmp > "2050-1-1")
  x2 = as.Date(format(tmp[bad],"19%y-%m-%d"))
  x1 = (tmp[!bad])
  xt = c(x1,x2)
  data[,1] = xt
  return(data)
  #summary(sp.data)
  #plot(data[,1],log(data$Volume))
  #
}
stock.list0[[5]] = stock.list0[[5]][!is.na(stock.list0[[5]][,2]),]
stock.list1 = lapply(stock.list0,fixdate)
names(stock.list1) = names(stock.list0)
#==================================================================
#SECTION-2: Functions to generate daily, weekly and monthly values
#   for prediction. Also generate backtestint data.
#--- Function: Collect/sumarize Data for all years-------------
create_corr_data = function(all.data,minyr,maxyr,v){
  # Define data frames ---
  n.day = numeric(1); miss.day = numeric(1)
  vol.day = data.frame(c(seq(1,365,1)))
  delta.day = data.frame(c(seq(1,365,1)))
  high.day = data.frame(c(seq(1,365,1)))
  low.day = data.frame(c(seq(1,365,1)))
  open.day = data.frame(c(seq(1,365,1)))
  close.day = data.frame(c(seq(1,365,1)))
  day.day = data.frame(c(seq(1,365,1)))
  t.test = data.frame(c(seq(1,365,1)))
  day_na.day = data.frame(c(seq(1,365,1)))
  #
  delta.wk = data.frame(c(seq(1,52,1)))
  high.wk = data.frame(c(seq(1,52,1)))
  low.wk = data.frame(c(seq(1,52,1)))
  vol.wk = data.frame(c(seq(1,52,1)))
  #
  delta.mth = data.frame(c(seq(1,12,1)))
  high.mth = data.frame(c(seq(1,12,1)))
  low.mth = data.frame(c(seq(1,12,1)))
  vol.mth = data.frame(c(seq(1,12,1)))
  #----------------------
  yr = 2014; k = 1
  for (yr in maxyr:minyr){  
    #Seperate stock data by year (Indexed by Staock dates. Problems!!)--
    yr.data = all.data[year(all.data$Date) == yr,] # dim(tmp1.dat)
    j=1; i=1
    #--- Data Cleaning and ETL for Day/Week/Month ---- 
    #browser()
    if (v==1){
      day.dat = st.day(yr.data,i,j) # <---Day storage function
      wk.dat = st.week(yr.data)
      mth.dat = st.month(yr.data)
    }
    else{
      day.dat = st.day1(yr.data,i,j) # <---Day storage function
      wk.dat = st.week1(yr.data)
      mth.dat = st.month1(yr.data)
    }
    #---------------------
    #Collect results from each year in data frames
    delta.day[,k] = rev(day.dat[[1]])
    vol.day[,k] = rev(day.dat[[2]])
    high.day[,k] = rev(day.dat[[3]])
    low.day[,k] = rev(day.dat[[4]])
    open.day[,k] = rev(day.dat[[5]])
    close.day[,k] = rev(day.dat[[6]])
    day.day[,k] = rev(day.dat[[7]])
    day_na.day[,k] = rev(day.dat[[8]])
    delta.wk[,k] = rev(wk.dat[[1]])
    delta.mth[,k] = rev(mth.dat[[1]])
    #
    names(delta.day)[k] = as.character(yr); names(vol.day)[k] = as.character(yr)
    names(high.day)[k] = as.character(yr); names(low.day)[k] = as.character(yr)
    names(open.day)[k] = as.character(yr); names(close.day)[k] = as.character(yr)
    names(day.day)[k] = as.character(yr); names(day_na.day)[k] = as.character(yr)
    #
    names(delta.wk)[k] = as.character(yr); names(delta.mth)[k] = as.character(yr)
    #browser()
    if (v==1){
      high.wk[,k] = rev(wk.dat[[2]])
      high.mth[,k] = rev(mth.dat[[2]])
      low.wk[,k] = rev(wk.dat[[3]])
      low.mth[,k] = rev(mth.dat[[3]])
      vol.wk[,k] = rev(wk.dat[[4]])
      vol.mth[,k] = rev(mth.dat[[4]])
      #
      names(vol.wk)[k] = as.character(yr); names(vol.mth)[k] = as.character(yr)
      names(high.wk)[k] = as.character(yr); names(high.mth)[k] = as.character(yr)
      names(low.wk)[k] = as.character(yr); names(low.mth)[k] = as.character(yr)
    }
    #
    k = k + 1
    #print(yr)
  }
  #
  if (v==1){
    op = list(delta.day,vol.day,high.day,low.day,open.day,close.day,day.day,day_na.day,
              delta.wk,vol.wk,high.wk,low.wk,
              delta.mth,vol.mth,high.mth,low.mth)
  }# I have forgotten why I have the v==1 condition...
  else{
    #browser()
    op = list(delta.day,vol.day,hgh.day,low.day,open.day,close.day,day.day,
              delta.wk,delta.mth)
  }
  return(op)
}
#----------Functons: Store Values by day, week and Month  1/1 to 12/31 ---
#  -> st.day: Returns delta price, volume, High & Low
st.day = function(tmp.dat,i,j){
  ndays = numeric(1); miss = numeric(1)
  delta=numeric(1); dvol=numeric(1)
  hgh=numeric(1); lw=numeric(1)
  opn=numeric(1); cls=numeric(1)
  vol=numeric(1)
  #
  t=seq(from=as.Date("2015-1-1"),to=as.Date("2015-1-1")+364,by="day")
  t_na=seq(from=as.Date("2015-1-1"),to=as.Date("2015-1-1")+364,by="day")
  ny.eve = c(as.character(year(max(tmp.dat$Date))),"12-31")
  d0 = as.Date(paste(ny.eve, collapse="-")) #Set start date to 12-31-xxxx
  #
  #browser()
  for (l in 1:365){
    #
    if (d0 == tmp.dat$Date[i]){
      delta[j] = 100 * (tmp.dat$Close[i] - tmp.dat$Open[i]) / tmp.dat$Close[i]
      #delta[j] = 100 * (tmp.dat$Close[i] - tmp.dat$Close[i+1]) / tmp.dat$Close[i]
      dvol[j] = 100 * (tmp.dat$Volume[i] - tmp.dat$Volume[i+1])/ tmp.dat$Volume[i]
      #
      hgh[j] = tmp.dat$High[i]; lw[j] = tmp.dat$Low[i]
      opn[j] = tmp.dat$Open[i]; cls[j] = tmp.dat$Close[i]
      vol[j] = tmp.dat$Volume[i]
      t_na[j] = tmp.dat$Date[i]
      t[j] = d0
      if (i < length(tmp.dat$Date)){
        i = i+1
      }
    }
    else{
      delta[j] = NA
      dvol[j] = NA
      t[j] = d0 ;t_na[j] = NA
      hgh[j] = NA; lw[j] = NA
      opn[j] = NA; cls[j] = NA
      vol[j] = NA
    }
    d0 = d0 - 1; j = j+1
    #-Check for Leap Year --
    if (month(tmp.dat$Date[i])==2 & day(tmp.dat$Date[i])==29){
      #delta[j-1] = 100 * (tmp.dat$Close[i-1] - tmp.dat$Close[i+1])/tmp.dat$Close[i-1]
      dvol[j] = 100 * (tmp.dat$Volume[i-1] - tmp.dat$Volume[i+1])/ tmp.dat$Volume[i-1]
      i = i + 1; d0 = d0 - 1
      print(paste("leap year in",year(d0)))
    }
  }
  ndays = i-1
  miss = sum(is.na(delta))
  #op.day = list(delta,t,ndays,miss)
  op.day = list(delta, vol, hgh, lw, opn, cls, t, t_na)
  #browser()
  return(op.day)
}
st.week = function(tmp.dat){
  #
  val = numeric(1); vol = numeric(1)
  hgh = numeric(1); lw = numeric(1)
  #
  maxwk = week(tmp.dat$Date[1])
  data = tmp.dat[week(tmp.dat$Date) == maxwk,]
  if (length(data$Close) < 3){maxwk = maxwk - 1} 
  i = 1
  for (wk in maxwk:1){
    data = tmp.dat[week(tmp.dat$Date) == wk,]
    #browser()
    if (length(data$Close) > 2){
      val[i] = (data$Close[data$Date == max(data$Date)] - data$Open[data$Date == min(data$Date)])/data$Close[data$Date == max(data$Date)]
      hgh[i] = max(data$High)
      lw[i] = min(data$Low)
      vol[i] = mean(data$Volume)
      i = i + 1
    }
    else{
      val[i] = NA; hgh[i] = NA
      lw[i] = NA; vol[i] = NA; i = i + 1
    }
    #
  }
  if (maxwk < 52){
    for (j in (maxwk+1):52){
      val[j] = NA; hgh[j] = NA
      lw[j] = NA; vol[j] = NA
    }
  }
  #print("week done...")
  #browser()
  wkop = list(val,hgh,lw,vol)
  return(wkop)
}
st.month = function(tmp.dat){
  #
  val = numeric(1); vol = numeric(1)
  hgh = numeric(1); lw = numeric(1)
  #
  maxmth = month(tmp.dat$Date[1])
  for (mth in maxmth:1){
    data = tmp.dat[month(tmp.dat$Date) == mth,]
    val[mth] = (data$Close[data$Date == max(data$Date)] - data$Open[data$Date == min(data$Date)])/data$Close[data$Date == max(data$Date)]
    hgh[mth] = max(data$High)
    lw[mth] = min(data$Low)
    vol[mth] = mean(data$Volume)
    #
  }
  #print("month done...")
  mthop = list(val,hgh,lw,vol)
  return(mthop)
}
#--- Function: Orgabize date for plotting
createplot.data = function (data){
  k = 1
  xt = seq(1:365)
  yrs = as.numeric(names(data))
  for (yr in max(yrs):min(yrs)){
    yt = rep(yr,365)
    if (k > 1){
      x = append(x,xt)
      y = append(y,yt)
      z = append(z,data[,k])
    }
    else{
      x = seq(1:365)
      y = yt
      z = data[,k]
    }
    k = k + 1
  }
  #browser()
  op = list(x,y,z)
  return(op)
}
#----Function: generate data for backtesting  
create_backtest_data = function(all.data,yr){
  #Seperate stock data by year (Indexed by Staock dates. Problems!!)--
  #yr = 2014
  #all.data = sp.data1
  tmp.dat = all.data[year(all.data$Date) == yr,] # dim(tmp1.dat)
  #tmp.dat = yr.data
  
  #ndays = numeric(1); miss = numeric(1)
  v_close=numeric(1); v_open=numeric(1)
  t=seq(from=as.Date("2014-1-1"),to=as.Date("2014-1-1")+364,by="day")
  cal = t
  ny.eve = c(as.character(year(max(tmp.dat$Date))),"12-31")
  d0 = as.Date(paste(ny.eve, collapse="-")) #Set start date to 12-31-xxxx
  #
  #browser()
  j=1; i=1
  for (l in 1:365){
    #
    if (d0 == tmp.dat$Date[i]){
      v_close[j] = tmp.dat$Close[i]
      v_open[j] = tmp.dat$Open[i]
      t[j] = tmp.dat$Date[i]
      if (i < length(tmp.dat$Date)){
        i = i+1
      }
    }
    else{
      v_close[j] = NA
      v_open[j] = NA
      t[j] = NA
    }
    d0 = d0 - 1; j = j+1
    #-Check for Leap Year --
    if (month(tmp.dat$Date[i])==2 & day(tmp.dat$Date[i])==29){
      #v_close[j-1] = tmp.dat$Close[i-1] 
      #v_open[j] = tmp.dat$Open[i-1]
      i = i + 1; d0 = d0 - 1
      print(paste("leap year in",year(d0)))
    }
  }# End for loop
  #
  ndays = i
  miss = sum(is.na(t))
  bt_frame = data.frame(cal.date = cal,business.day = rev(t),Open.price = rev(v_open), Close.price = rev(v_close))
  op.day = list(bt_frame,yr, ndays, miss)
  #browser()
  return(op.day)
}
#---

#--- MAIN CODE; Calls function to create backtest data
# -- argument is xxx.list1 (not xxx.list2)
backtest.year = 2015
backtest.data = create_backtest_data(stock.list1[[1]],backtest.year)
# Returns list with 4 terms: (i) backtest data frame with open and close values
#  (ii) year , (iii) number of business days and (iv) number non-buisness days
#
# -- Functions Call to Store Data  -------
#
# Function calls returns 13-member list into stock.list2():
# Each member is data frame of Values (e.g. 52 rows for week) x Year (columns)
# 1-4 Day Data: (1) delta, (2) Volume, (3) High, (4) Low (5) Open, (6) Close, (7) Day (8)Day_NA
# 9-12 Week Data: (9) delta, (10) Avg. Volume, (11) Max. High, (12) Min. Low
# 13-16 Month Data: (13) delta, (14) Avg. Volume, (15) Max. High, (16) Min. Low
#
#dat.name1 = c("sp.data1","hs.data1","dax.data1","n225.data1","rtsi.data1","
#               sti.data1","c.eur","c.jpy","c.cny")
stock.list2 = list(); maxyr = 2015
for (j2 in 1:length(stock.list1)){
  stock.list2[[j2]] = create_corr_data(stock.list1[[j2]],min(year(stock.list1[[j2]]$Date)),maxyr,1)
}
names(stock.list2) = names(stock.list1)

#=====================================================================
#SECRION 3: Correlate changes in feature stocks to changes oin target stock
#------------------------------
#---- Function: Performa cross correlation between two data sets
corr.stat = function(datay,datax,yvar,xvar,lag){
  maxlag = lag#; browser()
  kx = xvar; ky = yvar
  maxcol = min(length(names(datay[[ky]])),length(names(datax[[kx]])))
  yrs = as.numeric(names(datax[[kx]]))
  if (maxcol > length(names(datax[[kx]]))) {yrs = as.numeric(names(datay[[ky]]))}
  #
  corr = matrix(nrow = maxcol, ncol = (2*maxlag + 2))
  #browser()
  corr[,1] = yrs
  #
  for (j in 1:maxcol){
    ydat = datay[[ky]][,j]; xdat = datax[[kx]][,j]
    #auto.cor = acf(ydat, lag.max=maxlag, na.action=na.pass,plot=FALSE,type="correlation")
    cross.cor = ccf(ydat, xdat, lag.max=maxlag,na.action=na.pass,plot=FALSE,type="correlation")
    corr[j,2:(2*maxlag+2)] = cross.cor$acf
    #browser()
  }
  #browser()
  corr1 = as.data.frame(corr)
  n = c("Year",as.character(cross.cor$lag))
  names(corr1) = n
  #browser()
  return(corr1)
}
#----------
# Function: compute mean and var of correlation at each time lage (col)
#   over all years (rows)
corr_meanvar = function(df1){ 
  mn = numeric(0); vr = numeric(0)
  for (j in 2:(ncol(df1))){
    mn[j-1] = mean(df1[,j],na.rm=TRUE)
    vr[j-1] = var(df1[,j],na.rm=TRUE)
  }
  op = list(mean = mn, variance = vr)
}
# -----Stock Indices has 12-member list:
# # 1-4 Day Data: (1) delta, (2) Volume, (3) High, (4) Low (5) Open, (6) Close, (7) Day (8)Day_NA
# 9-12 Week Data: (9) delta, (10) Avg. Volume, (11) Max. High, (12) Min. Low
# 13-16 Month Data: (13) delta, (14) Avg. Volume, (15) Max. High, (16) Min. Low
#
#
# ===MAIN CODE: Summarizs the cross-correlations (col) for each year (row).
# Then computes the mean and variance of x-correlations for each offset
# across all years to generate a correlation statistic.
# corr.day/wk/mth : Each list element is data frame rows=year x col=time lag
# stat.dat/wk.mth: Each element has mean & var for each time lag for all years
#
# File Structure:
# -> corr.day/wk/mth = Cross correlation value (for time N-day/week/month offest) 
#         for eah year. nrows =  # years, ncol = -maxlag to +maxlaog
#         Since 1st stock is target stock, 1st term is autocorrelation.
# -> stat.day/wk/mth = mean and variance of x-correlation values (for each offset)
#         computed acroos all years. 
#
corr.day = list(); stat.day = list()
corr.wk = list(); stat.wk = list()
corr.mth = list(); stat.mth = list()
# 
yr = 2015
corr_data = stock_data[year(stock_data[,1])==yr,c(1:2,9,16,23,30,37)]


for (j in 1:length(stock.list2)){
  target = stock.list2[[1]]
  maxlag = 5
  corr.day[[j]] = corr.stat(target,stock.list2[[j]],1,1,maxlag)
  stat.day[[j]] = corr_meanvar(corr.day[[j]])
  #browser()
  corr.wk[[j]] = corr.stat(target,stock.list2[[j]],9,9,maxlag)
  stat.wk[[j]] = corr_meanvar(corr.wk[[j]])
  maxlag = 3
  corr.mth[[j]] = corr.stat(target,stock.list2[[j]],13,13,maxlag)
  stat.mth[[j]] = corr_meanvar(corr.mth[[j]])
}
#
names(corr.day) = names(stock.list2); names(stat.day) = names(stock.list2)
names(corr.wk) = names(stock.list2); names(stat.wk) = names(stock.list2)
names(corr.mth) = names(stock.list2); names(stat.mth) = names(stock.list2)
x.daywk = seq(from = -5, to=5); x.mth = seq(from = -3, to=3)
#
# Function: plot correlation (average of all years) vs time lag, with sd error bars
corr.plot = function(corr,lag,ind){
  x = seq(from=-lag, to=lag)
  points(x, corr$mean, pch = ind, col = ind)
  ybar.min = corr$mean - 0.5*sqrt(corr$var)
  ybar.max = corr$mean + 0.5*sqrt(corr$var)
  segments(x, ybar.min, x, ybar.max)
}
#
plot(x.daywk,stat.day[[1]]$mean,pch=19,type="b",ylim=range(0:1))
for (j3 in 1:length(stat.day)){
  corr.plot(stat.day[[j3]],5,j3)
}
#
#=====================================================================
#SECRION 4: Stock price change classification (SVM & Trees)
#-------------------------------------------------------------------
# Predict.data: list of data frames fr prediction. Each list element is dara for 1 stock
#    Each element contains data frame ncol=5 (Date, delta price, Volume, High, Low)
#    Date for non-business days = NA. Dates range from 2014 to earliest available date
#    (different for each stock)
#
#----Function: generate data for daily (not weekly or monthly) prediction
create_prediction_data = function(stock.list){
  feature.set = list()
  #browser()
  for (stock in 1:length(stock.list)){
    feature.set[[stock]] = data.frame()
    for (yr in 1:ncol(stock.list[[stock]][[1]])){
      #browser()
      new.data = data.frame(date = rev(stock.list[[stock]][[7]][,yr]), 
                            change=rev(stock.list[[stock]][[1]][,yr]), 
                            volume=rev(stock.list[[stock]][[2]][,yr] / 
                                         max(stock.list[[stock]][[2]][,yr],na.rm=TRUE)), 
                            high=rev(stock.list[[stock]][[3]][,yr]/
                                       max(stock.list[[stock]][[3]][,yr],na.rm=TRUE)),
                            low=rev(stock.list[[stock]][[4]][,yr] / 
                              max(stock.list[[stock]][[4]][,yr],na.rm=TRUE)),
                            open = rev(stock.list[[stock]][[5]][,yr]),
                            close = rev(stock.list[[stock]][[6]][,yr]))
      #select daily feature columns for 1 year
      #
      feature.set[[stock]] = rbind(feature.set[[stock]],new.data)#; print(stock)
    }
  }
  names(feature.set) = names(stock.list)
  return(feature.set)
}
#---
predict.data = create_prediction_data(stock.list2)
stock_sp500 = predict.data[[1]]
stock_Hengseng = predict.data[[2]]
stock_Nasdaq = predict.data[[3]]
stock_Nekkei = predict.data[[4]]
stock_ssmi = predict.data[[5]]
stock_sti = predict.data[[6]]
#
npts = 7000
for (js in 1: length(predict.data)){
  for (jcol in 1:ncol(predict.data[[js]])){
    colnames(predict.data[[js]])[jcol] = paste(names(predict.data[js]),
                                        colnames(predict.data[[js]][jcol]),sep="_")
  }
  #browser()
  if (js == 1) stock_data = predict.data[[js]][1:npts,]
  if (js > 1) stock_data = cbind(stock_data,predict.data[[js]][1:npts,])
}
#------------------------
# -- Support Vector Classification
library(e1071)
npts = 7000
svm.data = data.frame(predict.data[[1]][1:npts,1])
for (js in 1: length(predict.data)){
  svm.data = cbind(svm.data,predict.data[[js]][1:npts,2])
}
colnames(svm.data) = c("Date", names(predict.data))
svm.data[,2] = as.factor(sign(svm.data[,2]))
plot(svm.data[,3:7],col=4-as.numeric(svm.data[,2]))
svm.data_noNA = svm.data[!rowSums(is.na(svm.data)), ]
#
date1 = "2015-10-01"; date1x = as.Date(date1)
date2 = "2015-11-01"; date2x = as.Date(date2)

train.data = svm.data_noNA[(svm.data_noNA$Date>date1x)&(svm.data_noNA$Date<date2x),1:7]; train.data = train.data[,-3]

train.data = svm.data_noNA[501:nrow(svm.data_noNA),2:7]; train.data = train.data[,-3]
test.data = svm.data_noNA[1:500,2:7]; test.data = test.data[,-3]
#
set.seed(2)
svm_poly = svm(sp500~.,data = train.data, kernel = "polynomial", cost=1,scale=FALSE)
plot(svm_poly,train.data[,3],train.data[,4])

svm_poly = tune(svm, sp500~., data = train.data, kernel="polynomial",
                ranges = list(cost=c(0.1, 1.0,10), 
                              gamma=(c(0.5,1,2,4))), scale=FALSE)

svm_radial = tune(svm, sp500~., data = train.data, kernel="radial",
               ranges = list(cost=c(0.1, 1.0,10), gamma=(c(0.5,1,2,4))), 
               scale=FALSE)

svm_linear = tune(svm, sp500~., data = train.data, kernel="linear",
                ranges = list(cost=c(0.1, 1.0,10), gamma=(c(0.5,1,2,4))), 
                scale=FALSE)

poly.best = predict(svm_poly$best.model,test.data)
poly.result = table(predict=poly.best, truth = test.data$sp500)
poly.err = (poly.result[1,3] + poly.result[3,1])/length(poly.best); poly.err

radial.best = predict(svm_radial$best.model,test.data)
radial.result = table(predict=radial.best, truth = test.data$sp500)
radial.err = (radial.result[1,3] + radial.result[3,1])/length(radial.best); radial.err

lin.best = predict(svm_linear$best.model,test.data)
lin.result = table(predict=lin.best, truth = test.data$sp500)
lin.err = (lin.result[1,3] + lin.result[3,1])/length(lin.best); lin.err


#stock.fit = svm(sp500~., data = train.data, kernel="linear",cost=10, scale=FALSE)
#plot(stock.fit, train.data[,3:4])





#==============  END OF OPERATIONAL CODE ==============================
#Next Steps:
#1.Generate List structure for the data files --> DONE...
#2. Visualize correltions --> DONE...
#3. Set threshold for feature selection
#4. Convert delta's to 0's and 1's for prediction
#5. Install Support vector and trees
#6. Code the profit prediction






#============================================================================
#========= TEST CODE SECTION: Not part of the final code. Only for playing with 
#                             different ideas  
#----------------------------
m.day = mean(spdax.day[,2]); var.day = var(spdax.day[,2])


#legend("topright", legend = c('sp.new','hs.new','dax.new',
#              'n225.new','rtsi.new','sti.new'), col=c("black"."red","green",
#              "blue","lt.blue","pink", "yellow"))

#       text.width = strwidth("1,000,000"),
#       lty = 1:2, xjust = 1, yjust = 1,
#       title = "Line Types






#----------TEST CODE ----------


