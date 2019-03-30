#==============================================================================
# Reference paper: A New Anomaly: The Cross-Sectional
# Proﬁtability of Technical Analysis, Han et al., 2013, JFQA
# https://systematicinvestor.wordpress.com/?s=moving+average
# code reference: flexible stop loss strategy# moving average cross-over
#---------------------------------------------------------------------------------
# we apply the MA timing strategy to the TWSE volatility decile portfolios by
# computing the 10-day average prices of the decile portfolios (MA price). 
# For a given portfolio, the MA investment timing strategy is to buy or continue to 
# hold the portfolio today when yesterday’s price is above its 10-day MA price, 
# and to invest the money into the risk-free asset otherwise.
#===============================================================================
###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
# http://systematicinvestor.wordpress.com/?s=residual
###############################################################################
rm(list=ls())
#setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)
#=======================================================================
# http://systematicinvestor.wordpress.com/?s=minimum+variance+portfolio
#
#

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod') 
load.packages('PerformanceAnalytics')
load.packages('reshape2')
load.packages('fBasics')
load.packages('Hmisc')
load.packages('xts')
#load.packages('xtsExtra') 
load.packages('RColorBrewer')
load.packages('fPortfolio')
load.packages('BurStFin')
load.packages('lubridate')
load.packages('ggplot2')
load.packages('quadprog,corpcor,lpSolve')
load.packages('scales')
load.packages('Rsolnp')
load.packages('rbenchmark')
#install.packages("Rdonlp2", repos="http://R-Forge.R-project.org")

require("PerformanceAnalytics")
library(reshape2)
#library(xtsExtra)
library(zoo)
library(fPortfolio)
#na.locf(data.frame(rep("a",4), 1:4,1:4, c(1,NA,NA,NA)))
library(reshape2)
library(Hmisc)
library(RColorBrewer)
library(PerformanceAnalytics)
library(BurStFin)
library(lubridate)
library(ggplot2)
#library(Rdonlp2)
library(scales)
library(Rsolnp)
library(rbenchmark)
library(sandwich)
library(dynlm)
library(lmtest)

#=========================================================================
# The following part is about importing raw data and restructuring data
#=========================================================================
# import monthly closing price;
#=====================================
# As the data is big, you can importing rds data by runing codes from line 125  
#===============================
# Download data from TEJ, choose "common stock without ETF and ADR and F stocks" category, 
# and filled missing data with NA
#===============================
# setwd("~/Market Timing Strategy")
# import daily closing price of all listed stocks from 1990-2017
#price = read.table("TWSE_1990_2017_d_close.txt", stringsAsFactors=FALSE, fileEncoding="UTF-8")
price = read.table("TWSE_1990_2017_d1_close.txt")
head(price)
price = price[,-2]
price = price[-1,]
head(price,10)
colnames(price) = c("id","date","close", "cap")
dim(price)
head(price)
# use dcast to reorder dataframe by date;
mkv<-price[,-3]
price<-price[,-4]
price.reorder = dcast(price,date~id)
price.reorder[1:5, 1:5]
mkv<-dcast(mkv, date~id)
class(mkv)
mkv[1:5, 1:5]
head(price.reorder)
tail(price.reorder[,1])
class(price.reorder)
dim(price.reorder)
#=============================================
# output data
write.csv(price.reorder, "daily_price.csv")
write.csv(mkv, "mkv.csv")
#==============================================
ff3f = read.table("FF3F_1990_2017_d.txt")
head(ff3f)
ff3f = ff3f[,-c(1,2)]
ff3f = ff3f[-1,]
head(ff3f,10)
colnames(ff3f) = c("date","mktRF", "size", "BM")
dim(ff3f)
head(ff3f)
# write out csv
write.csv(ff3f, "ff3f.csv")
#==============================================
#import data
price.reorder<-read.csv('daily_price.csv', stringsAsFactors = FALSE)
mkv.reorder<-read.csv('mkv.csv', stringsAsFactors = FALSE)
price.reorder[1:5, 1:5]
mkv.reorder[1:5, 1:5]
ff3f.csv<-read.csv('ff3f.csv')
head(ff3f.csv)
# export data in rds so that the file size will be smaller than .csv
saveRDS(price.reorder, file = "price_reorder")
saveRDS(mkv.reorder, file = "mkv_reorder")
#===================================================
# start importing RDS data without .csv data 
#===================================================
price.reorder<-readRDS("price_reorder")
mkv.reorder<-readRDS("mkv_reorder")
#=================================================
date.seq<-as.Date(as.character(price.reorder$date), "%Y%m%d")
price.xts<-xts(price.reorder[,-c(1:2)], order.by = date.seq)
tickers<-colnames(price.xts)
dim(price.xts)
tickers.n<-length(tickers)
#price.xts[1:5, 1:5]
mkv.xts<-xts(mkv.order[,-c(1:2)], order.by = date.seq)
dim(mkv.xts)
#mkv.xts[1:5, 1:5]
ff3f.xts<-xts(ff3f.csv[,-1], order.by = date.seq)
ff3f.xts<-ff3f.xts/100
ff3f.xts<-ff3f.xts[,-1]
head(ff3f.xts)
dim(ff3f.xts)
#===================================================
# compute daily returns
ret<-price.xts/lag(price.xts) -1
ret[1:5, 1:5]
ret.yi<-ret["1990"]
tail(ret.yi)
period.ends<-endpoints(mkv.xts, on = 'years')
period.ends<-period.ends[period.ends>0]
period.ends
#compute std using daily returns of 1990
sd.yi<-apply(ret.yi, 2, sd, na.rm = TRUE)
head(sd.yi)
# calculate the number of stocks with available sd in year 1990
sum(!is.na(sd.yi))
sum(!is.na(tail(ret.yi,1)))
# daily std of year 1990
#std.yi <- as.vector(sd(coredata(ret.yi), na.rm = TRUE))
n.quantiles = 10
# write.csv(factor.tw, file="~/data/output test/factor_tw.csv")
# rank std into 10 groups
ranking.tw = ceiling(n.quantiles * rank(sd.yi, na.last = 'keep','first') / count(sd.yi))
ranking.tw[1:10]
sd.yi[1:10]
# write.csv(ranking.tw, file="~/data/output test/ranking_tw.csv")  
# data.tw$weight = ntop(price.tw.sample, tickers.n)
quantiles.yi = ranking.tw
ranking.tw[1:10]
weights.yi = 1/tapply(rep(1,tickers.n), ranking.tw, sum)[ranking.tw]
weights.yi[1:10]
# First we arrange data by year
# In order not to get missing data for long time moving average such as 200-day MA, 
# I extend the year data by including previous 10 months so that we can get 
# 200-day MA for each day
# For example, for data of year 1991, I include data from 1990-03 to 1991-12 
# as the data for 1991. 
# PP.y = average stock prices in each decile portfolio sorted by the volatility
#  in each year
# PP.yi = average stock prices in year i for each decile portfolio sorted by the volatility
PP.y<-list()
t=1990
for (t in 1990:2016){
  year<-as.character(t)
  ret.yi<-ret[year]
  #compute std using daily returns of 1990
  sd.yi<-apply(ret.yi, 2, sd, na.rm = TRUE)
  # rank return volatility in 1990 into deciles 
  ranking.tw = ceiling(n.quantiles * rank(sd.yi, na.last = 'keep','first') / count(sd.yi))
  year1<-t+1
  period<-paste(year, "-3/", sep="")
  period<-paste(period, paste(as.character(year1), "-12", sep=""), sep="")
  price.j<-price.xts[period]
  # price.j = daily prices from 1990-3/1991-12 for all stocks
  price.j<-na.locf(price.j, fromLast = T)
  PP.yi<-price.j*NA
  PP.yi<-PP.yi[,1:10]
  for (d in 1:nrow(price.j)){
    PPjd <- tapply(coredata(price.j[d,]), ranking.tw, mean, na.rm = T)
    PP.yi[d,]<-PPjd
  }
  colnames(PP.yi)<-c("Q1", "Q2","Q3", "Q4", "Q5", "Q6","Q7", "Q8","Q9", "Q10")
  year1<-as.character(year1)
  PP.y[[year1]]<-PP.yi
}
names(PP.y)
head(PP.y[['1991']])
# For each year, we need to generate 2 sets of decile portfolios: one is benchmark portfolios 
# sorted by standard deviation; the other is market timing MA portfolios

# j-th portfolio price at day d
#price.j<-price.xts["1990-3/1991-12"]
#price.j<-na.locf(price.j, fromLast = T)
#price.j[1:10, 1:10]
#PP.yi<-price.j*NA
#PP.yi<-PP.yi[,1:10]
#colnames(PP.yi)<-c("Q1", "Q2","Q3", "Q4", "Q5", "Q6","Q7", "Q8","Q9", "Q10")
#PPjd = tapply(coredata(price.j[1,]), ranking.tw, mean)[ranking.tw]
#d= 1
#for (d in 1:nrow(price.j)){
#   PPjd <- tapply(coredata(price.j[d,]), ranking.tw, mean)
#   PP.yi[d,]<-PPjd
#}

head(PP.yi)
tail(PP.yi)
#sma.fast = SMA(prices, 20)
#sma.fast = PP.yi[,1]
#sma.slow = SMA(PP.yi[,1], 20)
#buy.signal = iif(cross.up(sma.fast, sma.slow), 1, NA)
#data$weight[] = NA
#data$weight[] = iif(cross.up(sma.fast, sma.slow), 1, iif(cross.dn(sma.fast, sma.slow), 0, NA))
# model1 (buy and hold) is the volatility decile portfolio returns, which will be used as a benchmark for
# market timing: MA-strategy portfolio
data.1<-new.env()
model1<-list()
bench<-list()
#prices = data$prices 
j="1991"
i=1
for (j in names(PP.y)){
  for (i in 1:10){
    sma.fast = PP.y[j][[1]][,i]
    data.1$prices = sma.fast[j]
    # buy and hold strategy as a benchmark 
    data.1$execution.price = data.1$prices*NA
    data.1$weight = data.1$prices*NA
    data.1$weight[] = 1
    model1[[i]] = bt.run.share(data.1, clean.signal=F, trade.summary = TRUE)
  }
  bench[[j]] = model1
}
#-----------------------------------------------------------------------
# row bind the returns of decile portfolio from 1991 to 2017
# benchRet.i = returns of decile portfolios in each year from 1991-2017
benchRet.i<-list()
i="1991"
j=1
for (i in names(PP.y)){
  tempi<-bench[[i]][[1]]$ret*NA
  for (j in 1:10){
    tempij<-bench[[i]][[j]]$ret
    tempi<-merge.xts(tempi, tempij)
  }
  #tempi<-tempij[,c(seq(10,1,by=-1))]
  benchRet.i[[i]]<-tempi[,-1]
}
names(benchRet.i)
# benchmark daily returns from 1990/03 - 1991/12
benchRet.i[[2]]
#ret2017.bench<-benchRet.i[['2017']]

benchRet<-do.call(rbind, benchRet.i)
#write.csv(benchRet, "benchRet.csv")
dim(benchRet)
head(benchRet)
benchRet$Q10_1<-benchRet[,10] - benchRet[,1]
temp1<-benchRet
avg1<-apply(coredata(temp1), 2, mean)*252
std1<-apply(coredata(temp1), 2, stdev)*sqrt(252)
skew1<-apply(coredata(temp1), 2, skewness)
kur1<-apply(coredata(temp1),2, kurtosis)
SR1<-apply(temp1, 2, SharpeRatio)*sqrt(252)
table1<-matrix(c(avg1, std1, skew1, kur1, SR1[1,]), ncol = 11, byrow = TRUE )
rownames(table1)<-c("avg", "std", "skew", "kurtosis", "SR")
colnames(table1)<-c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q10-1")
table1
#write.table(t(table1), "table1_panelA.txt", sep="\t") 
tt1<-apply(coredata(temp1), 2, t.test, mu=0, conf.level=0.99)
#newL <- do.call('rbind', lapply(1:length(tt), function(x){cbind(tt[[x]]$p.value)}))
pvalue1 <- do.call('rbind', lapply(1:length(tt1), function(x){tt1[[x]]$p.value} ) )
pvalue1
#write.table(pvalue1, "table1_panelA_pv.txt", sep="\t") 
tvalue1 <- do.call('rbind', lapply(1:length(tt1), function(x){tt1[[x]]$statistic} ) )
tvalue1
#write.table(tvalue1, "table1_panelA_tv.txt", sep="\t")
# combine three tables together
tab1.panA<-as.data.frame(t(table1))
tab1.panA$avg.pv<-pvalue1
tab1.panA$avg.tv<-tvalue1
tab1.panA
#write.csv(tab1.panA, "tab1_panA.csv")
#======================================================================
#plotbt.strategy.sidebyside(model1[[3]], return.table = T, make.plot = F)
#plotbt(model1, plotX = T, log = 'y', LeftMargin = 3)            
#mtext('Cumulative Performance', side = 2, line = 1)
#plotbt.strategy.sidebyside(model1)
#=======================================================================
# data2 = momentum strategy 
data2<-new.env()
model2<-list()
# market timing portfolio
mom<-list()
j="2017"
q=10
k = 15 # momentum strategy using past k-day accumulated returns
c = 0 # cumulative returns hurdle rate
#
for (j in names(PP.y)){
  for (q in 1:10){
    mom.n = momentum(PP.y[j][[1]][,q], n=k, na.pad = TRUE)
    mom.n<-mom.n[j]
    data2$prices<-PP.y[j][[1]][,q][j]
    data2$weight = data2$prices*NA
    data2$execution.price = data2$prices*NA
    #data$weight[] = iif(cross.up(sma.fast, sma.slow), 1, iif(cross.dn(sma.fast, sma.slow), 0, NA))
    data2$weight[] = iif(mom.n > c, 1, 0)
    #data$weight<-data$weight[j]
    model2[[q]] = bt.run.share(data2, clean.signal=F, trade.summary = TRUE)
  }
  mom[[j]]<-model2   
}

# row bind the returns of decile portfolio from 1991 to 2017
momRet.i<-list()
# 
i="1991"
i='2017'
j=1
for (i in names(PP.y)){
  tempi<-mom[[i]][[1]]$ret*NA
  for (j in 1:10){
    tempij<-mom[[i]][[j]]$ret
    tempi<-merge.xts(tempi, tempij)
  }
  #tempi<-tempij[,c(seq(10,1,by=-1))]
  momRet.i[[i]]<-tempi[,-1]
}
# portRet.mktime.i = decile portfolio returns for ith year from 1991-2017
#ret2017.mktime<-portRet.mktime.i[['2017']]
momRet<-do.call(rbind, momRet.i)
#write.csv(portRet.mktime, "portRet_mktime.csv")
dim(momRet)
momRet$Q10_1<-momRet[,10] - momRet[,1]
temp<-momRet
avg2<-apply(coredata(temp), 2, mean)*252
std2<-apply(coredata(temp), 2, stdev)*sqrt(252)
skew2<-apply(coredata(temp), 2, skewness)
kur2<-apply(coredata(temp),2, kurtosis)
SR2<-apply(temp,2, SharpeRatio)*sqrt(252)
table2<-matrix(c(avg2, std2, skew2, kur2, SR2[1,]), ncol = 11, byrow = TRUE )
rownames(table2)<-c("avg", "std", "skew", "kurtosis", "SR")
colnames(table2)<-c("Q1", "Q2","Q3", "Q4", "Q5", "Q6","Q7", "Q8","Q9", "Q10", "Q10_1")
t(table2)
#write.table(t(table), "tab1_panelB.txt", sep="\t") 
tt2<-apply(coredata(temp), 2, t.test, mu=0, conf.level=0.99)
#newL <- do.call('rbind', lapply(1:length(tt), function(x){cbind(tt[[x]]$p.value)}))
pvalue2 <- do.call('rbind', lapply(1:length(tt2), function(x){tt2[[x]]$p.value} ) )
pvalue2
#write.table(pvalue, "tab1_panelB_pv.txt", sep="\t")
tvalue2<- do.call('rbind', lapply(1:length(tt2), function(x){tt2[[x]]$statistic} ) )
tvalue2
#write.table(tvalue, "tab1_panelB_tv.txt", sep="\t")
# combine three tables together
tab1.panB.mom<-as.data.frame(t(table2))
tab1.panB.mom$avg.pv<-pvalue2
tab1.panB.mom$avg.tv<-tvalue2
tab1.panB.mom
write.csv(tab1.panB.mom, "tab1_panB_mom_15.csv")
#=============
# MAP
#==============
map.mom.ret<-momRet - benchRet
temp2<-map.mom.ret
avg<-apply(coredata(temp2), 2, mean)*252
std<-apply(coredata(temp2), 2, stdev)*sqrt(252)
skew<-apply(coredata(temp2), 2, skewness)
table2<-matrix(c(avg, std, skew), ncol = 11, byrow = TRUE )
rownames(table2)<-c("avg", "std", "skew")
colnames(table2)<-c("Q1", "Q2","Q3", "Q4", "Q5", "Q6","Q7", "Q8","Q9", "Q10", "Q10_1")
t(table2)
#write.table(t(table2), "tab1_panelC.txt", sep="\t") 
tt2<-apply(coredata(temp2), 2, t.test, mu=0, conf.level=0.99)
#newL <- do.call('rbind', lapply(1:length(tt), function(x){cbind(tt[[x]]$p.value)}))
pvalue2 <- do.call('rbind', lapply(1:length(tt2), function(x){tt2[[x]]$p.value} ) )
pvalue2
#write.table(pvalue2, "tab1_panelC_pv.txt", sep="\t")
tvalue2<- do.call('rbind', lapply(1:length(tt2), function(x){tt2[[x]]$statistic} ) )
tvalue2
#write.table(tvalue2, "tab1_panelC_tv.txt", sep="\t")
# combine three tables together
tab1.panC.mom<-as.data.frame(t(table2))
tab1.panC.mom$avg.pv<-pvalue2
tab1.panC.mom$avg.tv<-tvalue2
tab1.panC.mom
write.csv(tab1.panC.mom, "tab1_panC_mom_15.csv")
# following Table 4 format but using map(20.50) to denote fma(20-d) and sma(50-d) 
tab4.fma.sma<-tab1.panC[,-c(2,3)]



















#============================================================
data<-new.env()
model<-list()
# market timing portfolio
markTime<-list()
j="2017"
q=10
fma = 10
sma = 20
for (j in names(PP.y)){
  for (q in 1:10){
    sma.fast = SMA(PP.y[j][[1]][,q], fma)
    sma.slow = SMA(PP.y[j][[1]][,q], sma)
    sma.fast = sma.fast[j]
    sma.slow = sma.slow[j]
    data$prices<-PP.y[j][[1]][,q][j]
    data$weight = sma.fast*NA
    data$execution.price = sma.fast*NA
    #data$weight[] = iif(cross.up(sma.fast, sma.slow), 1, iif(cross.dn(sma.fast, sma.slow), 0, NA))
    data$weight[] = iif(sma.fast > sma.slow, 1, 0)
    #data$weight<-data$weight[j]
    model[[q]] = bt.run.share(data, clean.signal=F, trade.summary = TRUE)
  }
  markTime[[j]]<-model   
}

# row bind the returns of decile portfolio from 1991 to 2017
portRet.mktime.i<-list()
i="1991"
i='2017'
j=1
for (i in names(PP.y)){
  tempi<-markTime[[i]][[1]]$ret*NA
  for (j in 1:10){
    tempij<-markTime[[i]][[j]]$ret
    tempi<-merge.xts(tempi, tempij)
  }
  #tempi<-tempij[,c(seq(10,1,by=-1))]
  portRet.mktime.i[[i]]<-tempi[,-1]
}
# portRet.mktime.i = decile portfolio returns for ith year from 1991-2017
#ret2017.mktime<-portRet.mktime.i[['2017']]
portRet.mktime<-do.call(rbind, portRet.mktime.i)
#write.csv(portRet.mktime, "portRet_mktime.csv")
dim(portRet.mktime)
portRet.mktime$Q10_1<-portRet.mktime[,10] - portRet.mktime[,1]
temp<-portRet.mktime
avg<-apply(coredata(temp), 2, mean)*252
std<-apply(coredata(temp), 2, stdev)*sqrt(252)
skew<-apply(coredata(temp), 2, skewness)
kur<-apply(coredata(temp),2, kurtosis)
SR<-apply(temp,2, SharpeRatio)*sqrt(252)
table<-matrix(c(avg, std, skew, kur, SR[1,]), ncol = 11, byrow = TRUE )
rownames(table)<-c("avg", "std", "skew", "kurtosis", "SR")
colnames(table)<-c("Q1", "Q2","Q3", "Q4", "Q5", "Q6","Q7", "Q8","Q9", "Q10", "Q10_1")
t(table)
#write.table(t(table), "tab1_panelB.txt", sep="\t") 
tt<-apply(coredata(temp), 2, t.test, mu=0, conf.level=0.99)
#newL <- do.call('rbind', lapply(1:length(tt), function(x){cbind(tt[[x]]$p.value)}))
pvalue <- do.call('rbind', lapply(1:length(tt), function(x){tt[[x]]$p.value} ) )
pvalue
#write.table(pvalue, "tab1_panelB_pv.txt", sep="\t")
tvalue<- do.call('rbind', lapply(1:length(tt), function(x){tt[[x]]$statistic} ) )
tvalue
#write.table(tvalue, "tab1_panelB_tv.txt", sep="\t")
# combine three tables together
tab1.panB<-as.data.frame(t(table))
tab1.panB$avg.pv<-pvalue
tab1.panB$avg.tv<-tvalue
tab1.panB
#write.csv(tab1.panB, "tab1_panB_20_100.csv")
#=============
# MAP
#==============
map.ret<-portRet.mktime - benchRet
temp2<-map.ret
avg<-apply(coredata(temp2), 2, mean)*252
std<-apply(coredata(temp2), 2, stdev)*sqrt(252)
skew<-apply(coredata(temp2), 2, skewness)
table2<-matrix(c(avg, std, skew), ncol = 11, byrow = TRUE )
rownames(table2)<-c("avg", "std", "skew")
colnames(table2)<-c("Q1", "Q2","Q3", "Q4", "Q5", "Q6","Q7", "Q8","Q9", "Q10", "Q10_1")
table2
#write.table(t(table2), "tab1_panelC.txt", sep="\t") 
tt2<-apply(coredata(temp2), 2, t.test, mu=0, conf.level=0.99)
#newL <- do.call('rbind', lapply(1:length(tt), function(x){cbind(tt[[x]]$p.value)}))
pvalue2 <- do.call('rbind', lapply(1:length(tt2), function(x){tt2[[x]]$p.value} ) )
pvalue2
#write.table(pvalue2, "tab1_panelC_pv.txt", sep="\t")
tvalue2<- do.call('rbind', lapply(1:length(tt2), function(x){tt2[[x]]$statistic} ) )
tvalue2
#write.table(tvalue2, "tab1_panelC_tv.txt", sep="\t")
# combine three tables together
tab1.panC<-as.data.frame(t(table2))
tab1.panC$avg.pv<-pvalue2
tab1.panC$avg.tv<-tvalue2
tab1.panC
#write.csv(tab1.panC, "tab1_panC_20_50.csv")
# following Table 4 format but using map(20.50) to denote fma(20-d) and sma(50-d) 
tab4.fma.sma<-tab1.panC[,-c(2,3)]
#===============================
# run ff3f model
#===============================
head(map.ret)
ff3f.1991<-ff3f.xts['1991/2017']
head(ff3f.1991)
dim(ff3f.1991)
ff3f.df<-as.data.frame(coredata(ff3f.1991))
mapRet.df<-as.data.frame(coredata(map.ret))
head(ff3f.df)
# a<-lm(benchRet.dt[,1]~ff3f.dt[,1]+ff3f.1991[,2]+ff3f.1991[,3])
#a<-lm(benchRet.df[,1]~mktRF+size+BM, data = ff3f.df)
#summary(a)
#str(a)
#coeftest(a)
#b<-coeftest(a, vcov = NeweyWest)
#coeftest(a, vcov = sandwich)
#coeftest(a, df = Inf, vcov = vcovHC, type = "HC0")
#NeweyWest(a)
#coef(summary(a))[,1]
# Estimate NeweyWest Robust Covariance 
coef.1<-t(apply(mapRet.df, 2, function(y.col) lm(y.col~mktRF+size+BM, data = ff3f.df)$coef))
coef.1[,1]<-coef.1[,1]*252
#write.table(coef.1, "table2_panelB", sep="\t")

adjR2.1<-apply(mapRet.df, 2, function(y.col) 
  summary(lm(y.col~mktRF+size+BM, data = ff3f.df))$r.squared)
adjR2.1
#write(adjR2.1, "table2_panelB_adjR2", sep="\t")
# compute Newey West Robust covariance 
# column 3 is t value
tvalue1.nw<-t(apply(mapRet.df, 2, function(y.col) 
  coeftest((lm(y.col~mktRF+size+BM, data = ff3f.df)), vcov=NeweyWest)[,3]))
tvalue1.nw
#write.csv(tvalue1.nw, "table2_panelB_tv.csv")
#apply(benchRet, 2, lm)
# column 4 is p-value
pvalue1.nw<-t(apply(mapRet.df, 2, function(y.col) 
  coeftest((lm(y.col~mktRF+size+BM, data = ff3f.df)), vcov=NeweyWest)[,4]))
pvalue1.nw
#write.csv(pvalue1.nw, "table2_panelB_pv.csv")
# combine results into one table
tab2.panB<-as.data.frame(coef.1)
tab2.panB$adjR2<-adjR2.1
tab2.panB$tv.nw<-tvalue1.nw
tab2.panB$pv<-pvalue1.nw
tab2.panB
#write.csv(tab2.panB, "tab2_panB_20_50.csv")
#
tab4.fma.sma$alpha<-tab2.panB[,1]
tab4.fma.sma$pv<-pvalue1.nw[,1]
tab4.fma.sma$tv<-tvalue1[,1]
tab4.fma.sma
ma = paste(fma, sma, sep="")
filename = paste(paste("tab4_", ma, sep=""), ".csv", sep="")
write.csv(tab4.fma.sma, filename)
#===============================================
# capm model
#===============================================
coef.2<-t(apply(mapRet.df, 2, function(y.col) lm(y.col~mktRF, data = ff3f.df)$coef))
coef.2
coef.2[,1]<-coef.2[,1]*252
#write.table(coef.2, "table2_panelA", sep="\t")

adjR2.2<-apply(mapRet.df, 2, function(y.col) 
  summary(lm(y.col~mktRF, data = ff3f.df))$r.squared)
adjR2.2
#write(adjR2.2, "table2_panelA_adjR2", sep="\t")
# compute Newey West Robust covariance 
# column 3 is t value
tvalue2.nw<-t(apply(mapRet.df, 2, function(y.col) 
  coeftest((lm(y.col~mktRF, data = ff3f.df)), vcov=NeweyWest)[,3]))
tvalue2.nw
#write.csv(tvalue2.nw, "table2_panelA_tv.csv")
#apply(benchRet, 2, lm)
# column 4 is p-value
pvalue2.nw<-t(apply(mapRet.df, 2, function(y.col) 
  coeftest((lm(y.col~mktRF, data = ff3f.df)), vcov=NeweyWest)[,4]))
pvalue2.nw
#write.csv(pvalue2.nw, "table2_panelA_pv.csv")
# combine results into one table
tab2.panA<-as.data.frame(coef.2)
tab2.panA$adjR2<-adjR2.2
tab2.panA$tv.nw<-tvalue2.nw
tab2.panA$pv<-pvalue2.nw
tab2.panA
write.csv(tab2.panA, "tab2_panA.csv")
# Plot perfromance
#plotbt(model, plotX = T, lo vg = 'y', LeftMargin = 3)            
#mtext('Cumulative Performance', side = 2, line = 1)

# Plot Strategy Statistics  Side by Side
#out<-matrix(rep(0,100), ncol=10)
# j=1
# for (j in 1:10){
#     out[,j]<-plotbt.strategy.sidebyside(model[[j]],return.table = T, make.plot = F)
# }
# rownames(out)<-c("Period", "Cagr", "Sharpe", "DVR", "Volatility", "MaxDD", 
#                  "AvgDD", "VaR", "CVaR", "Exposure")
# colnames(out)<-c("Q1", "Q2","Q3", "Q4","Q5", "Q6","Q7", "Q8","Q9", "Q10")
# as.data.frame(out)
# 
# strategy.performance.snapshoot(model, T)
# plotbt.strategy.sidebyside(model)
#equal weighting
#tapply(rep(1,tickers.n), ranking.tw, sum)[ranking.tw]
#> tapply(rep(1,tickers.n), ranking.tw, sum)
#1  2  3  4  5  6  7  8  9 10 
#13 13 13 13 14 13 13 13 13 14 

#=====================================
# convert into function

# benchmark <-function(year, PP.yi){
#     yr<-as.character(year)
#     #tem<-paste("model1", "1991", sep=".")
#     #eval(parse(tem="list()"))
#     for (i in 1:10){
#       sma.fast = PP.yi[yr][,i]
#       data.1$prices = sma.fast
#       data.1$execution.price = data.1$prices*NA
#       data.1$weight = data.1$prices*NA
#       data.1$weight[] = 1
#       model1[[i]] = bt.run.share(data.1, clean.signal=F, trade.summary = TRUE)
#     }
#     return(model1)
# }

#a<-benchmark(1991, PP.yi)












