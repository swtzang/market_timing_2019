###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
###############################################################################
# Load Systematic Investor Toolbox (SIT)
# github.com/systematicinvestor/SIT
###############################################################################
rm(list=ls())
library(curl)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
#con = gzcon(curl('https://github.com/systematicinvestor/SIT/raw/master/sit.gz','rb'))
source(con)
close(con)
#
plota.test()
#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')

tickers = spl('SPY,^VIX')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', fill.gaps = T)

VIX = Cl(data$VIX)
bt.prep.remove.symbols(data, 'VIX')

#*****************************************************************
# Setup
#*****************************************************************
prices = data$prices

models = list()

#*****************************************************************
# 200 SMA
#****************************************************************** 
data$weight[] = NA
data$weight[] = iif(prices > SMA(prices, 200), 1, 0)
models$ma200 = bt.run(data)
models$ma200 = bt.run.share(data, clean.signal=T)
#*****************************************************************
# 200 ROC
#****************************************************************** 
roc = prices / mlag(prices) - 1

data$weight[] = NA
data$weight[] = iif(SMA(roc, 200) > 0, 1, 0)
models$roc200 = bt.run.share(data, clean.signal=T)

#*****************************************************************
# 200 VIX MOM
#****************************************************************** 
data$weight[] = NA
data$weight[] = iif(SMA(roc/VIX, 200) > 0, 1, 0)
models$vix.mom = bt.run.share(data, clean.signal=T)