#Adaptive Asset Allocation
library(xts)
library(fPortfolio)
library(timeSeries)
library(PerformanceAnalytics)
library(corpcor)
setwd("~/Quant Trading/AdaptiveAA")
load("~/Quant Trading/datasets/Quandl.rdata")
names(returns.daily) #names of asset classes
source("aaa.r")

minrisk.model<-AdaptiveAA.model(n.top=4,n.mom=6*21,n.vol=2*21,target.sd=0)
table.AnnualizedReturns(minrisk.model$returns.monthly)
minrisk.model$returns.ac #returns of individual asset classes
minrisk.model$sd.ac #risk of individual asset classes
minrisk.model$returns.ac/minrisk.model$sd.ac
plot(minrisk.model$allocations,main="Allocations")
plot(minrisk.model$turnover.monthly,ylim=c(0,1),main="Turnover",ylab="Percent")
abline(h=mean(minrisk.model$turnover.monthly))
mean(minrisk.model$turnover.monthly)

charts.PerformanceSummary(minrisk.model$returns.monthly,colorset=rich6equal)
t(table.CalendarReturns(minrisk.model$returns.monthly))

chart.Boxplot(minrisk.model$returns.monthly)
table.Stats(minrisk.model$returns.monthly)
table.Drawdowns(minrisk.model$returns.monthly[,"Port"])
table.Drawdowns(minrisk.model$returns.monthly[,"Bench"])
table.Drawdowns(minrisk.model$returns.monthly[,"B6040"])
table.Drawdowns(minrisk.model$returns.monthly[,"EqN"])
chart.RollingPerformance(minrisk.model$returns.monthly)
chart.RollingPerformance(minrisk.model$returns.monthly,width=36)

