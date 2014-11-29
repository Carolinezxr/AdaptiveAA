#Adaptive Asset Allocation
# (after aaa_getpricedata.r)
library(xts)
library(fPortfolio)
library(timeSeries)
library(PerformanceAnalytics)
setwd("~/Quant Trading/AdaptiveAA")
source("aaa.r")
load("~/Quant Trading/datasets/Quandl.rdata")
names(returns.daily) #names of asset classes

#Demonstration of the model
first.model<-AdaptiveAA.model(n.top=5,n.mom=6*21,n.vol=2*21)
print(table.AnnualizedReturns(first.model$returns.monthly))
print(maxDrawdown(first.model$returns.monthly))
first.model$returns.ac #returns of individual asset classes
first.model$sd.ac #risk of individual asset classes
first.model$returns.ac/first.model$sd.ac
plot(first.model$allocations,main="Allocations")
plot(first.model$turnover.monthly,ylim=c(0,1),main="Turnover",ylab="Percent")
abline(h=mean(first.model$turnover.monthly))

charts.PerformanceSummary(first.model$returns.monthly,colorset=rich6equal)
t(table.CalendarReturns(first.model$returns.monthly))

chart.Boxplot(first.model$returns.monthly)
table.Stats(first.model$returns.monthly)
table.Drawdowns(first.model$returns.monthly[,"Port"])
table.Drawdowns(first.model$returns.monthly[,"Bench"])
table.Drawdowns(first.model$returns.monthly[,"B6040"])
chart.RollingPerformance(first.model$returns.monthly)
chart.RollingPerformance(first.model$returns.monthly,width=36)

#The following code will test variations of n.top, n.mon, and n.vol
# aaa<-matrix(data=NA,nrow=20,ncol=12,
#             dimnames=list(NULL,c("Top","Mom","Vol",
#                                  "P.ret","P.sd","P.shrp",
#                                  "E.ret","E.sd","E.shrp",
#                                  "B.ret","B.sd","B.shrp")))
# #Vary number of asset classes in top
# j<-0
# for (i in 2:9){
#     first.model<-AdaptiveAA.model(n.top=i,n.mom=6*21,n.vol=2*21)
#     j<-j+1
#     aaa[j,"Top"]=i
#     aaa[j,"Mom"]=6
#     aaa[j,"Vol"]=2
#     aaa[j,"P.ret"]=table.AnnualizedReturns(first.model$returns.monthly)[1,"Port"]
#     aaa[j,"P.sd"]=table.AnnualizedReturns(first.model$returns.monthly)[2,"Port"]
#     aaa[j,"P.shrp"]=table.AnnualizedReturns(first.model$returns.monthly)[3,"Port"]
#     aaa[j,"E.ret"]=table.AnnualizedReturns(first.model$returns.monthly)[1,"Bench"]
#     aaa[j,"E.sd"]=table.AnnualizedReturns(first.model$returns.monthly)[2,"Bench"]
#     aaa[j,"E.shrp"]=table.AnnualizedReturns(first.model$returns.monthly)[3,"Bench"]
#     aaa[j,"B.ret"]=table.AnnualizedReturns(first.model$returns.monthly)[1,"B6040"]
#     aaa[j,"B.sd"]=table.AnnualizedReturns(first.model$returns.monthly)[2,"B6040"]
#     aaa[j,"B.shrp"]=table.AnnualizedReturns(first.model$returns.monthly)[3,"B6040"]
# }
# for (i in 4:12){
#     first.model<-AdaptiveAA.model(n.top=5,n.mom=i*21,n.vol=2*21)
#     j<-j+1
#     aaa[j,"Top"]=5
#     aaa[j,"Mom"]=i
#     aaa[j,"Vol"]=2
#     aaa[j,"P.ret"]=table.AnnualizedReturns(first.model$returns.monthly)[1,"Port"]
#     aaa[j,"P.sd"]=table.AnnualizedReturns(first.model$returns.monthly)[2,"Port"]
#     aaa[j,"P.shrp"]=table.AnnualizedReturns(first.model$returns.monthly)[3,"Port"]
#     aaa[j,"E.ret"]=table.AnnualizedReturns(first.model$returns.monthly)[1,"Bench"]
#     aaa[j,"E.sd"]=table.AnnualizedReturns(first.model$returns.monthly)[2,"Bench"]
#     aaa[j,"E.shrp"]=table.AnnualizedReturns(first.model$returns.monthly)[3,"Bench"]
#     aaa[j,"B.ret"]=table.AnnualizedReturns(first.model$returns.monthly)[1,"B6040"]
#     aaa[j,"B.sd"]=table.AnnualizedReturns(first.model$returns.monthly)[2,"B6040"]
#     aaa[j,"B.shrp"]=table.AnnualizedReturns(first.model$returns.monthly)[3,"B6040"]
# }
# for (i in 1:3){
#     first.model<-AdaptiveAA.model(n.top=5,n.mom=6*21,n.vol=i*21)
#     j<-j+1
#     aaa[j,"Top"]=5
#     aaa[j,"Mom"]=6
#     aaa[j,"Vol"]=i
#     aaa[j,"P.ret"]=table.AnnualizedReturns(first.model$returns.monthly)[1,"Port"]
#     aaa[j,"P.sd"]=table.AnnualizedReturns(first.model$returns.monthly)[2,"Port"]
#     aaa[j,"P.shrp"]=table.AnnualizedReturns(first.model$returns.monthly)[3,"Port"]
#     aaa[j,"E.ret"]=table.AnnualizedReturns(first.model$returns.monthly)[1,"Bench"]
#     aaa[j,"E.sd"]=table.AnnualizedReturns(first.model$returns.monthly)[2,"Bench"]
#     aaa[j,"E.shrp"]=table.AnnualizedReturns(first.model$returns.monthly)[3,"Bench"]
#     aaa[j,"B.ret"]=table.AnnualizedReturns(first.model$returns.monthly)[1,"B6040"]
#     aaa[j,"B.sd"]=table.AnnualizedReturns(first.model$returns.monthly)[2,"B6040"]
#     aaa[j,"B.shrp"]=table.AnnualizedReturns(first.model$returns.monthly)[3,"B6040"]
# }
# 
# aaa





