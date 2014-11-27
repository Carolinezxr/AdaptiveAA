#Adaptive Asset Allocation
# (after aaa_getpricedata.r)
library(xts)
library(fPortfolio)
library(timeSeries)
library(PerformanceAnalytics)
setwd("~/Quant Trading/AdaptiveAA")
load("~/Quant Trading/datasets/Quandl.rdata")
names(returns.daily) #names of asset classes

AdaptiveAA.model<-function(n.top=4,n.mom=6*21,n.vol=1*21){
    #n.top     # number of momentum positions
    #n.mom     # length of momentum look back
    #n.vol     # length of volatility look back
    model=list()
    model$n.top<-n.top
    model$n.mom<-n.mom
    model$n.vol<-n.vol
    n.classes<-dim(returns.daily)[2]
    periods.end.idx = endpoints(returns.daily, 'months')  #all of the month ends and the last date
    periods.end.idx = periods.end.idx[periods.end.idx >= max(n.mom,n.vol)] #include those with enough data to calc mom and vol
    periods.end.dt<-index(returns.daily)[periods.end.idx]
    n.mos  <- length(periods.end.dt)
    
    model$periods<-periods.end.dt[2:(n.mos-1)]
    results.monthly<-matrix(data=NA,nrow=n.mos-1,ncol=4,
                            dimnames=list(format(periods.end.dt[2:(n.mos)],"%Y-%m-%d"),c("Port","Bench","B6040","EqN")))
    allocations<-matrix(data=0,nrow=n.mos-1,ncol=n.classes,
                        dimnames=list(format(periods.end.dt[2:(n.mos)],"%Y-%m-%d"),names(returns.daily)))
    returns.ac<-returns.daily[(periods.end.idx[1]+1):periods.end.idx[n.mos],]
    model$sd.ac<-apply(returns.ac,2,sd)*sqrt(252)
    returns.ac<-apply(1+returns.ac,2,prod)^(12/(n.mos-1))-1
    model$returns.ac<-returns.ac
    for (i in 1:(n.mos-1)){
        # use momentum to figure out which asset classes to use
        mom.returns<-returns.daily[(periods.end.idx[i]-n.mom+1):periods.end.idx[i],]
        mom.rank<-apply(1+mom.returns,2,prod)-1
        mom.rank<-rank(-mom.rank) # select 1 thru n.top
        mom.idx<-mom.rank<=n.top  #use these asset classes    
        # use min variance portfolio to determine the weights of those classes
        vol.returns<-as.timeSeries(returns.daily[(periods.end.idx[i]-n.vol+1):periods.end.idx[i],mom.idx])  
        mv<-minvariancePortfolio(vol.returns)
        wts<-getWeights(mv) #wts in the portfolio
        # return from next day to last day of next period
        port.return<-returns.daily[(periods.end.idx[i]+1):periods.end.idx[i+1],mom.idx]
        port.return<-apply(1+port.return,2,prod)-1
        port.return<-sum(port.return * wts)
        results.monthly[i,"Port"]<-port.return
        eqn.return<-returns.daily[(periods.end.idx[i]+1):periods.end.idx[i+1],mom.idx]
        eqn.return<-apply(1+eqn.return,2,prod)-1
        eqn.return<-sum(eqn.return * rep(1/n.top,n.top))
        results.monthly[i,"EqN"]<-eqn.return
        #eq wtd return for benchmark
        eq.wtd.return<-returns.daily[(periods.end.idx[i]+1):periods.end.idx[i+1],]
        eq.wtd.return<-apply(1+eq.wtd.return,2,prod)-1
        eq.wtd.return<-mean(eq.wtd.return)
        results.monthly[i,"Bench"]<-eq.wtd.return
        #b6040 return
        b6040.return<-returns.daily[(periods.end.idx[i]+1):periods.end.idx[i+1],c("SP500","IntTsy")]
        b6040.return<-apply(1+b6040.return,2,prod)-1
        b6040.return<-sum(b6040.return * c(.6,.4))
        results.monthly[i,"B6040"]<-b6040.return
        allocations[i,mom.idx]<-wts
    }
    model$allocations<-as.timeSeries(allocations)
    transactions<-diff(as.matrix(allocations))
    model$turnover.monthly<-as.xts(rowSums(abs(transactions))/2)
    model$returns.monthly<-as.xts(results.monthly)
    model$turnover.mean<-mean(model$turnover.monthly)
    return(model)
}

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





