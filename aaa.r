#Adaptive Asset Allocation
library(xts)
library(fPortfolio)
library(timeSeries)
library(PerformanceAnalytics)
library(corpcor)

myEstimator<-function(x,spec){
    result<-list()
    result$mu<-colMeans(x) #rep(0,dim(x)[2])
    result$Sigma<-corpcor::cov.shrink(x,verbose=F)
    return(result)
}
#find portfolio with max return with a sd less than the target, if none exist, min risk portfolio is returned
targetriskportfolio<-function(data,sd.target=0,nFrontierPoints=50){
    pspec<-portfolioSpec()
    setNFrontierPoints(pspec)=nFrontierPoints
    setEstimator(pspec)="myEstimator"
    Constraints="LongOnly"
    ep<-portfolioFrontier(data,pspec,Constraints)
    ep.weights<-getWeights(ep@portfolio)
    ep.points<-frontierPoints(ep)
    ep.min.ret<-ep.points[which.min(ep.points[,1]),2] #find return at min risk point
    ep.points.upper<-ep.points[ep.points[,2]>=ep.min.ret,]
    ep.weights.upper<-ep.weights[ep.points[,2]>=ep.min.ret,]
    idx.target<-max(1,sum(ep.points.upper[,1]<=sd.target))
    results=list()
    results$weights<-ep.weights.upper[idx.target,]    
    results$risk<-ep.points.upper[idx.target,1]
    results$return<-ep.points.upper[idx.target,2]
    return(results)
}
aaa<-function(data,n.top=4,n.mom=6*21,n.vol=1*21,target.sd=0){
    #generic function for adaptive asset allocation. 
    #n.top     # number of momentum positions
    #n.mom     # length of momentum look back
    #n.vol     # length of volatility look back
    model=list()
    model$n.top<-n.top
    model$n.mom<-n.mom
    model$n.vol<-n.vol
    n.classes<-dim(data)[2]
    periods.end.idx = endpoints(data, 'months')  #all of the month ends and the last date
    periods.end.idx = periods.end.idx[periods.end.idx >= max(n.mom,n.vol)] #include those with enough data to calc mom and vol
    periods.end.dt<-index(data)[periods.end.idx]
    n.mos  <- length(periods.end.dt)
    model$periods<-periods.end.dt[2:(n.mos-1)]
    results.monthly<-matrix(data=NA,nrow=n.mos-1,ncol=3,
                            dimnames=list(format(periods.end.dt[2:(n.mos)],"%Y-%m-%d"),c("Port","Bench","EqN")))
    allocations<-matrix(data=0,nrow=n.mos-1,ncol=n.classes,
                        dimnames=list(format(periods.end.dt[2:(n.mos)],"%Y-%m-%d"),names(data)))
    returns.ac<-data[(periods.end.idx[1]+1):periods.end.idx[n.mos],]
    model$sd.ac<-apply(returns.ac,2,sd)*sqrt(252)
    returns.ac<-apply(1+returns.ac,2,prod)^(12/(n.mos-1))-1
    model$returns.ac<-returns.ac
    
    pspec<-portfolioSpec()
    setEstimator(pspec)="myEstimator"
    Constraints="LongOnly"
    
    for (i in 1:(n.mos-1)){
        # use momentum to figure out which asset classes to use
        mom.returns<-data[(periods.end.idx[i]-n.mom+1):periods.end.idx[i],]
        mom.rank<-apply(1+mom.returns,2,prod)-1
        mom.rank<-rank(-mom.rank) # select 1 thru n.top
        mom.idx<-mom.rank<=n.top  #use these asset classes    
        # use min variance portfolio to determine the weights of those classes
        vol.returns<-as.timeSeries(data[(periods.end.idx[i]-n.vol+1):periods.end.idx[i],mom.idx])  
        if (target.sd<=0){
            mv<-suppressWarnings(minvariancePortfolio(vol.returns,pspec,Constraints))
            wts<-getWeights(mv) #wts in the portfolio
        } else {
            mv<-suppressWarnings(targetriskportfolio(vol.returns,target.sd))
            wts<-mv$weights
        }
        # return from next day to last day of next period
        port.return<-data[(periods.end.idx[i]+1):periods.end.idx[i+1],mom.idx]
        port.return<-apply(1+port.return,2,prod)-1
        port.return<-sum(port.return * wts)
        results.monthly[i,"Port"]<-port.return
        eqn.return<-data[(periods.end.idx[i]+1):periods.end.idx[i+1],mom.idx]
        eqn.return<-apply(1+eqn.return,2,prod)-1
        eqn.return<-sum(eqn.return * rep(1/n.top,n.top))
        results.monthly[i,"EqN"]<-eqn.return
        #eq wtd return for benchmark
        eq.wtd.return<-data[(periods.end.idx[i]+1):periods.end.idx[i+1],]
        eq.wtd.return<-apply(1+eq.wtd.return,2,prod)-1
        eq.wtd.return<-mean(eq.wtd.return)
        results.monthly[i,"Bench"]<-eq.wtd.return
        allocations[i,mom.idx]<-wts
    }
    model$allocations<-as.timeSeries(allocations)
    transactions<-diff(as.matrix(allocations))
    model$turnover.monthly<-as.xts(rowSums(abs(transactions))/2)
    model$returns.monthly<-as.xts(results.monthly)
    model$turnover.mean<-mean(model$turnover.monthly)
    return(model)
}
AdaptiveAA.model<-function(n.top=4,n.mom=6*21,n.vol=1*21,target.sd=0){
    #Should be deprecated. Use aaa instead.  This was original function for testing
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
    
    pspec<-portfolioSpec()
    setEstimator(pspec)="myEstimator"
    Constraints="LongOnly"
    
    for (i in 1:(n.mos-1)){
        # use momentum to figure out which asset classes to use
        mom.returns<-returns.daily[(periods.end.idx[i]-n.mom+1):periods.end.idx[i],]
        mom.rank<-apply(1+mom.returns,2,prod)-1
        mom.rank<-rank(-mom.rank) # select 1 thru n.top
        mom.idx<-mom.rank<=n.top  #use these asset classes    
        # use min variance portfolio to determine the weights of those classes
        vol.returns<-as.timeSeries(returns.daily[(periods.end.idx[i]-n.vol+1):periods.end.idx[i],mom.idx])  
        if (target.sd<=0){
            mv<-suppressWarnings(minvariancePortfolio(vol.returns,pspec,Constraints))
            wts<-getWeights(mv) #wts in the portfolio
        } else {
            mv<-suppressWarnings(targetriskportfolio(vol.returns,target.sd))
            wts<-mv$weights
        }
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
        b6040.return<-returns.daily[(periods.end.idx[i]+1):periods.end.idx[i+1],c("US","IntTsy")]
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