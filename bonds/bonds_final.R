###########################BONDS FINAL ######################################

bondprc<-function(coupon,maturity,yield,par,coupon.freq){
  periods=maturity*coupon.freq
  coupon.period=coupon/coupon.freq
  yield.period=yield/coupon.freq
  bond.coupon<-rep(coupon.period,times=periods,length.out=NA,each=1)
  bond.df<-as.data.frame(bond.coupon)
  for (i in 1:periods) {
    bond.df$cf[i]=par*coupon.period
    bond.df$period[i]=i
    bond.df$yield[i]=yield.period
  }
  bond.df$cf[periods]=bond.df$cf[periods]+par
  bond.df$PV=bond.df$cf/((1+bond.df$yield)^bond.df$period)
  value=sum(bond.df$PV)
  value
}

#Calculating the value of a bond
coupon=0.0425
maturity=4
yield=0.017
par=1000
coupon.freq=1

price <- bondprc(coupon,maturity,yield,par,coupon.freq)
price