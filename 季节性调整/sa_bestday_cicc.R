source('genhol_cicc.R')
crit_stock<-function (x,frequency=12,st=5,ed=20) {
  p=ed-st+1
  ans1=matrix(rep(0,p*p),p)
  ans2=matrix(rep(0,p*p),p)
  ans3=matrix(rep(0,p*p),p)
  for (i in seq(st,ed)){
    for (j in seq(st,ed)){
      mdl<-seas(x,xreg=genhol_stock(cny,pre=i,during=7,post=j,frequency=frequency),
                regression.aictest = NULL,regression.usertype='holiday')
      ans1[i-st+1,j-st+1]<-AIC(mdl)
      ans2[i-st+1,j-st+1]<-BIC(mdl)
      ans3[i-st+1,j-st+1]<-as.list(mdl$udg)$outlier.total
    }
  }
  ans<-rbind(paste('post=',st:ed,sep=''),ans1,paste('post=',st:ed,sep=''),
             ans2,paste('post=',st:ed,sep=''),ans3)
  ans<-cbind(c('AIC',paste('pre=',st:ed,sep=''),'BIC',paste('pre=',st:ed,sep=''),
    'No_outliers',paste('pre=',st:ed,sep='')),ans)
  ans
}
crit_flow<-function (x,frequency=12,st=5,ed=20) {
  p=ed-st+1
  ans1=matrix(rep(0,p*p),p)
  ans2=matrix(rep(0,p*p),p)
  ans3=matrix(rep(0,p*p),p)
  for (i in seq(st,ed)){
    for (j in seq(st,ed)){
      mdl<-seas(x,xreg=genhol_flow(cny,pre=i,during=7,post=j,frequency=frequency),
                regression.aictest = NULL,regression.usertype='holiday')
      ans1[i-st+1,j-st+1]<-AIC(mdl)
      ans2[i-st+1,j-st+1]<-BIC(mdl)
      ans3[i-st+1,j-st+1]<-as.list(mdl$udg)$outlier.total
    }
  }
  ans<-rbind(paste('post=',st:ed,sep=''),ans1,paste('post=',st:ed,sep=''),
             ans2,paste('post=',st:ed,sep=''),ans3)
  ans<-cbind(c('AIC',paste('pre=',st:ed,sep=''),'BIC',paste('pre=',st:ed,sep=''),
               'No_outliers',paste('pre=',st:ed,sep='')),ans)
  ans
}