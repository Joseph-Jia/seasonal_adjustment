genhol_mod<-function (x, start = 0, end = 0, frequency = 12, center = "none") 
{
  #end小于1为节前效应，start大于0为节后效应，其余为节中效应，请分别调用三次本函数以求得三个节日效应变量
  if (!inherits(x, "Date")) {
    stop("x must be of class 'Date'. Use 'as.Date' to convert.")
  }
  if (!center %in% c("none", "calendar", "mean")) {
    stop("wrong center argument. Use 'mean', 'calendar' or 'none'.")
  }
  if (start > end) {
    stop("start cannot be after end")
  }
  event.st <- x + start
  event.en <- x + end
  frequency <- frequency
  if ((end - start) > 150) {
    stop("holiday length must =< 150")
  }
  z.ts <- ts(NA, start = as.numeric(format(event.st[1], "%Y")), 
             end = c(as.numeric(format(event.en[length(event.en)], 
                                       "%Y")), frequency), frequency = frequency)
  by <- switch(as.character(frequency), `12` = "month", `4` = "3 month", 
               `2` = "6 month", `1` = "year")
  period.st <- seq(from = as.Date(paste0(format(event.st[1], 
                                                "%Y"), "/1/1")), by = by, length.out = length(z.ts))
  suffix <- switch(as.character(frequency), `12` = "/2/1", 
                   `4` = "/4/1", `2` = "/7/1")
  period.en <- seq(from = as.Date(paste0(format(event.st[1], 
                                                "%Y"), suffix)), by = by, length.out = length(z.ts)) - 
    1
  first.day <- as.Date(paste0(start(z.ts)[1], "/1/1"))
  if (!first.day %in% event.st) {
    event.st.added <- c(first.day, event.st)
    event.st <- c(as.Date(NA), event.st)
  }
  if (!first.day %in% event.en) {
    event.en.added <- c(first.day, event.en)
    event.en <- c(as.Date(NA), event.en)
  }
  event.st.ts <- z.ts
  event.st.ts[cut(event.st.added, by, labels = F)] <- as.character(event.st)
  event.en.ts <- z.ts
  event.en.ts[cut(event.en.added, by, labels = F)] <- as.character(event.en)
  days <- pmin((period.en), as.Date(as.character(event.en.ts)), 
               na.rm = T) - pmax(period.st, as.Date(as.character(event.st.ts)), 
                                 na.rm = T) + 1
  days[days==period.en-period.st] <-0
  fillNA <- function(x) {
    x.span <- x
    x.i <- NA
    for (i in 1:length(x.span)) {
      if (!is.na(x.span[i])) {
        x.i <- x.span[i]
      }
      else {
        x.span[i] <- x.i
      }
    }
    x.span
  }
  drop <- is.na(event.st.ts) & is.na(event.en.ts)
  drop[fillNA(event.st.ts) > fillNA(event.en.ts)] <- FALSE
  days[drop] <- 0
  days <- ts(c(days), start = start(z.ts), frequency = frequency(z.ts))
  z.raw <- ts(rep(0,length(days)), start = start(z.ts), frequency = frequency(z.ts))
  if (end<0){
    i=1
    while (i<length(days)){
      j=i
      while (days[j]){
        j=j+1
      }
      if (j>i){
        csum<-c(0,cumsum(days[i:(j-1)]))
        for (k in seq(i,j-1)){
          z.raw[k]<-sum(seq(csum[k-i+1]+1,csum[k-i+2]))
        }
      }
      i=j+1
    }
    z.raw<-z.raw/sum(seq(end-start+1))
  }
  else {
    if (start>0){
      i=1
      while (i<length(days)){
        j=i
        while (days[j]){
          j=j+1
        }
        if (j>i){
          csum<- end-start+2-c(0,cumsum(days[i:(j-1)]))
          for (k in seq(i,j-1)){
            z.raw[k]<-sum(seq(csum[k-i+1]-1,csum[k-i+2]))
          }
        }
        i=j+1
      }
      z.leap<-z.raw
      z.raw<-z.raw/sum(seq(end-start+1))
    }
    else
    {
    z.raw <- ts(c(days), start = start(z.ts), frequency = frequency(z.ts))
    z.raw <- days/(end - start + 1)
    }
  }
  if (center == "mean") {
    z <- z.raw - mean(z.raw)
  }
  else if (center == "calendar") {
    z.mat <- t(matrix(z.raw, ncol = frequency(z.raw), byrow = TRUE))
    z.mat <- z.mat - rowMeans(z.mat)
    z <- ts(as.numeric((z.mat)), start = start(z.raw), frequency = frequency(z.raw))
  }
  else {
    z <- z.raw
  }
  z
}

genhol_stock<- function(x, pre, during, post, frequency = 12)
{
  #存量数据 pre为节前日数，during为节中日数，post为节后日数，调用一次即可
  #输出第一列为日期，第三列为节日效应
  if (!inherits(x, "Date")) {
    stop("x must be of class 'Date'. Use 'as.Date' to convert.")
  }
  if (pre<0 | during<0 | post<0) {
    stop("pre, during and post cannot be smaller than zero")
  }
  event.st <- x - pre
  event.en <- x + during+post
  
  z.ts <- ts(NA, start = as.numeric(format(event.st[1], "%Y")), 
             end = c(as.numeric(format(event.en[length(event.en)], 
                                       "%Y")), frequency), frequency = frequency)
  by <- switch(as.character(frequency), `12` = "month", `4` = "3 month", 
               `2` = "6 month", `1` = "year")
  period.st <- seq(from = as.Date(paste0(format(event.st[1], 
                                                "%Y"), "/1/1")), by = by, length.out = length(z.ts))
  suffix <- switch(as.character(frequency), `12` = "/2/1", 
                   `4` = "/4/1", `2` = "/7/1")
  period.en <- seq(from = as.Date(paste0(format(event.st[1], 
                                                "%Y"), suffix)), by = by, length.out = length(z.ts)) - 
    1
  first.day <- as.Date(paste0(start(z.ts)[1], "/1/1"))
  if (!first.day %in% event.st) {
    event.st.added <- c(first.day, event.st)
    event.st <- c(as.Date(NA), event.st)
  }
  if (!first.day %in% event.en) {
    event.en.added <- c(first.day, event.en)
    event.en <- c(as.Date(NA), event.en)
  }
  event.st.ts <- z.ts
  event.st.ts[cut(event.st.added, by, labels = F)] <- as.character(event.st)
  event.en.ts <- z.ts
  event.en.ts[cut(event.en.added, by, labels = F)] <- as.character(event.en)
  days <- pmin((period.en), as.Date(as.character(event.en.ts)), 
               na.rm = T) - pmax(period.st, as.Date(as.character(event.st.ts)), 
                                 na.rm = T) + 1
  for (i in seq(length(event.st.ts)))
  {
    if (i>1 & is.na(event.st.ts[i]))
    {
      event.st.ts[i]=event.st.ts[i-1]
    }
  }
  days <- period.en - as.Date(as.character(event.st.ts)) + 1
  days[days>(pre+during+post)]=0
  days[days<pre]=days[days<pre]/pre
  days[days>(pre+during)]=(pre+during+post+1-days[days>(pre+during)])/post
  days[days>1]=1
  z.raw <- ts(c(days), start = start(z.ts), frequency = frequency(z.ts))
  z.mat <- t(matrix(z.raw, ncol = frequency(z.raw), byrow = TRUE))
  z.mat <- z.mat - rowMeans(z.mat)
  z <- ts(as.numeric((z.mat)), start = start(z.raw), frequency = frequency(z.raw))
  date<-paste(floor(time(z)),'-',round((time(z)-floor(time(z)))*12+1,0),sep='')
  write.table(cbind(date,z),quote=FALSE,row.names=FALSE,col.names = FALSE, 'xreg_stock.csv')      
  z
}
genhol_flow<- function(x, pre, during, post, frequency = 12)
  {
  #流量数据 pre为节前日数，during为节中日数，post为节后日数，调用一次即可
  #输出第一列为日期，第二至第四列为节日节中和节后效应
  z1<-genhol_mod(x,start=-pre,end=-1,center='calendar',frequency=frequency)
  z2<-genhol_mod(x,start=0,end=during-1,center='calendar',frequency=frequency)
  z3<-genhol_mod(x,start=during,end=during+post-1,center='calendar',frequency=frequency)
  z<-cbind(z1,z2,z3)
  date<-paste(floor(time(z)),'-',round((time(z)-floor(time(z)))*12+1,0),sep='')
  write.table(cbind(date,z),quote=FALSE,row.names=FALSE,col.names = FALSE,'xreg_flow.csv')
  z
}
