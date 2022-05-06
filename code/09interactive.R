# Script to analyses the cave log weather data
# by Hannes Becher

#install.packages("chron")
#?chron
library(chron)
library(rgl)
setwd("~/git_repos/cave-logs/")
dat <- read.table("data/Cave_log_from_20220428_to_20220507_.csv",
                  sep=",",
                  col.names = c("dati", "tempC", "hum"),
                  skip=1)
head(dat)


dat$date <- sapply(dat$dati, function(x) strsplit(x, " ")[[1]][[1]])
dat$time <- sapply(dat$dati, function(x) strsplit(x, " ")[[1]][[2]])
dat$dati <- chron(dat$date,
                   dat$time,
                   format=c(dates="y-m-d", times="h:m:s"))

plot.day <- function(day="22-05-02", l=F){
  d = chron(dates. = day, format = "y-m-d")
  ind <- (dat$dati >= (d)) & (dat$dati < (d+1))
  pdat <- dat[ind,]
  #print(pdat)
  cls <- rainbow(nrow(pdat))
  plot(hum ~ tempC,
       data=pdat,
       col=cls,
       main=day,
       ylab="Rel. humidity (%)",
       xlab="Temperature (C)")
  if(l){points(hum ~ tempC, type="l", data=pdat)}
  grid()
  fullHours <- pdat[substr(as.character(pdat$dati), 14, 18) == "00:00",]
  text(fullHours$tempC,
       fullHours$hum,
       labels=substr(as.character(fullHours$dati), 10, 12)
  )
}
plot.day("22-05-06")
plot.day("22-05-05")
plot.day("22-05-04")
plot.day("22-05-03")
plot.day("22-05-02")
plot.day("22-05-01")
plot.day("22-04-30")
plot.day("22-04-30", l=T)
plot.day("22-04-29")


plot.th <- function(day="22-05-02"){
  d = chron(dates. = day, format = "y-m-d")
  ind <- (dat$dati >= (d)) & (dat$dati < (d+1))
  pdat <- dat[ind,]
  
  cls <- rainbow(nrow(pdat))
  plot(tempC~dati,
       ylim=c(0, 70),
       ylab="Measure",
       xlab="Time",
       data=pdat)
  points(hum~dati,
         col="grey",
         data=pdat)
  grid()
  legend("topleft",
         fill=c("grey", "black"),
         legend = c("Hum (%)", "Temp (C)"))
  fullHours <- pdat[substr(as.character(pdat$dati), 14, 18) == "00:00",]
  # text(fullHours$tempC,
  #      fullHours$hum,
  #      labels=substr(as.character(fullHours$dati), 10, 12)
  # )
}

plot.th("22-04-29")
plot.th("22-04-30")
plot.th("22-05-01")
plot.th("22-05-02")
plot.th("22-05-03")
plot.th("22-05-04")
plot.th("22-05-05")
plot.th("22-05-06")
# Differentiation and rle to look nto smoothing
head(dat)
plot(dat$tempC)
d01 <- diff(dat$tempC)
plot(d01)
d01r <- d01 %/% 0.03 * 0.03
plot(d01r)
rle(d01r)$lengths
sum(rle(d01r)$lengths)
sum(rle(d01r)$lengths[rle(d01r)$lengths > 5])
rle(diff(dat$tempC))$lengths

diff2 <- function(x, y){
  a <- numeric()
  for(i in 2:length(x)){
    a[i-1] <- (x[i] - x[i-1])/(y[i] - y[i-1])
  }
  a
}
d02 <- diff2(dat$tempC, dat$hum)
plot(d02)
plot(plogis(d02))
plot(d01)
plot(d01r)
ld02 <- rle(d02)$lengths
sum(ld02)
sum(ld02[ld02>5])
rle(d01)
rle(d01r)

dat30 <- dat[dat$dati < chron(dates. = "22-04-30", format = "y-m-d") + 1 &
  dat$dati > chron(dates. = "22-04-30", format = "y-m-d"), ]
plot(tempC ~ dati,
     data=dat30)
diff(dat30$temp)
?diff
?rle
