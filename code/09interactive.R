# Script to analyses the cave log weather data
# by Hannes Becher

#install.packages("chron")
#install.packages("changepoint")
#?chron
library(chron)
library(changepoint)

setwd("~/git_repos/cave-logs/")
#dat <- read.table("data/Cave_log_from_20220428_to_20220507_.csv",
dat <- read.table("data/Cave_log_from_20220505_to_20220516_.csv",
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

# per minute
plot.day("22-05-06")
plot.day("22-05-07")
plot.day("22-05-08")
plot.day("22-05-09")
plot.day("22-05-10")
plot.day("22-05-11")
plot.day("22-05-12")
plot.day("22-05-13")
plot.day("22-05-14")
plot.day("22-05-15")

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

# per minute
plot.th("22-05-06")
plot.th("22-05-07")
plot.th("22-05-08")
plot.th("22-05-09")
plot.th("22-05-10")
plot.th("22-05-11")
plot.th("22-05-12")
plot.th("22-05-13")
plot.th("22-05-14")
plot.th("22-05-15")



# Differentiation and rle to look nto smoothing
head(dat)
plot(dat$tempC)
d01 <- diff(dat$tempC)
plot(d01)


under <- 10^seq(0.1,6, length=100)
ls <- sapply(under, function(x){
  length(cpts(cpt.mean(d01, method="PELT", penalty="Manual", pen.value=paste0("1/", x, "*log(n)"))))
})
plot(under, ls, log="")
cpt01 <- cpt.mean(d01, method="PELT", penalty="Manual", pen.value="1/100000*log(n)")
plot(cpt01)



abline(v=100000)
plot(cpt01)
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
sum(is.na(d02))
sum(is.infinite(d02))
d02 <- d02[is.finite(d02)]
ls2 <- sapply(under, function(x){
  length(cpts(cpt.mean((d02), method="PELT", penalty="Manual", pen.value=paste0("1/", x, "*log(n)"))))
})
plot(under, ls2, log="x")

cpt02 <- cpt.mean(d02, method="PELT", penalty="Manual", pen.value="1/100*log(n)")
str(cpt02)
signature(cpt02)
plot(cpt02)
plot(d02)
plot(plogis(d02))
plot(d01)
plot(d01r)
ld02 <- rle(d02)$lengths
sum(ld02)
sum(ld02[ld02>5])
rle(d01)
rle(d01r)

# one day
dat30 <- dat[dat$dati < chron(dates. = "22-04-30", format = "y-m-d") + 1 &
  dat$dati > chron(dates. = "22-04-30", format = "y-m-d"), ]
dat30 <- dat[dat$dati < chron(dates. = "22-05-10", format = "y-m-d") + 1 &
               dat$dati > chron(dates. = "22-05-10", format = "y-m-d"), ]
plot(tempC ~ dati,
     data=dat30)
diff(dat30$temp)
d30_02 <- diff2(dat30$temp, dat30$hum)
plot(d30_02)
d30_02 <- d30_02[is.finite(d30_02)]
plot(cpt.mean(d30_02, method = "PELT", penalty="Manual", pen.value = "1/100*log(n)"))
a <- cpt.mean(d30_02, method = "PELT", penalty="Manual", pen.value = "1/100*log(n)")
str(a)
plot(table(diff(a@cpts)))
ad <- diff(a@cpts)
adt <- table(ad)
str(adt)
plot(cumsum(adt*as.numeric(dimnames(adt)$ad))~
       as.numeric(dimnames(adt)$ad),
     ylim=c(0, 1500),
     type="l",
     ylab="Cumsum",
     xlab="Length of run")
points(cumsum(adt*as.numeric(dimnames(adt)$ad))~
        as.numeric(dimnames(adt)$ad))
grid()
which(diff(a@cpts)==50)
a@cpts[110:120]
dat30
