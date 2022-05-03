# Script to analyses the cave log weather data
# by Hannes Becher

#install.packages("chron")
#?chron
library(chron)
library(rgl)
setwd("~/git_repos/cave-logs/")
dat <- read.table("data/Cave_log_from_20220428_to_20220504_ (2).csv",
                  sep=",",
                  col.names = c("dati", "tempC", "hum"),
                  skip=1)
head(dat)


dat$date <- sapply(dat$dati, function(x) strsplit(x, " ")[[1]][[1]])
dat$time <- sapply(dat$dati, function(x) strsplit(x, " ")[[1]][[2]])
dat$dati <- chron(dat$date,
                   dat$time,
                   format=c(dates="y-m-d", times="h:m:s"))
dat$dati == "22-05-02"
chron(dates. = "22-05-02", format = "y-m-d") + 1

plot.day <- function(day="22-05-02"){
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
  fullHours <- pdat[substr(as.character(pdat$dati), 14, 18) == "00:00",]
  text(fullHours$tempC,
       fullHours$hum,
       labels=substr(as.character(fullHours$dati), 10, 12)
  )
}
plot.day("22-05-03")
plot.day("22-05-02")
plot.day("22-05-01")
plot.day("22-04-30")
plot.day("22-04-29")
chron(dates. = "22-05-02", format = "y-m-d") + 1 >
  chron(dates. = "22-05-02", times. = "00:00:01", format = c(dates="y-m-d", times="h:m:s"))


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
