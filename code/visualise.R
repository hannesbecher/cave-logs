# Script to analyses the cave log weather data
# by Hannes Becher

#install.packages("chron")
#?chron
library(chron)
library(rgl)


setwd("~/git_repos/cave-logs/")
dat01 <- read.table("data/Cave_log_from_20220429_to_20220430_.csv",
                    sep=",",
                    header=T,
                    skip=1,
                    col.names = c("timestamp", "tempC", "hum"))
head(dat01)

dat01$date <- sapply(dat01$timestamp, function(x) strsplit(x, " ")[[1]][[1]])
dat01$time <- sapply(dat01$timestamp, function(x) strsplit(x, " ")[[1]][[2]])
dat01$chron <- chron(dat01$date,
                   dat01$time,
                   format=c(dates="y-m-d", times="h:m:s"))
head(dat01)

plot(dat01$tempC~dat01$chron,
     ylim=c(0, 70),
     type="l",
     ylab="Measure",
     xlab="Time")
points(dat01$hum~dat01$chron,
       type="l",
       lty=2)
grid()
legend("left",
       lty=c(2, 1),
       legend = c("Hum (%)", "Temp (C)"))

plot(dat01$tempC, dat01$hum)

# plot in 3d
plot3d(dat01[,c(2, 3, 6)])
