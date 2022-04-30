# Script to analyses the cave log weather data
# by Hannes Becher

#install.packages("chron")
#?chron
library(chron)
library(rgl)

load(file = "processed/dat.R")
head(dat)
str(dat)
plot(dat$tempC~dat$chron,
     ylim=c(0, 70),
     ylab="Measure",
     xlab="Time")
points(dat$hum~dat$chron,
       col="grey")
grid()
legend("topleft",
       fill=c("grey", "black"),
       legend = c("Hum (%)", "Temp (C)"))

plot(dat$tempC,
     dat$hum,
#     type="l",
     xlab="Temp (C)",
     ylab="Hum (%)")
grid()

# plot in 3d
plot3d(dat[,c(2, 3, 6)])
