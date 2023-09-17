# compare measurements from two units

setwd("~/git_repos/cave-logs/")

# front door
datF <- read.table("data/Front door_log_from_20230106_to_20230917_.csv",
                   sep=",",
                   header=T, fill = T
                   )[,1:3]
head(datF)
names(datF) <- c("Time", "TempF", "HumF")

# rec room
datR <- read.table("data/Rec room_log_from_20230516_to_20230917_.csv",
                   sep=",",
                   header=T,
                   fill=T)[,1:3]
names(datR) <- c("Time", "TempR", "HumR")

head(datR)
str(datR)

datF$Time <- as.POSIXct(datF$Time)
datR$Time <- as.POSIXct(datR$Time)


mm <- merge(datR, datF, by="Time")
head(mm)
mm$piDays <- as.numeric(mm$Time)/60/60/24*2*pi
str(mm)
plot(-cos(mm$piDays)[1:100])
plot(TempF ~ TempR, data=mm[1:1000,])
24*6
?ar
ar(lh)
acf(mm$TempF, lag.max = 150)
acf(mm$TempR)
acf(mm$TempR, mm$TempF)
?acf


lm01 <- lm(TempF ~ 1,
           data=mm)
lm02 <- lm(TempF ~ cos(piDays),
           data=mm)
lm03 <- lm(TempF ~ sin(piDays),
           data=mm)
lm04 <- lm(TempF ~ cos(piDays) + sin(piDays),
           data=mm)
summary(lm01)
summary(lm02)
summary(lm03)
summary(lm04)


anova(lm01, lm02)
anova(lm01, lm03) # not much better
anova(lm01, lm02, lm04)
par(mfrow=c(2,2))
plot(lm01)
plot(lm02)
par(mfrow=c(1,1))

sin(datF$Time)


plot(TempF~I((piDays/2/pi/365 - 53)*365),
     data=mm[1:1000,])
points(fitted(lm02)[1:1000] ~ I((piDays/2/pi/365 - 53)*365),
       data=mm[1:1000,], type="l", col="grey", lwd=2)
points(fitted(lm04)[1:1000] ~ I((piDays/2/pi/365 - 53)*365),
       data=mm[1:1000,], type="l", col="grey", lwd=2, lty=2)
grid()

plot((mm$piDays/pi/2 - 53*365), resid(lm04))
grid()

plot((mm$piDays/pi/2 - 53*365), resid(lm04),
     xlim=c(200, 210))
grid()

plot(TempF ~ I((piDays/2/pi/365 - 53)*365),
     data=mm)

tail(mm)

plot(mm$TempF, resid(lm04))



# GAMs --------------------------------------------------------------------



library(mgcv)

gam01 <- gam(TempF ~ cos(piDays) + sin(piDays) + s(piDays, k=30),
             data=mm)
summary(gam01)
gam.check(gam01)
