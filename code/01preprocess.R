# Code to process and concatenate individual weather files
# by Hannes Becher

#install.packages("chron")
library(chron)
setwd("~/git_repos/cave-logs/")

files <- dir("data/", pattern = "csv")

read.wdat <- function(fl){
  read.table(fl,sep=",",
             header=T,
             skip=1,
             col.names = c("timestamp", "tempC", "hum")
  )
}

dat <- read.wdat(paste0("data/", files[1]))
for(i in files[-1]){
  dat <- rbind(dat, read.wdat(paste0("data/", i)))
}

head(dat)
tail(dat)
dat$date <- sapply(dat$timestamp, function(x) strsplit(x, " ")[[1]][[1]])
dat$time <- sapply(dat$timestamp, function(x) strsplit(x, " ")[[1]][[2]])
dat$chron <- chron(dat$date,
                     dat$time,
                     format=c(dates="y-m-d", times="h:m:s"))
head(dat)
save(dat, file = "processed/dat.R")
