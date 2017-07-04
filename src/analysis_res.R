library(xlsx)
setwd("C:/study/Stat")
source("getRESMeans.R")

CDAllResMeansDF = getRESMeans("CD")
MDALLResMeansDF = getRESMeans("MD")
EDAllResMeansDF = getRESMeans("ED")

# put the dataframe for the same signal in a list
speed_df = list(CDAllResMeansDF$speed, MDALLResMeansDF$speed, EDAllResMeansDF$speed)
acc_df = list(CDAllResMeansDF$acceleration, MDALLResMeansDF$acceleration, EDAllResMeansDF$acceleration)
brake_df = list(CDAllResMeansDF$brake, MDALLResMeansDF$brake, EDAllResMeansDF$brake)
steer_df = list(CDAllResMeansDF$steer, MDALLResMeansDF$steer, EDAllResMeansDF$steer)
lane_df = list(CDAllResMeansDF$lane, MDALLResMeansDF$lane, EDAllResMeansDF$lane)

sessionNames = c("CD", "MD", "ED")
names(speed_df) = sessionNames
names(acc_df) = sessionNames
names(brake_df) = sessionNames
names(steer_df) = sessionNames
names(lane_df) = sessionNames

# calculate the difference between ND and testing sessions
phaseNames = c("Phase1","Phase2","Phase3", "Phase4", "Phase5")

speed_delta = lapply(speed_df, function(x) x[,7:11]-x[,2:6])
speed_delta = lapply(speed_delta, setNames, nm = phaseNames)

acc_delta = lapply(acc_df, function(x) x[,7:11]-x[,2:6])
acc_delta = lapply(acc_delta, setNames, nm = phaseNames)

brake_delta = lapply(brake_df, function(x) x[,7:11]-x[,2:6])
brake_delta = lapply(brake_delta, setNames, nm = phaseNames)

steer_delta = lapply(steer_df, function(x) x[,7:11]-x[,2:6])
steer_delta = lapply(steer_delta, setNames, nm = phaseNames)

lane_delta = lapply(lane_df, function(x) x[,7:11]-x[,2:6])
lane_delta = lapply(lane_delta, setNames, nm = phaseNames)

# plot the changes the signals
par(mfrow=c(3,1),mar=c(2,5,3,3),oma = c(2,1,2,2))
for(i in 1:3){
  boxplot(speed_delta[[i]],ylab = "Speed change [kph]", ylim = c(-30, 30))
  abline(0,0, col="red")
  mtext(sessionNames[i], side = 4, line = 1)
}
mtext("Validation of Speed Change", outer = TRUE)

par(mfrow=c(3,1),mar=c(2,5,3,3),oma = c(2,1,2,2))
for(i in 1:3){
  boxplot(acc_delta[[i]],ylab =expression("Acceleration change ["~degree*"]"), ylim=c(-6,8))
  abline(0,0, col="red")
  mtext(sessionNames[i], side = 4, line = 1)
}
mtext("Validation of Acceleration Change", outer = TRUE)

par(mfrow=c(3,1),mar=c(2,5,3,3),oma = c(2,1,2,2))
for(i in 1:3){
  boxplot(brake_delta[[i]],ylab = "Braking force change [N]", ylim = c(-150,150))
  abline(0,0, col="red")
  mtext(sessionNames[i], side = 4, line = 1)
}
mtext("Validation of braking singal change", outer = TRUE)

par(mfrow=c(3,1),mar=c(2,5,3,3),oma = c(2,1,2,2))
for(i in 1:3){
  boxplot(steer_delta[[i]],ylab = "Steering change [rad]", ylim = c(-0.1, 0.8))
  abline(0,0, col="red")
  mtext(sessionNames[i], side = 4, line = 1)
}
mtext("Validation of steering singal Change", outer = TRUE)

par(mfrow=c(3,1),mar=c(2,5,3,3),oma = c(2,1,2,2))
for(i in 1:3){
  boxplot(lane_delta[[i]],ylab = "Lane pos. change [m]", ylim = c(-1.5, 1.5) )
  abline(0,0, col="red")
  mtext(sessionNames[i], side = 4, line = 1)
}
mtext("Validation of lane position change", outer = TRUE)

#normality test and data transformation