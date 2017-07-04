library(xlsx)
library(nortest)
library(data.table)
setwd("C:/study/Stat")
source("fun_getMeans.R")

#======================================================
#   Prepare data
#======================================================
# get the means of 5 phases for pp signals
CD_HR_df = getMeans("CD", "HR")
MD_HR_df = getMeans("MD", "HR")
ED_HR_df = getMeans("ED", "HR")

phaseNames = c("Phase1","Phase2","Phase3", "Phase4", "Phase5")
sessionNames = c("CD", "MD", "ED")
HR_df = list(CD_HR_df, MD_HR_df, ED_HR_df)
names(HR_df) = sessionNames

# calculate the difference between ND and testing sessions
HR_delta = lapply(HR_df, function(x) x[,7:11]-x[,2:6])
HR_delta = lapply(HR_delta, setNames, nm = phaseNames)

#======================================================
#   Normality analysis
#======================================================
# normality test and data transformation
#qqplot 
par(mfrow=c(3,5),mar=c(3,2,4,2),oma = c(2,1,2,2))
for(i in 1:3){
  for(j in 1:5){
    qqnorm(HR_delta[[i]][,j], main = phaseNames[j])
    qqline(HR_delta[[i]][,j], col="red")
  }
  mtext(sessionNames[i], side = 4, line = 1)
}
mtext("QQ-plot of the changes in HR signals without transformation", side = 3, outer = TRUE)

lapply(HR_delta, function(x){lapply(x,shapiro.test)})
lapply(HR_delta, function(x){lapply(x, pearson.test)})
# both test indicates the data are normally distributed 

#======================================================
#   Hypothesis testing
#======================================================
p_values = data.frame("Session"=character(), "phase1"=double(), "phase2"=double(),"phase3"=double(),"phase4"=double(),"phase5"=double(),stringsAsFactors = FALSE)
for (i in 1:3){
  p_values[i,1] = sessionNames[i]
  temp_p_vals = vector()
  for(j in 1:5){
    p = t.test(HR_delta[[i]][,j], conf.level = (1-0.0125))
    temp_p_vals = c(temp_p_vals, format(round(p$p.value,4),nsmall = 3))
  }
  print(c(sessionNames[i], temp_p_vals))
  p_values[i,2:6] = temp_p_vals
}

p_values[,2:6]=sapply(p_values[,2:6], as.numeric)

#======================================================
#   box plot
#======================================================
# plot the changes of HR signal
par(mfrow=c(3,1),mar=c(2,5,3,3),oma = c(2,1,2,2))
for(i in 1:3){
  b = boxplot(HR_delta[[i]],ylab = expression(paste(Delta, "HR [bpm]")), ylim=c(-20,15))
  text(1:length(b$n), c(rep(-19,4)), paste("n=", b$n))
  abline(0,0, col="red")
  mtext(sessionNames[i], side = 4, line = 1)
}
mtext("Validation of Heart Rate Signal Change", outer = TRUE)
