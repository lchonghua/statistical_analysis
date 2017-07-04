library(xlsx)
library(nortest)
library(data.table)
setwd("C:/study/Stat")
source("fun_getMeans.R")

#======================================================
#   Prepare data
#======================================================
# get the means of 5 phases for pp signals
CD_BR_df = getMeans("CD", "BR")
MD_BR_df = getMeans("MD", "BR")
ED_BR_df = getMeans("ED", "BR")

phaseNames = c("Phase1","Phase2","Phase3", "Phase4", "Phase5")
sessionNames = c("CD", "MD", "ED")
BR_df = list(CD_BR_df, MD_BR_df, ED_BR_df)
names(BR_df) = sessionNames

# calculate the difference between ND and testing sessions
BR_delta = lapply(BR_df, function(x) x[,7:11]-x[,2:6])
BR_delta = lapply(BR_delta, setNames, nm = phaseNames)

#======================================================
#   Normality analysis
#======================================================
# normality test and data transformation
#qqplot 
par(mfrow=c(3,5),mar=c(3,2,4,2),oma = c(2,1,2,2))
for(i in 1:3){
  for(j in 1:5){
    qqnorm(BR_delta[[i]][,j], main = phaseNames[j])
    qqline(BR_delta[[i]][,j], col="red")
  }
  mtext(sessionNames[i], side = 4, line =1)
}
mtext("QQ-plot of the changes in BR signals without transformation", side = 3, outer = TRUE)
lapply(BR_delta, function(x){lapply(x, shapiro.test)})
lapply(BR_delta, function(x){lapply(x, pearson.test)})
# pearson.test indicates that the majority of the data are normally distributed

#======================================================
#   Hypothesis testing
#======================================================
p_values = data.frame("Session"=character(), "phase1"=double(), "phase2"=double(),"phase3"=double(),"phase4"=double(),"phase5"=double(),stringsAsFactors = FALSE)
for (i in 1:3){
  p_values[i,1] = sessionNames[i]
  temp_p_vals = vector()
  for(j in 1:5){
    p = t.test(BR_delta[[i]][,j],conf.level = (1-0.0125))
    temp_p_vals = c(temp_p_vals, format(round(p$p.value,4),nsmall = 3))
  }
  print(c(sessionNames[i], temp_p_vals))
  p_values[i,2:6] = temp_p_vals
}
p_values[,2:6]=sapply(p_values[,2:6], as.numeric)

#======================================================
#   box plot
#======================================================
# plot the changes of BR signal
par(mfrow=c(3,1),mar=c(2,5,3,3),oma = c(2,1,2,2))
for(i in 1:3){
  b = boxplot(BR_delta[[i]],ylab = expression(paste(Delta, "BR [bpm]")), ylim = c(-10,10))
  text(1:length(b$n), c(rep(-9,4)), paste("n=", b$n))
  abline(0,0, col="red")
  mtext(sessionNames[i], side = 4, line = 1)
}
mtext("Validation of Breathing Rate Signal Change", outer = TRUE)
