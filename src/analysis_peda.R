library(xlsx)
library(nortest)
library(data.table)
setwd("C:/study/Stat")
source("fun_getMeans.R")

#======================================================
#   Prepare data
#======================================================
# get the means of 5 phases for pp signals
CD_peda_df = getMeans("CD", "peda")
MD_peda_df = getMeans("MD", "peda")
ED_peda_df = getMeans("ED", "peda")

phaseNames = c("Phase1","Phase2","Phase3", "Phase4", "Phase5")
sessionNames = c("CD", "MD", "ED")
peda_df = list(CD_peda_df, MD_peda_df, ED_peda_df)
names(peda_df) = sessionNames

# calculate the difference between ND and testing sessions
peda_delta = lapply(peda_df, function(x) x[,7:11]-x[,2:6])
peda_delta = lapply(peda_delta, setNames, nm = phaseNames)

#======================================================
#   Normality analysis
#======================================================
# normality test and data transformation
#qqplot 
par(mfrow=c(3,5),mar=c(3,2,4,2),oma = c(2,1,2,2))
for(i in 1:3){
  for(j in 1:5){
    qqnorm(peda_delta[[i]][,j], main = phaseNames[j])
    qqline(peda_delta[[i]][,j], col="red")
  }
  mtext(sessionNames[i], side = 4, line =1)
}
mtext("QQ-plot of the changes in PEDA signals without transformation", side = 3, outer = TRUE)
lapply(peda_delta, function(x){lapply(x, shapiro.test)})
lapply(peda_delta, function(x){lapply(x, pearson.test)})

# normalityTest: none of the data is normal, need to transform the data
peda_delta_mins = lapply(peda_delta, function(x){lapply(x, min, na.rm = TRUE)})
peda_delta_mins = lapply(peda_delta_mins, function(x){lapply(x, abs)})
peda_delta_mins = lapply(peda_delta_mins, function(x){lapply(x, "*", 1.01)})
peda_delta_CD_transformed = log(peda_delta$CD + peda_delta_mins$CD)
peda_delta_MD_transformed = log(peda_delta$MD + peda_delta_mins$MD)
peda_delta_ED_transformed = log(peda_delta$ED + peda_delta_mins$ED)
peda_delta_transformed = list(peda_delta_CD_transformed, peda_delta_MD_transformed, peda_delta_ED_transformed)

#qqplot after transformation
par(mfrow=c(3,5),mar=c(3,2,4,2),oma = c(2,1,2,2))
for(i in 1:3){
  for(j in 1:5){
    qqnorm(peda_delta_transformed[[i]][,j], main = phaseNames[j])
    qqline(peda_delta_transformed[[i]][,j], col="red")
  }
  mtext(sessionNames[i], side = 4, line =1)
}
mtext("QQ-plot of the change in PEDA signals after log transformation", side = 3, outer = TRUE)

# normality test after
lapply(peda_delta_transformed, function(x){lapply(x, shapiro.test)})
lapply(peda_delta_transformed, function(x){lapply(x, pearson.test)})
# both shapiro and pearson test indicates the data are still not normal after transformation


#======================================================
#   Hypothesis testing
#======================================================
p_values = data.frame("Session"=character(), "phase1"=double(), "phase2"=double(),"phase3"=double(),"phase4"=double(),"phase5"=double(),stringsAsFactors = FALSE)
for (i in 1:3){
  p_values[i,1] = sessionNames[i]
  temp_p_vals = vector()
  for(j in 1:5){
    #p = t.test(peda_delta_transformed[[i]][,j],conf.level = (1-0.0125))
    p = wilcox.test(peda_delta[[i]][,j],conf.level = (1-0.0125))
    temp_p_vals = c(temp_p_vals, format(round(p$p.value,4),nsmall = 3))
  }
  print(c(sessionNames[i], temp_p_vals))
  p_values[i,2:6] = temp_p_vals
}
p_values[,2:6]=sapply(p_values[,2:6], as.numeric)

#======================================================
#   box plot
#======================================================
# plot the changes of pp signal
par(mfrow=c(3,1),mar=c(2,5,3,3),oma = c(2,1,2,2))
for(i in 1:3){
  b = boxplot(peda_delta[[i]],ylab = expression(paste(Delta, "PEDA", "[", ~degree*C^{2}, "]")), ylim=c(-1500,1000))
  #b = boxplot(peda_delta_transformed[[i]],ylab = expression(paste(Delta, "PEDA", "[", ~degree*C^{2}, "]")))
  text(1:length(b$n), c(rep(-1400,4)), paste("n=", b$n))
  abline(0,0, col="red")
  mtext(sessionNames[i], side = 4, line = 1)
}
#mtext("Validation of Palm EDA Signal Change", outer = TRUE)
mtext("Validation of transformed Palm EDA Signal Change", outer = TRUE)
