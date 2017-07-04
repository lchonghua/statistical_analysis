library(xlsx)
library(nortest)
library(data.table)
setwd("C:/study/Stat")
source("fun_getMeans.R")

#======================================================
#   Prepare data
#======================================================
# get the means of 5 phases for pp signals
phaseNames = c("Phase1","Phase2","Phase3", "Phase4", "Phase5")
sessionNames = c("CD", "MD", "ED")
CD_PP_df = getMeans("CD", "pp")
MD_PP_df = getMeans("MD", "pp")
ED_PP_df = getMeans("ED", "pp")
pp_df = list(CD_PP_df, MD_PP_df, ED_PP_df)
names(pp_df) = sessionNames

# calculate the difference between ND and testing sessions
pp_delta = lapply(pp_df, function(x) x[,7:11]-x[,2:6])
pp_delta = lapply(pp_delta, setNames, nm = phaseNames)

#======================================================
#   Normality analysis
#======================================================
# normality test and data transformation
lapply(pp_delta, function(x){lapply(x, shapiro.test)})
lapply(pp_delta, function(x){lapply(x, pearson.test)})

#qqplot
par(mfrow=c(3,5),mar=c(3,2,4,2),oma = c(2,1,2,2))
for(i in 1:3){
  for(j in 1:5){
    qqnorm(pp_delta[[i]][,j], main = phaseNames[j])
    qqline(pp_delta[[i]][,j], col="red")
  }
  mtext(sessionNames[i], side = 4, line =1)
}
mtext("QQ-plot of the changes in pp signals before transformation", side = 3, outer = TRUE)

#normBefore_df = rbindlist(normalBefore, fill=TRUE, use.names = TRUE)
#write.table(normBefore_df, file="pp_normalityTestBefore.csv", sep=",", col.names = TRUE, row.names = FALSE)

# normalityTest: all columns of each session in pp_df are not normal
# transform the data: since there are negative values, find the minimum values 
# for each phase of each drive type, add abs(min *1.01) to all data in that column
pp_delta_mins = lapply(pp_delta, function(x){lapply(x, min, na.rm = TRUE)})
pp_delta_mins = lapply(pp_delta_mins, function(x){lapply(x, abs)})
pp_delta_mins = lapply(pp_delta_mins, function(x){lapply(x, "*", 1.01)})
pp_delta_CD_transformed = log(pp_delta$CD+pp_delta_mins$CD)
pp_delta_MD_transformed = log(pp_delta$MD+pp_delta_mins$MD)
pp_delta_ED_transformed = log(pp_delta$ED+pp_delta_mins$ED)
pp_delta_transformed = list(pp_delta_CD_transformed, pp_delta_MD_transformed, pp_delta_ED_transformed)

#qqplot after transformation
par(mfrow=c(3,5),mar=c(3,2,4,2),oma = c(2,1,2,2))
for(i in 1:3){
  for(j in 1:5){
    qqnorm(pp_delta_transformed[[i]][,j], main = phaseNames[j])
    qqline(pp_delta_transformed[[i]][,j], col="red")
  }
  mtext(sessionNames[i], side = 4, line =1)
}
mtext("QQ-plot of the change in pp signals after log transformation", side = 3, outer = TRUE)
# normality test after
lapply(pp_delta_transformed, function(x){lapply(x, shapiro.test)})
lapply(pp_delta_transformed, function(x){lapply(x, pearson.test)})

#======================================================
#   Hypothesis testing
#======================================================
# wilcox.test(x, y, paired=TRUE)
p_values = data.frame("Session"=character(), "phase1"=double(), "phase2"=double(),"phase3"=double(),"phase4"=double(),"phase5"=double(),stringsAsFactors = FALSE)
for (i in 1:3){
  p_values[i,1] = sessionNames[i]
  temp_p_vals = vector()
  for(j in 1:5){
    #p = wilcox.test(pp_delta[[i]][,j],conf.level = (1-0.0125))
    p = t.test(pp_delta[[i]][,j],conf.level = (1-0.0125))
    #p = t.test(pp_delta_transformed[[i]][,j],conf.level = (1-0.0125))
    temp_p_vals = c(temp_p_vals, format(round(p$p.value,4),nsmall = 3))
  }
  print(c(sessionNames[i], temp_p_vals))
  p_values[i,2:6] = temp_p_vals
}

p_values[,2:6]=sapply(p_values[,2:6], as.double)


#======================================================
#   box plot
#======================================================
  # plot the changes of pp signal
  par(mfrow=c(3,1),mar=c(2,5,3,3),oma = c(2,1,2,2))
  for(i in 1:3){
    b = boxplot(pp_delta[[i]],ylim = c(-0.004, 0.004),ylab = expression(paste(Delta, "pp", "[",~degree*C^{2}, "]")))
    #b = boxplot(pp_delta_transformed[[i]],ylab = expression(paste(Delta, "pp", "[",~degree*C^{2}, "]")))
    text(1:length(b$n), c(rep(-0.0035,4)), paste("n=", b$n))
    
    abline(0,0, col="red")
    mtext(sessionNames[i], side = 4, line = 1)
  }
  #mtext("Validation of Perinasal Perspiration Signal Change", outer = TRUE)
  mtext("Validation of transformed Perinasal Perspiration Signal Change", outer = TRUE)

