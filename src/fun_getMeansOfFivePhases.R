# this function splits the input file into 5 phases
# according to the 4 points in the splits
# return the means of each phases
getMeansOfFivePhases = function(theFile, splits){
  p1 = theFile[theFile[,2] < splits[1],] #2nd column in theFile is Time
  p1_mean = mean(p1[,3], na.rm = TRUE) #3rd column in theFile is the signal
  
  p2 = theFile[theFile[,2] >= splits[1] & theFile[,2] < splits[2],]
  p2_mean = mean(p2[,3], na.rm = TRUE)
  
  p3 = theFile[theFile[,2] >= splits[2] & theFile[,2] < splits[3],]
  p3_mean = mean(p3[,3], na.rm = TRUE)
  
  p4 = theFile[theFile[,2] >= splits[3] & theFile[,2] < splits[4],]
  p4_mean = mean(p4[,3], na.rm = TRUE)
  
  p5 = theFile[theFile[,2] >= splits[4],]
  p5_mean = mean(p5[,3], na.rm = TRUE)
  
  means = c(p1_mean, p2_mean, p3_mean, p4_mean, p5_mean)
  #print(means)
  return(means)
}