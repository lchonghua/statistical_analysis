# this function checks the data quality
# input: dataframe containing the data read from xlsx file (.pp, .peda, .HR, .BR), and the file type
# output: boolean

isInRange = function(theFile, fileType){
  flag = TRUE
  if(fileType == "peda"){
    if(max(theFile[,3]) > 4700 | min(theFile[,3]) < 10)
      flag = FALSE
  }else if(fileType == "HR"){
    if(max(theFile[,3]) > 120 | min(theFile[,3]) < 40)
      flag = FALSE
  }else if(fileType == "BR"){
    if(max(theFile[,3]) > 70 | min(theFile[,3]) < 4)
      flag = FALSE
  }
  return(flag)
}