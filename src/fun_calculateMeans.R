library(xlsx)
setwd("C:/study/Stat")

# this function returns the list of paths of the files in the specified session
# input1: drive type, such as "CD", "ND", "MD", etc
# input2: file type, such as "pp", "HR", "stm", etc
# return: "./T001/5 CD/T001-005.stm" "./T002/5 CD/T002-005.stm" "./T003/6 CD/T003-005.stm" "./T004/7 CD/T004-005.stm", ...
getFilePaths = function(driveType, fileType){
  subject = list.files(full.names = TRUE)
  subfolders = list.files(subject, full.names = TRUE, pattern = driveType)
  filePaths = list.files(subfolders, full.names = TRUE,  pattern = fileType)
  
  return (filePaths)
}

# this function returns the common subjects from the path lists generated in getFilePaths
# inputs are the results from getFilePaths
# for example: input1 - cd_pp_files; input2 - nd_pp_files; input3 - cd_stm_files
# return: a list of subject IDs that can be found in all three input lists
findCommonSubjects = function(pathList1, pathList2, pathList3){
  list1 = sapply(strsplit(pathList1, "/"), "[", 2)
  list2 = sapply(strsplit(pathList2, "/"), "[", 2)
  list3 = sapply(strsplit(pathList3, "/"), "[", 2)
  print("ss nd stm subject counts:")
  print(c(length(list1), length(list2), length(list3)))
  
  return(Reduce(intersect, list(list1, list2, list3)))
}

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

# this function split the designated signal in the specified session into 5 phases
# and calculate the means of each phase
# it will also calculate the signal in the ND session by default
# it reads in the stm file in the specified session by default
# output: a dataframe containing the subject ID, and the means of 5 phases from ND and the session
getMeans = function(driveType, fileType){
  sessionFilePaths = getFilePaths(driveType, fileType)
  NDFilePaths = getFilePaths("ND", fileType)
  stmFilePaths = getFilePaths(driveType, "stm")
  
  commonSubjectIDs = findCommonSubjects(sessionFilePaths, NDFilePaths, stmFilePaths)
  print("common subject counts:")
  print(length(commonSubjectIDs))
  
  # make dataframe for the means
  means = data.frame("ID"=character(), "nd_p1_means" = double(), "nd_p2_means" = double(), 
                     "nd_p3_means" = double(),"nd_p4_means" = double(), "nd_p5_means" = double(), 
                     "ss_p1_means" = double(), "ss_p2_means" = double(), "ss_p3_means" = double(), 
                     "ss_p4_means" = double(), "ss_p5_means" = double(), stringsAsFactors = FALSE)
  print(driveType)
  
  counts = 0;
  for(subjectID in commonSubjectIDs){
    print(subjectID)  
    gc()
    
    if(fileType == "pp")
      colindex = c(1,2,4)
    else
      colindex = c(1,2,3)
    
    # read .stm file
    stm_file_path = stmFilePaths[which(grepl(subjectID, stmFilePaths))[1]]
    stm_file = read.xlsx2(stm_file_path, sheetIndex = 1, header = TRUE, startRow = 9, endRow = 11, colIndex = 1:2, colClasses=rep("numeric",2))
    splitPoints = c(stm_file$StartTime[[1]], stm_file$EndTime[[1]], stm_file$StartTime[[2]], stm_file$EndTime[[2]])
    #print(splitPoints)
    
    # read file in ND and the specified session and perform quality control
    # if the data is not in range, skip this subject
    nd_file_path = NDFilePaths[which(grepl(subjectID, NDFilePaths))[1]]
    nd_file = read.xlsx2(nd_file_path, sheetIndex = 1, header = TRUE, startRow = 9, colIndex = colindex, colClasses=rep("numeric",3), as.data.frame = TRUE)
    
    session_file_path = sessionFilePaths[which(grepl(subjectID, sessionFilePaths))[1]]
    session_file = read.xlsx2(session_file_path, sheetIndex = 1, header = TRUE, startRow = 9, colIndex = colindex, colClasses=rep("numeric",3), as.data.frame = TRUE)
    
    if(!isInRange(nd_file, fileType)|!isInRange(session_file, fileType)) next
    counts = counts+1
    
    # split the data into 5 phases and calculate the means
    nd_file = nd_file[complete.cases(nd_file),]
    nd_means = getMeansOfFivePhases(nd_file, splitPoints)

    session_file = session_file[complete.cases(session_file),]
    session_means = getMeansOfFivePhases(session_file, splitPoints)
    
    # combine the means from ND and session together with subject ID into one vector
    # append the vector as a row into the means dataframe
    means[nrow(means)+1,] = c(subjectID, nd_means, session_means)
  }
  print("final subject counts")
  print(counts)
  means[,2:11]=sapply(means[,2:11], as.numeric)
  return(means)
}
