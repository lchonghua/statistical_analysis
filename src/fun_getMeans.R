library(xlsx)
source("fun_getFilePaths.R")
source("fun_findCommonSubjects.R")
source("fun_isInRange.R")
source("fun_getMeansOfFivePhases.R")

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