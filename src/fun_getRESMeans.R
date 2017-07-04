library(xlsx)
source("getFilePaths.R")
source("findCommonSubjects.R")
source("isInRange.R")
source("getMeansOfFivePhases.R")

# this function split all the RES variables in the specified session into 5 phases
# and calculate the means of each phase of each variable
# it will also calculate the signal in the ND session by default
# it reads in the stm file in the specified session by default
# output: a list dataframe containing the means dataframe for each RES variable
# in each dataframe, there are 11 columns. col1-subjectID, col2-6-the means of five phases in ND sesssion, col7-11-means of designated session
getRESMeans = function(driveType){
  sessionFilePaths = getFilePaths(driveType, "res")
  NDFilePaths = getFilePaths("ND", "res")
  stmFilePaths = getFilePaths(driveType, "stm")
  
  commonSubjectIDs = findCommonSubjects(sessionFilePaths, NDFilePaths, stmFilePaths)
  print("common subject counts:")
  print(length(commonSubjectIDs))
  
  # make dataframe for the means
  speed_means = data.frame("ID"=character(), "nd_p1_means" = double(), "nd_p2_means" = double(), 
                     "nd_p3_means" = double(),"nd_p4_means" = double(), "nd_p5_means" = double(), 
                     "ss_p1_means" = double(), "ss_p2_means" = double(), "ss_p3_means" = double(), 
                     "ss_p4_means" = double(), "ss_p5_means" = double(), stringsAsFactors = FALSE)
  acc_means = speed_means
  brake_means = speed_means
  steer_means = speed_means
  lane_means = speed_means
  print(driveType)
  
  counts = 0;
  for(subjectID in commonSubjectIDs){
    print(subjectID)  
    gc()
    
    
    # read .stm file
    stm_file_path = stmFilePaths[which(grepl(subjectID, stmFilePaths))[1]]
    stm_file = read.xlsx2(stm_file_path, sheetIndex = 1, header = TRUE, startRow = 9, endRow = 11, colIndex = 1:2, colClasses=rep("numeric",2))
    splitPoints = c(stm_file$StartTime[[1]], stm_file$EndTime[[1]], stm_file$StartTime[[2]], stm_file$EndTime[[2]])
    #print(splitPoints)
    
    # read file in ND and the specified session and perform quality control
    # if the data is not in range, skip this subject
    nd_file_path = NDFilePaths[which(grepl(subjectID, NDFilePaths))[1]]
    nd_file = read.xlsx2(nd_file_path, sheetIndex = 1, header = TRUE, startRow = 9, colIndex = 1:8, colClasses=rep("numeric",8), as.data.frame = TRUE)
    
    session_file_path = sessionFilePaths[which(grepl(subjectID, sessionFilePaths))[1]]
    session_file = read.xlsx2(session_file_path, sheetIndex = 1, header = TRUE, startRow = 9, colIndex = 1:8, colClasses=rep("numeric",8), as.data.frame = TRUE)
    
    # clean the data: col1:Frame; col2:Time; 
    # col3 - Speed: replace the values within the range of [-0.1 - +0.1] kph with 0. If the value is negative, mark it as NA.
    nd_file[3][nd_file[3]< -0.1]=NA
    nd_file[3][nd_file[3] > -0.1 & nd_file[3] <0.1]=0
    session_file[3][session_file[3]< -0.1]=NA
    session_file[3][session_file[3] > -0.1 & session_file[3] <0.1]=0
    
    # col4 - Acceleration: replace the negative value with NA.
    nd_file[4][nd_file[4] < 0]=NA
    session_file[4][session_file[4] < 0]=NA
    
    # col5 - Braking: replace values greater than 300N with 300N.
    nd_file[5][nd_file[5] > 300]=300
    session_file[5][session_file[5] > 300]=300
    
    # col6 - Steering: take absolute value
    nd_file[6] = abs(nd_file[6])
    session_file[6] = abs(session_file[6])
    
    # col7 - Lane.Offset; col8 - Lane.Position: we subtract the lane offset from the lane position.
    nd_file[8] = nd_file[8] - nd_file[7]
    session_file[8] = session_file[8] - session_file[7]
    
    # after cleaning, cols 1-Frame; 2-Time; 3-Speed; 4-Acceleration; 5-Braking; 6-Steering; 8-lane pos.
    
    counts = counts+1
    
    # split the data into 5 phases and calculate the means
    nd_speed_df = nd_file[, 1:3]
    nd_speed_df = nd_speed_df[complete.cases(nd_speed_df),]
    nd_speed_means = getMeansOfFivePhases(nd_speed_df, splitPoints)
    session_speed_df = session_file[, 1:3]
    session_speed_df = session_file[complete.cases(session_speed_df),]
    session_speed_means = getMeansOfFivePhases(session_speed_df, splitPoints)
    
    nd_acc_df = nd_file[, c(1,2,4)]
    nd_acc_df = nd_acc_df[complete.cases(nd_acc_df),]
    nd_acc_means = getMeansOfFivePhases(nd_acc_df, splitPoints)
    session_acc_df = session_file[, c(1,2,4)]
    session_acc_df = session_acc_df[complete.cases(session_acc_df),]
    session_acc_means = getMeansOfFivePhases(session_acc_df, splitPoints)
    
    nd_brake_df = nd_file[, c(1,2,5)]
    nd_brake_df = nd_brake_df[complete.cases(nd_brake_df),]
    nd_brake_means = getMeansOfFivePhases(nd_brake_df, splitPoints)
    session_brake_df = session_file[, c(1,2,5)]
    session_brake_df = session_brake_df[complete.cases(session_brake_df),]
    session_brake_means = getMeansOfFivePhases(session_brake_df, splitPoints)
    
    nd_steer_df = nd_file[, c(1,2,6)]
    nd_steer_df = nd_steer_df[complete.cases(nd_steer_df),]
    nd_steer_means = getMeansOfFivePhases(nd_steer_df, splitPoints)
    session_steer_df = session_file[, c(1,2,6)]
    session_steer_df = session_steer_df[complete.cases(session_steer_df),]
    session_steer_means = getMeansOfFivePhases(session_steer_df, splitPoints)
    
    nd_lane_df = nd_file[, c(1,2,8)]
    nd_lane_df = nd_lane_df[complete.cases(nd_lane_df),]
    nd_lane_means = getMeansOfFivePhases(nd_lane_df, splitPoints)
    session_lane_df = session_file[, c(1,2,8)]
    session_lane_df = session_lane_df[complete.cases(session_lane_df),]
    session_lane_means = getMeansOfFivePhases(session_lane_df, splitPoints)
    
    # combine the means from ND and session together with subject ID into one vector
    # append the vector as a row into the means dataframe
    speed_means[nrow(speed_means)+1,] = c(subjectID, nd_speed_means, session_speed_means)
    acc_means[nrow(acc_means)+1,] = c(subjectID, nd_acc_means, session_acc_means)
    brake_means[nrow(brake_means)+1,] = c(subjectID, nd_brake_means, session_brake_means)
    steer_means[nrow(steer_means)+1,] = c(subjectID, nd_steer_means, session_steer_means)
    lane_means[nrow(lane_means)+1,] = c(subjectID, nd_lane_means, session_lane_means)
  }
  print("final subject counts")
  print(counts)
  
  # set data type for means columns to be numeric
  speed_means[,2:11]=sapply(speed_means[,2:11], as.numeric)
  acc_means[,2:11]=sapply(acc_means[,2:11], as.numeric)
  brake_means[,2:11]=sapply(brake_means[,2:11], as.numeric)
  steer_means[,2:11]=sapply(steer_means[,2:11], as.numeric)
  lane_means[,2:11]=sapply(lane_means[,2:11], as.numeric)
  
  resMeansFromAllSubjects = list(speed_means, acc_means, brake_means, steer_means, lane_means)
  names(resMeansFromAllSubjects) = c("speed", "acceleration", "brake", "steer", "lane")
  return(resMeansFromAllSubjects)
}