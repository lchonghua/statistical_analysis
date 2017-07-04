plot_function <- function(driveType){
  # get pathes to all the files of interest
  subject <- list.files(full.names = TRUE)
  subfolders <- list.files(subject, full.names = TRUE, pattern = driveType)
  files <- list.files(subfolders, full.names = TRUE,  pattern = "res")
  print(paste(driveType, "Subject Counts:", length(files), sep = " "))
  
  # prepare to update index table
  indexTable<-read.xlsx2("index_table_result.xlsx",sheetIndex = 1,header = TRUE,stringsAsFactors=FALSE)
  
  # plot data of interest from each file into the plot
  speed <- ggplot()
  accel <- ggplot()
  brake <- ggplot()
  steer <- ggplot()
  lane <- ggplot()
  
  for(i in files){
    # i is the path to the file: such as "./T001/2 PD/T001-002.peda"
    subjectID <- substr(i,3,6) #e.g. subjectID = "T001"
    # use subjectID, drive type (e.g. "PD") and file type (e.g. "peda") to locate the cell in the index table
    rowNo <- which(indexTable$Subject==subjectID & indexTable$Session==driveType)
    colNo <- which(colnames(indexTable)=="res")
    # mark the index as 1 b/c the file is present
    indexTable[rowNo,colNo] <- 1
    
    df <- read.xlsx2(i, sheetIndex = 1,startRow = 9, colIndex = 2:8,  colClasses = c(rep("numeric",7)), header = TRUE)
    colName <- colnames(df)
    
    # if there is any modification in the data, mark it as -1
    if(min(df[2], na.rm = TRUE)<0.1 | min(df[3], na.rm = TRUE)<0 | max(df[4], na.rm = TRUE)>300)
      indexTable[rowNo,colNo] <- -1
    
    #2nd column in df is speed
    df[8]<-df[2]
    df[8][df[8] < -0.1] <- NA
    df[8][df[8] > -0.1 & df[8] <0.1]<-0
    colnames(df)[8]="Speed_NR"
    
    #3rd column in df is acceleration
    df[9]<-df[3]
    df[9][df[9] < 0]<-NA
    colnames(df)[9]="Acceleration_NR"
    
    #4th column in df is braking
    df[10]<-df[4]
    df[10][df[10] > 300]<-300
    colnames(df)[10]="Braking_NR"
    
    #7th column in df is lane positio
    df[11]<-df[7]-df[6]
    colnames(df)[11]="lane_position_NR"
    
    x_axis_max = max(df[1])
    speed <- speed + geom_line(data = df, aes_string(x = "Time", y = colName[2]))
    accel <- accel + geom_line(data = df, aes_string(x = "Time", y = colName[3]))
    brake <- brake + geom_line(data = df, aes_string(x = "Time", y = colName[4]))
    steer <- steer + geom_line(data = df, aes_string(x = "Time", y = colName[5]))
    lane <- lane + geom_line(data = df, aes_string(x = "Time", y = colName[7]))
    
    write.xlsx2(x=df,file=paste(subjectID,"_", driveType,".xls", sep = ""),row.names = FALSE,col.names = TRUE )
    gc()
  }
  
  # save index table
  write.xlsx2(x=indexTable,file="index_table_result.xlsx",row.names = FALSE)
  
  # return the figure for the current drive type
  speed <- speed + labs(x=NULL, y="Speed[km/h]") + theme(text = element_text(size=8))
  accel <- accel + labs(x=NULL, y=expression("Accel[" * degree * "]")) + theme(text = element_text(size=8))
  brake <- brake + labs(x=NULL, y="Brake[N]") + theme(text = element_text(size=8))
  steer <- steer + labs(x=NULL, y="Steer[rad]") + theme(text = element_text(size=8))
  lane <- lane + labs(x=NULL, y="Lane Pos.[m]") + theme(text = element_text(size=8))
  
  pList<-list(speed, accel, brake, steer, lane)
  return(pList)
}
