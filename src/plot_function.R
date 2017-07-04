plot_function <- function(driveType, filetype, ylabel, lowerBound, upperBound){
  # get pathes to all the files of interest
  subject <- list.files(full.names = TRUE)
  subfolders <- list.files(subject, full.names = TRUE, pattern = driveType)
  files <- list.files(subfolders, full.names = TRUE,  pattern = filetype)
  print(paste(filetype, driveType, "Subject Counts:", length(files), sep = " "))
  subjectCount <- length(files)
  subjectCount1 <- 0
  # prepare to update index table
  indexTable<-read.xlsx2("index_table_result.xlsx",sheetIndex = 1,header = TRUE,stringsAsFactors=FALSE)
  
  # plot data of interest from each file into the plot
  figure1 <- ggplot()
  figure2 <- ggplot()
  
  excludeCount = 0;
  
  for(i in files){
    # i is the path to the file: such as "./T001/2 PD/T001-002.peda"
    subjectID <- substr(i,3,6) #e.g. subjectID = "T001"
    # use subjectID, drive type (e.g. "PD") and file type (e.g. "peda") to locate the cell in the index table
    rowNo <- which(indexTable$Subject==subjectID & indexTable$Session==driveType)
    colNo <- which(colnames(indexTable)==filetype)
    # mark the index as 1 b/c the file is present
    indexTable[rowNo,colNo] <- 1
    
    df <- read.xlsx2(i, sheetIndex = 1,startRow = 9, colIndex = 1:3,  colClasses = c(rep("numeric",3)))
    colName <- colnames(df)[3]
    figure1 <- figure1+ geom_line(data = df, aes_string(x = "Time", y = colName))
    
    if(min(df[3])>=lowerBound & max(df[3])<=upperBound){
      figure2 <- figure2+ geom_line(data = df, aes_string(x = "Time", y = colName))
      subjectCount1 <- subjectCount1+1
    }
      else{
      # if the value is out of range, mark as -1 in the index table
      indexTable[rowNo,colNo] <- -1
      excludeCount <- excludeCount +1
    }
    
    gc()
  }
  correctedCount <- length(files) - excludeCount
  print(paste(filetype, driveType, "Corrected Subject Counts:", correctedCount, sep = " "))
  
  # return the figure for the current drive type
  figure1 <- figure1 +labs(x=NULL,y=ylabel)+theme(axis.text.y = element_text(size=8), axis.title.y = element_text(size = 8))
  legend <- grobTree(textGrob(paste("n=", subjectCount), x=0.91, y=0.91, gp=gpar(col="black")))
  figure1 <- figure1+annotation_custom(legend)
  figure2 <- figure2 +labs(x=NULL, y=NULL)+theme(axis.text.y = element_text(size=8))
  legend1 <- grobTree(textGrob(paste("n=", subjectCount1), x=0.92, y=0.92, gp=gpar(col="black")))
  figure2 <- figure2+annotation_custom(legend1)
  plist<-list(figure1, figure2)

  # save index table
  write.xlsx2(x=indexTable,file="index_table_result.xlsx", row.names = FALSE)
  
  return(plist)
}
