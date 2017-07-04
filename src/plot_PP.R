library(xlsx)
library(ggplot2)
library(grid)
library(gridExtra)
setwd("C:/study/Stat")

plot_PP <- function(driveType){
  # get pathes to all the files of interest
  subject <- list.files(full.names = TRUE)
  subfolders <- list.files(subject, full.names = TRUE, pattern = driveType)
  files <- list.files(subfolders, full.names = TRUE,  pattern = "pp")
  print(paste(driveType, "Subject Counts:", length(files), sep = " "))
  
  # prepare to update index table
  indexTable<-read.xlsx2("index_table_result.xlsx",sheetIndex = 1,header = TRUE, stringsAsFactors=FALSE)
  
  # plot data of interest from each file
  figure <- ggplot()
  for(i in files){
    # Update index table:
    # i is the path to the file: such as "./T001/2 PD/T001-002.peda"
    subjectID <- substr(i,3,6) #e.g. subjectID = "T001"
    
    # use subjectID, drive type (e.g. "PD") and file type (e.g. "peda") to locate the cell in the index table
    rowNo <- which(indexTable$Subject==subjectID & indexTable$Session==driveType)
    colNo <- which(colnames(indexTable)=="pp")
    
    # mark the index as 1 b/c the file is present
    indexTable[rowNo,colNo] <- 1
    
    # plot data
    df <- read.xlsx2(i, sheetIndex = 1,startRow = 9, colIndex = 1:4,  colClasses = c(rep("numeric",4)))
    colName <- colnames(df)[4]
    figure <- figure+ geom_line(data = df, aes_string(x = "Time", y = colName))
    gc()
  }
  
  # save index table
  write.xlsx2(x=indexTable,file="index_table_result.xlsx",row.names = FALSE)
  
  # return the figure for the current drive type
  figure <- figure +labs(x=NULL,y=expression(~degree*C^{2}))
  tempFig<-list(figure)
  
  return(tempFig)
}

p1<-plot_PP("BL")[[1]]
p2<-plot_PP("PD")[[1]]
p3<-plot_PP("RD")[[1]]
p4<-plot_PP("ND")[[1]]
p5<-plot_PP("ED")[[1]]
p6<-plot_PP("CD")[[1]]
p7<-plot_PP("MD")[[1]]
p8<-plot_PP("FD")[[1]]

plist<-list(p1,textGrob("BL"),p2,textGrob("PD"),p3,textGrob("RD"),
            p4,textGrob("ND"),p6,textGrob("CD"),p5,textGrob("ED"),
            p7,textGrob("MD"), p8, textGrob("FD"))
ggsave("PP.png",grid.arrange(grobs=plist, ncol=2,nrow=8,widths=c(10,1),top = "Perinasal Perspiration Signal Valid Dataset", bottom = "Time [s]"))
