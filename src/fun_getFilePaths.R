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