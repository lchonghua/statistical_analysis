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