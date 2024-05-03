# Build Hierarchical Document Database
# This code is for Course "Database Management Systems"(CS5200)
# Term: Spring 2023 Semester
# Author: Xuebao Zhao

# Change all directories here
rootDir <- "docDB"
path <- getwd() # The path where you want to create folder "docDB"
source <- "source" # The path where the images are stored

configDB <- function(root, path) {
  # Sets up all folders and database related structure
  #
  # Args:
  #   root: The folder name you want to create
  #   path: The directory you want the folder to be created
  # Returns: None
  
  dbPath <- paste(path, root, sep = "/")
  if (!dir.exists(dbPath)) { # Only create the folder when it does not exist
    dir.create(dbPath)
  }
}

genObjPath <- function(root, tag) {
  # Generates the correctly path to a tag folder
  #
  # Args:
  #   root: The directory you want the tag to be added
  #   tag: The tag string you want to add in the path
  # Returns: Path of tag folder
  
  objPath <- paste(root, gsub('#', '', tag), sep = "/")
  return (objPath)
}

getTags <- function(fileName) {
  # Extracts the tags followed by "#" in the fileName
  #
  # Args:
  #   fileName: File name like "CampusAtNight.jpg #Northeastern #ISEC"
  #             or "CampusAtNight #Northeastern #ISEC.jpg"
  # Returns: A vector of tags in the file name
  
  if(!grepl("#", fileName, fixed = TRUE))
    stop(paste(fileName,"doesn't have tags"))
  tagVec <- vector()
  tagList <- strsplit(fileName, ' ') # Separate file names with space to get tags
  #tagList <- sub(".*#", "", tagList)
  last <- length(tagList[[1]])  # Get the number of tags
  tagList[[1]][last] <- strsplit(tagList[[1]][last], '[.]')[[1]][1] # If the extension is at the end, remove it
  tagVec <- unlist(tagList[[1]][2:last])
  tagVec <- sub(".*#", "", tagVec)
  tagVec <- paste("#", tagVec, sep = '')
  return (tagVec)
}

getFileName <- function(fileName) {
  # Extracts the file name without tags
  #
  # Args:
  #   fileName: File name like "CampusAtNight.jpg #Northeastern #ISEC"
  #             or "CampusAtNight #Northeastern #ISEC.jpg""
  # Returns: File name after removing tags
  
  tagVec = getTags(fileName) # Get tags
  for (i in 1:length(tagVec)) {
    fileName <- sub(pattern = tagVec[i], replacement = "", fileName) # Remove the tags from filename
  }
  fileName <- gsub(pattern = " ", replacement = "", fileName) # Remove spaces
  fileName <- gsub(pattern = "#", replacement = "", fileName) # Remove "#"
  if (substring(fileName, 1, 1) == ".")
    stop(paste(fileName,"has a blank file name!"))
  return (fileName)
}

storeObjs <- function(folder, root, verbose) {
  # Copies all files in the folder underneath the root folder.
  # Creates folders for the tags as needed.
  #
  # Args:
  #   folder: The path where the original files are stored
  #   root: The folder/path where the tag folder should be
  #   verbose: If it is true, prints a message for every file that is copied
  # Returns: None
  
  root <- paste(path, root, sep = "/")
  files <- list.files(path = folder) # Get all the subfolders
  for (i in 1:length(files)) {
    oldFileName <- files[i]
    newFileName <- getFileName(files[i])
    tag <- getTags(oldFileName) # Get tags
    # tagOutput removes the "#" at the beginning of each tag. It is used in the output message
    tagOutput <- sub(pattern = "#", replacement = "", tag)
    tagOutput <- paste(tagOutput, collapse = ", ")
    for (j in 1:length(tag)) {
      oldPath <- paste(folder, oldFileName, sep = "/")
      newPath <- paste(genObjPath(root, tag[j]), newFileName, sep = "/")
      
      # Create the tag folder and copy file
      folderPath <- paste(root, substring(tag[j], 2, nchar(tag[j])), sep = "/")
      if (!dir.exists(folderPath)) {  # Only create the folder when it does not exist
        dir.create(folderPath)
      }
      file.copy(oldPath, newPath)
    }
    if (verbose == TRUE) {
      print(paste("Copying", newFileName, "to", tagOutput))
    }
  }
}

clearDB <- function(root) {
  # Removes all folders and files in the folder specified by root but not the folder for root itself
  #
  # Args:
  #   root: The folder/path where you do the clear
  # Returns: None
  
  unlink(file.path(getwd(), root, "*"), recursive=TRUE)
}

main <- function() {

  print("====================================")
  print("|       1. Test the workflow       |")
  print("|       2. Test each function      |")
  print("|       3. Clear the database      |")
  print("|       4. Quit                    |")
  print("====================================")
  inPut <- readline("Please enter a number: ")
  
  while (inPut != "4") {
    if (inPut == "1") {
      configDB(rootDir, path)
      print("Sets up root folder successful!")
      storeObjs(source, rootDir, TRUE)
    }
    else if (inPut == "2") {
      # Test genObjPath
      print("Test the function 'genObjPath'")
      print("Test case1:")
      print("    If root is 'docDB' and tag is '#Northeastern'")
      print("    The expected result should be 'docDB/Northeastern'")
      print(paste("    The result of a function run is:", genObjPath(rootDir, "#Northeastern")))
      print("Test case2:")
      print("    If root is 'docDB' and tag is '###Northeastern'")
      print("    The expected result should be 'docDB/Northeastern'")
      print(paste("    The result of a function run is:", genObjPath(rootDir, "###Northeastern")))
      print("----------------------------")
      
      # Test getTags
      print("Test the function 'getTags'")
      print("Test case1:")
      print("    If fileName is 'CampusAtNight.jpg #Northeastern #ISEC'")
      fileName <- "CampusAtNight.jpg #Northeastern #ISEC"
      print("    The expected result should be '#ISEC, #Northeastern'")
      print("    The result of a function run is")
      print(getTags(fileName))
      
      print("Test case2:")
      print("    If fileName is 'CampusAtNight #Northeastern #ISEC.jpg'")
      fileName <- "CampusAtNight #Northeastern #ISEC.jpg"
      print("    The expected result should be '#ISEC, #Northeastern'")
      print("    The result of a function run is")
      print(getTags(fileName))
      
      print("Test case3:")
      print("    If fileName is ' #Northeastern #ISEC.jpg'")
      fileName <- " #Northeastern #ISEC.jpg"
      print("    The expected result should be '#ISEC, #Northeastern'")
      print("    The result of a function run is")
      print(getTags(fileName))
      
      print("Test case4:")
      print("    If fileName is 'CampusAtNight ####Northeastern #ISEC.jpg'")
      fileName <- "CampusAtNight ####Northeastern #ISEC.jpg"
      print("    The expected result should be '#ISEC, #Northeastern'")
      print("    The result of a function run is: ")
      print(getTags(fileName))
      print("----------------------------")
      
      # Test getFileName
      print("Test the function 'getFileName'")
      print("Test case1:")
      print("    If fileName is 'CampusAtNight.jpg #Northeastern #ISEC'")
      fileName <- "CampusAtNight.jpg #Northeastern #ISEC"
      print("    The expected result should be 'CampusAtNight.jpg'")
      print("    The result of a function run is")
      print(getFileName(fileName))
      
      print("Test case2:")
      print("    If fileName is 'CampusAtNight #Northeastern #ISEC.jpg'")
      fileName <- "CampusAtNight #Northeastern #ISEC.jpg"
      print("    The expected result should be 'CampusAtNight.jpg'")
      print("    The result of a function run is")
      print(getFileName(fileName))
      
      print("Test case3:")
      print("    If fileName is 'CampusAtNight ####Northeastern #ISEC.jpg'")
      fileName <- "CampusAtNight ####Northeastern #ISEC.jpg"
      print("    The expected result should be 'CampusAtNight.jpg'")
      print("    The result of a function run is: ")
      print(getFileName(fileName))
      
      print("Test case4:")
      print("    If fileName is ' #Northeastern #ISEC.jpg'")
      print("    The code will stop")
    }
    else if (inPut == "3") {
      clearDB(rootDir)
      print("Clear the database successful!")
    }
    print("====================================")
    print("|       1. Test the workflow       |")
    print("|       2. Test each function      |")
    print("|       3. Clear the database      |")
    print("|       4. Quit                    |")
    print("====================================")
    inPut <- readline("Please enter a number: ")
  }
  quit()
}

main()

