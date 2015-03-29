
# This is used to take the text file and work as we need it 
# to be.
# 
# eye.txt <- read.table("ID_001_1.txt",
#                  header = TRUE,
#                  fill = TRUE
#                  # sep = ""
# )
# eye.txt <- eye.txt[c(-5,-6,-7,-8,-9,-10)]
# colnames(eye.txt) <- c("SAMPLE","X.DEGREE","Y.DEGREE","VALIDITY")

# This is the path of the files in the system
directory.dummy <- "/Users/mavezsinghdabas/StatisticsForBigData-/eyemovement_data/BioEye2015_DevSets/RAN_30min_dv"

# Function to get all the files in the folder and removing all the unwanted
# data from the files.

completeFile <- function(directory,id = 1:306){
  files_list <- list.files(directory,full.names = TRUE)
  dat <- data.frame()
  for( i in id){
    dat <- rbind(dat,read.table(files_list[i],header = TRUE,fill = TRUE))
  }
  dat<- dat[c(-5,-6,-7,-8,-9,-10)]
  colnames(dat) <- c("SAMPLE","X.DEGREE","Y.DEGREE","VALIDITY")
  dat
}
dataEye_1 <- completeFile(directory.dummy,id = 1:306)
copydataEye_1 <- dataEye_1

# dataVersion_1 <- completeFile(directory.dummy,id = 1:50)
# dataVersion_2 <- completeFile(directory.dummy,id = 51:100)
# dataVersion_3 <- completeFile(directory.dummy,id = 101:150)
# dataVersion_4 <- completeFile(directory.dummy,id = 151:200)
# dataVersion_5 <- completeFile(directory.dummy,id = 201:250)
# dataVersion_6 <- completeFile(directory.dummy,id = 251:300)
# dataVersion_7 <- completeFile(directory.dummy,id = 301:306)




































