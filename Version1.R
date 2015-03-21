# read.table()
# 
# eye1 <- read.csv(file = "ID_001_1.csv",
#            header = FALSE,
#            sep = ""
#            )
# colnames(eye1) <- c("SAMPLE","X.DEGREE","Y.DEGREE","VALIDITY","X.STIMULUS","Y.STIMULUS")
# eye1 <- eye1[-1,]
# eye2 <- eye1[c(-5,-6,-7,-8,-9,-10)]
# 

# This is used to take the text file and work as we need it 
# to be.

eye.txt <- read.table("ID_001_1.txt",
                 header = TRUE,
                 fill = TRUE
                 # sep = ""
)
eye.txt <- eye.txt[c(-5,-6,-7,-8,-9,-10)]
colnames(eye.txt) <- c("SAMPLE","X.DEGREE","Y.DEGREE","VALIDITY")

directory.dummy <- "/Users/mavezsinghdabas/StatisticsForBigData-/eyemovement_data/BioEye2015_DevSets/RAN_30min_dv"

completeFile <- function(directory,id = 1:30){
  files_list <- list.files(directory,full.names = TRUE)
  dat <- data.frame()
  for( i in id){
    #dat <- read.table(files_list[i],header = TRUE,fill = TRUE)
    #dat <- dat[c(-5,-6,-7,-8,-9,-10)]
    #colnames(dat) <- c("SAMPLE","X.DEGREE","Y.DEGREE","VALIDITY")
    #dat <- rbind(dat) #,read.table(files_list[i]))
    dat <- rbind(dat,read.table(files_list[i],header = TRUE,fill = TRUE))
    #dat <- dat[c(-5,-6,-7,-8,-9,-10)]
    #colnames(dat) <- c("SAMPLE","X.DEGREE","Y.DEGREE","VALIDITY")
    
  }
  
  dat<- dat[c(-5,-6,-7,-8,-9,-10)]
  colnames(dat) <- c("SAMPLE","X.DEGREE","Y.DEGREE","VALIDITY")
  dat
}

data1 <- completeFile(directory.dummy,id = 1:5)




































