read.table()

eye1 <- read.csv(file = "ID_001_1.csv",
           header = FALSE,
           sep = ""
           )
colnames(eye1) <- c("SAMPLE","X.DEGREE","Y.DEGREE","VALIDITY","X.STIMULUS","Y.STIMULUS")
eye1 <- eye1[-1,]
eye2 <- eye1[c(-5,-6,-7,-8,-9,-10)]


eye.txt <- read.table("ID_001_1.txt",
                 header = TRUE,
                 fill = TRUE
                 # sep = ""
)
eye.txt.2 <- eye.txt[c(-5,-6,-7,-8,-9,-10)]
colnames(eye.txt.2) <- c("SAMPLE","X.DEGREE","Y.DEGREE","VALIDITY")

# Omit the Validity avlue that is 1.