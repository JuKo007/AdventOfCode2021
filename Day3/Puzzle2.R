#### Setup ####

# setting working directory
setwd("C:/Users/julia/Desktop/Drive/SideProjects/AdventofCode2021/Day3")

# importing input (we need the colClasses argument because read.table drops zeros otherwise)
data <- read.table("input1.txt", sep="\n",colClasses=c('character'))

# checking
sapply(data,nchar)
table(sapply(data,nchar))

# renaming column
colnames(data) <- "binary"

# splitting the data into single digits
splitlist <- strsplit(as.character(data$binary),"")
splitmatrix <- t(matrix(unlist(splitlist), ncol = 1000, nrow = 12))
data <- as.data.frame(splitmatrix)
colnames(data) <- c("bit1","bit2","bit3","bit4","bit5","bit6","bit7","bit8","bit9","bit10","bit11","bit12")

# copying the data so we can edit in place in the loop
data_copy <- data

# cycling through all bit positions and only keeping those numbers with a majority bit in the given position
for (i in 1:ncol(data_copy)) {
  
  #computing majority for column i
  dist <- table(data_copy[,i])
  
  # check for equal amount (when it's equal, we chose 1) and check which number is in majority (i_max)
  if (dist[1] == dist[2]) {i_max <- 1} else {i_max <- as.numeric(names(dist)[dist==max(dist)])}
  
  # only keeping rows with majority number in position i
  data_copy <- data_copy[data_copy[,i] == i_max,]
  
  # check how many rows we are left with, stop at one row left
  if (nrow(data_copy) == 1) {
    
    oxygen_rate <- strtoi(paste(data_copy[1,],collapse=""), base = 2)
    break()
    
  }
  
  # sophistiacated debugger
  print(table(data_copy[i]))
  
}

#### repeating the same procedure for co2_scrubber_rate

# copying the data so we can edit in place in the loop
data_copy2 <- data

# cycling through all bit positions and only keeping those numbers with a majority bit in the given position
for (i in 1:ncol(data_copy2)) {
  
  #computing majority for column i
  dist <- table(data_copy2[,i])
  
  # check for equal amount (when it's equal, we chose 0) and check which number is in minority (i_min)
  if (dist[1] == dist[2]) {i_min <- 0} else {i_min <- as.numeric(names(dist)[dist==min(dist)])}
  
  # only keeping rows with minority number in position i
  data_copy2 <- data_copy2[data_copy2[,i] == i_min,]
  
  # check how many rows we are left with, stop at one row left
  if (nrow(data_copy2) == 1) {
    
    co2_scrubber_rate <- strtoi(paste(data_copy2[1,],collapse=""), base = 2)
    break()
    
  }
  
  # sophistiacated debugger
  print(table(data_copy2[i]))
  
}


# multiply co_2_scrubber_rate with oxygen_rating
co2_scrubber_rate * oxygen_rate
