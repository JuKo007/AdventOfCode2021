#### Setup ####

# setting working directory
setwd("C:/Users/julia/Desktop/Drive/SideProjects/AdventofCode2021/Day4")

# importing input (we need the colClasses argument because read.table drops zeros otherwise)
data <- read.table("input1.txt", sep="\n",colClasses=c('character'))

# extracting first line
bingo_numbers <- data[1,]
bingo_numbers <- unlist(strsplit(bingo_numbers,","))
bingo_numbers <- as.numeric(bingo_numbers)
data <- data[-1,]

# creating empty list for storing boards
board_list <- as.list(rep(NA,100))

# pasting all data together
data <- paste(data, collapse=" ")

# splitting into individual digits
data <- strsplit(data," ")

# deleting empty elements created by double spaces in front of single digit numbers
data <- sapply(data,function(y){y=y[y != ""]})

# transforming character string to numeric
data <- as.numeric(data)

# creating cutoff sequences
starts <- seq(1,2500,25)
ends <- seq(25,2500,25)
cutoff <- cbind.data.frame(starts,ends)

# looping through the data to create matrices
for (i in 1:length(board_list)){
  
  # assigning numbers to list element
  board_list[[i]] <- data[cutoff$starts[i]:cutoff$ends[i]]
  
  # transforming number list to matrix
  board_list[[i]] <- t(matrix(board_list[[i]],nrow=5,ncol=5))
  
  # sophisticated debugger
  print(board_list[[i]])

}




#### bingo loop ####
for (i in bingo_numbers) {
  
  # looping through all matrices
  for (j in 1:length(board_list)){
    
    # replacing matching value with a "HIT" marker (values get auto-transformed to characters)
    board_list[[j]][which(board_list[[j]] == i)] <- "HIT"
    
    
    
    ### counting the number of "hits" per row and column in each matrix
    
    # looping through rows
    for (m in 1:5) {
      
      # computing number of hits for each row in the matrix
      hit_number <- length(grep("HIT",as.character(board_list[j][[1]][m,])))
      
      # breaking loop if we count 5 hits in a row and printing winning board
      # we need to catch the case of hit_number == integer(0) cause we can't use it in a comparison
      if (length(hit_number) == 0) {} else{
        
        if (hit_number == 5) { # condition has length > 1
          
          # print winning board
          print(board_list[j][[1]])
          return() # this is bad style but enables us to break from a nested loop
          
        }
        
        
      }
      
    }
    
    # looping through columns
    for (n in 1:5) {
      
      # computing number of hits for each row in the matrix
      hit_number <- length(grep("HIT",as.character(board_list[j][[1]][,n])))
      
      # breaking loop if we count 5 hits in a column and printing winning board
      # we need to catch the case of hit_number == integer(0) cause we can't use it in a comparison
      if (length(hit_number) == 0) {} else{
        
        if (hit_number == 5) {
          
          # print winning board
          print(board_list[j][[1]])
          return() # this is bad style but enables us to break from a nested loop
          
        }
        
        
      }
      
    }
    
    # sophisticated debugger
    print(board_list[[j]])
    print(j)
    
  }

}

# look at winning board
board_list[[j]]
j
i



##### computing solution ####
sum(as.numeric((board_list[[j]])),na.rm=TRUE) * i

