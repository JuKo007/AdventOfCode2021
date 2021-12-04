#### Setup ####

# setting working directory
setwd("C:/Users/julia/Desktop/Drive/AdventofCode2021/Day2")

# importing input
data <- read.table("input1.txt", sep="\n")

# renaming column
colnames(data) <- "direction"

# splitting the string and extracting elements
splitlist <- strsplit(data$direction, " ")
direction <- sapply(splitlist, `[[`,1)
distance <- sapply(splitlist, `[[`,2)

# recombining the two
data <- cbind.data.frame(direction,distance = as.numeric(distance))

# checking possible directions
table(data$direction)

# initiating empty vectors for storing data (We use 0 instead of NA cause we need to start at 0 anyways)
depth <- 0
horizontal_pos <- 0
aim <- 0


# looping the dataframe with instructions based on direction
for (i in 1:dim(data)[1]){
  
  if (data$direction[i] == "down") {
    
    aim <- aim + data$distance[i]
    
  }
  
  if (data$direction[i] == "up") {
    
    aim <- aim - data$distance[i]
    
  }
  
  if (data$direction[i] == "forward") {
    
    horizontal_pos <- horizontal_pos + data$distance[i]
    depth <- depth + (aim*data$distance[i])
    
  }
  
  # sophisticated debugger
  print(paste(data$direction[i],aim,horizontal_pos,depth))
  
}

# multiplying final depth by hoprizontal_pos
depth*horizontal_pos