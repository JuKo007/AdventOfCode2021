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

# modifying distance to reflect direction
data[direction == "up",]$distance <- data[direction == "up",]$distance*(-1)

# computing depth
sum(data[direction != "forward",]$distance)

# computing horizontal position
sum(data[direction == "forward",]$distance)

# multiplying with each other
sum(data[direction != "forward",]$distance)*sum(data[direction == "forward",]$distance)
