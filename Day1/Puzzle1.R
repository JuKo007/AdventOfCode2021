#### Setup ####

# setting working directory
setwd("C:/Users/julia/Desktop/Drive/AdventofCode2021/Day1")

# importing input
data <- read.table("input1.txt", sep="\n")

# renaming column
colnames(data) <- "depth"



#### function to determine if a given value is larger than the one before ####

# creating empty vector for saving results
result <- rep(NA,dim(data)[1])

# creating loop (We start at 2 because the first value has no previous measurement)
for (i in 2:length(result)) {
  
  # checking decrease
  if (data$depth[i-1] > data$depth[i]) {result[i] <- "decreased"}
  
  # checking increase
  if (data$depth[i-1] < data$depth[i]) {result[i] <- "increased"}
  
  # equality
  if (data$depth[i-1] == data$depth[i]) {result[i] <- "no change"}
  
  print(result[i])
  
}

# checking resulks
table(result)
