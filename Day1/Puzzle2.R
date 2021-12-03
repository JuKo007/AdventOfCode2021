#### Setup ####

# setting working directory
setwd("C:/Users/julia/Desktop/Drive/AdventofCode2021/Day1")

# importing input
data <- read.table("input2.txt", sep="\n")

#test data
#data <- data.frame(c(199,200,208,210,200,207,240,269,260,263))

# renaming column
colnames(data) <- "depth"

##### Loop to compute sums for 3 value sliding windows ####

# creating empty vector for saving results
result <- rep(NA,dim(data)[1])

# we only go until 1998 because that is the last value we can compute a 3 value sliding window for
for (i in 1:(length(result)-2)) {
  
  result[i] <- sum(data$depth[i],data$depth[i+1],data$depth[i+2])
  
  # sophisticated debugger
  print(paste(i,result[i],sep="_"))

}

# we need to remove the two missing Values from the data
result <- na.omit(result)

# we can now use the loop from the previous puzzle to check the list of results

# creating empty vector for saving results
result_final <- rep(NA,length(result))

# creating loop (We start at 2 because the first value has no previous measurement)
for (i in 2:length(result)) {
  
  # checking decrease
  if (result[i-1] > result[i]) {result_final[i] <- "decreased"}
  
  # checking increase
  if (result[i-1] < result[i]) {result_final[i] <- "increased"}
  
  # equality
  if (result[i-1] == result[i]) {result_final[i] <- "no change"}
  
  # sophisticated debugger
  print(paste(i,result_final[i],sep="_"))
  
}

# getting table of results
table(result_final)
