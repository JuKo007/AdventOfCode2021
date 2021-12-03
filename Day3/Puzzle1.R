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

# checking distributions of bits per column
dist <- sapply(data,table)

# the gamma rate is the most common bit from each column
gamma_rate <- rep(NA,12)
for (i in 1:12) {gamma_rate[i] <-names(dist[,i] == max(dist[,i]))[which((dist[,i] == max(dist[,i])) == TRUE)]}
gamma_rate <- paste(gamma_rate,collapse="")

# the epsilon rate is the least common bit from each column
epsilon_rate <- rep(NA,12)
for (i in 1:12) {epsilon_rate[i] <-names(dist[,i] == min(dist[,i]))[which((dist[,i] == min(dist[,i])) == TRUE)]}
epsilon_rate <- paste(epsilon_rate,collapse="")

# converting from binary to decimals
gamma_rate <- strtoi(gamma_rate, base = 2)
epsilon_rate <- strtoi(epsilon_rate, base = 2)

# compute power consumption
pwr_cons <- gamma_rate*epsilon_rate
pwr_cons
