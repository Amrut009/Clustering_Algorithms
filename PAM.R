############################### Implementation of PAM #################################

## Working on a dataframe ##
mydata <- iris  # Loading iris dataframe into mydata
mydata$Species <- NULL # Remove the column representing classes
n <- ncol(mydata)  # Dimension for clustering
m <- nrow(mydata)  # Number of points for clustering
data <- t(mydata) # Calculating transpose to match the dimension used ahead

## Taking input from the user : Comment out if using dataframe ##
# n <- readline(prompt="Enter the dimension for clustering: ")
# n <- as.integer(n)
# m <- readline(prompt="Enter the number of points for clustering: ")
# m <- as.integer(m)

# Getting the random data points 
# data <- array(dim = c(n,m)) 
# for (i in 1:n)
#  for (j in 1:m)
#    data[i,j] <- sample(1:10,1)

# Pre-defined number of clusters
k <- readline(prompt="Enter the number of clusters: ")
k <- as.integer(k)

# Randomly select cluster centres
xcentre <- sample(1:m,k)

# Store the cluster centres in a new array
centre <- array(dim = c(n,k))
for (i in 1:k)
  centre[,i] = data[,xcentre[i]]

# Calculating the Hamming Distance
hammingdata <- array(dim = c(k,m))
for (i in 1:m)
  for (j in 1:k)
    hammingdata[j,i] <- sum(abs(centre[,j] - data[,i]))

# Membership function for each data point
membershipdata <- array(dim = c(1,m))
for (i in 1:m)
  for (j in 1:k)
    if (min(hammingdata[,i]) == hammingdata[j,i])
      membershipdata[i] <- j

# Total cost of K-mediods Clustering
cost <- 0
for (i in 1:m)
  cost <- cost + min(hammingdata[,i])

# Swapping with non-mediod to check if the cost decreases
newcentre <- array(dim = c(n,k))
for (i in 1:m)  # Swapping for all non-mediods
  for (j in 1:k) # Swapping a new non-mediods with all the existing mediods
    newcentre <- centre
    newcentre[,j] <- data[,i]
    for (j in 1:m)
      for (z in 1:k)
        hammingdata[z,j] <- sum(abs(newcentre[,z] - data[,j]))
    # Computing the new cost
    newcost <- 0
    for (j in 1:m)
      newcost <- newcost + min(hammingdata[,j])
    if (newcost < cost)
      centre <- newcentre
      cost <- newcost
      # new membeship based on the cluster centres
      for (j in 1:m)
        for (z in 1:k)
          if (min(hammingdata[,j]) == hammingdata[z,j])
            membershipdata[j] <- z



