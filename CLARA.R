############################### Implementation of CLARA #################################

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

# CLARA parameter for formation of samples
sample_points <- 40 + 2*k
originalcost <- Inf
centre <- array(dim = c(n,k)) # original cluster centres array
newcentre <- array(dim = c(n,k)) # new cluster centres array

# CLARA iterates 5 times
for (sample_number in 1:5)
{
  newcentre <- centre
  
  # Obtain a random sample set from the given dataset
  xsample <- sample(1:m,sample_points)
  
  # Store the cluster centres in a new array
  datasample <- array(dim = c(n,sample_points))
  
  for (i in 1:sample_points)
    datasample[,i] = data[,xsample[i]]

  ############# PAM on the subset of the original data ############
  
  # Randomly select cluster centres for the subdata
  xcentre <- sample(1:sample_points,k)
  
  # Store the cluster centres in a new array

  for (i in 1:k)
    centre[,i] = datasample[,xcentre[i]]
  
  # Calculating the Hamming Distance for the subdata
  hamming_subdata <- array(dim = c(k,sample_points))
  for (i in 1:sample_points)
    for (j in 1:k)
      hamming_subdata[j,i] <- sum(abs(centre[,j] - datasample[,i]))  
  
  # Membership function for each data point in subdata
  membership_subdata <- array(dim = c(1,sample_points))
  for (i in 1:sample_points)
    for (j in 1:k)
      if (min(hamming_subdata[,i]) == hamming_subdata[j,i])
        membership_subdata[i] <- j
  
  # Cost of Clustering for the given subdata
  subdata_cost <- 0
  for (i in 1:sample_points)
    subdata_cost <- subdata_cost + min(hamming_subdata[,i])
  
  # Swapping with non-mediod to check if the cost decreases
  for (i in 1:sample_points)  # Swapping for all non-mediods
    for (j in 1:k) # Swapping a new non-mediods with all the existing mediods
      newcentre <- centre
      newcentre[,j] <- datasample[,i]
      for (j in 1:sample_points)
        for (z in 1:k)
          hamming_subdata[z,j] <- sum(abs(newcentre[,z] - datasample[,j]))
      # Computing the new cost
      subdata_newcost <- 0
      for (j in 1:sample_points)
        subdata_newcost <- subdata_newcost + min(hamming_subdata[,j])
      if (subdata_newcost < subdata_cost)
        centre <- newcentre
        subdata_cost <- subdata_newcost
        # new membeship based on the cluster centres
        for (j in 1:sample_points)
          for (z in 1:k)
            if (min(hamming_subdata[,j]) == hamming_subdata[z,j])
              membership_subdata[j] <- z

  ######## PAM on the subset of the original data ends here #########
  
  # Calculating the Hamming Distance for the entire data
  hamming_data <- array(dim = c(k,m))
  for (i in 1:m)
    for (j in 1:k)
      hamming_data[j,i] <- sum(abs(centre[,j] - data[,i]))

  # Membership function for entire data
  membership_data <- array(dim = c(1,m))
  for (i in 1:m)
    for (j in 1:k)
      if (min(hamming_data[,i]) == hamming_data[j,i])
        membership_data[i] <- j

  # Cost of Clustering for the entire data
  cost <- 0
  for (i in 1:m)
    cost <- cost + min(hamming_data[,i])

  if (cost < originalcost)
    originalcost <- cost
    centre <- newcentre
}

    



