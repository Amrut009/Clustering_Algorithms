mydata <- iris  # Loading iris dataframe into mydata
mydata$Species <- NULL # Remove the column representing classes
mydata <- as.matrix(mydata)

n <- ncol(mydata)  # Dimension for clustering
m <- nrow(mydata)  # Number of points for clustering

# Pre-defined number of clusters
k <- readline(prompt="Enter the number of clusters: ")
k <- as.integer(k)

# Randomly select cluster centres
xcentre <- sample(1:m,k)

# Store the cluster centres in a new array
centre <- matrix(0,nrow=k,ncol=n)
for (i in 1:k)
  centre[i,] <- mydata[xcentre[i],]

# Membership function for each data point
membershipdata <- matrix(0,m,1)

# Convergence Step
cluster <- FALSE

while(!cluster)
{
  
  # Calculating the Euclidean Distance
  distancedata <- array(dim = c(m,k))
  for (i in 1:m)
    for (j in 1:k)
      distancedata[i,j] <- sqrt(sum((mydata[i,]-centre[j,])^2))

  # Updating Membership function for each data point
  membershipdata_new <- array(dim = c(m,1))
  for (i in 1:m)
    for (j in 1:k)
      if (min(distancedata[i,]) == distancedata[i,j])
        membershipdata_new[i] <- j
  
  if (all(membershipdata == membershipdata_new))
  {
    # k-Means Clustering converges at this step
    cluster = TRUE
  }
  
  else
  {
    membershipdata <- membershipdata_new
    
    # Calculate the new centre
    
    for (j in 1:k)
    {
      centre_sum <- matrix(c(0,0,0,0),nrow=1, ncol=n)
      count <- 0
      for (i in 1:m)
      {
        if(membershipdata[i] == j)
        {
          centre_sum[1,] <- centre_sum[1,] + mydata[i,]
          count <- count+1
        }
      }
      centre[j,] <- centre_sum[1,]/count
      count <- 0
    }  
  }
}
  

  



