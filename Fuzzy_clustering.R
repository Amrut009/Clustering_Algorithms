mydata <- iris  # Loading iris dataframe into mydata
mydata$Species <- NULL # Remove the column representing classes
mydata <- as.matrix(mydata)

n <- ncol(mydata)  # Dimension for clustering
p <- nrow(mydata)  # Number of points for clustering

# Pre-defined number of clusters
k <- readline(prompt="Enter the number of clusters: ")
k <- as.integer(k)

# Randomly select cluster centres
xcentre <- sample(1:p,k)

# Store the cluster centres in a new array
centres <- matrix(0,nrow=k,ncol=n)
for (i in 1:k)
{
  centres[i,] <- mydata[xcentre[i],]
}

# Membership data matrix
membershipdata <- matrix(0,nrow=p,ncol=k) # Membership as in the current iteration
previous_membershipdata <- matrix(0,nrow=p,ncol=k) # Membership as in the previous iteration

# Fuzzy Clustering constants
m <- 2 # Fuzziness index
threshold <- 1e-2 # Iteration Threshold
iteration <- 1 # Counting number of iterations

# Infinite loop. Break only when termination condition is satisfied
while(TRUE)
{
  
  # Store membership obtained in the previous step in the variable previous_membershipdata
  previous_membershipdata <- membershipdata
  
  # Iterating over all cluster centres
  for (j in 1:k)
  {
    # Iterating over all data-point for a particular cluster centre
    for (i in 1:p)
    {
      # Checking which data point is equal to the initial random cluster centre
      if (all(mydata[i,] == centres[j,]))
      {
        for (z in 1:k)
        {
          # assigning membership as 1 for the exact match
          if (z == j)
            membershipdata[i,z] <- 1
          # assigning membership as 0 for all remaning clusters
          else
            membershipdata[i,z] <- 0
        }
      }
      
      # Update the membership matrix based on the Fuzzy formulation
      else
      {
        membership_sum <- 0
        for (z in 1:k)
        {
          # Summation over all cluster centres using Euclidean distance since m = 2
          membership_sum <- membership_sum + ((sqrt(sum(mydata[i,]-centres[j,])^2))/(sqrt(sum(mydata[i,]-centres[z,])^2)))^(2/(m-1))
        }
        membershipdata[i,j] <- 1/membership_sum     
      }
      # membershipdata[i,j] <- round(membershipdata[i,j],digits=6)
    } 
  }

  # Termination condition comparing two consecutive membership matrices
  if (max(abs(membershipdata - previous_membershipdata)) < threshold && iteration > 1)
    break

  # Updating my cluster centres based on new membership matrix
  for (j in 1:k)
  {
    membership_total <- 0 # Summation for the membership matrix element (Denominator)
    centres_total <- 0 # Summation for theproduct of membership matrix and data-point element (Numerator)
    for (i in 1:p)
    {
      membership_total <- membership_total + membershipdata[i,j]^m  # Summation for Denominator
      centres_total <- centres_total + (membershipdata[i,j]^m)*mydata[i,] # Summation for Numerator
    }
    centres[j,] <- centres_total/membership_total
    # centres[j,] <- round(centres[j,],digits=6)
  }
  
  # Updating the number of iterations
  iteration = iteration + 1
}