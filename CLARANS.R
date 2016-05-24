##### Implementation of CLARANS #####

# install.packages("R.basic") # Install package if not avaiable already
library(R.basic)  ## Importing the necessary library
# install.packages("gtools") # Install package if not avaiable already
library(gtools)   ## Importing the necessary library

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

nodes_number <- choose(m,k)

# Getting the node data points 
node_data <- array(dim = c(nodes_number,k))
node_data <- combinations(m,k)
node_data <- t(node_data)

### CLARANS Initialization

# User input - maximum number of neighbours
maxneighbour <- readline(prompt="Enter the maximum number of neighbours: ")
maxneighbour <- as.integer(maxneighbour)

# User input - numlocal - How many times algorithm is iterating for
numlocal <- readline(prompt="Enter the number of iterations to reach global minimum: ")
numlocal <- as.integer(numlocal)
mincost <- Inf

i <- 1

while (i <= numlocal)
{
  # Consider a random node point as the current node
  xnode <- sample(1:nodes_number,1)
  current_node <- array(dim = c(n,k))
  for (alpha in (1:k))
    current_node[,alpha] <- data[,node_data[,xnode][alpha]]
    
  # Compute the clustering cost for current node
  hamming_current <- array(dim = c(k,m))
  for (i in 1:m)
    for (j in 1:k)
      hamming_current[j,i] <- sum(abs(current_node[,j] - data[,i])) 
  current_cost <- 0
  for (i in 1:m)
    current_cost <- current_cost + min(hamming_current[,i])
  
  while(1)
  {  
    # Calculating the neighbours of current node
  
    # Step-1: Get the index of current node from data
    index_node <- array(dim = c(k,1))
    for (beta in (1:k))
      for (alpha in (1:m))
        if (all(current_node[,beta] == data[,alpha]))
          index_node[beta,1] <- alpha
  
    # Step-2: Extract the value of index_node in node_data
    for(alpha in (1:nodes_number))
      if(all(index_node == node_data[,alpha]))
        index <- alpha
  
    # Step-3: Extracting the neighbours based on index from node_data
    neighbour_data <- array(dim = c(1,k*(m-k)))
    neighbour_point <- 1
    for(alpha in (1:k))
    {
      node_point <- node_data[alpha,index]
      for(beta in (1:k))
      {
        for(gamma in (1:nodes_number))
        {
          if(node_data[beta,gamma] == node_point && !all(node_data[,gamma] == node_data[,index]))
          {
            neighbour_data[1,neighbour_point] <- gamma
            neighbour_point = neighbour_point + 1
            if (neighbour_point > k*(m-k))
              neighbour_point = k*(m-k)
          }
        }
      }
    }
  
    j <- 1
    while(j <= maxneighbour)
    {
      # Consider a random neighbour of current node
      xnnode <- sample(1:neighbour_point,1)
      xnnode <- neighbour_data[1,xnnode]
      neighbour_node <- array(dim = c(n,k))
      for (alpha in (1:k))
        neighbour_node[,alpha] <- data[,node_data[,xnnode][alpha]]
    
      # Compute the clustering cost for neighbour node
      hamming_neighbour <- array(dim = c(k,m))
      for (i in 1:m)
        for (j in 1:k)
          hamming_neighbour[j,i] <- sum(abs(neighbour_node[,j] - data[,i])) 
      neighbour_cost <- 0
      for (i in 1:m)
        neighbour_cost <- neighbour_cost + min(hamming_neighbour[,i])
    
      if (neighbour_cost < current_cost)
      {
        current_node <- neighbour_node
        current_cost <- neighbour_cost
        break
      }
      else
      j = j+1
    }
    
    if (current_cost < mincost) # local Optimum
    {
      mincost <- current_cost
      # current node is the best node
      break
    }
  }
  
  i = i + 1 # Global optimum after multiple iterations
}






