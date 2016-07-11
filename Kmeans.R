# k-Means Clustering

# Task-1 : Verification Example Matrix 
# mydata <- matrix(c(0,1,7,8,2,0,1,7,8,2),nrow=5,ncol=2)

# Operating on iris data-set
# mydata <- iris  # Loading iris dataframe into mydata
# mydata$Species <- NULL # Remove the column representing classes
# mydata <- as.matrix(mydata)

# Operating on Wine data-set
# data(wine)
# mydata <- wine  # Loading wine dataframe into mydata
# mydata <- as.matrix(mydata)
# wine.class is defined separately so no need to make any column NULL

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
iteration <- 1 # Counting number of iterations

while(!cluster)
{
  
  # Calculating the Euclidean Distance
  distancedata <- array(dim = c(m,k))
  for (i in 1:m)
  {
    for (j in 1:k)
    {
      # Euclidean Distance
      distancedata[i,j] <- sqrt(sum((mydata[i,]-centre[j,])^2))
      
      # Manhattan Distance
      # distancedata[i,j] <- sum(abs(mydata[i,] - mydata[j,]))
      
      # Mahanalobis Distance with a digaonal covariance matrix 
      # distancedata[i,j] <- 0
      # for (z in 1:n)
      # {
        # distancedata[i,j] <- distancedata[i,j] + ((mydata[i,z] - centre[j,z])/sd(mydata[,z]))^2
      # }
      # distancedata[i,j] <- sqrt(distancedata[i,j])
    }
  }
  
      

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
  iteration = iteration + 1 # Counting number of iterations
}

# Integrating the Clustering class into the data-set
features <- colnames(mydata)
mydata <- cbind(mydata, membershipdata)
colnames(mydata) <- c(features, "Cluster")
print(table(mydata[,"Cluster"]))

# Getting Box Plots
library(ggplot2)

plotdata <- mydata
plotdata <- data.frame(plotdata)
plotdata$Cluster <- as.factor(plotdata$Cluster)

# Getting boxplot for all the input features
for (i in 1:n)
{
  bp <- ggplot(plotdata, aes(x=Cluster,y=plotdata[,i])) + geom_boxplot()
  print(bp)
}

# PCA Graphs and Further Analysis

library(caret) ## Loading required package: lattice

# Function for principal component analysis
pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow=c(2,2))
  plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="l")
  par(mfrow=c(1,1))
}

# Set input variable and target variable
data.input <- subset(mydata, select=features)
data.target <- mydata[,"Cluster"]
data.target <- as.factor(data.target)

# Apply PCA on the input variable
data.pca <- prcomp(data.input,center = TRUE,scale. = TRUE)

# Prnt PCA results
print(data.pca)

# Summarise PCA results to get PCA cumulative distribution
summary(data.pca)

# PCA Charts
pcaCharts(data.pca)

# PCA Biplot for the points
biplot(data.pca,scale=0, cex=.7)

# If PCA output concentrated more on negative side. 
# Following set of R code will flip the numbers
pca.out <- data.pca
pca.out$rotation <- -pca.out$rotation
pca.out$x <- -pca.out$x
biplot(pca.out,scale=0, cex=.7)

library(ggbiplot)  # Getting a colorful plot of PCA as per the clusters

g <- ggbiplot(data.pca,scale = 0, var.scale = 0, labels=data.target, groups = data.target)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')

# Print the plot
print(g)

 
  

  



