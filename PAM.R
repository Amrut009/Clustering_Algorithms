# k-Mediods Clustering (Partioning Around Mediods)

# Task-1 : Verification Example Matrix 
# mydata <- matrix(c(0,1,7,8,2,0,1,7,8,2),nrow=5,ncol=2)

# Operating on iris data-set
# mydata <- iris  # Loading iris dataframe into mydata
# mydata$Species <- NULL # Remove the column representing classes
# mydata <- as.matrix(mydata)

# Operating on Wine data-set
data(wine)
mydata <- wine  # Loading wine dataframe into mydata
mydata <- as.matrix(mydata)

n <- ncol(mydata)  # Dimension for clustering
m <- nrow(mydata)  # Number of points for clustering

# Pre-defined number of clusters
k <- readline(prompt="Enter the number of clusters: ")
k <- as.integer(k)

# Randomly select cluster mediods
xmediod <- sample(1:m,k)

# Store the cluster mediods in a new array
mediods <- matrix(0,nrow=k,ncol=n) # Original set of mediods
newmediods <- matrix(0,nrow=k,ncol=n) # Updated set of mediods
# Randomly selecting initial set of mediods
for (i in 1:k)
  mediods[i,] <- mydata[xmediod[i],]

# Initalizing Distance matrix for storing the distance between two points
distancedata <- matrix(0, nrow=m,ncol=k)
membershipdata <- matrix(0,nrow=m,ncol=1)

# Initializing cost
cost <- Inf
iteration <- 1 # Counting number of iterations

# Swapping with non-mediod to check if the cost decreases
for (i in 1:m)  # Swapping for all non-mediods
{
  for (j in 1:k) # Swapping a new non-mediods with all the existing mediods
  {
    newmediods <- mediods
    newmediods[j,] <- mydata[i,]
    
    # Calculating the distance matrix
    for (i in 1:m)
    {
      for (j in 1:k)
      {
        # Euclidean Distance
        distancedata[i,j] <- sqrt(sum((mydata[i,]-newmediods[j,])^2))
        
        # Hamming Distance
        # distancedata[i,j] <- sum(abs(mediods[j,] - mydata[i,]))
        
        # Mahanalobis Distance with a digaonal covariance matrix 
        # distancedata[i,j] <- 0
        # for (z in 1:n)
        # {
        # distancedata[i,j] <- distancedata[i,j] + ((mydata[i,z] - centre[j,z])/sd(mydata[,z]))^2
        # }
        # distancedata[i,j] <- sqrt(distancedata[i,j])
      }
    }   

    # Computing the new cost
    totalcost <- 0
    for (j in 1:m)
      totalcost <- totalcost + min(distancedata[j,])
  
    if (totalcost < cost)
    {
      mediods <- newmediods
      cost <- totalcost
  
      # Membeship based on the cluster centres
      for (j in 1:m)
        for (z in 1:k)
          if (min(distancedata[j,]) == distancedata[j,z])
            membershipdata[j] <- z
    }
    iteration = iteration + 1 # Counting number of iterations
  }
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

# Print PCA results
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




