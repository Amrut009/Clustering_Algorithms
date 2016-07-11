# Fuzzy Clustering

# Task-1 : Verification Example Matrix 
# mydata <- matrix(c(0,1,7,8,2,0,1,7,8,2),nrow=5,ncol=2)

# Operating on iris data-set
mydata <- iris  # Loading iris dataframe into mydata
mydata$Species <- NULL # Remove the column representing classes
mydata <- as.matrix(mydata)

# Operating on Wine data-set
# data(wine)
# mydata <- wine  # Loading wine dataframe into mydata
# mydata <- as.matrix(mydata)

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
        # Update the membership matrix based on the Fuzzy formulation
        membership_sum <- 0
        for (z in 1:k)
        {
          # Summation over all cluster centres using Euclidean distance since m = 2
          membership_sum <- membership_sum + ((sqrt(sum(mydata[i,]-centres[j,])^2))/(sqrt(sum(mydata[i,]-centres[z,])^2)))^(2/(m-1))
        }
        membershipdata[i,j] <- 1/membership_sum        
        membershipdata[i,j] <- round(membershipdata[i,j],digits=6)
    } 
  }
  membershipdata <- replace(membershipdata, is.na(membershipdata), 1)
  
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

# Final membership based on max fuzziness
membershipfinal <- matrix(0,nrow=p,ncol=1) 
for (i in 1:p)
{
  # which.max gives the index of the maximum value
  membershipfinal[i,1] <- which.max(membershipdata[i,])
}

# Integrating the Clustering class into the data-set
features <- colnames(mydata)
mydata <- cbind(mydata, membershipfinal)
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
