# Hierarchial Clustering 

# mydata <- iris  # Loading iris dataframe into mydata
# mydata$Species <- NULL # Remove the column representing classes
# mydata <- as.matrix(mydata)

mydata <- matrix(c(0,1,7,8,2,0,1,7,8,2),nrow=5,ncol=2)

n <- ncol(mydata)  # Dimension for clustering
m <- nrow(mydata)  # Number of points for clustering

# Generating the dissimilairty distance matrix from the given points
dissimilarity <- matrix(nrow=nrow(mydata),ncol=nrow(mydata))
rownames(dissimilarity) <- colnames(dissimilarity) <- LETTERS[1:nrow(mydata)]

for (i in 1:m)
{
  for (j in 1:m)
  {
    # Hamming Distance
    dissimilarity[i,j] <- sum(abs(mydata[i,] - mydata[j,]))
  }
}

diag(dissimilarity) <- NA # Diagonal are changed from 0 to NA for logical calculation of min/max
hierarchical <- list() # for storing the steps of hierarchical clustering in a list
k <- 1 # Counting the steps or giving the length of the dendogram

# Run while loop till the hierarchy reaches the root node
while(dim(dissimilarity)[1] > 1)
{
  # Calculating the first index of the minimum distance
  minDistance <- which(dissimilarity == min(dissimilarity,na.rm=TRUE), arr.ind=T)[1,]

  # Merge the respective elements [Single Linkage] 
  merge <- apply(cbind(dissimilarity[minDistance[2],],dissimilarity[minDistance[1],]),1,min)
  # '1' stands for row in the apply function
  
  # Merge the respective elements [Complete Linkage] 
  merge <- apply(cbind(dissimilarity[minDistance[2],],dissimilarity[minDistance[1],]),1,max)
  # '1' stands for row in the apply function
  
  # Updating the dissimilarity matrix
  dissimilarity[minDistance[2],] <- merge
  dissimilarity[,minDistance[2]] <- merge
  
  merge <- paste0(rownames(dissimilarity)[minDistance[2]],'/',rownames(dissimilarity)[minDistance[1]])
  # Paste '0' is used instead of paste since we do not want separation between the modified row names
  
  rownames(dissimilarity)[minDistance[2]] <- merge
  colnames(dissimilarity)[minDistance[2]] <- merge
  
  # Delete the merged column and reduce the dissimilarity matrix
  dissimilarity = dissimilarity[-minDistance[1],-minDistance[1]]
  
  diag(dissimilarity) <- NA # Diagonal element distances are redundant
  
  # Saving the steps in a hierarchical list
  hierarchical[[k]] <- dissimilarity
  k <- k + 1
}
