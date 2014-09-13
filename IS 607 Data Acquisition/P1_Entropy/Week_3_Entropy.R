# Sandesh Sadalge IS 607 Project 1 - Entropy

# Only the code is below.  Plese see the R-Markdown I made for the full explanations.
# After realizing I was commenting up the code so much that it was hard to read, I decided to make an R-Markdown file to explain everything I was thinking when I tackled this project
# R-Markdown Location: 


fileLoc <- "https://raw.githubusercontent.com/spenglerss/MSDAFall2014/master/IS%20607%20Data%20Acquisition/P1_Entropy/entropy-test-file.csv"
#fileLoc <- "C:/Users/ssadalge/Documents/GitHub/MSDAFall2014/IS 607 Data Acquisition/P1_Entropy/entropy-test-file.csv"

dataset <- read.table(file = fileLoc, header = TRUE, sep = ",", stringsAsFactors = FALSE)

entropy <- function (d)
{
  # So I figured out that using table() makes the Entropy calculations much easier!
  # (Originally, I was going to use unique to figure out the distinct values in a vector and a loop or something to count the occurances, etc...)
  
  partitions <- table(d)  # Table gives you all the unique categorigal values and its frequency.
  partitions <- partitions / sum(partitions) # Replace frequency the probability by dividing frequency by total count
  partitions <- partitions * log2(partitions)
  entropy <- -sum(partitions)
}

infogain <- function (d, a)
{
  if (all(d==a) == TRUE)
  {
    infogain <- 0 
  } else
  {
    # Total Entropy for d:
    entropy.d <- entropy(d)
  
    x <- table(a, d) # Cols = d partitions & rows = a partitions
    y <- (x / rowSums(x))  # Each row now has prob of a paritions given d partition
    y <- y * ifelse(is.infinite(log2(y)),0,log2(y))
    z <- rowSums(x) / sum(rowSums(x))  # This is calcualtes the probabilities of the partitions
    entropy.a <- sum(z * -1 * rowSums(y))  # -rowSums(y) = entropy of the partition and z has the weights so multiplying it gives you the weighte entropy
  
    infogain <- entropy.d - entropy.a
  }
}

decide <- function(inputDF, col)
{
  x <- apply(inputDF,2,infogain,inputDF[,col])
  decide <- list(max=which.max(x), gains=x)

}

# Check to see if the outputs are as expected (from assignment sheet)

(entropy(dataset$answer))

(infogain(dataset$answer, dataset$attr1))

(infogain(dataset$answer, dataset$attr2))

(infogain(dataset$answer, dataset$attr3))

(decide(dataset,4))


# My old decide() function that didn't use apply():

decide2 <- function(inputDF, col)
{
  decideDF <- data.frame(colnames=colnames(inputDF))
  n <- length(decideDF$colnames)
  infogain.values <- rep(0, n)
  for (i in 1:n)
  {
    infogain.values[i] <- infogain(inputDF[,col], inputDF[,i])
  }
  
  decideDF <- data.frame(colnames=decideDF$colnames,infogain=infogain.values)
  
  decide <- list(max=which.max(infogain.values), gains=decideDF)
}

(decide2(dataset,4))