# Sandesh Sadalge Week 3 Assignment

# Question 1
nmbrNAs <- function (vector)
{
    nmbrNAs <- sum(is.na(vector)) 
    # Since it returns a vector of True/False & True = 1 and False = 0, the sum is number of NAs
}

# Question 2
nmbrNAsDF <- function (inputDF)
{
  # Easy way to "apply" the function I created above to each column int he data frame
  x <- apply(inputDF, 2, nmbrNAs)  
}

# Question 3

# PLEASE NOTE! it seems that my version of Summary does not quite do it the way Summary() works.
# summary() seems to 'roundup' to give you quartiles.  i.e. if the vector was all integer, it seems to output an integer
# I show the decimal amount.  Not a huge deal but a small descrepancy none-the-less

numericInfo <- function(numericVector)
{
  
  # Use the Q1 funtion to get the number of NAs
  NAs <- nmbrNAs(numericVector)  
  
  # If there are NAs, remove them as they get in the way
  numericVector <- numericVector[!is.na(numericVector)]  
  
  # Calculate Mean:
  l <- length(numericVector)
  avg <- sum(numericVector) / l
  
  # Calculate Stdev:
  Stdev <- sqrt(sum((avg - numericVector)^2) / l)
  
  #Now, let's order the vector ascending:
  numericVector <- numericVector[order(numericVector)]  
  
  # Now that it's ordered, easy to get the min & max
  minval <- numericVector[1] # If your font is confusing, this is the number "One"
  maxval <- numericVector[l] # and this is the letter L (which was set to equal earlier in the function)
    
  # To find the quartiles & Median, I created a helper function called median() which is below this function.
  med <- median(numericVector)
  
  # The first quartile is simply the median of the first half of the vector!
  qtr1 <- median(numericVector[1:trunc(l/2)])
  
  # The 3rd quartile is the meadian of the second half of the vector!
  qtr3 <- median(numericVector[(trunc(l/2)+1):l])
  
  # Now, just return all the pertinent information back through the function name:
  numericInfo <- list(mean=avg, Stdev=Stdev, min=minval,quartile1=qtr1, median=med,quartile3=qtr3,max=maxval,NumberNAs=NAs)
}

# Helper function to calculate median for the numericInfo function above:
median <- function(numericVector)
{
  # To get the Median in a vector:
  # If you have EVEN number of elements, you have to AVERAGE the middle two values.
  # If you have an ODD number of elements, you can just use the middle numer.
  l <- length(numericVector)
  
  if (l %% 2 == 0) # Even?
  {
    medIndex1 <- trunc(l / 2)
    medIndex2 <- medIndex1 + 1
    
  } else # Odd?
  {
    medIndex1 <- trunc(l / 2) + 1
    medIndex2 <- medIndex1
  }
  
  numericVector <- numericVector[order(numericVector)]  # Order the vector
  median <-(numericVector[medIndex1]+numericVector[medIndex2])/2
}


# Question 4


characterInfo <- function(charVector)
{
  # Again, first use the Q1 function to determine the number of NAs
  NAs <- nmbrNAs(charVector)
  
  # Now get rid of the NAs to make life easier
  charVector <- charVector[!is.na(charVector)]
  
  u2 <- table(charVector) # Get a table with the categorical values and their frequencies
  u2 <- u2[order(u2)]   # Order them from fewest to most
  most.common.V <- rownames(u2)[dim(u2)]  # After the ordering, the last one is the most common element
  most.common.N <- as.numeric(u2[dim(u2)]) # and this is the number of times for it
  
  # Now, just return all the pertinent information back through the function name:
  characterInfo <- list(most.common.element = most.common.V, most.common.times = most.common.N, NumberNAs=NAs)
}


# Question 5

# Please note, for the proportion of true values, I'm not counting NAs in the total count

logicInfo <- function(logicVector)
{
  NAs <- nmbrNAs(logicVector) # Count the NAs using Q1 function again
  
  logicVector <- logicVector[!is.na(logicVector)] # Remove the NAs to make life easier
  
  numTrue <- sum(logicVector)
  totalNonNAs <- length(logicVector)  # Not counting NAs as I've removed them.
  numFalse <- totalNonNAs - numTrue
  proportionTrue <- numTrue / totalNonNAs
  
  # Now, just return all the pertinent information back through the function name:
  logicInfo <- list(Number.True=numTrue, Number.False=numFalse, Proportion.True=proportionTrue,NumberNAs=NAs)
}

# Question 6

dfInfo <- function(inputDF)
{
  # First, I wanted to figure out which columns in the data frame were in which type: Numeric, Logical or Character.
  # Found a nifty way to do this using sapply().
  # Interestingly, this does NOT work with apply() only sapply (Much to my consternation!)
  
  numericcols <- sapply(inputDF, is.numeric) # numericcols is now a logical vector with TRUEs for each Numeric column
  logicalcols <- sapply(inputDF, is.logical) # Similarily, logicalcols tells you all the logical columns
  charcols <- sapply(inputDF, is.character)  # and charcols will tell you what the character cols are.
  
  numericColInfoList <- lapply(inputDF[numericcols],numericInfo) # Simply 'apply' the numericInfo function (created above) to all the columns of type numeric (& save the summary info into a list)
  logicalColInforList <- lapply(inputDF[logicalcols],logicInfo)  # Ditto for logical columns
  charColInfoList <- lapply(inputDF[charcols],characterInfo)  # Ditto  for character
  
  
  
  # Since I store all the summary type information from each of the functions in their own list, I return a list comprised of the of the 3 summary lists
  # Whoa, so many nested lists.  Kind of like the movie inception!
  dfInfo <- list(Numeric.Column.Info=numericColInfoList, Character.Column.Info=charColInfoList, Logical.Column.Info=logicalColInforList)
  
  # Ok, so I realize now, a couple of hours later, I could replace the lapply with a sapply and it would return matrixes which are a lot easier to look at.
}


# Some sample data and outputs of the functions above:

sampleNumeric <- c(2,2,NA,7,18,200,NA,54,34,87,29,29,NA,21,64,28,71,81,81,95,NA,54,34,87,29,29,NA,21,64,34)
sampleChar <- c("red", "red", "wine",NA,"red", "red", "wine",NA,"you","make","me","feel","so","fine","red", "red", "wine",NA,"red", "red", "wine",NA,"you","make","me","feel","so","fine","two","more")
# Sample logical vector w/ missing values (I was inspired to make one like this from two different forumn responses online):
set.seed(124)
sampleLogical <- sample(0:2, 30, replace = TRUE) # Repeatable sequence of length 30 of 0,1 or 2 
sampleLogical <- (replace(sampleLogical,which(sampleLogical==2),NA)) # Why 2? so I can replace those with NA
(sampleLogical <- as.logical(sampleLogical)) # Now conver the 0 and 1 into FALSE and TRUE, respectively.


(numericInfo(sampleNumeric)) # Function created above in Q3

(characterInfo(sampleChar)) # Function created above in Q4

(logicInfo(sampleLogical)) # Function created above in Q5

# Going to make a sample data frame using the sample vectors above
sampleDF <- data.frame(sampleNumeric, sampleLogical, sampleChar, Numeric2=(3*sampleNumeric), logical2=sampleLogical, stringsAsFactors=FALSE)

(dfInfo(sampleDF)) # Function created above in Q6
