# Sandesh Sadalge Week 2 Quiz

# Question 1
sample.vector <- c(2,2,5,7,18,200,200,54,34,87,29,29,21,21,64,28,71,81,81,95)
sum(sample.vector)/length(sample.vector)
# Answer: 56.45


# Question 2

sample.vector <- c(2,2,NA,7,18,200,NA,54,34,87,29,29,NA,21,64,28,71,81,81,95)

sample.vector <- na.omit(sample.vector)
sum(sample.vector)/length(sample.vector)
# Answer: 53.11

# Question 3

require(numbers)  # Will need the numbers package for the GCD() function!

x <- 100
y <- 50

GCD(x,y)


# Question 4

# I used a recursive solution:

SandeshGCD <- function (a, b)
{
  # First, I make sure a >= b, if not, swap the values using an intermediary variable, c
  if (a < b)
  {
    c <- a
    a <- b
    b <- c
  }
  
  if (a %% b == 0) # If the Remainder is 0, then you're done!
  {
    SandeshGCD <- b
  } else # This is from the Euclid algorithm, gcd(a,b) = gcd(b,r) where r is the remainder: a = nb + r
  {
    SandeshGCD(b, a %% b)
  }
}

(SandeshGCD(63,27))
# Answer: 9

# Question 5

evaluateQ5 <- function(x,y)
{
  evaluateQ5 <- x^2*y + 2*x*y - x*y^2
}

(evaluateQ5(4,5))

# Question 6

#Linking to my github to make it easier:
location1 <- "https://raw.githubusercontent.com/spenglerss/MSDAFall2014/master/IS%20607%20Data%20Acquisition/Week%203/week-3-make-model-data.csv"
location2 <- "https://raw.githubusercontent.com/spenglerss/MSDAFall2014/master/IS%20607%20Data%20Acquisition/Week%203/week-3-price-data.csv"

make.model.data <- read.table(file = location1, header = TRUE, sep = ",", stringsAsFactors = FALSE)
price.data <-read.table(file = location2, header = TRUE, sep = ",", stringsAsFactors = FALSE)

head(make.model.data)
head(price.data)

merged.data <- merge(x=make.model.data, y=price.data, by.x=c("ModelNumber"), by.y=c("ModelNumber"))

#Answer:
# Make model has 8 observations & price data has 28 observations.
# Looks like Make Model data has unique Model numbers but in Price data, model numbers can duplicate
# There is one instance of model 23120 in price data that does NOT exist in Make Model data.
# With the resulting merge being 27 observations, it looks like merge is like an INNER JOIN in SQL.
# Additionally the total variables being 8 makes sense.  4 variables + 5 variables - one repeating variable (Model Number) = 8 variables.

# Question 7

require(plyr)

right.merged.data <- join(x=make.model.data, y=price.data, by=c("ModelNumber"), type = "right") 

#Answer: Results in 28 observations with one observation having NA in the row for model = 23120

# Question 8

Q8Subset <- subset(right.merged.data, Year == 2010)

# Answer: 14 Observations

# Question 9

Q9Subset <- subset(right.merged.data, Color == "Red" & Price > 10000)

# Answer: 4 Observations

# Question 10

Q10Subset <- subset(Q9Subset, select=c(Make:ID,Mileage:Price) )

# Ansers: 4 Obs of 6 variables

# Question 11


NmbrChars <- function(charvector)
{
  NmbrChars <- sapply(charvector, nchar) # Using sapply() to return a vector.  Found out that nchar counts character length for this function
}

sample.char.vector <- c("bird","plane","superman")

result <- NmbrChars(sample.char.vector)

(result)

# Question 12

concatByChar <- function (charvec1, charvec2)
{
  if (length(charvec1) == length(charvec2))
  {
      concatByChar <- paste(charvec1,charvec2)
  } else
  {
    concatByChar <- c()
  }
  
}

sample1 <- c("bird","plane","superman")
sample2 <- c("I", "am","Batman!")
sample3 <- c("Ironman")

(concatByChar(sample1, sample2))
# Above worked:
#[1] "bird I"           "plane am"         "superman Batman!"

(concatByChar(sample1, sample3))
#above resulted in a NULL because they were differing lengths

# Question 13

require(stringr)
sample1 <- c("bird","plane","bbb","superman")
str_extract(string = sample1, "[a,e,i,o,u,A,E,I,O,U][A-z]{2}")
# Used regular expressions to get a begining vowel and 2 occurances of letters after it.
# this returns a NA if nothing is found


# Question 14

months <- c(1,4,6,10,2,8)
days <- c(23,1,14,30,21,6)
years <- c(1980,1975,1985,1951,1945,1978)
datesDF <- data.frame(months,days,years)

# I use the  paste() with a seperator of "" and as.character() to convert the numbers into a string in the format "YYYY-MM-DD"
# Then, i use as.Date() to convert it to date format and store in a temp vector
newdatescol <- as.Date(c(paste(as.character(datesDF$years),"/",as.character(datesDF$months),"/",as.character(datesDF$days), sep="")))
# Now, adding it to the datafram:
new.datesDF <- data.frame(datesDF[],newdatescol)

# Question 15

some.date <- "04-23-2014"
as.Date(some.date, "%m-%d-%Y") # WOW!!! if you have a lowercase 'y', things get hairy!

# Question 16

# One possibility is to use the format() function:
some.date <- as.Date("2014-09-01")
format(some.date, format="%m") # Number but as a string with leading 0, i.e. "09"
as.numeric(format(some.date, format="%m")) # Actual number i.e. 9
format(some.date, format="%b") # Abbreviated word i.e. Sep
format(some.date, format="%B") # Full month name i.e September

# Question 17

# I'm relying on the fact that dates are stored as integers (& time as decimal part)
start.date <- as.numeric(as.Date("2005-01-01"))
end.date <- as.numeric(as.Date("2014-12-31"))

dates.span <- seq(from=start.date, to=end.date, by=1) # Essentially doing a vector of integers for each date
dates.span <- as.Date(dates.span, origin="1970-01-01") # Converting integers back to datese Using the Origin = 1/1/1970 as that's the R system default

(dates.span)

# Answer: There are 3652 dates in the vector dates.span now