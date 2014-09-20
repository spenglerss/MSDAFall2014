# Sandesh Sadalge Week 4 Quiz

require(ggplot2)

# ggplot 2 comes with IMDB movie data: movies

head(movies)
class(movies) # You can see it's in a dataframe:
dim(movies) # 58,788 Rows & 24 Columns
colnames(movies) # Getting all the col names (though I realized later the descriptions are in the ggplot2 documentation)


movies$rating[which.max(movies$rating)] # Wanted to verify Max.  It's 10
movies$rating[which.min(movies$rating)] # Wanted to verify Max.  It's 1
sum(movies$rating)/length(movies$rating) # Average Rating is 5.93


# QUESTION 1

m <- movies

m$decade <- m$year - (movies$year %% 10) # basically, I take the year portion out to get the decade
m$year[which.max(m$year)] # What's the last year in the data?
table(m$decade) # Just to check that the decades subdivision worked right

# let's look at some histograms:

# Using base hist()
hist(m$decade, main = "Number of Movies By Decade", xlab = "Decade") 

# Two ways to do a histogram using ggplot2.  I like the quick plot right now
ggplot(data = m) + geom_histogram(aes(x = decade))
qplot(decade, data=m, geom="histogram", xlab="Decade", ylab = "Nmbr Movies")


# QUESTION 2

# My plan in the following is to replace the 0/1 in each of the genre columns with NA and the rating itself

# This gives a DF with TRUE/FALSE by genre
tempDF <- movies[c("Action","Animation","Comedy","Drama","Documentary","Romance","Short")]==1  
tempDF <- ifelse(tempDF == FALSE, NA, 1) # Replacing FALSE with NA
tempDF <- tempDF * movies$rating # Now, the Genre columns have NA or the rating if there is one!

# What's the average rating by genre, regardless of year?
genre.rating <- apply(tempDF, 2, mean, na.rm=TRUE)
# Simple barplot of the average rating by Genre.  Really
barplot(genre.rating, ylim=c(0,10))

# Added Year and Rating using cbind() to start looking at changes over the years:
tempDF <- cbind(year=movies$year,rating=movies$rating,tempDF)

# I'd really like to know how to use some combination of apply or plyr to do the plots below:

#Change in Ratings by Year for: ACTION:
yearly.avgrating <- aggregate(Action ~ year, tempDF, mean)
ggplot(data=yearly.avgrating,aes(x=year,y=Action))+geom_line()
#Change in Ratings by Year for: Animation:
yearly.avgrating <- aggregate(Animation ~ year, tempDF, mean)
ggplot(data=yearly.avgrating,aes(x=year,y=Animation))+geom_line()
#Change in Ratings by Year for: Comedy:
yearly.avgrating <- aggregate(Comedy ~ year, tempDF, mean)
ggplot(data=yearly.avgrating,aes(x=year,y=Comedy))+geom_line()
#Change in Ratings by Year for: Drama:
yearly.avgrating <- aggregate(Drama ~ year, tempDF, mean)
ggplot(data=yearly.avgrating,aes(x=year,y=Drama))+geom_line()
#Change in Ratings by Year for: Documentary:
yearly.avgrating <- aggregate(Documentary ~ year, tempDF, mean)
ggplot(data=yearly.avgrating,aes(x=year,y=Documentary))+geom_line()
#Change in Ratings by Year for: Romance:
yearly.avgrating <- aggregate(Romance ~ year, tempDF, mean)
ggplot(data=yearly.avgrating,aes(x=year,y=Romance))+geom_line()
#Change in Ratings by Year for: Short:
yearly.avgrating <- aggregate(Short ~ year, tempDF, mean)
ggplot(data=yearly.avgrating,aes(x=year,y=Short))+geom_line()

# QUESTION 3

# When I looked at all the length vs. rating relationships, it was hard to see any pattern:
temp <- aggregate(rating ~ length, movies, mean)
ggplot(data=temp,aes(x=length,y=rating))+geom_point()

# But for movies < 240 mins (4 hours) one does see something interesting:
temp <- aggregate(rating ~ length, movies[movies$length<=240,], mean)
ggplot(data=temp,aes(x=length,y=rating))+geom_point()
# Looks like people tend not to like movies about 90 mins long.
# They either like relatively short moves (hour or less) or about 2 hour long movies.
# After about 150 mins, it's a bit of a crapshoot


# QUESTION 4
require(reshape)

# let's just get the pertinent columns
tempDF <- movies[c("length","rating","Action","Animation","Comedy","Drama","Documentary","Romance","Short")]
# "Melt it" (will be useful for plotting later)
tempDF <- melt.data.frame(tempDF,id.vars=c("length","rating"))
# Keep only the value = 1 rows to save space
tempDF <- tempDF[tempDF$value==1,]

# Violin plot of lengths & genres:
ggplot(tempDF, aes(x=variable, y=length))+geom_violin()

# The y-Axis went way too high and I couldn't get the ylim parameter to fix it so I'll just limit to length <= 240:
ggplot(tempDF[tempDF$length<=240,], aes(x=variable, y=length))+geom_violin()

# Now you can see the Action, Drama & Romance  movies are just about 90 mins long but Action is more concentrated at about 90 mins
# Animations are generally really short (even shorter than 'Short' movies)
# Comedies are either very short or about 90 mins and
# Documentaries really don't have a set length.

# QUESTION 5

# Votes vs. Length:
temp <- aggregate(votes ~ length, movies[movies$length<=240,], mean)
ggplot(data=temp,aes(x=length,y=votes))+geom_point()
# Seems to me you get more votes the longer the moview


#Votes vs. rating:
temp <- aggregate(votes ~ rating, movies, mean)
ggplot(data=temp,aes(x=rating,y=votes))+geom_point()
# Looks like the sweet spot for most votes is about ratings of 8.  from 5 to about 9, you have a nice increase if votes.


#Violin Plots of Votes vs. Genres (Similar to how I did things in Question 4):
tempDF <- movies[c("votes","Action","Animation","Comedy","Drama","Documentary","Romance","Short")]
tempDF <- melt.data.frame(tempDF,id.vars="votes")
tempDF <- tempDF[tempDF$value==1,]

# Looks like Action and Drama gets the most amount of total votes:
ggplot(tempDF, aes(x=variable, y=votes))+geom_violin()

# Limit the number of votes to see the bottom of the violin plots better:
ggplot(tempDF[tempDF$votes<500,], aes(x=variable, y=votes))+geom_violin()
# Really not much differentiation onthe bottom of the violin plots