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


head(m)

# Question 1




# **********   SCRATCH WORK  *********************

head(movies)

m <- movies

m$SumRs <- movies$r1 + movies$r2 + movies$r3 + movies$r4 + movies$r5 + movies$r6 + movies$r7 + movies$r8 + movies$r9 + movies$r10

m<- movies[,7:16]*10

m$SumRs <- apply(m,1,sum)

(movies[,6:10])

colnames(movies)


