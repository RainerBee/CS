#____________________________________________________________________________
    
# * Data import ------------------------------------------------------------
#____________________________________________________________________________

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier: 
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           # title = as.character(title),
                                            #genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                            title = as.character(title),
                                            genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
      semi_join(edx, by = "movieId") %>%
      semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


#____________________________________________________________________________
    
# * Load additional packages --------------------------------------------
#____________________________________________________________________________


  # define required packages
  packs <- c("lubridate", "ggthemes", "knitr", "gridExtra", "scales", "pryr", "tinytex" )
  
  # install packages (if needed)  and load them
  for (package in packs) {
    if (!require(package, character.only=T, quietly=T)) {
      install.packages(package, repos = "http://cran.us.r-project.org")
      library(package, character.only=T)
    }
  }



#____________________________________________________________________________
    
# ** Switch of scientific number format -------------------------------------
#____________________________________________________________________________

  # Switch of scientific number format 
        options(scipen=999)    #default=0



#____________________________________________________________________________
    
# ** Check data structure --------------------------------------------------
#____________________________________________________________________________

  # check the data-structure edx    
    str(edx)  



#____________________________________________________________________________
    
# ** Data examples --------------------------------------------------
#____________________________________________________________________________

  # Data examples
    knitr::kable(head(edx))  



#____________________________________________________________________________
    
# * Further data preparation -----------------------------------------------
#____________________________________________________________________________

#____________________________________________________________________________
    
# ** add variable 'ts_date' ------------------------------------------------
#____________________________________________________________________________
  
    edx$ts_date <- date(as.POSIXct(edx$timestamp, origin="1970-01-01"))  



#____________________________________________________________________________
    
# ** add movie year --------------------------------------------------------
#____________________________________________________________________________
      edx$year <- edx$title %>% str_sub(-5,-2) %>%  as.numeric() 



#____________________________________________________________________________
    
# ** convert genre to factor -----------------------------------------------
#____________________________________________________________________________
      edx$genres <- as.factor(edx$genres)



#____________________________________________________________________________
    
# ** add variables year.rating & rating.lag --------------------------------
#____________________________________________________________________________

      edx <- edx %>% mutate ( year.rating = as.numeric(isoyear(ts_date)),
                          rating_lag =  year.rating - year,
                          rating_lag = ifelse (rating_lag<0,0,rating_lag)) 



#____________________________________________________________________________
    
# * Data expolation: ratings ------------------------------------------------
#____________________________________________________________________________


  n.ratings <- length(edx$rating)



#____________________________________________________________________________
    
# ** calculate mu ----------------------------------------------------------
#____________________________________________________________________________

  mu <- mean(edx$rating)  #calculate the average rating



#____________________________________________________________________________
    
# ** P: rating -------------------------------------------------------------
#____________________________________________________________________________


#barplot number of ratings
edx %>% 
    select ( rating) %>% 
    group_by(rating) %>% 
    summarize( pertcentage= n()  / length(edx$rating) )  %>% 
    ggplot(aes(rating, pertcentage)) +
    geom_bar (stat="identity", col="blue", fill="steelblue" )   +
    geom_text(aes(label=paste(round(pertcentage*100),"%",sep='')), vjust=-1) +
    geom_vline(xintercept = mean(edx$rating),
               linetype='dashed', col='firebrick1') +
    annotate('text', label=paste('mu=', round(mean(edx$rating),4),sep = ''  ), 
             x=mean(edx$rating)*0.97, y=0.3 , col="firebrick1", angle = 90 )+
    scale_y_continuous(labels = percent, limits = c(0, .4))  +
    scale_x_continuous( labels=seq(0.5,5,0.5), breaks = seq(0.5,5,0.5)) +
    ggtitle("number of ratings  (%) ") +
    theme_hc() +
    labs( tag= "P: Rating", y = "percentage" ) 


#____________________________________________________________________________
    
# * Data expolation: movies ------------------------------------------------
#____________________________________________________________________________

  n.movies <- length(unique(edx$movieId))




#____________________________________________________________________________
    
# ** P: movie 1 ------------------------------------------------------------
#____________________________________________________________________________


# median of ratings per movie
  median.n_ratings <- edx %>% group_by(movieId) %>%  
  summarise(n.movieratings=n()) %>% .$n.movieratings %>% median()
 
# plot ratings per movie 
   edx %>% 
      count(movieId) %>% 
        ggplot(aes(n)) + 
        geom_histogram(bins = 30, col="blue", fill="lightblue" ) + 
        geom_vline(xintercept = median.n_ratings,linetype='dashed', col='firebrick1') +
        annotate('text', label='m e d i a n', 
                 x=median.n_ratings*0.85, y=400, col="firebrick1", angle = 90 )+
        scale_x_log10() + 
        ggtitle("number of ratings per movie") + 
        labs(x = 'number of ratings (log10 scaled)', y = 'number of movies') +
        theme_hc() +
        labs(tag = "P: movie 1" ) 
    


#____________________________________________________________________________
    
# ** T: 5 most rated movies ------------------------------------------------
#____________________________________________________________________________

  movie_ratings <- 
        edx %>% select (movieId,  title, rating) %>% 
        group_by (movieId, title) %>% 
        summarize(n_ratings = n(), avg_rating=mean(rating)) %>% 
        ungroup() 
      
  movie_ratings %>% arrange(desc(n_ratings)) %>% 
        head(10) %>% kable(caption = "movies with most ratings")



    


#____________________________________________________________________________
    
# ** P: movie 2 (incl. define sigma) ---------------------------------------
#____________________________________________________________________________

  # define sigma
      sigma = sd(edx$rating)

  # plot number of ratings vs. avg rating: 
     edx %>% 
          select (movieId, rating) %>% 
          group_by (movieId) %>% 
          summarize(n_ratings = n(), avg_rating = mean(rating))  %>%
          #mutate(movieId = reorder(movieId, avg_rating, FUN = median)) %>%  
          mutate(rank = dense_rank(avg_rating)) %>%    
          ggplot(aes(n_ratings, avg_rating, col=rank)) +
          geom_point(alpha=0.2) +
          geom_smooth(col="orange", method = lm) +
          geom_hline(yintercept=mu, linetype='solid', col="firebrick1")+
          geom_hline(yintercept=mu+sigma, linetype='dashed', col="firebrick1")+
          geom_hline(yintercept=mu-sigma, linetype='dashed', col="firebrick1")+
          annotate('text', label=paste("mu=",round(mu,4), sep=''), 
                   x=2.5, y=mu*1.05 , col="firebrick1" )+
          ggtitle("movies: avg rating vs number of ratings ", 
                  subtitle="incl. linear regression line") + 
          theme_hc() +
          theme(legend.position = "none")+
          scale_x_log10() +
          labs(tag = "P: movie 2", x = 'number of ratings per movie (log10 scaled)', 
               y = 'average rating' ) 
      

 

#____________________________________________________________________________
    
# ** P: movie 3 ------------------------------------------------------------
#____________________________________________________________________________

   # average rating vs movies
    edx %>% 
      select (movieId, rating) %>% 
      group_by (movieId) %>% 
      summarize(n_ratings = n(), avg_rating = mean(rating))  %>%
      mutate(movieId = reorder(movieId, avg_rating, FUN = median)) %>% 
      mutate(rank = dense_rank(avg_rating)) %>%   
      ggplot(aes(movieId, avg_rating, size=10*n_ratings, col=rank)) +
      geom_point( alpha=0.2) +
      geom_hline(yintercept=mu, linetype='solid', col="firebrick1")+
      geom_hline(yintercept=mu+sigma, linetype='dashed', col="firebrick1")+
      geom_hline(yintercept=mu-sigma, linetype='dashed', col="firebrick1")+
      annotate('text', label=paste("mu=",round(mu,4), sep=''), 
               x=2000, y=mu*1.05 , col="firebrick1" )+
      ggtitle("average rating vs movies  " ,  
              subtitle =  'point size refers to number of ratings') +
      theme_hc() +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = "none") +
      labs(tag = "P: movie 3", 
           x = paste('movies (', n.movies, ') sorted by avg. rating', sep=''), 
           y = 'average rating' ) 
      



#____________________________________________________________________________
    
# ** T: tables best worst movie  -------------------------------------------
#____________________________________________________________________________

  
### look at the bottom/top movies in terms of avg rating

    movie_ratings %>% arrange(desc(avg_rating)) %>% 
      head (10) %>% kable(caption = "Movies with best ratings")
      
    movie_ratings %>% arrange(avg_rating) %>% 
      head (10) %>% kable(caption = "Movies with worst ratings")
  
rm(movie_ratings)
gc()



#____________________________________________________________________________
    
# * Data expolation: users ------------------------------------------------
#____________________________________________________________________________

# number of users
  n.user <- length(unique(edx$userId))  



#user with most ratings
  max.user.ratings <- edx %>% group_by(userId) %>% 
    summarize(n=n()) %>% summarize(max=max(n))



#____________________________________________________________________________
    
# ** P: user 1 -------------------------------------------------------------
#____________________________________________________________________________
 
  
#median of ratings per user
  median.n_ratings <- edx %>% group_by(userId) %>%  
  summarise(n.user_ratings=n()) %>% .$n.user_ratings %>% median()
  
# number of ratings per user  
  edx %>% 
    count(userId) %>% 
    ggplot(aes(n)) + 
    geom_histogram(bins = 30, col="blue", fill="lightblue" ) + 
    geom_vline(xintercept = median.n_ratings,linetype='dashed', col='firebrick1') +
    annotate('text', label='m e d i a n', 
             x=median.n_ratings*0.88, y=4000 , col="firebrick1", angle = 90 )+
    scale_x_log10() + 
    ggtitle("number of ratings per user") + 
    labs(x = 'number of ratings (log10 scaled)', y = 'number of users') +
    theme_hc() +
    labs(tag = "P: user 1")
  


#____________________________________________________________________________
    
# ** P: user 2 -------------------------------------------------------------
#____________________________________________________________________________

#plot user number of ratings vs avg rating
   
  edx %>% 
    select (userId, rating) %>% 
    group_by (userId) %>% 
    summarize(n_ratings = n(), avg_rating = mean(rating))  %>%
    mutate(rank = dense_rank(avg_rating)) %>%    
    ggplot(aes(n_ratings, avg_rating, col=rank)) +
    geom_point(alpha=0.1) +
    geom_smooth(col="orange" , method = lm) +
    geom_hline(yintercept=mu, linetype='solid', col="firebrick1")+
    geom_hline(yintercept=mu+sigma, linetype='dashed', col="firebrick1")+
    geom_hline(yintercept=mu-sigma, linetype='dashed', col="firebrick1")+
    annotate('text', label=paste("mu=",round(mu,4), sep=''), 
             x=3000, y=mu*1.05 , col="firebrick1" )+
    ggtitle("user: avg rating vs number of ratings ", 
            subtitle="incl. linear regression line") + 
    theme_hc() +
    theme(legend.position = "none") +
    scale_x_log10() +
    labs(tag = "P: user 2", x = 'number of ratings per user (log10 scaled)', 
         y = 'average rating' ) 
  

 
#____________________________________________________________________________
    
# ** P: user 3 -------------------------------------------------------------
#____________________________________________________________________________

# plot average rating vs users

  edx %>% 
    select (userId, rating) %>% 
    group_by (userId) %>% 
    summarize(n_ratings = n(), avg_rating = mean(rating))  %>%
    mutate(userId = reorder(userId, avg_rating, FUN = median)) %>% 
    mutate(rank = dense_rank(avg_rating)) %>%   
    ggplot(aes(userId, avg_rating, size=10*n_ratings, col=rank)) +
    geom_point( alpha=0.2) +
    geom_hline(yintercept=mu, linetype='solid', col="firebrick1")+
    geom_hline(yintercept=mu+sigma, linetype='dashed', col="firebrick1")+
    geom_hline(yintercept=mu-sigma, linetype='dashed', col="firebrick1")+
    annotate('text', label=paste("mu=",round(mu,4), sep=''), 
             x=9000, y=mu*1.05 , col="firebrick1" )+
    ggtitle("average rating vs users  " ,  
            subtitle =  'point size refers to number of ratings') +
    theme_hc() +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position = "none") +
    labs(tag = "P: user 3", 
         x = paste('users (', n.user, ') sorted by avg. rating', sep=''),  
         y = 'average rating' ) 
  


#____________________________________________________________________________
    
# * Data expolation: genres ------------------------------------------------
#____________________________________________________________________________

    n.genres <- length(unique(edx$genres))



#____________________________________________________________________________
    
# ** P: genres 1 -----------------------------------------------------------
#____________________________________________________________________________

#median genres

  median.n_ratings <- edx %>% group_by(genres) %>%  summarise(n_ratings=n()) %>% .$n_ratings %>% median()
 
  
# number of ratings per genre 

  edx %>% 
    count(genres) %>% 
    ggplot(aes(n)) + 
    geom_histogram(bins = 30, col="blue", fill="lightblue" ) + 
    geom_vline(xintercept = median.n_ratings,linetype='dashed', col='firebrick') +
    annotate('text', label='m e d i a n', 
             x=median.n_ratings*0.8, y=60 , col="firebrick", angle = 90 )+
    scale_x_log10() + 
    ggtitle("number of ratings per genres") + 
    labs(x = 'number of ratings (log10 scaled)', y = 'number of genres') +
    theme_hc() +
    labs(tag = "P: genres 1")
  


#____________________________________________________________________________
    
# ** T: genres 1 -----------------------------------------------------------
#____________________________________________________________________________


  # tables movies/ratings per piped genres (tail / head)
    # kable has trouble showing "|" in R but not in RMD
  
#genres, movies
    edx %>% 
      select (genres, movieId) %>% 
      group_by (genres) %>% summarize(n.movie=n()) %>% 
      arrange(desc(n.movie)) %>% 
      head(n=6) %>% kable(caption = "Genres with most movies")  
    
    edx %>% 
      select (genres, movieId) %>% 
      group_by (genres) %>% summarize(n.movie=n()) %>% 
      arrange(desc(n.movie)) %>% 
      tail(n=6) %>% kable(caption = "Genres with least movies")  



#____________________________________________________________________________
    
# ** T: genres 2 -----------------------------------------------------------
#____________________________________________________________________________

#genres, rating
    edx %>% 
      select (genres, rating) %>% 
      group_by (genres) %>% summarize(avg_rating=mean(rating)) %>% 
      arrange(desc(avg_rating)) %>% 
      head(n=6) %>% kable(caption = "Genres with best ratings")  
    
    edx %>% 
      select (genres, rating) %>% 
      group_by (genres) %>% summarize(avg_rating=mean(rating)) %>% 
      arrange(desc(avg_rating)) %>% 
      tail(n=6) %>% kable(caption = "Genres with worst ratings") 



#____________________________________________________________________________
    
# ** P: genres 2 -----------------------------------------------------------
#____________________________________________________________________________


# genres unpiped as data.frame
    genres.unpiped <- edx %>% select (movieId, genres) %>% 
      separate_rows(genres, sep = "\\|") %>% 
      group_by(genres) %>% summarize (n.movie=n()) %>% 
      mutate(genres =as.factor(genres))
    
# bar chart with movies per unpiped genre
    genres.unpiped %>% 
      mutate(genres = reorder(genres, desc(n.movie), FUN=median))  %>% 
      ggplot(aes(genres, n.movie)) +
      geom_col(col="blue", fill="lightblue" )+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggtitle("movies per genre", subtitle = " 'unpiped' genres ") +
      theme_hc() +
      labs(tag= "P: Genre 2", y = "number of movies" ) 
   
rm(genres.unpiped)
gc()




#____________________________________________________________________________
    
# ** P: genres 3 -----------------------------------------------------------
#____________________________________________________________________________

# plot  genres avg rating  vs nmbr of ratings : 
  edx %>% 
    select (genres, rating) %>% 
    group_by (genres) %>% 
    summarize(n_ratings = n(), avg_rating = mean(rating))  %>%
    mutate(rank = dense_rank(avg_rating)) %>%    
    ggplot(aes(n_ratings, avg_rating, col=rank)) +
    geom_point(alpha=0.2) +
    geom_smooth(col="orange", method=lm) +
    geom_hline(yintercept=mu, linetype='solid', col="firebrick")+
    geom_hline(yintercept=mu+sigma, linetype='dashed', col="firebrick")+
    geom_hline(yintercept=mu-sigma, linetype='dashed', col="firebrick")+
    annotate('text', label=paste("mu=",round(mu,4), sep=''), 
             x=5, y=mu*1.03 , col="firebrick1" )+
    ggtitle("genres: avg rating vs number of ratings", 
            subtitle="incl. linear regression line") + 
    theme_hc() +
    theme(legend.position = "none")+
    scale_x_log10() +
    labs(tag = "P: genres 3", 
         x = 'number of ratings (log10 scaled)', 
         y = 'average rating' ) 



#____________________________________________________________________________
    
# ** P: genres 4 -----------------------------------------------------------
#____________________________________________________________________________


 
##Genres rating effect 
  edx %>% 
    select (genres, rating) %>% 
    group_by (genres) %>% 
    summarize(n_ratings = n(), avg_rating = mean(rating))  %>%
    mutate(genres = reorder(genres, avg_rating, FUN = sum)) %>% 
    ggplot(aes(genres, avg_rating, size=10*n_ratings, col=avg_rating)) +
    geom_point(col="steelblue", alpha=0.2) +
    geom_hline(yintercept=mu, linetype='solid', col="firebrick1")+
    geom_hline(yintercept=mu+sigma, linetype='dashed', col="firebrick1")+
    geom_hline(yintercept=mu-sigma, linetype='dashed', col="firebrick1")+
    annotate('text', label=paste("mu=",round(mu,4), sep=''), 
             x=100, y=mu*1.05 , col="firebrick1" )+
    ggtitle("avg rating vs genres", 
            subtitle =  'point size refers to number of ratings')+
    theme_hc() +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position = "none") +
    labs(tag = "P: genres 4", 
         x = paste('genres (',n.genres , ') sorted by avg. rating', sep=''),  
         y = 'average rating' ) 
  


#____________________________________________________________________________
    
# * Data expolation: Movie year ------------------------------------------------
#____________________________________________________________________________

#____________________________________________________________________________
    
# ** P: year 1 -------------------------------------------------------------
#____________________________________________________________________________

# p1 Year: movie year vs. avg rating

  edx %>% group_by(year) %>% 
    summarize(avg_rating=mean(rating), n_ratings = n()) %>% 
  ggplot(aes(year,avg_rating, size = n_ratings)) +
  geom_smooth(col="orange") +
  geom_point(col="steelblue", alpha=0.8) +
  geom_hline(yintercept=mu, linetype='solid', col="firebrick1") +
  geom_hline(yintercept=mu+sigma, linetype='dashed', col="firebrick")+
  geom_hline(yintercept=mu-sigma, linetype='dashed', col="firebrick")+
  annotate('text', label=paste("mu=",round(mu,4), sep=''), 
           x=1950, y=mu*1.02 , col="firebrick1" ) +
  ggtitle("avg rating vs movie year", 
          subtitle = "incl. loess smooth") +
  theme_hc() +
  theme(legend.position = "none") +
  labs(tag = "P: year 1" , caption="point size represents number of ratings")



#____________________________________________________________________________
    
# ** P: year 2 -------------------------------------------------------------
#____________________________________________________________________________


# p2 Year: % of ratings per movie year 

  edx %>% select(year) %>% 
  ggplot(aes(x = year)) +
  geom_histogram(aes(y = ..count../sum(..count..)), 
                 binwidth = 10, col="blue", fill="lightblue") +
  geom_vline(xintercept = median(edx$year),
             linetype='dashed', col='firebrick') +
  annotate('text', label='median', 
           x=median(edx$year)*0.998, y=0.1 , col="firebrick", angle = 90 )+
  ggtitle("% of ratings per movie year released") +
  theme_hc() +
  labs(tag = "P: year 2" , y="percentage", x="year (grouped by 10)") +
  scale_y_continuous(labels = scales::percent_format())



#____________________________________________________________________________
    
# * Data expolation: Year of rating ----------------------------------------
#____________________________________________________________________________


#____________________________________________________________________________
    
# ** P: year.rating 1 ------------------------------------------------------
#____________________________________________________________________________

  # p1 year.rating: year.rating vs. avg rating
  
        edx %>% group_by(year.rating) %>% 
          summarize(avg_rating=mean(rating), n_ratings = n()) %>% 
        ggplot(aes(year.rating,avg_rating, size = n_ratings)) +
        geom_smooth(col="orange") +
        geom_point(col="steelblue", alpha=0.8) +
        geom_hline(yintercept=mu, col="firebrick1") +
        geom_hline(yintercept=mu+sigma, linetype='dashed', col="firebrick")+
        geom_hline(yintercept=mu-sigma, linetype='dashed', col="firebrick")+
        annotate('text', label=paste("mu=",round(mu,4), sep=''), 
                 x=2005, y=mu*1.02 , col="firebrick1" ) +
        ggtitle("avg.rating vs year of rating", 
                subtitle = "incl. loess smooth") +
        theme_hc() +
        theme(legend.position = "none") +
        labs(tag = "P: year.rating 1" ,
             caption="point size represents number of ratings")



#____________________________________________________________________________
    
# ** P: year.rating 2 ------------------------------------------------------
#____________________________________________________________________________


# p2 year.rating: % of ratings per year.rating
  edx %>% select(year.rating) %>% 
  ggplot(aes(x = year.rating)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 col="blue", fill="lightblue") +
  geom_vline(xintercept = median(edx$year.rating),
             linetype='dashed', col='firebrick') +
  annotate('text', label='median', x=2001.5, y=0.1 , 
           col="firebrick", angle = 90 )+
  ggtitle("% of ratings per year") +
  theme_hc() +
  labs(tag="P: year.rating 2",y="percentage", x="year of rating") +
  scale_y_continuous(labels = scales::percent_format())



#____________________________________________________________________________
    
# * Data expolation: Rating lag --------------------------------------------
#____________________________________________________________________________

#____________________________________________________________________________
    
# ** P: rating lag 1 -------------------------------------------------------
#____________________________________________________________________________

# p1 plot avg rating vs rating_lag
  edx %>% group_by(rating_lag) %>% 
  summarize(avg_rating=mean(rating), n_ratings = n()) %>% 
  ggplot(aes(rating_lag,avg_rating, size = n_ratings)) +
  geom_smooth(col="orange") +
  geom_point(col="steelblue", alpha=0.8) +
  geom_hline(yintercept=mu, linetype='solid',col="firebrick1") +
  geom_hline(yintercept=mu+sigma, linetype='dashed', col="firebrick")+
  geom_hline(yintercept=mu-sigma, linetype='dashed', col="firebrick")+
  annotate('text', label=paste("mu=",round(mu,4), sep=''), 
           x=50, y=mu*1.02 , col="firebrick1" ) +
  ggtitle("avg rating vs rating_lag", subtitle = "incl. loess smooth") +
  theme_hc() +
  theme(legend.position = "none") +
  labs(tag = "P: rating lag 1", 
       caption="point size represents number of ratings")




#____________________________________________________________________________
    
# ** P: rating lag 2 -------------------------------------------------------
#____________________________________________________________________________


# p2 plot perc of ratings per rating_lag
edx %>% select(rating_lag) %>% 
  ggplot(aes(x = rating_lag)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 col="blue", fill="lightblue") +
  geom_vline(xintercept = median(edx$rating_lag),
             linetype='dashed', col='firebrick') +
  annotate('text', label='median', 
           x=median(edx$rating_lag)*0.7, y=0.1 , 
           col="firebrick", angle = 90 )+
  ggtitle("% of ratings per rating_lag ") +
  theme_hc() +
  labs(tag = "P: rating lag 2", y="percentage", x="rating lag") +
  scale_y_continuous(labels = scales::percent_format())



#____________________________________________________________________________
    
# * Building the prediction models -----------------------------------------
#____________________________________________________________________________

#____________________________________________________________________________
    
# ** setup cv  & define RMSE -----------------------------------------------
#____________________________________________________________________________


# set.seed in order to create reproducible results. 
  set.seed(123, sample.kind="Rounding")
  
# Create 5  folds for use in crossvalidation from edx (createFolds from caret)
  folds <- createFolds(as.numeric(rownames(edx)), k = 5) 
  
# RMSE function   
  RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))}



#____________________________________________________________________________

# ** m1 Base model: just the average ---------------------------------------
#____________________________________________________________________________

## model function   
  m1.simply.mu <- function (x) {
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    start <- as.numeric(Sys.time())
    
    train <- edx[-folds[[x]],]    #define train set
    test <-  edx[folds[[x]],]     #define test set    
    mu_hat <- mean(train$rating)  #mu_hat as prediction as avg. of all ratings
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    rmse <- RMSE(test$rating, mu_hat)   # calculate rmse
    end <- as.numeric(Sys.time())
    runtime <- round(end-start)
    
    result <- data.frame(method="simply.mu", 
                         fold=x, 
                         rmse=rmse, 
                         lambda='na', 
                         duration=runtime )  #store result in a data.frame
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
  }
  
## call model function  
  # apply for folds 1:5 the function 'simply_mu'
  result.m1.average <- map_df(1:5, m1.simply.mu) 

## save model result to overall results dataframe
  results <- data.frame(Method="m1 simply the average", 
                RMSE=mean(result.m1.average$rmse), 
                reference = "edx CV 5f", 
                lambda = 'na',
                duration = sum(result.m1.average$duration))  



## present overall results
  results %>% kable()  
    

 
#____________________________________________________________________________

# ** m2 Movie effect -------------------------------------------------------
#____________________________________________________________________________

## model function
  m2.movie.effect <- function (x) {

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>

    start <- as.numeric(Sys.time())

    train <- edx[-folds[[x]],]
    test <-  edx[folds[[x]],]

    test <- test %>%
      semi_join(train, by = 'movieId')

    mu <-mean ( train$rating )

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>

    b.movie <- train %>%
      group_by(movieId) %>%
      summarize(b.m = mean ( rating - mu))  # bias b.m for movie

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>

    predicted <-
      mu + test %>%
      left_join(b.movie, by='movieId') %>%
      pull(b.m)

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>

    rmse <- RMSE (test$rating, predicted)
    end <- as.numeric(Sys.time())
    runtime <- round(end-start)

    #result.movie.effect
    result <- data.frame(method="movie.effect",
                         fold=x,
                         rmse=rmse,
                         duration = runtime )

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
  }

## call model function
  result.m2.movie.effect <- map_df(1:5, m2.movie.effect)
 
## save model result to overall results dataframe
  duration.tmp = sum(result.m2.movie.effect$duration)  
  results <- rbind(results, data.frame(Method="m2 movie effect",
                               RMSE=mean(result.m2.movie.effect$rmse),
                               reference = "edx CV 5f",
                               lambda = 'na',
                               duration=duration.tmp))
  


  ## present overall results
  results %>% kable()  
    


#____________________________________________________________________________
  
# ** Setup parameters for crossvalidation and optimizing lambda ------------
#____________________________________________________________________________

# With regularization we need a range of lambda values 
  #in order to find the lambda resulting in the lowest rmse 
  
  #create parameters for folds and lambdas
  params <- expand.grid(1:5, seq(0,10,1)) 
  names(params) <- c("fold","lambda")




#____________________________________________________________________________
  
# ** m3.1 Movie effect regularized -----------------------------------------
#____________________________________________________________________________

# model function
  m31.movie.effect.reg <- function (x,l) {
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
    start <- as.numeric(Sys.time()) 
    
    train <- edx[-folds[[x]],]
    test <-  edx[folds[[x]],]  
    
    test <- test %>% 
      semi_join(train, by = 'movieId')
    
    mu <-mean ( train$rating )  
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    b.movie <- train %>% 
      group_by(movieId) %>% 
      summarize(s.m = sum ( rating - mu), n.m =n())  %>% 
      mutate ( b.m = s.m/(n.m + l))   # l  = lambda from params
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    predicted <-
      test %>% 
      left_join( b.movie, by='movieId') %>% 
      mutate (pred = mu + b.m) %>% 
      pull(pred)
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    rmse <- RMSE (test$rating, predicted)
    end <- as.numeric(Sys.time())
    runtime <- round(end-start)
    
    #result.movie.effect
    result <- data.frame(method="movie.effect.reg", 
                         fold=x, lambda= l, 
                         rmse=rmse, 
                         duration = runtime  ) 
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
  }

## call model function 
  # we handover 2 parameters (fold&lambda) to the function, so we use map2_dfr
  result.m31.movie.effect.reg <- 
    map2_dfr(params$fold, params$lambda, m31.movie.effect.reg)  

# calculate the optimized lambda
  lambda.m <- result.m31.movie.effect.reg %>% 
    group_by(lambda) %>%
    summarize(rmse=mean(rmse))%>% 
    filter(rmse==min(rmse)) %>% .$lambda
  
## calculate optimized rmse
  rmse.result.m31.movie.effect.reg <- result.m31.movie.effect.reg %>%
    group_by(lambda) %>% summarize(rmse=mean(rmse)) %>%
    filter(rmse==min(rmse)) %>% .$rmse
 
## save model result to overall results dataframe  
  duration.tmp = sum(result.m31.movie.effect.reg$duration)  
  results <- rbind(results, data.frame(Method="m3.1 movie effect reg.", 
                               RMSE=rmse.result.m31.movie.effect.reg, 
                               reference= "edx CV 5f", 
                               lambda=lambda.m,
                               duration=duration.tmp))

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>   
  


## quick view on the resulting data.frame
  result.m31.movie.effect.reg %>% head(7) %>% kable()
    


# quick visualitions of the lambda's 
result.m31.movie.effect.reg %>%   
  ggplot(aes(lambda, rmse, col=fold)) +
  geom_point() +
  geom_vline(xintercept = lambda.m, col="firebrick1") +
  ggtitle('result.m31.movie.effect.reg', 
          subtitle = '>> red line = optimized lambda <<') +
  theme_hc()



## present overall results
  results %>% kable()  




#____________________________________________________________________________
  
# *** Investigation regularization impact ----------------------------------
#____________________________________________________________________________
  
# Data frame with bias for movies
  b.movie <- edx %>% 
    group_by(title, movieId) %>% 
    summarize(s.m = sum ( rating - mu),        # sum of each 'rating - mu' 
              n.m =n(),                        # number of ratings
              mu.m = mean(rating))  %>%        # prediction as in Model M2
    mutate ( b.m.unreg = s.m/(n.m),            # unregularized bias
             b.m.reg = s.m/(n.m + lambda.m),   # regularized bias
             pred.m.unreg = mu+b.m.unreg,      # unregularized prediction
             pred.m.reg = mu+b.m.reg)          # regularized prediction
  
# Data frame only comparison movies
  comp.movie.bs <- b.movie %>% filter(movieId %in% c(3226, 296))  %>% 
    select(-movieId, -s.m)

  
# Review table  
  kable(comp.movie.bs, caption = "Investigate regularization", digits=5)
  
  


#____________________________________________________________________________
  
# ** m3.2 Movie+User effect regularized-------------------------------------
#____________________________________________________________________________
  
## model function    
m32.movie.user.effect.reg <- function (x,l) {
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  
  start <- as.numeric(Sys.time())       
  
  train <- edx[-folds[[x]],]
  test <-  edx[folds[[x]],]  
  
  test <- test %>% 
    semi_join(train, by = 'movieId') %>% 
    semi_join(train, by = 'userId')
  
  mu <-mean ( train$rating )  
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  b.movie <- train %>% 
    group_by(movieId) %>% 
    summarize(s.m = sum ( rating - mu), n.m =n())  %>% 
    mutate ( b.m = s.m/(n.m + lambda.m))  # opt.lambda movie-effect
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  b.user <- train %>% 
    left_join(b.movie, by='movieId') %>% 
    group_by(userId) %>% 
    summarize(s.u = sum ( rating - mu - b.m), n.u =n()) %>% 
    mutate ( b.u = s.u/(n.u + l))   # l  = lambda from params
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  predicted <-
    test %>% 
    left_join(b.movie, by='movieId') %>% 
    left_join(b.user, by='userId') %>% 
    mutate (pred = mu + b.m + b.u) %>% 
    pull(pred)
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>> 
  
  rmse <- RMSE (test$rating, predicted)
  end <- as.numeric(Sys.time())
  runtime <- round(end-start)
  
  #result.movie.effect
  result <- data.frame(method="user.effect.reg", 
                       fold=x, 
                       lambda=l, 
                       rmse=rmse, 
                       duration=runtime )  
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
}

## call model function     
    result.m32.movie.user.effect.reg <- 
      map2_dfr(params$fold, params$lambda, m32.movie.user.effect.reg)

# calculate the optimized lambda    
    lambda.u <- result.m32.movie.user.effect.reg %>% 
      group_by(lambda) %>% 
      summarize(rmse=mean(rmse))  %>% 
      filter(rmse==min(rmse)) %>% .$lambda

## calculate optimized rmse
  rmse.result.m32.movie.user.effect.reg <- 
    result.m32.movie.user.effect.reg %>% 
      group_by(lambda) %>% summarize(rmse=mean(rmse))  %>% 
      filter(rmse==min(rmse)) %>% .$rmse

## save model result to overall results dataframe   
  duration.tmp = sum(result.m32.movie.user.effect.reg$duration)  
  results <- rbind(results, data.frame(Method="m3.2 movie+user effect reg.", 
                               RMSE=rmse.result.m32.movie.user.effect.reg, 
                               reference = "edx CV 5f", 
                               lambda=paste('l.m=',lambda.m,'/','l.u=',
                                    lambda.u, sep=''),
                               duration=duration.tmp))
   
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>  



# quick visualitions of the lambda's 
    result.m32.movie.user.effect.reg %>%   
      ggplot(aes(lambda, rmse, col=fold)) +
      geom_point() +
      geom_vline(xintercept = lambda.u, col="firebrick1") +
      ggtitle('result.m32.movie.user.effect.reg', 
              subtitle = '>> red line = optimized lambda <<') +
      theme_hc()  



  ## present overall results
  results %>% kable()  
    


#____________________________________________________________________________
    
# ** m3.3 Movie+User+Genres effect regularized -----------------------------
#____________________________________________________________________________
 
## model function    
    m33.movie.user.genre.effect.reg <- function (x,l) {
      
      # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
      
      start <- as.numeric(Sys.time())
      
      train <- edx[-folds[[x]],]
      test <-  edx[folds[[x]],]  
      
      test <- test %>% 
        semi_join(train, by = 'movieId') %>% 
        semi_join(train, by = 'userId')
      
      mu <-mean ( train$rating )  
      
      # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
      
      b.movie <- train %>% 
        group_by(movieId) %>% 
        summarize(s.m = sum ( rating - mu), n.m =n())  %>% 
        mutate ( b.m = s.m/(n.m + lambda.m))   #  opt. lambda movie-effect
      
      # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
      
      b.user <- train %>% 
        left_join(b.movie, by='movieId') %>% 
        group_by(userId) %>% 
        summarize(s.u = sum ( rating - mu - b.m), n.u =n()) %>% 
        mutate ( b.u = s.u/(n.u + lambda.u))   # opt, lambda  user-effect
      
       # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
      
      b.genre <- train %>% 
        left_join(b.movie, by='movieId') %>% 
        left_join(b.user, by='userId') %>%  
        group_by(genres) %>% 
        summarize(s.g = sum ( rating - mu - b.m - b.u ), n.g = n()) %>% 
        mutate ( b.g = s.g/(n.g + l))   # l  = lambda from params
      
      # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
      
      predicted <-
        test %>% 
        left_join(b.movie, by='movieId') %>% 
        left_join(b.user, by='userId') %>% 
        left_join(b.genre, by='genres') %>% 
        mutate (pred = mu + b.m + b.u + b.g) %>% 
        pull(pred)
      
      # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
      rmse <- RMSE (test$rating, predicted)
      end <- as.numeric(Sys.time())
      runtime <- round(end-start)
      
      #result.movie.effect
      result <- data.frame(method="movie+user+genre.effect.reg", 
                           fold=x, 
                           lambda= l, 
                           rmse=rmse, 
                           duration=runtime )  
      
      # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    }
    
## call model function    
    result.m33.movie.user.genre.effect.reg <- 
      map2_dfr(params$fold, params$lambda, m33.movie.user.genre.effect.reg)

# calculate the optimized lambda
    lambda.g <- result.m33.movie.user.genre.effect.reg %>% 
      group_by(lambda) %>% 
      summarize(rmse=mean(rmse))%>% 
      filter(rmse==min(rmse)) %>% .$lambda

## calculate optimized rmse    
    rmse.result.m33.movie.user.genre.effect.reg <- 
      result.m33.movie.user.genre.effect.reg %>% 
      group_by(lambda) %>% summarize(rmse=mean(rmse)) %>% 
      filter(rmse==min(rmse)) %>% .$rmse
    
## save model result to overall results dataframe   
  duration.tmp = sum(result.m33.movie.user.genre.effect.reg$duration)  
  results <- rbind(results, data.frame(
                               Method="m3.3 movie+user+genre effect reg.", 
                               RMSE=rmse.result.m33.movie.user.genre.effect.reg, 
                               reference = "edx CV 5f", 
                               lambda=
                                 paste('l.m=',lambda.m,'/','l.u=',
                                    lambda.u,'/','l.g=',lambda.g, sep=''),
                               duration=duration.tmp))
    
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>     



# quick view on the results
    result.m33.movie.user.genre.effect.reg  %>%   
    ggplot(aes(lambda, rmse, col=fold)) +
      geom_point() +
      geom_vline(xintercept = lambda.g, col="firebrick1") +
      ggtitle('result.m33.movie.user.genre.effect.reg', 
              subtitle = '>> red line = optimized lambda <<') +
      theme_hc()   



  ## present overall results
  results %>% kable()  
    


#____________________________________________________________________________
    
# ** m4.1 Movie+User+Genres effect regularized one-for-all --------------------
#____________________________________________________________________________
    
## model function    
  m41.movie.user.genre.effect.one4all <- function (x,l) {
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    start <- as.numeric(Sys.time())
    
    train <- edx[-folds[[x]],]
    test <-  edx[folds[[x]],]  
    
    test <- test %>% 
      semi_join(train, by = 'movieId') %>% 
      semi_join(train, by = 'userId')
    
    mu <-mean ( train$rating )  
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    b.movie <- train %>% 
      group_by(movieId) %>% 
      summarize(s.m = sum ( rating - mu), n.m =n())  %>% 
      mutate ( b.m = s.m/(n.m + l))   #  l lambda l
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    b.user <- train %>% 
      left_join(b.movie, by='movieId') %>% 
      group_by(userId) %>% 
      summarize(s.u = sum ( rating - mu - b.m), n.u =n()) %>% 
      mutate ( b.u = s.u/(n.u + l))   # l lambda l
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    b.genre <- train %>% 
      left_join(b.movie, by='movieId') %>% 
      left_join(b.user, by='userId') %>%  
      group_by(genres) %>% 
      summarize(s.g = sum ( rating - mu - b.m - b.u ), n.g = n()) %>% 
      mutate ( b.g = s.g/(n.g + l))   # lambda
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    predicted <-
      test %>% 
      left_join(b.movie, by='movieId') %>% 
      left_join(b.user, by='userId') %>% 
      left_join(b.genre, by='genres') %>% 
      mutate (pred = mu + b.m + b.u + b.g) %>% 
      pull(pred)
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    rmse <- RMSE (test$rating, predicted)
    end <- as.numeric(Sys.time())
    runtime <- round(end-start)
    
    #result.movie.effect
    result <- data.frame(method="movie+user+genre.effect.reg", 
                         fold=x, 
                         lambda= l, 
                         rmse=rmse, duration=runtime )  
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
  }

## call model function    
result.m41.movie.user.genre.effect.one4all <- 
  map2_dfr(params$fold, params$lambda, m41.movie.user.genre.effect.one4all)

# calculate the optimized lambda
  lambda.mug <- result.m41.movie.user.genre.effect.one4all %>% 
    group_by(lambda) %>% summarize(rmse=mean(rmse))  %>% 
    filter(rmse==min(rmse)) %>% .$lambda

## calculate optimized rmse    
  rmse.result.m41.movie.user.genre.effect.one4all <- 
    result.m41.movie.user.genre.effect.one4all %>% 
    group_by(lambda) %>% summarize(rmse=mean(rmse))  %>% 
    filter(rmse==min(rmse)) %>% .$rmse

## save model result to overall results dataframe   
  duration.tmp = sum(result.m41.movie.user.genre.effect.one4all$duration)  
  results <- rbind(results, data.frame(
                        Method="m4.1 movie+user+genre.effect reg. one4all", 
                        RMSE=rmse.result.m41.movie.user.genre.effect.one4all, 
                        reference = "edx CV 5f", 
                        lambda=lambda.mug,
                        duration=duration.tmp))

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>       



# quick view on the results
    result.m41.movie.user.genre.effect.one4all  %>%   
    ggplot(aes(lambda, rmse, col=fold)) +
      geom_point() +
      geom_vline(xintercept = lambda.mug, col="firebrick1") +
      ggtitle('result.m41.movie.user.genre.effect.one4all', 
              subtitle = '>> red line = optimized lambda <<') +
      theme_hc()   



  ## present overall results
  results %>% kable()  
    


#____________________________________________________________________________

# ** m5.1 Movie+User+Genres+lag effect regularized one-for-all -------------
#____________________________________________________________________________

## model function    
  m51.movie.user.genre.effect.lag.one4all <- function (x,l) {
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    start <- as.numeric(Sys.time())
    
    train <- edx[-folds[[x]],]
    test <-  edx[folds[[x]],]  
    
    test <- test %>% 
      semi_join(train, by = 'movieId') %>% 
      semi_join(train, by = 'userId')
    
    mu <-mean ( train$rating )  
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    b.movie <- train %>% 
      group_by(movieId) %>% 
      summarize(s.m = sum ( rating - mu), n.m =n())  %>% 
      mutate ( b.m = s.m/(n.m + l))   #  l=lambda 
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    b.user <- train %>% 
      left_join(b.movie, by='movieId') %>% 
      group_by(userId) %>% 
      summarize(s.u = sum ( rating - mu - b.m), n.u =n()) %>% 
      mutate ( b.u = s.u/(n.u + l))   #  l=lambda 
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    b.genre <- train %>% 
      left_join(b.movie, by='movieId') %>% 
      left_join(b.user, by='userId') %>%  
      group_by(genres) %>% 
      summarize(s.g = sum ( rating - mu - b.m - b.u ), n.g = n()) %>% 
      mutate ( b.g = s.g/(n.g + l))   # l=lambda 
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    b.lag <- train %>% 
      left_join(b.movie, by='movieId') %>% 
      left_join(b.user, by='userId') %>% 
      left_join(b.genre, by='genres') %>% 
      group_by(rating_lag) %>% 
      summarize(s.l = sum ( rating - mu - b.m - b.u - b.g), n.l = n()) %>% 
      mutate(b.l = s.l/(n.l + l))     # l=lambda 
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    predicted <-
      test %>% 
      left_join(b.movie, by='movieId') %>% 
      left_join(b.user, by='userId') %>% 
      left_join(b.genre, by='genres') %>% 
      left_join(b.lag, by='rating_lag') %>% 
      mutate (pred = mu + b.m + b.u + b.g + b.l) %>% 
      pull(pred)
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    rmse <- RMSE (test$rating, predicted)
    end <- as.numeric(Sys.time())
    runtime <- round(end-start)
    
    #result.movie.effect
    result <- data.frame(method="movie+user+genre+lag.effect.one4all", 
                         fold=x, 
                         lambda= l, 
                         rmse=rmse, 
                         duration=runtime )  
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
  }
  
## call model function    
  result.m51.movie.user.genre.effect.lag.one4all <- 
    map2_dfr(params$fold, params$lambda, 
             m51.movie.user.genre.effect.lag.one4all)
  
# calculate the optimized lambda
  lambda.mugl <- result.m51.movie.user.genre.effect.lag.one4all %>% 
    group_by(lambda) %>% summarize(rmse=mean(rmse))  %>% 
    filter(rmse==min(rmse)) %>% .$lambda

## calculate optimized rmse    
  rmse.result.m51.movie.user.genre.effect.lag.one4all <- 
    result.m51.movie.user.genre.effect.lag.one4all %>% 
    group_by(lambda) %>% summarize(rmse=mean(rmse))  %>% 
    filter(rmse==min(rmse)) %>% .$rmse

## save model result to overall results dataframe   
  duration.tmp = sum(result.m51.movie.user.genre.effect.lag.one4all$duration)  
  results <- rbind(results, data.frame(
                    Method="m5.1 movie+user+genre+lag effect reg. one4all", 
                    RMSE=rmse.result.m51.movie.user.genre.effect.lag.one4all, 
                    reference = "edx CV 5f", 
                    lambda=lambda.mugl,
                    duration=duration.tmp))

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>  



# quick view on the results
  result.m51.movie.user.genre.effect.lag.one4all  %>%   
  ggplot(aes(lambda, rmse, col=fold)) +
    geom_point() +
    geom_vline(xintercept = lambda.mugl, col="firebrick1") +
    ggtitle('result.m51.movie.user.genre.effect.lag.one4all', 
            subtitle = '>> red line = optimized lambda <<') +
    theme_hc()    



  ## present overall results
  results %>% kable()  
    


#____________________________________________________________________________
  
# ** m5.2 Movie+User+Genres+year effect regularized one4all ----------------
#____________________________________________________________________________
  
## model function    
  m52.movie.user.genre.effect.year.one4all <- function (x,l) {
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    start <- as.numeric(Sys.time())
    
    train <- edx[-folds[[x]],]
    test <-  edx[folds[[x]],]  
    
    test <- test %>% 
      semi_join(train, by = 'movieId') %>% 
      semi_join(train, by = 'userId')
    
    mu <-mean ( train$rating )  
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    b.movie <- train %>% 
      group_by(movieId) %>% 
      summarize(s.m = sum ( rating - mu), n.m =n())  %>% 
      mutate ( b.m = s.m/(n.m + l))   #  l=lambda 
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    b.user <- train %>% 
      left_join(b.movie, by='movieId') %>% 
      group_by(userId) %>% 
      summarize(s.u = sum ( rating - mu - b.m), n.u =n()) %>% 
      mutate ( b.u = s.u/(n.u + l))   #  l=lambda 
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    b.genre <- train %>% 
      left_join(b.movie, by='movieId') %>% 
      left_join(b.user, by='userId') %>%  
      group_by(genres) %>% 
      summarize(s.g = sum ( rating - mu - b.m - b.u ), n.g = n()) %>% 
      mutate ( b.g = s.g/(n.g + l))   # l=lambda 
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    b.year <- train %>% 
      left_join(b.movie, by='movieId') %>% 
      left_join(b.user, by='userId') %>%  
      left_join(b.genre, by='genres') %>%  
      group_by(year) %>% 
      summarize(s.y = sum ( rating - mu - b.m - b.u - b.g), n.y = n()) %>% 
      mutate ( b.y = s.y/(n.y + l))   # l=lambda
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    predicted <-
      test %>% 
      left_join(b.movie, by='movieId') %>% 
      left_join(b.user, by='userId') %>% 
      left_join(b.genre, by='genres') %>% 
      left_join(b.year, by='year') %>% 
      mutate (pred = mu + b.m + b.u + b.g + b.y) %>% 
      pull(pred)
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
    rmse <- RMSE (test$rating, predicted)
    end <- as.numeric(Sys.time())
    runtime <- round(end-start)
    
    #result.movie.effect
    result <- data.frame(method="movie+user+genre+year.effect.one4all", 
                         fold=x, 
                         lambda= l, 
                         rmse=rmse, 
                         duration=runtime )  
    
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>
  }
  
## call model function    
  result.m52.movie.user.genre.effect.year.one4all <- 
    map2_dfr(params$fold, params$lambda, 
             m52.movie.user.genre.effect.year.one4all)
  
# calculate the optimized lambda
  lambda.mugy <- result.m52.movie.user.genre.effect.year.one4all %>% 
    group_by(lambda) %>% summarize(rmse=mean(rmse))  %>% 
    filter(rmse==min(rmse)) %>% .$lambda
 
## calculate optimized rmse    
  rmse.result.m52.movie.user.genre.effect.year.one4all <- 
    result.m52.movie.user.genre.effect.year.one4all %>% 
    group_by(lambda) %>% summarize(rmse=mean(rmse))  %>% 
    filter(rmse==min(rmse)) %>% .$rmse

## save model result to overall results dataframe   
  duration.tmp = sum(result.m52.movie.user.genre.effect.year.one4all$duration)  
  results <- rbind(results, data.frame(
                  Method="m5.2 movie+user+genre+year effect reg. one4all", 
                  RMSE=rmse.result.m52.movie.user.genre.effect.year.one4all, 
                  reference = "edx CV 5f", 
                  lambda=lambda.mugy,
                  duration=duration.tmp))
  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>  

gc()
  


# quick view on the results
  result.m52.movie.user.genre.effect.year.one4all  %>%   
  ggplot(aes(lambda, rmse, col=fold)) +
    geom_point() +
    geom_vline(xintercept = lambda.mugy  , col="firebrick1") +
    ggtitle('result.m52.movie.user.genre.effect.year.one4all', 
            subtitle = '>> red line = optimized lambda <<') +
    theme_hc()     



## present overall results
  results %>% kable()  
    


#____________________________________________________________________________  
    
# * Final evaluation--------------------------------------------------------

#____________________________________________________________________________


#____________________________________________________________________________  
    
# ** M6 Final Model --------------------------------------------------------

#____________________________________________________________________________

# lambda for final model
  lambda.opt <- lambda.mugl 
  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  edx <- edx %>% 
    semi_join(edx, by = 'movieId') %>% 
    semi_join(edx, by = 'userId')

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  b.movie.f <- edx %>% 
    group_by(movieId) %>% 
    summarize(s.m = sum ( rating - mu), n.m =n())  %>% 
    mutate ( b.m = s.m/(n.m + lambda.opt)) 
  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  b.user.f <- edx %>% 
    left_join(b.movie.f, by='movieId') %>% 
    group_by(userId) %>% 
    summarize(s.u = sum ( rating - mu - b.m), n.u =n()) %>% 
    mutate ( b.u = s.u/(n.u + lambda.opt))   
  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>

  b.genre.f <- edx %>% 
    left_join(b.movie.f, by='movieId') %>% 
    left_join(b.user.f, by='userId') %>%  
    group_by(genres) %>% 
    summarize(s.g = sum ( rating - mu - b.m - b.u ), n.g = n()) %>% 
    mutate ( b.g = s.g/(n.g + lambda.opt)) 
  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
    b.lag.f <- edx %>% 
    left_join(b.movie.f, by='movieId') %>% 
    left_join(b.user.f, by='userId') %>% 
    left_join(b.genre.f, by='genres') %>% 
    group_by(rating_lag) %>% 
    summarize(s.l = sum ( rating - mu - b.m - b.u - b.g), n.l = n()) %>% 
    mutate(b.l = s.l/(n.l+lambda.opt))

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>

gc()    



#____________________________________________________________________________
    
# ** Enhance validation ----------------------------------------------------
#____________________________________________________________________________
    
# Prepare validation-set for final model

  # genres as factor
      validation$genres <- as.factor(validation$genres)   
      
  # convert timestamp to POSIX date.time 
      validation$ts_date <- 
        as.POSIXct(validation$timestamp, origin="1970-01-01") 
      
  # extract movie year from title 
      validation$year <- validation$title %>% str_sub(-5,-2) %>% as.numeric()  
      
  # create 'year of rating'   &   'rating-lag' 
      validation <- validation %>% 
        mutate ( year.rating = as.numeric(isoyear(ts_date)),
                  rating_lag =  year.rating - year,
                  rating_lag = ifelse (rating_lag<0,0,rating_lag)) 
      


#____________________________________________________________________________

# Final prediction ---------------------------------------------------------
#____________________________________________________________________________

# Final prediction
  predicted <-
    validation %>% 
    left_join(b.movie.f, by='movieId') %>% 
    left_join(b.user.f, by='userId') %>% 
    left_join(b.genre.f, by='genres') %>% 
    left_join(b.lag.f, by='rating_lag') %>% 
    mutate (pred = mu + b.m + b.u + b.g + b.l) %>% 
    pull(pred)
  
  rmse.final <- RMSE (validation$rating, predicted)
 
## save model result to overall results dataframe   

  results <- rbind(results, data.frame(
                      Method  ="m6 movie+user+genre+lag effect reg. one4all", 
                      RMSE =  rmse.final,
                      reference = "validation CV 5f", 
                      lambda = lambda.opt,
                      duration = 'na'))



## present overall results
  results %>% kable()  
    
