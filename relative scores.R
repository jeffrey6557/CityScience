
library(ggplot2)


setwd('/Users/liuchang/Documents/STUDY/Projects/CityScience/RelativeScores')
yelp_city<-read.csv("yelp_data_CITY.csv")


# extract the ratings by cities
cities<-yelp_city$city

LA_rating<-subset(yelp_city,subset=(city=="Los Angeles"),select='rating')
BO_rating<-subset(yelp_city,subset=(city=="Boston"),select='rating')
NY_rating<-subset(yelp_city,subset=(city=="New York"),select='rating')
LA_rating<-LA_rating$rating


# REQUIRED ANALYSIS 2 - Student t confidence interval:
# Now that we assume each rating is a sample mean of the ratings for each restaurant, 
# and show that they are quite normally distributed; with a large sample of these means,
# we naturally want a student t confidence interval for the whole population in Boston.
T_CI <-  function(rating) {
n<-length(rating) # how many data points do we have?
student<-(rating-mean(rating))/(sd(LA_rating)/sqrt(length(LA_rating)))
hist(student,breaks=seq(-150,150,20),freq=FALSE,main="A t distribution?",xlab="rescaled student variable")
curve(dt(x,n-1),col='red',add=TRUE) 
}

t_CI(LA_rating)# does not fit at all
# We failed! Why?
# This is because our ratings are discrete values, and are not strictly normal variables,
# so we are not justified to use student t confidence interval 
# but for the sake of comparison later, we can bend the theory: 
classic.t<-t.test(LA_rating,conf.level=.99);classic.t 
# a very small interval with so many degrees of freedom!
# we are 99% confident that the true mean ratings is practically 4. Quite large a number!

rating<-LA_rating
#bootstrap_t_CI<-function(rating) {
# with a large sample, the best is to use a bootstrap t confidence interval
xbar <- mean(rating); xbar  #sample mean
S <- sd(rating); S          #sample standard deviation
n <- length(rating);n
SE <- S/(sqrt(n)) ; SE       #sample standard error

#Check our methodology with a single bootstrap resample
x <-sample(rating, size = n, replace = TRUE) #resample
Tstar<-(mean(x) - xbar)/(sd(x)/sqrt(n)); Tstar #a t statistic
#Now we will estimate the distribution of the t statistic

N = 10^4; Tstar = numeric(N) #vector of t statistics
means = numeric(N); StdErrs = numeric(N)
for (i in 1:N) {
  x <-sample(rating, size = n, replace = TRUE)
  Tstar[i] <-(mean(x) - xbar)/(sd(x)/sqrt(n))
  means[i] = mean(x); StdErrs[i] = sd(x)/sqrt(n)
}

#Tstar= bootstrap_t_CI(LA_rating)
#The bootstrap t statistic is approximately normal except for the tails
qqnorm(Tstar, main="Normal quantile plot for bootstrap t statistics")
qqline(Tstar)

# BONUS 6 - new plots
qplot(Tstar, geom = "blank") + 
  geom_histogram(aes(y = ..density..),
                 alpha = 0.4,colour="black",fill="white") + 
  stat_function(fun=dt, args=n-1,
                colour="blue") +
  ggtitle("Bootstrap Distribution of Ratings") + 
  xlab("Bootstrap statistics") + 
  ylab("Density") +
  theme(plot.title = element_text(size=20, face="bold", vjust=2))
#A Student t curve is quite good a match except for the center
#To get a confidence interval, use the bootstrap quantiles along with the sample mean and standard deviation
q<-quantile(Tstar, c(.005, .995),names=FALSE)
L <- xbar - q[2]*SE; U <- xbar - q[1]*SE; L; U
#Here, for comparison, is the bootstrap percentile and the classical t confidence interval
quantile(means, c(.005, .995),names=FALSE)
classic.t$conf.int
# we can display these 3 intervals 
hist(rating, breaks=seq(2.75,5.25,0.5), xlim=c(2.5,5.5),freq=FALSE,
     main="Distribution of sample restaurant ratings in Los Angeles") # the whole sample
abline(v=c(L,U),col='black',lty=3)
abline(v=quantile(means, c(.005, .995),names=FALSE),col='red',lty=4)
abline(v=classic.t$conf.int,col='blue',lty=5)
# these 3 intervals are so close to each other that we may practically use any one
# but theoretically, we are only allowed to use the bootstrap t statistics.
# so we can claim with 99% confidence that the true mean rating of Bostonian restaurants is 4!
# Good job, Boston! 
# Well, now we also have a sense of how inflated the ratings can be.  
