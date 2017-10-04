# define grid
p_grid <- seq(from = 0, to =1, length.out = 20)

# define prior
prior <- rep(1,20)

# compute the likelihood at each value in grid
likelihood <- dbinom(6 , size = 9,prob = p_grid)

# compute the product of likelihood and prior
unstd.posterior <- likelihood * prior

# standarize the posterior, so it sums to 1
posterior <- unstd.posterior/ sum(unstd.posterior)

plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )

# define grid
p_grid <- seq(from = 0, to =1, length.out = 5)

# define prior
prior <- rep(1,5)

# compute the likelihood at each value in grid
likelihood <- dbinom(6 , size = 9,prob = p_grid)

# compute the product of likelihood and prior
unstd.posterior <- likelihood * prior

# standarize the posterior, so it sums to 1
posterior <- unstd.posterior/ sum(unstd.posterior)

plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "5 points" )




##############Done with straight line prior

# define grid
p_grid <- seq(from = 0, to =1, length.out = 20)
# define prior

#basically p_grid is a sequence of numbers 0-1 for 20 length, 
prior <- ifelse(p_grid < 0.5,0,1)
print(prior)

# compute the likelihood at each value in grid
likelihood <- dbinom(6 , size = 9,prob = p_grid)

# compute the product of likelihood and prior
unstd.posterior <- likelihood * prior

# standarize the posterior, so it sums to 1
posterior <- unstd.posterior/ sum(unstd.posterior)

plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )



##############Done with straight line prior

# define grid
p_grid <- seq(from = 0, to =1, length.out = 20)
# define prior


#basically p_grid is a sequence of numbers 0-1 for 20 length, 
prior <- exp( -5*abs( p_grid - 0.5 ) )
print(prior)

# compute the likelihood at each value in grid
likelihood <- dbinom(6 , size = 9,prob = p_grid)

# compute the product of likelihood and prior
unstd.posterior <- likelihood * prior

# standarize the posterior, so it sums to 1
posterior <- unstd.posterior/ sum(unstd.posterior)

plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )


#R code 2.6
library(rethinking)

globe.qa <- map(
    alist(
      w ~ dbinom(9,p), #Binomial likelihood
      p ~ dunif(0,1) #uniform prior
    ) ,
    data=list(w=6)
)

#display summary of quadratic approximation
precis(globe.qa)



# analytical calculation
w <- 6
n <- 9
curve( dbeta( x , w+1 , n-w+1 ) , from=0 , to=1 )
# quadratic approximation
curve( dnorm( x , 0.67 , 0.16 ) , lty=2 , add=TRUE )


##### Home Work 2m)
#First define the grid equi distance 0 to 1
p_grid <- seq( from=0, to =1, length.out=20)

#define prior a bunch of 1's in a vector 20 long
prior <- rep(1,20)
print(prior)

# compute likeliehood at each value in the grid
likelihood <- dbinom(3, size = 3 ,prob = p_grid)

#compute product of likelihood and prior
unstd.posterior <- likelihood * prior

#standardize the posterior so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid,posterior,type = "b",xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )


## Other Guy's solution, we had the same http://www.rpubs.com/andersgs/my_solutions_chapter2_statrethink
count_w <- 3 # observed 3 water toss's
total_count <- 3 #total number of tosses
possible_p <- seq( from=0, to =1, length.out=100) #Grid of 20 check, he did 100
prior_p <- rep(1,100)

plot(possible_p,prior_p,type = "b",xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )

#create function to compute likelihood from variables
likelihood_p <- function(p,count,size){
  return(dbinom(x = count,size = size,prob = p))
}
# likelihood of p given the data
# the posterior is:
# P(p|n,w) = P(w|n,p)p(p)/(p(w))
unstd.posterior_1 <- likelihood_p(possible_p,count = count_w,size = total_count) * prior_p

posterior_1 <- unstd.posterior_1/sum(unstd.posterior_1)

plot(possible_p,posterior_1,type = "b",xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )





########################other other guys https://github.com/cavaunpeu/statistical-rethinking/blob/master/chapter-2/homework.R


## 2M1

p_grid <- seq(from = 0, to = 1, length.out = 20)
prior <- rep(x = 1, length = length(p_grid))

compute_posterior <- function(w, n, prior, p = p_grid) {
  likelihood <- dbinom(x = w, size = n, prob = p)
  unstandardized.posterior <- likelihood * prior
  return( unstandardized.posterior / sum(unstandardized.posterior) )
}

plot_posterior <- function(x, y) {
  plot(x = x, y = y, type="b", xlab = "Probability of Water", ylab = "Posterior Probability")
  title <- paste( length(x), "Points")
  mtext(title)
}

# (1)
w <- 6
n <-  9
posterior <- compute_posterior(w = w, n = n, prior = prior)
plot_posterior(x = p_grid, y = posterior)


### My Try at 3rd guy

p_grid <- seq(from = 0, to = 1, length.out = 100)
prior <- rep(1,length = length(p_grid))

compute_posterior <- function(count_w,total_count,prior, p = p_grid){
  likelihood <- dbinom(x = count_w,size = total_count, prob = p)
  unstanderized.posterior <- likelihood * prior
  standerized.posterior <- unstanderized.posterior/sum(unstanderized.posterior)
  return(standerized.posterior)
}

plot_posterior <- function(x,y){
  plot(x = x, y = y, type = "b", xlab = "Probability of water", ylab = "Posterior Probability")
  title <- paste(length(x), "Points")
  mtext(title)
} 

##Solve for 2m1
count_2m1_w <- 3
total_count_2m1 <- 3

plot_posterior(p_grid,compute_posterior(count_w = count_2m1_w,total_count = total_count_2m1, prior = prior ))

##solve for 2m2
count_2m2_w <- 3
totalA_count_2m2 <-4

plot_posterior(p_grid,compute_posterior(count_w = count_2m2_w,total_count = totalA_count_2m2,prior = prior))
