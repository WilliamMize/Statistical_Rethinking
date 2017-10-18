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

#define a grid, seq of numbers 0 to 1 
p_grid <- seq(from =0, to = 1, length.out = 20)
#defin the prior
prior <- rep(1,20)
#compute the likelihood at each value in grid
likelihood <- dbinom(x=3,size=3,prob=p_grid)
#unstd.posterior 
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior/sum(unstd.posterior)
print(posterior)
plot(p_grid,posterior)













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

##Solve for 2m1 1 w,w,w
count_2m1_w <- 3
total_count_2m1 <- 3

plot_posterior(p_grid,compute_posterior(count_w = count_2m1_w,total_count = total_count_2m1, prior = prior ))

##solve for 2m1 2 (2) W, W, W, L
count_2m2_w <- 3
total_count_2m2 <-4

plot_posterior(p_grid,compute_posterior(count_w = count_2m2_w,total_count = total_count_2m2,prior = prior))

##solve for 2m1 3  L, W, W, L, W, W, W
count_2m3_w <- 5
total_count_2m3 <- 7

plot_posterior(p_grid,compute_posterior(count_w = count_2m3_w,total_count = total_count_2m3,prior = prior))

##solve for 2m2 
prior <- ifelse( test = p_grid < 0.5, yes =0, no =1)
count_2m1_w <- 3
total_count_2m1 <- 3
plot_posterior(p_grid,compute_posterior(count_w = count_2m1_w,total_count = total_count_2m1, prior = prior ))

prior <- ifelse( test = p_grid < 0.5, yes =0, no =1)
count_2m2_w <- 3
total_count_2m2 <-4

plot_posterior(p_grid,compute_posterior(count_w = count_2m2_w,total_count = total_count_2m2,prior = prior))

prior <- ifelse( test = p_grid < 0.5, yes =0, no =1)
count_2m3_w <- 5
total_count_2m3 <- 7

plot_posterior(p_grid,compute_posterior(count_w = count_2m3_w,total_count = total_count_2m3,prior = prior))

##solve for 2m3
##posterior = likelihood X prior / sum of all(likelihood X prior)

likelihood_2m3 <- c(0.3,1)

#prior because it is the step before, like in the marble example

prior_2m3 <- c(0.5,0.5)

unstandardized.posterior_2m3 <- likelihood_2m3*prior_2m3

posterior_2m3 <- unstandardized.posterior/sum(unstandardized.posterior_2m3)

#both posteriors are in here for each one, so select first element since it is earth
print(posterior_2m3[1])


#2m4


## 2M5
card.1.likelihood <- 2
card.2.likelihood <- 1
card.3.likelihood <- 0
card.4.likelihood <- 2
likelihood <- c(card.1.likelihood, card.2.likelihood, card.3.likelihood, card.4.likelihood)
prior <- rep(x = 1, length = length(likelihood))
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# the probability the other size is black is equal to the probability that we've drawn card 1 or 4
posterior[1] + posterior[4]


## 2M6
card.1.likelihood <- 2
card.2.likelihood <- 1
card.3.likelihood <- 0
likelihood <- c(card.1.likelihood,card.2.likelihood,card.3.likelihood)
prior <- c(1,2,3)
unstandardized.posterior <- prior*likelihood
posterior <- unstandardized.posterior/sum(unstandardized.posterior)
print(posterior[1])


##2h1
prob.twins.species1 <- .1
prob.twins.species2 <- .2
likelihood <- c(prob.twins.species1,prob.twins.species2)
prior <- c(1,1)
unstandardized.posterior <- likelihood*prior
posterior <- unstandardized.posterior*prior/sum(unstandardized.posterior*prior)
new_posterior <- likelihood*posterior/sum(posterior)
print(sum(new_posterior))
##2h2

prob.twins.species1 <- .1
prob.twins.species2 <- .2
likelihood <- c(prob.twins.species1,prob.twins.species2)
prior <- c(1,1)
unstandardized.posterior <- likelihood*prior
posterior <- unstandardized.posterior*prior/sum(unstandardized.posterior*prior)
print(posterior[1])

##2h3

prob.twins.species1 <- .1
prob.twins.species2 <- .2
likelihood <- c(prob.twins.species1,prob.twins.species2)
prior <- c(1,1)
unstandardized.posterior <- likelihood*prior
posterior <- unstandardized.posterior*prior/sum(unstandardized.posterior*prior)
print(posterior[1])
#use the same 2h2 as above then new single baby
prob1.single.species1 <- .9
prob2.single.species2 <- .8
likelihood.single <- c(prob1.single.species1,prob2.single.species2)
new_post <- posterior*likelihood.single/sum(posterior*likelihood.single)

print(new_post[1])
#2h4
prob.specA <- .8
## 1 - specic B panda .65 because we are wanting to know probability that it is species A
prob.specB <- 1 - .65
likelihood <- c(prob.specA,prob.specB)
prior <- c(1,1)
unstandardized.posterior <- likelihood*prior
posterior <- unstandardized.posterior/sum(unstandardized.posterior)
print(posterior[1])

##then do 2h4.2 do 2h3 again
prob.twins.species1 <- .1
prob.twins.species2 <- .2
likelihood <- c(prob.twins.species1,prob.twins.species2)
prior <- c(1,1)
unstandardized.posterior <- likelihood*prior
posterior <- unstandardized.posterior*prior/sum(unstandardized.posterior*prior)
print(posterior[1])
#use the same 2h2 as above then new single baby
prob1.single.species1 <- .9
prob2.single.species2 <- .8
likelihood.single <- c(prob1.single.species1,prob2.single.species2)
twin.single.new_post <- posterior*likelihood.single/sum(posterior*likelihood.single)
###test posterior find, 
prob.specA <- .8
## 1 - specic B panda .65 because we are wanting to know probability that it is species A
prob.specB <- 1 - .65
likelihood <- c(prob.specA,prob.specB)
prior <- c(1,1)
unstandardized.posterior <- likelihood*prior
cow.test.posterior <- unstandardized.posterior/sum(unstandardized.posterior)


unstandardized.composit_post <- cow.test.posterior * twin.single.new_post
composit_post <- unstandardized.composit_post/sum(unstandardized.composit_post)
print(composit_post[1])
##0.5625


