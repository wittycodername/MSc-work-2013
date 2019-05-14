"""Write a function to simulate the assets of the company at year end. From your simulation, find the expected assets at year end and the probability that the company goes bankrupt. 
From the simulation below, the expected end-of-year assets are £1252331. The probability of bankruptcy is 0.0987. These figures were calculated with a value of N=100000.

##Below is the function that will simulate the expected assets at the end of year 1.
"""

assets<-250000
p<-0.1 # probability that a customer makes a claim
n<-1000 # no of customers
z<-6000 # premium
w<-0
a<-3
b<-100000 #parento parameters
#success or failure implies that x is a binomial function
### set the number of simulations
N=100000
#set the loop
for(i in 1:N)
  
{
  x=rbinom(1, n, p)
  #x is the number of claims made in a year
  
  U=runif(x)
  
  claim<-(((b^a)/(1-U))^(1/a)-b)
  
  claim
  d<-sum(claim)
  
  w[i]<-(assets+n*z-d)
}
#define q as a vector containing the results of the simulation
q<-c(w[1:N])
expectation<-sum(q)/N
expectation
#count the number of times the insurance company will go bankrupt
count=0
for (i in 1:N)
{
  if (q[i]<0) {
    count = count + 1
  }
}

#number of bankruptcies
print(count)

# probability of bankruptcy
pbankrupt<-count/N
pbankrupt

