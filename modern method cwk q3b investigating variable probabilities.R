##Program for variable probability
assets<-250000
p<-0.05 # initial probability that a customer makes a claim
n<-1000 # no of customers
z<-6000 # premium
w<-0
#define parento distribution parameters alpha (a) and beta (B)
a<-3
b<-100000 #parento parameters

#success or failure implies that x is a binomial function
### set the number of simulations
N=10000
#set the loop for the variable probability of claim
for(j in 0:20)
{
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
  #count the number of times the insurance company will go bankrupt
  count=0
  for (i in 1:N)
  {
    if (q[i]<0) {
      count = count + 1
    }
  }
  # probability of bankruptcy
  pbankrupt[j]<-count/N
  #Print the probability of bankruptcy with the corresponding probability of making a claim
  print(p)
  print(count/N)
  #reassign next value of the probability p
  p<-p+0.005
}
