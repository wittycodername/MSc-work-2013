"""Let X be the size of a typical claim as described above.

a)	Calculate the cumulative distribution function F(x) of X, the expectation E(X) and the variance var(X). State any conditions on α and/or β that need to be satisfied. 

See Attached 

b)	Describe a method to simulate from X.

See Attached 


Now assume α = 3, β = 100,000. These values will be fixed for the remainder of the assignment, unless you choose to alter them.

c) Provide code in R to simulate 1000 values drawn from X. You may only use basic R commands, not any preset routines to simulate from a Pareto distribution. Produce a histogram of these values, with the true density function superimposed. 
"""

Nsim=10^3
#number of random numbers

a<-3
b<-100000


U=runif(Nsim)
x=((b^a)/(1-U))^(1/a)-b

#plot the histogram
hist(x,freq=FALSE,col="grey",main="Parento from Uniform")


#Now plt the curve on top

curve((a*b^a)/((x+b)^(a+1)), col = 2, add = TRUE)
