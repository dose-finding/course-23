# install.packages("pocrm")
library("pocrm")


#Specify the possible orderings 
orders<-matrix(nrow=6,ncol=6) 
orders[1,]<-c(1, 2, 3, 4, 5, 6)
orders[2,]<-c(1, 2, 3, 5, 4, 6)
orders[3,]<-c(1, 2, 4, 3, 5, 6)
orders[4,]<-c(1, 2, 4, 5, 3, 6)
orders[5,]<-c(1, 2, 5, 3, 4, 6)
orders[6,]<-c(1, 2, 5, 4, 3, 6)

# Specify Skeleton
skeleton<-c(0.10,0.21,0.24,0.30,0.35,0.40)

# Get skelton under different orderings
alpha<-getwm(orders,skeleton)

# Prior probability of each ordering
prior.o<-rep(1/nrow(orders),nrow(orders))

# Initial escalation scheme
x0<-c(1,2,3,4,5,6)

# Scenarios from Lecture 6
r1.1<-c(0.05,0.10,0.20,0.30,0.45,0.70)
r1.2<-c(0.05,0.10,0.30,0.20,0.45,0.70)
r1.3<-c(0.05,0.10,0.20,0.45,0.30,0.70)

# Running simulation under scenarios
pocrm.sim(r=r1.1, alpha=alpha, prior.o=prior.o, x0=x0, stop=100, n=30, theta=0.30, nsim=2000, tox.range=0.05)
pocrm.sim(r=r1.2, alpha=alpha, prior.o=prior.o, x0=x0, stop=100, n=30, theta=0.30, nsim=2000, tox.range=0.05)
pocrm.sim(r=r1.3, alpha=alpha, prior.o=prior.o, x0=x0, stop=100, n=30, theta=0.30, nsim=2000, tox.range=0.05)

# Checking whether some scenarios are missing (all possible orderings)
getwm(orders,r1.1)


# Additional scenarios to consider
r1.4<-c(0.05,  0.1, 0.45, 0.20, 0.30,  0.7)
r1.5<-c(0.05,  0.1, 0.30, 0.45, 0.20,  0.7)
r1.6<-c(0.05,  0.1, 0.45, 0.30, 0.20,  0.7)

# Additional simulations
pocrm.sim(r=r1.4, alpha=alpha, prior.o=prior.o, x0=x0, stop=100, n=30, theta=0.30, nsim=2000, tox.range=0.05)
pocrm.sim(r=r1.5, alpha=alpha, prior.o=prior.o, x0=x0, stop=100, n=30, theta=0.30, nsim=2000, tox.range=0.05)
pocrm.sim(r=r1.6, alpha=alpha, prior.o=prior.o, x0=x0, stop=100, n=30, theta=0.30, nsim=2000, tox.range=0.05)

# First 3 orderings are more likely
one<-1/(2*3 + 3)
prior.o<-c(2*one,2*one,2*one,one,one,one)
sum(prior.o)

# Run Simulations
pocrm.sim(r=r1.1, alpha=alpha, prior.o=prior.o, x0=x0, stop=100, n=30, theta=0.30, nsim=2000, tox.range=0.05)
pocrm.sim(r=r1.2, alpha=alpha, prior.o=prior.o, x0=x0, stop=100, n=30, theta=0.30, nsim=2000, tox.range=0.05)
pocrm.sim(r=r1.3, alpha=alpha, prior.o=prior.o, x0=x0, stop=100, n=30, theta=0.30, nsim=2000, tox.range=0.05)
pocrm.sim(r=r1.4, alpha=alpha, prior.o=prior.o, x0=x0, stop=100, n=30, theta=0.30, nsim=2000, tox.range=0.05)
pocrm.sim(r=r1.5, alpha=alpha, prior.o=prior.o, x0=x0, stop=100, n=30, theta=0.30, nsim=2000, tox.range=0.05)
pocrm.sim(r=r1.6, alpha=alpha, prior.o=prior.o, x0=x0, stop=100, n=30, theta=0.30, nsim=2000, tox.range=0.05)


# Reduce number of possible orderings
orders<-matrix(nrow=3,ncol=6) 
orders[1,]<-c(1, 2, 3, 4, 5, 6)
orders[2,]<-c(1, 2, 3, 5, 4, 6)
orders[3,]<-c(1, 2, 4, 3, 5, 6)

skeleton<-c(0.10,0.21,0.24,0.30,0.35,0.40)
alpha<-getwm(orders,skeleton)
prior.o<-rep(1/nrow(orders),nrow(orders))

x0<-c(1,2,3,4,5,6)

r1.1<-c(0.05,0.10,0.20,0.30,0.45,0.70)
r1.2<-c(0.05,0.10,0.30,0.20,0.45,0.70)
r1.3<-c(0.05,0.10,0.20,0.45,0.30,0.70)

pocrm.sim(r=r1.1, alpha=alpha, prior.o=prior.o, x0=x0, stop=100, n=30, theta=0.30, nsim=2000, tox.range=0.05)
pocrm.sim(r=r1.2, alpha=alpha, prior.o=prior.o, x0=x0, stop=100, n=30, theta=0.30, nsim=2000, tox.range=0.05)
pocrm.sim(r=r1.3, alpha=alpha, prior.o=prior.o, x0=x0, stop=100, n=30, theta=0.30, nsim=2000, tox.range=0.05)


# Conduct of the study

# Two cohorts

combos<-c(1,1,1,2,2,2)
y<-     c(0,0,0,1,0,0)
fit<-pocrm.imp(alpha=alpha,prior.o=prior.o,theta=0.30,y=y,combos=combos)
fit$ord.prob
fit$order.est
fit$dose.rec
fit$ptox.est

# Three cohorts

combos<-c(1,1,1,2,2,2,2,2,2)
y<-     c(0,0,0,1,0,0,0,0,0)
fit<-pocrm.imp(alpha=alpha,prior.o=prior.o,theta=0.30,y=y,combos=combos)
fit$ord.prob
fit$order.est
fit$ptox.est
fit$dose.rec

# Four cohorts
combos<-c(1,1,1,2,2,2,2,2,2,3,3,3)
y<-     c(0,0,0,1,0,0,0,0,0,1,0,0)
fit<-pocrm.imp(alpha=alpha,prior.o=prior.o,theta=0.30,y=y,combos=combos)
fit$ord.prob
fit$order.est
fit$ptox.est
fit$dose.rec

# Five cohorts

combos<-c(1,1,1,2,2,2,2,2,2,3,3,3,4,4,4)
y<-     c(0,0,0,1,0,0,0,0,0,1,0,0,0,0,0)
fit<-pocrm.imp(alpha=alpha,prior.o=prior.o,theta=0.30,y=y,combos=combos)
fit$ord.prob
fit$ptox.est
fit$dose.rec
