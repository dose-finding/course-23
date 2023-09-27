# install.packages("pocrm")
library("pocrm")

# Design Stage (points (a)--(f))

#Specify the possible orderings 
orders<-matrix(nrow= , # number of ordering
               ncol=) # number of combination/regimens) 
orders[1,]<-c(1, 2, 3, 4, 5, 6)
orders[2,]<-c( )
orders[3,]<-c( )
orders[4,]<-c( )
orders[5,]<-c( )
orders[6,]<-c( )

# Specifying skeleton
skeleton<-c( )

# Generating working model (ordering-specific skeletons)
alpha<-getwm(orders=,skeleton=)

# Prior probability of each ordering
prior.o<-c()

# Initial escalation sceme before any DLTs are observed
x0<-c( )

# Specify scenario
r<-c()

# Run simulations
pocrm.sim(r= , # simulation scenario (as a vector)
          alpha= , # matrix of working model
          prior.o=, # prior probability of each ordering (vector)
          x0= , # initial escalation scheme (vector) 
          stop= , # number of patients at one regimen to stop trial earlier (scalar)
          n= , #maximum number of patients (scalar)
          theta= , # target toxicity level  (scalar)
          nsim= , # number of simulations (scalar)
          tox.range= ) # acceptable toxicity margin (scalar)




######
######
######


# Implementation Stage (point (g))

# Insert combinations tried (1 entry = 1 patients)
combos<-c(1,1,1,2,2,2)
# Insert the corresonding DLT outcomes for each patients
y<-     c(0,0,0,1,0,0)
# Fit CRM
fit<-pocrm.imp(alpha=, # matrix of working model
               prior.o=, # prior probability of each ordering (vector)
               theta= , # target toxicity level  (scalar)
               y= , # DLT data (vector)
               combos=) # combination tried (vector)


