# Input the Mean Prior Point Estimates that you have computed
# (vector of 5 connections with the first 3 corresponds to Agent A and the last 2 to Agent B)
t.prior<-(x,x,x,x,x)


# Defining the Strength of Prior
c.prior<-c(x,x,x,x,x) #  Defining the Strength of Prior
a.prior<-xxx # Finding the first shape parameters of Beta distribution
b.prior<-xxx # Finding the second shape parameters of Beta distribution

# The data input in a vector with the combination number as below 
# and in the exercise sheet

combination.index<-c(1,2,3,4,5,6,7,8,9)


################
# Points (a) -- (d) - Individual trial behaviour
################


# Number of patients assigned to each combination (vector of 9)
data.n<-c(0,0,0,0,0,0,0,0,0)
# Number of patients experienced DLTs at each combination (vector of 9)
data.dlt<-c(0,0,0,0,0,0,0,0,0)



design<-sfd.design.next(datas= , # vector of number of DLTs at each combination
                        datan=data.n, # vector of number of patients at each combination
                        target= , # target toxicity level (scalar)
                        a.prior= , # First shape parameter for the prior distribution (vector of length 5)
                        b.prior=b.prior,  # Second shape parameter for the prior distribution (vector of length 5)
                        current.combo= , # Starting/Current combination
                        no.skipping= , # Apply no dose skipping and no diagonal escalations? (T/F)
                        safety=, # Apply safety constraint (T/F)
                        c.overdose= # If safety=T, the value of the overdosing constant (scalar)
                        )

#Output
design$Combination.Notation # Notations for each combination
design$Next.Combo # The next recommended combination (taking escalation constraint into account)
design$Tox.Est # The toxicity estimates at each combination (matrix form)
design$Tox.Est.Constrained # The toxicity estimates with inadmissible combination having toxicity risk of 1
design$Overdose # The overdosing probability
design$Stop # The indicator whether the trial is recommended to be stopped




################
# Point (e) - Simulations
################

cohort<- # cohort size
n<-  # number of cohorts 
nsims<- # number of simulations
start.combo<- # starting combination 
safety<- # whether to apply the safety constraint in the simulation study

  # The vector of the true toxicity risks at each combination (length of 9)
true<-c(0.xxx,0.xxx,0.xxx,
        0.xxx,0.xxx,0.xxx,
        0.xxx,0.xxx,0.xxx)

# Skeleton code for a single trial

next.combo<-start.combo
data.n<-c(0,0,0,0,0,0,0,0,0)
data.dlt<-c(0,0,0,0,0,0,0,0,0)


for (i in  ){ # complete loop in the number of cohorts
  
  current.combo<-next.combo
  
  # Generate toxicity outcomes here
  # (Hint: use rbinom() function)
  
  # Update your vector data.n here
  # Update your vector data.dlt (with the generated toxicity outcomes)
  
  # Note that only the "current.combo" cell of the data vector changes
  
  design<-sfd.design.next(datas= , # vector of number of DLTs at each combination
                          datan=data.n, # vector of number of patients at each combination
                          target= , # target toxicity level (scalar)
                          a.prior= , # First shape parameter for the prior distribution (vector of length 5)
                          b.prior=b.prior,  # Second shape parameter for the prior distribution (vector of length 5)
                          current.combo= , # Starting/Current combination
                          no.skipping= , # Apply no dose skipping and no diagonal escalations? (T/F)
                          safety=, # Apply safety constraint (T/F)
                          c.overdose=, # If safety=T, the value of the overdosing constant (scalar)
                          iterations=, # number of MCMC samples - the default is 10000 but you might want to use fewer for a simulation study
                          )
  
  
  # check whether the study is recommended to stop
  # If yes, stop the study
  # If no, continue the study

  
  
}


# what is the final recommended combination in this one trial?

# When finished with the single trial, wrap the above code with the loop over several simulated trials









