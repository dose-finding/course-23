# Computing the Mean Prior Point Estimates
pA<-c(0.05,0.10,0.20)
pB<-c(0.10,0.20,0.30)

# Function to Compute Mean Prior Point Estimate of Connections Using the Monotherapy Data
compute.prior.means.SFD<-function(p1,p2){
  t.prior<-mat.or.vec(length(p1)+length(p2)-1,1)
  t.prior[1]<-1-p1[1]-p2[1]+p1[1]*p2[1]
  for (i in 2:(length(p1))){
    t.prior[i]<-(1-p1[i])/(1-p1[i-1])
  }
  for (i in 1:(length(p2)-1)){
    t.prior[length(p2)+i]<-(1-p2[i+1])/(1-p2[i])
  }
  return(t.prior)
}

# Mean estimates
t.prior<-compute.prior.means.SFD(p1=pB,p2=pA)


# Defining the Strength of Prior
c.prior<-rep(4,5)   # strength of Beta prior distributions on the connections
a.prior<-t.prior*c.prior # Finding the first parameters of Beta distribution
b.prior<-(1-t.prior)*c.prior # Finding the second parameters of Beta distribution

#Defining index for combination 
combination.index<-c(1,2,3,4,5,6,7,8,9)


# Vectors to store the results
doses.exp<-mat.or.vec(1,9)
doses.tox<-mat.or.vec(1,9)
datan<-c(doses.exp)
datas<-c(doses.tox)


# Running SFD
design<-sfd.design.next(datas=datas,datan=datan,
                        target=0.30,
                        a.prior=a.prior,b.prior=b.prior,
                        current.combo=1,
                        no.skipping=T,safety=T,
                        c.overdose=0.70)


# 0/3 on the first combination


doses.exp<-c(3,0,0,0,0,0,0,0,0)
doses.tox<-c(0,0,0,0,0,0,0,0,0)

datan<-c(doses.exp)
datas<-c(doses.tox)

design<-sfd.design.next(datas=datas,datan=datan,
                        target=0.30,
                        a.prior=a.prior,b.prior=b.prior,
                        current.combo=1,
                        no.skipping=T,safety=T,
                        iterations=10^4,
                        c.overdose=0.70)
design$Next.Combo
design$Tox.Est


# 0/3 followed by 0/3
doses.exp<-c(3,3,0,0,0,0,0,0,0)
doses.tox<-c(0,0,0,0,0,0,0,0,0)

datan<-c(doses.exp)
datas<-c(doses.tox)

design<-sfd.design.next(datas=datas,datan=datan,
                        target=0.30,
                        a.prior=a.prior,b.prior=b.prior,
                        current.combo=2,
                        no.skipping=T,safety=T,
                        iterations=10^4,
                        c.overdose=0.70)
design$Next.Combo
design$Tox.Est


# 0/3 followed by 0/3 followed by 0.3

doses.exp<-c(3,3,3,0,0,0,0,0,0)
doses.tox<-c(0,0,0,0,0,0,0,0,0)

datan<-c(doses.exp)
datas<-c(doses.tox)

design<-sfd.design.next(datas=datas,datan=datan,
                        target=0.30,
                        a.prior=a.prior,b.prior=b.prior,
                        current.combo=3,
                        no.skipping=T,safety=T,
                        iterations=10^4,
                        c.overdose=0.70)
design$Next.Combo
design$Tox.Est


# Followed by 2/3

doses.exp<-c(3,3,3,0,0,3,0,0,0)
doses.tox<-c(0,0,0,0,0,2,0,0,0)

datan<-c(doses.exp)
datas<-c(doses.tox)

design<-sfd.design.next(datas=datas,datan=datan,
                        target=0.30,
                        a.prior=a.prior,b.prior=b.prior,
                        current.combo=6,
                        no.skipping=T,safety=T,
                        iterations=10^4,
                        c.overdose=0.70)
design$Next.Combo
design$Tox.Est


# Followed by 3/3

doses.exp<-c(3,3,3,0,0,3,0,3,0)
doses.tox<-c(0,0,0,0,0,2,0,3,0)

datan<-c(doses.exp)
datas<-c(doses.tox)

design<-sfd.design.next(datas=datas,datan=datan,
                        target=0.30,
                        a.prior=a.prior,b.prior=b.prior,
                        current.combo=8,
                        no.skipping=T,safety=T,
                        iterations=10^4,
                        c.overdose=0.70)
design$Next.Combo
design$Tox.Est
design$Overdose


# Running Simulations

# Cohort size
cohort<-3
n<- 12 # number of cohorts
nsims<-100 # number of simulations
start.combo<-1 # starting combo
safety<-T # apply safety constraint?

#True combination-toxicity scenario
true<-c(0.020,0.050,0.120,
        0.100,0.200,0.300,
        0.150,0.300,0.500)

# Creating matrices to store the results

result.recommendation<-result.experiment<-mat.or.vec(nsims,length(true))
result.toxicity<-mat.or.vec(nsims,1)
experiment<-array(0,dim=c(n,9,nsims))
counter<-0

### Running the Design

for (z in 1:nsims){
  doses.exp<-mat.or.vec(1,length(true))
  doses.tox<-mat.or.vec(1,length(true))
  next.combo<-start.combo
  
  
  for (i in 1:n){
    current.combo<-next.combo
    
    toxicities<-sum(rbinom(cohort,1,true[current.combo]))
    doses.exp[current.combo]<-doses.exp[current.combo]+cohort
    doses.tox[current.combo]<-doses.tox[current.combo]+toxicities
    datan<-c(doses.exp)
    datas<-c(doses.tox)
    
    
    
    design<-sfd.design.next(datas=datas,datan=datan,
                            target=0.30,a.prior=a.prior,b.prior=b.prior,
                            current.combo=current.combo,
                            no.skipping=T,safety=T,
                            c.overdose=0.70)
    
    
    
    if(design$Stop==1){
      break()
    }else{
      
      next.combo<-design$Next.Combo
      
    }
    
    
  }
  
  next.combo
  
  
  
  if(safety){
    if(design$Stop==0){
      result.recommendation[z,next.combo]<-1 
    }else{
      result.recommendation[z,next.combo]<-0
      counter<-counter+1
    }
  }else{
    result.recommendation[z,next.combo]<-1 
  }
  
  result.experiment[z,]<-datan
  result.toxicity[z]<-sum(datas)
  cat(z,"out of",nsims,"\n")
  cat("The current proportion of correct selections is",(colSums(result.recommendation)[6]+colSums(result.recommendation)[8])/z,"\n")
}

# Proportion of each combination recommendation
colMeans(result.recommendation)
