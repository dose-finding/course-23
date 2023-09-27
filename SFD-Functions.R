########################################
###### This code can be used to implement the 
##### `A Surface-Free Design for Phase I Dual-Agent Combination Trials' 
##### in the case of 3 doses of each agent 
########################################

############## Specifying the Model ##############
########################################
library("rjags")
model1.string <-"
model {
s[1] ~ dbin(p[1], n[1])  
p[1] <- 1-theta[1]
s[2] ~ dbin(p[2], n[2])  
p[2] <- 1-theta[1]*theta[2]
s[3] ~ dbin(p[3], n[3])  
p[3] <- 1-theta[1]*theta[2]*theta[3]
s[4] ~ dbin(p[4], n[4])  
p[4] <- 1-theta[1]*theta[4]
s[5] ~ dbin(p[5], n[5])  
p[5] <- 1-theta[1]*theta[2]*theta[4]
s[6] ~ dbin(p[6], n[6])  
p[6] <- 1-theta[1]*theta[2]*theta[3]*theta[4]
s[7] ~ dbin(p[7], n[7])  
p[7] <- 1-theta[1]*theta[4]*theta[5]
s[8] ~ dbin(p[8], n[8])  
p[8] <- 1-theta[1]*theta[2]*theta[4]*theta[5]
s[9] ~ dbin(p[9], n[9])  
p[9] <- 1-theta[1]*theta[2]*theta[3]*theta[4]*theta[5]

theta[1] ~ dbeta(a[1],b[1])T(0,0.999999)
theta[2] ~ dbeta(a[2],b[2])T(0,0.999999)
theta[3] ~ dbeta(a[3],b[3])T(0,0.999999)
theta[4] ~ dbeta(a[4],b[4])T(0,0.999999)
theta[5] ~ dbeta(a[5],b[5])T(0,0.999999)
}
"
model1.spec<-textConnection(model1.string) # the truncation is needed for the computational purposes

sfd.design.next<-function(datas,datan,target,a.prior,b.prior,current.combo,no.skipping=T,safety=T,c.overdose=0.70,iterations=10000){

model1.spec<-textConnection(model1.string)
mydata <- list(s = datas,n = datan, a=a.prior,b=b.prior)
jags <- jags.model(model1.spec,data =mydata,n.chains=2,n.adapt=iterations,quiet=TRUE)
update(jags, iterations,progress.bar="none")
tt<-jags.samples(jags,c('theta'),iterations,progress.bar="none")
t<-cbind(c(tt$theta[1,,]),c(tt$theta[2,,]),c(tt$theta[3,,]),c(tt$theta[4,,]),c(tt$theta[5,,]))
t.mean<-colMeans(t)
p1<-1-t.mean[1]
p2<-1-t.mean[1]*t.mean[2]
p3<-1-t.mean[1]*t.mean[2]*t.mean[3]
p4<-1-t.mean[1]*t.mean[4]
p5 <- 1-t.mean[1]*t.mean[2]*t.mean[4]
p6 <- 1-t.mean[1]*t.mean[2]*t.mean[3]*t.mean[4]
p7 <- 1-t.mean[1]*t.mean[4]*t.mean[5]
p8 <- 1-t.mean[1]*t.mean[2]*t.mean[4]*t.mean[5]
p9 <- 1-t.mean[1]*t.mean[2]*t.mean[3]*t.mean[4]*t.mean[5]

p.raw<-c(p1,p2,p3,p4,p5,p6,p7,p8,p9)

if(no.skipping){
  if(current.combo==1){
    p3<-p5<-p6<-p7<-p8<-p9<-1}
  if(current.combo==2){
    p6<-p7<-p8<-p9<-1}
  if(current.combo==3){
    p7<-p8<-p9<-1
  }
  if(current.combo==4){
    p6<-p8<-p9<-1}
  if(current.combo==5){
    p9<-1}
  if(current.combo==7){
    p9<-1}
}


if(safety){
  
  if(safety){
    overdose.1<-length(which(1-t[,1]>target))/length(t[,1])
    overdose.2<-length(which(1-t[,1]*t[,2]>target))/length(t[,1])
    overdose.3<-length(which(1-t[,1]*t[,2]*t[,3]>target))/length(t[,1])
    overdose.4<-length(which(1-t[,1]*t[,4]>target))/length(t[,1])
    overdose.5<-length(which(1-t[,1]*t[,2]*t[,4]>target))/length(t[,1])
    overdose.6<-length(which(1-t[,1]*t[,2]*t[,3]*t[,4]>target))/length(t[,1])
    overdose.7<-length(which(1-t[,1]*t[,4]*t[,5]>target))/length(t[,1])
    overdose.8<-length(which(1-t[,1]*t[,2]*t[,4]*t[,5]>target))/length(t[,1])
    overdose.9<-length(which(1-t[,1]*t[,2]*t[,3]*t[,4]*t[,5]>target))/length(t[,1])
    overdose<-c(overdose.1,overdose.2,overdose.3,overdose.4,overdose.5,overdose.6,overdose.7,overdose.8,overdose.9)
    
    if(overdose.1>c.overdose){
      p<-rep(1,9)
    }else{
      if(overdose.2>c.overdose){
        p2<-1
      }
      if(overdose.3>c.overdose){
        p3<-1
      }
      if(overdose.4>c.overdose){
        p4<-1
      } 
      if(overdose.5>c.overdose){
        p5<-1
      } 
      if(overdose.6>c.overdose){
        p6<-1
      } 
      if(overdose.7>c.overdose){
        p7<-1
      }
      if(overdose.8>c.overdose){
        p8<-1
      }
      if(overdose.9>c.overdose){
        p9<-1
      }
    }
  }
  
  
}

p<-c(p1,p2,p3,p4,p5,p6,p7,p8,p9)

if(safety){
  if(all(p==1)){
    stop<-1
    next.combo<-0
  }else{
    stop<-0
    next.combo<-which(abs(p-target)==min(abs(p-target)))
  }
}

output<-list(Combination.Notation=matrix(1:9,3,3,byrow=T),Next.Combo=next.combo,Tox.Est=matrix(round(p.raw,3),3,3,byrow=T),Tox.Est.Constrained=matrix(round(p,3),3,3,byrow=T),Overdose=matrix(round(overdose,3),3,3,byrow=T),Stop=stop)


}