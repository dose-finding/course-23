## Practical with crmPack - template to start from

## load the package:
library(crmPack)

## define the dose grid: please insert the correct numbers
# Point 1.
doseGrid <- seq(from=1, to=150, by=1)

## get the model and prior for the parameters:
## please insert the correct numbers into the c(,) notation 
## - have a look at the help page for Quantiles2LogisticNormal
set.seed(92)
## let this next command run while you edit further below - 
## it may take a few minutes...
res <- Quantiles2LogisticNormal(dosegrid=c(10,100),
                                refDose=1,
                                lower=c(0.05,0.15),
                                median=c(0.10,0.40),
                                upper=c(0.40,0.80),
                                logNormal=TRUE,
                                control=list(max.time=10))

## look at the result:
res

## get the model
model <- res$model

## generate samples from the prior and plot it:
emptyData <- Data(doseGrid=doseGrid)
mcmcOptions <- McmcOptions()
set.seed(12)
## look at the crmPack slides code to complete the following two lines:
priorSamples<-mcmc(emptyData,model,mcmcOptions)
plot(priorSamples,model, emptyData)## how does it look? ok?

# Point 2.
## NCRM rule: please complete with the correct numbers
ncrm <- NextBestNCRM(target = c(0.20,0.35),
                     overdose = c(0.35,1),
                     maxOverdoseProb =0.25)

# Point 3.
## cohort size: please insert the correct vectors
## have a look at the help pages for CohortSizeDLT
## and CohortSizeRange
cohortSize1 <- CohortSizeDLT(DLTintervals= c(0,1),
                             cohortSize= c(1,3))
cohortSize2 <- CohortSizeRange(intervals= c(0,50),
                               cohortSize= c(1,3))
cohortSize <- maxSize(cohortSize1, cohortSize2)

# Point 4.
## stopping rule construction:
## please complete with the right numbers
stop1 <- StoppingPatientsNearDose(nPatients =6, percentage =0.1)
stop2 <- StoppingTargetProb(target=c(0.2,0.35), prob=0.4)
stop3 <- StoppingMinPatients(nPatients =10)
stop4 <- StoppingMinPatients(nPatients =45)
stopRule <- (stop1 & stop2 & stop3) | stop4

# Point 5.
## specify maximum increments
## please complete with the right vectors
increments <- IncrementsRelative(intervals= c(0,75),
                                 increments= c(1,0.50))

# Point 6.
## bundle everything together in the Design object:
## please complete
design <- Design(model=model ,
                 stopping=stopRule ,
                 increments=increments ,
                 nextBest=ncrm ,
                 cohortSize=cohortSize ,
                 data=emptyData ,
                 startingDose=10 )

## evaluate single trial behavior first!!
ex <- examine(design, 
              mcmcOptions=
                  McmcOptions(burnin=20000,
                             step=3,
                             samples=50000))
ex  
## how many cohorts at least until max. dose is reached? # after 15 cohorts given no DLT before CRM recommends 150mg

## define scenarios:

## first look at the prior means 
## (for the intercept alpha0 and for the log slope log(alpha1)): 
model@mean
exp(model@mean[2])


# Point 7.
## this one shall be similar to the prior assumptions:
## complete with the right numbers 
## taken from the two previous lines
scen1 <- function(dose)
{
    model@prob(dose, alpha0=model@mean[1] , alpha1=exp(model@mean[2]))
}

## this one shall be more toxic:
## choose higher numbers here for the intercept and slope
## prior means, in order to make it more toxic
scen2 <- function(dose)
{
  model@prob(dose, alpha0=model@mean[1] , alpha1=1.5*exp(model@mean[2]) )  # steeper
}

## and this one shall be less toxic:
## choose lower numbers here for the intercept and slope
## prior means, in orderto make it less toxic
scen3 <- function(dose)
{
  model@prob(dose, alpha0=model@mean[1]  , alpha1=0.75*exp(model@mean[2]) ) #flatter
}

## try it out:
curve(scen1(dose),from=1, to=150, xname="dose", ylim=c(0, 1))
curve(scen2(dose),from=1, to=150, xname="dose", add=TRUE, col="red")
curve(scen3(dose),from=1, to=150, xname="dose", add=TRUE, col="blue")

## check if intended ordering of scenarios is kept

## add the fit from the empty data => prior model:
lines(fit(priorSamples,model,emptyData),col="green")

## does the green line stay close to the black line?

## run simulations:
## please complete
sims1 <- simulate(design, 
                  nsim=20 ,
                  seed=12 ,
                  truth=scen1 ,                  
                  mcmcOptions=mcmcOptions,
                  parallel=TRUE)

sims2 <- simulate(design, 
                  nsim=20 ,
                  seed=12 ,
                  truth=scen2 ,                  
                  mcmcOptions=mcmcOptions,
                  parallel=TRUE)

sims3 <- simulate(design, 
                  nsim=20 ,
                  seed=12 ,
                  truth=scen3 ,                  
                  mcmcOptions=mcmcOptions,
                  parallel=TRUE)

## numerical summaries
summary(sims1,scen1,target=ncrm@target)
summary(sims2,scen2,target=ncrm@target)
summary(sims3,scen3,target=ncrm@target)


# Point 8.
## compare with 3+3 design:
## not clear which doses to pick -
## all of them will be very slow ->
## take 1, 10 (starting dose), the maximum 150,
## and four more in between respecting the max. increment
## please insert the doseGrid vector accordingly:
emptydata2 <- Data(doseGrid = c(1, 10, 20,40,75,112,150))
three <- RuleDesign(nextBest=NextBestThreePlusThree(),
                    data=emptydata2,
                    cohortSize=CohortSizeConst(size=3),
                    startingDose=10)
## need more code because starting dose shall not be 
## equal to the lowest dose 
## (may be improved in a next version of crmPack...)

## single trial behavior
examine(three)

## simulations:
simThree1 <-simulate(three,nsim=100,seed=20,truth=scen1)
simThree2 <-simulate(three,nsim=100,seed=20,truth=scen2)
simThree3 <-simulate(three,nsim=100,seed=20,truth=scen3)

## summary statistics
## please comment
summary(simThree1,scen1,target=ncrm@target)

summary(simThree2,scen2,target=ncrm@target)

summary(simThree3,scen3,target=ncrm@target)

