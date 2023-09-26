## Practical with crmPack - template to start from

## load the package:
library(crmPack)

## define the dose grid: please insert the correct numbers
doseGrid <- seq(from=  , to=  , by=  )

## get the model and prior for the parameters:
## please insert the correct numbers into the c(,) notation 
## - have a look at the help page for Quantiles2LogisticNormal
set.seed(92)
## let this next command run while you edit further below - 
## it may take a few minutes...
res <- Quantiles2LogisticNormal(dosegrid=c( , ),
                                refDose=1,
                                lower=c( , ),
                                median=c( , ),
                                upper=c( , ),
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
priorSamples <- 
plot(  )
## how does it look? ok?

## NCRM rule: please complete with the correct numbers
ncrm <- NextBestNCRM(target = c( , ),
                     overdose = c( , ),
                     maxOverdoseProb = )

## cohort size: please insert the correct vectors
## have a look at the help pages for CohortSizeDLT
## and CohortSizeRange
cohortSize1 <- CohortSizeDLT(DLTintervals= c( , ),
                             cohortSize= c( , ))
cohortSize2 <- CohortSizeRange(intervals= c( , ),
                               cohortSize= c( , ))
cohortSize <- maxSize(cohortSize1, cohortSize2)

## stopping rule construction:
## please complete with the right numbers
stop1 <- StoppingPatientsNearDose(nPatients = , percentage = )
stop2 <- StoppingTargetProb(target=c( , ), prob= )
stop3 <- StoppingMinPatients(nPatients = )
stop4 <- StoppingMinPatients(nPatients = )
stopRule <- (stop1 & stop2 & stop3) | stop4

## specify maximum increments
## please complete with the right vectors
increments <- IncrementsRelative(intervals= c( , ),
                                 increments= c( , ))

## bundle everything together in the Design object:
## please complete
design <- Design(model= ,
                 stopping= ,
                 increments= ,
                 nextBest= ,
                 cohortSize= ,
                 data= ,
                 startingDose= )

## evaluate single trial behavior first!!
ex <- examine(design, 
              mcmcOptions=
                  McmcOptions(burnin=20000,
                             step=3,
                             samples=50000))
ex  
## how many cohorts at least until max. dose is reached?

## define scenarios:

## first look at the prior means 
## (for the intercept alpha0 and for the log slope log(alpha1)): 
model@mean
exp(model@mean[2])

## this one shall be similar to the prior assumptions:
## complete with the right numbers 
## taken from the two previous lines
scen1 <- function(dose)
{
    model@prob(dose, alpha0= , alpha1= )
}

## this one shall be more toxic:
## choose higher numbers here for the intercept and slope
## prior means, in order to make it more toxic
scen2 <- function(dose)
{
  model@prob(dose, alpha0= , alpha1= )
}

## and this one shall be less toxic:
## choose lower numbers here for the intercept and slope
## prior means, in orderto make it less toxic
scen3 <- function(dose)
{
  model@prob(dose, alpha0= , alpha1= )
}

## try it out:
curve(scen1(dose),
      from=1, to=150, xname="dose", ylim=c(0, 1))
curve(scen2(dose),
      from=1, to=150, xname="dose", 
      add=TRUE, col="red")
curve(scen3(dose),
      from=1, to=150, xname="dose",
      add=TRUE, col="blue")
## check if intended ordering of scenarios is kept

## add the fit from the empty data => prior model:
lines(fit(priorSamples,
          model,
          emptyData),
      col="green")
## does the green line stay close to the black line?

## run simulations:
## please complete
sims1 <- simulate(design, 
                  nsim= ,
                  seed= ,
                  truth= ,                  
                  mcmcOptions=mcmcOptions,
                  parallel=TRUE)

sims2 <- simulate(design, 
                  nsim= ,
                  seed= ,
                  truth= ,                  
                  mcmcOptions=mcmcOptions,
                  parallel=TRUE)

sims3 <- simulate(design, 
                  nsim= ,
                  seed= ,
                  truth= ,                  
                  mcmcOptions=mcmcOptions,
                  parallel=TRUE)

## numerical summaries
summary(sims1,
        scen1,
        target=ncrm@target)
summary(sims2,
        scen2,
        target=ncrm@target)
summary(sims3,
        scen3,
        target=ncrm@target)

## compare with 3+3 design:
## not clear which doses to pick -
## all of them will be very slow ->
## take 1, 10 (starting dose), the maximum 150,
## and four more in between respecting the max. increment
## please insert the doseGrid vector accordingly:
emptydata2 <- Data(doseGrid = c( ... ))
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
simThree1 <-
  simulate(three, 
         nsim=100,
         seed=20,
         truth=scen1)
simThree2 <-
  simulate(three, 
           nsim=100,
           seed=200,
           truth=scen2)
simThree3 <-
  simulate(three, 
           nsim=100,
           seed=201,
           truth=scen3)

## summary statistics
## please comment
summary(simThree1,
        scen1,
        target=ncrm@target)

summary(simThree2,
        scen2,
        target=ncrm@target)

summary(simThree3,
        scen3,
        target=ncrm@target)

