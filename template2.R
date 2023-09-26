## Practical with crmPack, dual endpoint dose escalation example - template to start from

## load the package:
library(crmPack)

## 1) define the dose grid: please insert the correct numbers
doseGrid <- seq(from, to=, by=)

## 2) get the model and prior for the parameters:

## here we have not yet implemented Quantiles2ProbitLogNormal,
## but the prior mean values for alpha and beta can be obtained from 2 equations as:
betaMean <- (qnorm(0.5) - qnorm(0.1)) / 140
alphaMean <- qnorm(0.1) - 10 * betaMean

## test: which dose values do we want to test? please replace ? accordingly!
pnorm(alphaMean + ? * betaMean)
pnorm(alphaMean + ? * betaMean)
## ok!

## therefore the prior can look as follows:
model <- ProbitLogNormal(mu = c(alphaMean, log(betaMean)),
                         Sigma=matrix(c(1, -0.5, -0.5, 1), nrow=2))

## generate samples from the prior and plot it:
emptyData <- Data(doseGrid=doseGrid)
mcmcOptions <- McmcOptions()
set.seed(12)
priorSamples <- mcmc(emptyData, model, mcmcOptions)
plot(priorSamples, model, emptyData) 
## is this sufficiently good? please note that the "estimate" is the prior median.
## please feel free to adapt alphaMean and betaMean, as well as the Sigma matrix
## if you would like to increase uncertainty e.g.

## 3) Create the joint dose-tox/biomarker model:
## please complete with the correct numbers
dualmodel <- DualEndpointEmax(E0 = c(, ),
                              Emax = c(, ),
                              ED50 = c(, ),
                              sigma2W=c(a=, b=),
                              rho=c(a=, b=),
                              refDose=,
                              mu=model@mu, ## already filled in: directly from model
                              Sigma=model@Sigma)

## 4) next best dose recommendation rule: please complete
ncrm <- NextBestDualEndpoint(target = c(, ),
                             scale="absolute",
                             overdose = c(, ),
                             maxOverdoseProb = )

## 5) cohort size: please complete
cohortSize <- CohortSizeConst()

## 6) stopping rule construction: please complete
stop1 <- StoppingMinPatients(nPatients = )
stop2 <- StoppingTargetBiomarker(c(, ),
                                 scale="absolute",
                                 prob=)
stop3 <- StoppingPatientsNearDose(nPatients = , percentage = )
stop4 <- StoppingMinPatients(nPatients = )

stopRule <- stop1 | (stop2 & stop3 & stop4)

## 7) specify maximum increments
increments <- IncrementsRelative(intervals=c(0, 75),
                                 increments=c(1, 0.5))

## 8) bundle everything together in the Design object:
emptyDualData <- DataDual(doseGrid=emptyData@doseGrid)

design <- DualDesign(model=dualmodel,
                     stopping=stopRule,
                     increments=increments,
                     nextBest=ncrm,
                     cohortSize=cohortSize,
                     data=emptyDualData,
                     startingDose=) ## please insert the starting dose!

## define scenarios:

## for tox: just probit model again
scenario <- function(dose, alpha, beta, ...)
{
  pnorm(alpha + beta * dose)          
}

## generic function for biomarker:
scenarioBiomarker <- function(dose, ED50biom = 30, E0 = 0, Emax = 100, ...)
{
  
  m <- E0 + Emax * dose / (ED50biom + dose)
  return(m)  
}

## first biomarker scenario: here it never reaches the target biomarker level
## please insert different ED50biom and Emax values and see how the curve changes
## until you found a setting according to the request.
curve(scenarioBiomarker(dose,
                        ED50biom=,
                        Emax=),
      from=1, to=150, xname="dose", ylim=c(0, 100))

## second biomarker scearnio: reaches target in middle of dose range
## please similar here find ED50biom and Emax values
curve(scenarioBiomarker(dose,
                        ED50biom=,
                        Emax=),
      from=1, to=150, xname="dose", ylim=c(0, 100))

## run simulations:
## insert the values you have found above here, plus complete with values
## according to exercise description.
sims1 <- simulate(design, 
                  nsim=20,
                  seed=920,
                  trueTox=scenario,
                  trueBiomarker=scenarioBiomarker,
                  args=list(alpha=alphaMean, beta=betaMean, ED50biom=,
                            Emax=),
                  sigma2W=,
                  rho=,
                  mcmcOptions=mcmcOptions,
                  parallel=TRUE)

sims2 <- simulate(design, 
                  nsim=20,
                  seed=927,
                  trueTox=scenario,
                  trueBiomarker=scenarioBiomarker,
                  args=list(alpha=alphaMean, beta=betaMean, ED50biom=,
                            Emax=),
                  sigma2W=,
                  rho=,
                  mcmcOptions=mcmcOptions,
                  parallel=TRUE)

## numerical summaries: fill in here again the according parameters to scenarios 1 and 2
summary(sims1,
        trueTox=scenario,
        trueBiomarker=scenarioBiomarker,
        alpha=alphaMean, 
        beta=betaMean, 
        ED50biom=,
        Emax=,
        target=c(0, 0.25))

summary(sims2,
        trueTox=scenario,
        trueBiomarker=scenarioBiomarker,
        alpha=alphaMean, 
        beta=betaMean, 
        ED50biom=,
        Emax=,
        target=c(0, 0.25))
## do these results make sense? which other statistics would be nice to look at?

## please evaluate these plots:
plot(sims1)
plot(sims2)


