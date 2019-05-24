require(snow)
require(doParallel)
require(foreach)
require(iterators)

### Packages I will need
require(MicSim)
require(dplyr)
require(chron)
require(readr)
require(dplyr)

##Cluster set up stuff
cl <- makeCluster(3) #Number of cores you want 
registerDoParallel(cl)
clusterSetupRNG(cl,seed=1)

## Global Param Stuff (shared by all trials)
source("demog.tables.R")
source("demog.initialize.transitions.R")

county_migration_data <- read_csv("county_migration_data.csv")

simHorizon <- setSimHorizon(startDate="01/01/1991", endDate="31/12/2010")
maxAge <- 100 

set.seed(12345)
nTrials <- 1000
#### Normal, very small SD
#error.em <- rnorm(nTrials, mean=0, sd=0.025)  
#error.im <- rnorm(nTrials, mean=0, sd=0.025)

error.em <- runif(nTrials, min = -0.02, 0.1)
error.im <- runif(nTrials, min = -0.09, 0.02)


u18.em <- runif(nTrials, min=0.03, max=0.15)  
#### Only 20\% of the pop is under 18.
#### Since under 18 year olds are less likely to be moving, I HIGHLY doubt
#### That the prop. of emigration under 18 can get close to 20%. Ill cap at 15. 
o45.em <- runif(nTrials, min=0.05, max=0.25) 
## Over 45 year olds are like 40\% of pop. So I let this get a little higher. 
u18.im <- runif(nTrials, min=0.03, max=0.15)  
o45.im <- runif(nTrials, min=0.05, max=0.25)  

paramSpace <- cbind(error.em, error.im, u18.em, o45.em, u18.im, o45.im)
paramSpace_big <- data.frame(matrix(apply(paramSpace, 1, rep, 3), ncol=6, byrow=TRUE))
names(paramSpace_big) = colnames(paramSpace)
paramSpace_big$seed <- rep(c(123,456,789), nTrials)

#### TEMPORARY: PLEASE DELETE
paramSpace_big <- paramSpace_big[sample(1:NROW(paramSpace_big), NROW(paramSpace_big)),]

##### TO SAVE TIME: Only initialize the Berkshire Inital Pop once per seed.
set.seed(123)
source("demo.init.berkshire.R", local=TRUE)
initPop1 <- actual_pop
initPop1$birthDate <- chron(initPop1$birthDate, format=c(dates="d/m/Y", times="h:m:s"), out.format=c(dates="d/m/year", times="h:m:s"))

set.seed(456)
source("demo.init.berkshire.R", local=TRUE)
initPop2 <- actual_pop
initPop2$birthDate <- chron(initPop2$birthDate, format=c(dates="d/m/Y", times="h:m:s"), out.format=c(dates="d/m/year", times="h:m:s"))

set.seed(789)
source("demo.init.berkshire.R", local=TRUE)
initPop3 <- actual_pop
initPop3$birthDate <- chron(initPop3$birthDate, format=c(dates="d/m/Y", times="h:m:s"), out.format=c(dates="d/m/year", times="h:m:s"))

initPops <- list(initPop1, initPop2, initPop3)

###### THIS IS THE IMPORTANT THING: EXPORT VARS
clusterExport(cl, c("paramSpace_big", "initPops", "county_migration_data",
                    "transitionMatrix", "absStates", "initStates", "initStatesProb",
                    "getRandInitState", "maxAge", "simHorizon", "fertTr",
                    "fert", "sex", "fertTable", "mxFTable", "mxMTable",
                    "fertTrMatrix", "allTransitions", "absTransitions",
                    "fert1", "mortRates_m", "mortRates_f", "emigrRates"))


##### NOW RUN
nTrials=1
out <- foreach(j = c(1:(nTrials*3))) %dopar% {
  require(MicSim)
  require(dplyr)
  require(chron)
  require(readr)
  require(dplyr)
  require(eeptools)
  write(paste("Starting ", j, "th job.\n",sep=''),file='demog_log.txt',append=TRUE)
  
  currentParams = paramSpace_big[j,]
  seed <- currentParams$seed
  initPop <<- initPops[[1]]
  if (seed==456) {initPop <<- initPops[[2]]}
  if (seed==781) {initPop <<- initPops[[3]]}
  
  #### Make migration table given the params.
  error.em <- currentParams$error.em
  error.im <- currentParams$error.im
  u18.em <- currentParams$u18.em
  o45.em <- currentParams$o45.em
  u18.im <- currentParams$u18.im
  o45.im <- currentParams$o45.em
  source("migration_calculatios.R", local=TRUE)
  emigration_schedule <<- emigration_schedule
  #emigration_schedule[3,4]
  ### Makes immigrant population and emmigration schedule
  
  #### Now we should be all ready to run simulation
  source("demog.initialize.transitions.R", local=TRUE)
  emigrRates(17, 2005, 0.3)
  pop <- micSim(initPop=initPop[1:1000,], immigrPop=immigrPop, 
               transitionMatrix=transitionMatrix, absStates=absStates, 
               initStates=initStates, initStatesProb=initStatesProb, 
                maxAge=maxAge, simHorizon=simHorizon, fertTr=fertTr) #, cores=2, seeds=c(123, 456))
  source("process_res.R", local=TRUE)
  
   #write "res" to file
  write(paste(results, collapse=" "), file='results3.txt',append=TRUE)
  as.numeric(results)
}

new_out = as.data.frame(matrix(unlist(out), ncol=53 , byrow=TRUE))
names(new_out)=names(out[[1]])
L=3
save(new_out, file = paste("ALL_RES",L,".RData",sep=''))
stopCluster(cl)

