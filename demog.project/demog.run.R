rm(list=ls())
library(MicSim)

# Defining simulation horizon
simHorizon <- setSimHorizon(startDate="01/01/1990", endDate="31/12/2010")

# Seed for random number generator
set.seed(234)

# Definition of maximal age 
maxAge <- 105 

# Defintion of nonabsorbing and absorbing states
sex <- c("m","f")                     
fert <- c("0","1+")           
stateSpace <- expand.grid(sex=sex,fert=fert)
absStates <- c("dead","rest")   


#### Definition of an initial population 
#### Call function that will make a synthetic North Adams MA populaton 

getRandInitState <- function(birthDate){
  age <- trunc(as.numeric(simHorizon[1] - birthDate)/365.25)
  s1 <- sample(sex,1)
  s2 <- ifelse(age<=18, fert[1], sample(fert,1))
  initState <- paste(c(s1,s2),collapse="/")
  return(initState)
}


source('demo.init.berkshire.R')
N <- 200
initPop <- actual_pop[1:N,]
initPop$birthDate <- chron(initPop$birthDate, format=c(dates="d/m/Y", times="h:m:s"), out.format=c(dates="d/m/year", times="h:m:s"))
initPop$ID <- 1:200

# Definition of immigrants entering the population (for illustration purposes, create immigrants 
# randomly)
M = 400                                                           
immigrDatesRange <- as.numeric(simHorizon)
immigrDates <- dates(chron(immigrDatesRange[1] + runif(M, min=0,max=diff(immigrDatesRange)), 
                           format=c(dates="d/m/Y", times="h:m:s"), out.format=c(dates="d/m/year",times="h:m:s")))
immigrAges <- runif(M, min=15*365.25, max=70*365.25)
immigrBirthDates <- dates(chron(as.numeric(immigrDates) - immigrAges, 
                                format=c(dates="d/m/Y", times="h:m:s"), out.format=c(dates="d/m/year", times="h:m:s")))
IDmig <- max(as.numeric(initPop[,"ID"]))+(1:M)
immigrPop <- data.frame(ID = IDmig, immigrDate = immigrDates, birthDate=immigrBirthDates, 
                        immigrInitState=sapply(immigrBirthDates, getRandInitState))  

                             

source('demog.initialize.transitions.R')

# Definition of initial states for newborns 
initStates <- rbind(c("m","0"),c("f","0")) 
# Definition of related occurrence probabilities
initStatesProb <- c(0.515,0.485) 
# Transition pattern and assignment of functions specifying transition rates
fertTrMatrix <- cbind(c("0->1+","1+->1+"),                         
                      c("fert1", "fert1"))
allTransitions <- rbind(fertTrMatrix)
absTransitions <- rbind(c("f/dead","mortRates_f"), c("m/dead","mortRates_m") ,c("rest","emigrRates"))
transitionMatrix <- buildTransitionMatrix(allTransitions=allTransitions,
                                          absTransitions=absTransitions, stateSpace=stateSpace)
# Define transitions triggering a birth event
fertTr <- fertTrMatrix[,1]

# Execute microsimulation (sequentially, i.e., using only one CPU core)
#cores <- 2
#seeds <- c(123,345)
pop <- micSim(initPop=initPop, immigrPop=immigrPop, 
              transitionMatrix=transitionMatrix, absStates=absStates, 
              initStates=initStates, initStatesProb=initStatesProb, 
              maxAge=maxAge, simHorizon=simHorizon, fertTr=fertTr)# cores=cores, seeds=seeds)

ppl <- convertToWideFormat(pop)
ppl$finalState <- as.character(ppl$initState)
ppl$finalState[ppl$ns==1] <- as.character(ppl$To.1[ppl$ns==1])
ppl$finalState[ppl$ns==2] <- as.character(ppl$To.2[ppl$ns==2])
ppl$finalState[ppl$ns==3] <- as.character(ppl$To.3[ppl$ns==3])
totalPop <- NROW(ppl) - sum(ppl$finalState %in% c("dead", "rest"))
totalPop
