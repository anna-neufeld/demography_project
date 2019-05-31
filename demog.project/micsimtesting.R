# Defining simulation horizon

set.seed <- seed
error.em <- 0.02   #### Normal, very small SD
error.im <- -0.01  #### Normal, same very small SD
u18.em <- 0.1    #### Uniform on small probs
o45.em <- 0.1 #### Uniform on small probs
u18.im <- 0.05 #### Uniform on small probs
o45.im <- 0.15 #### Uniform on small probs


## Create the North Adams 1990 population
## Lots of stochasticity involved here

# Definition of immigrants entering the population (for illustration purposes, create immigrants 
# randomly)
M = 200                                                           
immigrDatesRange <- as.numeric(simHorizon)
immigrDates <- dates(chron(immigrDatesRange[1] + runif(M, min=0,max=diff(immigrDatesRange)), 
                           format=c(dates="d/m/Y", times="h:m:s"), out.format=c(dates="d/m/year",times="h:m:s")))
immigrAges <- runif(M, min=15*365.25, max=70*365.25)
immigrBirthDates <- dates(chron(as.numeric(immigrDates) - immigrAges, 
                                format=c(dates="d/m/Y", times="h:m:s"), out.format=c(dates="d/m/year", times="h:m:s")))
IDmig <- max(as.numeric(initPop[,"ID"]))+(1:M)
immigrPop <- data.frame(ID = IDmig, immigrDate = immigrDates, birthDate=immigrBirthDates, 
                        immigrInitState=sapply(immigrBirthDates, getRandInitState))  

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
source('demog.tables.R')
pop <- micSim(initPop=initPop, immigrPop=immigrPop, 
              transitionMatrix=transitionMatrix, absStates=absStates, 
              initStates=initStates, initStatesProb=initStatesProb, 
              maxAge=maxAge, simHorizon=simHorizon, fertTr=fertTr) #, cores=2, seeds=c(123, 456))

###### PROCESS RESULTS
nativeIDS <- initPop$ID
immigrantIDS <- immigrPop$ID

##### 2000 POP: For all people who are NOT immigrants:
#### Among people born before 2000:
####  if never transitioned (TO=NA), add as initState
#### If appear as one single row, if transitionDate before 2000 add as TO else add as init
#### if appear as many rows, same rule as above, but only on last row. 
#### Add immigrants who came before 2000

thresh <- chron("01/01/2001")
pop2000 <- pop %>% filter(birthDate < thresh)
library(eeptools)
pop2000$age <- age_calc(as.Date(pop2000$birthDate),as.Date("2001-01-01"), units="years")
pop2000_supp <- join(pop2000, immigrPop[,1:2], by="ID")
pop2000 <- pop2000_supp %>% filter(immigrDate < thresh | is.na(immigrDate))



tab <- count(pop2000, var="ID")
pop2000 <- join(pop2000, tab, by="ID")

pop2000$final_state = NA
pop2000$final_state[is.na(pop2000$To)]= pop2000$initState[is.na(pop2000$To)]
pop2000$final_state[pop2000$freq==1 & pop2000$transitionTime > thresh & !is.na(pop2000$transitionTime) ] = pop2000$initState[pop2000$freq==1 & pop2000$transitionTime > thresh  & !is.na(pop2000$transitionTime)]
pop2000$final_state[pop2000$freq==1 & pop2000$transitionTime < thresh & !is.na(pop2000$transitionTime) ] = pop2000$To[pop2000$freq==1 & pop2000$transitionTime < thresh  & !is.na(pop2000$transitionTime)]

repeats <- unique(pop2000$ID[pop2000$freq>1])
for (person in repeats) {
  rows <- pop2000 %>% filter(ID==person)
  times <- rows$transitionTime < thresh
  
  #### This person never transitioned before 2000.
  #### We just want one row for them, and we want it to be their 
  #### initial state
  if (sum(times)==0) {
    pop2000[pop2000$ID==person,][1,]$final_state <- pop2000[pop2000$ID==person,][1,]$init_state
  } else {
    #### This person did all their transitions before 2000.
    #### Assign them the last state they transitioned to
    if (sum(times)==length(times)) {
      pop2000[pop2000$ID==person,][length(times),]$final_state <-  pop2000[pop2000$ID==person,][length(times),]$To
    } else {
      swaps <- which(times[1:(length(times)-1)]!=times[2:(length(times))])
      pop2000[pop2000$ID==person,][swaps,]$final_state <-  pop2000[pop2000$ID==person,][swaps,]$To
      print(person)
    }
  }
}

pop2000_final <- pop2000[!is.na(pop2000$final_state),]
totalPop2000.res <- NROW(pop2000_final) - sum(pop2000_final$final_state %in% c("dead", "rest"))
livePop <- pop2000_final[!(pop2000_final$final_state %in% c("dead", "rest")),]
medianage2000.res <- median(livePop$age)
ageVec2000.res <- table(floor(livePop$age/5)+1)


###### Important stuff I haven't really thought of yet.
###### I really want to take this output and print an age-specific pop. vector for year 2000 and year 2010
ppl <- convertToWideFormat(pop)
ppl$finalState <- as.character(ppl$initState)
for (i in 1:max(ppl$ns)) {
  stateName <- paste0("To.",i)
  ppl$finalState[ppl$ns==i] <- as.character(ppl[[stateName]][ppl$ns==i])
}
pop2010 <- ppl[!(ppl$finalState %in% c("dead", "rest")),]
totPop2010 <- NROW(pop2010)
pop2010$age <- age_calc(as.Date(pop2010$birthDate),as.Date("2011-01-01"), units="years")
medianage2010.res <- median(pop2010$age)
ageVec2010.res <- table(floor(pop2010$age/5)+1)




