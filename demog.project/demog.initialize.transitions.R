sex <- c("m","f")                     
fert <- c("0","1+")           
stateSpace <- expand.grid(sex=sex,fert=fert)
absStates <- c("dead","rest")   


getRandInitState <- function(birthDate){
  age <- trunc(as.numeric(simHorizon[1] - birthDate)/365.25)
  s1 <- sample(sex,1)
  s2 <- ifelse(age<=18, fert[1], sample(fert,1))
  initState <- paste(c(s1,s2),collapse="/")
  return(initState)
}

fert1<- function(age, calTime, duration){  # parity 1
  age2 <- floor(age)+1
  bin <- floor((calTime-1990)/5)+1
  bin[bin>4] <- 4
  bin[bin<1] <- 1
  rate <- sapply(1:NROW(age2), function(u) fertTable[age2[u], bin[u]])
  return(rate)
}

mortRates_f <- function(age, calTime, duration){
  age <- floor(age)+1
  rate <- mxFTable[age, 1]
  rate[calTime >= 1995] <- mxFTable[age[calTime >= 1995], 2]
  rate[calTime >= 2000] <- mxFTable[age[calTime >= 2000], 3]
  rate[calTime >= 2005] <- mxFTable[age[calTime >= 2005], 4]
  return(rate)
}

mortRates_m <- function(age, calTime, duration){
  age <- floor(age)+1
  rate <- mxMTable[age, 1]
  rate[calTime >= 1995] <- mxMTable[age[calTime >= 1995], 2]
  rate[calTime >= 2000] <- mxMTable[age[calTime >= 2000], 3]
  rate[calTime >= 2005] <- mxMTable[age[calTime >= 2005], 4]
  return(rate)
}

# (8) Emigration rates 
### Obtained from emigration schedule; made in "migration_calculations"
emigrRates <- function(age, calTime, duration){
  year <- floor(calTime-1990)+1
  year[year>21] <- 21
  year[year < 1] <- 1 
  
 
  rate <- emigration_schedule[2, year]
  rate[age<18] <- emigration_schedule[1, year[age<18]]
  rate[age>45] <- emigration_schedule[3, year[age>45]]
  return(rate)
}

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
fertTr <- fertTrMatrix[,1]
