### Computed in "NorthAdams.Real.Info.R"
### Hardcoded here to save time
three.prop.1990 <- c(0.2162291, 0.4431744, 0.3402989)
three.prop.2000 <- c(0.2235543, 0.3840338, 0.3924120)
three.prop.2010 <- c(0.1610, 0.4711, 0.3679)

berkshire_im <- subset(county_migration_data, destination == "25003" & origin != "25003")[3:23]
berkshire_em <- subset(county_migration_data, origin == "25003" & destination != "25003")[3:23]
total_im <- apply(berkshire_im ,2,sum)
total_em <- apply(berkshire_em ,2,sum)
berkshire_net <- total_im-total_em
### This at least somewhat checks out with Wikipedia. 

### My BEST GUESS for immigration and emmigration each year. 
### Best guess is to assume IRS numbers are correct, and to assume that North Adams exerienced numbers proportional to total pop of Berkshires
### From the census:
NorthAdamsPop <- c(16960,16699,16267, 16098, 16072, 15991, 15955, 15896, 15686, 15549, 14638,
                   14523, 14429, 14354,14256, 14144, 14036, 14052, 13945, 
                   13925, 13785)
BerkCountyPop <- c(139322, 138159, 136920, 135890, 135571, 134913, 134334, 133910,
                   132839, 132218, 134751, 133783, 133255, 133004, 132532, 131873,
                   131104, 130870, 130195, 129921, 131294)
prop <- NorthAdamsPop/BerkCountyPop
#### Relatively constant

#### Total immigrants and immigrants by year based on assuming North Adams
#### perfectly proportional to county
#### Here are two of our uncertain parameters!!!!
NA_em <- (prop+error.em)*total_im
NA_im <- (prop+error.im)*total_em

#### Next, we want to divide these into age groups. 4 more uncertain parameters. 
NA_im_vec <- sapply(NA_im, function(u) floor(u*c(u18.im, 1-u18.im-o45.im, o45.im)))
NA_em_vec <- sapply(NA_em, function(u) floor(u*c(u18.em, 1-u18.em-o45.em, o45.em)))

#### For emigration, we need to now make three RATES.
#### We need to take NA_em_vec and divide each cell by the appropriate denominator
#### Unfortunately, only have the under18, 18-45, 45+ pop breakdown for 1990, 2000, and 2010
#### As crude estimate, assume 1990 numbers hold 1990-1995, 2000 holds 1995-2005, then 2010

popBreakGuesses <- matrix(0, ncol=21, nrow=3)
popBreakGuesses[,1:5] <- three.prop.1990
popBreakGuesses[,6:15] <- three.prop.2000
popBreakGuesses[,16:21] <- three.prop.2010
popBreakGuesses <- popBreakGuesses*NorthAdamsPop
emigration_schedule <- NA_em_vec/popBreakGuesses

#### For immigration, we now generate an immigrant POP.
maxID <- max(as.numeric(initPop[,"ID"]))
immigrPop <- data.frame(ID = NULL, immigrDate = NULL , birthDate=NULL, 
                        immigrInitState=NULL) 
for (i in 1:20) {
  M <- sum(NA_im_vec[,i])
  year <- colnames(NA_im_vec)[i]
  imHorizon <- setSimHorizon(startDate=paste0("01/01/", year), endDate=paste0("31/12/", year))
  immigrDatesRange <- as.numeric(simHorizon)
  immigrDates <- dates(chron(immigrDatesRange[1] + runif(M, min=0,max=diff(immigrDatesRange)), 
                             format=c(dates="d/m/Y", times="h:m:s"), out.format=c(dates="d/m/year",times="h:m:s")))
  immigrAges <- c(runif(NA_im_vec[1,i], min=0*365.25, max=18*365.25),
                  runif(NA_im_vec[2,i], min=18*365.25, max=45*365.25),
                  runif(NA_im_vec[3,i], min=45*365.25, max=90*365.25))
  immigrBirthDates <- dates(chron(as.numeric(immigrDates) - immigrAges, 
                                  format=c(dates="d/m/Y", times="h:m:s"), out.format=c(dates="d/m/year", times="h:m:s")))
  IDmig <- maxID + (1:M)
  maxID <- maxID+ M
  immigrPop <- rbind(immigrPop, data.frame(ID = IDmig, immigrDate = immigrDates, birthDate=immigrBirthDates, 
                          immigrInitState=sapply(immigrBirthDates, getRandInitState))) 
  
  
}
                                                        
  


