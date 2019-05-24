#### Information about North Adams in 1990
#### Source is that 1990 census booklet

totPop <- 16797
breakdown <- matrix(0, nrow=7, ncol=3)
rownames(breakdown) <- c("0-5", "6-17", "18-24", "25-44", "45-64", "65-80", "80+")
colnames(breakdown) <- c("m", "f", "tot")
breakdown[1,3] <- round(0.0631*totPop)
breakdown[2,3] <- round(0.1531*totPop)
breakdown[3,3] <- round(0.1741*totPop)
breakdown[4,3] <- round(0.2691*totPop)
breakdown[5,3] <- round(0.1721*totPop)
breakdown[6,3] <- round(0.1261*totPop)
breakdown[7,3] <- round(0.0421*totPop)

### Let's assume 0-5 Sex Dist follows US SRB
breakdown[1,1] <- round(0.5121951*breakdown[1,3])
### Let's assume 5-18 Sex Dist is 50-50
breakdown[2,1] <-  round(1/2*breakdown[2,3])
### Census says 18+ Sex Dist is heavility skewed female (53\% Female by some accounts, higher by others)
breakdown[3:7,1] <- round(0.47*breakdown[3:7,3])

breakdown[,2] <- breakdown[,3]-breakdown[,1]

#### I feel I don't have any choice but to assume uniform age distribution within age groups
#### Gonna just kind of make up a sampling mechanism

possibleAges <- 0:99

### THIS LINE HAS BEEN MODIFIED TO AVOID HAVING PEOPLE ABOVE 90. GO BACK LATER
probs <- c(rep(0.0631/5,5), rep(0.1531/13,13), rep(0.1741/7,7), rep(0.2691/20,20), rep(0.1721/20,20), rep(0.1261/15,15), rep(0.9999*0.0421/10, 10), rep(0.0001*0.0421/10, 10))  

#### SOURCE OF STOCHASTICITY: AGE DIST. 
my_total_pop <- sample(possibleAges, prob=probs, size=totPop, replace=TRUE)

### INITIALIZE POP.
start_year <- 1990
birthyear <- start_year - my_total_pop
actual_pop <- as.data.frame(matrix(0, nrow=totPop, ncol=3))
monthdays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
names(actual_pop) <- c("ID", "birthDate", "initState")
actual_pop$ID <- 1:NROW(actual_pop)
i=1
strmonths <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
strdays <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13",
             "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26",
             "27", "28", "29", "30", "31")

months <- sample(1:12, size=totPop, replace=TRUE)
days <- sapply(months, function(u) sample(1:monthdays[u], size=1))
dates <- sapply(1:NROW(months), function(u) chron(paste(strmonths[months[u]], strdays[days[u]], birthyear[u], sep="/")))

sex <- sample(c("m", "f"), prob=c(0.47, 0.53), size=totPop, replace=TRUE)
sex[my_total_pop < 5] <- sample(c("m", "f"), prob=c(1-0.4878,0.4878), size=sum(my_total_pop < 5), replace=TRUE)
sex[(my_total_pop < 18 & my_total_pop > 5)] <- sample(c("m", "f"), prob=c(0.5,0.5), size=sum((my_total_pop < 18 & my_total_pop > 5)), replace=TRUE)

ferts <- sapply(my_total_pop, function(u) ifelse(u<=15, fert[1], sample(fert,1)))
states <- sapply(1:totPop, function(u) paste(sex[u], ferts[u], sep='/'))

actual_pop$birthDate <- chron(dates)
actual_pop$initState <- states


