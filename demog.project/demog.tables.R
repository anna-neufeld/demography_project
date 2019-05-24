#### Whatever population I pick, I am just going to make tables. 
fertTable <- matrix(0, nrow=106, ncol=4)
ageToFert <- rep(0,105)
yearToPCTS <- c(rep(1,5), rep(2,5), rep(3,5), rep(4,5)) ## Specific to the 1990-2010 Example
ageToFert[15:19] <- 1
ageToFert[20:24] <- 2
ageToFert[25:29] <- 3
ageToFert[30:34] <- 4
ageToFert[35:39] <- 5
ageToFert[40:44] <- 6
ageToFert[45:49] <- 7
data(tfr, package="wpp2017")
data(percentASFR, package="wpp2017")
tfrs <- subset(tfr, name=="United States of America")[, c("1990-1995", "1995-2000", "2000-2005", "2005-2010")]
tfr_by_col <- as.numeric(c(rep(tfrs[1], 5),rep(tfrs[2], 5),rep(tfrs[3], 5), rep(tfrs[4], 5)))
pct <- subset(percentASFR, name=="United States of America")[, c("1990-1995", "1995-2000", "2000-2005", "2005-2010")]/100
for (age in c(15:49)) {
  rate <- as.numeric(pct[ageToFert[age],]*tfrs)/5
  fertTable[age,] <- rate
}

mxFTable <- matrix(0, nrow=107, ncol=4)
ageToIndex <- floor((0:105)/5)+2
ageToIndex[106] <- 22
ageToIndex[1] <- 1
data(mxF, package="wpp2017")
mxFs <- subset(mxF, name=="United States of America")[, c("1990-1995", "1995-2000", "2000-2005", "2005-2010")]/5
for (age in c(0:105)) {
  mxFTable[age+1,] <- as.numeric(mxFs[ageToIndex[age+1],])
}
mxFTable[107,] <- c(1,1,1,1)


mxMTable <- matrix(0, nrow=107, ncol=4)
data(mxM, package="wpp2017")
mxMs <- subset(mxM, name=="United States of America")[, c("1990-1995", "1995-2000", "2000-2005", "2005-2010")]/5
for (age in c(0:105)) {
  mxMTable[age+1,] <- as.numeric(mxMs[ageToIndex[age+1],])
}
mxMTable[107,] <- c(1,1,1,1)

#### EMIGRATION TABLE IS FOUND IN THE MIGRATION CALCULATIONS DOC