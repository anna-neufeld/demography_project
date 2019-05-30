#realPop2010 <- 13851
realProps2010 <- c(0.0582, 0.0361, 0.0372, 0.1043, 0.1403,
                   0.0622, 0.0332, 0.0541, 0.0681, 0.0812,
                   0.0811, 0.0412, 0.0422, 0.0292, 0.0392, 0.02371,
                   0.0321, 0.0252)
#### NEW SOURCE: https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=CF
realPop2010 <- 13708
realNums2010 <- c(749, 654, 703, 1253, 1384, 851, 710, 740, 791,
                  925, 1050, 683, 763, 585, 541, 432, 400, 405)
names(realNums2010) <- c("0-4", "5-9",
                            "10-14", "15-19",
                            "20-24", "25-29",
                            "30-34", "35-39",
                            "40-44", "45-49",
                            "50-54", "55-59",
                            "60-64", "65-69",
                            "70-74", "75-79", 
                            "80-84", "85+")
#realNums2010 <- floor(realProps2010*realPop2010)+1 
medianAge.2010 <- 38.9
sex.ratio.2010 <- 95.1
#### MALE POP IS 47% in 2010

northAdams2000 <- read_csv("DEC_00_SF1_DP1_with_ann.csv", skip=1)[,1:53]
realPop2000 <- northAdams2000$`Number; Total population`
realNums2000 <- northAdams2000[,c(10,12,14,16,18,20,22,24,26,28,30,32,34)]
medianAge.2000 <- northAdams2000[,36]
realProps.2000 <-realNums2000/realPop2000

realPop.1990 <- 16797
props.1990 <- rep(0, 7)
names(props.1990 ) <- c("0-5", "6-17", "18-24", "25-44", "45-64", "65-80", "80+")
props.1990[1] <- round(0.0631*realPop.1990)
props.1990[2] <- round(0.1531*realPop.1990)
props.1990[3] <- round(0.1741*realPop.1990)
props.1990[4] <- round(0.2691*realPop.1990)
props.1990[5] <- round(0.1721*realPop.1990)
props.1990[6] <- round(0.1261*realPop.1990)
props.1990[7] <- round(0.0421*realPop.1990)


#### HOW MUCH DID PROPORTIONS U18, 18-45, and 45+ change over time?
three.prop.1990 <- c(sum(props.1990[1:2]), sum(props.1990[3:4]),sum(props.1990[5:7]))/realPop.1990
prop.under.18.2000 <- as.numeric((realPop2000-northAdams2000[,38])/realPop2000)
prop.over.45.2000 <- sum(realProps.2000[8:13])
three.prop.2000 <- c(prop.under.18.2000, 1-prop.over.45.2000-prop.under.18.2000, prop.over.45.2000)
three.prop.2010 <- c(0.161, 1-0.161-sum(realProps2010[8:13]), sum(realProps2010[8:13]))

