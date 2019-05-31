NorthAdamsPop <- c(16960,16699,16267, 16098, 16072, 15991, 15955, 15896, 15686, 15549, 14638,
                   14523, 14429, 14354,14256, 14144, 14036, 14052, 13945, 
                   13925, 13785)
BerkCountyPop <- c(139322, 138159, 136920, 135890, 135571, 134913, 134334, 133910,
                   132839, 132218, 134751, 133783, 133255, 133004, 132532, 131873,
                   131104, 130870, 130195, 129921, 131294)
prop <- NorthAdamsPop/BerkCountyPop

years <- seq(1990, 2010, by=1)


png("yearplot.png")
par(mfrow=c(2,2))
plot(years, NorthAdamsPop, pch=19, xlab="Year", ylab="Population", main="Raw Population")
plot(years, prop, pch=19, xlab="Year", ylab="Proportion", main="Population as % of County")
dev.off()

