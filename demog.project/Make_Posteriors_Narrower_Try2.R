#### Set a max on sigma^2 i to downweight unreasonable scenarios??
### Being off by more than 200/age group on average is gonna be pretty bad
### Max sigma^2_i is correponding to SD=100, so 10,000? Let's try that. 
library(readr)
library(dplyr)
source("NorthAdams.Real.Info.R")
y2010 <- realNums2010
y2010 <- c(y2010[1:5], sum(y2010[6:7]),sum(y2010[8:9]), 
           sum(y2010[10:11]), y2010[12:13], sum(y2010[14:15]),
           sum(y2010[16:17]), y2010[18])
y2000 <- as.numeric(realNums2000)
names(y2000) <-c("0-4", "5-9", "10-14", "15-19", "20-24", "25-34",
                 "35-44", "45-54", "55-59", "60-64", "65-74", "75-84",
                 "85+")
names(y2010) <- names(y2000)
#### Just realized that I have 5-year age groups for 2010, but I actually only
#### have a mix of 5- and 10-year age groups for 2000. I need these to match, so I need to truncate
#### everything to 10 year age groups. Bummer. 
#### Source for this data: https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=CF



#### SIMULATION RESULTS
load("ALL_RES5.RData") 
first_its <- new_out ## First 100*3 iterations
load("ALL_RES6.RData") ### Next 900*3 iterations
total_res <- as.data.frame(rbind(as.matrix(first_its),as.matrix(new_out)))
#### Eventually- hopefully CBIND this with another set of results to have more 
#### total trials

names(total_res) <- c("error.em", "error.im",
                      "u18.em", "o45.em", "u18.im",
                      "o45.im", "seed", "pop.2000", "median.age.2000", 
                      "pop.0.2000", "pop.5.2000", "pop.10.2000", "pop.15.2000",
                      "pop.20.2000", "pop.25.2000", "pop.30.2000", "pop.35.2000",
                      "pop.40.2000", "pop.45.2000", "pop.50.2000", "pop.55.2000",
                      "pop.60.2000", "pop.65.2000", "pop.70.2000", "pop.75.2000",
                      "pop.80.2000", "pop.85.2000", "pop.90.2000", "pop.95.2000", 
                      "pop.2010", "median.age.2010",
                      "pop.0.2010", "pop.5.2010", "pop.10.2010", "pop.15.2010",
                      "pop.20.2010", "pop.25.2010", "pop.30.2010", "pop.35.2010",
                      "pop.40.2010", "pop.45.2010", "pop.50.2010", "pop.55.2010",
                      "pop.60.2010", "pop.65.2010", "pop.70.2010", "pop.75.2010",
                      "pop.80.2010", "pop.85.2010", "pop.90.2010", "pop.95.2010",
                      "pop.100.2010", "pop.105.2010")
K <- 13 ### I have only 13 age groups since I had the unfortunate 10 year thing
I <- NROW(total_res)/NROW(unique(total_res$seed)) ### number of param sets
J <- NROW(unique(total_res$seed)) ### seeds per param

#### Summarize. Need to do some annoying work to turn my
#### output, which is 5 year age vecs, into this set of a mix
#### of 5 and 10 year age groups
phi2000 <- cbind(total_res[,10:14], 
                 apply(total_res[,15:16], 1, sum),apply(total_res[,17:18], 1, sum),
                 apply(total_res[,19:20], 1, sum),total_res[,21:22], 
                 apply(total_res[,23:24], 1, sum),apply(total_res[,25:26], 1, sum),
                 apply(total_res[,27:29], 1, sum))
names(phi2000) <- names(y2000)
phi2000$ID <- unlist(lapply(1:I, function(u) rep(u, J)))
phi2000$seed <- rep(c(1:J), I)

psi2010 <- cbind(total_res[,32:36], apply(total_res[,37:38], 1, sum),
                 apply(total_res[,39:40], 1, sum), apply(total_res[,41:42], 1, sum),
                 total_res[,43:44], apply(total_res[,45:46], 1, sum), 
                 apply(total_res[,47:48], 1, sum), apply(total_res[,49:53], 1, sum))
names(psi2010) <- names(y2000)
psi2010$ID <- unlist(lapply(1:I, function(u) rep(u, J)))
psi2010$seed <- rep(c(1:J), I)



mus <- phi2000 %>% group_by(ID) %>% summarize_all(mean)


SSE <- sum(apply(phi2000, 1, function(u) sum((u[1:13]-mus[mus$ID==u[14],2:14])^2)))
sigma_sq_delta <- SSE/(I*J*K) 

a <- 0

sigma_sq_is <-rep(0, I)
for (i in 1:I) {
  sigma_sq_is[i] <- min(1/K*sum((y2000-a-mus[i,2:14])^2), 22500)
}

##### POSTERIOR
post.means <- a+mus[2:14]
post.var <- sigma_sq_is+sigma_sq_delta/J
weights <- rep(0, I)
for (i in 1:I) {
  means <- as.numeric(post.means[i,])
  var <- post.var[i]
  product <- 1
  for (k in 1:K) {
    product = product*dnorm(x=y2000[k], mean=means[k], sd=sqrt(var))
  }
  weights[i] <- product
} 


posterior2000 <- function(x, k) {
  mysum <- 0
  for (i in 1:I) {
    postmean <- post.means[i,k]
    postsd <- sqrt(post.var[i])
    mysum = mysum + weights[i]*dnorm(x, mean=postmean, sd=postsd)
  }
  return(mysum)
}

posterior2010 <- function(x, k) {
  ba <- 2
  bv <- 2
  mysum <- 0
  for (i in 1:I) {
    postmean <- a*ba + 1/J*sum(psi2010[psi2010$ID==i,k])
    postsd <- sqrt((sigma_sq_is[i]+sigma_sq_delta/J)*bv)
    mysum = mysum + weights[i]*dnorm(x, mean=postmean, sd=postsd)
  }
  return(mysum)
}

png('posterior2010_1_maxsig.png')
par(mfrow=c(3,3))
for (age in 1:9) {
  xrange <- c(0:2500)
  posteriors <- posterior2010(xrange, age)
  plot(xrange, posteriors, type='l', xlab="People", ylab="Density", main = paste("Posterior for", names(y2010)[age], ", 2010"), cex=0.8)
  abline(v=y2010[age], col="red")
  samp <- sample(xrange, size=10000,prob=posteriors, replace=TRUE)
  abline(v=quantile(samp, 0.05), lty=2)
  abline(v=quantile(samp, 0.95), lty=2)
}
dev.off()
png('posterior2010_2_maxsig.png')
par(mfrow=c(3,3))
for (age in 10:13) {
  xrange <- c(0:2500)
  posteriors <- posterior2010(xrange, age)
  plot(xrange, posteriors, type='l', xlab="People", ylab="Density", main = paste("Posterior for", names(y2010)[age], ", 2010"), cex=0.8)
  abline(v=y2010[age], col="red")
  samp <- sample(xrange, size=10000,prob=posteriors, replace=TRUE)
  abline(v=quantile(samp, 0.05), lty=2)
  abline(v=quantile(samp, 0.95), lty=2)
}
dev.off()

png("weighed_vs_unweighted_success_and_fail.png")
k <- 6
allpreds <- psi2010[,k]
real <- as.numeric(y2010[k])
par(mfrow=c(2,2))
plot(density(allpreds), main="All Simulation Results, 25-34", xlim=c(0,3000), xlab="People")
abline(v=quantile(allpreds, 0.05), lty=2)
abline(v=quantile(allpreds, 0.95), lty=2)
abline(v=real, col="red")

xrange <- c(0:3000)
posteriors <- posterior2010(xrange, k)
plot(xrange, posteriors, type='l', xlab="People", ylab="Density", main = "Weighted Posterior, 25-34", cex=0.8, xlim=c(0,3000))
abline(v=y2010[6], col="red")
samp <- sample(xrange, size=10000,prob=posteriors, replace=TRUE)
abline(v=quantile(samp, 0.05), lty=2)
abline(v=quantile(samp, 0.95), lty=2)

k <- 8
allpreds <- psi2010[,8]
real <- as.numeric(y2010[k])
plot(density(allpreds), main="All Simulation Results, 45-54", xlim=c(0,3000), xlab="People")
abline(v=quantile(allpreds, 0.05), lty=2)
abline(v=quantile(allpreds, 0.95), lty=2)
abline(v=real, col="red")

xrange <- c(0:3000)
posteriors <- posterior2010(xrange, 8)
plot(xrange, posteriors, type='l', xlab="People", ylab="Density", main = "Weighted Posterior, 45-54", cex=0.8, xlim=c(0,3000))
abline(v=y2010[k], col="red")
samp <- sample(xrange, size=10000,prob=posteriors, replace=TRUE)
abline(v=quantile(samp, 0.05), lty=2)
abline(v=quantile(samp, 0.95), lty=2)
dev.off()
