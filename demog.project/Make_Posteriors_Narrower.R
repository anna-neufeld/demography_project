#### Here's the issue I am noticing.
#### For a lot of my parameter draws, the results are WAY off from truth.
#### So standard deviations sigma_i are kind of big
#### So even though the result from this param set is totally implausible, weight isn't THAT small because
#### it gets to have a massive SD. 

### IDEA: I have 400 param values. Can I do the method (no a), and then afterwards take only the 100 param values with
### highest weights. And then redo the sd cals, etc, on only these 100 sets?
source("Modified_Bayesian_Melding.R")
biggest <- order(weights, decreasing=TRUE)[1:100]
res <- total_res[phi2000$ID %in% biggest,]
total_res <- res

#### REDO THE METHOD!!
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
  sigma_sq_is[i] <- 1/K*sum((y2000-a-mus[i,2:14])^2)
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

png('posterior2000_MOD.png')
par(mfrow=c(3,3))
for (age in 1:9) {
  xrange <- c(0:2500)
  posteriors <- posterior2000(xrange, age)
  plot(xrange, posteriors, type='l', xlab="People", ylab="Density", main = paste("Posterior for", names(y2000)[age], ", 2000"))
  abline(v=y2000[age], col="red")
  samp <- sample(xrange, size=10000,prob=posteriors, replace=TRUE)
  abline(v=quantile(samp, 0.05), lty=2)
  abline(v=quantile(samp, 0.95), lty=2)
}
dev.off()
png('posterior2000_2_MOD.png')
par(mfrow=c(3,3))
for (age in 10:13) {
  xrange <- c(0:2500)
  posteriors <- posterior2000(xrange, age)
  plot(xrange, posteriors, type='l', xlab="People", ylab="Density", main = paste("Posterior for", names(y2000)[age], ", 2000"))
  abline(v=y2000[age], col="red")
  samp <- sample(xrange, size=10000,prob=posteriors, replace=TRUE)
  abline(v=quantile(samp, 0.05), lty=2)
  abline(v=quantile(samp, 0.95), lty=2)
}
dev.off()

png('posterior2010_1_noa.png')
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
png('posterior2010_2_noa.png')
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

#### For comparison, what happens if we just look at histogram of ALL
#### predictions for 25-34 in 2010; no weighting

png("weighed_vs_unweighted_no_a.png")
k <- 6
allpreds <- psi2010$`25-34`
real <- as.numeric(y2010[k])
par(mfrow=c(2,2))
plot(density(allpreds), main="Unweighted Posterior, 25-34", xlim=c(0,3000), xlab="People")
abline(v=quantile(allpreds, 0.05), lty=2)
abline(v=quantile(allpreds, 0.95), lty=2)
abline(v=real, col="red")

xrange <- c(0:3000)
posteriors <- posterior2010(xrange, 6)
plot(xrange, posteriors, type='l', xlab="People", ylab="Density", main = "Weighted Posterior, 25-34", cex=0.8, xlim=c(0,3000))
abline(v=y2010[6], col="red")
samp <- sample(xrange, size=10000,prob=posteriors, replace=TRUE)
abline(v=quantile(samp, 0.05), lty=2)
abline(v=quantile(samp, 0.95), lty=2)
dev.off()

biggest <- order(weights, decreasing=TRUE)[1:5]
for (b in biggest) {
  res <- total_res[phi2000$ID==b,]
  print(round(apply(res[, c(1:8, 30)], 2, mean), 4))
  print(sum(y2010) - mean(res[,30]))
  print(sum(y2000) - mean(res[,8]))
}

biggest <- order(weights, decreasing=TRUE)[1:5]
for (b in biggest) {
  res <- total_res[phi2000$ID==b,]
  print(round(apply(res[, c(1:8, 30)], 2, mean), 4))
  print(sum(y2010) - mean(res[,30]))
}


