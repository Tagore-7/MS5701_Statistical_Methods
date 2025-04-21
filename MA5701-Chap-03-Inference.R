# Developer: Kui Zhang, MTU Department of Mathematical Sciences
# R Program for MA5701 - Statistical Methods
# R Program for Chapter 03 - Principles of Inference

#--------------------------------------------------------------
# specify working directory
setwd("G:/My Drive/Zkui/Teaching/DataSets/MA5701")

#--------------------------------------------------------------
# Example 3.3 - Filling Peanuts Jars
# Rejection Region: Y^bar > 8.1 or Y^bar < 7.9
# H0: \mu = 8.0
n <- 16 # sample size
sigma <- 0.2 # population standard deviation 
# Probability for Type I error
mu <- 8.0
alpha <- 1 - pnorm(8.1, mean = mu, sd = sigma / sqrt(n)) 
alpha <- alpha + pnorm(7.9, mean = mu, sd = sigma / sqrt(n)) 
round(alpha, 4)
# Probability for Type II error
mu <- 8.15
beta <- pnorm(8.1, mean = mu, sd = sigma / sqrt(n)) 
beta <- beta - pnorm(7.9, mean = mu, sd = sigma / sqrt(n)) 
round(beta, 4)
# Test statistic and p-value
ybar <- 7.89 # sample mean
mu0 <- 8.0 # H0
z <- (ybar - mu0) / (sigma / sqrt(n) )
round(z, 4)
p <- 2 * (1 - pnorm(abs(z)) )
round(p, 4)

#--------------------------------------------------------------
# Example 3.4 - Aptitude Test
sigma <- 10 # population standard deviation 
mu0 <- 50 # H0 
n <- 500 # sample size
ybar <- 51.07 # sample mean
z <- (ybar - mu0) / (sigma / sqrt(n) )
round(z, 4)
p <- 2 * (1 - pnorm(abs(z)) )
round(p, 4)

#--------------------------------------------------------------
# Example 3.1 - Reading SCore
sigma <- 35.73 # population standard deviation 
mu0 <- 220.99 # H0 
n <- 50 # sample size
ybar <- 230.2 # sample mean
z <- (ybar - mu0) / (sigma / sqrt(n) )
round(z, 4)
p <- 1 - pnorm(z)
round(p, 4)

#--------------------------------------------------------------
# Example 3.3 - Filling Peanuts Jars
sigma <- 0.2 # population standard deviation 
n <- 16 # sample size
ybar <- 7.89 # sample mean
# 99% CI
alpha <- 0.01 # significance level
z.cut <- qnorm(1 - alpha / 2) # critical point  
ci.low <- ybar - z.cut * sigma / sqrt(n)
ci.upp <- ybar + z.cut * sigma / sqrt(n)
round(c(ci.low, ci.upp), 4)
# 95% CI
alpha <- 0.05 # significance level
z.cut <- qnorm(1 - alpha / 2) # critical point  
ci.low <- ybar - z.cut * sigma / sqrt(n)
ci.upp <- ybar + z.cut * sigma / sqrt(n)
round(c(ci.low, ci.upp), 4)

#--------------------------------------------------------------
# Example 3.4 - Aptitude Test
sigma <- 10 # population standard deviation 
n <- 500 # sample size
ybar <- 51.07 # sample mean
# 99% CI
alpha <- 0.01 # significance level
z.cut <- qnorm(1 - alpha / 2) # critical point  
ci.low <- ybar - z.cut * sigma / sqrt(n)
ci.upp <- ybar + z.cut * sigma / sqrt(n)
round(c(ci.low, ci.upp), 4)

#--------------------------------------------------------------
# Example - Power and Sample Size
mu0 <- 10.6 # H0
alpha <- 0.05 # significance level
sigma <- 2 # population standard deviation 
n <- 25 # sample size
mua <- 10 # Ha
#--------------------------------------------------------------
# For Ha: mu < 10.6
# Rejection region:
rr <- mu0 - qnorm(1 - alpha) * sigma / sqrt(25)
# power
res <- pnorm(rr, mean = mua, sd = sigma / sqrt(n))
round(res, 4)
#--------------------------------------------------------------
# For Ha: mu < 10.6 or mu > 10.6
# Rejection region:
rr1 <- mu0 - qnorm(1 - alpha / 2) * sigma / sqrt(25)
rr2 <- mu0 + qnorm(1 - alpha / 2) * sigma / sqrt(25)
# power
res <- pnorm(rr1, mean = mua, sd = sigma / sqrt(n))
res <- res + 1 - pnorm(rr2, mean = mua, sd = sigma / sqrt(n))
round(res, 4)

#--------------------------------------------------------------
# Example 3.6 - Power and Sample Size
d <- 2 # differnece of two means
sigma <- 11.25 # population standard deviation
alpha <- 0.05 # significance level
beta <- 0.10 # 1 - power 
# sample size for one-sized test
n <- sigma^2 * (qnorm(1 - alpha) + qnorm(1 - beta))^2 / d^2
n
ceiling(n)






