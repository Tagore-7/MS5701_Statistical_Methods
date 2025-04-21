# Developer: Kui Zhang, MTU Department of Mathematical Sciences
# R Program for MA5701 - Statistical Methods
# R Program for Chapter 03 - Principles of Inference

#--------------------------------------------------------------
# specify working directory
setwd("G:/My Drive/Zkui/Teaching/DataSets/MA5701")

#--------------------------------------------------------------
# Example 4.2 - Filling Peanuts Jars
# data and summary statistics
peanuts <- read.csv("peanuts.csv", stringsAsFactors = FALSE)
n <- length(peanuts$weight) # sample size 
ybar <- mean(peanuts$weight) # sample mean
s2 <- var(peanuts$weight) # sample variance
alpha <- 0.05
# Rejection Region for Ha: \mu not 8.0
qt(1 - alpha / 2, df = n - 1)
# test statistic
t.stat <- (ybar - 8) / ( sqrt(s2) / sqrt(n))
round(t.stat, 4)
# p-value
p.value <- 2 * (1 - pt(abs(t.stat), df = n - 1) )
round(p.value, 4)
# two-sided CI
t.value <- qt(1 - alpha / 2, df = n - 1)
ci.low <- ybar - t.value * sqrt(s2) / sqrt(n)
ci.upp <- ybar + t.value * sqrt(s2) / sqrt(n)
round(c(ci.low, ci.upp), 4)
# one-sample t-test
t.test(x = peanuts$weight, mu = 8)
t.test(x = peanuts$weight, mu = 8, 
	alternative = "two.sided", conf.level = 1 - alpha)
t.test(x = peanuts$weight, mu  = 8,
	alternative = "less", conf.level = 1 - alpha)
	
#--------------------------------------------------------------
# Exercise - Porosity of Battery
# data and summary statistics
battery <- c(79.1, 79.5, 79.3, 79.3, 78.8, 79.0, 79.2, 79.7, 79.0, 79.2)
n <- length(battery) # sample size 
ybar <- mean(battery) # sample mean
s2 <- var(battery) # sample variance
alpha <- 0.05
# Rejection Region for Ha: \mu < 80
-qt(1 - alpha, df = n - 1)
# test s1tatistic
t.stat <- (ybar - 80) / ( sqrt(s2) / sqrt(n))
round(t.stat, 4)
# p-value
p.value <- pt(t.stat, df = n - 1)
round(p.value, 4)
# upper CI
t.value <- qt(1 - alpha, df = n - 1)
ci.upp <- ybar + t.value * sqrt(s2) / sqrt(n)
round(ci.upp, 4)
# Q-Q plot
qqnorm(battery, main = "Q-Q Plot for Battery Data")
qqline(battery)
# one-sample t-test
t.test(x = battery, mu  = 80,
	alternative = "less", conf.level = 1 - alpha)
	
#--------------------------------------------------------------
# Example 4.4 - Painkiller
# data
n <- 120
y <- 82
ybar <- y / n
p0 <- 0.60
alpha <- 0.05
# Rejection Region for Ha: p > p0 = 0.60
qnorm(1 - alpha) 
# test statistic
z.stat <- (ybar - p0) / sqrt(p0 * (1 - p0) / n)
round(z.stat, 4)
# p-value
p.value <- 1 - pnorm(z.stat)
round(p.value, 4)
# upper CI
ci.low <- ybar - qnorm(1 - alpha) * sqrt(ybar * (1 - ybar) / n)
round(ci.low, 4)
# one proportional test
prop.test(x = y, n = n, p = 0.60,
	alternative = "greater", conf.level = 1 - alpha, correct = FALSE)

#--------------------------------------------------------------
# Exercise - Breaking Strengths Carbon Fiber
# data
n <- 100
y <- 6
ybar <- y / n
p0 <- 0.10
alpha <- 0.01
# Rejection Region for Ha: p not p0 = 0.10
qnorm(1 - alpha / 2) 
# test statistic
z.stat <- (ybar - p0) / sqrt(p0 * (1 - p0) / n)
round(z.stat, 4)
# p-value
p.value <- 2 * (1 - pnorm(abs(z.stat)) )
round(p.value, 4)
# two-side CI
ci.low <- ybar - qnorm(1 - alpha / 2) * sqrt(ybar * (1 - ybar) / n)
ci.upp <- ybar + qnorm(1 - alpha / 2) * sqrt(ybar * (1 - ybar) / n)
round(c(ci.low, ci.upp), 4)
# one proportional test
prop.test(x = y, n = n, p = 0.10,
	alternative = "two.sided", conf.level = 1 - alpha, correct = FALSE)

#--------------------------------------------------------------
# Example 4.5 - Election
# data
n <- 150
y <- 84
ybar <- y / n
alpha <- 0.01
# two-side CI
ci.low <- ybar - qnorm(1 - alpha / 2) * sqrt(ybar * (1 - ybar) / n)
ci.upp <- ybar + qnorm(1 - alpha / 2) * sqrt(ybar * (1 - ybar) / n)
round(c(ci.low, ci.upp), 4)









