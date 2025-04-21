# Developer: Kui Zhang, MTU Department of Mathematical Sciences
# R Program for MA5701 - Statistical Methods
# R Program for Chapter 05 - Inference for Two Populations

#--------------------------------------------------------------
# specify working directory
setwd("G:/My Drive/Zkui/Teaching/DataSets/MA5701")

#--------------------------------------------------------------
# Example - Packing of Ground Beef
# data and summary statistics
beef <- read.csv("beef.csv", stringsAsFactors = FALSE)
alpha <- 0.05
# summary statistics: sample size, mean, variance
n1 <- length(beef$first)
n2 <- length(beef$second)
mu1 <- mean(beef$first)
mu2 <- mean(beef$second)
var1 <- var(beef$first)
var2 <- var(beef$second)
res1 <- c(n1 = n1, n2 = n2, mean.day.1 = mu1, mean.day.2 = mu2, 
	variance.day.1 = var1, variance.dya.2 = var2)
res1
# rejection region
t.crit <- qt(1 - alpha / 2, df = n1 + n2 - 2)
t.crit
# test statistic & pvlaue
sp2 <- (n1 - 1) * var1 + (n2 - 1) * var2
sp2 <- sp2 / (n1 + n2 - 2)
t.stat <- (mu1 - mu2) / sqrt(sp2) / sqrt(1 / n1 + 1 / n2)
p.value <- 2 * (1 - pt(abs(t.stat), df = n1 + n2 - 2) )
res2 <- c(t.crit = t.crit, sp2 = sp2, t.stat = t.stat, p.value = p.value)
res2
# confidence interval
low.ci <- mu1 - mu2 - t.crit * sqrt(sp2) * sqrt(1 / n1 + 1 / n2)
upp.ci <- mu1 - mu2 + t.crit * sqrt(sp2) * sqrt(1 / n1 + 1 / n2)
res3 <- c(low.ci = low.ci, upp.ci = upp.ci)
res3
# Q-Q plot for normality assumption
qqnorm(y = beef$first, main = "Normal Q-Q Plot for First Day")
qqline(y = beef$first)
qqnorm(y = beef$second, main = "Normal Q-Q Plot for Second Day")
qqline(y = beef$second)
# box plot for equal variance assumption
boxplot(beef, main = "Boxplot for Packing of Beef",
	ylab = "Beef Packed")
# with R function t.test
t.test(x = beef$first, y = beef$second,	alternative = "two.sided",
	mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 1 - alpha)
	
#--------------------------------------------------------------
# Exercise - Diet Formulation
# data and summary statistics
reg.diet <- c(831, 858, 833, 860, 922, 875, 797, 788)
new.diet <- c(870, 882, 896, 925, 842, 908, 944, 927, 965, 887)
alpha <- 0.05
# summary statistics: sample size, mean, variance
n1 <- length(reg.diet)
n2 <- length(new.diet)
mu1 <- mean(reg.diet)
mu2 <- mean(new.diet)
var1 <- var(reg.diet)
var2 <- var(new.diet)
res1 <- c(n1 = n1, n2 = n2, mean.reg = mu1, mean.new = mu2, 
	variance.reg = var1, variance.new = var2)
res1
# rejection region
t.crit <- qt(1 - alpha, df = n1 + n2 - 2)
-t.crit
# test statistic & p-value
sp2 <- (n1 - 1) * var1 + (n2 - 1) * var2
sp2 <- sp2 / (n1 + n2 - 2)
t.stat <- (mu1 - mu2) / sqrt(sp2) / sqrt(1 / n1 + 1 / n2)
p.value <- pt(t.stat, df = n1 + n2 - 2)
res2 <- c(t.crit = t.crit, sp2 = sp2, t.stat = t.stat,
	p.value = p.value)
res2
# confidence interval
upp.ci <- mu1 - mu2 + t.crit * sqrt(sp2) * sqrt(1 / n1 + 1 / n2)
upp.ci
# Q-Q plot for normality assumption
qqnorm(y = reg.diet, main = "Normal Q-Q Plot for Regular Diet")
qqline(y = reg.diet)
qqnorm(y = new.diet, main = "Normal Q-Q Plot for New Diet")
qqline(y = new.diet)
# box plot for equal variance assumption
boxplot(reg.diet, main = "Boxplot for Regular Drug",
	ylab = "Weight")
boxplot(new.diet, main = "Boxplot for New Drug",
	ylab = "Weight")
# with R function t.test
t.test(x = reg.diet, y = new.diet,	alternative = "less",
	mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 1 - alpha)

#--------------------------------------------------------------
# Example 5.6 - Baseball Teams
# data and summary statistics
baseball <- read.csv("baseball.csv", stringsAsFactors = FALSE)
head(baseball)
alpha <- 0.05
n <- nrow(baseball) # sample size
# summary statistics
mu <- mean(baseball$diff)
sig2 <- var(baseball$diff)
mu1 <- mean(baseball$F2)
mu2 <- mean(baseball$F3)
var1 <- var(baseball$F2)
var2 <- var(baseball$F3)
res1 <- c(mean.diff = mu, var.diff = sig2, 
	mean.1960 = mu1, variance.1960 = var1,
	mean.1961 = mu2, variance.1961 = var2 )
res1
# rejection region
t.crit <- qt(1 - alpha, df = n - 1)
-t.crit
# test statistic & p-value
t.stat <- mu / sqrt(sig2) / sqrt(1 / n)
p.value <- pt(t.stat, df = n - 1)
res2 <- c(t.crit = t.crit, t.stat = t.stat, p.value = p.value)
res2
# confidence interval
upp.ci <- mu + t.crit * sqrt(sig2) * sqrt(1 / n)
upp.ci
# Q-Q plot
qqnorm(y = baseball$diff, main = "Normal Q-Q Plot")
qqline(y = baseball$diff)
# use R function t.test
t.test(x = baseball$F3, y = baseball$F2, alternative = "less",
	mu = 0, paired = TRUE, conf.level = 1 - alpha)
t.test(x = baseball$diff, alternative = "less",
	mu = 0, conf.level = 1 - alpha)

#--------------------------------------------------------------
# Exercise - BUN in Cat
bun <- read.csv("bun.csv", stringsAsFactors = FALSE)
head(bun)
alpha <- 0.05
n <- nrow(bun) # sample size
# summary statistics
mu <- mean(bun$diff)
sig2 <- var(bun$diff)
mu1 <- mean(bun$init)
mu2 <- mean(bun$final)
var1 <- var(bun$init)
var2 <- var(bun$final)
res1 <- c(mean.diff = mu, var.diff = sig2, 
	mean.initial = mu1, variance.initial = var1,
	mean.final = mu2, variance.final = var2 )
res1
# rejection region
t.crit <- qt(1 - alpha, df = n - 1)
t.crit
# test statistic & p-value
t.stat <- mu / sqrt(sig2) / sqrt(1 / n)
p.value <- 1 - pt(t.stat, df = n - 1)
res2 <- c(t.crit = t.crit, t.stat = t.stat, p.value = p.value)
res2
# confidence interval
low.ci <- mu - t.crit * sqrt(sig2) * sqrt(1 / n)
low.ci
qqline(y = baseball$diff)
# use R function t.test
t.test(x = bun$final, y = bun$init, alternative = "greater",
	mu = 0, paired = TRUE, conf.level = 1 - alpha)
t.test(x = bun$diff, alternative = "greater",
	mu = 0, conf.level = 1 - alpha)

#--------------------------------------------------------------
# Example 5.8 - Candidate
# data
y1 <- 105
n1 <- 250
y2 <- 128
n2 <- 250
alpha <- 0.04
# rejection region
z.crit <- qnorm(1 - alpha)
-z.crit
# test statistic and p-vlaue
p1 <- y1 / n1
p2 <- y2 / n2
p.hat <- (y1 + y2) / (n1 + n2)
z.stat <- (p1 - p2) / sqrt(p.hat * (1 - p.hat)) / sqrt(1 / n1 + 1 / n2)
p.value <- pnorm(z.stat)
res2 <- c(prop.man = p1, prop.woman = p2, prop.pooled = p.hat,
	z.stat = z.stat, p.value = p.value)
res2
# confidence interval
upp.ci <- p1 - p2 + z.crit * sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
upp.ci
# use R function prop.test
prop.test(x = c(y1, y2), n = c(n1, n2), 
	alternative = "less", conf.level = 1 - alpha, correct = FALSE)

#--------------------------------------------------------------
# Exercise - Newly hatched Larvae
# data
y1 <- 38
n1 <- 100
y2 <- 18
n2 <- 80
alpha <- 0.04
# rejection region
z.crit <- qnorm(1 - alpha / 2)
-z.crit
# test statistic and p-vlaue
p1 <- y1 / n1
p2 <- y2 / n2
p.hat <- (y1 + y2) / (n1 + n2)
z.stat <- (p1 - p2) / sqrt(p.hat * (1 - p.hat)) / sqrt(1 / n1 + 1 / n2)
p.value <- 2 * (1 - pnorm(abs(z.stat)))
res2 <- c(prop.copper = p1, prop.lead = p2, prop.pooled = p.hat,
	z.stat = z.stat, p.value = p.value)
res2
# confidence interval
low.ci <- p1 - p2 - z.crit * sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
upp.ci <- p1 - p2 + z.crit * sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
res3 <- c(low.ci = low.ci, upp.ci = upp.ci)
res3
# use R function prop.test
prop.test(x = c(y1, y2), n = c(n1, n2), 
	alternative = "two.sided", conf.level = 1 - alpha, correct = FALSE)
	
#--------------------------------------------------------------
# Example 5.9 - Drug on Headache
# data
y <- 6
n <- 20
alpha <- 0.05
# rejection region
z.crit <- qnorm(1 - alpha)
-z.crit
# test statistic and p-value
p <- y / n
z.stat <- ( p - 0.5) / sqrt(0.5 * (1 - 0.5)) / sqrt(1 / n)
p.value <- pnorm(z.stat)
res2 <- c(z.test = z.stat, p.value = p.value)
res2
# confidence interval
upp.ci <- p + z.crit * sqrt(p * (1 - p)) * sqrt(1 / n)
upp.ci









	