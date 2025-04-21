# Developer: Kui Zhang, MTU Department of Mathematical Sciences
# R Program for MA5701 - Statistical Methods
# R Program for Chapter 02 - Probability

#--------------------------------------------------------------
# specify working directory
setwd("G:/My Drive/Zkui/Teaching/DataSets/MA5701")

#--------------------------------------------------------------
# cdf (pbinom - Pr(Y <=y) ) and pmf (dbinom - Pr(Y = y)) for binomial
# Y ~ binom(25, 0.05) Pr(Y = 2)
round(dbinom(2, size = 25, prob = 0.05), 4)
# Y ~ binom(25, 0.05) Pr(Y >= 1)
round( 1 - pbinom(0, size = 25, prob = 0.05), 4)
round( 1 - dbinom(0, size = 25, prob = 0.05), 4)
round( sum(dbinom(1:25, size = 25, prob = 0.05)), 4)
# Y ~ binom(1500, 0.002) Pr(Y <= 2)
round(pbinom(2, size = 1500, prob = 0.002), 4) 
round( sum(dbinom(0:2, size = 1500, prob = 0.002) ), 4) 
# Y ~ binom(100, 0.80) Pr(Y >= 85)
round(1 - pbinom(84, size = 100, prob = 0.80), 4) 
round( sum(dbinom(85:100, size = 100, prob = 0.80) ), 4) 

#--------------------------------------------------------------
# cdf (pnorm - Pr(Y <=y) ), pdf (dnorm), and qunatile (qnorm) for normal
# Z ~ N(0, 1)
round(1 - pnorm(2.0), 4 ) # Pr(Z > 2.0)
round(pnorm(-1.0), 4) # Pr(Z < -1)
round( 1- pnorm(1.0), 4) # Pr(Z < -1)
round(pnorm(1.53), 4) # Pr(Z < 1.53)
round(pnorm(1.53) - pnorm(-1.0), 4) # Pr( -1 < Z < 1.53)
round(qnorm(1 - 0.05), 4) # z_0.05
round(qnorm(1 - 0.025), 4) # z_0.025
round(qnorm(1 - 0.085), 4) # z_0.085
round(qnorm(1 - 0.70), 4) # z_0.70
# Z ~N(0,1), Exercise
round(1 - pnorm(1.02), 4 ) # Pr(Z > 1.02)
round(pnorm(-0.80), 4 ) # Pr(Z < -0.80)
round(pnorm(1.35), 4 ) # Pr(Z < 1.35)
round(pnorm(1.35) - pnorm(-0.80), 4 ) # Pr(-0.8 < 0Z < 1.35)
# Y ~ N(10, 20)
round(1 - pnorm(15, mean = 10, sd = sqrt(20)), 4) # Pr(Y > 15)
round(1 - pnorm( (15 - 10) / sqrt(20)), 4) # Pr(Y > 15)
# Y ~ N(500, 50 * 50)
round(1 - pnorm(600, mean = 500, sd = 50), 4) # Pr(Y > 600)
round(1 - pnorm( (600 - 500) / 50 ), 4) # Pr(Y > 15)
# Y ~ N(90, 20 * 20 / 100 = 4)`
round(pnorm(86, mean = 90, sd = 2), 4) # Pr(Y > 600)
round(pnorm( (86 - 90) / 2 ), 4) # Pr(Y > 15)

#--------------------------------------------------------------
# cdf (pt - Pr(Y <=y) ), pdf (Dt), and qunatile (qt) for t-distribution
round( 1 - pt(2.0, df = 15), 4) # Pr(T_15 > 2.0)

#--------------------------------------------------------------
# Q-Q plot
texas <- read.csv("texas-house.csv", stringsAsFactors = FALSE)
qqnorm(texas$size, 
	main = "Normal Q-Q Plot for House Size" )
qqline(texas$size)

