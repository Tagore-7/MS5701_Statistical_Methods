# Developer: Kui Zhang, MTU Department of Mathematical Sciences
# R Program for MA5701 - Statistical Methods
# R Program for Chapter 05 - Inference for Two or More Means

#--------------------------------------------------------------
# specify working directory
setwd("G:/My Drive/Zkui/Teaching/DataSets/MA5701")

#--------------------------------------------------------------
# Example 6.1 - Soil Silt Content
# data and summary statistics
soil <- read.csv("soil.csv", stringsAsFactors = FALSE)
head(soil)
# box-plot
boxplot(silt ~ site, data = soil, xlab = "Site", 
	ylab = "Content", main = "")
# overall mean, variance, etc.
mean(soil$silt)
var(soil$silt)
# mean and variance for each site
tapply(X = soil$silt, INDEX = soil$site, FUN = mean)
tapply(X = soil$silt, INDEX = soil$site, FUN = var)

#--------------------------------------------------------------
# Exercises – Sick Leave by Branch
# Create data
sick <- data.frame(branch = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3),
	days = c(15, 20, 19, 14, 11, 15, 11, 18, 19, 23) )
# sample size
nrow(sick)
table(sick$branch)
# sample mean
mean(sick$days)
tapply(X = sick$days, INDEX = sick$branch, FUN = mean)
# sample variance
var(sick$days)
tapply(X = sick$days, INDEX = sick$branch, FUN = var)
# sum of squares
(nrow(sick) - 1) * var(sick$days)
sum( (sick$days - mean(sick$days))^2)

#--------------------------------------------------------------
# Example 6.2 - Yield of Rice Data
# data and summary statistics
rice <- read.csv("rice.csv", stringsAsFactors = FALSE)
head(rice)
# box-plot
boxplot(yield ~ variety, data = rice, xlab = "Variety", 
	ylab = "Yield", main = "")
# sample size
nrow(rice)
table(rice$variety)
# sample mean
mean(rice$yield)
tapply(X = rice$yield, INDEX = rice$variety, FUN = mean)
# sample variance
var(rice$yield)
tapply(X = rice$yield, INDEX = rice$variety, FUN = var)
# sum of squares
(nrow(rice) - 1) * var(rice$yield)
( table(rice$variety) - 1) * tapply(X = rice$yield, INDEX = rice$variety, FUN = var)
# F test statistic
tss <- (nrow(rice) - 1) * var(rice$yield)
ssw <- ( table(rice$variety) - 1) * tapply(X = rice$yield, INDEX = rice$variety, FUN = var)
ssw <- sum(ssw)
tdf <- nrow(rice) - 1
wdf <- nrow(rice) - length(table(rice$variety))
ssb <- tss - ssw
bdf <- tdf - wdf
res.ss <- c(ssb = ssb, ssw = ssw, tss = tss)
res.df <- c(bdf = bdf, wdf = wdf, tdf = tdf)
res.test <- c(msb = ssb / bdf, msw = ssw / wdf, F = (ssb / bdf) / (ssw / wdf) )
res.ss
res.df
res.test
# p-vlaue 
1 - pf(res.test["F"], df1 = bdf, df2 = wdf)
# anova analysis
summary( aov(yield ~ factor(variety), data = rice) )

#--------------------------------------------------------------
# Exercise - Sleeping Drug
# data
drug <- data.frame(hour = c(5.6, 5.7, 5.1, 3.8, 4.6, 8.4, 8.2, 8.8, 7.1, 7.2, 8.0, 10.6, 6.6, 8.0, 8.0, 6.8),
	drug = c("P", "P", "P", "P", "P", "S", "S", "S", "S", "S", "S", "N", "N", "N", "N", "N") )
# box plot
boxplot(hour ~ drug, data = drug, xlab = "Drug", 
	ylab = "hour", main = "")
# sample size
nrow(drug)
table(drug$drug)
# sample mean
mean(drug$hour)
tapply(X = drug$hour, INDEX = drug$drug, FUN = mean)
# sample variance
var(drug$hour)
tapply(X = drug$hour, INDEX = Drug$drug, FUN = var)
# sum of squares
(nrow(drug) - 1) * var(drug$hour)
( table(drug$hour) - 1) * tapply(X = drug$hour, INDEX = drug$drug, FUN = var)
# F test statistic
tss <- (nrow(drug) - 1) * var(drug$hour)
ssw <- ( table(drug$drug) - 1) * tapply(X = drug$hour, INDEX = drug$drug, FUN = var)
ssw <- sum(ssw)
tdf <- nrow(drug) - 1
wdf <- nrow(drug) - length(table(drug$drug))
ssb <- tss - ssw
bdf <- tdf - wdf
res.ss <- c(ssb = ssb, ssw = ssw, tss = tss)
res.df <- c(bdf = bdf, wdf = wdf, tdf = tdf)
res.test <- c(msb = ssb / bdf, msw = ssw / wdf, F = (ssb / bdf) / (ssw / wdf) )
res.ss
res.df
res.test
# p-vlaue 
1 - pf(res.test["F"], df1 = bdf, df2 = wdf)
# for Levene's Test	
library(car) 
leveneTest(hour ~ factor(drug), data = drug, center = mean)
# analysis of variance
summary( aov(hour ~ drug, data = drug) )




