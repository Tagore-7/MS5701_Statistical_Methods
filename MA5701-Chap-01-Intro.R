# Developer: Kui Zhang, MTU Department of Mathematical Sciences
# R Program for MA5701 - Statistical Methods
# R Program for Chapter 01 - Data and Statistics

#--------------------------------------------------------------
# specify working directory
setwd("G:/My Drive/Zkui/Teaching/DataSets/MA5701")

#--------------------------------------------------------------
# program for vector
# vector
x <- c(2, 4, 6, 8, 10)
# subscripting with positive integers
x[2]
x[c(1, 3, 5)]
x[c(5, 1, 3)]
# subscripting with negative integers
x[-2] # same as x[c(1, 3, 4, 5)]
x[c(-2, -4)] # same as x[c(1, 3, 5)]
x[c(-4, -2)] # same as x[c(1, 3, 5)]
# subscripting with logical
x[c(TRUE, FALSE, FALSE, TRUE, FALSE)] # same as x[c(1,4)]
x[TRUE] # same as x[c(TRUE, TRUE, TRUE, TRUE, TRUE)]
# elment-wise operation
c(1, 3, 5) + c(2, 4, 6)
c(1, 3, 5) + 10 

#--------------------------------------------------------------
# program for data frame
# set up working directory
setwd("G:/My Drive/Zkui/Teaching/DataSets/MA5701")
# read data frame
norc <- read.csv(file = "norc.csv", stringsAsFactors = FALSE)
# first or last few rows
head(norc) # same as head(x = norc)
tail(norc)
# name of columns
names(norc) 
# structure of data frame
str(norc)
# subscripting
norc[1, ] # first row
norc[c(1, 3), ] # first and third rows
norc[, 2] # second column, now a vector
norc[ , c(2, 3)] # second and third columns, still a data frame
norc[c(1, 3), c(1, 2)] # first and third rows, first and second columns
norc$age # second column

#--------------------------------------------------------------
# frequency table for one variable
# data used
norc <- read.csv(file = "norc.csv", stringsAsFactors = FALSE)
texas <- read.csv(file = "texas-house.csv", stringsAsFactors = FALSE)
# frequency table for qualitative/discrete variable
table(texas$bed) # frequency table
prop.table( table(texas$bed) )  # relative frequency
b <- prop.table( table(texas$bed) )
round(b, digits = 4)
# frequency table for continuous variables
a <- cut(x = texas$price, 
   breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400) * 1000,
   right = FALSE)
b <- table(a)
d <- b / sum(b)
round(d, digits = 4)
# compare a and Data
a[c(1, 21, 31, 41)]
texas$price[c(1, 21, 31, 41)]

#--------------------------------------------------------------
# bar plot
barplot(table(texas$exter),
   main = "Bar plot for House Exerior",
   xlab = "Types of Exterior",
   ylab = "Frequency")

#--------------------------------------------------------------
# histogram with break points
hist(texas$price, 
   breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400) * 1000,
   right = FALSE,
   main = "Histogram for House Price",
   xlab = "House Price in Dollar",
   ylab = "Frequency")
# histogram without break points
hist(texas$price, 
   main = "Histogram for House Price",
   xlab = "House Price in Dollar",
   ylab = "Frequency")
# compare with
hist(texas$price)
# smallest and largest
min(texas$price)
max(texas$price)

#--------------------------------------------------------------
# summary Statistics
# norc data
norc <- read.csv(file = "norc.csv", stringsAsFactors = FALSE)
# sample size
n <- length(norc$age)
# smallest and largest
min(norc$age)
max(norc$age)
# sample mean
mean(norc$age)
sum(norc$age) / n
# sample variance/standard deviation
var(norc$age)
sd(norc$age)
sqrt( var(norc$age) )
sum( (norc$age - mean(norc$age))^2 ) / (n - 1)
( sum(norc$age^2) - n * mean(norc$age)^2) / (n - 1)
# median
median(norc$age)
a <- sort(norc$age)
a
(a[25] + a[26]) / 2
# quantile
quantile(norc$age, probs = 0.5)
quantile(norc$age, probs = 0.25)
quantile(norc$age, probs = 0.75)

#--------------------------------------------------------------
# box plot of house price
boxplot(texas$price, 
   main = "Boxplot for House Price",
   xlab = "",
   ylab = "House Price in Dollar")
# box plot for Wall Thickness of Aircraft Parts
wall <- c(0.223, 0.193, 0.218, 0.201, 0.231, 0.204,
   0.228, 0.223, 0.215, 0.223, 0.237, 0.226,
   0.214, 0.213, 0.233, 0.224, 0.217, 0.210)
# box plot of house price
boxplot(wall, 
   main = "Boxplot for Wall Thickness",
   xlab = "",
   ylab = "Thinckness")

#--------------------------------------------------------------
# box plot of tree height
trees <- read.csv("trees.csv", stringsAsFactors = FALSE)
head(trees)
# box plot for tree height
boxplot(trees$ht,
   main = "Boxplot for Tree Height",
   xlab = "",
   ylab = "Height in Feet")
# some summary statistics related to box plot
mean(trees$ht)
median(trees$ht)
quantile (trees$ht, probs = 0.25)
quantile (trees$ht, probs = 0.75)
min(trees$ht)
max(trees$ht)


#--------------------------------------------------------------
# two-dimensional contingency tables
a <- table(texas$exter, texas$bed) 
a
prop.table(a) 
prop.table(a, margin = 1) 
b <- prop.table(a, margin = 2) 
# bar plot
barplot(b, main = "", xlab = "# of Bedrooms", ylab = "Percentage")

#--------------------------------------------------------------
# plots for two variables
# box plot
boxplot(price ~ bath, data = texas,
	main = "",
	xlab = "Number of Bathrooms",
	ylab = "House Price in Dollar")
# scatter plot
plot(price ~ age, data = texas,
	main = "",
	xlab = "Age of House in Years",
	ylab = "House Price in Dollar")
# scatter plot & boxplot
plot(price ~ bed, data = texas,
	main = "",
	xlab = "Number of Bedrooms",
	ylab = "House Price in Dollar")
boxplot(price ~ bed, data = texas,
	main = "",
	xlab = "Number of Bedrooms",
	ylab = "House Price in Dollar")




