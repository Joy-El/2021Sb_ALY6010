# Week 6 Examples - multivariate regression models
# Created by Joy-El Talbot
# For ALY6010
# On 2021-07-01
# Modified on 2021-07-01 by Joy-El Talbot

# Purpose: Highlight regression models with multiple continuous and categorical
#   predictor variables.

### Working Directory = 2021Sb_ALY6010 folder
WORKINGDIR <- "C:/Users/joyel/Documents/Roux/ALY6010/2021Sb_ALY6010/"
setwd(WORKINGDIR)

### Load Libaries
library(ggplot2)
library(dplyr)
library(tidyr)

options(scipen = 99) # suppress scientific notation

### Define some constants
DATADIR <- "data/"

# build results directory
ANALYSISNAME <- "week6_MVRegressionExamples"  # NO spaces!
DATESTAMP <- format(Sys.time(), "%Y-%m-%d")
RESULTSDIR <- paste0("results/",DATESTAMP,"-",ANALYSISNAME,"-results/")
dir.create(RESULTSDIR)

################################################################################
###  Example - Iris Data                                                     ###
###    Predicting flower size (petal length) based on sepal length + width   ###
################################################################################

iris <- datasets::iris

#
## Step 1: (if needed) Summarize data to get numeric values
head(iris)
# We already have the data in a format with continous numeric values for our 
# fields of interest (petal length, sepal length, and sepal width)


#
## Step 2: Create histograms of numeric variables
for (field in c("Sepal.Length", "Sepal.Width", "Petal.Length")) {
  hist(x = iris[,field],
       main = paste0("Distribution of ", field),
       xlab = field)
}

# examine small size outlier in Petal Length
# Does the species relate to the outlier?
(ggplot(iris)
  + aes(x = Petal.Length, fill = Species)
  + geom_histogram()
  )
# Yes - we may need to exclude setosa in our modeling.


#
## Step 3: Create scatterplots of combinations of numeric variables

# Combinations with value we want to predict (Petal Length) as y
plot(x = iris$Sepal.Length,
     y = iris$Petal.Length,
     xlab = "Sepal Length",
     ylab = "Petal Length",
     main = "Iris data - all species")
plot(x = iris$Sepal.Width,
     y = iris$Petal.Length,
     xlab = "Sepal Width",
     ylab = "Petal Length",
     main = "Iris data - all species")
# See some evidence of linear relationships in this data

# Combinations of potential predictors (x values)
plot(x = iris$Sepal.Length,
     y = iris$Sepal.Width,
     xlab = "Sepal Length",
     ylab = "Sepal Width",
     main = "Iris data - all species")
# Don't see much evidence of linear relationships between predictors (good)


#
## Step 4: Identify potential linear relationships
## Step 5: Calculate correlation coefficient (r)
## Step 6: Test significance of linear correlation coefficient

# all at once let's address steps 4-6
r.sepalLength.petalLength <- cor.test(x = iris$Sepal.Length,
                                      y = iris$Petal.Length,
                                      alternative = "two.sided",
                                      conf.level = 0.95,
                                      method = "pearson")
r.sepalLength.petalLength
r.sepalWidth.petalLength <- cor.test(x = iris$Sepal.Width,
                                     y = iris$Petal.Length,
                                     alternative = "two.sided",
                                     conf.level = 0.95,
                                     method = "pearson")
r.sepalLength.sepalWidth <- cor.test(x = iris$Sepal.Length,
                                     y = iris$Sepal.Width,
                                     alternative = "two.sided",
                                     conf.level = 0.95,
                                     method = "pearson")
# combine results into a summary table
cor.result <- data.frame("x" = c("Sepal.Length", "Sepal.Width", "Sepal.Length"),
                         "y" = c("Petal.Length", "Petal.Length", "Sepal.Width"),
                         "p-val" = round(c(r.sepalLength.petalLength$p.value,
                                     r.sepalWidth.petalLength$p.value,
                                     r.sepalLength.sepalWidth$p.value),5),
                         "r" = round(c(r.sepalLength.petalLength$estimate,
                                 r.sepalWidth.petalLength$estimate,
                                 r.sepalLength.sepalWidth$estimate),3))
cor.result
# sepal length and width have significant deviations from a rho=0 with petal length
#   a good sign that these will help predict petal length
#
# sepal length and width have a non-significant deviation from rho=0 between each other
#   a good sign that these variables are not themselves correlated and therefore
#   will add new information to a regression


#
## Step 7: Create a linear regression model
## Step 8: Calculate coefficient of determination (r-squared)
# Going to make a few models so that we can compare
fit.sepalLength <- lm(Petal.Length ~ Sepal.Length, 
                      data = iris)
summary(fit.sepalLength) # adj R-squared = 0.7583; R-squared = 0.7600
fit.sepalWidth <- lm(Petal.Length ~ Sepal.Width, 
                      data = iris)
summary(fit.sepalWidth) # adj R-squared = 0.1780; R-squared = 0.1836

fit.sepalLength.sepalWidth <- lm(Petal.Length ~ Sepal.Length + Sepal.Width,
                                 data = iris)
summary(fit.sepalLength.sepalWidth) # adj R-squared = 0.8659; R-squared = 0.8677
# better fit (R-squared) with BOTH continuous variables
par(mfrow=c(2,2))
plot(fit.sepalLength.sepalWidth)
# some deviation in Normal Q-Q at extremes
# some grouping in Residuals vs Fitted

# let's add our categorical data
fit.sepalLength.sepalWidth.Species <- lm(Petal.Length ~ Sepal.Length + Sepal.Width + factor(Species),
                                         data = iris)
summary(fit.sepalLength.sepalWidth.Species) # adj R-squared = 0.9742; R-squared = 0.9749
# sepal width is no longer significant
par(mfrow=c(2,2))
plot(fit.sepalLength.sepalWidth.Species)
# different Normal Q-Q and Residuals vs Fitted, though same interpretation remains

# let's remove sepal.Width based on results of fit.sepalLength.sepalWidth.Species
fit.sepalLength.Species <- lm(Petal.Length ~ Sepal.Length + factor(Species),
                              data = iris)
summary(fit.sepalLength.Species) # adj R-squared = 0.9744; R-squared = 0.9749
# while our Multiple R-squared remains the same, our adjusted R-squared has increased 
# slightly due to the loss of one of our predictor variables.
par(mfrow=c(2,2))
plot(fit.sepalLength.Species)
# look nearly the same as fit.sepalLength.sepalWidth.Species
par(mfrow=c(1,1))

#
## Step 9: Make point prediction intervals
## Step 10: Interpret results in context

# visualize the raw data and the regression line
# we will have to visualize in chunks because the data has more than 2 dimensions

slope = fit.sepalLength.Species$coefficients['Sepal.Length']
intercept_setosa = fit.sepalLength.Species$coefficients['(Intercept)']
intercept_versicolor = intercept_setosa + fit.sepalLength.Species$coefficients['factor(Species)versicolor']
intercept_virginica = intercept_setosa + fit.sepalLength.Species$coefficients['factor(Species)virginica']


# Petal Length vs Sepal Length
palette(c("cyan", "orange", "purple"))
plot(x = iris$Sepal.Length,
     y = iris$Petal.Length,
     xlab = "Sepal Length",
     ylab = "Petal Length",
     col = iris$Species,
     pch = 19)
legend("bottomright",
       legend = unique(iris$Species),
       col = 1:3,
       pch = 19)
# generic line without species
abline(fit.sepalLength, col = "darkgrey", lwd = 2) 

# setosa line
abline(a = intercept_setosa, 
       b = slope,
       col = "cyan", lwd = 2)
# versicolor line
abline(a = intercept_versicolor, 
       b = slope,
       col = "orange", lwd = 2)
# virginica line
abline(a = intercept_virginica, 
       b = slope,
       col = "purple", lwd = 2)

# Now let's instead subset the data into species
# and create simpler linear regressions
fit.setosa.sepalLength <- lm(Petal.Length ~ Sepal.Length,
                             data = iris[iris$Species == "setosa", ])
fit.versicolor.sepalLength <- lm(Petal.Length ~ Sepal.Length,
                             data = iris[iris$Species == "versicolor", ])
fit.virginica.sepalLength <- lm(Petal.Length ~ Sepal.Length,
                             data = iris[iris$Species == "virginica", ])

# summary results
extract_statistics <- function(fit, x.vals, species) {
  result <- data.frame("Species" = species,
                       "Group" = x.vals,
                       "Adj.R.sq" = round(summary(fit)$adj.r.squared, 4))
  return(result)
}

fit.result <- data.frame() # initialize data.frame
fit.result <- rbind(fit.result,
                    extract_statistics(fit.sepalLength, "Sepal Length", "All"))
fit.result <- rbind(fit.result,
                    extract_statistics(fit.sepalLength.Species, "Sepal Length + Species", "All"))
fit.result <- rbind(fit.result,
                    extract_statistics(fit.setosa.sepalLength, "Sepal Length", "Setosa"))
fit.result <- rbind(fit.result,
                    extract_statistics(fit.versicolor.sepalLength, "Sepal Length", "Versicolor"))
fit.result <- rbind(fit.result,
                    extract_statistics(fit.virginica.sepalLength, "Sepal Length", "Virginica"))
fit.result


# Petal Length vs Sepal Length
palette(c("cyan", "orange", "purple"))
plot(x = iris$Sepal.Length,
     y = iris$Petal.Length,
     xlab = "Sepal Length",
     ylab = "Petal Length",
     col = iris$Species,
     pch = 19)
legend("bottomright",
       legend = unique(iris$Species),
       col = 1:3,
       pch = 19)
# generic line without species (Petal.Length ~ Sepal.Length)
abline(fit.sepalLength, col = "darkgrey", lwd = 2) 

# lines of Petal.Length ~ Sepal.Length + factor(Species)
# setosa line
abline(a = intercept_setosa, 
       b = slope,
       col = "cyan", lwd = 2)
# versicolor line
abline(a = intercept_versicolor, 
       b = slope,
       col = "orange", lwd = 2)
# virginica line
abline(a = intercept_virginica, 
       b = slope,
       col = "purple", lwd = 2)

# lines of Petal.Length ~ Sepal.Length fit by species
# setosa line
abline(fit.setosa.sepalLength,
       col = "cyan", lwd = 2, lty = 2) # dashed line with lty = 2
# versicolor line
abline(fit.versicolor.sepalLength,
       col = "orange", lwd = 2, lty = 2)
# virginica line
abline(fit.virginica.sepalLength,
       col = "purple", lwd = 2, lty =2)

fit.result
