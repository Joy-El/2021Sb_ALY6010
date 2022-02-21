# Week 5 examples - correlations & regression
# Created by Joy-El Talbot
# For ALY6010
# On 2021-06-24
# Modified on 2021-06-24 by Joy-El Talbot

# Purpose: Highlight how to run correlations & regressions in R

library(ggplot2)
library(dplyr)

iris <- datasets::iris

# Histograms
hist(iris$Sepal.Length, xlab = "Sepal Length",
     main = "Sepal Length Frequencies",
     col="darkgrey", border = "darkgrey")

hist(iris$Sepal.Width, xlab = "Sepal Width", 
     main = "Sepal Width Frequencies",
     col="darkgrey", border="darkgrey")

# Scatterplot (plain)
plot(iris$Sepal.Width, iris$Sepal.Length,
     xlab="Sepal Width", 
     ylab = "Sepal Length",
     main = "Sepal Width vs Sepal Length")

# Scatterplot (grouping by color)
# switching to ggplot for easier legends/color changes
(ggplot(iris)
  + aes(x = Sepal.Width, y = Sepal.Length, color = Species)
  + geom_point(size=3)
  + scale_x_continuous("Sepal Width")
  + scale_y_continuous("Sepal Length")
  + theme_minimal()
  + theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom"))


# correlation coefficient (r)
# get just setosa data
setosa <- iris[iris$Species == "setosa", ]

# Building the procedure table
cormatrix <- data.frame(x = setosa$Sepal.Width,
                        y = setosa$Sepal.Length)
cormatrix$xy <- cormatrix$x * cormatrix$y
cormatrix$x2 <- cormatrix$x^2
cormatrix$y2 <- cormatrix$y^2

head(cormatrix)

n = length(cormatrix$x)
sum_x <- sum(cormatrix$x)
sum_y <- sum(cormatrix$y)
sum_xy <- sum(cormatrix$xy)
sum_x2 <- sum(cormatrix$x2)
sum_y2 <- sum(cormatrix$y2)

# plug & chug
r_top <- (n * sum_xy) - (sum_x * sum_y) 
r_bottom <- sqrt((n * sum_x2 - (sum_x)^2) * (n * sum_y2 - (sum_y)^2))
r <- r_top / r_bottom
r # 0.7425467

# using the cor() function
cor(setosa$Sepal.Width, setosa$Sepal.Length,
    method = "pearson") # 0.7425467

# use cor() in a dplyr group_by/summarize
iris %>%
  group_by(Species) %>%
  summarize(r = cor(Sepal.Width, Sepal.Length)) 


# Significance of linear correlation coefficient
# Calculate "by hand"
r <- cor(setosa$Sepal.Width, setosa$Sepal.Length,
         method = "pearson")
n <- length(setosa$Sepal.Width)
t <- r * sqrt((n-2) / (1 - r^2))
t.crit <- qt(0.95, df = n-2)
if (abs(t) > abs(t.crit)) {
  print("Reject null hypothesis")
} else {
  print("Fail to rejct null hypothesis")
}

# with the cor.test() function
cor.test(setosa$Sepal.Width, setosa$Sepal.Length,
         method = "pearson")

# p-value 6.71e-10 <<< 0.05, therefore
# reject null hypothesis that true correlation (rho) = 0



# linear regression model
fit <- lm(formula = Sepal.Length ~ Sepal.Width, # y ~ x
          data = setosa)

# numerically - Do we trust our model?
summary(fit)

# visually - Do we trust our model?
par(mfrow = c(2,2)) # make a 2x2 grid of plots
plot(fit) # will by default give the 4 key graphs
par(mfrow = c(1,1)) # return to normal

# Plotting the results
# Scatterplot (plain)
plot(setosa$Sepal.Width, setosa$Sepal.Length,
     xlab="Sepal Width", 
     ylab = "Sepal Length",
     main = "Setosa Sepal Width vs Sepal Length")
abline(fit)

# Scatterplot (grouping by color)
# switching to ggplot for easier legends/color changes
(ggplot(setosa)
  + aes(x = Sepal.Width, y = Sepal.Length)
  + geom_point(size=3)
  + stat_smooth(method = "lm", col = "red", se = TRUE)
  + scale_x_continuous("Sepal Width")
  + scale_y_continuous("Sepal Length")
  + ggtitle("Setosa Sepal Width vs Sepal Length")
  + theme_minimal()
  + theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
)

# Custom function
# credit Susane Johnston: https://sejohnston.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

ggplotRegression(fit)

# Predict a y value
newdata <- data.frame(Sepal.Width = 4.0) # match names from original data
predict(object = fit, 
        newdata = newdata,
        interval = "predict", 
        level = 0.95)

