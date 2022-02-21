# Week 5 group exercise - regression model with date data
# Created by Joy-El Talbot
# For ALY6010
# On 2021-06-24
# Modified on 2021-06-24 by Joy-El Talbot

# Purpose: Highlight how to build a regression model with date data

# Question to answer: Given the weather in Portland (or Orono),
#   can we predict if it is likely to be raining in Acadia? 

# NOTE: time series data (such as date date) technically has a whole realm of 
# estimation to itself. Here we make some concessions to model the data with 
# linear regression instead - both for ease of analysis/interpretation and 
# technical ease for the scope of ALY6010.

# See Week5_data_cleanup.R script for how we got from raw noaa data to this summary file

### Working Directory = 2021Sb_ALY6010 folder
WORKINGDIR <- "C:/Users/joyel/Documents/Roux/ALY6010/2021Sb_ALY6010/"
setwd(WORKINGDIR)

### Load Libaries
library(ggplot2)
library(dplyr)
library(tidyr)

### Load data
rain <- utils::read.csv("data/noaa_summarized_rain.csv",
                        stringsAsFactors = FALSE)

head(rain)
unique(rain$station)

rain <- rain[rain$station != "PORTLAND JETPORT ME US", ]

# Create scatterplot of nearby location (x) vs Acadia rain (y)

# remove time of day
rainByDay <- rain %>%
  group_by(date, year, season, dayOfYear, station) %>%
  summarise(inchesRain = sum(inchesRain))

# remove NA rows as NA suggests measurements were missing 
sum(is.na(rainByDay$inchesRain))
rainByDay <- rainByDay[!is.na(rainByDay$inchesRain), ]

head(rainByDay)

# get Orono & Acadia rainfalls as columns (pivot)
rainByDay$station[rainByDay$station == "ORONO 2 ME US"] <- "Orono"
rainByDay$station[rainByDay$station == "ACADIA NATIONAL PARK ME US"] <- "Acadia"

# ASSUMPTION: let's treat NAs as zero rain days 
rainByDay_wide <- rainByDay %>%
  pivot_wider(names_from = station,
              values_from = inchesRain,
              values_fill = 0) # fill missing values with zero

head(rainByDay_wide)

# TODO expand dates to include all missing dates --> rainfall would be zero (assumed)

(ggplot(rainByDay_wide)
  + aes(x = Orono, y = Acadia)
  + geom_point()
  + geom_abline(intercept = 0, slope = 1, color = "magenta")
  )

(ggplot(rainByDay_wide)
  + aes(x = Orono, y = Acadia)
  + geom_jitter(aes(color = factor(year)))
  + geom_abline(intercept = 0, slope = 1, color = "magenta")
)

(ggplot(rainByDay_wide)
  + aes(x = Orono, y = Acadia)
  + geom_jitter(aes(color = factor(season)))
  + geom_abline(intercept = 0, slope = 1, color = "magenta")
)

(ggplot(rainByDay_wide[rainByDay_wide$season == "Spring", ])
  + aes(x = Orono, y = Acadia)
  + geom_jitter(aes(color = factor(year)))
  + geom_abline(intercept = 0, slope = 1, color = "magenta")
)

# TODO: share blogs? about zero-heavy data in modeling


# Determine correlation coefficient (r) and its significance (hint: cor.test())

# ASSUMPTION: limiting our predictions to Spring data
rainSpring <- rainByDay_wide[rainByDay_wide$season == "Spring", ]

cor.test(x = rainSpring$Orono, y = rainSpring$Acadia,
         method = "pearson", 
         conf.level = 0.95)

# r = 0.611 --> Weak positive correlation
# data supports rejecting null hypothesis therefore supports rho not being zero

# Create linear regression (hint: lm())
acadia_lm <- lm(formula = Acadia ~ Orono,
                data = rainSpring) # y ~ x

# Interpret results of linear regression (hint: summary() and plot())
summary(acadia_lm)
stargazer::stargazer(acadia_lm, type = "text")

par(mfrow = c(2,2))
plot(acadia_lm)


# Add linear regression line to your plot
(ggplot(rainByDay_wide[rainByDay_wide$season == "Spring", ])
  + aes(x = Orono, y = Acadia)
  + geom_jitter(aes(color = factor(year)))
  + geom_abline(intercept = acadia_lm$coefficients["(Intercept)"],
                slope = acadia_lm$coefficients["Orono"])
)

# If it is raining heavily (> 2 inches) in your location, how likely is it to be raining in Acadia?

print(paste0("y-prime = ", 
             round(acadia_lm$coefficients["(Intercept)"], 3),
             " + ", 
             round(acadia_lm$coefficients["Orono"], 3),
             "x"))
acadia_lm$coefficients["(Intercept)"] + acadia_lm$coefficients["Orono"]*2
.103 + .8*2

predict(object = acadia_lm,
        newdata = data.frame(Orono = 2),
        interval = "predict",
        level = 0.95)

# it is likely raining in Acadia if I see 2 inches in Orono
# because 95% confidence interval is 0.65 to 2.76 inches

# would rather do this by week...
acadia_lm_weeks <- lm(Acadia ~ Orono + factor(dayOfYear),
                      data = rainSpring)
summary(acadia_lm_weeks) # dummy variables out of control!

# I'm not certain I believe this....
par(mfrow = c(1,1))
plot(rainSpring$Orono, rainSpring$Acadia)
abline(acadia_lm_weeks, col="red")
abline(acadia_lm, col = "cyan")
