# ALY6010 - 81014 - Spring B 2021
# Module 4 - Check Your Understanding - Solution
# created: 2021-06-18
# by: Joy-El Talbot
# updated: 2021-06-18
# by: Joy-El Talbot

#*************** Prompt ************************************
#*
#* The workers at two plants were given lead blood tests as 
#* part of health monitoring. 
#* Compare their averages at 90% significance.
#* 
#* Plant 1: 155 133 122 193 145 143 142
#* Plant 2: 142 128 123 175 133
#* 
#***********************************************************

# load the data
plant1 <- c(155, 133, 122, 193, 145, 143, 142)
plant2 <- c(142, 128, 123, 175, 133)


#***** Question 1: What is the null hypothesis?
#*
#* H0: mu(plant1) = mu(plant2) AKA mu(plant1) - mu(plant2) = 0
#*


#***** Question 2: Is this a one or two-tailed test?
#*
#* Option 1: 
#* H1: mu(plant1) != mu(plant2)
#* Two-tailed because we are not asked if one plant is doing
#* better or worse than the other. If we reject the null, we 
#* could then compare the sample means to say which sample is 
#* higher/lower and that our test supports the population means 
#* being different.
#* 
#* Option 2: 
mean(plant1) # 148 (with rounding)
mean(plant2) # 140
#* H1: mu(plant1) > mu(plant2)
#* One-tailed because, given no specific mandate in the prompt, 
#* we might see what the sample means are and use that to determine
#* in which direction a one-tailed test should go. 
#* 
#* For the rest of the assignment, let's go with option 2
#* H1: mu(plant1) > mu(plant2)
#* 


#***** Question 3: What is the alpha value?
#*
#* alpha = 0.1
#* because we want a significance of 90% and alpha = 1 - significance
#* or alpha = 1 - 0.9 in this particular case
#* 


#***** Question 4: What is the critical value?
#*
#* We need to know a few things first...
#* 1) using a t-test because we don't know the population standard deviation
#* 2) degrees of freedom = 4 
df <- min(length(plant1) - 1, length(plant2) - 1)
df # 4
#* 
#* For the two-sided test, our critical value would be:
crit <- qt(p = 0.1, # our alpha
           df = df,
           lower.tail = FALSE) # because we want a right-tail test
crit # 1.5332
#* critical value = 1.5332
#* 


#***** Question 5: What is the test statistic?
#* 
#* Our samples are INDEPENDENT (aka UNPAIRED) 
#* t = ((xbar_1 - xbar_2) - (mu_1 - mu_2)) / sqrt(s2_1/n_1 + s2_2/n_2)
xbar_1 <- mean(plant1)
xbar_2 <- mean(plant2)
s2_1 <- sd(plant1)^2 # to square it :-)
s2_2 <- sd(plant2)^2
n_1 <- length(plant1)
n_2 <- length(plant2)
t <- ((xbar_1 - xbar_2) - 0) / sqrt(s2_1/n_1 + s2_2/n_2)
t # 0.5864
#* test statistic = 0.5864
#* 


#***** Interpretation...
#* test statistic (0.5864) < critical value (1.5332)
#* so we fail to reject the null hypothesis that the 
#* two plants have different mean levels of blood lead
#* in their workers
#* 


#***** Question 6: What is the p-value?
#* 
#* Let's use t.test() to calculate this one
result <- t.test(x = plant1,
                 y = plant2,
                 alternative = "greater",
                 paired = FALSE,
                 var.equal = TRUE, # normally would do FALSE for 
                                   # more conservative estimate
                                   # Doing TRUE to best match the 
                                   # hand calculation from above
                 conf.level = 0.9)
result # to see the whole output; note the t value is slightly different
       # because the underlying formulas are likely a little bit different
result$p.value # 0.288
#* p-value: 0.288
#* fail to reject the null hypothesis because 0.288 > 0.1
#* 
