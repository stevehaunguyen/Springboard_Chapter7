#  Introduction
##

# Learning objectives:
##     Learn the R formula interface
##     Specify factor contrasts to test specific hypotheses
##     Perform model comparisons
##     Run and interpret variety of regression models in R

## Set working directory
##

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
# setwd("~/Desktop/Rstatistics")
# setwd("C:/Users/dataclass/Desktop/Rstatistics")

##   You might also start by listing the files in your working directory
setwd("~/Springboard Projects/Chapter 7 - Machine Learning/linear_regression/linear_regression")
getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

## Load the states data
##

# read the states data
states.data <- readRDS("dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

## Linear regression
##

## Examine the data before fitting models
##

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
##

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
##

##   Linear regression models can be fit with the `lm()' function
##   For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
##

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
##

##   OK, we fit our model. Now what?
##   Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##  Use function methods to get more information about the fit

confint(sat.mod)
# hist(residuals(sat.mod))

## Linear Regression Assumptions
##

##   Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##  Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
##

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise: least squares regression
##

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

## Interactions and factors
##

## Modeling interactions
##

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
##

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
##

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
##

##   Use the states data set.

setwd("~/Springboard Projects/Chapter 7 - Machine Learning/linear_regression/linear_regression/dataSets")
# read the states data
states.data <- readRDS("states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

## Use the states.rds data set. Fit a model predicting energy consumed per capita (energy) from the percentage of 
#residents living in metropolitan areas (metro).
states.metro.energy <- subset(states.data, select = c("metro", "energy"))

#Be sure to

#Examine/plot the data before fitting the model
summary(states.metro.energy)
#There is an NA which will not calculate a correlation, so removing single NA from energy and metro
states.metro.energy <- na.omit(states.metro.energy)
cor(states.metro.energy)
plot(states.metro.energy)

#Print and interpret the model summary
model.metro.energy <- lm(energy ~ metro, data = states.data)
summary(model.metro.energy)
#Since probability that there is a strong relationship of energy and metro due to chance is very small, 
#we can assume there is a relationship between metro and energy. However, since the R-squared is very small,
#metro is not the only variable that predicts energy well

#plot the model to look for deviations from modeling assumptions
model.metro.energy.resid <- resid(model.metro.energy)
plot(states.metro.energy$metro, model.metro.energy.resid, ylab = "Residuals", xlab = "metro", main = "Residual vs Fitted of metro and energy") 
abline(0, 0)
qqnorm(model.metro.energy.resid)
#Residual vs fitted plot does not suggest any relationship. Can assume residuals are independent
#QQ plot with a few outliers suggest normality of residuals

#Select one or more additional predictors to add to your model and repeat steps 1-3.
states.new <- subset(states.data, select = c("metro", "green", "energy"))
states.new <- na.omit(states.new)
cor(states.new)
plot(states.new)
#Model with green as only independent variable
model.new1 <- lm(energy ~ green, data = na.omit(states.data))
summary(model.new1)
model.new.resid1 <- resid(model.new1)
qqnorm(model.new.resid1)
#Model with green and metro as independent variables
model.new2 <- lm(energy ~ metro + green, data = na.omit(states.data))
summary(model.new2)

model.new.resid2 <- resid(model.new2)
qqnorm(model.new.resid2)
#Is this model significantly better than the model with metro as the only predictor?
 #Using green only, rather than metro + green model, is a better model. Adjusted R-squared is relatively the same
 #using one less independent variable meaning more simple model.


##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

#Model with green and senate as independent variables
model.add1 <- lm(energy ~ green + senate, data = na.omit(states.data))
summary(model.add1)

model.add.resid1 <- resid(model.add1)
qqnorm(model.add.resid1) #Testing for normality

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

#Model with green, senate, and region as independent variables
model.add2 <- lm(energy ~ green + senate + region, data = na.omit(states.data))
summary(model.add2)

model.add.resid2 <- resid(model.add2)
qqnorm(model.add.resid2)
anova(model.add2)

#There are differences in coefficients across the four regions, but the significance of North East, South, West,
#and Midwest are not significant enough to be a predictor for energy
