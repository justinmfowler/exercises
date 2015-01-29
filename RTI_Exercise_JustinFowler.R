# RTI Exercise for Data Scientist 3 application
# Justin Fowler 01/29/2015

# Possible libraries to use
library(aod)
# library(foreign)
library(gmodels)
# library(vcd)
# library(epiR)
# library(NSM3)
# library(MASS)
library(rms)
library(ggplot2)
# library(lsmeans)
# library(contrast)
library(pROC)
# library(caTools)
# library(lmtest)

# Reading in data
# records is the original file called 'records' that was provided (not used)
# records_flat is the flattened file containing all tables from the database
# csvs were retrieved from SQLite
records <- read.csv("C:/MSA/Spring/Job Apps/RTI/records.csv")
View(records)
records_flat <- read.csv("C:/MSA/Spring/Job Apps/RTI/records_flat.csv")
View(records_flat)

# Create factors for categorical variables
records_flat$over_50k <- factor(over_50k)
records_flat$workclass <- factor(workclass)
records_flat$education_level <- factor(education_level)
records_flat$marital_status <- factor(marital_status)
records_flat$occupation <- factor(occupation)
records_flat$relationship <- factor(relationship)
records_flat$race <- factor(race)
records_flat$sex <- factor(sex)
records_flat$country <- factor(country)

# Attach file for easier use
attach(records_flat)
names(records_flat)

# Checking various descriptive statistics
table(over_50k)

hist(age)
hist(hours_week)

table(workclass)
table(education_level, education_num) # these 2 variables provide the same information, don't use both
table(marital_status, relationship)
table(occupation)
table(sex)
table(race)
table(country)

table(over_50k, capital_gain)
table(over_50k, capital_loss) # the capital variables seem to create separation issues

# I'm not as familiar with R as I'd like
# I did what I could in the time I had, and will comment on things I missed
# Ideally I would create training, validation, and testing sets here
# I should also decide what to do with missing values (?)
# I should also check the logistic regression assumption for the continuous variables
# More time should be spent checking for interactions or collinearity too, I don't use any interactions

# Here I'm attempting to create a logistic regression
# I leave out education_num because it is redundant with education_level (perfect multicollinearity)
# I also leave out capital_gain and capital_loss due to separation issues
# Some of the variables could also possibly be combined or recoded for better results
# Use the summary function to see the parameter estimates
Logit.Model <- glm(over_50k ~ age + workclass + education_level + marital_status + occupation + relationship + race + sex + hours_week, family="binomial")
summary(Logit.Model)

# Many of the variables have signicicance at alpha=0.05
# However, a tighter significance level should be used
# To evaluate the categorical variables, I would want to obtain the type III analysis of effects (I tried, no luck)

# I attempted variable selection techniques, but it didn't seem to help
# Logit.ModelI <- glm(over_50k ~ 1, family=binomial(logit))
# step(Logit.ModelI, ~ age + workclass + education_level + marital_status + occupation + race + sex + hours_week + country, direction="forward")

# Obtaining and plotting ROC curve
# 0.88 area under the curve
Model.ROC <- roc(Logit.Model$y, Logit.Model$fitted)
print(Model.ROC)
plot(Model.ROC)

# With more time I would like to dive into further diagnostics as well
