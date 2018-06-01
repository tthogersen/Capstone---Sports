# packages:

library(cluster)
library(fpc)
library(MASS)
library(car)
library(lattice)
library("PerformanceAnalytics")
#library(mclust)
library(DAAG) # K-fold cross-validation
#install.packages("naivebayes")
library(car)
#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
#install.packages("ResourceSelection") # enable Hosmer and Lemeshow goodness of fit (GOF) test
library(ResourceSelection)
library(rpart)
library(ggplot2)

# Imported dataset from excel#

# Packages:

library(readxl)
OffNFLSalary <-
  read_excel("School/DA 485/NFLPlayerSalaryData.xlsx",
             sheet = "2017OffensivePlayerSalary_New")
View(OffNFLSalary)


OffNFLSalary[is.na(OffNFLSalary)] = 0

INT[is.na(INT)] = 0

# Convert data set to data.frame#

OffNFLSalary = data.frame(OffNFLSalary)
attach(OffNFLSalary)
head(OffNFLSalary)

# Scatterplot / Correlation Matrices from the car Package

corOffNFLSalary = cor(data.frame(OffNFLSalary[sapply(OffNFLSalary, is.numeric)]))

chart.Correlation(salary, histogram = TRUE, pch = 19)



# Basic Correlation  Matrix

INT[is.na(INT)] = 0

OFFSalaryCor = cor(data.frame(OffNFLSalary[sapply(OffNFLSalary, is.numeric)]))
OFFSalaryCor


# Basic Scatterplot Matrix
pairs(
  ~ Salary + Rank + GP + GS + Snaps + Snaps_percent + ATT + ATT_Comp +
    Comp_Percent  + YDS  + YDS.ATT + TD + INT + Fum,
  data = OffNFLSalary,
  main = "Simple Scatterplot Matrix"
)

names(OffNFLSalary)

# NOrmilizing Varaiables:

par(mfrow = c(4, 4))


## Salary Dependent Variable

#Before:

hist(OffNFLSalary$Salary,
     xlab = "NFL Salary $ million",
     main = "Historam of NFL Salary
     ",
     col = "green")
#After

sqsalary = sqrt((OffNFLSalary$Salary))

hist(sqsalary,
     xlab = "Normalized NFL Salary $ million",
     main = "Historam of NFL Salary
     ",
     col = "blue")


## Rank - Independant Variable
#Before:

hist(OffNFLSalary$Rank,
     xlab = "NFL Postion Rank",
     main = "Historam of Postion Rank
     ",
     col = "green")
#After
sqrank = sqrt(OffNFLSalary$Rank)
hist(
  sqrank,
  xlim = c(0, 20),
  xlab = "Normalized NFL Rank",
  main = "Historam of NFL Rank",
  col = "blue"
)


## GP (Games Played) - Independant Variable
#Before:

hist(OffNFLSalary$GP,
     xlab = "NFL Games Played",
     main = "Histogram  of Games Played
     ",
     col = "green")
#After

GP = (OffNFLSalary$GP)
hist(GP,
     xlab = "Normalized NFL GP ",
     main = "Histogram  of NFL Games Played",
     col = "blue")


## GS (Games Started) - Independant Variable
#Before:

hist(OffNFLSalary$GS,
     xlab = "NFL Games Stared",
     main = "Histogram of Games Started
     ",
     col = "green")
#After

GS = (OffNFLSalary$GS)
hist(GS,
     #xlim = c(0,1.5),
     #ylim = c(0, 100),
     xlab = "Normalized NFL Games Started",
     main = "Histogram of NFL Games Started",
     col = "blue")

## Snaps (Games Snaps) - Independant Variable

names(OffNFLSalary)
#Before:

hist(OffNFLSalary$Snaps,
     xlab = "NFL Snaps",
     main = "Histogram ofSnaps
     ",
     col = "green")
#After

sqsnaps = sqrt(Snaps)
hist(
  sqsnaps,
  probability = FALSE,
  xlim = c(0, 35),
  xlab = "Normalized Snaps",
  main = "Histogram of Snaps",
  col = "blue"
)

## Snap_Percnt (Snap Percentage) - Independant Variable
#Before:

hist(
  OffNFLSalary$Snaps_percent,
  xlab = "NFL Snaps %",
  main = "Histogram of NFL Snaps %
  ",
  col = "green"
)
#After

sqsnapsper = sqrt(Snaps_percent)
hist(
  sq_snaps_per,
  probability = FALSE,
  xlab = "Normalized NFL Snaps %",
  main = "Histogram of NFL Snaps %",
  col = "blue"
)

## Plays (Total Plays) - Independant Variable

names(OffNFLSalary)
#Before:

hist(OffNFLSalary$ATT,
     xlab = "NFL Total Attempts",
     main = "Histogram of NFL Total Attempts
     ",
     col = "green")
#After

nlatt = log(ATT)
hist(
  nlatt,
  probability = FALSE,
  xlim = c(0, 7),
  xlab = "Normalized Total Attempts",
  main = "Histogram of NFL Total Attempts",
  col = "blue"
)

## Plays_Comp (Total Plays Completed) - Independant Variable

#Before:

hist(
  OffNFLSalary$ATT_Comp,
  xlab = "NFL Attempts Completed",
  main = "Histogram of NFL Attempts Completed
  ",
  col = "green"
)
#After

nlattcom = log(ATT_Comp)
hist(
  nlattcom,
  probability = FALSE,
  xlab = "Normalized Attempts Completed",
  main = "Histogram of Total Attempts Completed",
  col = "blue"
)

## Comp_Percent (Completion Percentage) - Independant Variable
#Before:

hist(
  OffNFLSalary$Com_Percent,
  xlab = "NFL Completion %",
  main = "Histogram of NFL Completion %
  ",
  col = "green"
)

#After
nlcompPer = log(Comp_Percent)
hist(
  nlcompPer,
  probability = FALSE,
  xlim = c(2.5, 5),
  xlab = "Normalized Completion %",
  main = "Histogram of NFL Completion %",
  col = "blue"
)

## YDS (Total Yardage) - Independant Variable
#Before:

hist(OffNFLSalary$YDS,
     xlab = "NFL Total Yardage",
     main = "Histogram of NFL Total Yardage
     ",
     col = "green")
#After
nlyds = log(YDS)
hist(
  nlyds,
  probability = FALSE,
  xlim = c(0, 10),
  xlab = "Normalized YDS",
  main = "Histogram of NFL Total Yardage",
  col = "blue"
)

## YDS.ATT (YDS/AT1T) - Independant Variable
#Before:

hist(OffNFLSalary$YDS.ATT,
     xlab = "NFL Yards per Attempt",
     main = "Histogram of NFL Total Yardage per Attempted
     ",
     col = "green")
#After
YDS.ATT = (YDS.ATT)
hist(
  YDS.ATT,
  probability = FALSE,
  #xlim = c(2, 5),
  xlab = "Normalized Yards per Attempt",
  main = "Histogram of NFL Total Yardage per Attempted",
  col = "blue"
)

## TD (Touch Downs) - Independant Variable
#Before:

hist(OffNFLSalary$TD,
     xlab = "NFL Touch Downs",
     main = "Histogram of NFL Touch Downs
     ",
     col = "green")
#After
nltd = log(TD)
hist(nltd,
     xlab = "Normalized TD",
     main = "Histogram of NFL Touch Downs",
     col = "blue")

## INT (Interception) - Independant Variable

#before
hist(OffNFLSalary$INT,
     xlab = "NFL Interception",
     main = "Histogram of NFL Interception
     ",
     col = "green")
#After

nlint = log(INT)
hist(
  nlint,
  probability = FALSE,
  #xlim = c(2, 5),
  xlab = "Normalized INT",
  main = "Histogram of NFL Interception",
  col = "blue"
)

## FUM (Fumbles) - Independant Variable

names(OffNFLSalary)
#before
hist(OffNFLSalary$Fum,
     xlab = "NFL Fumbles",
     main = "Histogram of NFL Fumbles
     ",
     col = "green")
#After

nlfum = log(Fum)
hist(
  nlfum,
  probability = FALSE,
  #xlim = c(2, 5),
  xlab = "Normalized Fumbles",
  main = "Histogram of NFL Fumbles",
  col = "blue"
)

par(mfrow = c(1, 1))

# Cbind new normalized da

# New Correlation Matrix with Normalized data

# removed NA value column

nlatt[is.infinite(nlatt)] = 0
nlattcom[is.infinite(nlattcom)] = 0
nlcompPer[is.infinite(nlcompPer)] = 0
nlyds[is.infinite(nlyds)] = 0
nltd[is.infinite(nltd)] = 0
nlint[is.infinite(nlint)] = 0
nlfum[is.infinite(nlfum)] = 0

nlatt[is.na(nlatt)] = 0
nlattcom[is.na(nlattcom)] = 0
nlcompPer[is.infinite(nlcompPer)] = 0
nlyds[is.na(nlyds)] = 0
nltd[is.na(nltd)] = 0
nlint[is.na(nlint)] = 0
nlfum[is.na(nlfum)] = 0

nlatt[is.nan(nlatt)] = 0
nlattcom[is.nan(nlattcom)] = 0
nlcompPer[is.nan(nlcompPer)] = 0
nlyds[is.nan(nlyds)] = 0
nltd[is.nan(nltd)] = 0
nlint[is.nan(nlint)] = 0
nlfum[is.nan(nlfum)] = 0

Norm_OFF_Salary = data.frame(
  cbind(
    sqrank,
    
    sqsalary,
    GP,
    GS,
    sqsnaps,
    sqsnapsper,
    nlatt,
    nlattcom,
    nlcompPer,
    nlyds,
    YDS.ATT,
    nltd,
    nlint,
    nlfum
  )
)

# removed NA value column

nlatt[is.infinite(nlatt)] = 0
nlattcom[is.infinite(nlattcom)] = 0
nlcompPer[is.infinite(nlcompPer)] = 0
nlyds[is.infinite(nlyds)] = 0
nltd[is.infinite(nltd)] = 0
nlint[is.infinite(nlint)] = 0
nlfum[is.infinite(nlfum)] = 0

nlatt[is.na(nlatt)] = 0
nlattcom[is.na(nlattcom)] = 0
nlcompPer[is.infinite(nlcompPer)] = 0
nlyds[is.na(nlyds)] = 0
nltd[is.na(nltd)] = 0
nlint[is.na(nlint)] = 0
nlfum[is.na(nlfum)] = 0

nlatt[is.nan(nlatt)] = 0
nlattcom[is.nan(nlattcom)] = 0
nlcompPer[is.nan(nlcompPer)] = 0
nlyds[is.nan(nlyds)] = 0
nltd[is.nan(nltd)] = 0
nlint[is.nan(nlint)] = 0
nlfum[is.nan(nlfum)] = 0

head(Norm_OFF_Salary)
Norm_OFF_Salary = data.frame(Norm_OFF_Salary)
attach(Norm_OFF_Salary)
summary(Norm_OFF_Salary)


# Cor and Matrix plot with normalized data

NFLOffSalary_Cor = cor(Norm_OFF_Salary)
NFLOffSalary_Cor

#install.packages("PerformanceAnalytics")


#library("PerformanceAnalytics")
Norm_OFF_Salary[is.na(Norm_OFF_Salary)] = 0

chart.Correlation(Norm_OFF_Salary, histogram = TRUE, pch = 19)


# Setting up lm for initial modeling
## linear models are fit using the lm( ) function. The form of the lm function is
###lm(formula, family=familytype(link=linkfunction), data=)


# Regression Tree with library(rpart)
# used this information with matrix plot to build initial model


sink(
  "School/DA 485/regression Tree.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)

# grow tree
treefit <-
  rpart(
    sqsalary ~ POS + sqrank + GP + GS + sqsnapsper + nlatt + nlattcom +
      nlcompPer + nlyds + YDS.ATT + nltd + nlint + nlfum ,
    method = "anova",
    data = Norm_OFF_Salary
  )

print(printcp(treefit)) # display the results
print(plotcp(treefit)) # visualize cross-validation results
print(summary(treefit)) # detailed summary of split
sink()

# create additional plots
par(mfrow = c(1, 2)) # two plots on one page
rsq.rpart(treefit) # visualize cross-validation results

# plot tree
par(mfrow = c(1, 1))

plot(treefit, uniform = TRUE,
     main = "Regression Tree for Offensive NFL Salary ")
text(treefit,
     use.n = TRUE,
     all = TRUE,
     cex = .8)


# getting the names of all variables
names(Norm_OFF_Salary)

# Creating lm model

# Check Cor matrix for collinearity and correlation
sink(
  "School/DA 485/NFLOffSalary_Cor.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)
print(NFLOffSalary_Cor)
sink()

# choose the following variables due to least amount of collinearity form the correlation matrix

## Created lm Model 1
fit_Off_NFL_Salary = lm(sqsalary ~ GP + GS + nlatt + nlattcom +
                          nlcompPer + nlyds + YDS.ATT + nltd,
                        data = Norm_OFF_Salary)
sink(
  "School/DA 485/lm_Summary_AOV.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)
print(summary(fit_Off_NFL_Salary))
print(coefficients(fit_Off_NFL_Salary))# model coefficients
print(confint(fit_Off_NFL_Salary, level = 0.95)) # CIs for model parameters
print(fitted(fit_Off_NFL_Salary)) # predicted values
print(residuals(fit_Off_NFL_Salary)) # residuals
print(anova(fit_Off_NFL_Salary)) # anova table
print(vcov(fit_Off_NFL_Salary)) # covariance matrix for model parameters
print(influence(fit_Off_NFL_Salary)) # regression diagnostics

sink()

# Stepwise Regression for NFL Salary information

# AIC Stepwise - Both
aic_both_nfl_Salary = stepAIC(fit_Off_NFL_Salary, direction = "both")


# stepwise - both
step_both_nfl_Salary = step(fit_Off_NFL_Salary, direction = "both")

#output: for stepwise step and AIC

sink(
  "School/DA 485/stepwise_aicstepwise.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)
print(step_both_nfl_Salary)
print(aic_both_nfl_Salary)
sink()

# new Final Data model with stepwise/aic output

fit2_Off_NFL_Salary = lm(formula = sqsalary ~ GS + nlatt + nlattcom + YDS.ATT + nltd,
                         data = Norm_OFF_Salary)

sink(
  "School/DA 485/mod2_lm_Summary_AOV.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)

Norm_OFF_Salary[is.na(Norm_OFF_Salary)] = 0
nl_td[is.infinite(nl_td)] = 0
INT[is.infinite(INT)] = 0
nl_fum[is.infinite(nl_fum)] = 0


print(summary(fit2_Off_NFL_Salary)) # Show summary results
print(coefficients(fit2_Off_NFL_Salary)) # model coefficients
print(confint(fit2_Off_NFL_Salary, level = 0.95))# CIs for model parameters
print(fitted(fit2_Off_NFL_Salary)) # predicted values
print(residuals(fit2_Off_NFL_Salary))# residuals
print(anova(fit2_Off_NFL_Salary))# anova table
print(vcov(fit2_Off_NFL_Salary)) # covariance matrix for model parameters
print(influence(fit2_Off_NFL_Salary)) # regression diagnostics

sink()

# diagnostic plots

layout(matrix(c(1, 2, 3, 4), 2, 2)) # optional 4 graphs/page
plot(model3)


# Bootstrap Measures of Relative Importance (100 samples)
# library(relaimpo)

sink(
  "School/DA 485/NFL Bootstrap Measures of Relative Importance.txtf",
  type = "output",
  append = FALSE,
  split = TRUE
)

# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(
  model3,
  type = c("lmg", "last", "first", "pratt"),
  rela = TRUE
)

# Bootstrap Measures of Relative Importance (1000 samples)
NLF_booth <-
  boot.relimp(
    model3,
    b = 1000,
    type = c("lmg",
             "last", "first", "pratt"),
    rank = TRUE,
    diff = TRUE,
    rela = TRUE
  )
booteval.relimp(NLF_booth) # print result
plot(booteval.relimp(NLF_booth, sort = TRUE)) # plot result

par(mfrow=c(2,1))

# Normality of Residuals
# qq plot for studentized resid
qqPlot(model3, main = "QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(model3)
hist(sresid, freq = FALSE,
     main = "Distribution of NFL Offensive Salary")
xfit <- seq(min(sresid), max(sresid), length = 40)
yfit <- dnorm(xfit)
lines(xfit, yfit)


## OUTLIERS
# Assessing Outliers
#library(car)
outlierTest(fit2_Off_NFL_Salary) # Bonferonni p-value for most extreme obs
qqPlot(fit2_Off_NFL_Salary, main="QQ Plot") #qq plot for studentized resid
leveragePlots(fit2_Off_NFL_Salary) # leverage plot



##OUTLIERS
# Influential Observations
par(mfrow = c(1, 1))
# added variable plots
av.Plots(fit2_Off_NFL_Salary)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4 / ((nrow(fit2_Off_NFL_Salary) - length(fit2_Off_NFL_Salary$coefficients) - 2))
plot(fit2_Off_NFL_Salary, which = 4, cook.levels = cutoff)
# Influence Plot
influencePlot(fit2_Off_NFL_Salary,
              id.method = "identify",
              main = "Influence Plot",
              sub = "Circle size is proportial to Cook's Distance")

# Removing outlier
outlierdata <-Norm_OFF_Salary[-c(6,7,13, 15, 16,21,22,31,41,46,64,211,214,225, 247,376,377,400,408,454,466), ]

model3 <- lm(formula = sqsalary ~ GS + nlattcom + nltd,
             data = outlierdata)
summary(model3)

##OUTLIERS
# Influential Observations
par(mfrow = c(1, 1))
# added variable plots
av.Plots(model3)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4 / ((nrow(model3) - length(model3$coefficients) - 2))
plot(model3, which = 4, cook.levels = cutoff)
# Influence Plot
influencePlot(model3,
              id.method = "identify",
              main = "Influence Plot",
              sub = "Circle size is proportial to Cook's Distance")


# Evaluate Collinearity
vif(model3) # variance inflation factors
sqrt(vif(model3)) > 2 # problem?

install.packages("gvlma")
library(gvlma)
gvlma(model3)


viflm = lm(sqsalary ~ sqrank + GP + GS +  
             nlcompPer +  YDS.ATT + nltd + nlint + nlfum ,
           data = Norm_OFF_Salary)
vif(viflm)


summary(viflm)

stepAIC(viflm)

viflm2 = lm(formula = sqsalary ~ sqrank + GS + nltd + nlint, data = Norm_OFF_Salary)


summary(viflm2)