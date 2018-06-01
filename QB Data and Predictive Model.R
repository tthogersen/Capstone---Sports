#install.packages("ISLR") - used to fix x must be numeric issue
# load the libraries
library(caret)
library(klaR)
library(ISLR)
library(DAAG)
library(relaimpo) #- used to for relative importacelibrary(car)
library(Hmisc) #for rcorr function
library(MASS)
library(rsample)  # data splitting
library(dplyr)    # data transformation
library(ggplot2)  # data visualization
library(caret)    # implementing with caret
library(h2o)      # implementing with h2o
library(cluster)
library(fpc)
library(MASS)
library(car)
library(lattice)
library(PerformanceAnalytics)
library(lars)
library(arm)
library(rpart)
library(openxlsx)

par(mfrow = c(1, 1))

# Subsetting data into 2 different position groups QB and WR

# Packages:

library(readxl)
OffNFLSalary <-
  read_excel("School/DA 485/NFLPlayerSalaryData.xlsx",
             sheet = "2017OffensivePlayerSalary_New")
View(OffNFLSalary)


head(OffNFLSalary)
OffNFLSalary = data.frame(OffNFLSalary)
attach(OffNFLSalary)
summary(OffNFLSalary)

View(Norm_OFF_Salary)

# Cbind new normalized da

# New Correlation Matrix with Normalized data

NFLOffSalary = data.frame(
  cbind(
    sqrank,
    POS,
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
head(NFLOffSalary)

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

head(NFLOffSalary)
NFLOffSalary = data.frame(NFLOffSalary)
attach(NFLOffSalary)
summary(NFLOffSalary)

## creating  table with POS variables so I will be able to subset by Position

## subsetting QB postion from new data table based upon POS varaible

QB_Data <- subset(NFLOffSalary, V2 == 'QB')
QB_Data <- QB_Data[c(-2)]

View(QB_Data)
QB_Data = data.matrix(QB_Data)
QB_Data = data.frame(QB_Data)
head(QB_Data)
attach(QB_Data)


# Scatterplot / Correlation Matrices from the car Package

corQB_Data = cor(data.frame(QB_Data[sapply(QB_Data, is.numeric)]))
corQB_Data
chart.Correlation(sq, histogram = TRUE, pch = 19)

POS[is.na(POS)] = 0


# Setting up lm for initial modeling
## linear models are fit using the lm( ) function. The form of the lm function is
###lm(formula, family=familytype(link=linkfunction), data=)


# Regression Tree with library(rpart)
# used this information with matrix plot to build initial model

sink(
  "School/DA 485/QB regression Tree.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)

# grow tree
names(QB_Data)
QBtreefit <-
  rpart(
    sqsalary ~ sqrank + GP + GS + sqsnaps + sqsnapsper + nlatt + nlattcom +
      nlcompPer + nlyds +  nltd + nlint + nlfum ,
    method = "anova",
    data = QB_Data
  )

print(printcp(QBtreefit)) # display the results
print(plotcp(QBtreefit)) # visualize cross-validation results
print(summary(QBtreefit)) # detailed summary of split
sink()

# create additional plots
par(mfrow = c(1, 2)) # two plots on one page
rsq.rpart(QBtreefit) # visualize cross-validation results

# plot tree
par(mfrow = c(1, 1))

plot(QBtreefit, uniform = TRUE,
     main = "Regression Tree for QB NFL Salary ")
text(QBtreefit,
     use.n = TRUE,
     all = TRUE,
     cex = .8)

# Fitting new LM model for QB
names(QB_Data)
View(QB_Data)

QB_Data = data.matrix(QB_Data)
QB_Data = data.frame(QB_Data)
# Creating my LM Model using the tree and matrix plot info

sink(
  "School/DA 485/GLM&LM_QB_FitMod.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)


QBmod1 = lm(
  sqsalary ~  GS  + nlatt + nlcompPer+nltd+ sqsnapsper,
  data = QB_Data)
print(summary(QBmod1))

sink()

coefficients(QBmod1) # model coefficients
confint(QBmod1, level = 0.95) # CIs for model parameters
fitted(QBmod1) # predicted values
residuals(QBmod1) # residuals
anova(QBmod1) # anova table
vcov(QBmod1) # covariance matrix for model parameters
influence(QBmod1) # regression diagnostic

#Stepwise Variable selection

sink(
  "School/DA 485/GLM&LM_Stepwise_QB.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)

# Stepwise Regression AIC LMM
print(QBStep_LM <- stepAIC(QBmod1, direction = "both"))
print(QBStep_LM$anova) # display results
sink()

# Model 2 after stepwise selection

sink(
  "School/DA 485/FinalModelResults.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)

QB_Data[is.na(QB_Data)] = 0


QBmod2 = lm(formula = sqsalary ~ nltd, data = QB_Data)

QB_Data[is.na(QB_Data)] = 0

#Model 3 - after removing outliers

QBmod3 = lm(formula = sqsalary ~ nltd, data = QBOutlier)


print(summary(QBmod3)) # Show summary results
print(coefficients(QBmod3)) # model coefficients
print(confint(QBmod3, level = 0.95))# CIs for model parameters
print(fitted(QBmod3)) # predicted values
print(residuals(QBmod3))# residuals
print(anova(QBmod3))# anova table
print(vcov(QBmod3)) # covariance matrix for model parameters
print(influence(QBmod3)) # regression diagnostics
print(cv.lm(QBOutlier, QBmod3, 3)) # K-fold cross-validation with 3 folds
sink()

# diagnostic plots

layout(matrix(c(1, 2, 3, 4), 2, 2)) # optional 4 graphs/page
plot(QBmod3)


# Bootstrap Measures of Relative Importance (100 samples)
# library(relaimpo)

sink(
  "School/DA 485/QB Bootstrap Measures of Relative Importance.txtf",
  type = "output",
  append = FALSE,
  split = TRUE
)

# Calculate Relative Importance for Each Predictor
#library(relaimpo)
calc.relimp(QBmod3,
            type = c("lmg", "last", "first", "pratt"),
            rela = TRUE)

# Bootstrap Measures of Relative Importance (1000 samples)
QB_booth <-
  boot.relimp(
    QBmod3,
    b = 1000,
    type = c("lmg",
             "last", "first", "pratt"),
    rank = TRUE,
    diff = TRUE,
    rela = TRUE
  )
booteval.relimp(QB_booth) # print result
plot(booteval.relimp(QB_booth, sort = TRUE)) # plot result

par(mfrow = c(1, 1))
# Normality of Residuals
# qq plot for studentized resid
qqPlot(QBmod3, main = "QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(QBmod3)
hist(sresid, freq = FALSE,
     main = "Distribution of NFL Offensive Salary")
xfit <- seq(min(sresid), max(sresid), length = 40)
yfit <- dnorm(xfit)
lines(xfit, yfit)

## OUTLIERS
# Assessing Outliers
#library(car)
outlierTest(QBmod3) # Bonferonni p-value for most extreme obs
qqPlot(QBmod3, main = "QQ Plot") #qq plot for studentized resid
leveragePlots(QBmod3) # leverage plot



##OUTLIERS
# Influential Observations
par(mfrow = c(1, 1))
# added variable plots
av.Plots(QBmod2)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4 / ((nrow(QBmod2) - length(QBmod2$coefficients) - 2))
plot(QBmod2, which = 4, cook.levels = cutoff)
# Influence Plot
influencePlot(QBmod2,
              id.method = "identify",
              main = "Influence Plot",
              sub = "Circle size is proportial to Cook's Distance")

# Removing outlier
## re-do this until no more improvement
QBOutlier <- QB_Data[-c(9, 16, 19,46 ), ]

QBmod3 = lm(formula = sqsalary ~ nltd, data = QBOutlier)
summary(QBmod3)

library(car)
install.packages("car")

##OUTLIERS
# Influential Observations
par(mfrow = c(1, 1))
# added variable plots
av.Plots(QBmod3)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4 / ((nrow(QBmod3) - length(QBmod3$coefficients) - 2))
plot(QBmod3, which = 4, cook.levels = cutoff)
# Influence Plot
influencePlot(QBmod3,
              id.method = "identify",
              main = "Influence Plot",
              sub = "Circle size is proportial to Cook's Distance")


##Multi-collinearity

# Evaluate Collinearity
vif(QBmod3) # variance inflation factors
sqrt(vif(QBmod3)) > 2 # problem?


