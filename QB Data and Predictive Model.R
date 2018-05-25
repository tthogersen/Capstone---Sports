#install.packages("ISLR") - used to fix x must be numeric issue
# load the libraries
library(caret)
library(klaR)
library(ISLR)
library(DAAG)
library(relaimpo) #- used to for relative importacelibrary(car)
library(Hmisc) #for rcorr function
library(MASS)
library(PerformanceAnalytics)
library(rsample)  # data splitting 
library(dplyr)    # data transformation
library(ggplot2)  # data visualization
library(caret)    # implementing with caret
library(h2o)      # implementing with h2o
library(ggplot2)
library(cluster)
library(fpc)
library(MASS)
library(car)
library(lattice)
library("PerformanceAnalytics")
library(lars)
library(arm)

par(mfrow=c(1,1))

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

QB_Data<- subset(NFLOffSalary, POS == 'QB')
View(QB_Data)
QB_Data = data.frame(QB_Data)
head(QB_Data)
attach(QB_Data)


# Scatterplot / Correlation Matrices from the car Package

corQB_Data = cor(data.frame(QB_Data[sapply(QB_Data, is.numeric)]))
corQB_Data
chart.Correlation(sq, histogram = TRUE, pch = 19)


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
treefit <-
  rpart(
    sqsalary ~ sqrank + GP + GS + sqsnaps + sqsnapsper + nlatt +nlattcom+
      nlcompPer + nlyds + YDS.ATT + nltd + nlint + nlfum ,
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

FitLM_mod_QB = lm(sqsalary ~ GP+GS+nlatt+nlattcom+nlcompPer+nltd+nlyds+YDS.ATT,
  data = QB_Data
)
print(summary(FitLM_mod_QB))

sink()

coefficients(FitLM_mod_QB) # model coefficients
confint(FitLM_mod_QB, level=0.95) # CIs for model parameters
fitted(FitLM_mod_QB) # predicted values
residuals(FitLM_mod_QB) # residuals
anova(FitLM_mod_QB) # anova table
vcov(FitLM_mod_QB) # covariance matrix for model parameters
influence(FitLM_mod_QB) # regression diagnostic

#Stepwise Variable selection

sink(
  "School/DA 485/GLM&LM_Stepwise_QB.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)

# Stepwise Regression AIC LMM
print(QBStep_LM <- stepAIC(FitLM_mod_QB, direction = "both"))
print(QBStep_LM$anova) # display results
sink()

#Final Model after stepwise selection

sink(
  "School/DA 485/FinalModelResults.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)

QB_Data[is.na(QB_Data)] = 0


QB_mod2_fit = lm(sqsalary ~ nlatt + nlattcom + nlcompPer + nltd + nlyds, data = QB_Data)

QB_Data[is.na(QB_Data)] = 0



print(summary(QB_mod2_fit)) # Show summary results
print(coefficients(QB_mod2_fit)) # model coefficients
print(confint(QB_mod2_fit, level = 0.95))# CIs for model parameters
print(fitted(QB_mod2_fit)) # predicted values
print(residuals(QB_mod2_fit))# residuals
print(anova(QB_mod2_fit))# anova table
print(vcov(QB_mod2_fit)) # covariance matrix for model parameters
print(influence(QB_mod2_fit)) # regression diagnostics
print(cv.lm(QB_Data, QB_mod2_fit, 3)) # K-fold cross-validation with 3 folds
sink()

# diagnostic plots

layout(matrix(c(1, 2, 3, 4), 2, 2)) # optional 4 graphs/page
plot(QB_mod2_fit)


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
calc.relimp(
  QB_mod2_fit,
  type = c("lmg", "last", "first", "pratt"),
  rela = TRUE
)

# Bootstrap Measures of Relative Importance (1000 samples)
QB_booth <-
  boot.relimp(
    QB_mod2_fit,
    b = 1000,
    type = c("lmg",
             "last", "first", "pratt"),
    rank = TRUE,
    diff = TRUE,
    rela = TRUE
  )
booteval.relimp(QB_booth) # print result
plot(booteval.relimp(QB_booth, sort = TRUE)) # plot result

par(mfrow=c(1,1))
# Normality of Residuals
# qq plot for studentized resid
qqPlot(QB_mod2_fit, main = "QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(QB_mod2_fit)
hist(sresid, freq = FALSE,
     main = "Distribution of NFL Offensive Salary")
xfit <- seq(min(sresid), max(sresid), length = 40)
yfit <- dnorm(xfit)
lines(xfit, yfit)
