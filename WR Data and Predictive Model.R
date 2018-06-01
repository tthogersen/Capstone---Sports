#install.packages("ISLR") - used to fix x must be numeric issue
# load the libraries
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
library(cluster)
library(fpc)
library(car)
library(lattice)
library("PerformanceAnalytics")
library(lars)
library(arm)
library(rpart)
library(car)

par(mfrow=c(1,1))

# Subsetting data into 2 different position groups WR and WR

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
    OffNFLSalary$POS,
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
View(NFLOffSalary)

## subsetting WR postion from new data table based upon POS varaible

WR_Data <- subset(NFLOffSalary, V2 == 'WR')
WR_Data <- WR_Data[c(-2)]
View(WR_Data)
WR_Data = data.matrix(WR_Data)
WR_Data = data.frame(WR_Data)
head(WR_Data)
attach(WR_Data)


## creating  table with POS variables so I will be able to subset by Position

# Cor and Matrix plot with normalized data

WR_Cor = cor(WR_Data)
WR_Cor

#install.packages("PerformanceAnalytics")


#library("PerformanceAnalytics")
Norm_OFF_Salary[is.na(Norm_OFF_Salary)] = 0

chart.Correlation(Norm_OFF_Salary, histogram = TRUE, pch = 19)


# Scatterplot / Correlation Matrices from the car Package

corWR_Data = cor(data.frame(WR_Data[sapply(WR_Data, is.numeric)]))
corWR_Data
chart.Correlation(sq, histogram = TRUE, pch = 19)


# Setting up lm for initial modeling
## linear models are fit using the lm( ) function. The form of the lm function is
###lm(formula, family=familytype(link=linkfunction), data=)


# Regression Tree with library(rpart)
# used this information with matrix plot to build initial model

sink(
  "School/DA 485/WR regression Tree.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)

# grow tree
names(WR_Data)
WRtreefit <-
  rpart(
    sqsalary ~ sqrank + GP + GS + sqsnaps + sqsnapsper + nlatt +nlattcom+
      nlcompPer + nlyds + YDS.ATT + nltd + nlfum ,
    method = "anova",
    data = WR_Data
  )

print(printcp(WRtreefit)) # display the results
print(plotcp(WRtreefit)) # visualize cross-validation results
print(summary(WRtreefit)) # detailed summary of split
sink()
# create additional plots
par(mfrow = c(1, 2)) # two plots on one page
rsq.rpart(WRtreefit) # visualize cross-validation results


# plot tree
par(mfrow = c(1, 1))

plot(WRtreefit, uniform = TRUE,
     main = "Regression Tree for WR NFL Salary ")
text(WRtreefit,
     use.n = TRUE,
     all = TRUE,
     cex = .8)

# Fitting new LM model for WR
names(WR_Data)
View(WR_Data)

WR_Data = data.matrix(WR_Data)
WR_Data = data.frame(WR_Data)
# Creating my LM Model using the tree and matrix plot info

sink(
  "School/DA 485/GLM&LM_WR_FitMod.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)

FitLM_mod_WR = lm(sqsalary ~ GP + GS + nlatt+nlcompPer+sqrank +sqsnaps+sqsnapsper +
                    YDS.ATT,
                  data = WR_Data)

print(summary(FitLM_mod_WR))

sink()

coefficients(FitLM_mod_WR) # model coefficients
confint(FitLM_mod_WR, level=0.95) # CIs for model parameters
fitted(FitLM_mod_WR) # predicted values
residuals(FitLM_mod_WR) # residuals
anova(FitLM_mod_WR) # anova table
vcov(FitLM_mod_WR) # covariance matrix for model parameters
influence(FitLM_mod_WR) # regression diagnostic

#Stepwise Variable selection

sink(
  "School/DA 485/GLM&LM_Stepwise_WR.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)

# Stepwise Regression AIC LMM
print(WRStep_LM <- stepAIC(FitLM_mod_WR, direction = "both"))
print(WRStep_LM$anova) # display results
sink()

#Final Model after stepwise selection

sink(
  "School/DA 485/FinalModelResults.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)

WR_Data[is.na(WR_Data)] = 0


WRmod2 = lm(sqsalary ~ nlatt + YDS.ATT, data = WR_Data)

WR_Data[is.na(WR_Data)] = 0


print(summary(WRmod2)) # Show summary results


##OUTLIERS
# Influential Observations
par(mfrow=c(1,1))
# added variable plots
av.Plots(WRMod3)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(WRMod3)-length(WRMod3$coefficients)-2))
plot(WRMod3, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(WRMod3, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Removing outlier
## re-do this until no more improvement
WROutlier <- WR_Data[-c(236,267,377,405), ]

WRMod3 = lm(sqsalary ~ nlatt + YDS.ATT, data = WROutlier)
summary(WRMod3)


#Final Model#
WRMod4 = lm(sqsalary ~ nlatt + YDS.ATT, data = WROutlier)

WR_Data[is.na(WR_Data)] = 0



print(summary(WRMod4)) # Show summary results
print(coefficients(WRMod4)) # model coefficients
print(confint(WRMod4, level = 0.95))# CIs for model parameters
print(fitted(WRMod4)) # predicted values
print(residuals(WRMod4))# residuals
print(anova(WRMod4))# anova table
print(vcov(WRMod4)) # covariance matrix for model parameters
print(influence(WRMod4)) # regression diagnostics
print(cv.lm(WR_Data, WRMod4, 3)) # K-fold cross-validation with 3 folds
sink()

# diagnostic plots

layout(matrix(c(1, 2, 3, 4), 2, 2)) # optional 4 graphs/page
plot(WRMod4)

# Bootstrap Measures of Relative Importance (100 samples)
# library(relaimpo)

sink(
  "School/DA 485/WR Bootstrap Measures of Relative Importance.txtf",
  type = "output",
  append = FALSE,
  split = TRUE
)

# Calculate Relative Importance for Each Predictor
#library(relaimpo)
calc.relimp(
  WRMod4,
  type = c("lmg", "last", "first", "pratt"),
  rela = TRUE
)

# Bootstrap Measures of Relative Importance (1000 samples)
WR_booth <-
  boot.relimp(
    WRMod4,
    b = 1000,
    type = c("lmg",
             "last", "first", "pratt"),
    rank = TRUE,
    diff = TRUE,
    rela = TRUE
  )
booteval.relimp(WR_booth) # print result
plot(booteval.relimp(WR_booth, sort = TRUE)) # plot result

par(mfrow=c(1,1))
# Normality of Residuals
# qq plot for studentized resid
qqPlot(WRMod4, main = "QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(WRMod4)
hist(sresid, freq = FALSE,
     main = "Distribution of NFL Offensive Salary")
xfit <- seq(min(sresid), max(sresid), length = 40)
yfit <- dnorm(xfit)
lines(xfit, yfit)

##Multi-collinearity

# Evaluate Collinearity
vif(WRMod4) # variance inflation factors
sqrt(vif(WRMod4)) > 2 # problem?



