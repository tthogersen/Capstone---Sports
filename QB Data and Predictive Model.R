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

library(readxl)
OffNFLSalary <-
  read_excel("School/DA 485/NFLSalaryOff.xlsx",
             sheet = "2017OffensivePlayerSalary_New")
View(OffNFLSalary)


head(OffNFLSalary)
OffNFLSalary = data.frame(OffNFLSalary)
attach(OffNFLSalary)
summary(OffNFLSalary)

# Cbind new normalized da

# New Correlation Matrix with Normalized data

nl_td[is.infinite(nl_td)] = 0
INT[is.infinite(INT)] = 0
nl_fum[is.infinite(nl_fum)] = 0

Norm_OFF_Salary = data.frame(
  cbind(
    Rank,
    POS,
    log10_salary,
    sq_gp,
    sq_gs,
    sq_snaps,
    sq_snaps_per,
    nl_plays,
    nl_plays_comp,
    Comp_Percent,
    YDS,
    YDS.ATT,
    nl_td,
    INT,
    nl_fum
  )
)

head(Norm_OFF_Salary)
Norm_OFF_Salary = data.frame(Norm_OFF_Salary)
attach(Norm_OFF_Salary)
summary(Norm_OFF_Salary)


## creating  table with POS variables so I will be able to subset by Position
attach(NFLOffSalary)

sink(
  "School/DA 485/NewNFLData.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)

nl_td[is.infinite(nl_td)] = 0
INT[is.infinite(INT)] = 0
nl_fum[is.infinite(nl_fum)] = 0
Norm_OFF_Salary[is.na(Norm_OFF_Salary)] = 0

NewNFLData = data.frame(Norm_OFF_Salary)
print(head(NewNFLData))
sink()
View(NewNFLData)

# attaching table and checking summary stats

sink(
  "School/DA 485/NewNFLData.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)

NewNFLData = data.frame(NewNFLData)
attach(NewNFLData)
print(head(NewNFLData))
print(summary(NewNFLData))
print(names(NewNFLData))
sink()

## subsetting QB postion from new data table based upon POS varaible

QB_Data<- subset(NewNFLData, POS == 'QB')
QB_Data = QB_Data[, 2:15]
QB_Data = data.frame(QB_Data)
head(QB_Data)

QB_Data[is.infinite(QB_Data)] = 0
INT[is.infinite(INT)] = 0
nl_fum[is.infinite(nl_fum)] = 0
QB_Data[is.na(QB_Data)] = 0

QBScatterplot = scatterplotMatrix(
  ~ log10_salary + sq_gp + sq_gs + sq_snaps + sq_snaps_per +
    nl_plays + nl_plays_comp +
    Comp_Percent + YDS + YDS.ATT + nl_td + INT + nl_fum, sq_rank,
  data = QB_Data,
  main = "QB Offensive Salary"
)



# Cor Matix

sink(
  "School/DA 485/QB_Data_corr.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)
# New Correlation Matrix with Normalized data

QB_Data[is.infinite(QB_Data)] = 0
INT[is.infinite(INT)] = 0
nl_fum[is.infinite(nl_fum)] = 0
QB_Data[is.na(QB_Data)] = 0
INT[is.na(INT)] = 0
nl_fum[is.na(nl_fum)] = 0
  

#convert data set from factor to numeric
QB_Data[is.na(QB_Data)] = 0
QB_Data = data.matrix(QB_Data)
QB_Data = data.frame(QB_Data)
names(QB_Data)

## Correlation Matrix

QB_Data[is.na(QB_Data)] = 0
QB_Data = data.frame(QB_Data)

chart.Correlation(QB_Data, histogram = TRUE, pch = 19)

# Fitting new LM model for QB
View(QB_Data)

# Creating my GLM & LM Model

sink(
  "School/DA 485/GLM&LM_QB_FitMod.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)

FitGLM_mod_QB = glm(
  log10_salary ~ sq_snaps + nl_plays + nl_plays_comp +
    Comp_Percent + YDS + YDS.ATT + nl_td + INT + nl_fum,
  data = QB_Data
)

FitLM_mod_QB = lm(
  log10_salary ~ sq_snaps+nl_plays + nl_plays_comp +
    Comp_Percent + YDS + YDS.ATT + nl_td + INT + nl_fum,
  data = QB_Data
)
print(summary(FitGLM_mod_QB))
print(summary(FitLM_mod_QB))

sink()

#Stepwise Variable selection

sink(
  "School/DA 485/GLM&LM_Stepwise_QB.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)


# Stepwise Regression AIC GLM
print(QBStepAIC_GLM <- stepAIC(FitGLM_mod_QB, direction = "both"))
print(QBStepAIC_GLM$anova) # display results

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


QB_mod2_fit = lm(log10_salary ~ sq_snaps + nl_plays + Comp_Percent + YDS+
                    nl_td, data = QB_Data)

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

# Relative Variable Importance

sink(
  "School/DA 485/Relative Variable Importance.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)

# Calculate Relative Importance for Each Predictor
print(calc.relimp(QB_mod2_fit,
                  type = c("lmg", "last", "first", "pratt"),
                  rela = TRUE))
sink()

# Bootstrap Measures of Relative Importance (100 samples)

sink(
  "School/DA 485/Bootstrap Measures of Relative Importance.txtf",
  type = "output",
  append = FALSE,
  split = TRUE
)


boot_QB <- boot.relimp(
  QB_mod2_fit,
  type = c("lmg","last", "first", "pratt"),
  
  rank = TRUE,
  diff = TRUE,
  rela = TRUE
)

print(booteval.relimp(boot_QB)) # print result
print(plot(booteval.relimp(boot_QB, sort = TRUE))) # plot result
sink()

# diagnostic plots 

sink(
  "School/DA 485/diagnostic plots.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(QB_mod2_fit)


# Creating Predictive Model for QB Salary
## Resorting Data
print(QB_mod2_fit)
head(QB_Data)

pred_QB <-
  QB_Data[c(
            "sq_snaps",
            "nl_plays",
            "nl_plays_comp",
            "nl_td",
            "log10_salary")]



pred_QB[is.na(pred_QB)] = 0
attach(pred_QB)


# define an 70%/30% train/test split of the dataset
set.seed(12345)
split=0.70
QB_train <- createDataPartition(pred_QB$log10_salary, p=split, list = FALSE)
QB_Data_train <- pred_QB[ QB_train,]
QB_Data_test <- pred_QB[-QB_train,]
# train a naive bayes model
model <- NaiveBayes(log10_salary~ ., data=QB_Data_train)
# make predictions
QB_x_test <- pred_QB[,1:4]
QB_y_test <- pred_QB[,5]
predictions <- predict(model, QB_x_test)
# summarize results
confusionMatrix(predictions$class, QB_y_test)



