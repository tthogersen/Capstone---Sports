# Imported dataset from excel#

library(readxl)
OffNFLSalary <-
  read_excel("School/DA 485/NFLSalaryOff.xlsx",
             sheet = "2017OffensivePlayerSalary_New",
             na.rm = TRUE)
View(OffNFLSalary)



# Convert data set to data.frame#

OffNFLSalary = data.frame(NFLPlayerSalaryData)
attach(OffNFLSalary)
head(OffNFLSalary)

# Correlation

OFFSalaryCor = cor(data.frame(OffNFLSalary[sapply(OffNFLSalary, is.numeric)]))
OFFSalaryCor

# Scatterplot Matrices from the car Package


library(car)
library(lattice)

# Basic Scatterplot Matrix
pairs(
  ~ Salary + Rank + GP + GS + Snaps + Snaps_Percent + Plays + Plays_Comp +
    Comp_Percent  + YDS  + YDS.ATT + TD + INT + Fum,
  data = OffNFLSalary,
  main = "Simple Scatterplot Matrix"
)

names(OffNFLSalary)

# NOrmilizing Varaiables:

## Salary Dependent Variable

log10_salary = log10((Salary))


hist(log10_salary,
     xlab = "Normalized NFL Salary $ million",
     main = "Historam of NFL Salary
     ",
     col = "blue")


## Rank - Independant Variable

sq_rank = sqrt(Rank)
hist(
  sq_rank,
  xlim = c(0, 15),
  xlab = "Normalized NFL Rank",
  main = "Historam of NFL Rank",
  col = "blue"
)

## GP (Games Played) - Independant Variable

names(OffNFLSalary)

sq_gp = sqrt((GP))
hist(sq_gp,
     xlab = "Normalized NFL GP",
     main = "Historam of NFL Games Played",
     col = "blue")


## GS (Games Started) - Independant Variable

names(OffNFLSalary)

sq_gs = sqrt(GS)
hist(sq_gs,
     #xlim = c(0,1.5),
     #ylim = c(0, 100),
     xlab = "Normalized NFL GS",
     main = "Historam of NFL Games Started",
     col = "blue")

## Snaps (Games Snaps) - Independant Variable

names(OffNFLSalary)

sq_snaps = sqrt(Snaps)
hist(
  sq_snaps,
  probability = FALSE,
  xlab = "Normalized NFL Snaps",
  main = "Historam of NFL Snaps",
  col = "blue"
)

## Snap% (Snasp Percentage) - Independant Variable

names(OffNFLSalary)

sq_snaps_per = sqrt(Snaps_Percent)
hist(
  sq_snaps_per,
  probability = FALSE,
  xlab = "Normalized NFL Snaps %",
  main = "Historam of NFL Snaps %",
  col = "blue"
)

## Plays (Total Plays) - Independant Variable

names(OffNFLSalary)

nl_plays = sqrt(Plays)
hist(
  nl_plays,
  probability = FALSE,
  xlab = "Normalized Total Plays",
  main = "Historam of NFL Total Plays",
  col = "blue"
)

## Plays_Comp (Total Plays Completed) - Independant Variable

names(OffNFLSalary)

nl_plays_comp = sqrt(Plays_Comp)
hist(
  nl_plays_comp,
  probability = FALSE,
  xlab = "Normalized Plays Comp",
  main = "Historam of NFL Total Plays Completed",
  col = "blue"
)

## Comp_Percent (Completion Percentage) - Independant Variable

names(OffNFLSalary)

Comp_Percent = (Comp_Percent)
hist(
  Comp_Percent,
  probability = FALSE,
  #xlim = c(2.5,5),
  xlab = "Normalized Completion %",
  main = "Historam of NFL Completion %",
  col = "blue"
)

## YDS (Total Yardage) - Independant Variable

names(OffNFLSalary)

YDS = (YDS)
hist(
  YDS,
  probability = FALSE,
  #xlim = c(2, 5),
  xlab = "Normalized YDS",
  main = "Historam of NFL Total Yardage",
  col = "blue"
)

## YDS.ATT (YDS/ATT) - Independant Variable

names(OffNFLSalary)

YDS.ATT = (YDS.ATT)
hist(
  YDS.ATT,
  probability = FALSE,
  #xlim = c(2, 5),
  xlab = "Normalized YDS/ATT",
  main = "Historam of NFL Total Yardage per Attempt",
  col = "blue"
)

## TD (Touch Downs) - Independant Variable

names(OffNFLSalary)

nl_td = sqrt(TD)
hist(nl_td,
     xlab = "Normalized TD",
     main = "Historam of NFL Touch Downs",
     col = "blue")

## INT (Interception) - Independant Variable

names(OffNFLSalary)

INT = (INT)
hist(
  INT,
  probability = FALSE,
  #xlim = c(2, 5),
  xlab = "Normalized INT",
  main = "Historam of NFL Interception",
  col = "blue"
)

## FUM (Fumbles) - Independant Variable

names(OffNFLSalary)

nl_fum = sqrt(Fum)
hist(
  nl_fum,
  probability = FALSE,
  #xlim = c(2, 5),
  xlab = "Normalized Fumbles",
  main = "Historam of NFL Fumbles",
  col = "blue"
)

# Cbind new normalized da

head(Norm_OFF_Salary)
Norm_OFF_Salary = data.frame(Norm_OFF_Salary)
attach(Norm_OFF_Salary)
summary(Norm_OFF_Salary)


# New Correlation Matrix with Normalized data


Norm_OFF_Salary = data.frame(
  cbind(
    Player,
    POS,
    sq_rank,
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




cor(Norm_OFF_Salary)

# removed NA value in INT column

Norm_OFF_Salary[is.na(Norm_OFF_Salary)] = 0

# Cor and Matrix plot with normalized data

NFLOffSalary_Cor = cor(Norm_OFF_Salary)
NFLOffSalary_Cor

# Scatterplot Matrices from the car Package



library(car)
scatterplotMatrix(
  ~ Player + POS + sq_rank + log2_salary + sq_gp + sq_gs + sq_snaps + sq_snaps_per +
    nl_plays + nl_plays_comp +
    Comp_Percent + YDS + YDS.ATT + nl_td + INT + nl_fum,
  data = Norm_OFF_Salary,
  main = "NFL Offensive Salary"
)

install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(Norm_OFF_Salary, histogram = TRUE, pch = 19)


# Setting up GLM for initial modeling
## Generalized linear models are fit using the glm( ) function. The form of the glm function is
###glm(formula, family=familytype(link=linkfunction), data=)


# getting the names of all variables
names(Norm_OFF_Salary)

# Creating GLM

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
## Created GLM Model
fit_Off_NFL_Salary = glm(log10_salary ~ Player + POS + sq_rank+
                           YDS + YDS.ATT + INT + nl_td + nl_fum, data = Norm_OFF_Salary)
sink(
  "School/DA 485/GLM_Summary_AOV.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)
print(summary(fit_Off_NFL_Salary))
print(anova(fit_Off_NFL_Salary))
print(Confint(fit_Off_NFL_Salary))
print(exp(coef(fit_Off_NFL_Salary)))
print(confint(fit_Off_NFL_Salary))

sink()

# Stepwise Regression for NFL Salary information

library(MASS)

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

# new GLM Data model with stepwise/aic output

fit2_Off_NFL_Salary = glm(log10_salary ~ sq_rank+
                           YDS + YDS.ATT + INT + nl_td, data = Norm_OFF_Salary)
sink(
  "School/DA 485/mod2_GLM_Summary_AOV.txt",
  type = "output",
  append = FALSE,
  split = TRUE
)
print(summary(fit2_Off_NFL_Salary))
print(anova(fit2_Off_NFL_Salary, test = "Chisq"))
print(Confint(fit2_Off_NFL_Salary))
print(exp(coef(fit2_Off_NFL_Salary)))
print(confint(fit2_Off_NFL_Salary))

sink()








