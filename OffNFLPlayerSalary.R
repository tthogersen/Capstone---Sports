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

log2_salary = log2((Salary))


hist(log2_salary,
     #xlim = c(10, 18),
     #ylim = c(0, 150),
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
    log2_salary,
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








