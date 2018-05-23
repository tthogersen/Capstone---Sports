# packages:

library(cluster)
library(fpc)
library(MASS)
library(car)
library(lattice)
library("PerformanceAnalytics")
library(mclust)
library(DAAG) # K-fold cross-validation
#install.packages("naivebayes")
library(naivebayes)
library(car)
library(lattice)
#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
library(MASS)
library(cluster)
library(fpc)
#install.packages("ResourceSelection") # enable Hosmer and Lemeshow goodness of fit (GOF) test
library(ResourceSelection)

# Imported dataset from excel#

# Packages:



library(readxl)
OffNFLSalary <-
  read_excel("School/DA 485/NFLSalaryOff.xlsx",
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
    ~ Salary + Rank + GP + GS + Snaps + Snap + Plays + Plays_Comp +
      Comp_Percent  + YDS  + YDS.ATT + TD + INT + Fum,
    data = OffNFLSalary,
    main = "Simple Scatterplot Matrix"
  )
  
  names(OffNFLSalary)
  
  # NOrmilizing Varaiables:
  
  par(mfrow = c(1, 2))
  
  
  ## Salary Dependent Variable
  
  #Before:
  
  hist(
    OffNFLSalary$Salary,
    xlab = "NFL Salary $ million",
    main = "Historam of NFL Salary
    ",
    col = "green"
  )
  #After
  
  log10_salary = log10((OffNFLSalary$Salary))
  
  
  hist(
    log10_salary,
    xlab = "Normalized NFL Salary $ million",
    main = "Historam of NFL Salary
    ",
    col = "blue"
  )
  
  
  ## Rank - Independant Variable
  #Before:
  
  hist(
    OffNFLSalary$Rank,
    xlab = "NFL Postion Rank",
    main = "Historam of Postion Rank
    ",
    col = "green"
  )
  #After
  Rank = (OffNFLSalary$Rank)
  hist(
    Rank,
    #xlim = c(0, 15),
    xlab = "Normalized NFL Rank",
    main = "Historam of NFL Rank",
    col = "blue"
  )
  
  ## GP (Games Played) - Independant Variable
  #Before:
  
  hist(
    OffNFLSalary$GP,
    xlab = "NFL Games Played",
    main = "Historam of Games Played
    ",
    col = "green"
  )
  #After
  
  names(OffNFLSalary)
  
  gp = ((GP))
  hist(
    GP,
    xlab = "Normalized NFL GP ",
    main = "Historam of NFL Games Played",
    col = "blue"
  )
  
  
  ## GS (Games Started) - Independant Variable
  #Before:
  
  hist(
    OffNFLSalary$GS,
    xlab = "NFL Games Stared",
    main = "Historam of Games Started
    ",
    col = "green"
  )
  #After
  
  names(OffNFLSalary)
  
  sq_qs = sqrt(GS)
  hist(
    sq_gs,
    #xlim = c(0,1.5),
    #ylim = c(0, 100),
    xlab = "Normalized NFL Games Started",
    main = "Historam of NFL Games Started",
    col = "blue"
  )
  
  ## Snaps (Games Snaps) - Independant Variable
  
  names(OffNFLSalary)
  #Before:
  
  hist(
    OffNFLSalary$Snaps,
    xlab = "NFL Snaps",
    main = "Historam ofSnaps
    ",
    col = "green"
  )
  #After
  
  sq_snaps = sqrt(Snaps)
  hist(
    sq_snaps,
    probability = FALSE,
    xlab = "Normalized Snaps",
    main = "Historam of Snaps",
    col = "blue"
  )
  
  ## Snap_Percnt (Snap Percentage) - Independant Variable
  #Before:
  
  hist(
    OffNFLSalary$Snaps_percent,
    xlab = "NFL Snaps %",
    main = "Historam of NFL Snaps %
    ",
    col = "green"
  )
  #After
  
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
  #Before:
  
  hist(
    OffNFLSalary$Plays,
    xlab = "NFL Total Plays",
    main = "Historam of NFL Total Plays
    ",
    col = "green"
  )
  #After
  
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
  #Before:
  
  hist(
    OffNFLSalary$Plays_Com,
    xlab = "NFL Plays Completed",
    main = "Historam of NFL Total Plays Completed
    ",
    col = "green"
  )
  #After
  
  nl_plays_comp = sqrt(Plays_Com)
  hist(
    nl_plays_comp,
    probability = FALSE,
    xlab = "Normalized Plays Completed",
    main = "Historam of NFL Total Plays Completed",
    col = "blue"
  )
  
  ## Comp_Percent (Completion Percentage) - Independant Variable
  
  names(OffNFLSalary)
  #Before:
  
  hist(
    OffNFLSalary$Com_Percent,
    xlab = "NFL Completion %",
    main = "Historam of NFL Completion %
    ",
    col = "green"
  )
  #After
  
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
  #Before:
  
  hist(
    OffNFLSalary$YDS,
    xlab = "NFL Total Yardage",
    main = "Historam of NFL Total Yardage
    ",
    col = "green"
  )
  #After
  log_yds = log(YDS)
  hist(
    log_yds,
    probability = FALSE,
    xlim = c(0, 10),
    xlab = "Normalized YDS",
    main = "Historam of NFL Total Yardage",
    col = "blue"
  )
  
  ## YDS.ATT (YDS/AT1T) - Independant Variable
  
  names(OffNFLSalary)
  
  #Before:
  
  hist(
    OffNFLSalary$YDS.ATT,
    xlab = "NFL Yards per Attempt",
    main = "Historam of NFL Total Yardage per Attempted
    ",
    col = "green"
  )
  #After
  YDS.ATTt = (YDS.ATT)
  hist(
    YDS.ATT,
    probability = FALSE,
    #xlim = c(2, 5),
    xlab = "Normalized Yards per Attempt",
    main = "Historam of NFL Total Yardage per Attempted",
    col = "blue"
  )
  
  ## TD (Touch Downs) - Independant Variable
  
  names(OffNFLSalary)
  #Before:
  
  hist(
    OffNFLSalary$TD,
    xlab = "NFL Touch Downs",
    main = "Historam of NFL Touch Downs
    ",
    col = "green"
  )
  #After
  nl_td = log(TD)
  hist(
    nl_td,
    xlab = "Normalized TD",
    main = "Historam of NFL Touch Downs",
    col = "blue"
  )
  
  ## INT (Interception) - Independant Variable
  
  names(OffNFLSalary)
  #before
  hist(
    OffNFLSalary$INT,
    xlab = "NFL Interception",
    main = "Historam of NFL Interception
    ",
    col = "green"
  )
  #After
  
  log_int = log(INT)
  hist(
    log_int,
    probability = FALSE,
    #xlim = c(2, 5),
    xlab = "Normalized INT",
    main = "Historam of NFL Interception",
    col = "blue"
  )
  
  INT[is.na(INT)] = 0
  
  
  ## FUM (Fumbles) - Independant Variable
  
  names(OffNFLSalary)
  #before
  hist(
    OffNFLSalary$Fum,
    xlab = "NFL Fumbles",
    main = "Historam of NFL Fumbles
    ",
    col = "green"
  )
  #After
  
  nl_fum = log(Fum)
  hist(
    nl_fum,
    probability = FALSE,
    #xlim = c(2, 5),
    xlab = "Normalized Fumbles",
    main = "Historam of NFL Fumbles",
    col = "blue"
  )
  
  par(mfrow = c(1, 1))
  
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
  
  
  # removed NA value in INT column
  
  Norm_OFF_Salary[is.na(Norm_OFF_Salary)] = 0
  
  # Cor and Matrix plot with normalized data
  
  NFLOffSalary_Cor = cor(Norm_OFF_Salary)
  NFLOffSalary_Cor
  
  #install.packages("PerformanceAnalytics")
  
  
  #library("PerformanceAnalytics")
  Norm_OFF_Salary[is.na(Norm_OFF_Salary)] = 0
  
  chart.Correlation(Norm_OFF_Salary, histogram = TRUE, pch = 19)
  
  
  # Setting up lm for initial modeling
  ## Generalized linear models are fit using the lm( ) function. The form of the lm function is
  ###lm(formula, family=familytype(link=linkfunction), data=)
  
  
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

  ## Created lm Model
  fit_Off_NFL_Salary = lm(log10_salary ~ Rank + POS +sq_gp+sq_snaps_per+Comp_Percent+
                            YDS + YDS.ATT + INT + nl_td + nl_fum,
                          data = Norm_OFF_Salary)
  sink(
    "School/DA 485/lm_Summary_AOV.txt",
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
  
    # new lm Data model with stepwise/aic output
  
  fit2_Off_NFL_Salary = lm(log10_salary ~ Rank + sq_gp + YDS + nl_td+POS,data = Norm_OFF_Salary)
  
  
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
  plot(fit2_Off_NFL_Salary)
  
  
  
  # Cluster Analysis
  
  par(mfrow = c(1, 1))
  
  
  ## Data Prep mydata <- na.omit(mydata) # listwise deletion of missing
  

  # Prepare Data
  cadata <- na.omit(Norm_OFF_Salary) # listwise deletion of missing
  cadata <- scale(Norm_OFF_Salary) # standardize variables 
  
  view(cadata)
  
  
  
  ## Partitioning using K-means
  
  # Determine number of clusters
  
  wss <-
    (nrow(cadata) - 1) * sum(apply(cadata, 2, var))
  for (i in 2:15)
    wss[i] <- sum(kmeans(cadata,
                         centers = i)$withinss)
  
  plot(
    1:15,
    wss,
    type = "b",
    xlab = "Number of Clusters",
    ylab = "Within groups sum of squares"
  )
  sink(
    "School/DA 485/clusterAnalysis.txt",
    type = "output",
    append = FALSE,
    split = TRUE
  )
  
  Norm2_OFF_Salary <-  na.omit(cadata)
  

  # append cluster assignment
  print(mydata <- data.frame(Norm2_OFF_Salary, fit$cluster))
  sink()
  
  # Plotting Cluster Solutions
  
  # K-Means Clustering with 5 clusters
  fit <- kmeans(Norm2_OFF_Salary, 5)
  
  # Cluster Plot against 1st 2 principal components
  
  # vary parameters for most readable graph
  
  
  library(cluster)
  clusplot(
    Norm2_OFF_Salary,
    fit$cluster,
    color = TRUE,
    shade = TRUE,
    labels = 2,
    lines = 0
  )
  
  # Centroid Plot against 1st 2 discriminant functions
  plotcluster(Norm2_OFF_Salary, fit$cluster)
  
  # Ward Hierarchical Clustering
  
  d <- dist(Norm2_OFF_Salary, method = "maximum") # distance matrix
  fit <- hclust(d, method = "ward")
  plot(fit) # display dendogram
  groups <- cutree(fit, k = 5) # cut tree into 5 clusters
  
  # draw dendogram with red borders around the 5 clusters
  
  rect.hclust(fit, k = 5, border = "red")
  
  
 
  # Ward Hierarchical Clustering
  
  d <- dist(Norm2_OFF_Salary, method = "maximum") # distance matrix
  fit <- hclust(d, method = "ward")
  plot(fit) # display dendogram
  groups <- cutree(fit, k = 5) # cut tree into 5 clusters
  
  # draw dendogram with red borders around the 5 clusters
  
  rect.hclust(fit, k = 5, border = "red")
  
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
  calc.relimp(fit2_Off_NFL_Salary,type=c("lmg","last","first","pratt"),
              rela=TRUE)
  
  # Bootstrap Measures of Relative Importance (1000 samples)
  NLF_booth <- boot.relimp(fit2_Off_NFL_Salary, b = 1000, type = c("lmg",
                                              "last", "first", "pratt"), rank = TRUE,
                      diff = TRUE, rela = TRUE)
  booteval.relimp(NLF_booth) # print result
  plot(booteval.relimp(NLF_booth,sort=TRUE)) # plot result
  
  
  
  # Normality of Residuals
  # qq plot for studentized resid
  qqPlot(fit2_Off_NFL_Salary, main="QQ Plot")
  # distribution of studentized residuals
  library(MASS)
  sresid <- studres(fit2_Off_NFL_Salary)
  hist(sresid, freq=FALSE,
       main="Distribution of NFL Offensive Salary")
  xfit<-seq(min(sresid),max(sresid),length=40)
  yfit<-dnorm(xfit)
  lines(xfit, yfit) 
  
  

  
  
