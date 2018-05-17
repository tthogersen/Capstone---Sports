# Subsetting data into 2 different position groups QB and WR

## creating  table with POS variables so I will be able to subset by Position

NewNFLData = data.frame(
  cbind(
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

# Attacing table and checking summary stats

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

QB_Data <- subset(NewNFLData, POS=='QB')
QB_Data <- QB_Data[,2:15]
QB_Data = data.frame(QB_Data)
head(QB_Data)

## subsetting WR postion from new data table based upon POS varaible

WR_Data <- subset(NewNFLData, POS=="WR")
WR_Data <- WR_Data[,2:15]
WR_Data = data.frame(WR_Data)
head(WR_Data)
