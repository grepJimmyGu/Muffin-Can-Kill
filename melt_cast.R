# Develope the Pivot Table Function in R
library(RODBC)
con<- odbcConnect("HSS_test", uid = "jgu", pwd = "McKinsey1!")
Tenet <- sqlQuery(con, "SELECT * FROM Tenet_Aggregated_Data_09_13_v3") # How dare you run this line?
colnames(Sample)[14] <- "HospitalCode"; colnames(Sample)[15] <- "PatientClassification"
colnames(Sample)[19] <- "DRG_PL"
colname <- colnames(Tenet) <- colnames(Sample)

# Pivot Table in R
library(reshape)
melt_Tenet <- melt(Tenet, id = c("HospitalCode", "FY"), measure = "Volume")
Tenet_Facility_FY <- cast(melt_Tenet, FY ~ variable, sum)

cast(melt_Tenet, FY + PatientClassification + ED_Indicator ~ variable, sum)
cast(melt_Tenet, HospitalCode ~ FY, sum, margins = TRUE)

# Reference does not work in this case, so no way to make a function.
Test_function <- function(data, formula, ...){
  data_melt <- melt(data, id = c(14:19), measure = c(1,4,9,11,12))
  result <- cast(data_melt, formula, ...)
  return(result)
}

Test_function(Tenet, FY ~ variable, sum)

# How to pass into a bunch of formulas at the same time? I created my own FORMULA!!!!
rowname <- colname
fmla <- as.formula(paste(paste(colname[14], collapse = "+"), "~ variable"))
Test_function(Tenet, fmla, sum)
measurement <- colname[c(1,4,9,11,12)]
melt_Tenet_2 <- melt(Tenet, id = paste(colname[14:19]), measure = measurement)

# Now I am developing a new function to conduct pivot table analysis in R
require(reshape)

# I can make it work for column number reference but I do not need to
Pivot_Table <- function(data, row, column = NULL, value, fun, ...){
  ID <- c(row,column)
  if(sum(colnames(data) == "variable") + sum(colnames(data) == "value") < 2){
    data_melt <- melt(data, id = ID, measure = value)
  }
  else{
    data_melt <- data
  }
  if(class(ID) == "integer"){
    factors <- colnames(data)[ID]
  }
  else{
    factors <- ID
  }
  if(length(value) > 1 || length(ID) == 1){ # Greater than 1??
    fmla <- as.formula(paste(paste(factors, collapse = "+"), "~ variable"))
  }
  else{
    fmla <- as.formula(paste(paste(row, collapse = "+"), "~", paste(column, collapse = "+")))
  }
  result <- cast(data_melt, fmla, fun, ...)
  return(result)
}

# Examples:
# If I want to know the volume/NR of different hospital across the last 4 years.
Pivot_Table(Tenet, "HospitalCode", "FY", "Volume", sum)
Pivot_Table(Tenet, "HospitalCode", "FY", "Net_Revenue", sum)

Pivot_Table(Tenet, "HospitalCode", "FY", "Volume", sum, margins = TRUE)

# If I want to add more row/colume restrictions
Pivot_Table(Tenet, c("HospitalCode","ED_Indicator"), "FY", "Volume", sum)
Pivot_Table(Tenet, c("HospitalCode","ED_Indicator"), c("FY","PatientClassification"), "Volume", sum)

# If I need to calculate different values
Pivot_Table(Tenet, c("HospitalCode"), c("FY"), c("Volume","Net_Revenue"), sum)

# A small test on the scale of dataset

library(RODBC)
con<- odbcConnect("HSS_test", uid = "jgu", pwd = "McKinsey1!")
Tenet_big <- sqlQuery(con, "SELECT * FROM Tenet_Aggregated_Data_09_13")
colnames(Tenet_huge) <- colnames(Sample[-1])
A <- Tenet_big
B <- Tenet_big
C <- Tenet_big
D <- Tenet_big
E <- Tenet_big
t <- Tenet_big
Tenet_huge <- rbind(Tenet_big, A, B, C, D, E, t, Tenet_huge, Tenet_huge, Tenet_huge)

Pivot_Table(Tenet_huge, "HospitalCode", "FY", "Net_Revenue", sum)
