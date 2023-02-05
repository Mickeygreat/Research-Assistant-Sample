library(readxl)
library(xlsx)
library(stringr)

yearlist <- c(2007:2021)

##-- reading HUD Funding Data --##
# NOTE: only things needed to be changed is last 2 lines to save dataframe as excel file  
# *reading for different years-HUD*
# for (i in yearlist){
#   filename <- paste0("HUD_Funding", i) # Concatenate Strings in R
#   wd <- paste0("/Users/mickeyyeh/Desktop/Research Assistant/Part2 - Node Details/HUD Funding/HUD_Funding_Working_File.xlsx")
#   assign(filename, read_excel(wd, sheet = as.character(i)))
# }

# *reading for same years data-CPDP*
for (i in yearlist){
  filename <- paste0("HUD_Funding", i) # Concatenate Strings in R
  wd <- paste0("/your/file/location/CPDP_all_working_file.xlsx")
  assign(filename, read_excel(wd, sheet = as.character(i)))
}

# to get all names of the data frames in global environment #!! To let HUD Funding Run
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)] # created before other modified df are created in global env.


##-- reading Homelessness dataset --##
for (i in yearlist){
  filename <- paste0("df", i) # Concatenate Strings in R
  wd <- paste0("/your/file/location/data.xlsx")
  assign(filename, read_excel(wd, sheet = as.character(i)))
}
# NOTE: 2017-2021 missing one row data => AS=row4
# 2007-2016 missing Two rows data => AS=row4 & MP=row28


##-- reading Governor Data --##
US_Governors_all <- read.csv("/your/file/location/US_Governors_all.csv")


##-- reading Median Income Data --##
Median_Income_By_State <- read_excel("/your/file/location/Median_Income_By_State.xlsx")



##-- create a data.frame using all_row as row names --##
##-- col names(4): states, political party, median income, HUD funding --##
df_column <- c("State", "Political Party", "Party Switch Check", "Switch Date", "Median Income", "HUD Funding")

# year starts from 2007 to 2021
for (year in yearlist){
  empty_matrix_name <- paste0("matrix", year,"_empty")
  matrix_empty <- matrix(NA,    # Create empty data frame
                         nrow = 56,
                         ncol = 6)
  # matrix_empty
  assign(empty_matrix_name, matrix_empty)
  
  colnames(matrix_empty) <- df_column
  colnames(matrix_empty)
  
  # add states from Homelessness file into first col 
  # NOTE: 6 not states!
  states <- c(df2007$State[1:56]) # not including last col "total"
  matrix_empty[, 1] <- states
  matrix_empty
  
  # transform character to number for future use
  US_Governors_all[, c(9, 10)] <- sapply(US_Governors_all[, c(9, 10)], as.numeric)
  class(US_Governors_all[1,9])
  class(US_Governors_all[2,10])
  
  # sort political matrix
  US_Governors_all <- US_Governors_all[order(US_Governors_all$StateAbbrev, US_Governors_all$GovernorNumber),]
  # reset row number in sequence
  rownames(US_Governors_all) <- seq(length=nrow(US_Governors_all))
  
  
  ##-- read the right political party into Matrix --##
  for (i in 1:length(states))
  {
    data <- subset(US_Governors_all, StateAbbrev == states[i])
    loc <- which(data$TookOffice_Year <= year & data$LeaveOffice_Year > year)
    if(length(loc) == 0){
      # state.party[i] <- "NA"
      matrix_empty[i, 2] <- "NA"
    }
    else{
      matrix_empty[i, 2] <- data$PartyAffiliation[loc]
    }
  }
  
  # party switch check 
  all_states <- Median_Income_By_State$`State Abbreviations`
  for (i in 1:length(states))
  {
    if (matrix_empty[i, 1] %in% all_states)
    {
      data <- subset(US_Governors_all, StateAbbrev == states[i])
      loc <- which(data$TookOffice_Year == year & data$LeaveOffice_Year > year)
      if(length(loc) == 0){
        # state.party[i] <- NA
        matrix_empty[i, 3] <- 'x'
        matrix_empty[i, 4] <- 'x'
      }
      else {
        matrix_empty[i, 3] <- data$PartyAffiliation[loc] # switched party
        matrix_empty[i, 4] <- data$TookOffice[loc] # switched date
      }
    }
    else {
      matrix_empty[i, 3] <- "NA"
      matrix_empty[i, 4] <- "NA"
    }
  }
  
  
  ##-- add median income --##
  all_states <- Median_Income_By_State$`State Abbreviations`
  year_colnum <- match(year, names(Median_Income_By_State)) # get column number given column name
  for (i in 1:length(states))
  {
    if (matrix_empty[i, 1] %in% all_states) # to make sure 50 states are matched properly
    {
      for (j in 1:length(all_states))
      {
        if (Median_Income_By_State[j, 2] == matrix_empty[i, 1])
        {
          matrix_empty[i, 5] <- as.numeric(Median_Income_By_State[j, year_colnum])
          # cat(i, as.character(Median_Income_By_State[j, 2]), as.numeric(Median_Income_By_State[j, year_colnum]), "\n")
        }
        else
        {
          invisible() # empty to skip that i "pass"
        }
      }
    }
    else # to add NA to non states(matrix has 56 => 6 more regions that are not states)
    {
      # cat(i, as.character(Median_Income_By_State[i, 2]), "NA", "\n")
      matrix_empty[i, 5] <- "NA" # end loop
    }
  }
  
  
  ##-- add HUD Funding --##
  states <- c(df2007$State[1:56]) # not including last col "total"
  for (i in 1:length(states)){
    HUD_funding_year <- mget(paste0("HUD_Funding", year)) # use mget to tell it it's a dataframe or else will have errors!! # mget turns it into a list!!
    subsetdata <- subset(HUD_funding_year[[1]], State == states[i]) # State is the State in HUD
    total_HUD_funding <- sum(subsetdata$`Grant Amount`)
    matrix_empty[i, 6] <- as.numeric(total_HUD_funding)
  }
  
  
  ##-- save into Excel sheets with seperate years --##
  df_name <- paste0("NodeDetail", year)
  df_nodeDetail <- as.data.frame(matrix_empty)
  df_nodeDetail[, c(5, 6)] <- as.numeric(unlist(df_nodeDetail[, c(5, 6)]))
  assign(df_name, df_nodeDetail)
  # write.xlsx(df_nodeDetail, file="/your/file/location/Node_Details.xlsx", sheetName=df_name, append=TRUE, row.names=FALSE) # for HUD different files
  write.xlsx(df_nodeDetail, file="/your/file/location/Node_Details_same_CPDP.xlsx", sheetName=df_name, append=TRUE, row.names=FALSE) # for CPD same files
}

