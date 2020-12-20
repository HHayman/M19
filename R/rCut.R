#'rCut



#' This produces cut offs for each variable using the maximal rank statistics method.

#' @import survival
#' @import maxstat
#' @import survminer
#' @import tidyverse


#' @export



#Function
rCut <- function(Data, CSS = "No", OS = "No", MFS = "No", ID = "ID", Survival = "SurvivalTime", SurvivalStatus = "SurvivalStatus", Progression = "ProgressionTime", ProgressionStatus = "ProgresionStatus", Variable1 = "Variable1", Variable1Unit = "Variable1Unit", Variable2 = "Variable2", Variable2Unit = "Variable2Unit", Variable3 = "Variable3", Variable3Unit = "Variable3Unit", Variable4 = "Variable4", Variable4Unit = "Variable4Unit", Variable5 = "Variable5", Variable5Unit = "Variable5Unit", Variable6 = "Variable6", Variable6Unit = "Variable6Unit", Variable7 = "Variable7", Variable7Unit = "Variable7Unit", Variable8 = "Variable8", Variable8Unit = "Variable8Unit") {



  names(Data)[names(Data) == ID] <- "ID"
  names(Data)[names(Data) == Survival] <- "SurvivalTime"
  names(Data)[names(Data) == SurvivalStatus] <- "SurvivalStatus"
  names(Data)[names(Data) == Progression] <- "ProgressionTime"
  names(Data)[names(Data) == ProgressionStatus] <- "ProgressionStatus"
  names(Data)[names(Data) == Variable1] <- "Variable1"
  names(Data)[names(Data) == Variable1Unit] <- "Variable1Unit"
  names(Data)[names(Data) == Variable2] <- "Variable2"
  names(Data)[names(Data) == Variable2Unit] <- "Variable2Unit"
  names(Data)[names(Data) == Variable3] <- "Variable3"
  names(Data)[names(Data) == Variable3Unit] <- "Variable3Unit"
  names(Data)[names(Data) == Variable4] <- "Variable4"
  names(Data)[names(Data) == Variable4Unit] <- "Variable4Unit"
  names(Data)[names(Data) == Variable5] <- "Variable5"
  names(Data)[names(Data) == Variable5Unit] <- "Variable5Unit"
  names(Data)[names(Data) == Variable6] <- "Variable6"
  names(Data)[names(Data) == Variable6Unit] <- "Variable6Unit"
  names(Data)[names(Data) == Variable7] <- "Variable7"
  names(Data)[names(Data) == Variable7Unit] <- "Variable7Unit"
  names(Data)[names(Data) == Variable8] <- "Variable8"
  names(Data)[names(Data) == Variable8Unit] <- "Variable8Unit"



  Data$CancerSpecificStatus <- ifelse(Data$SurvivalStatus == 1, c("1"), c("0"))
  Data$OverallSpecificStatus <- ifelse(Data$SurvivalStatus > 0, c("1"), c("0"))

  NewData <- Data

  NewData

  return(NewData)
}
