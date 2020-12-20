#'rCut



#' This produces cut offs for each variable using the maximal rank statistics method.

#' @import survival
#' @import maxstat
#' @import survminer
#' @import tidyverse


#' @export



#Function
rCut <- function(Data, CSS = "No", OS = "No", PFS = "No", ID = "ID", Survival = "SurvivalTime", SurvivalStatus = "SurvivalStatus", Progression = "ProgressionTime", ProgressionStatus = "ProgresionStatus", Variables)
{


  if (!inherits(data, "data.frame"))
    stop("data should be an object of class data.frame")
  data <- as.data.frame(data)
  if (!all(c(ID, Survival, SurvivalStatus, Progression, ProgressionStatus) %in% colnames(data)))
    stop("Specify correct column names containing ID, Survival, SurvivalStatus, Progression and ProgressionStatusvalues.")
  if (!all(Variables %in% colnames(data)))
    stop("Some variables are not found in the data: ",
         paste(setdiff(variables, colnames(data)), collapse = ", "))



  #Splits survival status into CSS and OS status
  Data$CancerSpecificStatus <- ifelse(Data$SurvivalStatus == 1, c("1"), c("0"))
  Data$OverallSpecificStatus <- ifelse(Data$SurvivalStatus > 0, c("1"), c("0"))



  #Determine cut off for cancer-specific survival
  if (CSS == "Yes") {
    res.cut <- surv_cutpoint(Data, time = Survival, event = CSS, Variables)
    plot(res.cut, Variables, palette = c("#d70033", "#5596e6"))
  }



  #Determine cut off for overall survival
  if (OS == "Yes") {
    res.cut <- surv_cutpoint(Data, time = Survival, event = OS, Variables)
    plot(res.cut, Variables, palette = c("#d70033", "#5596e6"))
  }



  #Determine cut off for progression-free survival
  if (PFS == "Yes") {
    res.cut <- surv_cutpoint(Data, time = Progression, event = ProgressionStatus, Variables)
    plot(res.cut, Variables, palette = c("#d70033", "#5596e6"))
  }





}
