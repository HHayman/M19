#'rCut



#' This produces cut offs for each variable using the maximal rank statistics method.

#' @import survival
#' @import maxstat
#' @import survminer
#' @import tidyverse


#' @export



#Function
#rCut(Data, CSS = "Yes", OS = "No", PFS = "No", ID = "ID", Survival = "CSS_2017", SurvivalStatus = "a0cd1ncd2_2017", Variables = c("GD_Tumour_Stroma_Perc", "GD_Tumour_Epithelium_Perc", "GD_Healthy_LaminaPropria_Perc", "GD_Healthy_Epithelium_Perc"))


rCut <- function(Data, CSS, OS, PFS, ID, Survival, SurvivalStatus, Progression, ProgressionStatus, Variables)
{
  if (!all(Variables %in% colnames(Data)))
    stop("Some variables are not found in the data: ",
         paste(setdiff(Variables, colnames(Data)), collapse = ", "))



  #Reconfigure survival variables
  names(Data)[names(Data) == SurvivalStatus] <- "SurvivalStatus"
  names(Data)[names(Data) == Survival] <- "Survival"
  Data$CSS <- ifelse(Data$SurvivalStatus == 1, c("1"), c("0"))
  Data$OS <- ifelse(Data$SurvivalStatus > 0, c("1"), c("0"))
  Data$CSS <- as.numeric(Data$CSS)
  Data$OS <- as.numeric(Data$OS)



  #Determine cut off for cancer-specific survival
  if (CSS == "Yes") {
    res.cut <- surv_cutpoint(Data, time = "Survival", event = "CSS", Variables)
    CSS_Plots <- plot(res.cut, Variables, palette = c("#d70033", "#5596e6"))
  }



  #Determine cut off for overall survival
  if (OS == "Yes") {
    res.cut <- surv_cutpoint(Data, time = "Survival", event = "OS", Variables)
    OS_Plots <- plot(res.cut, Variables, palette = c("#d70033", "#5596e6"))
  }



  #Determine cut off for progression-free survival
  if (PFS == "Yes") {
    res.cut <- surv_cutpoint(Data, time = Progression, event = ProgressionStatus, Variables)
    PFS_Plots <- plot(res.cut, Variables, palette = c("#d70033", "#5596e6"))
  }


  Plots <- list(CSS_Plots, OS_Plots)

  return(Plots)

  #return(CSS_Plots)
  #return(OS_Plots)
  #return(PFS_Plots)



}
