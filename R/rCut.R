#'rCut



#' This produces cut offs for each variable using the maximal rank statistics method.

#' @import survival
#' @import maxstat
#' @import survminer
#' @import tidyverse


#' @export



#Function

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



  Title <- "Enjoy your cut offs. Hail Wonder Woman!"

  Plots <- list(Title)



  #Determine cut off for cancer-specific survival
  if (CSS == "Yes") {
    CSS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "CSS", Variables)
    CSS_Plots <- plot(CSS.res.cut, Variables, palette = c("#d70033", "#5596e6"), main=list(font=2, cex=5, label="Cancer-Specific Survival"))
    CSS_Title <- "Cancer-specific survival cut offs;"
    Plots <- c(Plots, CSS_Title, CSS_Plots)
    YourCSSPlots <<- CSS_Plots
  }



  #Determine cut off for overall survival
  if (OS == "Yes") {
    OS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "OS", Variables)
    OS_Plots <- plot(OS.res.cut, Variables, palette = c("#d70033", "#5596e6"), main="Overall Survival", font.main=2, cex.main=5)
    OS_Title <- "Overall survival cut offs;"
    Plots <- c(Plots, OS_Title, OS_Plots)
    YourOSPlots <<- OS_Plots
  }



  #Determine cut off for progression-free survival
  if (PFS == "Yes") {
    PFS.res.cut <- surv_cutpoint(Data, time = Progression, event = ProgressionStatus, Variables)
    PFS_Plots <- plot(PFS.res.cut, Variables, palette = c("#d70033", "#5596e6"), main="Progression-Free Survival", font.main=2, cex.main=5)
    PFS_Title <- "Progression-free survival cut offs;"
    Plots <- c(Plots, PFS_Title, PFS_Plots)
    YourPFSPlots <<- PFS_Plots
  }

  YourPlotsAll <<- Plots



  return(Plots)

  # pdf("C:/Users/liamj/Desktop/R/Plots.pdf")
  # CSS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "CSS", Variables)
  # CSS_Plots <- plot(CSS.res.cut, Variables, palette = c("#d70033", "#5596e6"), main=list(font=2, cex=5, label="Cancer-Specific Survival"))
  #print(CSS_Plots)
  #dev.off()


  for(i in seq_along(Plots) ){
    filename <- paste(names(Plots[i]),'-myFile.pdf', sep = "")
    pdf(filename)
    plot(Plots[i])
    dev.off()
  }



}
