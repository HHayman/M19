#'rCut



#' This produces cut offs for each variable using the maximal rank statistics method.

#' @import survival
#' @import maxstat
#' @import survminer
#' @import tidyverse



#' @export



#Function

rCut <- function(Data, CSS, OS, PFS, PlotPalette="SPSS", ID, Survival, SurvivalStatus, Progression, ProgressionStatus, Variables)
{
  if (!all(Variables %in% colnames(Data)))
    stop("Some variables are not found in the data: ",
         paste(setdiff(Variables, colnames(Data)), collapse = ", "))



  #Reconfigure variables
  names(Data)[names(Data) == SurvivalStatus] <- "SurvivalStatus"
  names(Data)[names(Data) == Survival] <- "Survival"
  Data$CSS <- ifelse(Data$SurvivalStatus == 1, c("1"), c("0"))
  Data$OS <- ifelse(Data$SurvivalStatus > 0, c("1"), c("0"))
  Data$CSS <- as.numeric(Data$CSS)
  Data$OS <- as.numeric(Data$OS)


  Number <- 1
  CurrentDirectory <- toString(getwd())
  NewDirectory <- paste("CutOffs", Sys.Date(), Number, sep = '_')


  while(file.exists(NewDirectory))
  {
    Number <- Number + 1
    NewDirectory <- paste("CutOffs", Sys.Date(), Number, sep = '_')
  }


  dir.create(NewDirectory)
  setwd(NewDirectory)




  Plots <- list()

  sink("waste.txt")

  if (PlotPalette == "SPSS") {
    #Determine cut off for cancer-specific survival
    if (CSS == "Yes") {
      CSS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "CSS", Variables)
      CSS_Plots <- plot(CSS.res.cut, Variables, palette = c("#d70033", "#5596e6"), main="Cancer-Specific Survival")
      CSS_Title <- "Cancer-specific survival cut offs;"
      Plots <- c(Plots, CSS_Plots)
      YourCSSPlots <<- CSS_Plots
    }



    #Determine cut off for overall survival
    if (OS == "Yes") {
      OS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "OS", Variables)
      OS_Plots <- plot(OS.res.cut, Variables, palette = c("#d70033", "#5596e6"), main="Overall Survival")
      OS_Title <- "Overall survival cut offs;"
      Plots <- c(Plots, OS_Plots)
      YourOSPlots <<- OS_Plots
    }



    #Determine cut off for progression-free survival
    if (PFS == "Yes") {
      PFS.res.cut <- surv_cutpoint(Data, time = Progression, event = ProgressionStatus, Variables)
      PFS_Plots <- plot(PFS.res.cut, Variables, palette = c("#d70033", "#5596e6"), main="Progression-Free Survival")
      PFS_Title <- "Progression-free survival cut offs;"
      Plots <- c(Plots, PFS_Plots)
      YourPFSPlots <<- PFS_Plots
    }
  }







  if (PlotPalette == "Grayscale") {
    #Determine cut off for cancer-specific survival
    if (CSS == "Yes") {
      CSS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "CSS", Variables)
      CSS_Plots <- plot(CSS.res.cut, Variables, palette = c("#000000", "#ABABAB", "#545454", "#FFFFFF"), main="Cancer-Specific Survival")
      CSS_Title <- "Cancer-specific survival cut offs;"
      Plots <- c(Plots, CSS_Plots)
      YourCSSPlots <<- CSS_Plots
    }



    #Determine cut off for overall survival
    if (OS == "Yes") {
      OS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "OS", Variables)
      OS_Plots <- plot(OS.res.cut, Variables, palette = c("#000000", "#FFFFFF"), main="Overall Survival")
      OS_Title <- "Overall survival cut offs;"
      Plots <- c(Plots, OS_Plots)
      YourOSPlots <<- OS_Plots
    }



    #Determine cut off for progression-free survival
    if (PFS == "Yes") {
      PFS.res.cut <- surv_cutpoint(Data, time = Progression, event = ProgressionStatus, Variables)
      PFS_Plots <- plot(PFS.res.cut, Variables, palette = c("#000000", "#FFFFFF"), main="Progression-Free Survival")
      PFS_Title <- "Progression-free survival cut offs;"
      Plots <- c(Plots, PFS_Plots)
      YourPFSPlots <<- PFS_Plots
    }
  }



  YourPlotsAll <<- Plots

  pdf("YourPlots.pdf");
  print(YourPlotsAll);
  dev.off();



  #VariableNames <- names(Variables)
  #VariableLength=1:length(VariableNames)
  #for (i in VariableLength) {
  #png(paste0("CSS_", names(Variables)[i], ".png"))
  #CSS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "CSS", Variables[i])
  #plot(CSS.res.cut, Variables[i], palette = c("#d70033", "#5596e6"), main="Cancer-Specific Survival")
  #dev.off()
  #}


  #for (i in 1:length(CSS_Plots)) {
  #png(paste0("CSS_", CSS_Plots[i], ".png"))
  #dev.off()
  #}


  message("Wonder Woman hopes that you enjoy your cut-offs, which she has popped in a PDF (YourPlots) within a folder (CutOffs_Date) in your R directory.")


  return(Plots);


  sink();

  setwd(CurrentDirectory)

}
