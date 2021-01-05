#'rCut



#' This produces cut offs for each variable using the maximal rank statistics method.

#' @import survival
#' @import maxstat
#' @import survminer
#' @import tidyverse
#' @import gplots



#' @export



#Function

rCut <- function(Data, CSS, OS, PFS, PlotPalette="SPSS", ID, Survival, SurvivalStatus, Progression, ProgressionStatus, Variables)
{
  if (!all(Variables %in% colnames(Data)))
    stop("Some variables are not found in the data: ",
         paste(setdiff(Variables, colnames(Data)), collapse = ", "))


  OriginalData <- Data


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

  setwd(CurrentDirectory)
  dir.create(NewDirectory)
  setwd(NewDirectory)
  on.exit(setwd(CurrentDirectory), add = TRUE)




  Plots <- list()

  sink("waste.txt")

  if (PlotPalette == "SPSS") {
    #Determine cut off for cancer-specific survival
    if (CSS == "Yes") {
      CSS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "CSS", Variables)
      CSSCutPoints <- summary(CSS.res.cut)
      CSS_Plots <- plot(CSS.res.cut, Variables, palette = c("#d70033", "#5596e6"), main="Cancer-Specific Survival")
      CSS_Title <- "Cancer-specific survival cut offs;"
      Plots <- c(Plots, CSS_Plots)
      YourCSSPlots <<- CSS_Plots
      n = length(Variables)
      for (i in 1:n) {
        png(paste0("CSS_", Variables[i], ".png"))
        CSS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "CSS", Variables[i])
        SPSSCSSSinglePlot <- plot(CSS.res.cut, Variables[i], palette = c("#d70033", "#5596e6"), main="Cancer-Specific Survival")
        print(SPSSCSSSinglePlot)
        dev.off()
      }
    }



    #Determine cut off for overall survival
    if (OS == "Yes") {
      OS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "OS", Variables)
      OSCutPoints <- summary(OS.res.cut)
      OS_Plots <- plot(OS.res.cut, Variables, palette = c("#d70033", "#5596e6"), main="Overall Survival")
      OS_Title <- "Overall survival cut offs;"
      Plots <- c(Plots, OS_Plots)
      YourOSPlots <<- OS_Plots
      n = length(Variables)
      for (i in 1:n) {
        png(paste0("OS_", Variables[i], ".png"))
        OS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "OS", Variables[i])
        SPSSOSSinglePlot <- plot(OS.res.cut, Variables[i], palette = c("#d70033", "#5596e6"), main="Overall Survival")
        print(SPSSOSSinglePlot)
        dev.off()
      }
    }



    #Determine cut off for progression-free survival
    if (PFS == "Yes") {
      PFS.res.cut <- surv_cutpoint(Data, time = Progression, event = ProgressionStatus, Variables)
      PFSCutPoints <- summary(PFS.res.cut)
      PFS_Plots <- plot(PFS.res.cut, Variables, palette = c("#d70033", "#5596e6"), main="Progression-Free Survival")
      PFS_Title <- "Progression-free survival cut offs;"
      Plots <- c(Plots, PFS_Plots)
      YourPFSPlots <<- PFS_Plots
      n = length(Variables)
      for (i in 1:n) {
        png(paste0("PFS_", Variables[i], ".png"))
        PFS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "PFS", Variables[i])
        SPSSPFSSinglePlot <- plot(PFS.res.cut, Variables[i], palette = c("#d70033", "#5596e6"), main="Progression-Free Survival")
        print(SinglePlot)
        dev.off()
      }
    }
  }






  if (PlotPalette == "Grayscale") {
    #Determine cut off for cancer-specific survival
    if (CSS == "Yes") {
      CSS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "CSS", Variables)
      CSSCutPoints <- summary(CSS.res.cut)
      CSS_Plots <- plot(CSS.res.cut, Variables, palette = c("#000000", "#ABABAB", "#545454", "#FFFFFF"), main="Cancer-Specific Survival")
      CSS_Title <- "Cancer-specific survival cut offs;"
      Plots <- c(Plots, CSS_Plots)
      YourCSSPlots <<- CSS_Plots
      n = length(Variables)
      for (i in 1:n) {
        png(paste0("CSS_", Variables[i], ".png"))
        CSS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "CSS", Variables[i])
        GSCSSinglePlot <- plot(CSS.res.cut, Variables[i], palette = c("#000000", "#ABABAB", "#545454", "#FFFFFF"), main="Cancer-Specific Survival")
        print(GSCSSinglePlot)
        dev.off()
      }
    }



    #Determine cut off for overall survival
    if (OS == "Yes") {
      OS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "OS", Variables)
      OSCutPoints <- summary(OS.res.cut)
      OS_Plots <- plot(OS.res.cut, Variables, palette = c("#000000", "#ABABAB", "#545454", "#FFFFFF"), main="Overall Survival")
      OS_Title <- "Overall survival cut offs;"
      Plots <- c(Plots, OS_Plots)
      YourOSPlots <<- OS_Plots
      n = length(Variables)
      for (i in 1:n) {
        png(paste0("OS_", Variables[i], ".png"))
        OS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "OS", Variables[i])
        GSOSinglePlot <- plot(OS.res.cut, Variables[i], palette = c("#000000", "#ABABAB", "#545454", "#FFFFFF"), main="Overall Survival")
        print(GSOSinglePlot)
        dev.off()
      }
    }



    #Determine cut off for progression-free survival
    if (PFS == "Yes") {
      PFS.res.cut <- surv_cutpoint(Data, time = Progression, event = ProgressionStatus, Variables)
      PFSCutPoints <- summary(PFS.res.cut)
      PFS_Plots <- plot(PFS.res.cut, Variables, palette = c("#000000", "#ABABAB", "#545454", "#FFFFFF"), main="Progression-Free Survival")
      PFS_Title <- "Progression-free survival cut offs;"
      Plots <- c(Plots, PFS_Plots)
      YourPFSPlots <<- PFS_Plots
      n = length(Variables)
      for (i in 1:n) {
        png(paste0("PFS_", Variables[i], ".png"))
        PFS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "PFS", Variables[i])
        GSPFSSinglePlot <- plot(PFS.res.cut, Variables[i], palette = c("#000000", "#ABABAB", "#545454", "#FFFFFF"), main="Progression-Free Survival")
        print(GSPFSSinglePlot)
        dev.off()
      }
    }
  }



  YourPlotsAll <<- Plots

  pdf("YourPlots.pdf");
  if (CSS == "Yes") {
    textplot(CSSCutPoints, halign="center", valign="top", cex = 1)
    title("CSS Cut Offs")
  }
  if (OS == "Yes") {
    textplot(OSCutPoints, halign="center", valign="top", cex = 1)
    title("OS Cut Offs")
  }
  if (PFS == "Yes") {
    textplot(PFSCutPoints, halign="center", valign="top", cex = 1)
    title("PFS Cut Offs")
  }
  print(YourPlotsAll);
  dev.off();


  write.csv(OriginalData,"OriginalData.csv", row.names = FALSE)
  write.csv(Data,"ModifiedData.csv", row.names = FALSE)



  message("Wonder Woman hopes that you enjoy your cut-offs. Blessings of Gaea be with you :)")


  return(Plots);


  sink();

}
