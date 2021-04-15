#'rCut



#' This produces cut offs for each variable using the maximal rank statistics method.

#' @import survival
#' @import maxstat
#' @import survminer
#' @import tidyverse
#' @import gplots



#' @export



#Function

rCut <- function(Data, CSS, OS, PFS, DFS, RFS, MISC, minprop="0.1", PlotPalette="SPSS", ID, Survival="Survival", SurvivalStatus="SurvivalStatus", Progression="Progression", ProgressionStatus="ProgressionStatus", DiseaseFree="DiseaseFree", DiseaseFreeStatus="DiseaseFreeStatus", Recurrence="Recurrence", RecurrenceStatus="RecurrenceStatus", Miscellaneous="Miscellaneous", MiscellaneousStatus="MiscellaneousStatus", Variables)
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
  names(Data)[names(Data) == ProgressionStatus] <- "ProgressionStatus"
  names(Data)[names(Data) == Progression] <- "Progression"
  names(Data)[names(Data) == DiseaseFreeStatus] <- "DiseaseFreeStatus"
  names(Data)[names(Data) == DiseaseFree] <- "DiseaseFree"
  Data$DFS <- ifelse(Data$DiseaseFreeStatus > 0, c("1"), c("0"))
  Data$DFS <- as.numeric(Data$DFS)
  names(Data)[names(Data) == RecurrenceStatus] <- "RecurrenceStatus"
  names(Data)[names(Data) == Recurrence] <- "Recurrence"
  Data$RFS <- ifelse(Data$RecurrenceStatus == 1, c("1"), c("0"))

  names(Data)[names(Data) == MiscellaneousStatus] <- "MiscellaneousStatus"
  names(Data)[names(Data) == Miscellaneous] <- "Miscellaneous"
  Data$MISC <- ifelse(Data$MiscellaneousStatus == 1, c("1"), c("0"))


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
      CSS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "CSS", minprop = minprop, Variables)
      CSSCutPoints <- summary(CSS.res.cut)
      CSS_Plots <- plot(CSS.res.cut, Variables, palette = c("#d70033", "#5596e6"), main="Cancer-Specific Survival")
      CSS_Title <- "Cancer-specific survival cut offs;"
      Plots <- c(Plots, CSS_Plots)
      YourCSSPlots <<- CSS_Plots
      n = length(Variables)
      for (i in 1:n) {
        png(paste0("CSS_", Variables[i], ".png"))
        CSS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "CSS", minprop = minprop, Variables[i])
        SPSSCSSSinglePlot <- plot(CSS.res.cut, Variables[i], palette = c("#d70033", "#5596e6"), main="Cancer-Specific Survival")
        print(SPSSCSSSinglePlot)
        dev.off()
      }
    }



    #Determine cut off for overall survival
    if (OS == "Yes") {
      OS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "OS", minprop = minprop, Variables)
      OSCutPoints <- summary(OS.res.cut)
      OS_Plots <- plot(OS.res.cut, Variables, palette = c("#d70033", "#5596e6"), main="Overall Survival")
      OS_Title <- "Overall survival cut offs;"
      Plots <- c(Plots, OS_Plots)
      YourOSPlots <<- OS_Plots
      n = length(Variables)
      for (i in 1:n) {
        png(paste0("OS_", Variables[i], ".png"))
        OS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "OS", minprop = minprop, Variables[i])
        SPSSOSSinglePlot <- plot(OS.res.cut, Variables[i], palette = c("#d70033", "#5596e6"), main="Overall Survival")
        print(SPSSOSSinglePlot)
        dev.off()
      }
    }



    #Determine cut off for progression-free survival
    if (PFS == "Yes") {
      PFS.res.cut <- surv_cutpoint(Data, time = "Progression", event = "PFS", minprop = minprop, Variables)
      PFSCutPoints <- summary(PFS.res.cut)
      PFS_Plots <- plot(PFS.res.cut, Variables, palette = c("#d70033", "#5596e6"), main="Progression-Free Survival")
      PFS_Title <- "Progression-free survival cut offs;"
      Plots <- c(Plots, PFS_Plots)
      YourPFSPlots <<- PFS_Plots
      n = length(Variables)
      for (i in 1:n) {
        png(paste0("PFS_", Variables[i], ".png"))
        PFS.res.cut <- surv_cutpoint(Data, time = "Progression", event = "PFS", minprop = minprop, Variables[i])
        SPSSPFSSinglePlot <- plot(PFS.res.cut, Variables[i], palette = c("#d70033", "#5596e6"), main="Progression-Free Survival")
        print(SPSSPFSSinglePlot)
        dev.off()
      }
    }



    #Determine cut off for disease-free survival
    if (DFS == "Yes") {
      DFS.res.cut <- surv_cutpoint(Data, time = "DiseaseFree", event = "DFS", minprop = minprop, Variables)
      DFSCutPoints <- summary(DFS.res.cut)
      DFS_Plots <- plot(DFS.res.cut, Variables, palette = c("#d70033", "#5596e6"), main="Disease-Free Survival")
      DFS_Title <- "Disease-free survival cut offs;"
      Plots <- c(Plots, DFS_Plots)
      YourDFSPlots <<- DFS_Plots
      n = length(Variables)
      for (i in 1:n) {
        png(paste0("DFS_", Variables[i], ".png"))
        DFS.res.cut <- surv_cutpoint(Data, time = "DiseaseFree", event = "DFS", minprop = minprop, Variables[i])
        SPSSDFSSinglePlot <- plot(DFS.res.cut, Variables[i], palette = c("#d70033", "#5596e6"), main="Disease-Free Survival")
        print(SPSSDFSSinglePlot)
        dev.off()
      }
    }

    if (RFS == "Yes") {
      RFS.res.cut <- surv_cutpoint(Data, time = "Recurrence", event = "RFS", minprop = minprop, Variables)
      RFSCutPoints <- summary(RFS.res.cut)
      RFS_Plots <- plot(RFS.res.cut, Variables, palette = c("#d70033", "#5596e6"), main="Recurrence Survival")
      RFS_Title <- "Recurrence-free survival cut offs;"
      Plots <- c(Plots, RFS_Plots)
      YourRFSPlots <<- RFS_Plots
      n = length(Variables)
      for (i in 1:n) {
        png(paste0("RFS_", Variables[i], ".png"))
        RFS.res.cut <- surv_cutpoint(Data, time = "Recurrence", event = "RFS", minprop = minprop, Variables[i])
        SPSSRFSSinglePlot <- plot(RFS.res.cut, Variables[i], palette = c("#d70033", "#5596e6"), main="Recurrence-Free Survival")
        print(SPSSRFSSinglePlot)
        dev.off()
      }
    }

    if (MISC == "Yes") {
      MISC.res.cut <- surv_cutpoint(Data, time = "Miscellaneous", event = "MISC", minprop = minprop, Variables)
      MISCCutPoints <- summary(MISC.res.cut)
      MISC_Plots <- plot(MISC.res.cut, Variables, palette = c("#d70033", "#5596e6"), main="Miscellaneous Survival")
      MISC_Title <- "Miscellaneous survival cut offs;"
      Plots <- c(Plots, MISC_Plots)
      YourMISCPlots <<- MISC_Plots
      n = length(Variables)
      for (i in 1:n) {
        png(paste0("CSS_", Variables[i], ".png"))
        MISC.res.cut <- surv_cutpoint(Data, time = "Miscellaneous", event = "MISC", minprop = minprop, Variables[i])
        SPSSCSSSinglePlot <- plot(MISC.res.cut, Variables[i], palette = c("#d70033", "#5596e6"), main="Miscellaneous Survival")
        print(SPSSMISCSinglePlot)
        dev.off()
      }
    }

  }






  if (PlotPalette == "Grayscale") {

    #Determine cut off for cancer-specific survival
    if (CSS == "Yes") {
      CSS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "CSS", minprop = minprop, Variables)
      CSSCutPoints <- summary(CSS.res.cut)
      CSS_Plots <- plot(CSS.res.cut, Variables, palette = c("#000000", "#ABABAB", "#545454", "#FFFFFF"), main="Cancer-Specific Survival")
      CSS_Title <- "Cancer-specific survival cut offs;"
      Plots <- c(Plots, CSS_Plots)
      YourCSSPlots <<- CSS_Plots
      n = length(Variables)
      for (i in 1:n) {
        png(paste0("CSS_", Variables[i], ".png"))
        CSS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "CSS", minprop = minprop, Variables[i])
        GSCSSinglePlot <- plot(CSS.res.cut, Variables[i], palette = c("#000000", "#ABABAB", "#545454", "#FFFFFF"), main="Cancer-Specific Survival")
        print(GSCSSinglePlot)
        dev.off()
      }
    }



    #Determine cut off for overall survival
    if (OS == "Yes") {
      OS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "OS", minprop = minprop, Variables)
      OSCutPoints <- summary(OS.res.cut)
      OS_Plots <- plot(OS.res.cut, Variables, palette = c("#000000", "#ABABAB", "#545454", "#FFFFFF"), main="Overall Survival")
      OS_Title <- "Overall survival cut offs;"
      Plots <- c(Plots, OS_Plots)
      YourOSPlots <<- OS_Plots
      n = length(Variables)
      for (i in 1:n) {
        png(paste0("OS_", Variables[i], ".png"))
        OS.res.cut <- surv_cutpoint(Data, time = "Survival", event = "OS", minprop = minprop, Variables[i])
        GSOSinglePlot <- plot(OS.res.cut, Variables[i], palette = c("#000000", "#ABABAB", "#545454", "#FFFFFF"), main="Overall Survival")
        print(GSOSinglePlot)
        dev.off()
      }
    }


    #Determine cut off for progression-free survival
    if (PFS == "Yes") {
      PFS.res.cut <- surv_cutpoint(Data, time = Progression, event = ProgressionStatus, minprop = minprop, Variables)
      PFSCutPoints <- summary(PFS.res.cut)
      PFS_Plots <- plot(PFS.res.cut, Variables, palette = c("#000000", "#ABABAB", "#545454", "#FFFFFF"), main="Progression-Free Survival")
      PFS_Title <- "Progression-free survival cut offs;"
      Plots <- c(Plots, PFS_Plots)
      YourPFSPlots <<- PFS_Plots
      n = length(Variables)
      for (i in 1:n) {
        png(paste0("PFS_", Variables[i], ".png"))
        PFS.res.cut <- surv_cutpoint(Data, time = "Progression", event = "ProgressionStatus", minprop = minprop, Variables[i])
        GSPFSSinglePlot <- plot(PFS.res.cut, Variables[i], palette = c("#000000", "#ABABAB", "#545454", "#FFFFFF"), main="Progression-Free Survival")
        print(GSPFSSinglePlot)
        dev.off()
      }
    }

    #Determine cut off for disease-free survival
    if (DFS == "Yes") {
      DFS.res.cut <- surv_cutpoint(Data, time = DiseaseFree, event = DiseaseFreeStatus, minprop = minprop, Variables)
      DFSCutPoints <- summary(DFS.res.cut)
      DFS_Plots <- plot(DFS.res.cut, Variables, palette = c("#000000", "#ABABAB", "#545454", "#FFFFFF"), main="Disease-Free Survival")
      DFS_Title <- "Disease-free survival cut offs;"
      Plots <- c(Plots, DFS_Plots)
      YourDFSPlots <<- DFS_Plots
      n = length(Variables)
      for (i in 1:n) {
        png(paste0("DFS_", Variables[i], ".png"))
        DFS.res.cut <- surv_cutpoint(Data, time = "DiseaseFree", event = "DiseaseFreeStatus", minprop = minprop, Variables[i])
        GSDFSSinglePlot <- plot(DFS.res.cut, Variables[i], palette = c("#000000", "#ABABAB", "#545454", "#FFFFFF"), main="Disease-Free Survival")
        print(GSDFSSinglePlot)
        dev.off()
      }
    }

    if (RFS == "Yes") {
      RFS.res.cut <- surv_cutpoint(Data, time = "Recurrence", event = "RFS", minprop = minprop, Variables)
      RFSCutPoints <- summary(RFS.res.cut)
      RFS_Plots <- plot(RFS.res.cut, Variables, palette = c("#000000", "#ABABAB", "#545454", "#FFFFFF"), main="Recurrence Survival")
      RFS_Title <- "Recurrence-free survival cut offs;"
      Plots <- c(Plots, RFS_Plots)
      YourRFSPlots <<- RFS_Plots
      n = length(Variables)
      for (i in 1:n) {
        png(paste0("RFS_", Variables[i], ".png"))
        RFS.res.cut <- surv_cutpoint(Data, time = "Recurrence", event = "RFS", minprop = minprop, Variables[i])
        SPSSRFSSinglePlot <- plot(RFS.res.cut, Variables[i], palette = c("#000000", "#ABABAB", "#545454", "#FFFFFF"), main="Recurrence-Free Survival")
        print(SPSSRFSSinglePlot)
        dev.off()
      }
    }

    if (MISC == "Yes") {
      MISC.res.cut <- surv_cutpoint(Data, time = "Miscellaneous", event = "MISC", minprop = minprop, Variables)
      MISCCutPoints <- summary(MISC.res.cut)
      MISC_Plots <- plot(MISC.res.cut, Variables, palette = c("#000000", "#ABABAB", "#545454", "#FFFFFF"), main="Miscellaneous Survival")
      MISC_Title <- "Miscellaneous survival cut offs;"
      Plots <- c(Plots, MISC_Plots)
      YourMISCPlots <<- MISC_Plots
      n = length(Variables)
      for (i in 1:n) {
        png(paste0("CSS_", Variables[i], ".png"))
        MISC.res.cut <- surv_cutpoint(Data, time = "Miscellaneous", event = "MISC", minprop = minprop, Variables[i])
        SPSSCSSSinglePlot <- plot(MISC.res.cut, Variables[i], palette = c("#000000", "#ABABAB", "#545454", "#FFFFFF"), main="Miscellaneous Survival")
        print(SPSSMISCSinglePlot)
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
  if (DFS == "Yes") {
    textplot(DFSCutPoints, halign="center", valign="top", cex = 1)
    title("DFS Cut Offs")
  }
  if (RFS == "Yes") {
    textplot(RFSCutPoints, halign="center", valign="top", cex = 1)
    title("RFS Cut Offs")
  }
  if (MISC == "Yes") {
    textplot(MISCCutPoints, halign="center", valign="top", cex = 1)
    title("MISC Cut Offs")
  }
  print(YourPlotsAll);
  dev.off();


  write.csv(OriginalData,"OriginalData.csv", row.names = FALSE)
  write.csv(Data,"ModifiedData.csv", row.names = FALSE)



  message("Wonder Woman hopes that you enjoy your cut-offs. Blessings of Gaea be with you :)")


  return(Plots);


  sink();

}
