#'ImmSurv


#' This function plots two variables on a scatter plot and contextualises the plot using survival time.

#' @import ggpubr
#' @import ggplot2


#' @export

#Function
ImmSurv <- function(Data, ID = "ID", Status = "Status", Survival = "Quartiles", Stage = "Stage", SurvivalGroups = "Groups", Marker = "Marker", Variable1 = "Variable1", Variable1Unit = "Variable1Unit", Variable2 = "Variable2", Variable2Unit = "Variable2Unit") {

  names(Data)[names(Data) == ID] <- "ID"
  names(Data)[names(Data) == Status] <- "Status"
  names(Data)[names(Data) == Survival] <- "Survival"
  names(Data)[names(Data) == Stage] <- "Stage"
  names(Data)[names(Data) == Variable1] <- "Variable1"
  names(Data)[names(Data) == Variable1Unit] <- "Variable1Unit"
  names(Data)[names(Data) == Variable2] <- "Variable2"
  names(Data)[names(Data) == Variable2Unit] <- "Variable2Unit"



  #Selects data based on chosen survival metric
  Data <- Data[which(Data$Status=='1'),]

  #Adds a column for determining colour
  Data$Colour <- "Colour"

  #Removal of variable outliers
  V1Q <- quantile(Data$Variable1, probs=c(.25, .75), na.rm = FALSE)
  V1IQR <- IQR(Data$Variable1)
  V2Q <- quantile(Data$Variable1, probs=c(.25, .75), na.rm = FALSE)
  V2IQR <- IQR(Data$Variable1)
  Data <- Data[which(Data$Variable1 > (V1Q[1] - 1.5*V1IQR) & Data$Variable1 < (V1Q[2]+1.5*V1IQR)),]
  Data <- Data[which(Data$Variable1 > (V2Q[1] - 1.5*V2IQR) & Data$Variable1 < (V2Q[2]+1.5*V2IQR)),]


  if (SurvivalGroups == "Quartiles") {
    #Determination of survival groups
    SurvQ <- quantile(Data$Survival, probs=c(.25, .75), na.rm = FALSE)
    SurvIQR <- IQR(Data$Survival)
    MaxSurvival <- max(Data$Survival)
    MinSurvival <- min(Data$Survival)
    MeanSurvival <- mean(Data$Survival)
    SdSurvival <- sd(Data$Survival)

    #Colour determination
    Data$Colour[Data$Survival < SurvQ[1]] = "#CC3232"
    Data$Colour[Data$Survival >= SurvQ[1] & Data$Survival < SurvQ[2]] = "#E7B416"
    Data$Colour[Data$Survival >= SurvQ[2]] = "#2DC937"
  }



  if (SurvivalGroups == "Mean") {
    #Determination of survival groups
    MeanSurvival <- mean(Data$Survival)

    #Colour determination
    Data$Colour[Data$Survival <= MeanSurvival] = "#CC3232"
    Data$Colour[Data$Survival > MeanSurvival] = "#2DC937"
  }



  if (SurvivalGroups == "Median") {
    #Determination of survival groups
    MedianSurvival <- median(Data$Survival)

    #Colour determination
    Data$Colour[Data$Survival <= MedianSurvival] = "#CC3232"
    Data$Colour[Data$Survival > MedianSurvival] = "#2DC937"
  }



  #determination of variable descriptives
  customCeiling <- function(x, Decimals=1) {
    x2<-x*10^Decimals
    ceiling(x2)/10^Decimals
  }
  MaxVariable1 = max(Data$Variable1)
  MaxVariable1 <- customCeiling(MaxVariable1)
  MaxVariable2 = max(Data$Variable2)
  MaxVariable2 <- customCeiling(MaxVariable2)
  AxisMax <- max(MaxVariable1, MaxVariable2)



  if (SurvivalGroups == "Quartiles") {
    #Plots

    xAxis = paste(Variable1, " (", Variable1Unit, ")", sep = "")
    yAxis = paste(Variable2, " (", Variable2Unit, ")", sep = "")
    Title = paste(Variable1, " vs ", Variable2, " - Survival by Quartiles", sep = "")

    if (Marker == "Default") {
      plot(Data$Variable1,Data$Variable2, col=Data$Colour, xlim=c(0,AxisMax), ylim=c(0,AxisMax), cex=0.7, xlab = xAxis, ylab = yAxis, main = Title, pch = 16)
      legend("topright", legend=c("Q1", "Q2 + Q3", "Q4"), col=c("#CC3232", "#E7B416", "#2DC937"), pch = 16, cex=0.7)
    }

    if (Marker == "Stage") {
      plot(Data$Variable1,Data$Variable2, col=Data$Colour, xlim=c(0,AxisMax), ylim=c(0,AxisMax), cex=0.7, xlab = xAxis, ylab = yAxis, main = Title, pch = as.character(Data$Stage))
      legend("topright", legend=c("Q1", "Q2 + Q3", "Q4"), col=c("#CC3232", "#E7B416", "#2DC937"), pch = 16, cex=0.7)
    }
  }


  if (SurvivalGroups == "Mean") {
    #Plots

    xAxis = paste(Variable1, " (", Variable1Unit, ")", sep = "")
    yAxis = paste(Variable2, " (", Variable2Unit, ")", sep = "")
    Title = paste(Variable1, " vs ", Variable2, " - Survival by Mean", sep = "")
    plot(Data$Variable1,Data$Variable2, col=Data$Colour, xlim=c(0,AxisMax), ylim=c(0,AxisMax), cex=0.7, xlab = xAxis, ylab = yAxis, main = Title, pch = 16)
    legend("topright", legend=c("<= mean", "> mean"), col=c("#CC3232", "#2DC937"), pch = 16, cex=0.7)
  }


  if (SurvivalGroups == "Median") {
    #Plots

    xAxis = paste(Variable1, " (", Variable1Unit, ")", sep = "")
    yAxis = paste(Variable2, " (", Variable2Unit, ")", sep = "")
    Title = paste(Variable1, " vs ", Variable2, " - Survival by Median", sep = "")
    plot(Data$Variable1,Data$Variable2, col=Data$Colour, xlim=c(0,AxisMax), ylim=c(0,AxisMax), cex=0.7, xlab = xAxis, ylab = yAxis, main = Title, pch = 16)
    legend("topright", legend=c("<= median", "> median"), col=c("#CC3232", "#2DC937"), pch = 16, cex=0.7)
  }



  return(plot)
}
