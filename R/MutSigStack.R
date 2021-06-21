#'MutSigStack


#' @author Hannah Hayman, \email{l.hayman.1@@research.gla.ac.uk}
#' This function takes DMG signatures for X number of comparisons and stacks them for easy visualisation.


#' @import tidyverse
#' @import ggpubr
#' @import ggplot2


#' @param Subdirectory A subdirectory of your working directory, in which you store your comparison files.
#' @param ORlow The lower boundary for your OR. DMGs with an odds ratio between this value and your ORhigh will be excluded.Default value is 0.5.
#' @param ORhigh The upper boundary for your OR. DMGs with an odds ratio between this value and your ORlow will be excluded.Default value is 2.
#' @param pval Your chosen cut off for 'statistical significance'. Default value is 0.05.
#' @param Palette Choose your colour palette; greyscale, blue, red, pink, green. Default palette is greyscale.


#' @return Returns a matrix plot, stacking mutational signatures generated from the input comparison files.


#' @examples MutSigStack(Subdirectory = "Files", ORlow = "0.5", ORhigh = "2", pval = "0.1", Palette = "Pink")


#' @export




#Function
MutSigStack <- function(Subdirectory = "", ORlow = "0.5", ORhigh = "2", pval = "0.05", Palette = "Greyscale") {


  #Check input for errors
  Colours = c("Greyscale", "Blue", "Red", "Green", "Pink")
  if (!all(Palette %in% Colours))
    stop("Incompatible input for 'Palette'. Available colours schemes are; Greyscale, Red, Pink, Blue and Green.")



  #Alter working directory
  WD <- getwd()
  if (file.exists(Subdirectory)){
  setwd(Subdirectory)
  } else {
  stop('Incompatible subdirectory. Please check the name of the folder containing your files.')
  }


  on.exit(setwd(WD), add = TRUE)



  #Read and rename data files
  filenames <- gsub("\\.csv$","", list.files(pattern="\\.csv$"))
  DataFrames <- lapply(list.files(pattern="\\.csv$"), read.csv)
  names(DataFrames) <- filenames
  setwd(WD)


  #Cut metadata from comparison files
  for(i in 1:length(filenames)){
    DataFrames[[filenames[i]]] <- head(DataFrames[[filenames[i]]], - 6)
  }


  #Change infinite ORs to a value of '2'
  for(i in 1:length(filenames)){
    DataFrames[[filenames[i]]]$or <- ifelse(DataFrames[[filenames[i]]]$or == "Inf", 2, DataFrames[[filenames[i]]]$or)
  }


  #Change p values to numeric class
  for(i in 1:length(filenames)){
    DataFrames[[filenames[i]]]$pval <- as.numeric(DataFrames[[filenames[i]]]$pval)
  }


  #Select cases within parameters of chosen OR and p value
  for(i in 1:length(filenames)){
    DataFrames[[filenames[i]]] <- DataFrames[[filenames[i]]][ which(DataFrames[[filenames[i]]]$or >= as.numeric(ORhigh) & DataFrames[[filenames[i]]]$pval <= as.numeric(pval) | DataFrames[[filenames[i]]]$or <= as.numeric(ORlow) & DataFrames[[filenames[i]]]$pval <= as.numeric(pval)), ]
  }


  #Variables cut down to genes and odds ratio
  Variables <- c("Hugo_Symbol", "or")
  for(i in 1:length(filenames)){
    DataFrames[[filenames[i]]] <- DataFrames[[filenames[i]]][Variables]
  }


  #Append 'Compartment' variable
  for(i in 1:length(filenames)){
    DataFrames[[filenames[i]]]$Compartment <- filenames[i]
  }


  #Rearrange order of variables
  for(i in 1:length(filenames)){
    DataFrames[[filenames[i]]] <- DataFrames[[filenames[i]]][c("Compartment", "Hugo_Symbol", "or")]
  }


  #Merging data frames
  Merge1 = do.call(rbind, DataFrames)
  Merge2 <- tidyr::spread(Merge1, Hugo_Symbol, or, fill = 0)
  Data <- tidyr::gather(Merge2, Hugo_Symbol, or, -Compartment)


  #Coding of odds ratio
  Data$or_coded <- Data$or
  Data$or_coded <- ifelse(Data$or_coded >1, 1, Data$or_coded)
  Data$or_coded <- ifelse(Data$or_coded <1 & Data$or_coded >0, 0.5, Data$or_coded)
  names(Data)[names(Data) == "or_coded"] <- "Odds_Ratio"



  #Generation of plot

  if (Palette == "Greyscale") {
  MutSigStack <<- ggplot(data = Data, aes(x = Hugo_Symbol, y = Compartment)) + geom_tile(aes(fill = Odds_Ratio)) +
    labs(title = "Mutational Signatures by Lymphocyte Compartment", subtitle = "", y = "Lymphocyte Compartment", x = "Gene", tag = "") +
    scale_fill_manual(values = c("slategrey", "black", "white"), name = "Odds_Ratio", limits = c("0.5", "1", "0"),
    labels = c("OR < 0.5", "OR > 2", "")) + theme(panel.background = element_rect(fill = "white", colour = "white"),
    axis.line = element_line(size = 0.5, linetype = "solid", colour = "white")) +
    theme(plot.title = element_text(face = "bold", colour = "black", size = 15, hjust = 0.5)) + theme(axis.title.x = element_text(face = "bold",
    colour = "black", size = 12)) + theme(axis.text.x = element_text(colour = "black", size = 10, angle = 90,
    vjust = 0.5, hjust=1)) + theme(axis.title.y = element_text(face = "bold", colour = "black", size = 12, angle = 90,
    vjust = 0.5, hjust = 0.5)) + theme(axis.text.y = element_text(colour = "black", size = 10, vjust = 0.5, hjust=1)) +
    theme(legend.title = element_text(face = "bold", colour = "black", size = 12)) + theme(legend.text = element_text(colour = "black",
    size = 10, vjust = 0.5, hjust=1)) + theme(legend.text.align = 0)
  }

  if (Palette == "Blue") {
    MutSigStack <<- ggplot(data = Data, aes(x = Hugo_Symbol, y = Compartment)) + geom_tile(aes(fill = Odds_Ratio)) +
      labs(title = "Mutational Signatures by Lymphocyte Compartment", subtitle = "", y = "Lymphocyte Compartment", x = "Gene", tag = "") +
      scale_fill_manual(values = c("deepskyblue", "dodgerblue4", "white"), name = "Odds_Ratio", limits = c("0.5", "1", "0"),
                        labels = c("OR < 0.5", "OR > 2", "")) + theme(panel.background = element_rect(fill = "white", colour = "white"),
                                                                      axis.line = element_line(size = 0.5, linetype = "solid", colour = "white")) +
      theme(plot.title = element_text(face = "bold", colour = "black", size = 15, hjust = 0.5)) + theme(axis.title.x = element_text(face = "bold",
                                                                                                                                    colour = "black", size = 12)) + theme(axis.text.x = element_text(colour = "black", size = 10, angle = 90,
                                                                                                                                                                                                     vjust = 0.5, hjust=1)) + theme(axis.title.y = element_text(face = "bold", colour = "black", size = 12, angle = 90,
                                                                                                                                                                                                                                                                vjust = 0.5, hjust = 0.5)) + theme(axis.text.y = element_text(colour = "black", size = 10, vjust = 0.5, hjust=1)) +
      theme(legend.title = element_text(face = "bold", colour = "black", size = 12)) + theme(legend.text = element_text(colour = "black",
                                                                                                                        size = 10, vjust = 0.5, hjust=1)) + theme(legend.text.align = 0)
  }

  if (Palette == "Red") {
    MutSigStack <<- ggplot(data = Data, aes(x = Hugo_Symbol, y = Compartment)) + geom_tile(aes(fill = Odds_Ratio)) +
      labs(title = "Mutational Signatures by Lymphocyte Compartment", subtitle = "", y = "Lymphocyte Compartment", x = "Gene", tag = "") +
      scale_fill_manual(values = c("firebrick1", "firebrick4", "white"), name = "Odds_Ratio", limits = c("0.5", "1", "0"),
                        labels = c("OR < 0.5", "OR > 2", "")) + theme(panel.background = element_rect(fill = "white", colour = "white"),
                                                                      axis.line = element_line(size = 0.5, linetype = "solid", colour = "white")) +
      theme(plot.title = element_text(face = "bold", colour = "black", size = 15, hjust = 0.5)) + theme(axis.title.x = element_text(face = "bold",
                                                                                                                                    colour = "black", size = 12)) + theme(axis.text.x = element_text(colour = "black", size = 10, angle = 90,
                                                                                                                                                                                                     vjust = 0.5, hjust=1)) + theme(axis.title.y = element_text(face = "bold", colour = "black", size = 12, angle = 90,
                                                                                                                                                                                                                                                                vjust = 0.5, hjust = 0.5)) + theme(axis.text.y = element_text(colour = "black", size = 10, vjust = 0.5, hjust=1)) +
      theme(legend.title = element_text(face = "bold", colour = "black", size = 12)) + theme(legend.text = element_text(colour = "black",
                                                                                                                        size = 10, vjust = 0.5, hjust=1)) + theme(legend.text.align = 0)
  }

  if (Palette == "Green") {
    MutSigStack <<- ggplot(data = Data, aes(x = Hugo_Symbol, y = Compartment)) + geom_tile(aes(fill = Odds_Ratio)) +
      labs(title = "Mutational Signatures by Lymphocyte Compartment", subtitle = "", y = "Lymphocyte Compartment", x = "Gene", tag = "") +
      scale_fill_manual(values = c("darkolivegreen1", "darkgreen", "white"), name = "Odds_Ratio", limits = c("0.5", "1", "0"),
                        labels = c("OR < 0.5", "OR > 2", "")) + theme(panel.background = element_rect(fill = "white", colour = "white"),
                                                                      axis.line = element_line(size = 0.5, linetype = "solid", colour = "white")) +
      theme(plot.title = element_text(face = "bold", colour = "black", size = 15, hjust = 0.5)) + theme(axis.title.x = element_text(face = "bold",
                                                                                                                                    colour = "black", size = 12)) + theme(axis.text.x = element_text(colour = "black", size = 10, angle = 90,
                                                                                                                                                                                                     vjust = 0.5, hjust=1)) + theme(axis.title.y = element_text(face = "bold", colour = "black", size = 12, angle = 90,
                                                                                                                                                                                                                                                                vjust = 0.5, hjust = 0.5)) + theme(axis.text.y = element_text(colour = "black", size = 10, vjust = 0.5, hjust=1)) +
      theme(legend.title = element_text(face = "bold", colour = "black", size = 12)) + theme(legend.text = element_text(colour = "black",
                                                                                                                        size = 10, vjust = 0.5, hjust=1)) + theme(legend.text.align = 0)
  }

  if (Palette == "Pink") {
    MutSigStack <<- ggplot(data = Data, aes(x = Hugo_Symbol, y = Compartment)) + geom_tile(aes(fill = Odds_Ratio)) +
      labs(title = "Mutational Signatures by Lymphocyte Compartment", subtitle = "", y = "Lymphocyte Compartment", x = "Gene", tag = "") +
      scale_fill_manual(values = c("deeppink", "deeppink4", "white"), name = "Odds_Ratio", limits = c("0.5", "1", "0"),
                        labels = c("OR < 0.5", "OR > 2", "")) + theme(panel.background = element_rect(fill = "white", colour = "white"),
                                                                      axis.line = element_line(size = 0.5, linetype = "solid", colour = "white")) +
      theme(plot.title = element_text(face = "bold", colour = "black", size = 15, hjust = 0.5)) + theme(axis.title.x = element_text(face = "bold",
                                                                                                                                    colour = "black", size = 12)) + theme(axis.text.x = element_text(colour = "black", size = 10, angle = 90,
                                                                                                                                                                                                     vjust = 0.5, hjust=1)) + theme(axis.title.y = element_text(face = "bold", colour = "black", size = 12, angle = 90,
                                                                                                                                                                                                                                                                vjust = 0.5, hjust = 0.5)) + theme(axis.text.y = element_text(colour = "black", size = 10, vjust = 0.5, hjust=1)) +
      theme(legend.title = element_text(face = "bold", colour = "black", size = 12)) + theme(legend.text = element_text(colour = "black",
                                                                                                                        size = 10, vjust = 0.5, hjust=1)) + theme(legend.text.align = 0)
  }



  #Printing of plot
  print(MutSigStack)
  tiff("MutSigStack.tiff")
  print(MutSigStack)
  dev.off()





  #Returning objects to global environment
  DataFrames <<- DataFrames
  Merge1 <<- Merge1
  Merge2 <<- Merge2
  Data <<- Data

  message("Wonder Woman hopes that you enjoy your stacked mutational signatures. Blessings of Gaea be with you :)")


}





