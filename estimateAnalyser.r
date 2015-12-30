### this script computes the estimate effect of habitat and productivity for each species

                                        # 0.1. user defined paths
setwd <- getwd()
pfem <- read.csv("data/P3-HS_thre_AUC.csv")
pjuv <- read.csv("data/P4-HS_thre_AUC.csv")
                                        # 0.2. loading libraries
library(lme4)
                                        # 0.3. reading the data

                                        # MAIN
                                        # 1. defining the species
species.list <- levels(pfem$Species) 

# 1.2. converting to log scale the number of birds
pfem$logNtotc <- log(pfem$Ntot+1) - mean(log(pfem$Ntot+1)) 

# 1.3. defining the species names
allSpecies <- levels(pfem$Species)

### 2. building graphs for each buffer size
bufferNames <- colnames(pfem)[9:14]
#bufferNames <- colnames(pfem)[9:9]
for (indexBuffer in  1:length(bufferNames)){
  bufferTag <- bufferNames[indexBuffer]
  
  # 2.1. centering the distribution of habitat
  distribution <- pfem[bufferTag][,1] 
  pfem$centredHS <- distribution - mean(distribution)
 
  ### 3. computing the models for each buffer
  
  ### 3.1. defining genaral variables
  plotLabels <- c()
  estimates <- c()
  estimateErrors <- c()
  figureFileName <- paste("estimates_",bufferTag,".pdf",sep="")
  print(figureFileName)
  pdf(figureFileName)
  
  # 3.2. computing the model for each species
  for (i in  1:length(allSpecies)){
    # 3.2.1. defining the species-specific data for the model 
    workingSpecies <- allSpecies[i]
    workingData <- pfem[pfem$Species == workingSpecies,]
    # 3.2.2. building the model
    mod <- glmer(Brood_patch ~ centredHS * logNtotc + (1 | CES_CODE), family = binomial, data = workingData)
    # 3. checking the model, oh yeah 
    drop1(mod,test="Chisq")
    # 4. extracting the estimates and its error and appending the species-specific values into the corresponding list 
    mean <- as.data.frame(summary(mod)$coefficients)$Estimate[2]
    se <- as.data.frame(summary(mod)$coefficients)$Std.[2]
    plotLabels <- c(plotLabels,c(workingSpecies))
    estimates <- c(estimates,c(mean))
    estimateErrors <- c(estimateErrors,c(se))
    #print(summary(mod))
    #print(mean)
    #print(se)
    #print("next model")
  }     
  
  # 3.3. sorting the species for the plot
  estimates <- estimates[order(estimates, decreasing=TRUE)]
  estimateErrors <- estimateErrors[order(estimates, decreasing=TRUE)]
  plotLabels <- plotLabels[order(estimates, decreasing=TRUE)]
  # 4. defining the plot for each buffer
  xlim = c(min(estimates - 2*estimateErrors), max(estimates + 2*estimateErrors))
  thexlab = bufferTag
  theylab = ""
  plot(x=estimates,length(estimates):1, pch = 20, ylim = c(0, length(estimates)+1),xlab=thexlab,ylab=theylab,xlim=xlim,yaxt="n")
  abline(v=0, col = "grey50")
  segments( estimates-estimateErrors, length(estimates):1, estimates+estimateErrors, length(estimates):1, lwd = 2)
  segments( estimates-2*estimateErrors, length(estimates):1, estimates+2*estimateErrors, length(estimates):1, lwd = 1)
  axis(side = 2, at = length(estimates):1, labels = plotLabels, tick = FALSE, las = 1)
  ### closing the graph
  dev.off()
}
