### this script computes the estimate effect of habitat and productivity for each species

                                        # 0.1. user defined paths
setwd <- getwd()
pfem <- read.csv("data/P3-HS_thre_AUC.csv")
pjuv <- read.csv("data/P4-HS_thre_AUC.csv")
                                        # 0.2. loading common functions
source("commonFunctions.r")

                                        # MAIN
                                        # 1. defining some initial variables
species.list <- levels(pfem$Species)[1:2]
species.pfem.models <- data.frame()
species.juv.models <- data.frame()
                                        # for each species
for(i in 1:length(species.list)){
    working.species <- species.list[i]
    print(working.species)
                                        # 2. computing model
    print("computing model for pfem...")
    model.pfem <- model.calculator(fitness.data=pfem,working.species,label="patch")
    print(model.pfem)
    print("computing model for juv...")
    model.juv <- model.calculator(fitness.data=pjuv,working.species,label="age")
    print(model.juv)
                                        # 3. appending the computed models into a list
    species.pfem.models <- rbind( species.pfem.models, data.frame(working.species,model.pfem) )
    species.juv.models <- rbind( species.juv.models, data.frame(working.species,model.juv) )
}
                                        # 4. building a figure for each buffer size
bufferNames <- colnames(pfem)[9:14]
bufferNames <- colnames(pfem)[9:10]
for (indexBuffer in  1:length(bufferNames)){
    bufferTag <- bufferNames[indexBuffer]
                                        # 4.1. defining plot general variables
    plotLabels <- c()
    estimates <- c()
    estimateErrors <- c()
    figureFileName <- paste("estimates_",bufferTag,".pdf",sep="")
    print(figureFileName)
    pdf(figureFileName)



    WORKING LINE
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


    

    dev.off()
