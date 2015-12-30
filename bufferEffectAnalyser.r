### 0. preminaries

# 0.1. loading libraries
library(lme4)
library(ggplot2)
library(reshape2)

# 0.3 defining functions
model.calculator <- function (fitness.data,working.species,label){        
  
  # f.1. defining the species-specific data for the model 
  species.data <- fitness.data[fitness.data$Species == working.species,]
  if (label == "age") {
  
  } 
  
  # f.2. defining productivity
  if (label == "patch") {
    species.data["BroodPatchBinary"] <- 0  # Creates a new column full of 0
    species.data$BroodPatchBinary[species.data$BroodPatch <= 6 & species.data$BroodPatch > 0] <- 1  
    productivity <- species.data$BroodPatchBinary
  } 
  else if (label == "age") {
  pjuv <- pjuv[pjuv$Agecorrectedcode != 0,]
pjuv$Binary.Age <- pjuv$Agecorrectedcode
pjuv[pjuv$Binary.Age == 3, "Binary.Age"] <- 1
pjuv[pjuv$Binary.Age == 4, "Binary.Age"] <- 0
    productivity <- species.data$Agecorrectedcode[species.data$Agecorrectedcode != 0]
  } else
    stop("ERROR. about defining the productivity.") 
  
  # f.3. converting to log scale the number of birds, also centering
  species.data$logNtotc <- log(species.data$Ntot+1) - mean(log(species.data$Ntot+1)) 
  numberOfBirds <- species.data$logNtotc
  
  # f.4. defining the stations code factor
  stationsFactor <- species.data$CES_CODE
  
  # f.4. defining the variables to fill up
  estimates <- c()
  estimate.errors <- c()
  
  # f.5. iterating over the buffer values
  bufferNames <- colnames(species.data)[9:14]
  for (indexBuffer in  1:length(bufferNames)){  
    bufferTag <- bufferNames[indexBuffer]
    print(bufferTag)
    
    # f.5.1. centering the distribution of habitat
    raw.habitat <- species.data[bufferTag][,1] 
    habitat <- raw.habitat - mean(raw.habitat)
      
    #print(summary(productivity))
    #print(summary(habitat))
    #print(summary(numberOfBirds))
    #print(summary(stationsFactor))
    
    # f.5.2. building the model
    mod <- glmer(productivity ~ habitat * numberOfBirds + (1 | stationsFactor),family=binomial)

    #print(summary(mod))
   
    # f.5.3. extracting the estimates and its error and appending the species-specific values into the corresponding list 
    the.mean <- as.data.frame(summary(mod)$coefficients)$Estimate[2]
    the.se <- as.data.frame(summary(mod)$coefficients)$Std.[2]
    estimates <- c(estimates,c(the.mean))
    estimate.errors <- c(estimate.errors,c(the.se))
    print(the.mean)
    print(the.se)
    }
     
  # f.6. final returning estimates for each species                       
  return(data.frame(estimates,estimate.errors))
  } 

###### MAIN

### 1. reading the data
pfem <- read.csv("NewModels-Results/P3-HS_thre_AUC.csv")
pjuv <- read.csv("NewModels-Results/P4-HS_thre_AUC.csv")
species.list <- levels(pfem$Species)

### for each species
#for(i in 1:length(species.list)) 
for (i in 1:1){
  working.species <- species.list[i]
  print(working.species)
  
  ### 2. computing model
  print("computing model for pfem...")
  model.pfem <- model.calculator(fitness.data=pfem,working.species,label="patch")
  print("computing model for juv...")
  model.juv <- model.calculator(fitness.data=pjuv,working.species,label="age")
  
  ### 3. making plot for each species
  plot.file.name <- paste("figure_",working.species,".pdf",sep="")
  pdf(plot.file.name)
  plot(x=,length(estimates):1, pch = 20, ylim = c(0, length(estimates)+1),xlab=thexlab,ylab=theylab,xlim=xlim,yaxt="n")
  #  print(figureFileName)
  #  pdf(figureFileName)
  dev.off()
  
  }             
