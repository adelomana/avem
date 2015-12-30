### this script computes for each species the effect of buffer size
                                        # 0.1. user defined paths
setwd <- getwd()
pfem <- read.csv("data/P3-HS_thre_AUC.csv")
pjuv <- read.csv("data/P4-HS_thre_AUC.csv")
                                        # 0.2. loading libraries
library(lme4)
library(ggplot2)
                                        # 0.3 defining functions
model.calculator <- function (fitness.data,working.species,label){
                                        # f.1. defining the species-specific data for the model 
    species.data <- fitness.data[fitness.data$Species == working.species,]
    if (label == "age") {
        species.data <- species.data[species.data$Agecorrectedcode != 0,]
    } 
                                        # f.2. defining productivity
    if (label == "patch") {
        species.data["BroodPatchBinary"] <- 0  # Creates a new column full of 0
        species.data$BroodPatchBinary[species.data$BroodPatch <= 6 & species.data$BroodPatch > 0] <- 1  
        productivity <- species.data$BroodPatchBinary
    } 
    else if (label == "age") {
        productivity <- species.data$Agecorrectedcode
        productivity[productivity == 3] <- 1
        productivity[productivity == 4] <- 0        
    } else
        stop("ERROR while defining productivity") 
                                        # f.3. converting to log scale the number of birds, also centering
    species.data$logNtotc <- log(species.data$Ntot+1) - mean(log(species.data$Ntot+1)) 
    numberOfBirds <- species.data$logNtotc
                                        # f.4. defining the stations code factor
    stationsFactor <- species.data$CES_CODE
                                        # f.5. defining the variables to fill up
    estimates <- c()
    estimate.errors <- c()
                                        # f.6. iterating over the buffer values
    bufferNames <- colnames(species.data)[9:14]
    for (indexBuffer in  1:length(bufferNames)){  
        bufferTag <- bufferNames[indexBuffer]
                                        # f.6.1. centering the distribution of habitat
        raw.habitat <- species.data[bufferTag][,1] 
        habitat <- raw.habitat - mean(raw.habitat)
                                        # f.6.2. building the model
        mod <- glmer(productivity ~ habitat * numberOfBirds + (1 | stationsFactor),family=binomial)   
                                        # f.6.3. extracting the estimates and its error and appending the species-specific values into the corresponding list 
        the.mean <- as.data.frame(summary(mod)$coefficients)$Estimate[2]
        the.se <- as.data.frame(summary(mod)$coefficients)$Std.[2]
        estimates <- c(estimates,c(the.mean))
        estimate.errors <- c(estimate.errors,c(the.se))
    }
                                        # f.7. final returning estimates for each species                       
    return(data.frame(estimates,estimate.errors))
} 

                                        # MAIN
                                        # 1. defining the species
species.list <- levels(pfem$Species)
                                        # for each species
for(i in 1:length(species.list)){
#for (i in 1:1){
    working.species <- species.list[i]
    print(working.species)
                                        # 2. computing model
    print("computing model for pfem...")
    model.pfem <- model.calculator(fitness.data=pfem,working.species,label="patch")
    print(model.pfem)
    print("computing model for juv...")
    model.juv <- model.calculator(fitness.data=pjuv,working.species,label="age")
    print(model.juv)
                                        # 3. making plot for each species
    plot.file.name <- paste("figures/be_",working.species,".pdf",sep="")
    pdf(plot.file.name)
    shift <- 0.2
                                        # 3.1. defining the plotting variables
    x <- c(1,2,4,8,12,16)

    y.pfem <- model.pfem$estimates
    z.pfem <- model.pfem$estimate.errors
    y.juv <- model.juv$estimates
    z.juv <- model.juv$estimate.errors
    
    top.pfem <- max(y.pfem+2*z.pfem)
    bottom.pfem <- min(y.pfem-2*z.pfem)

    top.juv <- max(y.juv+2*z.juv)
    bottom.juv <- min(y.juv-2*z.juv)

    the.ylim <- c(min(c(bottom.pfem,bottom.juv)),max(c(top.pfem,top.juv)))
    
                                        # 3.2. actual plotting
    plot(x-shift,y.pfem,pch=20,xlim=c(0,max(x)+2),ylim=the.ylim,xlab="buffer size",ylab="estimate",col="blue")
    segments(x-shift,y.pfem-z.pfem,x-shift,y.pfem+z.pfem,lwd=2,col="blue")
    segments(x-shift,y.pfem-2*z.pfem,x-shift,y.pfem+2*z.pfem,lwd=1,col="blue")
    par(new=TRUE)
    plot(x+shift,y.juv,pch=20,col="red",xlim=c(0,max(x)+2),ylim=the.ylim,xlab="",ylab="")
    segments(x+shift,y.juv-z.juv,x+shift,y.juv+z.juv,lwd=2,col="red")
    segments(x+shift,y.juv-2*z.juv,x+shift,y.juv+2*z.juv,lwd=1,col="red")
    
    dev.off()
  
}             
