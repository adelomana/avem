library(lme4)
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
    pvalues <- c()
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
        the.pvalue <- as.data.frame(summary(mod)$coefficients)$Pr[2]
        estimates <- c(estimates,c(the.mean))
        estimate.errors <- c(estimate.errors,c(the.se))
        pvalues <- c(pvalues,c(the.pvalue))
    }
                                        # f.7. final returning estimates for each species                       
    return(data.frame(estimates,estimate.errors,pvalues))
} 
