### this script computes the estimate effect of habitat and productivity for each species

                                        # 0.1. user defined paths
setwd <- getwd()
pfem <- read.csv("data/P3-HS_thre_AUC.csv")
juv <- read.csv("data/P4-HS_thre_AUC.csv")
                                        # 0.2. loading common functions
source("commonFunctions.r")

                                        # MAIN
                                        # 1. defining some initial variables
species.list <- levels(pfem$Species)
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
    model.juv <- model.calculator(fitness.data=juv,working.species,label="age")
    print(model.juv)
                                        # 3. appending the computed models into a list
    species.pfem.models <- rbind( species.pfem.models, data.frame(working.species,model.pfem) )
    species.juv.models <- rbind( species.juv.models, data.frame(working.species,model.juv) )
}
                                        # 4. building a figure for each buffer size
bufferNames <- colnames(pfem)[9:14]
for (indexBuffer in  1:length(bufferNames)){
    bufferTag <- bufferNames[indexBuffer]
                                        # 4.1. defining the pfem plot
    figureFileName <- paste("figures/estimates_pfem_",bufferTag,".pdf",sep="")
    pdf(figureFileName)
    estimates <- c()
    estimate.errors <- c()
    plotLabels <- c()
    for(i in 1:length(species.list)){
        estimate <- species.pfem.models[species.pfem.models$working.species == species.list[i],]$estimates[indexBuffer]
        estimates <- c(estimates,c(estimate))
        estimate.error <- species.pfem.models[species.pfem.models$working.species == species.list[i],]$estimate.errors[indexBuffer]
        estimate.errors <- c(estimate.errors,c(estimate.error))
        plotLabels <- c(plotLabels,c(species.list[i]))
    }
    estimates <- estimates[order(estimates, decreasing=TRUE)]
    estimate.errors <- estimate.errors[order(estimates, decreasing=TRUE)]
    plotLabels <- plotLabels[order(estimates, decreasing=TRUE)]
    xlim = c(min(estimates - 2*estimate.errors), max(estimates + 2*estimate.errors))
    thexlab = bufferTag
    theylab = ""
    plot(x=estimates,length(estimates):1, pch = 20, ylim = c(0, length(estimates)+1),xlab=thexlab,ylab=theylab,xlim=xlim,yaxt="n",main="pfem")
    abline(v=0, col = "grey50")
    segments( estimates-estimate.errors, length(estimates):1, estimates+estimate.errors, length(estimates):1, lwd = 2)
    segments( estimates-2*estimate.errors, length(estimates):1, estimates+2*estimate.errors, length(estimates):1, lwd = 1)
    axis(side = 2, at = length(estimates):1, labels = plotLabels, tick = FALSE, las = 1)
    dev.off()
                                        # 4.1. defining the juv plot
    figureFileName <- paste("figures/estimates_juv_",bufferTag,".pdf",sep="")
    pdf(figureFileName)
    estimates <- c()
    estimate.errors <- c()
    plotLabels <- c()
    for(i in 1:length(species.list)){
        estimate <- species.juv.models[species.juv.models$working.species == species.list[i],]$estimates[indexBuffer]
        estimates <- c(estimates,c(estimate))
        estimate.error <- species.juv.models[species.juv.models$working.species == species.list[i],]$estimate.errors[indexBuffer]
        estimate.errors <- c(estimate.errors,c(estimate.error))
        plotLabels <- c(plotLabels,c(species.list[i]))
    }
    estimates <- estimates[order(estimates, decreasing=TRUE)]
    estimate.errors <- estimate.errors[order(estimates, decreasing=TRUE)]
    plotLabels <- plotLabels[order(estimates, decreasing=TRUE)]
    xlim = c(min(estimates - 2*estimate.errors), max(estimates + 2*estimate.errors))
    thexlab = bufferTag
    theylab = ""
    plot(x=estimates,length(estimates):1, pch = 20, ylim = c(0, length(estimates)+1),xlab=thexlab,ylab=theylab,xlim=xlim,yaxt="n",main="juv")
    abline(v=0, col = "grey50")
    segments( estimates-estimate.errors, length(estimates):1, estimates+estimate.errors, length(estimates):1, lwd = 2)
    segments( estimates-2*estimate.errors, length(estimates):1, estimates+2*estimate.errors, length(estimates):1, lwd = 1)
    axis(side = 2, at = length(estimates):1, labels = plotLabels, tick = FALSE, las = 1)
    dev.off()
}     
