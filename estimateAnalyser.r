### this script computes the estimate effect of habitat and productivity for each species

modelAnalysis=function(bufferNames,indexBuffer,label) {
  
  bufferTag=bufferNames[indexBuffer]
  
  # f.1. defining the plot
  figureFileName=paste("figures/estimates.",label,".",bufferTag,".pdf",sep="")
  pdf(figureFileName)
  
  # f.2. defining the currently working model
  if (label == "juv") {
    workingModels=species.juv.models
  } else if (label == "pfem") {
    workingModels=species.pfem.models
  } else
    stop("fatal error: unknown model label.")
  
  # f.3. going over the species
  estimates=c()
  estimate.errors=c()
  plotLabels=c()
  w=c()
  for(i in 1:length(species.list)){
    estimate=workingModels[workingModels$working.species == species.list[i],]$estimates[indexBuffer]
    estimates=c(estimates,c(estimate))
    estimate.error=workingModels[workingModels$working.species == species.list[i],]$estimate.errors[indexBuffer]
    estimate.errors=c(estimate.errors,c(estimate.error))
    plotLabels=c(plotLabels,c(species.list[i]))
    pvalue=workingModels[workingModels$working.species == species.list[i],]$pvalues[indexBuffer]
    w=c(w,c(pvalue))
  }
  
  # f.4. saving the order of the first iteration for following iterations
  temporalFileName=paste('tempo.',label,'.RData',sep='')
  if (indexBuffer == 1) {
    orderedIndexes=order(estimates,decreasing=TRUE)
    save(orderedIndexes,file=temporalFileName)
  } else {
    load(temporalFileName)
  }
  
  estimates=estimates[orderedIndexes]
  estimate.errors=estimate.errors[orderedIndexes]
  plotLabels=plotLabels[orderedIndexes]
  w=w[orderedIndexes] 
  
  #the.xlim = c(min(estimates - 2*estimate.errors)-0.1*min(estimates - 2*estimate.errors), max(estimates + 2*estimate.errors)+0.1*max(estimates + 2*estimate.errors))
  the.xlim = c(-0.5,0.8) # forcing the x limit
  thexlab = bufferTag
  theylab = ""
  
  plot(x=estimates,length(estimates):1,pch=20,ylim=c(0,length(estimates)+1),xlab=thexlab,ylab=theylab,xlim=the.xlim,yaxt="n",main="juv",abline(h=c(1:length(estimates)),col="lightgray",lty="dotted"),cex=2)
  abline(v=0, col = "grey50")
  segments( estimates-estimate.errors, length(estimates):1, estimates+estimate.errors, length(estimates):1, lwd = 4)
  segments( estimates-2*estimate.errors, length(estimates):1, estimates+2*estimate.errors, length(estimates):1, lwd = 1)
  axis(side = 2, at = length(estimates):1, labels = plotLabels, tick = FALSE, las = 1)
  
  z=0
  z[(w < 0.05) & (estimates > 0.)]=the.xlim[2]-0.05*the.xlim[2]
  z[(w < 0.05) & (estimates < 0.)]=the.xlim[1]+0.05*the.xlim[1]
  z[w >= 0.05]=100
  
  points(z,length(estimates):1,pch=8)
  
  dev.off()
  
}

# 0.1. user defined paths
setwd("/Users/adriandelomana/git/avem/")
juv=read.csv("data/P4-HS_raw_AUC.csv")
pfem=read.csv("data/P3-HS_raw_AUC.csv")

# 0.2. loading common functions
source("commonFunctions.r")

# MAIN
# 1. defining some initial variables
species.list=levels(juv$Species)
species.juv.models=data.frame()
species.pfem.models=data.frame()

# 1.1. for each species 
for(i in 1:length(species.list)){
#!for(i in 1:1){
    working.species=species.list[i]
    print(working.species)
    
    # 2. computing model
    print("computing model for juv...")
    model.juv=model.calculator(fitness.data=juv,working.species,label="age")
    print(model.juv)
    print("computing model for pfem...")
    model.pfem=model.calculator(fitness.data=pfem,working.species,label="patch")
    print(model.pfem)
    
    # 3. appending the computed models into a list
    species.juv.models=rbind(species.juv.models,data.frame(working.species,model.juv))
    species.pfem.models=rbind(species.pfem.models,data.frame(working.species,model.pfem))
}
      
# 4. building a figure for each buffer size
bufferNames=colnames(juv)[c(9,10,11,12,13,14)]

for (indexBuffer in  1:length(bufferNames)){
    
    # 4.1. performing analysis for juv model
    modelAnalysis(bufferNames,indexBuffer,'juv')
    
    # 4.2. performing analysis for pfem model
    modelAnalysis(bufferNames,indexBuffer,'pfem')
    
}
