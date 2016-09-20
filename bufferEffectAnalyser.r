### this script computes for each species the effect of buffer size, stores it in a table and makes a figure. to be run as source("bufferEffectAnalyser.r")
                                        # 0.1. user defined paths
setwd("/Users/alomana/github/avem/")
pjuv <- read.csv("data/P4-HS_raw_AUC.csv")
outputFile='tables/formattedInfo.txt'
formattedData=c()
                                        # 0.2. loading common functions
source("commonFunctions.r")
                                        # MAIN
                                        # 1. defining the species
species.list <- levels(pjuv$Species)
                                        # for each species
for(i in 1:length(species.list)){
#for(i in 1:3){
    working.species <- species.list[i]
    print(working.species)
                                        # 2. computing model
    print("computing model for juv...")
    model.juv <- model.calculator(fitness.data=pjuv,working.species,label="age")
    print(model.juv)
                                        # 3. making plot for each species
    plot.file.name <- paste("figures/be_",working.species,".pdf",sep="")
    pdf(plot.file.name)
    
                                        # 3.1. defining the plotting variables
    x <- c(1,2,4,8,12,16)

    y.juv <- model.juv$estimates
    z.juv <- model.juv$estimate.errors

                                        # dealing with less buffers part 1
    subsetIndexes <- c(1,2,3,4,6) # forcing all buffers, but 12
    x <- x[subsetIndexes]
    y.juv <- y.juv[subsetIndexes]
    z.juv <- z.juv[subsetIndexes]
                                        # end of dealing with less buffers part 1
    top.juv <- max(y.juv+2*z.juv)
    bottom.juv <- min(y.juv-2*z.juv)

    the.ylim <- c(min(c(bottom.juv)),max(c(top.juv))+0.2*max(c(top.juv)))
    the.ylim <- c(-0.5,0.8) # forcing the ylim 
    the.xlim <- c(0,17) # this line sets the limits of the figure, the x axis

                                        # this block should be after ylim has been properly defined, otherwise the position of the stars will be misplaced
    w.juv <- 0
    w.juv[(model.juv$pvalues[subsetIndexes] < 0.05) & (y.juv > 0.)] <- the.ylim[2]-0.01*the.ylim[2]
    w.juv[(model.juv$pvalues[subsetIndexes] < 0.05) & (y.juv < 0.)] <- the.ylim[1]+0.01*the.ylim[1]
    w.juv[model.juv$pvalues[subsetIndexes] >= 0.05] <- 100    

                                        # 3.2. actual plotting
    plot(x,y.juv,pch=20,xlim=the.xlim,ylim=the.ylim,xlab="buffer size",ylab="estimate",main=working.species,col="black",panel.first=abline(h=0,col="gray60"),cex=2)
    segments(x,y.juv-z.juv,x,y.juv+z.juv,lwd=4,col="black")
    segments(x,y.juv-2*z.juv,x,y.juv+2*z.juv,lwd=1,col="black")

    points(x,w.juv,col="black",pch=8)

    dev.off()  
    
    # formatting data for writing table
    for(bufferIndex in 1:length(x)){
      data2Write=c(working.species,x[bufferIndex],y.juv[bufferIndex],z.juv[bufferIndex])
      formattedData <- rbind(formattedData,data2Write)
    }
}           
colnames(formattedData)=c('species','buffer','estimate','standard deviation')
write.table(formattedData,outputFile,sep='\t',quote=FALSE,row.names=FALSE)
