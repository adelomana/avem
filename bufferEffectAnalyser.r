### this script computes for each species the effect of buffer size
                                        # 0.1. user defined paths
setwd <- getwd()
pfem <- read.csv("data/P3-HS_thre_AUC.csv")
pjuv <- read.csv("data/P4-HS_thre_AUC.csv")
                                        # 0.2. loading common functions
source("commonFunctions.r")
                                        # MAIN
                                        # 1. defining the species
species.list <- levels(pfem$Species)
                                        # for each species
for(i in 1:length(species.list)){
#for(i in 1:1){
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

                                        # dealing with less buffers part 1
    subsetIndexes <- c(1,3,6)
    x <- x[subsetIndexes]
    y.pfem <- y.pfem[subsetIndexes]
    z.pfem <- z.pfem[subsetIndexes]
    y.juv <- y.juv[subsetIndexes]
    z.juv <- z.juv[subsetIndexes]
                                        # end of dealing with less buffers part 1
    
    top.pfem <- max(y.pfem+2*z.pfem)
    bottom.pfem <- min(y.pfem-2*z.pfem)

    top.juv <- max(y.juv+2*z.juv)
    bottom.juv <- min(y.juv-2*z.juv)

    the.ylim <- c(min(c(bottom.pfem,bottom.juv)),max(c(top.pfem,top.juv))+0.2*max(c(top.pfem,top.juv)))
    the.ylim <- c(-0.4,1.3) # forcing the ylim 
    the.xlim <- c(0,22)

                                        # this block should be after ylim has been properly defined, otherwise the position of the stars will be misplaced
    w.pfem <- 0; w.juv <- 0
    w.pfem[model.pfem$pvalues < 0.05] <- the.ylim[2]-0.025*the.ylim[2]
    w.pfem[model.pfem$pvalues >= 0.05] <- 100
    w.juv[model.juv$pvalues < 0.05] <- the.ylim[2]-0.025*the.ylim[2]
    w.juv[model.juv$pvalues >= 0.05] <- 100
    
                                        # dealing with less buffers part 2
    w.pfem <- w.pfem[subsetIndexes]
    w.juv <- w.juv[subsetIndexes]
                                        # dealing with less buffers part 2

                                        # 3.2. actual plotting
    plot(x-shift,y.pfem,pch=20,xlim=the.xlim,ylim=the.ylim,xlab="buffer size",ylab="estimate",main=working.species,col="blue")
    segments(x-shift,y.pfem-z.pfem,x-shift,y.pfem+z.pfem,lwd=2,col="blue")
    segments(x-shift,y.pfem-2*z.pfem,x-shift,y.pfem+2*z.pfem,lwd=1,col="blue")
    par(new=TRUE)
    plot(x+shift,y.juv,pch=20,col="red",xlim=the.xlim,ylim=the.ylim,xlab="",ylab="")
    segments(x+shift,y.juv-z.juv,x+shift,y.juv+z.juv,lwd=2,col="red")
    segments(x+shift,y.juv-2*z.juv,x+shift,y.juv+2*z.juv,lwd=1,col="red")

    legend(18,the.ylim[2],c("pfem","juv"),lty=c(1,1),lwd=c(2.5,2.5),pch=20,col=c("blue","red"))
    points(x-shift,w.pfem,col="blue",pch=8)
    points(x+shift,w.juv,col="red",pch=8)

    abline(h=0,col="gray60") # adding a horizontal line
    
    dev.off()
  
}             
