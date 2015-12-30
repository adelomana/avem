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
