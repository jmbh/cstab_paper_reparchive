# dirk.wulff@gmail.com, July 2017;

# -----------------------------------------------------------------------------------------------------------------------
# -------------------------- Code to reproduce Figure 1 in the paper ----------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------

if(!require(cstab)) install.packages('cstab')
if(!require(gtools)) install.packages('gtools')
require(gtools)
require(cstab)

# ----- settings

# define allocation function producing M
allct = function(n,g){
  ps = c(rdirichlet(1,rep(1,g)))
  ns = c(rmultinom(1,100,ps))
  return(ns)
}

# simulation settings
nrep = 10
kseq = 2:100

# ----- simulation

# create result container 
res = matrix(NA,nrow=nrep,ncol=length(kseq))

# iterate over k sequence
for(i in 1:length(kseq)){
  
  # iterate over number of repetitions
  for(j in 1:nrep){
    
    # draw two random and independent allocations
    a = allct(100,kseq[i])
    b = allct(100,kseq[i])
    
    # compute instability due to change using cstab
    res[j,i] = cstab:::instabLookup(a,b)
  }
}


# ----- plot


pdf('Figure_2_InstabilityPath.pdf', width = 8, height = 6)

# setup margins
par(mar=c(4,4,1,1))

# settup plot
plot(colMeans(res),ylim=c(0,1),ylab='Instability due to chance',xlab='Number of clusters k',type='n',las=1)

# draw background rectangles
sapply(1:ncol(res),function(x) points(rep(x,nrow(res)),res[,x],col=rgb(0,0,0,alpha=.015),pch=15))

# draw aggregate line
lines(colMeans(res),lwd=3,lty=1,col='red')

# draw legend
legend('topright',pch=c(15,NA),lwd=c(NA,3),col=c(rgb(0,0,0,alpha=1),'red'),legend=c('Simulation','Average'),pt.cex = 2)

dev.off()


