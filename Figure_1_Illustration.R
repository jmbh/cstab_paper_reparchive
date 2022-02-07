# jonashaslbeck@gmail.com, April 2020

# -----------------------------------------------------------------------------------------------------------------------
# -------------------------- Code to reproduce Figure 1 in the paper ----------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------

# setwd(...) # !!! Set Working Directory to Reproducability Archive !!!

# library(devtools)
# install_github("jmbh/cstab")

codeDir <- ""

# ---------- Load Packages ----------

library(MASS)
library(RColorBrewer)
library(fastcluster)

library(cstab)

# ---------- Generate Data ----------

n <- 50
s <- 1.1
set.seed(1)
data1 <- cbind(rnorm(n, -1, s), rnorm(n, -2, s))
data2 <- cbind(rnorm(n, -1, s), rnorm(n, 2, s))
data3 <- cbind(rnorm(n, 2, s), rnorm(n, 0, s))
x <- data <- rbind(data1, data2, data3)
nrow(unique(x))

# ---------- K Selection with/without re-normalization ----------

kseq <- 2:50
nB <- 100

set.seed(1)
cls_MF <- cStability(data = x, 
                     kseq = kseq, 
                     nB = nB, 
                     norm = TRUE, 
                     predict = FALSE, 
                     pbar = T)

set.seed(1)
cls_MB <- cStability(data = x, 
                     kseq = kseq, 
                     nB = nB, 
                     norm = TRUE, 
                     predict = TRUE, 
                     pbar = T)



# ---------- Figure 1: Reproduce Figure 2 in F+W ----------

bp <- brewer.pal(3, 'Set1')

# Scatter plot
sc <- .8
pdf(paste0(codeDir, 'Fig1_FangWangRepo.pdf'), width = 12*sc, height = 6*sc)
par(mfrow=c(1,2))

plot.new()
par(mar=c(4,3,1,1))
plot.window(xlim=c(-5,5), ylim=c(-5,5))
box()
points(data1, cex=.8, col=bp[2], pch = 1)
points(data2, cex=.8, col=bp[3], pch = 2)
points(data3, cex=.8, col=bp[1], pch = 3)

axis(side=1, at = seq(-5,5,length=5))
axis(side=2, at = seq(-5,5,length=5), las=2)


# Instabilities

plot.new()
par(mar=c(4,5,1,1))
plot.window(xlim=c(2,50), ylim=c(-1.5,1))
box()

## CIs:
# model free
# polygon(c(2:50,50:2),c(cls_MF$instab_path_CI[1,],rev(cls_MF$instab_path_CI[2,])),col=rgb(0,0,0,alpha=.2),border=NA)
# polygon(c(2:50,50:2),c(cls_MF$instab_path_norm_CI[1,],rev(cls_MF$instab_path_norm_CI[2,])),col=rgb(0,0,0,alpha=.2),border=NA)
# model based
# polygon(c(2:50,50:2),c(cls_MB$instab_path_CI[1,],rev(cls_MB$instab_path_CI[2,])),col=rgb(0.8941176,0.1019608,0.1098039,alpha=.2),border=NA)
# polygon(c(2:50,50:2),c(cls_MB$instab_path_norm_CI[1,],rev(cls_MB$instab_path_norm_CI[2,])),col=rgb(0.8941176,0.1019608,0.1098039,alpha=.2),border=NA)

## Means:
# model free
lines(2:50,cls_MF$instab_path,lwd=2)
lines(2:50,cls_MF$instab_path_norm, lty=2,lwd=2)
# model based
lines(2:50,cls_MB$instab_path, col=bp[1],lwd=2)
lines(2:50,cls_MB$instab_path_norm, lty=2, col=bp[1],lwd=2)
# local minimum
abline(h=min(cls_MB$instab_path[1:6]), lty=3, col=bp[1])
abline(h=min(cls_MF$instab_path[1:6]), lty=3, col='black')

axis(side=1, at = round(seq(3,50, length=5)))
axis(side=2, at = seq(-1.5,1,length=6), las=2)
#abline(v=10, lty=2, col='green')


legend("topright", c('Model-Based', 'Model-Based (Corrected)', 'Model-Free', 'Model-Free (Corrected)'), 
       lty=c(1,2,1,2), col=c(bp[1], bp[1], 'black', 'black'), cex=.9, bty="n",lwd=2)


title(xlab='k', ylab = 'Clustering Instability', line=3, cex.lab=1.5)


dev.off()


# When does the instability path cross the local minimum at k = 3? (reported in the paper)

cls_MF$instab_path < min(cls_MB$instab_path[1:6]) # 23
cls_MB$instab_path < min(cls_MF$instab_path[1:6]) # 25



