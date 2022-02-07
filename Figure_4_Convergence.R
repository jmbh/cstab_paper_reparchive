# jonashaslbeck@gmail.com, April 2020

# -----------------------------------------------------------------------------------------------------------------------
# -------------------------- Code to reproduce Figure 4 in the paper ----------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------

# setwd(...) # !!! Set Working Directory to Reproducability Archive !!!

# library(devtools)
# install_github("jmbh/cstab")

# --------- Generate Data ----------

n <- 50
s <- 1
set.seed(1)
data1 <- cbind(rnorm(n, -1, s), rnorm(n, -2, s))
data2 <- cbind(rnorm(n, -1, s), rnorm(n, 2, s))
data3 <- cbind(rnorm(n, 2, s), rnorm(n, 0, s))
x <- data <- rbind(data1, data2, data3)


# --------- Compute Instability Paths for both approaches ----------

library(cstab)

kseq <- 3

set.seed(1)
cls_k3_MF5000 <- cStability(data = x, 
                            kseq = kseq, 
                            nB = 5000, 
                            norm = TRUE, 
                            predict = FALSE, 
                            pbar = T)

set.seed(2)
cls_k3_MB5000 <- cStability(data = x, 
                            kseq = kseq, 
                            nB = 5000, 
                            norm = TRUE, 
                            predict = TRUE, 
                            pbar = T)


# --------- Plotting ----------

x1 <- cls_k3_MF5000$instab_path_nrom_matrix[, 1]
x2 <- cls_k3_MB5000$instab_path_nrom_matrix[, 1]

sc <- .7
pdf(paste0('Fig4_Convergence.pdf'), width = 12*sc, height = 6*sc)

x1_av <- cumsum(x1) / seq_along(x1)
x2_av <- cumsum(x2) / seq_along(x2)
Diff <- x1_av - x2_av

par(mfrow=c(1,2))
par(mar=c(4,4.5,2,1))

# plot1: stability for k=3 as a function of bootstrap samples
plot.new()
plot.window(xlim=c(1,5000), ylim=c(-1, -.8))
box()
lines(x1_av, type='l')
lines(x2_av, type='l', col='red')
legend("topright", c('Model-free (C)', 'Model-based (C)'), col=c('black', 'red'), lwd=c(1,1), cex=1, bty="n")
axis(1, c(1,2500,5000), cex.axis=.8)
axis(2, round(seq(-1, -.8, length=9), 3), las=2, cex.axis=.8)
title(xlab='Bootstrap sample pairs B', line=2.5)
title(ylab='Instability', line=3.5)

# plot2: absolute difference between those functions for MB/MF
plot.new()
plot.window(xlim=c(1,5000), ylim=c(-.03, .03))
box()
lines(Diff, lty=2)
abline(h=0, col='grey')
axis(1, c(1,2500,5000), cex.axis=.8)
axis(2, round(seq(-.03, .03, length=7), 3), las=2, cex.axis=.8)
title(xlab='Bootstrap sample pairs B', line=2.5)
title(ylab='Difference', line=3.5)


dev.off()





