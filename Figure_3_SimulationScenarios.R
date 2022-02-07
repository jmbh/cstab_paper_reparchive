# jonashaslbeck@gmail.com, July 2017;

# -----------------------------------------------------------------------------------------------------------------------
# -------------------------- Code to reproduce Figure 3 in the paper ----------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------

# setwd(...) # !!! Set Working Directory to Reproducability Archive !!!


# -------- Load package / source Helper Functions --------

source(paste0('Helper_Functions.R'))
library(MASS)
library(RColorBrewer)


# -------- Define Parameters for Data Generation --------

l_radius <- rep(1, 4)
l_k  <- c(3,3,7,7)
vars <- list(c(.15, .15), # k = 3
             c(.15, .15),
             c(.04, .04), # k = 7
             c(.04, .04))
dims <- rep(c(2,10), 2)

N <- 50
iter <- 1


# -------- Generate Data --------

d <- 1
data_k3 <- f_circular(seed=iter, n=N, radius=l_radius[d], k=l_k[[d]], vars=vars[[d]], dims=dims[[d]])
d <- 3
data_k7 <- f_circular(seed=iter, n=N, radius=l_radius[d], k=l_k[[d]], vars=vars[[d]], dims=dims[[d]])

data_elo_k3 <- f_elongated(seed = iter,
                           n = N,
                           k = 3,
                           dims = 3,
                           sd = .1,
                           dist = 15)

data_elo_k7 <- f_elongated(seed = iter,
                           n = N,
                           k = 7,
                           dims = 3,
                           sd = .1,
                           dist = 15)


# -------- Plotting --------

# Define Colors
bp3 <- brewer.pal(3, 'Set1')
bp7 <- brewer.pal(7, 'Set1')


width <- 8
pdf(paste0('Fig2_SimulationSetup.pdf'), width = width, height = width/3.6)
par(mfrow=c(1,4))
par(mar=c(1,1,2.5,1))

# circular, k = 3
plot.new()
plot.window(xlim=c(-2,2), ylim=c(-2,2))
# box()
seqq <- c(1, 51, 101)
for(i in 1:3) points(data_k3[seqq[i]:(seqq[i]+49),], col=bp3[i], pch=i)
# axis(1, seq(-2,2,length=5))
# axis(2, seq(-2,2,length=5), las=2)
mtext(text = "3 cyclic clusters",side = 3)
# title('3 cyclic clusters')

# circular, k = 7
plot.new()
plot.window(xlim=c(-2,2), ylim=c(-2,2))
# box()
seqq <- c(1, 51, 101, 151, 201, 251, 301)
for(i in 1:7) points(data_k7[seqq[i]:(seqq[i]+49),], col=bp7[i], pch=i)
# axis(1, seq(-2,2,length=5))
# axis(2, seq(-2,2,length=5), las=2)
# title('7 cyclic clusters')
mtext(text = "7 cyclic clusters",side = 3)

# elongated, k = 3
plot.new()
plot.window(xlim=c(-10,40), ylim=c(-10,40))
# box()
bp <- brewer.pal(3, 'Set1')
seqq <- c(1, 51, 101)
for(i in 1:3) points(data_elo_k3[, 1:2][seqq[i]:(seqq[i]+49),], col=bp3[i], pch=i)
# title('3 elongated clusters')
mtext(text = "3 elongated clusters",side = 3)

# elongated, k = 7
plot.new()
plot.window(xlim=c(-10,100), ylim=c(-10,100))
# box()
bp <- brewer.pal(3, 'Set1')
seqq <- c(1, 51, 101, 151, 201, 251, 301)
for(i in 1:7) points(data_elo_k7[seqq[i]:(seqq[i]+49),], col=bp7[i], pch=i)
# title('7 elongated clusters')
mtext(text = "7 elongated clusters",side = 3)

dev.off()


