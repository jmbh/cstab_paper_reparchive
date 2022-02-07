# jonashaslbeck@gmail.com, July 2017;

# -----------------------------------------------------------------------------------------------------------------------
# -------------------------- Preprocessing of Simulation Output ---------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------

# setwd(...) # !!! Set Working Directory to Reproducability Archive !!!


# --------- Load Simulation Output ----------

simResDir <- paste0('output/')

v_files <- list.files(simResDir)
n_files <- length(v_files)
l_files <- list()
for(i in 1:n_files) l_files[[i]] <- readRDS(paste0(simResDir, v_files[i]))


# --------- Function to extract paths of different methods ----------

f_getpath <- function(l_files, # data list
                      sim, # simulation 1:12
                      m) # instability method 1:3
{
  
  l_d <- lapply(l_files, function(x) {
    xd <- x[[sim]]
    if(m==1) out <- xd$instab_path_FW
    if(m==2) out <- xd$instab_path_FWn
    if(m==3) out <- xd$instab_path_WE
    if(m==4) out <- xd$instab_path_WEn
    if(m==5) out <- xd$WCD_data
    
    return(out)
  })
  
  m_out <- do.call(rbind, l_d)
  
  return(m_out)
  
}


# --------- Function to Compute JUMP Statistic ----------

ComputeJump <- function(x, dims=2) {
  xT <- x^(-dims/2)
  jump <- (xT - c(0, xT[-length(xT)]))[-1]
  return(jump)
}


# --------- Extract paths & Plotting ----------

library(scales)

sc <- .7
pdf(paste0('Fig5_JumpPath.pdf'), width = 12*sc, height = 6*sc)

par(mfrow=c(1,2))
par(mar=c(4,4,2,1))

# for k = 3
m_out <- f_getpath(l_files, sim=1, m=5)
m_jump <- t(apply(m_out, 1, ComputeJump))
plot.new()
plot.window(xlim=c(2,50), ylim=c(-.02,.02))
box()
for(i in 1:(n_files-1)) lines(2:50,m_jump[i,1:49], col=alpha('black', 0.2) )
abline(v=3, col='red')
axis(1, c(3,25,50))
axis(2, seq(-.02,0.02, length=5), las=2)
title(ylab="Jump", line = 3)
title(xlab = "Number of clusters k", line = 2.5)
title(main='Three true clusters', cex.main=.9)

# for k = 7
m_out <- f_getpath(l_files, sim=3, m=5)
m_jump <- t(apply(m_out, 1, ComputeJump))
plot.new()
plot.window(xlim=c(2,50), ylim=c(-.02,.02))
box()
for(i in 1:(n_files-1)) lines(2:50,m_jump[i,], col=alpha('black', 0.2) )
abline(v=7, col='red')
axis(1, c(7,25,50))
axis(2, seq(-.02,0.02, length=5), las=2)
title(ylab="Jump", line = 3)
title(xlab = "Number of clusters k", line = 2.5)
title(main='Seven true clusters', cex.main=.9)

dev.off()


