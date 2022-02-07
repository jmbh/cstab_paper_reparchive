
# -------------------------------------------------------------------- ------------------
# ---------------- Simulation: Comparing Performance between popular methods ------------
# ---------------------------------------------------------------------------------------

#!/usr/bin/env Rscript
iter <- commandArgs(trailingOnly=TRUE)
print(iter)
iter <- as.numeric(iter)

# ---------- 1) Load Packages & Helper Functions ----------

# repos="http://cran.fhcrc.org"

# if(!require(devtools)) install.packages('devtools', repos=repos)
# if(!require(MASS)) install.packages('MASS', repos=repos)
# if(!require(flexclust)) install.packages('flexclust', repos=repos)
# if(!require(cluster)) install.packages('cluster', repos=repos)
# if(!require(foreach)) install.packages('foreach', repos=repos)
# if(!require(parallel)) install.packages('parallel', repos=repos)
# if(!require(doParallel)) install.packages('doParallel', repos=repos)
# if(!require(mclust)) install.packages('mclust', repos=repos)
# if(!require(cstab)) install.packages('cstab', repos=repos)

library(devtools)
library(MASS) # generate multivariate normal data

library(cstab) # for cluster evaluation
library(flexclust) # for k-means and k-means prediction
library(cluster)
library(mclust)

library(foreach)
library(parallel)
library(doParallel)


#codeDir <- '/Users/jmb/Dropbox/MyData/_PhD/__projects/cl_stability_paper/3_code/'
#dataDir <- '/Users/jmb/Dropbox/MyData/_PhD/__projects/cl_stability_paper/2_data/'
#simDir <- '/Users/jmb/Dropbox/MyData/_PhD/__projects/cl_stability_paper/3_code/simulation_results/'

source(paste0('Helper_Functions.R'))

# ---------- 2) Call Simulation ----------

# Define lists for different data scenarios
l_radius <- rep(1, 4)
l_k  <- c(3,3,7,7)
vars <- list(c(.15, .15), # k = 3
             c(.15, .15),
             c(.04, .04), # k = 7
             c(.04, .04))
dims <- rep(c(2,10), 2)

# Tuning parameters
Bcomp <- 100 # number of bootstrap samples
kseq <- 2:50 # k-sequence
N <- 50 # total N
kmIter <- 10 # k-means restarts

# Register Workers
cluster <- 6
cl <- makeCluster(cluster, outfile="")
registerDoParallel(cl)

# tt <- proc.time()[3]

out <- foreach(d = 1:6, 
               .packages=c("cstab", "flexclust", "cluster", "Rcpp", "MASS", "mclust"),
               .export=c("f_kselect", "f_circular", "N", "iter", "l_radius", "f_elongated",
                         "l_k", "vars", "dims"), 
               .verbose=TRUE) %dopar% {

                
                 if(d %in% 1:4) {
                   data <- f_circular(seed = iter, 
                                      n = N, 
                                      radius = l_radius[d], 
                                      k = l_k[[d]], 
                                      vars = vars[[d]], 
                                      dims = dims[[d]])
                 } 

                 if(d == 5) data <- f_elongated(seed = iter,
                                        n = N,
                                        k = 3,
                                        dims = 3,
                                        sd = .1,
                                        dist = 15)

                 if(d == 6) data <- f_elongated(seed = iter,
                                                n = N,
                                                k = 7,
                                                dims = 3,
                                                sd = .1,
                                                dist = 15)

                 out <- f_kselect(x = data,
                                  kseq = kseq,
                                  Bcomp = Bcomp,
                                  kmIter = kmIter,
                                  seed = iter,
                                  pbar = F)
                 

                 return(out)
               }

# TT <- proc.time()[3] - tt
# TT

#saveRDS(TT, file='timer.RDS')

stopCluster(cl)

saveRDS(out, paste0('SimResultsIter', iter, '.RDS'))


