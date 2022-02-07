# jonashaslbeck@gmail.com, July 2017;

# -----------------------------------------------------------------------------------------------------------------------
# -------------------------- Compute Table 1 and 2 from Simulation Results ----------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------

# setwd(...) # !!! Set Working Directory to Reproducability Archive !!!


# --------- Load Simulation Output ----------

simResDir <- paste0('output_newBS/')
# simResDir <- paste0('output_newBS/')
# simResDir <- paste0('output_newCMB/')



v_files <- list.files(simResDir)
n_files <- length(v_files)
l_files <- list()
for(i in 1:n_files) l_files[[i]] <- readRDS(paste0(simResDir, v_files[i]))


# --------- Compute Table k = 1:50 ----------

# Storage
l_time <- list()
l_outAll <- list()

# Loop over six scenarios
for(sim in 1:6) {
  
  # Get optimal Ks out
  l_d <- lapply(l_files, function(x) {
    xd <- x[[sim]]
    out <- c(xd$kopt_FW,
             xd$kopt_FWn,
             xd$kopt_WE,
             xd$kopt_WEn,
             xd$kopt_Gap, 
             xd$kopt_Jump, 
             xd$kopt_Slope, 
             xd$kopt_GMix)
    return(out)
  })
  
  # Get time
  l_time[[sim]] <- unlist(lapply(l_files, function(x) x[[sim]]$time))
  m_d <- do.call(rbind, l_d) # combine
  
  tb_d <- apply(m_d, 2, table)
  tarmat_d <- matrix(NA, nrow=8, ncol=49)
  
  # loop over estimation methods
  for(dd in 1:8){
    for(k in 2:50) {
      kemp <- tb_d[[dd]][which(names(tb_d[[dd]])==k)]
      if(length(kemp)==0) kemp <- 0
      tarmat_d[dd, k-1] <- kemp
    }
  }
  tarmat_d <- cbind(sim, tarmat_d)
  colnames(tarmat_d) <- c('Sim', 2:50)
  tarmat_d <- rbind(tarmat_d, NA)
  rownames(tarmat_d) <- c('S_MB', 'S_MB_n', 'S_MF', 'S_MF_n', 'Gap', 'Jump', 'Slope', 'GM', 'NA')
  l_outAll[[sim]] <- tarmat_d
  
}

m_all <- do.call(rbind, l_outAll)

# Markdown table
# library(knitr)
# kable(m_all, format = "markdown")


# --------- Collapsed Table k = 1:20 ----------

m_all_new <- m_all[,1:20]
TWplus <- rowSums(m_all[,20:ncol(m_all)])
m_all_new[,20] <- TWplus

# kable(m_all_new, format = "markdown")

library(xtable)

xt <- xtable(m_all_new)
print.xtable(xt, digits = rep(0, 20))








