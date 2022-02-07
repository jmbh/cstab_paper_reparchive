# jonashaslbeck@gmail.com, July 2017


# -----------------------------------------------------------------------------------------------------------------------
# -------------------------- Functions to Generate Data -----------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------

# Generating circular scenarios

f_circular <- function(seed, n, radius, k, vars, dims) {
  
  set.seed(seed)
  
  # define points on circle
  theta <- seq(0, 2 * pi, length = k+1)
  x <- radius * cos(theta)
  y <- radius * sin(theta)
  
  # generate means and variances for clusters
  l_mean <- list()
  
  for(i in 1:k) l_mean[[i]] <- c(x[i], y[i], rep(0,dims-2))
  l_var <- list()
  for(i in 1:k) {
    l_var[[i]] <- matrix(0, dims, dims)
    if(i/2 == round(i/2)) { var_i <- vars[1] } else { var_i <- vars[2] }
    diag(l_var[[i]]) <- var_i
  }
  
  # Sample from MVN
  l_data <- list()
  for(i in 1:k) l_data[[i]] <- mvrnorm(n, l_mean[[i]], l_var[[i]])
  
  # collapse list in data frame
  return(do.call(rbind, l_data))
}


# Generating elongated scenarios

f_elongated <- function(seed, 
                        n, 
                        k,
                        dims,
                        sd,
                        dist) 
  
{
  
  set.seed(seed)
  
  # create initial data
  m_data <- matrix(NA, nrow = n, ncol = dims)
  for(d in 1:dims) m_data[, d] <- seq(-5, 5, length = n) + rnorm(n, 0, sd)
  
  # create k clusters
  constant_add <- seq(0, dist*(k-1), length = k)
  
  l_data <- list()
  for(i in 1:k) l_data[[i]] <- m_data + constant_add[i]
  
  return(do.call(rbind, l_data))
  
}


# -----------------------------------------------------------------------------------------------------------------------
# -------------------------- Function to call different clustering methods ----------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------


f_kselect <- function(x, # data
                      kseq, # k-sequence
                      type = 'kmeans',
                      Bcomp,
                      kmIter,
                      seed,
                      pbar=FALSE)
{
  
  tt <- proc.time()[3]
  
  # 1) Stability: FangWang (unnormalized)
  set.seed(seed)
  k_1 <- cStability(data = x,
                    kseq = kseq,
                    nB = Bcomp,
                    norm = TRUE,
                    predict = TRUE,
                    method = 'kmeans',
                    kmIter = 10,
                    pbar = F)
  
  # 2) Stability: We (normalized)
  set.seed(seed)
  k_2 <- cStability(data = x,
                    kseq = kseq,
                    nB = Bcomp,
                    norm = TRUE,
                    predict = FALSE,
                    method = 'kmeans',
                    kmIter = 10,
                    pbar = F)
  
  # 3) Gap Statistic + JUMP statistic (based on WCD) +Slope Statistic (based on silhouette)
  set.seed(seed)
  k_3 <- cDistance(data = x,
                   kseq = kseq,
                   method = 'kmeans',
                   kmIter = 10,
                   gapIter = 10)
  
  
  # 4) Gaussian Mixture
  BIC = mclustBIC(x)
  mod1 = Mclust(x, x = BIC)
  
  
  timer <- proc.time()[3] - tt
  
  # output: favoured k of each method
  outlist <- list('data' = x,
                  'kopt_FW' = k_1$k_instab,
                  'kopt_FWn' = k_1$k_instab_norm,
                  'kopt_WE' = k_2$k_instab,
                  'kopt_WEn' = k_2$k_instab_norm,
                  'instab_path_FW' = k_1$instab_path,
                  'instab_path_FWn' = k_1$instab_path_norm,
                  'instab_path_WE' = k_2$instab_path,
                  'instab_path_WEn' = k_2$instab_path_norm,
                  'kopt_Gap' = k_3$k_Gap,
                  'kopt_Jump' = k_3$k_Jump,
                  'kopt_Slope' = k_3$k_Slope,
                  'Silhouettes' = k_3$Silhouettes,
                  'Gaps' = k_3$Gaps,
                  'WCD_data' = k_3$WCD,
                  'WCD_syn' = k_3$WCD_syn,
                  'kopt_GMix' = mod1$G,
                  'time' = timer)
  
  return(outlist)
} # EoF







