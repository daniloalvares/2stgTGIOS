# ####################################################
#
#   2stgTGIOS
#   Individual weighted residuals (IWRES)
#   Cox–Snell and Martingale residuals
#   
#   Q1-2024
#   Danilo Alvares 
#
# ####################################################


# =========================================================
# FUNCTIONS FOR IWRES, COX-SNELL, AND MARTINGALE RESIDUALS
# =========================================================
# Individual weighted residuals (IWRES) function
iwres_fc <- function(fit1, y, times1, times2, treat_times, ID1, ID2, N2StgM=FALSE, fit2=NULL){
  
  # y: outcome (SLD)
  # pred: y_hat (predicted SLD)
  # iwres: (y - y_hat) / sigma
  
  theta <- apply(extract(fit1, "theta")$theta, 2, mean)
  SD_e <- mean(sqrt(extract(fit1, "Var_e")$Var_e))
  if(N2StgM){ bi <- apply(extract(fit2, "bi")$bi, c(2,3), mean)
  }else{ bi <- apply(extract(fit1, "bi")$bi, c(2,3), mean) }

  omega1 <- exp(theta[1] + bi[ID1,1]);
  omega2 <- exp(theta[1] + bi[ID2,1]);
  g1 <- exp(theta[2] + bi[ID1,2]);
  g2 <- exp(theta[2] + bi[ID2,2]);
  d2 <- exp(theta[3] + bi[ID2,3]);
  part1 <- exp( -d2*(times2-treat_times) )
  part2 <- exp( g2*(times2-treat_times) )
  pred <- c(omega1*exp( times1*g1 ), omega2*exp( treat_times*g2 )*(part1 + part2 - 1))

  iwres <- (y - pred)/SD_e
  
  return( data.frame(time=c(times1,times2), iwres=iwres, pred=pred) )
}


# Cox-Snell and Martingale residuals
cox_snell_martingale_fc <- function(fit1, time, status, X, N2StgM=FALSE, fit2=fit1){
  
  # r_CS (Cox-Snell): H(t) = int_0^t h(u) du [cumulative hazard]
  # r_M (Martingale): delta - r_CS [delta: censoring indicator]
  
  theta <- apply(extract(fit1, "theta")$theta, 2, mean)
  gamma <- mean(extract(fit2, "gamma")$gamma)
  beta <- apply(extract(fit2, "beta")$beta, 2, mean)
  alpha <- mean(extract(fit2, "alpha")$alpha)
  if(N2StgM){ bi <- apply(extract(fit2, "bi")$bi, c(2,3), mean)
  }else{ bi <- apply(extract(fit1, "bi")$bi, c(2,3), mean) }
  
  gi <- exp(theta[2] + bi[,2])
  r_CS <- time^gamma * exp( X %*% beta + alpha * gi )
  r_M <- status - r_CS
  
  return( data.frame(r_CS=r_CS, r_M=r_M, gi=gi, status=status) )
}


# =========================================================
# INDIVIDUAL WEIGHTED RESIDUALS (IWRES)
# =========================================================
iwresJM <- iwres_fc(fitJM, y, times1, times2, treat_times, ID1, ID2, N2StgM=FALSE)
iwresN2StgM <- iwres_fc(fitLong, y, times1, times2, treat_times, ID1, ID2, N2StgM=TRUE, fitN2StgM)
iwres2StgM <- iwres_fc(fitLong, y, times1, times2, treat_times, ID1, ID2, N2StgM=FALSE)

# =========================================================
# COX-SNELL AND MARTINGALE RESIDUALS
# =========================================================
cs_m_JM <- cox_snell_martingale_fc(fitJM, time, status, X, N2StgM=FALSE)
cs_m_N2StgM <- cox_snell_martingale_fc(fitLong, time, status, X, N2StgM=TRUE, fit2=fitN2StgM)
cs_m_2StgM <- cox_snell_martingale_fc(fitLong, time, status, X, N2StgM=FALSE, fit2=fit2StgM)
