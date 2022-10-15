# ####################################################
#
#   2stgTGIOS
#   Summary for models (JM, 2StgM, and N2StgM)
#   
#   Q1-2022
#   Danilo Alvares 
#
# ####################################################


# ===============================================
# REQUIRED SOURCES
# ===============================================
# Package names
pkg4 <- c("tidyverse", "rstan")

# Install packages not yet installed
installed_packages <- pkg4 %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){ 
  install.packages(pkg4[!installed_packages])
}

# Packages loading
invisible(lapply(pkg4, library, character.only = TRUE))


# ===============================================
# EXTRACTING THE POSTERIOR SAMPLE BY PARAMETER
# ===============================================
# LONGITUDINAL PARAMETERS
# Joint Modelling (JM) approach
postJMexpw0 <- exp(extract(fitJM, "theta")$theta[,1])
postJMexpg0 <- exp(extract(fitJM, "theta")$theta[,2])
postJMexpd0 <- exp(extract(fitJM, "theta")$theta[,3])
postJMsigmaw <- sqrt(extract(fitJM, "Var_b")$Var_b[,1])
postJMsigmag <- sqrt(extract(fitJM, "Var_b")$Var_b[,2])
postJMsigmad <- sqrt(extract(fitJM, "Var_b")$Var_b[,3])
postJMsigma <- sqrt(extract(fitJM, "Var_e")$Var_e)

# Longitudinal model for both two-stage (2StgM and N2StgM) approaches
postLongexpw0 <- exp(extract(fitLong, "theta")$theta[,1])
postLongexpg0 <- exp(extract(fitLong, "theta")$theta[,2])
postLongexpd0 <- exp(extract(fitLong, "theta")$theta[,3])
postLongsigmaw <- sqrt(extract(fitLong, "Var_b")$Var_b[,1])
postLongsigmag <- sqrt(extract(fitLong, "Var_b")$Var_b[,2])
postLongsigmad <- sqrt(extract(fitLong, "Var_b")$Var_b[,3])
postLongsigma <- sqrt(extract(fitLong, "Var_e")$Var_e)

# SURVIVAL PARAMETERS
# Joint Modelling (JM) approach
postJMbetasex <- extract(fitJM, "beta")$beta[,2]
postJMbetaldh <- extract(fitJM, "beta")$beta[,3]
postJMalpha <- extract(fitJM, "alpha")$alpha
postJMWeibshape <- extract(fitJM, "gamma")$gamma
postJMWeibscale <- exp(extract(fitJM, "beta")$beta[,1])

# Two-stage Modelling (2StgM) approach
post2StgMbetasex <- extract(fit2StgM, "beta")$beta[,2]
post2StgMbetaldh <- extract(fit2StgM, "beta")$beta[,3]
post2StgMalpha <- extract(fit2StgM, "alpha")$alpha
post2StgMWeibshape <- extract(fit2StgM, "gamma")$gamma
post2StgMWeibscale <- exp(extract(fit2StgM, "beta")$beta[,1])

# Novel Two-stage Modelling (N2StgM) approach
postN2StgMbetasex <- extract(fitN2StgM, "beta")$beta[,2]
postN2StgMbetaldh <- extract(fitN2StgM, "beta")$beta[,3]
postN2StgMalpha <- extract(fitN2StgM, "alpha")$alpha
postN2StgMWeibshape <- extract(fitN2StgM, "gamma")$gamma
postN2StgMWeibscale <- exp(extract(fitN2StgM, "beta")$beta[,1])


# ===============================================
# SUMMARY FOR MODELS
# ===============================================
# Joint Modelling (JM) approach
mtJM01 <- paste0(round(mean(postJMexpw0),2)," (",round(quantile(postJMexpw0,0.025),2),", ",round(quantile(postJMexpw0,0.975),2),")")
mtJM02 <- paste0(round(mean(postJMexpg0),2)," (",round(quantile(postJMexpg0,0.025),2),", ",round(quantile(postJMexpg0,0.975),2),")")
mtJM03 <- paste0(round(mean(postJMexpd0),2)," (",round(quantile(postJMexpd0,0.025),2),", ",round(quantile(postJMexpd0,0.975),2),")")
mtJM04 <- paste0(round(mean(postJMsigmaw),2)," (",round(quantile(postJMsigmaw,0.025),2),", ",round(quantile(postJMsigmaw,0.975),2),")")
mtJM05 <- paste0(round(mean(postJMsigmag),2)," (",round(quantile(postJMsigmag,0.025),2),", ",round(quantile(postJMsigmag,0.975),2),")")
mtJM06 <- paste0(round(mean(postJMsigmad),2)," (",round(quantile(postJMsigmad,0.025),2),", ",round(quantile(postJMsigmad,0.975),2),")")
mtJM07 <- paste0(round(mean(postJMsigma),2)," (",round(quantile(postJMsigma,0.025),2),", ",round(quantile(postJMsigma,0.975),2),")")
mtJM08 <- paste0(round(mean(postJMbetasex),2)," (",round(quantile(postJMbetasex,0.025),2),", ",round(quantile(postJMbetasex,0.975),2),")")
mtJM09 <- paste0(round(mean(postJMbetaldh),2)," (",round(quantile(postJMbetaldh,0.025),2),", ",round(quantile(postJMbetaldh,0.975),2),")")
mtJM10 <- paste0(round(mean(postJMalpha),2)," (",round(quantile(postJMalpha,0.025),2),", ",round(quantile(postJMalpha,0.975),2),")")
mtJM11 <- paste0(round(mean(postJMWeibshape),2)," (",round(quantile(postJMWeibshape,0.025),2),", ",round(quantile(postJMWeibshape,0.975),2),")")
mtJM12 <- paste0(round(mean(postJMWeibscale),2)," (",round(quantile(postJMWeibscale,0.025),2),", ",round(quantile(postJMWeibscale,0.975),2),")")

# Longitudinal model for both two-stage (2StgM and N2StgM) approaches
mtLong01 <- paste0(round(mean(postLongexpw0),2)," (",round(quantile(postLongexpw0,0.025),2),", ",round(quantile(postLongexpw0,0.975),2),")")
mtLong02 <- paste0(round(mean(postLongexpg0),2)," (",round(quantile(postLongexpg0,0.025),2),", ",round(quantile(postLongexpg0,0.975),2),")")
mtLong03 <- paste0(round(mean(postLongexpd0),2)," (",round(quantile(postLongexpd0,0.025),2),", ",round(quantile(postLongexpd0,0.975),2),")")
mtLong04 <- paste0(round(mean(postLongsigmaw),2)," (",round(quantile(postLongsigmaw,0.025),2),", ",round(quantile(postLongsigmaw,0.975),2),")")
mtLong05 <- paste0(round(mean(postLongsigmag),2)," (",round(quantile(postLongsigmag,0.025),2),", ",round(quantile(postLongsigmag,0.975),2),")")
mtLong06 <- paste0(round(mean(postLongsigmad),2)," (",round(quantile(postLongsigmad,0.025),2),", ",round(quantile(postLongsigmad,0.975),2),")")
mtLong07 <- paste0(round(mean(postLongsigma),2)," (",round(quantile(postLongsigma,0.025),2),", ",round(quantile(postLongsigma,0.975),2),")")

# Two-stage Modelling (2StgM) approach
mt2StgM08 <- paste0(round(mean(post2StgMbetasex),2)," (",round(quantile(post2StgMbetasex,0.025),2),", ",round(quantile(post2StgMbetasex,0.975),2),")")
mt2StgM09 <- paste0(round(mean(post2StgMbetaldh),2)," (",round(quantile(post2StgMbetaldh,0.025),2),", ",round(quantile(post2StgMbetaldh,0.975),2),")")
mt2StgM10 <- paste0(round(mean(post2StgMalpha),2)," (",round(quantile(post2StgMalpha,0.025),2),", ",round(quantile(post2StgMalpha,0.975),2),")")
mt2StgM11 <- paste0(round(mean(post2StgMWeibshape),2)," (",round(quantile(post2StgMWeibshape,0.025),2),", ",round(quantile(post2StgMWeibshape,0.975),2),")")
mt2StgM12 <- paste0(round(mean(post2StgMWeibscale),2)," (",round(quantile(post2StgMWeibscale,0.025),2),", ",round(quantile(post2StgMWeibscale,0.975),2),")")

# Novel Two-stage Modelling (N2StgM) approach
mtN2StgM08 <- paste0(round(mean(postN2StgMbetasex),2)," (",round(quantile(postN2StgMbetasex,0.025),2),", ",round(quantile(postN2StgMbetasex,0.975),2),")")
mtN2StgM09 <- paste0(round(mean(postN2StgMbetaldh),2)," (",round(quantile(postN2StgMbetaldh,0.025),2),", ",round(quantile(postN2StgMbetaldh,0.975),2),")")
mtN2StgM10 <- paste0(round(mean(postN2StgMalpha),2)," (",round(quantile(postN2StgMalpha,0.025),2),", ",round(quantile(postN2StgMalpha,0.975),2),")")
mtN2StgM11 <- paste0(round(mean(postN2StgMWeibshape),2)," (",round(quantile(postN2StgMWeibshape,0.025),2),", ",round(quantile(postN2StgMWeibshape,0.975),2),")")
mtN2StgM12 <- paste0(round(mean(postN2StgMWeibscale),2)," (",round(quantile(postN2StgMWeibscale,0.025),2),", ",round(quantile(postN2StgMWeibscale,0.975),2),")")

# Summary
postsummary <- cbind(rbind(mtJM01,mtJM02,mtJM03,mtJM04,mtJM05,mtJM06,mtJM07,mtJM08,mtJM09,mtJM10,mtJM11,mtJM12),
                     rbind(mtLong01,mtLong02,mtLong03,mtLong04,mtLong05,mtLong06,mtLong07,mtN2StgM08,mtN2StgM09,mtN2StgM10,mtN2StgM11,mtN2StgM12),
                     rbind(mtLong01,mtLong02,mtLong03,mtLong04,mtLong05,mtLong06,mtLong07,mt2StgM08,mt2StgM09,mt2StgM10,mt2StgM11,mt2StgM12))
colnames(postsummary) <- c("JM", "N2StgM", "2StgM")
rownames(postsummary) <- c("exp{w0}","exp{g0}","exp{d0}","sigmaw","sigmag","sigmad","sigma",
                           "betasex","betaldh1","alpha","Weib shape","Weib scale")
postsummary
