# ####################################################
#
#   2stgTGIOS
#   Models (JM, 2StgM, and N2StgM)
#   
#   Q1-2022
#   Danilo Alvares 
#
# ####################################################


# ===============================================
# REQUIRED SOURCES
# ===============================================
# Package names
pkg3 <- c("rstanarm", "rstan", "shinystan")

# Install packages not yet installed
installed_packages <- pkg3 %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){ 
  install.packages(pkg3[!installed_packages])
}

# Packages loading
invisible(lapply(pkg3, library, character.only = TRUE))


# ===============================================
# FUNCTION FOR MODE ESTIMATION
# ===============================================
mode2 <- function(x){

  lim.inf <- min(x)-1
  lim.sup <- max(x)+1
  s <- density(x,from=lim.inf,to=lim.sup,bw=0.2)
  n <- length(s$y)
  v1 <- s$y[1:(n-2)]
  v2 <- s$y[2:(n-1)]
  v3 <- s$y[3:n]
  ix <- 1+which((v1<v2)&(v2>v3))
  md <- s$x[which(s$y==max(s$y))]
  
  return(md)
}


# ===============================================
# LOADING DATASET
# ===============================================
dta <- readRDS("../datalocal/HorizOSTGI.rds")

# Setting the number of CPU cores for the Stan model
options(mc.cores=parallel::detectCores())


# ===============================================
# REDEFINING THE DATA FORMAT
# ===============================================
# Creating sequential IDs
uniqueUID <- unique(dta$UID)
NewUID <- rep(NA,nrow(dta))
for(i in 1:length(uniqueUID)){
  pos <- which(dta$UID==uniqueUID[i])
  NewUID[pos] <- i
}
dta$UID <- NewUID

# Creating an artificial time of when the treatment was initiated
treat_t <- -dta$TKYEAR[which(dta$BL_VIS=="1")]

y1 <- y2 <- ID1 <- ID2 <- times1 <- times2 <- treat_times <- NULL
for(i in 1:length(uniqueUID)){
  pos1 <- which(dta$UID==i & dta$TKYEAR <= 0)
  pos2 <- which(dta$UID==i & dta$TKYEAR > 0)
  dta$TKYEAR[pos1] <- 0
  dta$TKYEAR[pos2] <- dta$TKYEAR[pos2] + treat_t[i]
  y1 <- c(y1,dta$SLD[pos1])
  y2 <- c(y2,dta$SLD[pos2])
  ID1 <- c(ID1,rep(i,length(pos1)))
  ID2 <- c(ID2,rep(i,length(pos2)))
  times1 <- c(times1,dta$TKYEAR[pos1])
  times2 <- c(times2,dta$TKYEAR[pos2])
  treat_times <- c(treat_times,rep(treat_t[i],length(pos2)))
}


# ===============================================
# DATA FOR THE STAN MODEL
# ===============================================
n <- length(unique(dta$UID))                                     # Total number of individuals
y <- c(y1,y2)                                                    # Longitudinal outcomes
N <- length(y)                                                   # Total number of longitudinal outcomes
N1 <- length(y1)                                                 # Number of longitudinal outcomes before treatment is initiated
N2 <- length(y2)                                                 # Number of longitudinal outcomes after treatment is initiated
time <- dta$OSYEAR[which(dta$BL_VIS=="1")]                       # Survival time
status <- 1-dta$OSCEN[which(dta$BL_VIS=="1")]                    # Vital status (1 = dead, 0 = alive)
X <- model.matrix(~ SEX + LDH1_5, dta[which(dta$BL_VIS=="1"),])  # Design matrix X


# ===============================================
# JOINT MODELLING (JM) APPROACH
# ===============================================
# Setting initial values
init_fun1 <- function(...){ 
  list(theta=c(0,0,0), Var_b=c(1,1,1), Var_e=1, beta=rep(0,ncol(X)), alpha=0, gamma=1, bi=matrix(0,nrow=n,ncol=3))
}

i.timeJM <- Sys.time()
fitJM <- stan(file = "stan/JM.stan", 
               data = list(N=N, N1=N1, N2=N2, n=n, y=y, ID1=ID1, ID2=ID2, 
                           times1=times1, times2=times2, treat_times=treat_times, 
                           time=time, status=status, X=X, nbetas=ncol(X)),
               init = init_fun1,
               warmup = 1000,                 
               iter = 2000,
               thin = 1,
               chains = 3,
               seed = 1,
               cores = getOption("mc.cores",3)) 
e.timeJM <- Sys.time()
e.timeJM-i.timeJM

# Effective sample size
summary(summary(fitJM)$summary[,9])
# R-hat
summary(summary(fitJM)$summary[,10])

print(fitJM)
launch_shinystan(fitJM)


# ===============================================
# LONGITUDINAL MODEL FOR TWO-STAGE APPROACHES
# ===============================================
# Setting initial values
init_fun2 <- function(...){ 
  list(theta=c(0,0,0), Var_b=c(1,1,1), Var_e=1, bi=matrix(0,nrow=n,ncol=3))
}

i.timeLong <- Sys.time()
fitLong <- stan(file = "stan/Long.stan", 
             data = list(N=N, N1=N1, N2=N2, n=n, y=y, ID1=ID1, ID2=ID2, 
                         times1=times1, times2=times2, treat_times=treat_times),
             init = init_fun2,
             warmup = 600,                 
             iter = 1000,
             thin = 1,
             chains = 3,
             seed = 1,
             cores = getOption("mc.cores",3))
e.timeLong <- Sys.time()
e.timeLong-i.timeLong

# Effective sample size
summary(summary(fitLong)$summary[,9])
# R-hat
summary(summary(fitLong)$summary[,10])

print(fitLong)
launch_shinystan(fitLong)


# Maximum a posteriori (MAP)
m.bi <- apply(extract(fitLong, "bi")$bi,c(2,3),mode2)
m.theta <- apply(extract(fitLong, "theta")$theta,2,mode2)
m.Var_b <- apply(extract(fitLong, "Var_b")$Var_b,2,mode2)
m.Var_e <- mode2(extract(fitLong, "Var_e")$Var_e)


# ===============================================
# TWO-STAGE MODELLING (2StgM) APPROACH
# ===============================================
# Setting initial values
init_fun3 <- function(...){ 
  list(beta=rep(0,ncol(X)), alpha=0, gamma=1)
}

i.time2StgM <- Sys.time()
fit2StgM <- stan(file = "stan/2StgM.stan", 
                 data = list(N=N, N1=N1, N2=N2, n=n, y=y, ID1=ID1, ID2=ID2, 
                             times1=times1, times2=times2, treat_times=treat_times, 
                             time=time, status=status, X=X, nbetas=ncol(X),
                             theta=m.theta, bi=m.bi),
                 init = init_fun3,
                 warmup = 100,                 
                 iter = 400,
                 thin = 1,
                 chains = 3,
                 seed = 1,
                 cores = getOption("mc.cores",3)) 
e.time2StgM <- Sys.time()
e.time2StgM-i.time2StgM

# Effective sample size
summary(summary(fit2StgM)$summary[,9])
# R-hat
summary(summary(fit2StgM)$summary[,10])

print(fit2StgM)
launch_shinystan(fit2StgM)


# ===============================================
# NOVEL TWO-STAGE MODELLING (N2StgM) APPROACH
# ===============================================
# Setting initial values
init_fun4 <- function(...){ 
  list(beta=rep(0,ncol(X)), alpha=0, gamma=1, bi=matrix(0,nrow=n,ncol=3))
}

i.timeN2StgM <- Sys.time()
fitN2StgM <- stan(file = "stan/N2StgM.stan", 
                  data = list(N=N, N1=N1, N2=N2, n=n, y=y, ID1=ID1, ID2=ID2, 
                              times1=times1, times2=times2, treat_times=treat_times, 
                              time=time, status=status, X=X, nbetas=ncol(X),
                              theta=m.theta, Var_b=m.Var_b, Var_e=m.Var_e),
                  init = init_fun4,
                  warmup = 100,                 
                  iter = 400,
                  thin = 1,
                  chains = 3,
                  seed = 1,
                  cores = getOption("mc.cores",3))
e.timeN2StgM <- Sys.time()
e.timeN2StgM-i.timeN2StgM

# Effective sample size
summary(summary(fitN2StgM)$summary[,9])
# R-hat
summary(summary(fitN2StgM)$summary[,10])

print(fitN2StgM)
launch_shinystan(fitN2StgM)
