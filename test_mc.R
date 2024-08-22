library(ggplot2)
library(glmnet)
library(grf)
library(policytree)
library(splines)
library(lmtest)
library(sandwich)
library(causalTree)
library(MCPanel)


X <- read.csv('https://drive.google.com/uc?export=download&id=1b-jfaWF18m4G6DNYeTJrkx9zjG83Gt9n',header=F)
Y <- t(read.csv('https://drive.google.com/uc?export=download&id=12QfMzIABPWZJUnX-_May178zU-___EGU',header=F))
treat <- t(read.csv('https://drive.google.com/uc?export=download&id=1RCXwDJSSdQ2dvIH2XhEwK7ZOFQimYdQe',header=F))
years <- 1970:2000

## First row (treated unit)
CA_y <- Y[1,]

## Working with the rest of matrix
treat <- treat[-1,]
Y <- Y[-1,]

## Setting up the configuration
N <- nrow(treat)
T <- ncol(treat)
number_T0 <- 5
T0 <- ceiling(T*((1:number_T0)*2-1)/(2*number_T0))
N_t <- 35
num_runs <- 10

## Whether to simulate Simultaneous Adoption or Staggered Adoption
is_simul <- 0  ## 0 for Staggered Adoption and 1 for Simultaneous Adoption

# Matrices to save RMSE values 
MCPanel_RMSE_test <- matrix(0L,num_runs,length(T0))
EN_RMSE_test <- matrix(0L,num_runs,length(T0))
ENT_RMSE_test <- matrix(0L,num_runs,length(T0))
DID_RMSE_test <- matrix(0L,num_runs,length(T0))
ADH_RMSE_test <- matrix(0L,num_runs,length(T0))

## Run different methods

for(i in c(1:num_runs)){
  print(paste0(paste0("Run number ", i)," started"))
  ## Fix the treated units in the whole run for a better comparison
  treat_indices <- sample(1:N, N_t)
  for (j in c(1:length(T0))){
    treat_mat <- matrix(1L, N, T);
    t0 <- T0[j]
    ## Simultaneuous (simul_adapt) or Staggered adoption (stag_adapt)
    if(is_simul == 1){
      treat_mat <- simul_adapt(Y, N_t, t0, treat_indices)
    }
    else{
      treat_mat <- stag_adapt(Y, N_t, t0, treat_indices)
    }
    Y_obs <- Y * treat_mat
    
    ## ------
    ## MC-NNM
    ## ------
    
    est_model_MCPanel <- mcnnm_cv(Y_obs, treat_mat, to_estimate_u = 1, to_estimate_v = 1)
    est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
    est_model_MCPanel$msk_err <- (est_model_MCPanel$Mhat - Y)*(1-treat_mat)
    est_model_MCPanel$test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_MCPanel$msk_err^2))
    MCPanel_RMSE_test[i,j] <- est_model_MCPanel$test_RMSE
    
    ## -----
    ## EN : It does Not cross validate on alpha (only on lambda) and keep alpha = 1 (LASSO).
    ##      Change num_alpha to a larger number, if you are willing to wait a little longer.
    ## -----
    
    est_model_EN <- en_mp_rows(Y_obs, treat_mat, num_alpha = 1)
    est_model_EN_msk_err <- (est_model_EN - Y)*(1-treat_mat)
    est_model_EN_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_EN_msk_err^2))
    EN_RMSE_test[i,j] <- est_model_EN_test_RMSE
    
    ## -----
    ## EN_T : It does Not cross validate on alpha (only on lambda) and keep alpha = 1 (LASSO).
    ##        Change num_alpha to a larger number, if you are willing to wait a little longer.
    ## -----
    est_model_ENT <- t(en_mp_rows(t(Y_obs), t(treat_mat), num_alpha = 1))
    est_model_ENT_msk_err <- (est_model_ENT - Y)*(1-treat_mat)
    est_model_ENT_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ENT_msk_err^2))
    ENT_RMSE_test[i,j] <- est_model_ENT_test_RMSE
    
    ## -----
    ## DID
    ## -----
    
    est_model_DID <- DID(Y_obs, treat_mat)
    est_model_DID_msk_err <- (est_model_DID - Y)*(1-treat_mat)
    est_model_DID_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_DID_msk_err^2))
    DID_RMSE_test[i,j] <- est_model_DID_test_RMSE
    
    ## -----
    ## ADH
    ## -----
    est_model_ADH <- adh_mp_rows(Y_obs, treat_mat)
    est_model_ADH_msk_err <- (est_model_ADH - Y)*(1-treat_mat)
    est_model_ADH_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ADH_msk_err^2))
    ADH_RMSE_test[i,j] <- est_model_ADH_test_RMSE
  }
}

