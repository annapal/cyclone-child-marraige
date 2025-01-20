
# Run ETWFE logit model with a unit-level linear trend to test PT assumption

run_etwfe_logit_pt <- function(dat_all, adm_level, iso) {
  
  # De-mean age and rural-urban status
  dat_all$rural <- ifelse(dat_all$res=="rural", 1, 0) # Make indicator for rural status
  dat_all <- dat_all %>%
    group_by(cohort, year) %>% # De-mean within each cohort and year
    mutate(age_dm = age_turned - mean(age_turned),
           rural_dm = rural - mean(rural)) %>%
    ungroup()
  
  # Run the ETWFE model with unit-level linear trend
  min_yr <- min(dat_all$year)
  if (adm_level==2) {
    mod <- feglm(married ~ cyclone2:i(cohort, i.year, ref=1, ref2=min_yr)/
                   (age_dm + rural_dm) +
                   i(cohort, year, ref=1)| # Add cohort-level linear trend
                   cohort[age_turned, rural] + year[age_turned, rural],
                 data=dat_all, family="binomial",
                 vcov=~GID_2, weights=~Denorm_Wt,
                 mem.clean=TRUE, notes = FALSE)
  } else {
    mod <- feglm(married ~ cyclone2:i(cohort, i.year, ref=1, ref2=min_yr)/
                   (age_dm + rural_dm) +
                   i(cohort, year, ref=1)| # Add cohort-level linear trend
                   cohort[age_turned, rural] + year[age_turned, rural],
                 data=dat_all, family="binomial",
                 vcov=~GID_1, weights=~Denorm_Wt,
                 mem.clean=TRUE, notes = FALSE)
  }
  
  # Extract coefficients for unit-level linear trends
  res_pt <- cbind(mod$coefficients, confint(mod), mod$coeftable[,4])
  colnames(res_pt) <- c("coef", "lower", "upper", "p")
  res_pt2 <- res_pt[1:(max(dat_all$cohort)-1),]
  res_pt2$iso <- iso
  res_pt2$cohort <- 2:(max(dat_all$cohort))
  rownames(res_pt2) <- NULL
  
  # Return the results
  res_pt2
}
