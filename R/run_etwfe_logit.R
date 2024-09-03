
# Run the ETWFE model (logistic regression for binary outcome)

run_etwfe_logit <- function(dat_all, adm_level, iso) {
  
  # De-mean age and rural-urban status
  dat_all$rural <- ifelse(dat_all$res=="rural", 1, 0) # Make indicator for rural status
  dat_all <- dat_all %>%
    group_by(cohort, year) %>% # De-mean within each cohort and year
    mutate(age_dm = age_turned - mean(age_turned),
           rural_dm = rural - mean(rural)) %>%
    ungroup()
  
  # Run the ETWFE model
  min_yr <- min(dat_all$year)
  if (adm_level==2) {
    mod <- feglm(married ~ cyclone2:i(cohort, i.year, ref=1, ref2=min_yr)/
                   (age_dm + rural_dm)|
                   cohort[age_turned, rural] + year[age_turned, rural],
                 data=dat_all, family="binomial",
                 vcov=~GID_2, weights=~Denorm_Wt,
                 mem.clean=TRUE, notes = FALSE)
  } else {
    mod <- feglm(married ~ cyclone2:i(cohort, i.year, ref=1, ref2=min_yr)/
                   (age_dm + rural_dm)|
                   cohort[age_turned, rural] + year[age_turned, rural],
                 data=dat_all, family="binomial",
                 vcov=~GID_1, weights=~Denorm_Wt,
                 mem.clean=TRUE, notes = FALSE)
  }
  
  # Calculate main TE
  result <- slopes(
    mod,
    newdata = subset(dat_all, cyclone2==1), # Only region-years exposed to cyclone
    variables = "cyclone2",
    by = "cyclone2",
    wts = "Denorm_Wt"
  )
  result$iso <- iso # Append iso code
  
  # Return the results
  result
}