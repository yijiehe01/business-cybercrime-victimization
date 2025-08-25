install.packages("pscl")

library(dplyr)
library(pscl)

# Function to calculate the number of distinct cyber breaches/attacks (DV3)
recodeDV3 <- function(cyber_vars, fraud_vars) {
  if (all(is.na(c(cyber_vars, fraud_vars)))) {
    return(NA)  # If all values are NA, return NA
  }

  # count how many different cyberattack types were experienced (each coded as binary 0/1)
  cyber_count <- sum(cyber_vars > 0, na.rm = TRUE)  # Count distinct cybercrime events

  # If any fraud-related type occurred, treat as one single type
  fraud_flag <- sum(fraud_vars > 0, na.rm = TRUE)  # 1 if any, 0 if none

  # Return the total number of distinct types (cyber + fraud treated separately)
  total_count <- cyber_count + fraud_flag
  return(total_count)
}

# Create DV3
private_sector <- private_sector %>%
  rowwise() %>%
  mutate(
    DV3 = recodeDV3(c_across(all_of(cybercrime_vars)), c_across(all_of(fraud_vars)))
  ) %>%
  ungroup()

# Check the distribution of DV3
table(private_sector$DV3, useNA = "ifany")

# Keep only key columns and merge IVs by respondent ID
final_data_dv3 <- private_sector %>%
  dplyr::select(imid, DV3, weight) %>%
  left_join(ivs_final, by = "imid")

table(final_data_dv3$DV3, useNA = "ifany")

# Define Independent Variables (IVs)
iv_vars <- c("Storing personal data securely", "Removable device storage policy",
             "Separate staff-visitor WiFi", "Cloud & digital services policy", "Restricting access",
             "Remote & personal device policy", "VPN for remote working", "Two-factor authentication",
             "Tools for security monitoring", "Software protections", "Self-protective cyber behaviors",
             "Board members on CS", "Internal audit", "External audit",
             "Roles assigned to individuals after CS incidents", "Security controls & monitoring",
             "Outsourced provider that manages CS", "Admin or Real Estate",
             "Construction, Hospitality & Agriculture", "Education, Entertainment & Health",
             "Finance & Professional", "Info or Communication", "Transport & Utilities",
             "Small", "Medium & Large")

iv_vars_wrapped <- paste0("`", iv_vars, "`")

# Construct model formula for DV3 (Count of distinct cyberattacks)
formula_incl <- as.formula(paste("DV3 ~", paste(iv_vars_wrapped, collapse = " + ")))

# Fit the Hurdle Negative Binomial model for DV3 (including phishing)
hnb_incl <- hurdle(formula_incl,
                         data = final_data_dv3,
                         dist = "negbin",         # Negative Binomial for count part
                         zero.dist = "binomial",   # Binomial logit for zero vs. nonzero
                         weights = weight)
summary(hnb_incl)

# Also fit a Hurdle Poisson model for comparison
hp_incl <- hurdle(formula_incl,
                  data = final_data_dv3,
                  dist = "poisson",      # Poisson for count part
                  zero.dist = "binomial",
                  weights = weight)
summary(hp_incl)

# Compare Model Fit (LogLik, AIC, BIC)
# HNB model fit statistics
logLik(hnb_incl)
AIC(hnb_incl)
BIC(hnb_incl)

# Poisson model fit statistics
logLik(hp_incl)
AIC(hp_incl)
BIC(hp_incl)

# Interpret Model Results (IRRs and ORs with 95% CI)

# Extract model summary object
s_incl <- summary(hnb_incl)

# Count part (Negative Binomial)
# Get coefficients for count component
count_coef_incl <- s_incl$coef$count

# Compute Incidence Rate Ratios (IRRs) and 95% CI
or_count_incl <- exp(count_coef_incl[,"Estimate"])  # IRRs
ci_lower_count_incl <- exp(count_coef_incl[,"Estimate"] - 1.96 * count_coef_incl[,"Std. Error"])
ci_upper_count_incl <- exp(count_coef_incl[,"Estimate"] + 1.96 * count_coef_incl[,"Std. Error"])

# Combine into a summary table for Count model (IRRs)
count_results <- data.frame(
  Variable = rownames(count_coef_incl),
  IRR = round(or_count_incl, 3),
  CI_Lower = round(ci_lower_count_incl, 3),
  CI_Upper = round(ci_upper_count_incl, 3),
  p_value = round(count_coef_incl[, "Pr(>|z|)"], 4)
)

# Add significance stars
count_results$sig <- symnum(count_results$p_value,
                                 corr = FALSE,
                                 na = FALSE,
                                 cutpoints = c(0, .001, .01, .05, .1, 1),
                                 symbols = c("***", "**", "*", ".", " "))
print(count_results)

# Zero hurdle part
# Get coefficients for zero component
zero_coef_incl <- s_incl$coef$zero

# Compute Odds Ratios (ORs) and 95% CI
or_zero_incl <- exp(zero_coef_incl[,"Estimate"])
ci_lower_zero_incl <- exp(zero_coef_incl[,"Estimate"] - 1.96 * zero_coef_incl[,"Std. Error"])
ci_upper_zero_incl <- exp(zero_coef_incl[,"Estimate"] + 1.96 * zero_coef_incl[,"Std. Error"])

# Combine into a summary table for Zero model (ORs)
zero_results <- data.frame(
  Variable = rownames(zero_coef_incl),
  OR = round(or_zero_incl, 3),
  CI_Lower = round(ci_lower_zero_incl, 3),
  CI_Upper = round(ci_upper_zero_incl, 3),
  p_value = round(zero_coef_incl[, "Pr(>|z|)"], 4)
)

# Add significance stars
zero_results$sig <- symnum(zero_results$p_value,
                            corr = FALSE,
                            na = FALSE,
                            cutpoints = c(0, .001, .01, .05, .1, 1),
                            symbols = c("***", "**", "*", ".", " "))
print(zero_results)
