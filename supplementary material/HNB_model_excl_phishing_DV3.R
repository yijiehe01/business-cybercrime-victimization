# EXCLUDING PHISHING CASES

# Recode DV3_excl: Number of distinct cyber-attacks excluding phishing
private_sector <- private_sector %>%
  rowwise() %>%
  mutate(
    DV3_excl = recodeDV3(c_across(all_of(cybercrime_vars[-6])), c_across(all_of(fraud_vars)))
  ) %>%
  ungroup()

table(private_sector$DV3_excl, useNA = "ifany")

formula_excl <- as.formula(paste("DV3_excl ~", paste(iv_vars_wrapped, collapse = " + ")))

# Merge DV3_excl and IVs into one dataframe
final_data_dv3 <- private_sector %>%
  dplyr::select(imid,DV3_excl, weight) %>%
  left_join(ivs_final, by = "imid")

table(final_data_dv3$DV3_excl, useNA = "ifany")

# Fit the Hurdle Negative Binomial model for DV3_excl
hnb_excl <- hurdle(formula_excl,
                   data = final_data_dv3,
                   dist = "negbin",
                   zero.dist = "binomial",
                   weights = weight)
summary(hnb_excl)


#Hurdle NB Model Results for DV3 Excluding Phishing
# Get the summary from your hurdle model (excluding phishing)
s_excl <- summary(hnb_excl)

# Count Part
count_coef_excl <- s_excl$coef$count

# Calculate Incidence Rate Ratios (IRRs) and 95% Confidence Intervals
irr_count_excl <- exp(count_coef_excl[, "Estimate"])
ci_lower_count_excl <- exp(count_coef_excl[, "Estimate"] - 1.96 * count_coef_excl[, "Std. Error"])
ci_upper_count_excl <- exp(count_coef_excl[, "Estimate"] + 1.96 * count_coef_excl[, "Std. Error"])

# Create summary table for Count Part
count_results_excl <- data.frame(
  Variable = rownames(count_coef_excl),
  IRR = round(irr_count_excl, 3),
  CI_Lower = round(ci_lower_count_excl, 3),
  CI_Upper = round(ci_upper_count_excl, 3),
  p_value = round(count_coef_excl[, "Pr(>|z|)"], 4)
)

# Add significance stars
count_results_excl$sig <- symnum(count_results_excl$p_value,
                                 corr = FALSE,
                                 na = FALSE,
                                 cutpoints = c(0, .001, .01, .05, .1, 1),
                                 symbols = c("***", "**", "*", ".", " "))
print(count_results_excl)

# Zero Part
zero_coef_excl <- s_excl$coef$zero

# Calculate Odds Ratios (ORs) and 95% Confidence Intervals
or_zero_excl <- exp(zero_coef_excl[, "Estimate"])
ci_lower_zero_excl <- exp(zero_coef_excl[, "Estimate"] - 1.96 * zero_coef_excl[, "Std. Error"])
ci_upper_zero_excl <- exp(zero_coef_excl[, "Estimate"] + 1.96 * zero_coef_excl[, "Std. Error"])

# Create summary table for Zero Part
zero_results_excl <- data.frame(
  Variable = rownames(zero_coef_excl),
  OR = round(or_zero_excl, 3),
  CI_Lower = round(ci_lower_zero_excl, 3),
  CI_Upper = round(ci_upper_zero_excl, 3),
  p_value = round(zero_coef_excl[, "Pr(>|z|)"], 4)
)

# Add significance stars
zero_results_excl$sig <- symnum(zero_results_excl$p_value,
                                corr = FALSE,
                                na = FALSE,
                                cutpoints = c(0, .001, .01, .05, .1, 1),
                                symbols = c("***", "**", "*", ".", " "))
print(zero_results_excl)
