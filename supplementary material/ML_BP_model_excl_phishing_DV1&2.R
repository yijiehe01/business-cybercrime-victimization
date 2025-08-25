# EXCLUDING PHISHING CASES

# Create DV1_excl (victimisation status excluding phishing)
private_sector <- private_sector %>%
  mutate(
    DV1_excl = case_when(
      if_all(all_of(c(cybercrime_vars[-6], fraud_vars)), is.na) ~ NA_real_,
      rowSums(across(all_of(c(cybercrime_vars[-6], fraud_vars))), na.rm = TRUE) > 0 ~ 1,
      rowSums(across(all_of(c(cybercrime_vars[-6], fraud_vars))), na.rm = TRUE) == 0 ~ 0
    )
  )
table(private_sector$DV1_excl, useNA = "ifany")

# Create DV2_excl (repeat victimisation excluding phishing)
private_sector <- private_sector %>%
  rowwise() %>%
  mutate(
    DV2_excl = recodeDV2(c_across(all_of(c(cybercrime_vars[-6], fraud_vars))), DV1_excl)
  ) %>%
  ungroup()
table(private_sector$DV2_excl, useNA = "ifany") # exclude phishing

# Select core variables for merging with IVs
private_sel <- private_sector %>%
  dplyr::select(imid,DV1_excl, DV2_excl, weight)
final_data <- private_sel %>% left_join(ivs_final, by = "imid")

# Create 3-category DV: status_excl (nonvictim, single, repeat) excluding phishing
final_data <- final_data %>%
  mutate(status_excl = case_when(
    DV1_excl == 0 ~ "nonvictim",
    DV1_excl == 1 & DV2_excl == 0 ~ "single",
    DV1_excl == 1 & DV2_excl == 1 ~ "repeat"
  ))

final_data$status_excl <- factor(final_data$status_excl, levels = c("nonvictim", "single", "repeat"))

# Keep complete cases only
final_data_excl <- final_data[!is.na(final_data$status_excl), ]

# Measure VIF using the three-category DV excluding phishing
model_data_excl <- cbind(
  ivs_final[, -which(names(ivs_final) == "imid")],  # Include all IVs except "imid"
  status_excl = final_data$status_excl             # Include three-category DV
)

# Fit a multinomial logistic regression model (status_excl as DV)
multinom_model_excl <- multinom(status_excl ~ ., data = model_data_excl)

# Compute VIF values
vif_values_excl <- vif(multinom_model_excl)
print(vif_values_excl)

iv_vars_excl <- c("Storing personal data securely","Removable device storage policy",
                  "Separate staff-visitor WiFi",
                  "Cloud & digital services policy", "Restricting access",
                  "Remote & personal device policy","VPN for remote working",
                  "Two-factor authentication",
                  "Tools for security monitoring","Software protections",
                  "Self-protective cyber behaviors","Board members on CS",
                  "Internal audit","External audit",
                  "Roles assigned to individuals after CS incidents",
                  "Security controls & monitoring",
                  "Outsourced provider that manages CS",
                  "Admin or Real Estate", "Construction, Hospitality & Agriculture",
                  "Education, Entertainment & Health", "Finance & Professional",
                  "Info or Communication",
                  "Transport & Utilities","Small", "Medium & Large")

# Now wrap each name in backticks
iv_vars_excl <- paste0("`", iv_vars_excl, "`")

# Multinomial Logit Model (ML1) Base = Nonvictim
ml_formula_excl <- as.formula(paste("status_excl ~", paste(iv_vars_excl, collapse = " + ")))
ml_model_excl <- multinom(ml_formula_excl, data = final_data_excl, weights = weight)

# Extract p-values from ML1
ml_coef <- summary(ml_model_excl)$coefficients
ml_se <- summary(ml_model_excl)$standard.errors
ml_z <- ml_coef / ml_se
ml_p <- 2 * (1 - pnorm(abs(ml_z)))
print(round(ml_p, 3))


# Compute odds ratios and 95% confidence intervals
ml_or <- exp(ml_coef)
ci_lower <- exp(ml_coef - 1.96 * ml_se)  # Lower bound
ci_upper <- exp(ml_coef + 1.96 * ml_se)  # Upper bound

print(ml_or)
print(ci_lower)
print(ci_upper)

# Helper function to remove backticks from a string
clean_name <- function(x) {
  gsub("`", "", x)
}

# Select significant predictors (p < 0.10) for either contrast (single or repeat)
iv_ml1_excl <- iv_vars[sapply(iv_vars, function(var) {
  var_clean <- clean_name(var)
  ml_p_names <- clean_name(colnames(ml_p))
  col_indices <- grep(var_clean, ml_p_names, value = FALSE)
  if (length(col_indices) == 0) return(FALSE)
  p_values <- ml_p[c("single", "repeat"), col_indices, drop = FALSE]
  return(any(p_values < 0.10, na.rm = TRUE))
})]
print(iv_ml1_excl)

# Define selection equation for biprobit (DV1)
bp1_formula_excl <- as.formula(paste("DV1_excl ~", paste(iv_ml1_excl, collapse = " + ")))

# Recode status to set 'single' as the reference category
final_data_excl <- final_data_excl %>%
  mutate(status_excl2 = relevel(status_excl, ref = "single")) %>%
  dplyr::select(-status_excl)

# Fit ML2 with new base (single)
ml_formula2_excl <- as.formula(paste("status_excl2 ~", paste(iv_vars_excl, collapse = " + ")))
ml_model2_excl <- multinom(ml_formula2_excl, data = final_data_excl, weights = weight)

# Compute p-values for ML2
ml_coef2 <- summary(ml_model2_excl)$coefficients
ml_se2   <- summary(ml_model2_excl)$standard.errors
ml_z2    <- ml_coef2 / ml_se2
ml_p2    <- 2 * (1 - pnorm(abs(ml_z2)))
print(round(ml_p2, 3))

# Compute odds ratios (OR) and 95% CIs
ml_or <- exp(ml_coef2)
ci_lower <- exp(ml_coef2 - 1.96 * ml_se2)  # Lower bound
ci_upper <- exp(ml_coef2 + 1.96 * ml_se2)  # Upper bound

print(ml_or)
print(ci_lower)
print(ci_upper)

# Select significant IVs from ML2
iv_ml2_excl <- Filter(function(var) {
  any(ml_p2["repeat", grep(paste0("^", var), colnames(ml_p2))] < 0.1, na.rm = TRUE)
}, iv_vars_excl)
print(iv_ml2_excl)

# Define outcome equation for biprobit
bp2_formula_excl <- as.formula(paste("DV2_excl ~", paste(iv_ml2_excl, collapse = " + ")))

# Prepare final predictors and dataset
bp_predictors_excl <- unique(c(iv_ml1_excl, iv_ml2_excl))
bp_predictors_excl <- gsub("`", "", bp_predictors_excl)
bp_vars_excl <- c("DV1_excl", "DV2_excl", "weight", bp_predictors_excl)

bp_data_excl <- final_data %>% dplyr::select(all_of(bp_vars_excl))
bp_data_excl <- bp_data_excl[!is.na(bp_data_excl$DV1_excl), ]

biprobit_model_excl <- selection(
  selection = bp1_formula_excl,
  outcome   = bp2_formula_excl,
  data      = bp_data_excl,
  method    = "biprobit",
  weights   = bp_data_excl$weight
)

# Summarise final model output
summary(biprobit_model_excl)

# Function: Calculate AMEs for binary predictors (point estimate only)
calc_mfx_binary <- function(model, data, predictors, part = "outcome", type = "unconditional") {
  pred_base <- predict(model, newdata = data, part = part, type = type)
  mfx <- numeric(length(predictors))
  names(mfx) <- predictors
  for (i in seq_along(predictors)) {
    data_mod <- data
    data_mod[[predictors[i]]] <- 1 - data_mod[[predictors[i]]]
    pred_new <- predict(model, newdata = data_mod, part = part, type = type)
    mfx[i] <- mean(pred_new - pred_base, na.rm = TRUE)
  }
  return(mfx)
}

# Function: Bootstrap SEs and p-values for binary predictors
calc_mfx_bootstrap_binary <- function(model, data, predictors, part = "outcome", type = "unconditional", nboot = 1000) {
  orig_mfx <- calc_mfx_binary(model, data, predictors, part, type)
  boot_mfx <- matrix(NA, nrow = nboot, ncol = length(predictors))
  colnames(boot_mfx) <- predictors

  set.seed(123)
  for (i in 1:nboot) {
    boot_sample <- data[sample(nrow(data), replace = TRUE), ]
    boot_mfx[i, ] <- calc_mfx_binary(model, boot_sample, predictors, part, type)
  }

  se_mfx <- apply(boot_mfx, 2, sd, na.rm = TRUE)
  z_stats <- orig_mfx / se_mfx
  p_values <- 2 * (1 - pnorm(abs(z_stats)))

  results <- data.frame(
    Predictor = predictors,
    MarginalEffect = orig_mfx,
    StdError = se_mfx,
    zValue = z_stats,
    pValue = p_values
  )
  return(results)
}

# Function: Add significance stars and formatted p-values
add_sig_labels <- function(results_df) {
  results_df %>%
    mutate(
      pLabel = case_when(
        is.na(pValue) ~ "NA",
        pValue < 0.001 ~ "< .001",
        pValue < 0.01  ~ "< .01",
        pValue < 0.05  ~ "< .05",
        pValue < 0.10  ~ "< .10",
        TRUE           ~ paste0("= ", round(pValue, 3))
      ),
      sigStar = case_when(
        is.na(pValue)  ~ "",
        pValue < 0.001 ~ "***",
        pValue < 0.01  ~ "**",
        pValue < 0.05  ~ "*",
        pValue < 0.10  ~ ".",
        TRUE           ~ ""
      )
    )
}

# Average Marginal Effects for Phishing-Excluded Bivariate Probit Model
# Re-define predictor names for phishing-excluded BP model
bp_predictors_selection_excl <- c(
  "Separate staff-visitor WiFi",
  "Restricting access",
  "Remote & personal device policy",
  "External audit",
  "Roles assigned to individuals after CS incidents",
  "Admin or Real Estate",
  "Education, Entertainment & Health",
  "Finance & Professional"
)

bp_predictors_outcome_excl <- c("Remote & personal device policy")

# Compute AMEs for Selection Equation (DV1_excl)
bp_mfx_selection_excl <- calc_mfx_bootstrap_binary(
  model = biprobit_model_excl,
  data = bp_data_excl,
  predictors = bp_predictors_selection_excl,
  part = "selection",
  type = "response",
  nboot = 1000
)

# Compute AMEs for Outcome Equation (DV2_excl)
bp_mfx_outcome_excl <- calc_mfx_bootstrap_binary(
  model = biprobit_model_excl,
  data = bp_data_excl,
  predictors = bp_predictors_outcome_excl,
  part = "outcome",
  type = "unconditional",
  nboot = 1000
)

# Format output with significance stars and p-value labels
bp_mfx_selection_excl <- add_sig_labels(bp_mfx_selection_excl)
bp_mfx_outcome_excl   <- add_sig_labels(bp_mfx_outcome_excl)

# View results in console
print(bp_mfx_selection_excl)
print(bp_mfx_outcome_excl)
