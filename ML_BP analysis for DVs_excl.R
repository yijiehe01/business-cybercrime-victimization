# -------------------------------
# Step 0: Create a Combined DV for ML Analysis (Using DV1_excl & DV2_excl)
# -------------------------------
final_data <- final_data %>%
  mutate(status_excl = case_when(
    DV1_excl == 0 ~ "nonvictim",
    DV1_excl == 1 & DV2_excl == 0 ~ "single",
    DV1_excl == 1 & DV2_excl == 1 ~ "repeat"
  ))

final_data$status_excl <- factor(final_data$status_excl, levels = c("nonvictim", "single", "repeat"))

# Keep only relevant variables for ML analysis
final_data_excl <- final_data[, !(names(final_data) %in% c("imid", "DV1_incl", "DV2_incl", "DV1_excl", "DV2_excl"))]
final_data_excl <- final_data_excl[!is.na(final_data_excl$status_excl), ]

# Define IVs, removin sector_comb23 due to its perfect collinearity in VIF measurement when using DV2_excl
iv_vars_excl <- setdiff(c("rules15", "policy1", "rules9", "policy5", "policy13",
                     "rules4", "rules8", "policy2", "policy4", "rules18", "rules20",
                     "ident11", "rules1", "rules2", "rules3", "ident14", "rules17", "rules19",
                     "trained", "info_comb4", "manage1", "incidcontent3", "rules5", "rules7",
                     "info_comb8", "manage2", "internal_audit", "external_audit",
                     "sector_comb21", "sector_comb22", "sector_comb23", "sector_comb24", "sector_comb25",
                     "sector_comb26", "sector_comb27", "sector_comb28", "sector_comb29",
                     "sector_comb211", "sector_comb212", "sector_comb213",
                     "sizeb2", "sizeb3", "sizeb4"),
                   "sector_comb23")

# -------------------------------
# Step 1: Multinomial Logit Model (ML1) Base = Nonvictim
# -------------------------------
# Estimate ML model
mli_formula <- as.formula(paste("status_excl ~", paste(iv_vars_excl, collapse = " + ")))
mli_model <- multinom(mli_formula, data = final_data_excl, weights = weight)
summary(mli_model)

# Compute p-values for IV selection
mli_coef <- summary(mli_model)$coefficients
mli_se <- summary(mli_model)$standard.errors
mli_z <- mli_coef / mli_se
mli_p <- 2 * (1 - pnorm(abs(mli_z)))
print(round(mli_p, 3))

# Select significant IVs from ML1 with p<0.2
iv_ml1_excl <- iv_vars_excl[sapply(iv_vars_excl, function(var) {
  any(mli_p[, grep(paste0("^", var, "$"), colnames(mli_p))] < 0.2, na.rm = TRUE)
})]

print(iv_ml1_excl)

# -------------------------------
# Step 2: Selection Equation for BP Model (BP1) Using DV1_excl
# -------------------------------
bp1_formula_excl <- as.formula(paste("DV1_excl ~", paste(iv_ml1_excl, collapse = " + ")))


# -------------------------------
# Step 3: Multinomial Logit Model (ML2)  Base = Single Victim
# -------------------------------
# Recode status to set 'single' as the reference category
final_data_excl2 <- final_data_excl %>%
  mutate(status_excl2 = relevel(status_excl, ref = "single")) %>%
  select(-status_excl)

# Estimate ML model with new reference category
ml_formula_ml2_excl <- as.formula(paste("status_excl2 ~", paste(iv_vars_excl, collapse = " + ")))
ml_model_ml2_excl <- multinom(ml_formula_ml2_excl, data = final_data_excl2, weights = weight)
summary(ml_model_ml2_excl)

# Compute p-values for ML2
ml_coef_ml2 <- summary(ml_model_ml2_excl)$coefficients
ml_se_ml2   <- summary(ml_model_ml2_excl)$standard.errors
ml_z_ml2    <- ml_coef_ml2 / ml_se_ml2
ml_p_ml2    <- 2 * (1 - pnorm(abs(ml_z_ml2)))
print(round(ml_p_ml2, 3))

# Select significant IVs from ML2 with p<0.2
iv_ml2_excl <- Filter(function(var) {
  any(ml_p_ml2["repeat", grep(paste0("^", var), colnames(ml_p_ml2))] < 0.2, na.rm = TRUE)
}, iv_vars_excl)

print(iv_ml2_excl)

# -------------------------------
# Step 4: Outcome Equation for BP Model (BP2) using DV2_excl
# -------------------------------
bp2_formula_excl <- as.formula(paste("DV2_excl ~", paste(iv_ml2_excl, collapse = " + ")))

# -------------------------------
# Step 5: Bivariate Probit Model (BP1 + BP2) for DV Excluding Phishing
# -------------------------------
# Prepare dataset for BP model
bp_predictors_excl <- unique(c(iv_ml1_excl, iv_ml2_excl))
bp_vars_excl <- c("DV1_excl", "DV2_excl", "weight", bp_predictors_excl)

bp_data_excl <- final_data %>% select(all_of(bp_vars_excl))
bp_data_excl <- bp_data_excl[!is.na(bp_data_excl$DV1_excl), ]

# Estimate bivariate probit model
biprobit_model_excl <- selection(
  selection = bp1_formula_excl,
  outcome   = bp2_formula_excl,
  data      = bp_data_excl,
  method    = "biprobit",
  weights   = bp_data_excl$weight
)

# -------------------------------
# Model Summaries & Fit Statistics
# -------------------------------
summary(biprobit_model_excl)

BIC(biprobit_model_excl)
AIC(biprobit_model_excl)