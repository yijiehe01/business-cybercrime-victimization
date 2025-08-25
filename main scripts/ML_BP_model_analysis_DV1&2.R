# Load required packages
library(survey)
library(dplyr)
library(ggplot2)
library(haven)
library(nnet)
library(sampleSelection)
library(here)
library(car)

# Define relative data path
data_path <- here("data", "csbs_2024_archive_data_public.sav")

# Load .sav file
csbs2024 <- read_sav(data_path)

# Subset to private sector businesses only
private_sector <- csbs2024 %>% filter(questtype == 1)

# Extract all IVs of interest (excluding the unique ID “imid” variable which is only for merging later)
ivs <- private_sector[, c("imid", "sector_comb2", "sizeb", "rules15", "policy1", "rules9", "policy5",
                          "policy13", "rules4", "rules8", "policy2", "policy4", "rules18",
                          "rules20", "ident11", "rules1", "rules2", "rules3", "ident14",
                          "rules17", "rules19", "trained", "manage1", "audit",
                          "incidcontent3", "rules5", "rules7", "manage2")]

# Rename IVs to descriptive labels for clarity
private_sector <- private_sector %>%
  rename(
    `Business sectors` = sector_comb2,
    `Business sizes` = sizeb,
    `Storing personal data securely` = rules15,
    `Removable device storage policy` = policy1,
    `Separate staff-visitor WiFi` = rules9,
    `Cloud computing policy` = policy5,
    `Digital service providers policy` = policy13,
    `Restricting access rights` = rules4,
    `Allowing access only via company-owned devices` = rules8,
    `Remote or mobile working policy` = policy2,
    `Use of personally-own devices policy` = policy4,
    `VPN for remote working` = rules18,
    `Two-factor authentication` = rules20,
    `Tools for security monitoring` = ident11,
    `Regular software security updates` = rules1,
    `Up-to-date malware protection` = rules2,
    `Firewall` = rules3,
    `Testing staff CS awareness` = ident14,
    `Guidance strong passwords` = rules17,
    `Process for reporting fraudulent emails or websites` = rules19,
    `CS trainings or awareness sessions` = trained,
    `Board members on CS` = manage1,
    `CS audit` = audit,
    `Roles assigned to individuals after CS incidents` = incidcontent3,
    `Monitoring of user activity` = rules5,
    `Security controls on company-owned devices` = rules7,
    `Outsourced provider that manages CS` = manage2
  )

ivs <- ivs %>%
  rename(
    `Business sectors` = sector_comb2,
    `Business sizes` = sizeb,
    `Storing personal data securely` = rules15,
    `Removable device storage policy` = policy1,
    `Separate staff-visitor WiFi` = rules9,
    `Cloud computing policy` = policy5,
    `Digital service providers policy` = policy13,
    `Restricting access rights` = rules4,
    `Allowing access only via company-owned devices` = rules8,
    `Remote or mobile working policy` = policy2,
    `Use of personally-own devices policy` = policy4,
    `VPN for remote working` = rules18,
    `Two-factor authentication` = rules20,
    `Tools for security monitoring` = ident11,
    `Regular software security updates` = rules1,
    `Up-to-date malware protection` = rules2,
    `Firewall` = rules3,
    `Testing staff CS awareness` = ident14,
    `Guidance strong passwords` = rules17,
    `Process for reporting fraudulent emails or websites` = rules19,
    `CS trainings or awareness sessions` = trained,
    `Board members on CS` = manage1,
    `CS audit` = audit,
    `Roles assigned to individuals after CS incidents` = incidcontent3,
    `Monitoring of user activity` = rules5,
    `Security controls on company-owned devices` = rules7,
    `Outsourced provider that manages CS` = manage2
  )

# Recode missing values: design-missing (NA) as 0; -97 (Don't Know) as NA
missing_counts <- colSums(is.na(ivs))
print(missing_counts[missing_counts > 0])

# Recode design-missing values as 0, and -97 ("Don't Know") as NA
ivs <- ivs %>%
  mutate(across(c("Removable device storage policy", "Cloud computing policy",
                  "Digital service providers policy", "Remote or mobile working policy",
                  "Use of personally-own devices policy", "CS audit"),
                ~ ifelse(is.na(.), 0, .))) %>%
  # Then, recode values equal to -97 as NA
  mutate(across(-imid, ~ ifelse(. == -97, NA_real_, .)))

# Check the missing value counts after recoding
print(colSums(is.na(ivs)))

# Aggregate the three technical self-protection variables into a composite binary variable to reduce VIF
ivs <- ivs %>%
  mutate(`Software protections` = case_when(
    `Regular software security updates` == 1 &
      `Up-to-date malware protection` == 1 &
      `Firewall` == 1 ~ 1,  # If all three measures are followed, assign 1
    TRUE ~ 0  # Otherwise, assign 0
  )) %>%
  dplyr::select(-c(`Regular software security updates`, `Up-to-date malware protection`, `Firewall`))

# Aggregate the two accessibility-related variables
ivs <- ivs %>%
  mutate(`Restricting access` = case_when(
    `Restricting access rights` == 1 & `Allowing access only via company-owned devices` == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  dplyr::select(-c(`Restricting access rights`, `Allowing access only via company-owned devices`))

# Convert the "trained" variable into (0/1) format like other IVs
ivs$`CS trainings or awareness sessions` <- case_when(
  ivs$`CS trainings or awareness sessions` == 1 ~ 1,  # Yes
  ivs$`CS trainings or awareness sessions` == 2 ~ 0,  # No
  TRUE ~ NA_real_
)

# Aggregate two visibility-related variables
ivs <- ivs %>%
  mutate(`Cloud & digital services policy` = case_when(
    `Cloud computing policy` == 1 & `Digital service providers policy` == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  dplyr::select(-c(`Cloud computing policy`, `Digital service providers policy`))

# Aggregate two accessibility-related variables
ivs <- ivs %>%
  mutate(`Remote & personal device policy` = case_when(
    `Remote or mobile working policy` == 1 & `Use of personally-own devices policy` == 1 ~ 1,
    TRUE ~ 0
  ))%>%
  dplyr::select(-c(`Remote or mobile working policy`, `Use of personally-own devices policy`))

# Aggregate two internal guardianship variables
ivs <- ivs %>%
  mutate(`Security controls & monitoring` = case_when(
    `Security controls on company-owned devices` == 1 & `Monitoring of user activity` == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  dplyr::select(-c(`Security controls on company-owned devices`, `Monitoring of user activity`))

# Aggregate the four personal self-protection variables
ivs <- ivs %>%
  mutate(`Self-protective cyber behaviors` = case_when(
    rowSums(across(c(
      `Testing staff CS awareness`,
      `CS trainings or awareness sessions`,
      `Guidance strong passwords`,
      `Process for reporting fraudulent emails or websites`
    )) == 1, na.rm = TRUE) >= 3 ~ 1,    # if the businesses followed three or all measures, assign 1
    TRUE ~ 0 # otherwise 0
  )) %>%
  dplyr::select(-c(
    `Testing staff CS awareness`,
    `CS trainings or awareness sessions`,
    `Guidance strong passwords`,
    `Process for reporting fraudulent emails or websites`
  ))

# Create dummy variables for "audit" to classify as internal and external audits
dummy_audit <- data.frame(
  `Internal audit` = ifelse(ivs$`CS audit` %in% c(1, 3), 1,
                            ifelse(ivs$`CS audit` == 2, 0, 0)),
  `External audit` = ifelse(ivs$`CS audit` %in% c(2, 3), 1,
                            ifelse(ivs$`CS audit` == 1, 0, 0)),
  check.names = FALSE
)

ivs$`Business sectors` <- as.character(ivs$`Business sectors`)

# Aggregate sectors based on conceptual similarities and assign meaningful names
ivs$`Business sectors`[ivs$`Business sectors` %in% c("2", "6", "13")] <- "Construction, Hospitality & Agriculture"
ivs$`Business sectors`[ivs$`Business sectors` %in% c("3", "4", "7")]  <- "Education, Entertainment & Health"
ivs$`Business sectors`[ivs$`Business sectors` %in% c("5", "9")]      <- "Finance & Professional"
ivs$`Business sectors`[ivs$`Business sectors` %in% c("11", "12")]    <- "Transport & Utilities"

# Convert remaining single, individual sectors to meaningful names
ivs$`Business sectors`[ivs$`Business sectors` == "1"]  <- "Admin or Real Estate"
ivs$`Business sectors`[ivs$`Business sectors` == "8"]  <- "Info or Communication"
ivs$`Business sectors`[ivs$`Business sectors` == "10"] <- "Retail or Wholesale"

# Convert to factor with ordered levels (set "Retail_Wholesale" as reference group)
ivs$`Business sectors` <- factor(ivs$`Business sectors`, levels = c(
  "Retail or Wholesale", "Admin or Real Estate", "Construction, Hospitality & Agriculture",
  "Education, Entertainment & Health", "Finance & Professional", "Info or Communication", "Transport & Utilities"
))

# Create dummy variables excluding reference group
dummy_sector <- model.matrix(~ `Business sectors` - 1, data = ivs)

# Remove the literal text `Business sectors` from column names
colnames(dummy_sector) <- gsub(
  "^`Business sectors`|^Business\\.sectors",  # pattern to remove
  "",                                         # replace with nothing
  colnames(dummy_sector)
)

# Convert 'Business sizes' into a factor with redefined levels
ivs$`Business sizes` <- factor(ivs$`Business sizes`, levels = c(1, 2, 3, 4))

# Aggregate 'Medium' and 'Large' into 'Medium-Large'
levels(ivs$`Business sizes`) <- c("Micro", "Small", "Medium & Large", "Medium & Large")

# Generate dummy variables
dummy_sizeb <- model.matrix(~ `Business sizes` - 1, data = ivs)

# Remove the literal text `Business sizes` from column names
colnames(dummy_sizeb) <- gsub(
  "^`Business sizes`|^Business\\.sizes",
  "",
  colnames(dummy_sizeb)
)

# Combine all selected IVs with the dummy variables into the final IV dataset
ivs_final <- cbind(
  ivs[, c("imid", "Storing personal data securely", "Removable device storage policy",
          "Separate staff-visitor WiFi",
          "Cloud & digital services policy", "Restricting access",
          "Remote & personal device policy",
          "VPN for remote working", "Two-factor authentication",
          "Tools for security monitoring", "Software protections",
          "Self-protective cyber behaviors","Board members on CS",
          "Roles assigned to individuals after CS incidents",
          "Security controls & monitoring",
          "Outsourced provider that manages CS")],
  dummy_audit,
  dummy_sector[, -1],   # Exclude the reference level (Retail/Wholesale)
  dummy_sizeb[, -1] # Exclude the reference group (Micro)
)

cybercrime_vars <- c("ranssoft", "hacksiv", "tkvrsuc", "dossoft", "virussoft", "phisheng")
fraud_vars <- c("fraud1", "fraud2", "fraud3", "fraud4", "fraudconta",
                "fraudcontb", "fraudcontc", "fraudcontd", "fraudconte",
                "fraudcontf", "fraudcontg", "fraudconth", "fraudconti")

# Convert special missing values (-97 = "Don't Know", -99 = "Refused") into NA
private_sector <- private_sector %>%
  mutate(across(all_of(c(cybercrime_vars, fraud_vars)),
                ~ case_when(
                  . == -97 ~ NA_real_,  # "Don't know"
                  . == -99 ~ NA_real_,  # "Refused"
                  TRUE     ~ .
                )))

# DV1: Victimisation status
# 1 = experienced at least one cybercrime or fraud incident
# 0 = did not experience any such incident
private_sector <- private_sector %>%
  mutate(
    DV1 = case_when(
      if_all(all_of(c(cybercrime_vars, fraud_vars)), is.na) ~ NA_real_,
      rowSums(across(all_of(c(cybercrime_vars, fraud_vars))), na.rm = TRUE) > 0 ~ 1,
      rowSums(across(all_of(c(cybercrime_vars, fraud_vars))), na.rm = TRUE) == 0 ~ 0
    )
  )

# Frequency table for DV1
table(private_sector$DV1, useNA = "ifany")

# DV2: Repeat victimisation among those with DV1 = 1
# 1 = experienced any type of cybercrime/fraud more than once
# 0 = experienced only once
recodeDV2 <- function(vars, dv1) {
  if (is.na(dv1)) return(NA)
  if (dv1 == 0) return(NA)
  rep_flag <- any(vars > 1, na.rm = TRUE)
  return(ifelse(rep_flag, 1, 0))
}

private_sector <- private_sector %>%
  rowwise() %>%
  mutate(
    DV2 = recodeDV2(c_across(all_of(c(cybercrime_vars, fraud_vars))), DV1)
  ) %>%
  ungroup()

# Frequency table for DV2
table(private_sector$DV2, useNA = "ifany")

# Merge DV1, DV2, and weights with IVs
private_sel <- private_sector %>%
  dplyr::select(imid, DV1,DV2, weight)
final_data <- private_sel %>% left_join(ivs_final, by = "imid")

# create the three-category DV using DV1 and DV2
final_data <- final_data %>%
  mutate(status = case_when(
    DV1 == 0 ~ "nonvictim",
    DV1 == 1 & DV2 == 0 ~ "single",
    DV1 == 1 & DV2 == 1 ~ "repeat"
  ))

final_data$status <- factor(final_data$status, levels = c("nonvictim", "single", "repeat"))

# Drop unneeded columns and rows with missing status
final_data_incl <- final_data[, !(names(final_data) %in% c("imid", "DV1", "DV2", "DV1_excl", "DV2_excl"))]
final_data_incl <- final_data_incl[!is.na(final_data_incl$status), ]

# Check for missing data
print(colSums(is.na(final_data_incl)))

# Create dataset for VIF testing
model_data_incl <- cbind(
  ivs_final[, -which(names(ivs_final) == "imid")],
  status = final_data$status
)

# Multinomial model for VIF check
multinom_model_incl <- multinom(status ~ ., data = model_data_incl)
vif_values_incl <- vif(multinom_model_incl)
print(vif_values_incl)  # All should be under 5

# Define IV list
iv_vars <- c("Storing personal data securely","Removable device storage policy",
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

iv_vars <- paste0("`", iv_vars, "`")

# Fit multinomial logit model (reference: nonvictim)
ml_formula <- as.formula(paste("status ~", paste(iv_vars, collapse = " + ")))
ml_model <- multinom(ml_formula, data = final_data_incl, weights = weight)
print(ml_model)

# Calculate p-values
ml_coef <- summary(ml_model)$coefficients
ml_se <- summary(ml_model)$standard.errors
ml_z <- ml_coef / ml_se
ml_p <- 2 * (1 - pnorm(abs(ml_z)))
print(round(ml_p, 3))

# Compute odds ratios and 95% CIs
ml_or <- exp(ml_coef)
ci_lower <- exp(ml_coef - 1.96 * ml_se)  # Lower bound
ci_upper <- exp(ml_coef + 1.96 * ml_se)  # Upper bound

print(ml_or)
print(ci_lower)
print(ci_upper)

# Helper function to remove backticks
clean_name <- function(x) {
  gsub("`", "", x)
}

# Identify significant IVs (with p < 0.10) from your iv_vars.
iv_ml1 <- iv_vars[sapply(iv_vars, function(var) {
  # Clean the variable name from iv_vars
  var_clean <- clean_name(var)
  # Clean the column names of ml_p
  ml_p_names <- clean_name(colnames(ml_p))
  col_indices <- grep(var_clean, ml_p_names, value = FALSE)
  if (length(col_indices) == 0) return(FALSE)
  # Extract p-values for both "single" and "repeat" contrasts for these columns
  p_values <- ml_p[c("single", "repeat"), col_indices, drop = FALSE]
  return(any(p_values < 0.10, na.rm = TRUE))
})]

print(iv_ml1)

# DV1 model (selection) formula
bp1_formula <- as.formula(paste("DV1 ~", paste(iv_ml1, collapse = " + ")))

# Conduct the Second Multinomial Logit Model (ML2)
# Recode status to set 'single victims' as the reference category
final_data_incl2 <- final_data_incl %>%
  mutate(status2 = relevel(status, ref = "single")) %>%
  dplyr::select(-status)

ml_formula2 <- as.formula(paste("status2 ~", paste(iv_vars, collapse = " + ")))
ml_model2 <- multinom(ml_formula2, data = final_data_incl2, weights = weight)

ml_coef2 <- summary(ml_model2)$coefficients
ml_se2   <- summary(ml_model2)$standard.errors
ml_z2    <- ml_coef2 / ml_se2
ml_p2    <- 2 * (1 - pnorm(abs(ml_z2)))
print(round(ml_p2, 3))

ml_or <- exp(ml_coef2)
ci_lower <- exp(ml_coef2 - 1.96 * ml_se2)  # Lower bound
ci_upper <- exp(ml_coef2 + 1.96 * ml_se2)  # Upper bound

print (ml_or)
print(ci_lower)
print(ci_upper)

# Identify significant IVs in the "repeat vs single" contrast
iv_ml2 <- Filter(function(var) {
  any(ml_p2["repeat", grep(paste0("^", var), colnames(ml_p2))] < 0.10, na.rm = TRUE)
}, iv_vars)

print(iv_ml2)

bp2_formula <- as.formula(paste("DV2 ~", paste(iv_ml2, collapse = " + ")))

# Prepare dataset for BP model
bp_predictors <- unique(c(iv_ml1, iv_ml2))
bp_predictors <- gsub("`", "", bp_predictors)
print(bp_predictors)
bp_vars <- c("DV1", "DV2", "weight", bp_predictors)

bp_data <- final_data %>% dplyr::select(all_of(bp_vars))
bp_data <- bp_data[!is.na(bp_data$DV1), ]

# Run the bivariate probit model (DV1 = selection, DV2 = outcome)
biprobit_model <- selection(
  selection = bp1_formula,
  outcome   = bp2_formula,
  data      = bp_data,
  method    = "biprobit",
  weights   = bp_data$weight
)

summary(biprobit_model)

# Function to calculate average marginal effects for binary predictors
calc_mfx_binary <- function(model, data, predictors, part = "outcome", type = "unconditional") {
  # Step 1: Get baseline predictions
  pred_base <- predict(model, newdata = data, part = part, type = type)

  # Initialize vector to store marginal effects
  mfx <- numeric(length(predictors))
  names(mfx) <- predictors

  # Step 2: Loop through each binary predictor
  for (i in seq_along(predictors)) {
    data_mod <- data
    # Flip binary variable: 0 → 1 and 1 → 0
    data_mod[[predictors[i]]] <- 1 - data_mod[[predictors[i]]]

    # Predict counterfactual probability
    pred_new <- predict(model, newdata = data_mod, part = part, type = type)
    # Compute AME as average difference between counterfactual and baseline
    mfx[i] <- mean(pred_new - pred_base, na.rm = TRUE)
  }
  return(mfx)
}

# Bootstrap AMEs for binary predictors to compute standard errors and p-values
calc_mfx_bootstrap_binary <- function(model, data, predictors, part = "outcome", type = "unconditional", nboot = 1000) {
  # Step 1: Compute AME from original (full) sample
  orig_mfx <- calc_mfx_binary(model, data, predictors, part, type)
  # Step 2: Prepare to store bootstrap AMEs
  boot_mfx <- matrix(NA, nrow = nboot, ncol = length(predictors))
  colnames(boot_mfx) <- predictors

  set.seed(123)  # For reproducibility

  # Step 3: Run bootstrap loop
  for (i in 1:nboot) {
    boot_sample <- data[sample(nrow(data), replace = TRUE), ]
    boot_mfx[i, ] <- calc_mfx_binary(model, boot_sample, predictors, part, type)
  }

  # Step 4: Compute SEs, z-values, p-values
  se_mfx <- apply(boot_mfx, 2, sd, na.rm = TRUE)
  z_stats <- orig_mfx / se_mfx
  p_values <- 2 * (1 - pnorm(abs(z_stats)))

  # Step 5: Output AME table
  results <- data.frame(
    Predictor = predictors,
    MarginalEffect = orig_mfx,
    StdError = se_mfx,
    zValue = z_stats,
    pValue = p_values
  )
  return(results)
}

# Binary predictors from the selection equation (DV1: victimisation)
bp_predictors_selection <- c("Separate staff-visitor WiFi", "Restricting access", "Software protections",
                             "External audit", "Roles assigned to individuals after CS incidents",
                             "Admin or Real Estate", "Finance & Professional", "Small", "Medium & Large")

# Binary predictors from the outcome equation (DV2: repeat victimisation)
bp_predictors_outcome <- c("Software protections", "Construction, Hospitality & Agriculture")

# Run AME bootstrapping for selection equation (DV1)
bp_mfx_selection <- calc_mfx_bootstrap_binary(
  model = biprobit_model,
  data = bp_data,
  predictors = bp_predictors_selection,
  part = "selection", # part = DV1
  type = "response", # return response probabilities
  nboot = 1000
)

# Run AME bootstrapping for outcome equation (DV2)
bp_mfx_outcome <- calc_mfx_bootstrap_binary(
  model = biprobit_model,
  data = bp_data,
  predictors = bp_predictors_outcome,
  part = "outcome", # part = DV2
  type = "unconditional", # unconditional probabilities recommended for outcome
  nboot = 1000
)

# Add formatted p-values and significance stars to AME output table
add_sig_labels <- function(results_df) {
  results_df <- results_df %>%
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
        is.na(pValue)        ~ "",
        pValue < 0.001       ~ "***",
        pValue < 0.01        ~ "**",
        pValue < 0.05        ~ "*",
        pValue < 0.10        ~ ".",
        TRUE                 ~ ""
      )
    )
  return(results_df)
}

# Apply significance formatting to both outputs
bp_mfx_selection <- add_sig_labels(bp_mfx_selection)
bp_mfx_outcome   <- add_sig_labels(bp_mfx_outcome)

# Print formatted AMEs with SEs, p-values, and stars for both equations
print(bp_mfx_selection)
print(bp_mfx_outcome)

