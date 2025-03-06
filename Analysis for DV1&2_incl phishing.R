install.packages(c("survey", "dplyr", "ggplot2", "haven", "nnet", "sampleSelection", "here"))
library(survey)
library(dplyr)
library(ggplot2)
library(haven)
library(nnet)             # For multinomial logistic regression
library(sampleSelection)  # For bivariate probit (selection) models
library(here)             # For relative file paths

# -------------------------------
# Load and Filter Data
# -------------------------------
csbs2024 <- read_sav(here("csbs_2024_archive_data_public.sav"))

# keep only private sector businesses
private_sector <- csbs2024 %>% filter(questtype == 1)

# -------------------------------
# Define Cybercrime and Fraud Variables (DVs)
# -------------------------------
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

# -------------------------------
# Create DV1: the Likelihood of Victimization (binary)
# -------------------------------
private_sector <- private_sector %>%
  mutate(
    DV1_incl = case_when(
      if_all(all_of(c(cybercrime_vars, fraud_vars)), is.na) ~ NA_real_,
      rowSums(across(all_of(c(cybercrime_vars, fraud_vars))), na.rm = TRUE) > 0 ~ 1,
      rowSums(across(all_of(c(cybercrime_vars, fraud_vars))), na.rm = TRUE) == 0 ~ 0
    ),
    DV1_excl = case_when(
      if_all(all_of(c(cybercrime_vars[-6], fraud_vars)), is.na) ~ NA_real_,
      rowSums(across(all_of(c(cybercrime_vars[-6], fraud_vars))), na.rm = TRUE) > 0 ~ 1,
      rowSums(across(all_of(c(cybercrime_vars[-6], fraud_vars))), na.rm = TRUE) == 0 ~ 0
    )
  )

# Check the distribution of DV1
table(private_sector$DV1_incl, useNA = "ifany") # include phishing
table(private_sector$DV1_excl, useNA = "ifany") # exclude phishing as phishing is the most common attack

# -------------------------------
# Create DV2: the Likelihoof of Repeat Victimization (binary)
# -------------------------------
recodeDV2 <- function(vars, dv1) {
  if (is.na(dv1)) return(NA)
  if (dv1 == 0) return(NA)
  rep_flag <- any(vars > 1, na.rm = TRUE)
  return(ifelse(rep_flag, 1, 0))
}

private_sector <- private_sector %>%
  rowwise() %>%
  mutate(
    DV2_incl = recodeDV2(c_across(all_of(c(cybercrime_vars, fraud_vars))), DV1_incl),
    DV2_excl = recodeDV2(c_across(all_of(c(cybercrime_vars[-6], fraud_vars))), DV1_excl)
  ) %>%
  ungroup()

# Check the distribution of DV2
table(private_sector$DV2_incl, useNA = "ifany") # include phishing
table(private_sector$DV2_excl, useNA = "ifany") # exclude phishing

# -------------------------------
# Create DV3: the Number ofMultiple Victimization (Count)
# -------------------------------
# Function to calculate the number of distinct cyber breaches/attacks
recodeDV3 <- function(cyber_vars, fraud_vars) {
  if (all(is.na(c(cyber_vars, fraud_vars)))) {
    return(NA)  # If all values are NA, return NA
  }

  # Count unique cybercrime types (each cybercrime type is treated separately)
  cyber_count <- sum(cyber_vars > 0, na.rm = TRUE)  # Count distinct cybercrime events

  # Check if any fraud type is experienced, all frauds are seen as one general type of cyber-faciliated fraud
  fraud_flag <- any(fraud_vars > 0, na.rm = TRUE)  # If any fraud is experienced, count as 1

  # Total number of distinct cybercrime types
  total_count <- cyber_count + fraud_flag

  return(total_count)
}

private_sector <- private_sector %>%
  rowwise() %>%
  mutate(
    DV3_incl = recodeDV3(c_across(all_of(cybercrime_vars)), c_across(all_of(fraud_vars))),     # include phishing
    DV3_excl = recodeDV3(c_across(all_of(cybercrime_vars[-6])), c_across(all_of(fraud_vars)))  # exclude phishing
  ) %>%
  ungroup()

# Check the distribution of DV3
table(private_sector$DV3_incl, useNA = "ifany")
table(private_sector$DV3_excl, useNA = "ifany")

# -------------------------------
# Merge Independent Variables (IVs) with DVs, weight
# -------------------------------
# the file VIF Measurement for IVs need to be preloaded here
private_sel <- private_sector %>% select(imid, DV1_incl, DV1_excl, DV2_incl, DV2_excl, weight)
final_data <- private_sel %>% left_join(ivs_final, by = "imid")

# -------------------------------
# Create the Combined Three-Category DV for Multinomial Logit Model (base category = nonvictim)
# -------------------------------
final_data <- final_data %>%
  mutate(status_incl = case_when(
    DV1_incl == 0 ~ "nonvictim",
    DV1_incl == 1 & DV2_incl == 0 ~ "single",
    DV1_incl == 1 & DV2_incl == 1 ~ "repeat"
  ))
final_data$status_incl <- factor(final_data$status_incl, levels = c("nonvictim", "single", "repeat"))
final_data_incl <- final_data[, !(names(final_data) %in% c("imid", "DV1_incl", "DV2_incl", "DV1_excl", "DV2_excl"))]

# -------------------------------
# Conduct the First Multinomial Logit Model (ML1)
# -------------------------------
final_data_incl <- final_data_incl[!is.na(final_data_incl$status_incl), ]

iv_vars <- c("rules15", "policy1", "rules9", "policy5", "policy13",
             "rules4", "rules8", "policy2", "policy4", "rules18", "rules20",
             "ident11", "rules1", "rules2", "rules3", "ident14", "rules17", "rules19",
             "trained", "info_comb4", "manage1", "incidcontent3", "rules5", "rules7",
             "info_comb8", "manage2", "internal_audit", "external_audit",
             "sector_comb21","sector_comb22", "sector_comb23","sector_comb24", "sector_comb25",
             "sector_comb26", "sector_comb27", "sector_comb28", "sector_comb29",
             "sector_comb211", "sector_comb212", "sector_comb213",
             "sizeb2", "sizeb3", "sizeb4")

ml_formula <- as.formula(paste("status_incl ~", paste(iv_vars, collapse = " + ")))
ml_model <- multinom(ml_formula, data = final_data_incl, weights = weight)
summary(ml_model)

# Extract p-values for filtering significant predictors for BP1
ml_coef <- summary(ml_model)$coefficients
ml_se <- summary(ml_model)$standard.errors
ml_z <- ml_coef / ml_se
ml_p <- 2 * (1 - pnorm(abs(ml_z)))
print(round(ml_p, 3))

# Identify IVs significant at p < 0.2 for BP1
iv_ml1 <- iv_vars[sapply(iv_vars, function(var) {
  any(ml_p[, grep(paste0("^", var), colnames(ml_p))] < 0.2, na.rm = TRUE)
})]
print(iv_ml1)

# -------------------------------
# Build Bivariate Probit Model (BP1) Equation using DV1_incl
# -------------------------------
bp1_formula <- as.formula(paste("DV1_incl ~", paste(iv_ml1, collapse = " + ")))

# -------------------------------
# Conduct the Second Multinomial Logit Model (ML2) (based category = single)
# -------------------------------
# Recode status to set 'single' as the reference category
final_data_incl2 <- final_data_incl %>%
  mutate(status_incl2 = relevel(status_incl, ref = "single")) %>%
  select(-status_incl)

# estimate ML2 model
ml_formula2 <- as.formula(paste("status_incl2 ~", paste(iv_vars, collapse = " + ")))
ml_model2 <- multinom(ml_formula2, data = final_data_incl2, weights = weight)
summary(ml_model2)

# Extract p-values for filtering significant predictors for BP2
ml_coef2 <- summary(ml_model2)$coefficients
ml_se2   <- summary(ml_model2)$standard.errors
ml_z2    <- ml_coef2 / ml_se2
ml_p2    <- 2 * (1 - pnorm(abs(ml_z2)))
print(round(ml_p2, 3))

# Identify IVs significant at p < 0.2 for BP2
iv2 <- Filter(function(var) {
  any(ml_p2["repeat", grep(paste0("^", var), colnames(ml_p2))] < 0.2, na.rm = TRUE)
}, iv_vars)

print(iv2)

# -------------------------------
# Build Bivariate Probit Model (BP2) Equation using DV2_incl
# -------------------------------
bp2_formula <- as.formula(paste("DV2_incl ~", paste(iv_ml2, collapse = " + ")))

# -------------------------------
# Conduct Final Bivariate Probit Model (BP1 + BP2) - DV Including Phishing
# -------------------------------
# Prepare dataset for BP model
bp_predictors <- unique(c(iv_ml1, iv_ml2))
bp_vars <- c("DV1_incl", "DV2_incl", "weight", bp_predictors)

bp_data <- final_data %>% select(all_of(bp_vars))
bp_data <- bp_data[!is.na(bp_data$DV1_incl), ]

# Estimate bivariate probit model
biprobit_model <- selection(
  selection = bp1_formula,
  outcome   = bp2_formula,
  data      = bp_data,
  method    = "biprobit",
  weights   = bp_data$weight
)

# -------------------------------
# Model Summaries & Fit Statistics
# -------------------------------
summary(biprobit_model)

AIC(biprobit_model)
BIC(biprobit_model)
