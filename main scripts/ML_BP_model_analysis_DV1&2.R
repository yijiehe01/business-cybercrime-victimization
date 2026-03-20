library(survey)
library(dplyr)
library(ggplot2)
library(haven)
library(nnet)
library(sampleSelection)
library(here)
library(car)

data <- here("data", "csbs_2024_archive_data_public.sav")

# Load csbs dataset
csbs2024 <- read_sav(data)

# Subset to private-sector businesses only
private_sector <- csbs2024 %>% filter(questtype == 1)

# Extract all IVs (the unique ID “imid” extracted for merging later)
ivs <- private_sector[, c("imid", "sector_comb2", "sizeb", "rules15", "policy1", "rules9", "policy5",
                          "policy13", "rules4", "rules8", "policy2", "policy4", "rules18",
                          "rules20", "ident11", "rules1", "rules2", "rules3", "ident14",
                          "rules17", "rules19", "trained", "manage1", "audit",
                          "incidcontent3", "rules5", "rules7", "manage2")]

# Rename IVs
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

# Recode design-missing values as 0
ivs <- ivs %>%
  mutate(across(c("Removable device storage policy", "Cloud computing policy",
                  "Digital service providers policy", "Remote or mobile working policy",
                  "Use of personally-own devices policy", "CS audit"),
                ~ ifelse(is.na(.), 0, .))) %>%
  # recode values equal to -97 as NA
  mutate(across(-imid, ~ ifelse(. == -97, NA_real_, .)))

# Aggregate the three technical self-protection variables
ivs <- ivs %>%
  mutate(`Software protections` = case_when(
    `Regular software security updates` == 1 &
      `Up-to-date malware protection` == 1 &
      `Firewall` == 1 ~ 1, 
    TRUE ~ 0
  )) %>%
  select(-c(`Regular software security updates`, `Up-to-date malware protection`, `Firewall`))

# Aggregate the two accessibility-related variables
ivs <- ivs %>%
  mutate(`Restricting access` = case_when(
    `Restricting access rights` == 1 & `Allowing access only via company-owned devices` == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  select(-c(`Restricting access rights`, `Allowing access only via company-owned devices`))

# Convert the "trained" variable into (0/1)
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
 select(-c(`Cloud computing policy`, `Digital service providers policy`))

# Aggregate two accessibility-related variables
ivs <- ivs %>%
  mutate(`Remote & personal device policy` = case_when(
    `Remote or mobile working policy` == 1 & `Use of personally-own devices policy` == 1 ~ 1,
    TRUE ~ 0
  ))%>%
  select(-c(`Remote or mobile working policy`, `Use of personally-own devices policy`))

# Aggregate two internal guardianship variables
ivs <- ivs %>%
  mutate(`Security controls & monitoring` = case_when(
    `Security controls on company-owned devices` == 1 & `Monitoring of user activity` == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  select(-c(`Security controls on company-owned devices`, `Monitoring of user activity`))

# Aggregate the four personal self-protection variables
ivs <- ivs %>%
  mutate(`Self-protective cyber behaviors` = case_when(
    rowSums(across(c(
      `Testing staff CS awareness`,
      `CS trainings or awareness sessions`,
      `Guidance strong passwords`,
      `Process for reporting fraudulent emails or websites`
    )) == 1, na.rm = TRUE) >= 3 ~ 1,
    TRUE ~ 0
  )) %>%
  select(-c(
    `Testing staff CS awareness`,
    `CS trainings or awareness sessions`,
    `Guidance strong passwords`,
    `Process for reporting fraudulent emails or websites`
  ))

# Create internal and external audits
dummy_audit <- data.frame(
  `Internal audit` = ifelse(ivs$`CS audit` %in% c(1, 3), 1,
                            ifelse(ivs$`CS audit` == 2, 0, 0)),
  `External audit` = ifelse(ivs$`CS audit` %in% c(2, 3), 1,
                            ifelse(ivs$`CS audit` == 1, 0, 0)),
  check.names = FALSE
)

ivs$`Business sectors` <- as.character(ivs$`Business sectors`)

# Aggregate sectors
ivs$`Business sectors`[ivs$`Business sectors` %in% c("2", "6", "13")] <- "Construction, Hospitality & Agriculture"
ivs$`Business sectors`[ivs$`Business sectors` %in% c("3", "4", "7")]  <- "Education, Entertainment & Health"
ivs$`Business sectors`[ivs$`Business sectors` %in% c("5", "9")]      <- "Finance & Professional"
ivs$`Business sectors`[ivs$`Business sectors` %in% c("11", "12")]    <- "Transport & Utilities"

# keep remaining single sectors
ivs$`Business sectors`[ivs$`Business sectors` == "1"]  <- "Admin or Real Estate"
ivs$`Business sectors`[ivs$`Business sectors` == "8"]  <- "Info or Communication"
ivs$`Business sectors`[ivs$`Business sectors` == "10"] <- "Retail or Wholesale"

# set "Retail_Wholesale" as reference group
ivs$`Business sectors` <- factor(ivs$`Business sectors`, levels = c(
  "Retail or Wholesale", "Admin or Real Estate", "Construction, Hospitality & Agriculture",
  "Education, Entertainment & Health", "Finance & Professional", "Info or Communication", "Transport & Utilities"
))

# Create dummy variables
dummy_sector <- model.matrix(~ `Business sectors` - 1, data = ivs)

colnames(dummy_sector) <- gsub(
  "^`Business sectors`|^Business\\.sectors",  
  "",                                         
  colnames(dummy_sector)
)

ivs$`Business sizes` <- factor(ivs$`Business sizes`, levels = c(1, 2, 3, 4))

# Aggregate 'Medium' and 'Large' into 'Medium & Large'
levels(ivs$`Business sizes`) <- c("Micro", "Small", "Medium & Large", "Medium & Large")

# Generate dummy variables
dummy_sizeb <- model.matrix(~ `Business sizes` - 1, data = ivs)

colnames(dummy_sizeb) <- gsub(
  "^`Business sizes`|^Business\\.sizes",
  "",
  colnames(dummy_sizeb)
)

# Combine all selected IVs into the final IV dataset
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
  dummy_sector[, -1],
  dummy_sizeb[, -1]
)

# Dependent variables
cybercrime_vars <- c("ranssoft", "hacksiv", "tkvrsuc", "dossoft", "virussoft", "phisheng")
fraud_vars <- c("fraud1", "fraud2", "fraud3", "fraud4", "fraudconta",
                "fraudcontb", "fraudcontc", "fraudcontd", "fraudconte",
                "fraudcontf", "fraudcontg", "fraudconth", "fraudconti")

# Convert missing values into NA
private_sector <- private_sector %>%
  mutate(across(all_of(c(cybercrime_vars, fraud_vars)),
                ~ case_when(
                  . == -97 ~ NA_real_,  # "Don't know"
                  . == -99 ~ NA_real_,  # "Refused"
                  TRUE     ~ .
                )))

# DV1: Victimisation
private_sector <- private_sector %>%
  mutate(
    DV1 = case_when(
      if_all(all_of(c(cybercrime_vars, fraud_vars)), is.na) ~ NA_real_,
      rowSums(across(all_of(c(cybercrime_vars, fraud_vars))), na.rm = TRUE) > 0 ~ 1,
      rowSums(across(all_of(c(cybercrime_vars, fraud_vars))), na.rm = TRUE) == 0 ~ 0
    )
  )

table(private_sector$DV1, useNA = "ifany")

# DV2: Repeat victimisation
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

table(private_sector$DV2, useNA = "ifany")

# Merge DV1, DV2, and weights with IVs
private_sel <- private_sector %>%
  select(imid, DV1,DV2, weight)
final_data <- private_sel %>% left_join(ivs_final, by = "imid")

# create the three-category DV
final_data <- final_data %>%
  mutate(status = case_when(
    DV1 == 0 ~ "nonvictim",
    DV1 == 1 & DV2 == 0 ~ "single",
    DV1 == 1 & DV2 == 1 ~ "repeat"
  ))

final_data$status <- factor(final_data$status, levels = c("nonvictim", "single", "repeat"))

# Drop missingness
final_data_incl <- final_data[, !(names(final_data) %in% c("imid", "DV1", "DV2", "DV1_excl", "DV2_excl"))]
final_data_incl <- final_data_incl[!is.na(final_data_incl$status), ]

print(colSums(is.na(final_data_incl)))

# VIF testing
model_data_incl <- cbind(
  ivs_final[, -which(names(ivs_final) == "imid")],
  status = final_data$status
)

multinom_model_incl <- multinom(status ~ ., data = model_data_incl)
vif_values_incl <- vif(multinom_model_incl)
print(vif_values_incl)

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

# Fit first ML model (reference: nonvictim)
ml_formula <- as.formula(paste("status ~", paste(iv_vars, collapse = " + ")))
ml_model <- multinom(ml_formula, data = final_data_incl, weights = weight)
print(ml_model)

# Calculate p-values, ORs, 95% CIs
ml_coef <- summary(ml_model)$coefficients
ml_se <- summary(ml_model)$standard.errors
ml_z <- ml_coef / ml_se
ml_p <- 2 * (1 - pnorm(abs(ml_z)))
print(round(ml_p, 3))

ml_or <- exp(ml_coef)
ci_lower <- exp(ml_coef - 1.96 * ml_se)
ci_upper <- exp(ml_coef + 1.96 * ml_se)

print(ml_or)
print(ci_lower)
print(ci_upper)

clean_name <- function(x) {
  gsub("`", "", x)
}

# Identify significant IVs (with p < 0.10)
iv_ml1 <- iv_vars[sapply(iv_vars, function(var) {
  var_clean <- clean_name(var)
  ml_p_names <- clean_name(colnames(ml_p))
  col_indices <- grep(var_clean, ml_p_names, value = FALSE)
  if (length(col_indices) == 0) return(FALSE)
  p_values <- ml_p[c("single", "repeat"), col_indices, drop = FALSE]
  return(any(p_values < 0.10, na.rm = TRUE))
})]

print(iv_ml1)

# BP selection formula
bp1_formula <- as.formula(paste("DV1 ~", paste(iv_ml1, collapse = " + ")))

# fit second ML Model (reference: single victim)
final_data_incl2 <- final_data_incl %>%
  mutate(status2 = relevel(status, ref = "single")) %>%
  select(-status)

ml_formula2 <- as.formula(paste("status2 ~", paste(iv_vars, collapse = " + ")))
ml_model2 <- multinom(ml_formula2, data = final_data_incl2, weights = weight)

ml_coef2 <- summary(ml_model2)$coefficients
ml_se2   <- summary(ml_model2)$standard.errors
ml_z2    <- ml_coef2 / ml_se2
ml_p2    <- 2 * (1 - pnorm(abs(ml_z2)))
print(round(ml_p2, 3))

ml_or <- exp(ml_coef2)
ci_lower <- exp(ml_coef2 - 1.96 * ml_se2)
ci_upper <- exp(ml_coef2 + 1.96 * ml_se2)

print (ml_or)
print(ci_lower)
print(ci_upper)

# Identify significant IVs in the "repeat vs single" contrast
iv_ml2 <- Filter(function(var) {
  any(ml_p2["repeat", grep(paste0("^", var), colnames(ml_p2))] < 0.10, na.rm = TRUE)
}, iv_vars)

print(iv_ml2)

# BP outcome formula
bp2_formula <- as.formula(paste("DV2 ~", paste(iv_ml2, collapse = " + ")))

# Prepare dataset for BP model
bp_predictors <- unique(c(iv_ml1, iv_ml2))
bp_predictors <- gsub("`", "", bp_predictors)
print(bp_predictors)
bp_vars <- c("DV1", "DV2", "weight", bp_predictors)

bp_data <- final_data %>% select(all_of(bp_vars))
bp_data <- bp_data[!is.na(bp_data$DV1), ]

# Run the BP model
biprobit_model <- selection(
  selection = bp1_formula,
  outcome   = bp2_formula,
  data      = bp_data,
  method    = "biprobit",
  weights   = bp_data$weight
)

summary(biprobit_model)

# AME calculations
mf <- model.frame(biprobit_model)
bp_data_ame <- bp_data[rownames(mf), , drop = FALSE]

# Dummy variables groups
sector_dummies <- c(
  "Admin or Real Estate", "Construction, Hospitality & Agriculture",
  "Education, Entertainment & Health", "Finance & Professional",
  "Info or Communication", "Transport & Utilities"
)

size_dummies <- c("Small", "Medium & Large")

set_group_to <- function(dat, group_vars, target) {
  dat[group_vars] <- 0
  dat[[target]] <- 1
  dat
}

# Calculate AMEs as weighted average differences in predicted probabilities
calc_ame <- function(model, data, predictors, part,
                     weight_var = "weight",
                     cond_var = "DV1",
                     sector_group = sector_dummies,
                     size_group = size_dummies) {
  if (part == "outcome") {
    data <- data[data[[cond_var]] == 1 & !is.na(data[[cond_var]]), , drop = FALSE]
  }

  w <- as.numeric(data[[weight_var]])
  ok_w <- !is.na(w) & is.finite(w) & w > 0

  get_p <- function(newdata) {
    if (part == "selection") {
      return(as.numeric(
        predict(model, newdata = newdata, part = "selection", type = "response")
      ))
    } else {
      p <- predict(model, newdata = newdata, part = "outcome", type = "conditional")

      if (is.matrix(p) || is.data.frame(p)) {
        p <- as.matrix(p)
        if (ncol(p) == 1) return(as.numeric(p[, 1]))
        return(as.numeric(p[, ncol(p)]))
      }

      return(as.numeric(p))
    }
  }

  ame <- setNames(rep(NA_real_, length(predictors)), predictors)

  for (v in predictors) {
    if (!v %in% names(data)) next

    d0 <- data
    d1 <- data

    if (v %in% sector_group) {
      d1 <- set_group_to(d1, sector_group, v)
    } else if (v %in% size_group) {
      d1 <- set_group_to(d1, size_group, v)
    } else {
      d1[[v]] <- 1
      d0[[v]] <- 0
    }

    diff <- get_p(d1) - get_p(d0)
    ok <- !is.na(diff) & is.finite(diff) & ok_w

    if (isTRUE(any(ok)) && sum(w[ok], na.rm = TRUE) > 0) {
      ame[v] <- sum(diff[ok] * w[ok], na.rm = TRUE) / sum(w[ok], na.rm = TRUE)
    }
  }

  ame
}

# Predictors
bp_selection <- c("Separate staff-visitor WiFi", "Restricting access", "Software protections",
  "External audit", "Roles assigned to individuals after CS incidents",
  "Admin or Real Estate", "Finance & Professional", "Small", "Medium & Large")

bp_outcome <- c("Software protections", "Construction, Hospitality & Agriculture")

# Point-estimate AMEs
ame_selection <- calc_ame(
  model = biprobit_model,
  data = bp_data_ame,
  predictors = bp_selection,
  part = "selection",
  weight_var = "weight"
)

ame_repeat <- calc_ame(
  model = biprobit_model,
  data = bp_data_ame,
  predictors = bp_outcome,
  part = "outcome",
  weight_var = "weight"
)

# bootstrap
boot_ame <- function(data_est, predictors, part, weight_var = "weight") {
  idx <- sample.int(nrow(data_est), nrow(data_est), replace = TRUE)
  d_boot <- data_est[idx, , drop = FALSE]

  tryCatch({
    boot_model <- selection(
      selection = bp1_formula,
      outcome   = bp2_formula,
      data      = d_boot,
      method    = "biprobit",
      weights   = d_boot[[weight_var]]
    )

    calc_ame(boot_model, d_boot, predictors, part, weight_var)

  }, error = function(e) {
    setNames(rep(NA_real_, length(predictors)), predictors)
  })
}

bootstrap <- function(B, data_est, predictors, part,
                              weight_var = "weight") {
  m <- matrix(NA_real_, nrow = B, ncol = length(predictors))
  colnames(m) <- predictors

  for (b in seq_len(B)) {
    m[b, ] <- boot_ame(data_est, predictors, part, weight_var)
  }

  m
}

# Combine point estimates with bootstrap standard errors and p-values
summarise_ame <- function(ame_point, boot_matrix) {
  se <- apply(boot_matrix, 2, sd, na.rm = TRUE)
  se[se == 0] <- NA_real_

  z <- ame_point / se
  p <- 2 * pnorm(-abs(z))

  stars <- cut(
    p,
    breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
    labels = c("***", "**", "*", ".", "")
  )

  data.frame(
    predictor = names(ame_point),
    ame = as.numeric(ame_point),
    se = se,
    z = z,
    p = p,
    stars = stars,
    row.names = NULL
  )
}

# Bootstrap AME standard errors
set.seed(123)
B <- 500

boot_sel <- bootstrap(
  B = B,
  data_est = bp_data_ame,
  predictors = bp_selection,
  part = "selection",
  weight_var = "weight"
)

boot_cond <- bootstrap(
  B = B,
  data_est = bp_data_ame,
  predictors = bp_outcome,
  part = "outcome",
  weight_var = "weight"
)

# Final AME tables
selection_ame <- summarise_ame(ame_selection, boot_sel)
outcome_ame <- summarise_ame(ame_repeat, boot_cond)

selection_ame
outcome_ame

