# EXCLUDING PHISHING CASES

# DV1_excl
private_sector <- private_sector %>%
  mutate(
    DV1_excl = case_when(
      if_all(all_of(c(cybercrime_vars[-6], fraud_vars)), is.na) ~ NA_real_,
      rowSums(across(all_of(c(cybercrime_vars[-6], fraud_vars))), na.rm = TRUE) > 0 ~ 1,
      rowSums(across(all_of(c(cybercrime_vars[-6], fraud_vars))), na.rm = TRUE) == 0 ~ 0
    )
  )
table(private_sector$DV1_excl, useNA = "ifany")

# DV2_excl
private_sector <- private_sector %>%
  rowwise() %>%
  mutate(
    DV2_excl = recodeDV2(c_across(all_of(c(cybercrime_vars[-6], fraud_vars))), DV1_excl)
  ) %>%
  ungroup()
table(private_sector$DV2_excl, useNA = "ifany")

private_sector_excl <- private_sector %>%
  select(imid, DV1_excl, DV2_excl, weight)

final_data_excl <- private_sector_excl %>% left_join(ivs_final, by = "imid")

# Create 3-category DV status_excl
final_data_excl <- final_data_excl %>%
  mutate(status_excl = case_when(
    DV1_excl == 0 ~ "nonvictim",
    DV1_excl == 1 & DV2_excl == 0 ~ "single",
    DV1_excl == 1 & DV2_excl == 1 ~ "repeat",
    TRUE ~ NA_character_
  ))

final_data_excl$status_excl <- factor(final_data_excl$status_excl,
                                      levels = c("nonvictim", "single", "repeat"))

final_data_excl_cc <- final_data_excl %>%
  filter(!is.na(status_excl))

print(colSums(is.na(final_data_excl_cc)))

# VIF check
model_data_excl_vif <- cbind(
  ivs_final[, -which(names(ivs_final) == "imid")],
  status_excl = final_data_excl$status_excl
)

multinom_model_excl_vif <- multinom(status_excl ~ ., data = model_data_excl_vif)

vif_values_excl <- vif(multinom_model_excl_vif)
print(vif_values_excl)

iv_vars_excl <- c(
  "Storing personal data securely","Removable device storage policy",
  "Separate staff-visitor WiFi","Cloud & digital services policy",
  "Restricting access","Remote & personal device policy","VPN for remote working",
  "Two-factor authentication","Tools for security monitoring","Software protections",
  "Self-protective cyber behaviors","Board members on CS",
  "Internal audit","External audit","Roles assigned to individuals after CS incidents",
  "Security controls & monitoring","Outsourced provider that manages CS",
  "Admin or Real Estate","Construction, Hospitality & Agriculture",
  "Education, Entertainment & Health","Finance & Professional",
  "Info or Communication", "Transport & Utilities","Small","Medium & Large"
)

iv_vars_excl_bt <- paste0("`", iv_vars_excl, "`")
ml_formula_excl <- as.formula(paste("status_excl ~", paste(iv_vars_excl_bt, collapse = " + ")))
ml_model_excl <- multinom(ml_formula_excl, data = final_data_excl_cc, weights = weight)

ml_coef_excl <- summary(ml_model_excl)$coefficients
ml_se_excl   <- summary(ml_model_excl)$standard.errors
ml_z_excl    <- ml_coef_excl / ml_se_excl
ml_p_excl    <- 2 * (1 - pnorm(abs(ml_z_excl)))

ml_or_excl <- exp(ml_coef_excl)
ci_lower_excl <- exp(ml_coef_excl - 1.96 * ml_se_excl)
ci_upper_excl <- exp(ml_coef_excl + 1.96 * ml_se_excl)

print(ml_or_excl)
print(ci_lower_excl)
print(ci_upper_excl)

clean_name <- function(x) gsub("`", "", x)

ml1_cols_clean <- clean_name(colnames(ml_p_excl))

is_sig_ml1 <- function(var_bt) {
  v <- clean_name(var_bt)
  j <- which(ml1_cols_clean == v)
  if (length(j) != 1) return(FALSE)
  any(ml_p_excl[c("single","repeat"), j] < 0.10, na.rm = TRUE)
}

iv_ml1_excl <- iv_vars_excl_bt[sapply(iv_vars_excl_bt, is_sig_ml1)]
print(iv_ml1_excl)

bp1_formula_excl <- as.formula(paste("DV1_excl ~", paste(iv_ml1_excl, collapse = " + ")))


final_data_excl_cc2 <- final_data_excl_cc %>%
  mutate(status_excl2 = relevel(status_excl, ref = "single"))

ml_formula2_excl <- as.formula(paste("status_excl2 ~", paste(iv_vars_excl_bt, collapse = " + ")))
ml_model2_excl <- multinom(ml_formula2_excl, data = final_data_excl_cc2, weights = weight)


ml_coef2_excl <- summary(ml_model2_excl)$coefficients
ml_se2_excl   <- summary(ml_model2_excl)$standard.errors
ml_z2_excl    <- ml_coef2_excl / ml_se2_excl
ml_p2_excl    <- 2 * (1 - pnorm(abs(ml_z2_excl)))

ml_or2_excl <- exp(ml_coef2_excl)
ci_lower2_excl <- exp(ml_coef2_excl - 1.96 * ml_se2_excl)
ci_upper2_excl <- exp(ml_coef2_excl + 1.96 * ml_se2_excl)

print(ml_or2_excl)
print(ci_lower2_excl)
print(ci_upper2_excl)

ml2_cols_clean <- clean_name(colnames(ml_p2_excl))

is_sig_ml2_repeat <- function(var_bt) {
  v <- clean_name(var_bt)
  j <- which(ml2_cols_clean == v)
  if (length(j) != 1) return(FALSE)
  ml_p2_excl["repeat", j] < 0.10
}

iv_ml2_excl <- iv_vars_excl_bt[sapply(iv_vars_excl_bt, is_sig_ml2_repeat)]

print(iv_ml2_excl)

bp2_formula_excl <- as.formula(paste("DV2_excl ~", paste(iv_ml2_excl, collapse = " + ")))

# BP model
bp_predictors_excl <- unique(c(iv_ml1_excl, iv_ml2_excl))
bp_predictors_excl <- clean_name(bp_predictors_excl)

bp_vars_excl <- c("DV1_excl", "DV2_excl", "weight", bp_predictors_excl)

bp_data_excl <- final_data_excl %>% select(all_of(bp_vars_excl))
bp_data_excl <- bp_data_excl[!is.na(bp_data_excl$DV1_excl), ]

biprobit_model_excl <- selection(
  selection = bp1_formula_excl,
  outcome   = bp2_formula_excl,
  data      = bp_data_excl,
  method    = "biprobit",
  weights   = bp_data_excl$weight
)

print(summary(biprobit_model_excl))

# AME calculations (exclude phishing)
mf_excl <- model.frame(biprobit_model_excl)
bp_data_ame_excl <- bp_data_excl[rownames(mf_excl), , drop = FALSE]

# Dummy variable groups
sector_dummies <- c(
  "Admin or Real Estate",
  "Construction, Hospitality & Agriculture",
  "Education, Entertainment & Health",
  "Finance & Professional",
  "Info or Communication",
  "Transport & Utilities"
)

size_dummies <- c("Small", "Medium & Large")

set_group_to <- function(dat, group_vars, target) {
  dat[group_vars] <- 0
  dat[[target]] <- 1
  dat
}

extract_p1 <- function(p) {
  if (!is.matrix(p) && !is.data.frame(p)) return(as.numeric(p))

  p <- as.matrix(p)
  if (ncol(p) == 1) return(as.numeric(p[, 1]))

  cn <- colnames(p)
  if (!is.null(cn)) {
    cn_low <- tolower(cn)

    hit <- which(cn_low %in% c("1", "pr(1)", "p(1)", "y=1", "prob1", "class1", "true", "yes"))
    if (length(hit) == 1) return(as.numeric(p[, hit]))

    hit2 <- which(grepl("1", cn_low, fixed = TRUE) & !grepl("0", cn_low, fixed = TRUE))
    if (length(hit2) == 1) return(as.numeric(p[, hit2]))
  }

  as.numeric(p[, ncol(p)])
}

# Calculate AMEs as weighted average differences in predicted probabilities
calc_ame_excl <- function(model, data, predictors, part,
                          weight_var = "weight",
                          cond_var = "DV1_excl",
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
      return(extract_p1(p))
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
bp_selection_excl <- bp_predictors_excl
bp_outcome_excl <- clean_name(iv_ml2_excl)

# Point-estimate AMEs
ame_selection_excl <- calc_ame_excl(
  model = biprobit_model_excl,
  data = bp_data_ame_excl,
  predictors = bp_selection_excl,
  part = "selection",
  weight_var = "weight"
)

ame_repeat_excl <- calc_ame_excl(
  model = biprobit_model_excl,
  data = bp_data_ame_excl,
  predictors = bp_outcome_excl,
  part = "outcome",
  weight_var = "weight"
)

# Bootstrap
boot_ame_excl <- function(data_est, predictors, part, weight_var = "weight") {
  idx <- sample.int(nrow(data_est), nrow(data_est), replace = TRUE)
  d_boot <- data_est[idx, , drop = FALSE]

  tryCatch({
    boot_model <- selection(
      selection = bp1_formula_excl,
      outcome   = bp2_formula_excl,
      data      = d_boot,
      method    = "biprobit",
      weights   = d_boot[[weight_var]]
    )

    calc_ame_excl(
      model = boot_model,
      data = d_boot,
      predictors = predictors,
      part = part,
      weight_var = weight_var
    )

  }, error = function(e) {
    setNames(rep(NA_real_, length(predictors)), predictors)
  })
}

run_bootstrap_ame_excl <- function(B, data_est, predictors, part,
                                   weight_var = "weight") {
  m <- matrix(NA_real_, nrow = B, ncol = length(predictors))
  colnames(m) <- predictors

  for (b in seq_len(B)) {
    m[b, ] <- boot_ame_excl(data_est, predictors, part, weight_var)
  }

  m
}

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

# Bootstrap SEs
set.seed(123)
B <- 500

boot_sel_excl <- run_bootstrap_ame_excl(
  B = B,
  data_est = bp_data_ame_excl,
  predictors = bp_selection_excl,
  part = "selection",
  weight_var = "weight"
)

boot_cond_excl <- run_bootstrap_ame_excl(
  B = B,
  data_est = bp_data_ame_excl,
  predictors = bp_outcome_excl,
  part = "outcome",
  weight_var = "weight"
)

# Final AME tables
selection_ame_excl <- summarise_ame(ame_selection_excl, boot_sel_excl)
outcome_ame_excl <- summarise_ame(ame_repeat_excl, boot_cond_excl)

selection_ame_excl
outcome_ame_excl
