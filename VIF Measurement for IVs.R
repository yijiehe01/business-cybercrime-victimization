# -------------------------------
# Step 1: Convert Independent Variables into Correct Forms
# -------------------------------

# extract all IVs for the study (excluding the unique ID “imid” variable which is only for merging later)
ivs <- private_sector[, c("imid", "sector_comb2", "sizeb", "rules15", "policy1", "rules9", "policy5",
                              "policy13", "rules4", "rules8", "policy2", "policy4", "rules18",
                              "rules20", "ident11", "rules1", "rules2", "rules3", "ident14",
                              "rules17", "rules19", "trained", "info_comb4", "manage1", "audit",
                              "incidcontent3", "rules5", "rules7", "info_comb8", "manage2")]


# --- Check Missing Data ---
# Count how many missing values are in each IV (except imid)
missing_counts <- colSums(is.na(ivs))
print(missing_counts[missing_counts > 0])

# --- Recode Missing Values ---
# for "policy1", "policy5", "policy13", "policy2", "policy4", and "audit", as their NA is due to the survey design so recode as 0
# keep info_comb8 and info_comb4, audit NA as NA
# then recode any instance of -97 (Don't Know) as NA
ivs <- ivs %>%
  mutate(across(c("policy1", "policy5", "policy13", "policy2", "policy4", "audit"),
                ~ ifelse(is.na(.), 0, .))) %>%
  # Then, recode values equal to -97 as NA
  mutate(across(-imid, ~ ifelse(. == -97, NA_real_, .)))

# Check the missing value counts after recoding
print(colSums(is.na(ivs)))

# -------------------------------
# Continue with IV Pre-Processing
# -------------------------------
# Convert the "trained" variable into binary format like other IVs
ivs$trained <- case_when(
  ivs$trained == 1 ~ 1,  # Yes
  ivs$trained == 2 ~ 0,  # No
  TRUE ~ NA_real_
)

# Create dummy variables for "audit"
dummy_audit <- data.frame(
  internal_audit = ifelse(ivs$audit %in% c(1, 3), 1,
                          ifelse(ivs$audit == 2, 0, 0)),
  external_audit = ifelse(ivs$audit %in% c(2, 3), 1,
                          ifelse(ivs$audit == 1, 0, 0))
)

# Set Retail/Wholesale (sector_comb2 = 10) as the reference group as it is the most common sector in the dataset
ivs$sector_comb2 <- factor(ivs$sector_comb2,
                                      levels = c("10", "1", "2", "3", "4", "5", "6", "7", "8", "9", "11", "12", "13"))

# Create dummy variables for sector (excluding the reference group "10")
dummy_sector <- model.matrix(~ sector_comb2 - 1, data = ivs)

# Convert "sizeb" into a factor (with Micro business size, coded as 1, as reference group)
ivs$sizeb <- factor(ivs$sizeb, levels = c(1, 2, 3, 4))
dummy_sizeb <- model.matrix(~ sizeb - 1, data = ivs)

# Combine all selected IVs with the dummy variables into the final IV dataset
ivs_final <- cbind(
  ivs[, c("imid", "rules15", "policy1", "rules9", "policy5",
                     "policy13", "rules4", "rules8", "policy2",
                     "policy4", "rules18", "rules20", "ident11",
                     "rules1", "rules2", "rules3", "ident14",
                     "rules17", "rules19", "trained", "info_comb4",
                     "manage1", "incidcontent3", "rules5", "rules7",
                     "info_comb8", "manage2")],
  dummy_audit,
  dummy_sector[, -1],  # Exclude reference group (Retail/Wholesale)
  dummy_sizeb[, -1]    # Exclude reference group (Micro)
)

# -------------------------------
# Step 2: VIF Measurement using DV1_incl, DV2_incl with linear model
# -------------------------------
# for DV1_incl
model_data <- cbind(
  ivs_final[, -which(names(ivs_final) == "imid")],
  DV1_incl = private_sector$DV1_incl
)

linear_model <- lm(DV1_incl ~ ., data = model_data)
vif_values <- vif(linear_model)
print(vif_values)

# for DV2_incl
model_data <- cbind(
  ivs_final[, -which(names(ivs_final) == "imid")],
  DV2_incl = private_sector$DV2_incl
)

linear_model <- lm(DV2_incl ~ ., data = model_data)
vif_values <- vif(linear_model)
print(vif_values)

# All VIF values using DV1_incl, DV2_incl is under 5

# -------------------------------------------------
# Step 3: VIF Measurement Using DV1_excl and DV2_excl
# -------------------------------------------------
# for DV1_excl
model_data <- cbind(
  ivs_final[, -which(names(ivs_final) == "imid")],
  DV1_excl = private_sector$DV1_excl
)

linear_model <- lm(DV1_excl ~ ., data = model_data)
vif_values <- vif(linear_model)
print(vif_values)

# VIF value using DV1_excl is under 5

# for DV2_excl
model_data <- cbind(
  ivs_final[, -which(names(ivs_final) == "imid")],
  DV2_excl = private_sector$DV2_excl
)

linear_model <- lm(DV2_excl ~ ., data = model_data)
vif_values <- vif(linear_model)
print(vif_values) #there are aliased coefficients

alias(linear_model) # sector_comb23 is perfectly collinear with other variables

# for DV2_excl (excluding sector_comb23 from ivs_final)
model_data <- cbind(
  ivs_final[, !(names(ivs_final) %in% c("imid", "sector_comb23"))],
  DV2_excl = private_sector$DV2_excl
)
linear_model <- lm(DV2_excl ~ ., data = model_data)
vif_values <- vif(linear_model)
print(vif_values)

# Now VIF value using DV2_excl is under 10，which could be acceptable considering that DV2_excl has the smallest sample size that might inflate VIF

# As VIF for other DV versions are under 5, this it not a structural issue with the current IVs