## 🧾 Analytic Code for "Once Bitten, Twice Shy? Understanding Repeat and Multiple Victimisation in Business Cybercrime"

### 📄 Overview

This repository contains the R code used in the study _"Once Bitten, Twice Shy? Understanding Repeat and Multiple Victimisation in Business Cybercrime."_  
The study analyzes predictors of victimisation (DV1), repeat victimisation (DV2), and multiple victimisation (DV3) using the 2024 Cyber Security Breaches Survey (CSBS).  
It applies Multinomial Logit (ML), Bivariate Probit (BP), and Hurdle Negative Binomial (HNB) models.

---

### 📁 Repository Structure

📂 /main_manuscript  
   ─ ML_BP_model_analysis_DV1&2.R      # Multinomial logit and bivariate probit models for DV1 & DV2  
   ─ HNB_model_analysis_DV3.R          # Hurdle negative binomial model for DV3  

📂 /supplementary_material  
   ─ ML_BP_model_excl_phishing_DV1&2.R # ML & BP models excluding phishing cases  
   ─ HNB_model_excl_phishing_DV3.R     # HNB model excluding phishing cases  

📂 /data                               # Folder for raw CSBS2024 data (not included in repo)

---

### ▶️ How to Run the Code
 
1. Open the project `Analysis_scripts.Rproj` in RStudio.  
2. Download the raw CSBS2024 dataset (not included here) and save it in the `/data` folder.
3. Run the main analysis scripts first.
4. Then run the supplementary analysis scripts.

---

### 📊 Data Availability

The data come from the 2024 UK **Cyber Security Breaches Survey (CSBS)**, administered by the UK Department for Science, Innovation and Technology (DSIT).  
Available at UK Data Service with **Study Number (SN): 9285**.

---
