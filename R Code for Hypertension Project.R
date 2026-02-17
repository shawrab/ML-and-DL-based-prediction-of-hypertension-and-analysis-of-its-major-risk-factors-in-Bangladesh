# Load required library
library(haven)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(caret)
library(pROC)
library(survey)
library(srvyr)
library(flextable)
library(gtsummary)
library(car)

# Load the personal recode dataset
PR <- read_dta("E:\\Project\\Hyprtension\\BDHS Data\\STATA\\BDPR81DT\\BDPR81FL.DTA")

# Hypertension variables
hypertension <- PR %>%
  filter(
    (!is.na(wbp24) & wbp24 < 500) |
    (!is.na(wbp25) & wbp25 < 500) |
    (!is.na(mbp24) & mbp24 < 500) |
    (!is.na(mbp25) & mbp25 < 500)
  )

# Age variable
hypertension <- hypertension %>%
  mutate(
    age_women = ifelse(!is.na(ha32), (hv008 - ha32) / 12, NA),
    age_men = ifelse(!is.na(hb32), (hv008 - hb32) / 12, NA),
    age = coalesce(age_women, age_men) # Combine women's and men's ages into one variable
  )

# Extract the BMI variable
hypertension$BMI_women <- hypertension$ha2 / ((hypertension$ha3 / 100) ^ 2)
hypertension$BMI_men <- hypertension$hb2 / ((hypertension$hb3 / 100) ^ 2)
hypertension$women.weight <- (hypertension$ha2) / 10
hypertension$women.height <- (((hypertension$ha3) / 10)/100) ^ 2
hypertension$women.BMI <- hypertension$women.weight / hypertension$women.height
hypertension$men.weight <- (hypertension$hb2) / 10
hypertension$men.height <- (((hypertension$hb3) / 10)/100) ^ 2
hypertension$men.BMI <- hypertension$men.weight / hypertension$men.height
hypertension$BMI <- ifelse(hypertension$hv104 == 1, hypertension$men.BMI, hypertension$women.BMI)
# hypertension$BMI
# hypertension$BMI <- (hypertension$ha40)/100

# Diabetes (plasma blood glucose in mmol/l)
hypertension$blood_glucose <- coalesce(hypertension$sb267g, hypertension$sb367g)


# create new dataframe with related variables
new_data <- hypertension %>%
  filter(hv106 != 8) %>%  # Remove rows where hv106 == 8
  select(hv005, hv021, hv023, wbp24, wbp25, mbp24, mbp25, mbp19, age,
         hv104, hv105, hv106, hv009, hv115, hv024, hv025, hv270, hb60, BMI,
         blood_glucose, sb240, sb336)
View(new_data)


# Transform and clean variables
clean_data <- new_data %>%
  # Create hypertension variable (1 = yes, 0 = no)
  mutate(
    hypertension = case_when(
      wbp24 >= 140 ~ 1, wbp25 >= 90 ~ 1, mbp24 >= 140 ~ 1, mbp25 >= 90 ~ 1, mbp19 == 1  ~ 1,
      TRUE        ~ 0
    ),
    hypertension = factor(hypertension, levels = c(0, 1), labels = c("No", "Yes")),
    sex = factor(hv104, levels = c(1, 2), labels = c("Male", "Female")),
    age_group = cut(age,
                    breaks = c(0, 34, 59, Inf),
                    labels = c("18–34", "35–59", "60+"),
                    right = TRUE # hypertension$hv009
    ),
    family_size = case_when(
      hv009 >= 0 & hv009 <= 3 ~ "Small",
      hv009 >= 4 & hv009 <= 6 ~ "Medium",
      hv009 >= 7 ~ "Large",
      TRUE    ~ NA_character_
    ),
    family_size = factor(family_size, levels = c("Small", "Medium", "Large")),
    marital_status = case_when(
      hv115 == 0 ~ "Never married",
      hv115 %in% c(1, 2, 3, 4, 5) ~ "Married",
      TRUE ~ NA_character_
    ),
    marital_status = factor(marital_status, levels = c("Never married", "Married")),
    division = factor(hv024, levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                      labels = c("Barisal", "Chittagong", "Dhaka", "Khulna", 
                                 "Mymensingh", "Rajshahi", "Rangpur", "Sylhet")),
    residence_type = factor(hv025, labels = c("Urban", "Rural")),
    wealth_index = factor(hv270, levels = c(1, 2, 3, 4, 5),
                          labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
    education_level_detailed = factor(hv106, levels = c(0, 1, 2, 3, 8),
                             labels = c("No education", "Primary", "Secondary", "Higher", "NA")),
    education_level = case_when(
      education_level_detailed %in% c("No education", "Primary") ~ "No/Primary",
      education_level_detailed %in% c("Secondary", "Higher") ~ "Secondary/Higher",
      TRUE ~ NA_character_
    ),
    education_level = factor(education_level, levels = c("No/Primary", "Secondary/Higher")),
    bmi_category = case_when(
      BMI < 18.5             ~ "Underweight",
      BMI >= 18.5 & BMI < 25 ~ "Normal",
      BMI >= 25   ~ "Obese/Overweight",
      TRUE                   ~ NA_character_
    ),
    bmi_category = factor(bmi_category, levels = c("Underweight", "Normal", "Obese/Overweight")),
    diabetes = case_when(
      blood_glucose >= 7.0 ~ "Yes",
      blood_glucose < 7.0  ~ "No",
      sb240 == 1 ~ "Yes",
      sb336 == 1 ~ "Yes",
      TRUE                 ~ NA_character_
    ),
    diabetes = factor(diabetes, levels = c("No", "Yes")),
    sampling_weight = new_data$hv005 / 1000000,
    cluster = new_data$hv021,
    strata = new_data$hv023
  )  

# Drop unused original variables
final_data <- clean_data %>% 
  select(hypertension, sex, age_group, family_size, marital_status, 
         division, residence_type, wealth_index,
         education_level, bmi_category, diabetes,sampling_weight,
         cluster, strata)

table(final_data$hypertension)

# Save as Stata file
# write_dta(final_data, "C:/Users/Asus/Dropbox/02_Shawrab Chandra/Project Hypertension/BDHS data analysis in R/final_data.dta")

# Sampling weight, cluster, strata
sampling_weight = final_data$sampling_weight
cluster = final_data$cluster
strata = final_data$strata
bdhs_design <- svydesign(
  ids = cluster, strata = strata, 
  weights = sampling_weight, 
  data = final_data
)

# Convert to srvyr object for tidy syntax
bdhs_srvyr <- as_survey_design(
  final_data,
  ids = cluster,
  strata = strata,
  weights = sampling_weight,
  nest = TRUE
)

# List of variables
vars <- c("sex", "age_group", "family_size", "marital_status",
          "bmi_category", "division", "residence_type",
          "wealth_index", "education_level", "diabetes")


# Fit standard logistic regression for the calculation of VIF
vif_model <- glm(
  hypertension ~ sex + age_group + family_size + marital_status +
    division + residence_type + wealth_index +
    education_level + bmi_category + diabetes,
  data = final_data,
  family = binomial()
)

# Calculate VIF
vif_values <- vif(vif_model)
print(vif_values)

# Loop: create Total N(%), Hypertension(95% CI), and p-values
table_list <- lapply(vars, function(var) {
  # Total N (%)
  total_table <- bdhs_srvyr %>%
    group_by(.data[[var]]) %>%
    summarize(
      n = unweighted(n()),
      weighted_percent = survey_mean(vartype = NULL) * 100,
      .groups = "drop"
    ) %>%
    mutate(
      Total_N = paste0(n, " (", round(weighted_percent, 1), "%)")
    ) %>%
    select(level = .data[[var]], Total_N)

# Hypertension prevalence (95% CI) + Standard Error
prev_table <- bdhs_srvyr %>%
  group_by(.data[[var]]) %>%
  summarize(
    prevalence = survey_mean(as.numeric(hypertension == "Yes"), vartype = c("ci", "se")),
    .groups = "drop"
  ) %>%
  mutate(
    Hypertension_CI = paste0(round(prevalence * 100, 2), " (",
                             round(prevalence_low * 100, 2), "–",
                             round(prevalence_upp * 100, 2), ")"),
    Standard_Error = round(prevalence_se * 100, 2)  # <-- Added SE as %
  ) %>%
  select(level = .data[[var]], Hypertension_CI, Standard_Error)  # <-- Added SE column

  # p-value (survey-adjusted Chi-square)
  pval <- svychisq(as.formula(paste("~", var, "+ hypertension")), design = bdhs_srvyr)$p.value
  pval_formatted <- ifelse(pval < 0.001, "<0.001", round(pval, 3))

  # Combine Total N and Prevalence
  combined <- total_table %>%
    left_join(prev_table, by = "level") %>%
    mutate(variable = var) %>%
    select(variable, level, Total_N, Hypertension_CI) %>%
    mutate(Chi_square_test = ifelse(row_number() == 1, pval_formatted, ""))

  return(combined)
})

# Combine all into one big table
final_table <- bind_rows(table_list)

#  View table
print(final_table)

# Export to Word
# final_table %>%
#   flextable::flextable() %>%
#   flextable::save_as_docx(path = "Hypertension_Prevalence_Publication_Table.docx")

