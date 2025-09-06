# Load required libraries
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(dplyr)

# Read and merge datasets
dataset1 <- read.csv("dataset1.csv")
dataset2 <- read.csv("dataset2.csv")

implants <- dataset2 %>%
  rename(De.identified.Joint.ID = De.identified.primary.Joint.ID) %>%
  right_join(dataset1, by = "De.identified.Joint.ID")

# Ensure year is numeric
implants <- implants %>% mutate(SURGERY_YEAR = as.numeric(SURGERY_YEAR))

# ============= FILTER BY INCLUSION/EXCLUSION CRITERIA =========
total_cases <- nrow(implants)

## Keep only primary TKA cases -- should already be filtered for primary and total TKA by CJRR team
primary_tka <- implants %>%
  filter(Primary.procedure.type == "Total Knee Arthroplasty" & Replacement.Type == "Primary")

primary_tka_n <- nrow(primary_tka)

assessed_for_eligibility <- primary_tka # rename for flow chart purposes
assessed_n <- nrow(assessed_for_eligibility)

## Exclude Hybrid / Reverse Hybrid -- check if/where this information is 
included_cases <- assessed_for_eligibility %>%
  filter(!ProductName %in% c("Hybrid", "Reverse Hybrid"))

included_n <- nrow(included_cases)
excluded_hybrid <- assessed_n - included_n

## Count by fixation type (cemented vs cementless) -- may need to re-run after fixation validation 
cement_counts <- included_cases %>%
  count(Fixation)

cemented_n <- cement_counts %>% filter(Fixation == "Cemented") %>% pull(n)
cementless_n <- cement_counts %>% filter(Fixation == "Cementless") %>% pull(n)

# Output flowchart values
flowchart_summary <- tibble(
  Stage = c(
    "Total records in CJRR",
    "Primary TKA cases",
    "Assessed for eligibility",
    "Excluded: Hybrid / Reverse Hybrid",
    "Included for analysis",
    "Cemented implants",
    "Cementless implants"
  ),
  Count = c(
    total_cases,
    primary_tka_n,
    assessed_n,
    excluded_hybrid,
    included_n,
    cemented_n,
    cementless_n
  )
)

print(flowchart_summary)


# ========= DATA MISSINGNESS ===========

library(naniar)

# Visualize missingness overall
gg_miss_var(implants, show_pct = TRUE) +
  labs(title = "Missingness Across All Variables")

# Stratified by Sex
gg_miss_var(implants, facet = GENDER, show_pct = TRUE) +
  labs(title = "Missing Data by Sex")

# Stratified by Fixation Type 
gg_miss_var(implants, facet = Fixation, show_pct = TRUE) +
  labs(title = "Missing Data by Implant Fixation Type")

# ========= COHORT CHARACTERISTICS =======

# Sex Distribution 
ggplot(implants, aes(x = GENDER, fill = GENDER)) +
  geom_bar() +
  scale_x_discrete(labels = c("Male", "Female")) +
  labs(title = "Sex Distribution in Sample", x = "Sex", y = "Count") +
  theme_minimal()

# Age Distribution
ggplot(implants, aes(x = AGE_SURGERY)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_density(aes(y = ..count.. * 5), color = "darkblue", size = 1) +
  labs(title = "Age Distribution of TKA Patients", x = "Age", y = "Count") +
  theme_minimal()

## Age Distribution by sex
ggplot(implants, aes(x = AGE_SURGERY)) +
  facet_wrap(~ GENDER) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_density(aes(y = ..count.. * 5), color = "darkblue", size = 1) +
  labs(title = "Age Distribution of TKA Patients", x = "Age", y = "Count") +
  theme_minimal()

## Age Distribution by sex and Fixation method 
ggplot(implants, aes(x = AGE_SURGERY)) +
  facet_wrap(~ GENDER + Fixation) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_density(aes(y = ..count.. * 5), color = "darkblue", size = 1) +
  labs(title = "Age Distribution of TKA Patients", x = "Age", y = "Count") +
  theme_minimal()

# Region Distribution (provincial) -- 
if ("Province.Territory" %in% names(implants)) {
  ggplot(implants, aes(x = Province.Territory, fill = Province.Territory)) +
    geom_bar() +
    labs(title = "Distribution by Province", x = "Province", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# ========== PROCEDURE COUNTS ==========

# TKA Counts over time by sex
implants %>%
  filter(Primary.procedure.type == "Total Knee Arthroplasty") %>%
  count(SURGERY_YEAR, GENDER) %>%
  ggplot(aes(x = SURGERY_YEAR, y = n, color = GENDER)) +
  geom_line() +
  labs(title = "Annual TKA Volume by Sex", y = "Count of Procedures", x = "Year") +
  scale_x_continuous(breaks = unique(implants$SURGERY_YEAR)) +
  theme_minimal()

# ========== TRENDS IN CEMENT USE ==========

# Cemented vs Uncemented by CEMENT_USED_FLAG_CJRR --- check to exclude hybrids? 
implants %>%
  filter(CEMENT_USED_FLAG_CJRR %in% c("Y", "N")) %>%
  count(SURGERY_YEAR, GENDER, CEMENT_USED_FLAG_CJRR) %>%
  ggplot(aes(x = SURGERY_YEAR, y = n, color = CEMENT_USED_FLAG_CJRR)) +
  geom_line() +
  facet_wrap(~ GENDER) +
  labs(title = "Cemented vs Uncemented Implants by Sex (CJRR Flag)", x = "Year", y = "Count") +
  theme_minimal()

# Chi-square test for Cement Flag
chisq_test_flag <- implants %>%
  filter(CEMENT_USED_FLAG_CJRR %in% c("Yes", "No")) %>%
  with(table(GENDER, CEMENT_USED_FLAG_CJRR)) %>%
  chisq.test()
chisq_test_flag

# Cemented vs Uncemented by Fixation Method -- check hybrids 
implants %>%
  count(SURGERY_YEAR, GENDER, Fixation) %>%
  ggplot(aes(x = SURGERY_YEAR, y = n, color = Fixation)) +
  geom_line() +
  facet_wrap(~ GENDER) +
  labs(title = "Cemented vs Uncemented Implants by Sex (Fixation)", x = "Year", y = "Count") +
  theme_minimal()

# Chi-square test for Fixation method
chisq_test_fixation <- implants %>%
  filter(Fixation %in% c("Cemented", "Cementless")) %>%
  with(table(GENDER, Fixation)) %>%
  chisq.test()
chisq_test_fixation

# ========== PROPORTION TRENDS ==========

# Total uncemented trend
trend_over_time <- implants %>%
  group_by(SURGERY_YEAR) %>%
  summarise(
    total = n(),
    uncemented_count = sum(Fixation == "Cementless", na.rm = TRUE),
    cemented_count = sum(Fixation == "Cemented", na.rm = TRUE),
    cemented_prop = cemented_count / total, 
    uncemented_prop = uncemented_count / total
  )

ggplot(trend_over_time, aes(x = SURGERY_YEAR, y = uncemented_prop)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkblue") +
  labs(title = "Trend in Uncemented Implant Use in Canada",
       x = "Year", y = "Proportion Uncemented") +
  theme_minimal()

# By sex
trend_by_sex <- implants %>%
  group_by(SURGERY_YEAR, GENDER) %>%
  summarise(
    total = n(),
    uncemented = sum(Fixation == "Cementless", na.rm = TRUE),
    proportion = uncemented / total,
    .groups = "drop"
  )

ggplot(trend_by_sex, aes(x = SURGERY_YEAR, y = proportion, color = GENDER)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Uncemented Implant Use by Sex", x = "Year", y = "Proportion Uncemented") +
  theme_minimal()

# ========== Add historic trends from previous years ===== 
# Data from CJRR annual reports, CIHI (fixation proportions not published for some years)
historic_data <- tibble::tribble(
  ~SURGERY_YEAR, ~total, ~uncemented_count, ~cemented_count, ~cemented_prop, ~uncemented_prop,
  2003, NA, NA, NA, 0.848, 0.008,
  2004, NA, NA, NA, 0.87, 0.008,
  2005, NA, NA, NA, 0.879, 0.027,
  2006, NA, NA, NA, 0.886, 0.031,
  2007, NA, NA, NA, 0.888, 0.033,
  2008, NA, NA, NA, 0.896, 0.034,
  2009, NA, NA, NA, 0.897, 0.033,
  2010, NA, NA, NA, 0.895, 0.032, 
  2011, NA, NA, NA, 0.891, 0.029,
  2013, NA, NA, NA, 0.8, NA
)
historic_trends_over_time <- bind_rows(trend_over_time, historic_data)
   
historic_plot <- historic_trends_over_time %>%
  select(SURGERY_YEAR, cemented_prop, uncemented_prop) %>%
  pivot_longer(cols = c(cemented_prop, uncemented_prop),
               names_to = "Fixation",
               values_to = "Proportion")

# plot both on same graph
ggplot(historic_plot, aes(x = SURGERY_YEAR, y = Proportion, color = Fixation)) +
  geom_point(size = 2) +
  geom_line(size = 1.2) +
  geom_smooth(se = FALSE, method = "loess", span = 0.6, linetype = "dashed") +
  labs(title = "Trend in Cemented vs Uncemented Implant Use in Canada",
       x = "Year", y = "Proportion") +
  scale_color_manual(values = c("cemented_prop" = "firebrick",
                                "uncemented_prop" = "steelblue"),
                     labels = c("Cemented", "Uncemented")) +
  theme_minimal()

# ========== TRENDS BY AGE GROUP ==========

# Create age groups
implants <- implants %>%
  mutate(age_group = cut(age,
                         breaks = c(0, 50, 60, 70, 80, Inf),
                         labels = c("<50", "50-60", "60-70", "70-80", "80+")))

trend_by_age_sex <- implants %>%
  group_by(SURGERY_YEAR, age_group, GENDER) %>%
  summarise(
    total = n(),
    uncemented = sum(uncemented, na.rm = TRUE),
    proportion = uncemented / total,
    .groups = "drop"
  )



# ========== TESTS FOR TRENDS ==========

# Chi-square test across years
chisq_data <- implants %>%
  group_by(SURGERY_YEAR) %>%
  summarise(cemented = sum(cemented, na.rm = TRUE), uncemented = sum(uncemented, na.rm = TRUE))

chisq.test(as.matrix(chisq_data[, c("cemented", "uncemented")]))

# ========== LOGISTIC REGRESSION ==========
# Logistic regression (age continuous)
implants$CEMENT_USED_FLAG_CJRR <- factor(implants$CEMENT_USED_FLAG_CJRR, levels = c("Y", "N"))

logit_model <- glm(CEMENT_USED_FLAG_CJRR ~ SURGERY_YEAR + age + GENDER, data = implants, family = binomial)
summary(logit_model)
exp(coef(logit_model))  # Odds ratios

# Visualize probability of uncemented use by age
ggplot(implants, aes(x = age, y = as.numeric(uncemented))) +
  geom_smooth(method = "loess", se = TRUE, color = "steelblue") +
  labs(title = "Probability of Uncemented Implant Use by Age and Sex",
       x = "Age", y = "Probability (Uncemented)") +
  theme_minimal()

# ----------Sex Disaggregated ------------
## MALE ##
implants_M <- implants %>%
  filter(GENDER == "M")

logit_model_M <- glm(CEMENT_USED_FLAG_CJRR ~ SURGERY_YEAR + age, data = implants_M, family = binomial)
summary(logit_model_M)
exp(coef(logit_model_M))  # Odds ratios

# Visualize probability of uncemented use by age
ggplot(implants_M, aes(x = age, y = as.numeric(uncemented))) +
  geom_smooth(method = "loess", se = TRUE, color = "steelblue") +
  labs(title = "Probability of Uncemented Implant Use by Age",
       x = "Age", y = "Probability (Uncemented)") +
  theme_minimal()

## FEMALE ##
implants_F <- implants %>%
  filter(GENDER == "F")

logit_model_F <- glm(CEMENT_USED_FLAG_CJRR ~ SURGERY_YEAR + age, data = implants_F, family = binomial)
summary(logit_model_F)
exp(coef(logit_model_F))  # Odds ratios

# Visualize probability of uncemented use by age
ggplot(implants_F, aes(x = age, y = as.numeric(uncemented))) +
  geom_smooth(method = "loess", se = TRUE, color = "steelblue") +
  labs(title = "Probability of Uncemented Implant Use by Age",
       x = "Age", y = "Probability (Uncemented)") +
  theme_minimal()

# ========== AGE AT SURGERY ==========

# Mean age at surgery over time by sex
implants %>%
  group_by(SURGERY_YEAR, GENDER) %>%
  summarise(
    age_mean = mean(AGE_SURGERY, na.rm = TRUE),
    age_sd = sd(AGE_SURGERY, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = SURGERY_YEAR, y = age_mean, color = GENDER)) +
  geom_line(size = 1.1) +
  geom_ribbon(aes(ymin = age_mean - age_sd, ymax = age_mean + age_sd, fill = GENDER), alpha = 0.2) +
  labs(title = "Mean Age at TKA by Year and Sex",
       y = "Mean Age", x = "Year") +
  theme_minimal()

# Mean age at surgery over time by sex and fixation
implants %>%
  group_by(SURGERY_YEAR, GENDER, Fixation) %>%
  summarise(
    age_mean = mean(AGE_SURGERY, na.rm = TRUE),
    age_sd = sd(AGE_SURGERY, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = SURGERY_YEAR, y = age_mean, color = GENDER)) +
  geom_line(size = 1.1) +
  geom_ribbon(aes(ymin = age_mean - age_sd, ymax = age_mean + age_sd, fill = GENDER), alpha = 0.2) +
  labs(title = "Mean Age at TKA by Year and Sex",
       y = "Mean Age", x = "Year") +
  facet_wrap(~Fixation) +
  theme_minimal()

# Regression test of age over time
lm_fit <- lm(AGE_SURGERY ~ SURGERY_YEAR * GENDER, data = implants)
summary(lm_fit)

# ============ PROVINCIAL TRENDS IN CEMENT USE =================
prov_cement <- implants %>%
  count(SUBMITTING_PROV, SURGERY_YEAR, GENDER, CEMENT_USED_FLAG_CJRR)

ggplot(prov_cement, aes(x = SURGERY_YEAR, y = n,
                        color = SUBMITTING_PROV,
                        linetype = CEMENT_USED_FLAG_CJRR,
                        group = interaction(SUBMITTING_PROV, CEMENT_USED_FLAG_CJRR))) +
  geom_line(size = 1) +
  labs(title = "Cemented vs Uncemented Use Over Time by Province",
       x = "Surgery Year",
       y = "Number of Procedures",
       color = "Province",
       linetype = "Cement Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(prov_cement, aes(x = SUBMITTING_PROV, y= n, fill = SUBMITTING_PROV)) +
    geom_bar(stat = "identity") +
    labs(title = "Cemented vs Uncemented TKAs by Province", x = "Province", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# By Sex
ggplot(prov_cement, aes(x = SURGERY_YEAR, y = n, color = CEMENT_USED_FLAG_CJRR)) +
  geom_line() +
  facet_wrap(~ GENDER) +
  labs(title = "Cemented vs Uncemented Implants by Sex (CJRR Flag)", x = "Year", y = "Count") +
  theme_minimal()

ggplot(prov_cement, aes(x = SUBMITTING_PROV, y= n, fill = GENDER)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ CEMENT_USED_FLAG_CJRR) +
    labs(title = "Cemented vs Uncemented TKAs by Province & Sex", x = "Province", y = "Count", fill = "Sex") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(prov_cement, aes(x = SUBMITTING_PROV, y = n,
                        fill = interaction(GENDER, CEMENT_USED_FLAG_CJRR))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "TKAs by Province, Sex & Cementation Status",
       x = "Province",
       y = "Count",
       fill = "Sex & Cement Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ================= Hosptial Type ==========================
hosp_cement <- implants %>%
  mutate(org_category_code = case_when(
    org_category_code %in% c("H1", "H2", "H3") ~ "H",
    TRUE ~ org_category_code
  )) %>%
  count(ORG_CATEGORY_CODE, SUBMITTING_PROV, SURGERY_YEAR, GENDER, CEMENT_USED_FLAG_CJRR)

ggplot(hosp_cement, aes(x = SUBMITTING_PROV, y = n, fill = ORG_CATEGORY_CODE)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of TKAs performed at Community vs Teaching Hospitals by Province",
       x = "Province",
       y = "Count",
       fill = "Hospital Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(hosp_cement, aes(x = ORG_CATEGORY_CODE, y= n, fill = ORG_CATEGORY_CODE)) +
    geom_bar(stat = "identity") +
    labs(title = "Cemented vs Uncemented TKAs by Hospital Type", x = "Hospital Type", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# By Sex
ggplot(hosp_cement, aes(x = ORG_CATEGORY_CODE, y= n, fill = GENDER)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ CEMENT_USED_FLAG_CJRR) +
    labs(title = "Cemented vs Uncemented TKAs by Hospital Type & Sex", x = "Hospital Type", y = "Count", fill = "Sex") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Hospital type Over time  
ggplot(hosp_cement, aes(x = SURGERY_YEAR, y = n,
                        color = ORG_CATEGORY_CODE,
                        linetype = CEMENT_USED_FLAG_CJRR,
                        group = interaction(ORG_CATEGORY_CODE, CEMENT_USED_FLAG_CJRR))) +
  geom_line(size = 1) +
  labs(title = "Cemented vs Uncemented Use Over Time by Hospital Type",
       x = "Surgery Year",
       y = "Number of Procedures",
       color = "Hospital Type",
       linetype = "Cement Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#===================== TABLES FOR PAPER ======================
# Demographics 
implants_unique <- implants %>%
  group_by(Joint_ID) %>%
  summarise(
    AGE_SURGERY = first(AGE_SURGERY),
    GENDER = first(GENDER),
    CEMENT_USED_FLAG_CJRR = first(CEMENT_USED_FLAG_CJRR),
    FISCAL_YEAR = first(FISCAL_YEAR),
    SUBMITTING_PROV = first(SUBMITTING_PROV),
    org_category_code = first(org_category_code),
    MR_JOINT_DIAG = first(MR_JOINT_DIAG),
    ProductName = paste(unique(ProductName), collapse = "; "),
    .groups = "drop"
  )

implants_unique <- implants_unique %>%
  mutate(uncemented = ifelse(CEMENT_USED_FLAG_CJRR == "N", 1, 0),
         cemented = ifelse(CEMENT_USED_FLAG_CJRR == "Y", 1, 0))

# Age summary
age_summary <- implants %>%
  summarise(
    mean_age = round(mean(AGE_SURGERY, na.rm = TRUE), 1),
    min_age = min(AGE_SURGERY, na.rm = TRUE),
    max_age = max(AGE_SURGERY, na.rm = TRUE)
  )

# Sex (% Female)
sex_summary <- implants %>%
  summarise(percent_female = round(mean(GENDER == "F", na.rm = TRUE) * 100, 1))

# TKA counts
tka_counts <- implants %>%
  summarise(
    cemented = sum(cemented, na.rm = TRUE),
    uncemented = sum(uncemented, na.rm = TRUE)
  )

# Chi-squared test
chisq_table <- table(implants$GENDER, implants$CEMENT_USED_FLAG_CJRR)
chisq_result <- chisq.test(chisq_table)

# Combine results
demographics_table <- tibble(
  Metric = c("Mean Age", "Age Range", "% Female", "Total TKA Count", "Chi-squared p-value"),
  Cemented = c(age_summary$mean_age,
               paste(age_summary$min_age, "-", age_summary$max_age),
               sex_summary$percent_female,
               tka_counts$cemented,
               round(chisq_result$p.value, 4)),
  Uncemented = c("", "", "", tka_counts$uncemented, "")
)

# Brand Trends 
brand_table <- implants %>%
  group_by(ProductName, CEMENT_USED_FLAG_CJRR) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = CEMENT_USED_FLAG_CJRR, values_from = count, values_fill = 0) %>%
  rename(Cemented = Y, Uncemented = N)

# Brand total by component 
library(dplyr)

component_brand_table <- implants %>%
  group_by(ProductName, CEMENT_USED_FLAG_CJRR) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = CEMENT_USED_FLAG_CJRR,
    values_from = count,
    values_fill = 0
  ) %>%
  rename(Cemented = Y, Uncemented = N)

# Brand per component 
component_brand_table <- implants %>%
  group_by(JOINT_COMP, ProductName, CEMENT_USED_FLAG_CJRR) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = CEMENT_USED_FLAG_CJRR,
    values_from = count,
    values_fill = 0
  ) %>%
  rename(Cemented = Y, Uncemented = N)

# Reason for TKA 
reason_table <- implants %>%
  group_by(MR_JOINT_DIAG, CEMENT_USED_FLAG_CJRR) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(MR_JOINT_DIAG) %>%
  mutate(percent = round(count / sum(count) * 100, 1)) %>%
  pivot_wider(names_from = CEMENT_USED_FLAG_CJRR,
              values_from = c(count, percent),
              names_glue = "{CEMENT_USED_FLAG_CJRR}_{.value}",
              values_fill = 0) %>%
  rename(Cemented_N = Y_count, Cemented_% = Y_percent,
         Uncemented_N = N_count, Uncemented_% = N_percent)
  
# Provincial Counts
prov_table <- implants %>%
  group_by(SUBMITTING_PROV, CEMENT_USED_FLAG_CJRR) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(SUBMITTING_PROV) %>%
  mutate(percent = round(count / sum(count) * 100, 1)) %>%
  pivot_wider(names_from = CEMENT_USED_FLAG_CJRR,
              values_from = c(count, percent),
              names_glue = "{CEMENT_USED_FLAG_CJRR}_{.value}",
              values_fill = 0) %>%
  rename(Cemented_N = Y_count, Cemented_% = Y_percent,
         Uncemented_N = N_count, Uncemented_% = N_percent)

# Cement Use Over Time by Sex
cement_year_sex <- implants %>%
  group_by(FISCAL_YEAR, GENDER, CEMENT_USED_FLAG_CJRR) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(FISCAL_YEAR, GENDER) %>%
  mutate(percent = round(count / sum(count) * 100, 1)) %>%
  pivot_wider(names_from = CEMENT_USED_FLAG_CJRR,
              values_from = c(count, percent),
              names_glue = "{CEMENT_USED_FLAG_CJRR}_{.value}",
              values_fill = 0) %>%
  rename(Cemented_N = Y_count, Cemented_% = Y_percent,
         Uncemented_N = N_count, Uncemented_% = N_percent)

#  Hospital Type Summary
implants <- implants %>%
  mutate(hospital_type = case_when(
    org_category_code %in% c("H1", "H2", "H3") ~ "H",
    TRUE ~ org_category_code
  ))

hospital_table <- implants %>%
  group_by(hospital_type, CEMENT_USED_FLAG_CJRR) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(hospital_type) %>%
  mutate(percent = round(count / sum(count) * 100, 1)) %>%
  pivot_wider(names_from = CEMENT_USED_FLAG_CJRR,
              values_from = c(count, percent),
              names_glue = "{CEMENT_USED_FLAG_CJRR}_{.value}",
              values_fill = 0) %>%
  rename(Cemented_N = Y_count, Cemented_% = Y_percent,
         Uncemented_N = N_count, Uncemented_% = N_percent)

#  Hospital Type Over Time by Sex
hospital_year_table <- implants %>%
  group_by(hospital_type, FISCAL_YEAR, GENDER, CEMENT_USED_FLAG_CJRR) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(hospital_type, FISCAL_YEAR, GENDER) %>%
  mutate(percent = round(count / sum(count) * 100, 1)) %>%
  pivot_wider(names_from = CEMENT_USED_FLAG_CJRR,
              values_from = c(count, percent),
              names_glue = "{CEMENT_USED_FLAG_CJRR}_{.value}",
              values_fill = 0) %>%
  rename(Cemented_N = Y_count, Cemented_% = Y_percent,
         Uncemented_N = N_count, Uncemented_% = N_percent)

write.csv(demographics_table, "demographics_table.csv", row.names = FALSE)
write.csv(brand_table, "brand_fixation_table.csv", row.names = FALSE)
write.csv(reason_table, "reason_for_TKA_table.csv", row.names = FALSE)
write.csv(prov_table, "provincial_counts_table.csv", row.names = FALSE)
write.csv(cement_year_sex, "cement_over_time_by_sex.csv", row.names = FALSE)
write.csv(hospital_table, "hospital_type_table.csv", row.names = FALSE)
write.csv(hospital_year_table, "hospital_type_over_time_table.csv", row.names = FALSE)