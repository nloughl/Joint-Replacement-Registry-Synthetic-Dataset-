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
implants <- implants %>% mutate(Surgery.Year = as.numeric(Surgery.Year))

# ============= FILTER BY INCLUSION/EXCLUSION CRITERIA =========
total_cases <- nrow(implants)

## Keep only primary TKA cases -- should already be filtered for primary and total TKA by CJRR team
primary_tka <- implants %>%
  filter(Primary.procedure.type == "Total Knee Arthroplasty" & Replacement.Type == "Primary")

primary_tka_n <- nrow(primary_tka)

## Exclude Hybrid / Reverse Hybrid -- check if/where this information is 
included_cases <- assessed_for_eligibility %>%
  filter(!Product.name %in% c("Hybrid", "Reverse Hybrid"))

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
gg_miss_var(implants, facet = sex, show_pct = TRUE) +
  labs(title = "Missing Data by Sex")

# Stratified by Fixation Type 
gg_miss_var(implants, facet = uncemented, show_pct = TRUE) +
  labs(title = "Missing Data by Implant Fixation Type")

# ========= COHORT CHARACTERISTICS =======

# Sex Distribution 
ggplot(implants, aes(x = sex, fill = sex)) +
  geom_bar() +
  scale_x_discrete(labels = c("Male", "Female")) +
  labs(title = "Sex Distribution in Sample", x = "Sex", y = "Count") +
  theme_minimal()

# Age Distribution 
ggplot(implants, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_density(aes(y = ..count.. * 5), color = "darkblue", size = 1) +
  labs(title = "Age Distribution of TKA Patients", x = "Age", y = "Count") +
  theme_minimal()

# Region Distribution (provincial) -- 
if ("Province.Territory" %in% names(implants)) {
  ggplot(implants, aes(x = region, fill = Province.Territory)) +
    geom_bar() +
    labs(title = "Distribution by Province", x = "Province", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Summary table 
library(summarytools)
dfSummary(implants)

# ========== PROCEDURE COUNTS ==========

# TKA Counts over time by sex
implants %>%
  filter(Primary.procedure.type == "Total Knee Arthroplasty") %>%
  count(Surgery.Year, Patient.Sex) %>%
  ggplot(aes(x = Surgery.Year, y = n, color = Patient.Sex)) +
  geom_line() +
  labs(title = "Annual TKA Volume by Sex", y = "Count of Procedures", x = "Year") +
  scale_x_continuous(breaks = unique(implants$Surgery.Year)) +
  theme_minimal()

# ========== TRENDS IN CEMENT USE ==========

# Cemented vs Uncemented by Cement.Flag..CJRR. --- check to exclude hybrids? 
implants %>%
  filter(Cement.Flag..CJRR. %in% c("Yes", "No")) %>%
  count(Surgery.Year, Patient.Sex, Cement.Flag..CJRR.) %>%
  ggplot(aes(x = Surgery.Year, y = n, color = Cement.Flag..CJRR.)) +
  geom_line() +
  facet_wrap(~ Patient.Sex) +
  labs(title = "Cemented vs Uncemented Implants by Sex (CJRR Flag)", x = "Year", y = "Count") +
  theme_minimal()

# Chi-square test for Cement Flag
chisq_test_flag <- implants %>%
  filter(Cement.Flag..CJRR. %in% c("Yes", "No")) %>%
  with(table(Patient.Sex, Cement.Flag..CJRR.)) %>%
  chisq.test()
chisq_test_flag

# Cemented vs Uncemented by Fixation Method -- check hybrids 
implants %>%
  count(Surgery.Year, Patient.Sex, Fixation) %>%
  ggplot(aes(x = Surgery.Year, y = n, color = Fixation)) +
  geom_line() +
  facet_wrap(~ Patient.Sex) +
  labs(title = "Cemented vs Uncemented Implants by Sex (Fixation)", x = "Year", y = "Count") +
  theme_minimal()

# Chi-square test for Fixation method
chisq_test_fixation <- implants %>%
  filter(Fixation %in% c("Cemented", "Cementless")) %>%
  with(table(Patient.Sex, Fixation)) %>%
  chisq.test()
chisq_test_fixation

# ========== PROPORTION TRENDS ==========

# Total uncemented trend
trend_over_time <- implants %>%
  group_by(Surgery.Year) %>%
  summarise(
    total = n(),
    uncemented_count = sum(uncemented, na.rm = TRUE),
    uncemented_prop = uncemented_count / total
  )

ggplot(trend_over_time, aes(x = Surgery.Year, y = uncemented_prop)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkblue") +
  labs(title = "Trend in Uncemented Implant Use in Canada",
       x = "Year", y = "Proportion Uncemented") +
  theme_minimal()

# By sex
trend_by_sex <- implants %>%
  group_by(Surgery.Year, sex) %>%
  summarise(
    total = n(),
    uncemented = sum(uncemented, na.rm = TRUE),
    proportion = uncemented / total,
    .groups = "drop"
  )

ggplot(trend_by_sex, aes(x = Surgery.Year, y = proportion, color = sex)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Uncemented Implant Use by Sex", x = "Year", y = "Proportion Uncemented") +
  theme_minimal()

# ========== TRENDS BY AGE GROUP ==========

# Create age groups
implants <- implants %>%
  mutate(age_group = cut(age,
                         breaks = c(0, 50, 60, 70, 80, Inf),
                         labels = c("<50", "50-60", "60-70", "70-80", "80+")))

trend_by_age <- implants %>%
  group_by(Surgery.Year, age_group) %>%
  summarise(
    total = n(),
    uncemented = sum(uncemented, na.rm = TRUE),
    proportion = uncemented / total,
    .groups = "drop"
  )

ggplot(trend_by_age, aes(x = Surgery.Year, y = proportion, color = age_group)) +
  geom_line(size = 1.1) +
  geom_point() +
  labs(title = "Uncemented Implant Use by Age Group", x = "Year", y = "Proportion Uncemented") +
  theme_minimal()

# ========== TESTS FOR TRENDS ==========

# Chi-square test across years
chisq_data <- implants %>%
  group_by(Surgery.Year) %>%
  summarise(cemented = sum(cemented, na.rm = TRUE), uncemented = sum(uncemented, na.rm = TRUE))

chisq.test(as.matrix(chisq_data[, c("cemented", "uncemented")]))

# ========== LOGISTIC REGRESSION ==========

# Logistic regression (age continuous)
implants$uncemented <- as.factor(implants$uncemented)

logit_model <- glm(uncemented ~ Surgery.Year + age + sex, data = implants, family = binomial)
summary(logit_model)
exp(coef(logit_model))  # Odds ratios

# Visualize probability of uncemented use by age
ggplot(implants, aes(x = age, y = as.numeric(uncemented))) +
  geom_smooth(method = "loess", se = TRUE, color = "steelblue") +
  labs(title = "Probability of Uncemented Implant Use by Age",
       x = "Age", y = "Probability (Uncemented)") +
  theme_minimal()

# ========== AGE AT SURGERY ==========

# Mean age at surgery over time by sex
implants %>%
  group_by(Surgery.Year, Patient.Sex) %>%
  summarise(
    age_mean = mean(Patient.Age, na.rm = TRUE),
    age_sd = sd(Patient.Age, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Surgery.Year, y = age_mean, color = Patient.Sex)) +
  geom_line(size = 1.1) +
  geom_ribbon(aes(ymin = age_mean - age_sd, ymax = age_mean + age_sd, fill = Patient.Sex), alpha = 0.2) +
  labs(title = "Mean Age at TKA by Year and Sex",
       y = "Mean Age", x = "Year") +
  theme_minimal()

# Regression test of age over time
lm_fit <- lm(Patient.Age ~ Surgery.Year * Patient.Sex, data = implants)
summary(lm_fit)
