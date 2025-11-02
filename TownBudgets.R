#loading Libraries
library(tidyverse)


#importingdata
data = read_csv("CombinedDataSet.csv")

#loading crosswalk
crosswalk = read_csv("Crosswalk.csv")

View(data)
names(data)
names(crosswalk)

# 1. Clean and pad both sides to 5-digit strings
# data <- data %>%
#   mutate(ZCTA20 = str_pad(as.character(ZCTA20), 5, pad = "0"))
# 
# crosswalk <- crosswalk %>%
#   rename(ZCTA = zcta, ZIP = ZIP_CODE) %>%        # make column names consistent
#   mutate(
#     ZCTA = str_pad(as.character(ZCTA), 5, pad = "0"),
#     ZIP  = str_pad(as.character(ZIP), 5, pad = "0")
#   )
# 
# # 2. Prefer “Zip matches ZCTA” where possible
# crosswalk_best <- crosswalk %>%
#   arrange(ZCTA, desc(zip_join_type == "Zip matches ZCTA")) %>%
#   group_by(ZCTA) %>%
#   slice(1) %>%
#   ungroup() %>%
#   select(ZCTA, ZIP, PO_NAME, STATE, ZIP_TYPE, zip_join_type)
# 
# # 3. Join with your dataset
# data_with_zip <- data %>%
#   left_join(crosswalk_best, by = c("ZCTA20" = "ZCTA"))
# 
# # 4. Quick sanity check
# data_with_zip %>%
#   count(zip_join_type)
# 
# view(data_with_zip)

data <- data %>%
  mutate(ZCTA20 = str_pad(as.character(ZCTA20), 5, pad = "0"))

crosswalk <- crosswalk %>%
  rename(ZCTA = zcta, ZIP = ZIP_CODE) %>%
  mutate(
    ZCTA = str_pad(as.character(ZCTA), 5, pad = "0"),
    ZIP  = str_pad(as.character(ZIP), 5, pad = "0")
  )

# --- 2. Pick best ZIP per ZCTA (prefer exact matches) -------------------------
crosswalk_best <- crosswalk %>%
  arrange(ZCTA, desc(zip_join_type == "Zip matches ZCTA")) %>%
  group_by(ZCTA) %>%
  slice(1) %>%
  ungroup() %>%
  select(ZCTA, ZIP, STATE)   # ✅ only keep these

# --- 3. Join to your main dataset --------------------------------------------
data_with_zip <- data %>%
  left_join(crosswalk_best, by = c("ZCTA20" = "ZCTA"))

# --- 4. Quick check -----------------------------------------------------------
head(data_with_zip %>% select(ZCTA20, ZIP, STATE))


#Now extracting all of MA
data_MA <- data_with_zip %>%
  filter(STATE == "MA")
view(data_MA)

#Making Zips to Towns

towns = read_csv("ZiptoTown.csv")
townsMA = towns %>% 
  filter(State == "Massachusetts")

#Combining

data_MA_with_town <- data_MA %>%
  left_join(
    townsMA,
    by = c("ZIP" = "ZIP Code")
  )

# Check the result
head(data_MA_with_town)
View(data_MA_with_town)

#Now joining the financial data

town_fin = read_csv("PublicWorks.csv", col_types = cols(.default = "c"))
view(town_fin)
head(town_fin)

#Cuttingdown to only what we need - Culture and Recreation and Town/County
town_fin_trimmed <- town_fin %>%
  select(Municipality, County, `Culture and Recreation`)

head(town_fin_trimmed)

# Collapse ZIP-level data into one per municipality
data_by_town <- data_MA_with_town %>%
  mutate(Municipality = str_to_title(str_trim(`USPS Default City for ZIP`))) %>%
  group_by(Municipality) %>%
  summarize(
    n_ZIPs = n(),
    mean_park_area = mean(TOT_PARK_AREA, na.rm = TRUE),
    mean_income = mean(MEDFAMINC, na.rm = TRUE)
  )

# Then join to town financials
merged_final <- data_by_town %>%
  left_join(town_fin_trimmed, by = "Municipality")
View(merged_final)

head(merg)
head(merged_final)

#Now to run the analysis:


library(broom)
library(ggplot2)

df <- merged_final %>%
  mutate(
    cr_budget      = parse_number(`Culture and Recreation`),  # was character with commas
    mean_income    = as.numeric(mean_income),                 # already numeric; just ensure
    mean_park_area = as.numeric(mean_park_area)
  )

# See how many usable values you actually have
df %>%
  summarize(
    n_cr   = sum(is.finite(cr_budget)),
    n_inc  = sum(is.finite(mean_income)),
    n_park = sum(is.finite(mean_park_area)),
    n_all3 = sum(complete.cases(cr_budget, mean_income, mean_park_area))
  )

df_all3 <- df %>% filter(is.finite(cr_budget), is.finite(mean_income), is.finite(mean_park_area))
df_all3

ci_budget <- t.test(df$cr_budget)
ci_income <- t.test(df$mean_income)
ci_park   <- t.test(df$mean_park_area)

ci_table <- tibble(
  metric = c("Culture & Recreation budget", "Mean income", "Mean park area"),
  mean   = c(ci_budget$estimate, ci_income$estimate, ci_park$estimate),
  lwr    = c(ci_budget$conf.int[1], ci_income$conf.int[1], ci_park$conf.int[1]),
  upr    = c(ci_budget$conf.int[2], ci_income$conf.int[2], ci_park$conf.int[2])
)
ci_table

# --- 3. Correlations with 95% CIs --------------------------------------------
cor_income <- cor.test(df$cr_budget, df$mean_income)
cor_park   <- cor.test(df$cr_budget, df$mean_park_area)

cor_summary <- tibble(
  pair = c("Budget ~ Income", "Budget ~ Park Area"),
  r    = c(cor_income$estimate, cor_park$estimate),
  lwr  = c(cor_income$conf.int[1], cor_park$conf.int[1]),
  upr  = c(cor_income$conf.int[2], cor_park$conf.int[2]),
  pval = c(cor_income$p.value, cor_park$p.value)
)
cor_summary

fit <- lm(log1p(cr_budget) ~ mean_income + mean_park_area, data = df)
model_ci <- tidy(fit, conf.int = TRUE)
model_ci


rng <- df %>%
  summarize(
    income_lo = quantile(mean_income, 0.10, na.rm = TRUE),
    income_hi = quantile(mean_income, 0.90, na.rm = TRUE),
    park_lo   = quantile(mean_park_area, 0.10, na.rm = TRUE),
    park_hi   = quantile(mean_park_area, 0.90, na.rm = TRUE)
  )

pred_grid <- expand.grid(
  mean_income    = seq(rng$income_lo, rng$income_hi, length.out = 25),
  mean_park_area = seq(rng$park_lo,   rng$park_hi,   length.out = 25)
)

preds <- predict(fit, newdata = pred_grid, interval = "confidence")
pred_out <- cbind(pred_grid, preds) %>%
  mutate(
    fit_dollars = exp(fit) - 1,
    lwr_dollars = exp(lwr) - 1,
    upr_dollars = exp(upr) - 1
  )
head(pred_out)
