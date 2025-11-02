
install.packages("dplyr")
#Loading Libraries

library(dplyr)
library(tidyverse)
#Importing Dataset
data = read_csv("CombinedDataSet.csv")



# data <- data %>%
#   mutate(
#     areapop = ifelse(TOTPOP > 0 & !is.na(TOTPOP),
#                      TOT_PARK_AREA / TOTPOP,
#                      NA)
#   )
# 
# quantile(data$areapop, probs = seq(0, 1, 0.25), na.rm = TRUE)
# 
# top10 <- data %>%
#   arrange(desc(areapop)) %>%
#   slice_head(n = 10)
# 
# # Bottom 10 lowest areapop
# bottom10 <- data %>%
#   arrange(areapop) %>%
#   slice_head(n = 10)
# 
# # View results
# top10
# bottom10
# 
# top10 %>% select(ZCTA20, MEDFAMINC, areapop)
# bottom10 %>% select(ZCTA20,MEDFAMINC, areapop)


row_1810 <- data %>%
  filter(ZCTA20 == "01810")

# View it
row_1810

# ---- 1) Set your total parks funding ----
total_funding <- 1000000  

# ---- 2) Calculate per-acre and per-person metrics ----
summary_stats = row_1810 %>%
  mutate(
    dollars_per_acre   = total_funding / TOT_PARK_AREA,
    dollars_per_person = total_funding / TOTPOP,
    areapop = TOT_PARK_AREA / TOTPOP
  )

print(summary_stats)

# Example: nicely formatted output
cat(sprintf(
  paste0(
    "Total Funding: $%s\n",
    "Total Park Area: %s acres\n",
    "Total Population: %s\n\n",
    "Median Family Income: $%s\n",
    "Spending per Acre: $%s\n",
    "Spending per Person: $%s\n",
    "Park Area per Person (areapop): %s acres/person\n"
  ),
  format(round(total_funding, 0), big.mark = ","), 
  format(round(summary_stats$TOT_PARK_AREA, 2), big.mark = ","), 
  format(round(summary_stats$TOTPOP, 0), big.mark = ","), 
  format(round(summary_stats$MEDFAMINC, 0), big.mark = ","), 
  format(round(summary_stats$dollars_per_acre, 2), big.mark = ","), 
  format(round(summary_stats$dollars_per_person, 2), big.mark = ","), 
  format(round(summary_stats$areapop, 6), big.mark = ",")
))

#Low Income Area New Bedford


row_02745 <- data %>%
  filter(ZCTA20 == "02745")

# View it
row_02745

# ---- 1) Set your total parks funding ----
total_fundingNB <- 546038 

# ---- 2) Calculate per-acre and per-person metrics ----
summary_statsNB = row_02745 %>%
  mutate(
    dollars_per_acre   = total_funding / TOT_PARK_AREA,
    dollars_per_person = total_funding / TOTPOP,
    areapop = TOT_PARK_AREA /TOTPOP
  )

print(summary_statsNB)

# Example: nicely formatted output
cat(sprintf(
  paste0(
    "Total Funding: $%s\n",
    "Total Park Area: %s acres\n",
    "Total Population: %s\n\n",
    "Median Family Income: $%s\n",
    "Spending per Acre: $%s\n",
    "Spending per Person: $%s\n",
    "Park Area per Person (areapop): %s acres/person\n"
  ),
  format(round(total_fundingNB, 0), big.mark = ","), 
  format(round(summary_statsNB$TOT_PARK_AREA, 2), big.mark = ","), 
  format(round(summary_statsNB$TOTPOP, 0), big.mark = ","), 
  format(round(summary_statsNB$MEDFAMINC, 0), big.mark = ","), 
  format(round(summary_statsNB$dollars_per_acre, 2), big.mark = ","), 
  format(round(summary_statsNB$dollars_per_person, 2), big.mark = ","), 
  format(round(summary_statsNB$areapop, 6), big.mark = ",")
))

02630 
row_02630  <- data %>%
  filter(ZCTA20 == "02630")
view(row_02630$MEDFAMINC)
