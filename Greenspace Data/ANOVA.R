# Analysis of Variance

#----------Packages------------#
# install.packages("tidyverse")
library(tidyverse)
library(broom)

#----------Data pull-----------#
data = read.csv('/cloud/project/Greenspace Data/CombinedDataSet.csv')

data = filter(data,between(ZCTA20,1001,2791)) # ZCTAs restricted to Massachussetts

viewdata = cut(data$PED1,3)
print(table(viewdata))
#----------Data analysis (Manual if desired) ------#
# #----------ANOVA BY RACE AS GROUPS ------#
# anova_pnhblack = data %>%
#   mutate(binned_pnhblack = cut(PNHBLACK, breaks = 3)) %>%
#     filter(!is.na(binned_pnhblack)) %>%
#     #ANOVA Binned by pnhblack
#     reframe(
#       # tidy(aov(COUNT_OPEN_PARKS ~ PNHBLACK)) # If uncomment and run this, the p values tell us that the variance is too different between groups
#       tidy(oneway.test(formula = COUNT_OPEN_PARKS~binned_pnhblack, var.equal = FALSE))

#----------Data analysis (Function)------#
#----------ANOVA FUNCTION ------#
# Creating a function similar to above ^ 
#' @details Does not include dependencies in function. Runs ANOVA for dataset and variables of interest.
#' Independent var is the GROUPs that we want to compare across
#' Dependent var is the OUTCOME that we want to analyze
#' Group count is how many bins we wanna split into
run_anova = function(inputdata, independvar, dependvar, group_count){
  anova_results = inputdata %>% 
    mutate(binned_var = cut({{independvar}}, breaks = group_count)) %>% 
    filter(!is.na(binned_var)) %>% 
    reframe(
      tidy(oneway.test(formula = {{dependvar}} ~ binned_var, var.equal = FALSE), data = cur_data())
    )
  return(anova_results)
}

#----------Data analysis Meat and Bones ------#
#----------RUN ANOVA FOR MULTIPLE FACTORS ------#
# Result is a bit funky, almost like saying p value <0.05 means there exists a statistically significant difference between groups
# p > 0.05 indicates that there is not a significant difference in the groups
bin_count = 3;
bind_rows(
  list(
    "% Non Hispanic Black" = run_anova(data, PNHBLACK,COUNT_OPEN_PARKS, bin_count),
    #"% Hispanic" = run_anova(data, PHISPANIC,COUNT_OPEN_PARKS, bin_count),
    "% White" = run_anova(data, PNHWHITE,COUNT_OPEN_PARKS, bin_count),
    "% People foreign born" = run_anova(data, PFBORN,COUNT_OPEN_PARKS, bin_count),
    # "% Less than High School" = run_anova(data, PED1,COUNT_OPEN_PARKS, bin_count),
    "% Highschool/some College" = run_anova(data, PED2,COUNT_OPEN_PARKS, bin_count),
    "% Bachelors or Higher" = run_anova(data, PED3,COUNT_OPEN_PARKS, bin_count),
    "Median Family Income" = run_anova(data, MEDFAMINC,COUNT_OPEN_PARKS, bin_count),
    # "% Unemployed" = run_anova(data, PUNEMP,COUNT_OPEN_PARKS, bin_count),
    "% Living w/ Public Assistance" = run_anova(data, PPUBAS,COUNT_OPEN_PARKS, bin_count),
    "Family Affluence (Ed, Income, Management)" = run_anova(data, AFFLUENCE,COUNT_OPEN_PARKS, bin_count),
    "Disadvantaged Folks (Minority, Poverty, Unemployed)" = run_anova(data, DISADVANTAGE,COUNT_OPEN_PARKS, bin_count)
  ),
  .id = "ANOVA on Open Parks Based on..."
) %>% kable("html", caption = "ANOVA Results") %>%   #UNCOMMENT OUT THE KABLE TO VIEW RAW P VALUES
  kable_styling(full_width = FALSE, position = "center")  


bind_rows(
  list(
    "% Non Hispanic Black" = run_anova(data, PNHBLACK,TOT_PARK_AREA_SQMILES, 5),
    "% Hispanic" = run_anova(data, PHISPANIC,TOT_PARK_AREA_SQMILES, 5),
    "% White" = run_anova(data, PNHWHITE,TOT_PARK_AREA_SQMILES, 5),
    "% People foreign born" = run_anova(data, PFBORN,TOT_PARK_AREA_SQMILES, 5),
    "% Less than High School" = run_anova(data, PED1,TOT_PARK_AREA_SQMILES, 5),
    "% Highschool/some College" = run_anova(data, PED2,TOT_PARK_AREA_SQMILES, 5),
    "% Bachelors or Higher" = run_anova(data, PED3,TOT_PARK_AREA_SQMILES, 5),
    "Median Family Income" = run_anova(data, MEDFAMINC,TOT_PARK_AREA_SQMILES, 5),
    "% Unemployed" = run_anova(data, PUNEMP,TOT_PARK_AREA_SQMILES, 5),
    "% Living w/ Public Assistance" = run_anova(data, PPUBAS,TOT_PARK_AREA_SQMILES, 5),
    "Family Affluence (Ed, Income, Management)" = run_anova(data, AFFLUENCE,TOT_PARK_AREA_SQMILES, 5),
    "Disadvantaged Folks (Minority, Poverty, Unemployed)" = run_anova(data, DISADVANTAGE,TOT_PARK_AREA_SQMILES, 5)
  ),
  .id = "ANOVA on Open Parks Based on..."
) %>% kable("html", caption = "ANOVA Results") %>%   #UNCOMMENT OUT THE KABLE TO VIEW RAW P VALUES
  kable_styling(full_width = FALSE, position = "center")  


######################################################
############## FINANCIAL IMPACTS #####################
######################################################

#----------Data pull-----------#
data = read.csv('/cloud/project/Greenspace Data/CombinedDataSet.csv')
massdata = data[133:671,] #provisions here to save my system memory usage

massdata = massdata %>% 
  filter(!is.na(TOTPOP), !is.na(TOT_PARK_AREA))

cost = 0.02 #placeholder. Cost or unrealized benefit value per square meter of space
lcl = 50 #m^2 based on NIH recommendations

# Calculate group data 
fmass = function(massdata){
  massmetrics = massdata %>% 
    mutate(
      greenspace_per_person = TOT_PARK_AREA/TOTPOP,
      groupmeanarea = mean(TOT_PARK_AREA), # Meters^2 #arbitrarily setting this as the LCL for now. Due to the disparity between zctas with lots of park area and those with literally 0, we may want to consider a LCL of the lower quartile of the dataset
      parkgap = greenspace_per_person-lcl,
      parkgap = ifelse(parkgap>lcl,0,parkgap), # omitting people that already have parks
      # parkgap = pmin(0,lcl), #capping maximum gap to 0 to just look at the negatives 
      ind_cost = parkgap * cost, #individual cost of missing square footage
      tot_cost = sum(ind_cost, na.rm = TRUE) # Calculating the total cost to the state by summing up all the nonconforming ZCTA's.
    ) 
  return(massmetrics) #can return massmetrics if desiterd, we only care about tot_cost for now
}

masscost = fmass(massdata)
masscost

# random samples bootstrapping
massdata %>% sample_n(size = n(), replace = TRUE) %>% fmass()

bootstrapped_totcost = replicate(
  n = 1000,
  expr = {
    massdata %>% sample_n(size = n(), replace = TRUE) %>% 
      fmass()
  }
)
bootstrap = tibble(
  rep = 1:1000,
  masscost = bootstrapped_totcost
)
bootstrap

boot = tibble(rep = 1:1000) %>% #doesnt work
  group_by(rep) %>%
  reframe(massdata) %>%
  group_by(rep) %>%
  sample_n(size = n(), replace = TRUE) %>%
  group_by(rep) %>%
  summarize(fmass(massdata))
boot


