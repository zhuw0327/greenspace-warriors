# Analysis of Variance

#----------Packages------------#
# install.packages("tidyverse")
library(tidyverse)
library(broom)

#----------Data pull-----------#
data = read.csv('/cloud/project/Greenspace Data/CombinedDataSet.csv')

data = data[1:1000,] #provisions here to save my system memory usage

#----------Data analysis (Manual if desired) ------#
# #----------ANOVA BY RACE AS GROUPS ------#
# anova_pnhblack = data %>% 
#   mutate(binned_pnhblack = cut(PNHBLACK, breaks = 5)) %>% 
#     filter(!is.na(binned_pnhblack)) %>% 
#     #ANOVA Binned by pnhblack
#     reframe(
#       # tidy(aov(COUNT_OPEN_PARKS ~ PNHBLACK)) # If uncomment and run this, the p values tell us that the variance is too different between groups
#       tidy(oneway.test(formula = COUNT_OPEN_PARKS~binned_pnhblack, var.equal = FALSE))
#     )

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
bind_rows(
  list(
    "% Non Hispanic Black" = run_anova(data, PNHBLACK,COUNT_OPEN_PARKS, 5),
    "% Hispanic" = run_anova(data, PHISPANIC,COUNT_OPEN_PARKS, 5),
    "% White" = run_anova(data, PNHWHITE,COUNT_OPEN_PARKS, 5),
    "% People foreign born" = run_anova(data, PFBORN,COUNT_OPEN_PARKS, 5),
    "% Less than High School" = run_anova(data, PED1,COUNT_OPEN_PARKS, 5),
    "% Highschool/some College" = run_anova(data, PED2,COUNT_OPEN_PARKS, 5),
    "% Bachelors or Higher" = run_anova(data, PED3,COUNT_OPEN_PARKS, 5),
    "Median Family Income" = run_anova(data, MEDFAMINC,COUNT_OPEN_PARKS, 5),
    "% Unemployed" = run_anova(data, PUNEMP,COUNT_OPEN_PARKS, 5),
    "% Living w/ Public Assistance" = run_anova(data, PPUBAS,COUNT_OPEN_PARKS, 5),
    "Family Affluence (Ed, Income, Management)" = run_anova(data, AFFLUENCE,COUNT_OPEN_PARKS, 5),
    "Disadvantaged Folks (Minority, Poverty, Unemployed)" = run_anova(data, DISADVANTAGE,COUNT_OPEN_PARKS, 5)
  ),
  .id = "ANOVA on Open Parks Based on..."
) %>% kable("html", caption = "ANOVA Results") %>%   #UNCOMMENT OUT THE KABLE TO VIEW RAW P VALUES
  kable_styling(full_width = FALSE, position = "center")  
