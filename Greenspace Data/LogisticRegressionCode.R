#Starting a logistic regression for our data
#Installing Libraries
install.packages("pscl")       # for pseudo R²
install.packages("tidyverse")
install.packages("broom") 
install.packages("pROC")
install.packages("car")
install.packages("kableExtra")


#Loading Libraries
library(broom)
library(tidyverse)
library(pscl)
library(pROC)
library(car)
library(kableExtra)

#Importing Dataset
data = read_csv("CombinedDataSet.csv")

names(data)

#Turning Park Count into a Binary Variable

data$Binary = ifelse(data$COUNT_OPEN_PARKS > 0, 1, 0)

#Setting up the variables

form = Binary ~ PUNEMP + MEDFAMINC + ETHNICIMMIGRANT + PPOV + PPUBAS + PED1 + PED3 + P18YR_ + P18_29 + PGE70 + PFAMINCGE125K + PFAMINCLT40K

#Running the Logistic Regression
model = glm(form, data = data, family = binomial, na.action = na.exclude)
summary(model) 

#Translating the Model into Odds Ratios and 95% CI

ortable = tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
  arrange(p.value)

#Assigning Probabilities to Data Set

data$p_hat <- predict(model, newdata = data, type = "response")

#Model Fit 

pR2(model)

#AUC finding the area under the curve. 


data$y_hat = ifelse(data$p_hat >= 0.5, 1, 0)
table(Actual = data$Binary, Predicted = data$y_hat)

roc_obj = roc(data$Binary, data$p_hat)
auc(roc_obj)

#Checking for multi collinearity 

vif(model)

#Visualizing the Odd Ratio Table

#Renaming the terms
ortable <- ortable %>%
  mutate(term = case_when(
    term == "PED3" ~ "% Bachelor's Degree or Higher",
    term == "PED1" ~ "% Less than High School",
    term == "PPUBAS" ~  "% Bachelor's Degree (General)",
    term == "P18YR_" ~  "% Under Age 18",
    term == "P18_29" ~  "% Age 18–29",
    term == "PGE70" ~  "% Age 70+",
    term == "PFAMINCGE125K" ~  "% High-Income Families",
    term == "PFAMINCLT40K" ~  "% Low-Income Families",
    term == "MEDFAMINC" ~  "Median Family Income",
    term == "PUNEMP" ~  "% Unemployed",
    term == "ETHNICIMMIGRANT" ~  "% Immigrant Population",
    term == "PPOV" ~ "% Below Poverty",
    term == "(Intercept)" ~  "Intercept",
    TRUE ~ term
  ))

#Vizualize as a table

ortable %>%
  kable("html", caption = "Logistic Regression Odds Ratios") %>%
  kable_styling(full_width = FALSE, position = "center")



