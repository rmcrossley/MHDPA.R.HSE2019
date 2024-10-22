
# Load required packages --------------------------------------------------
library(tidyverse)
library(haven)

hse_2019_in <- read_sav("C:/Users/Becky.Crossley/Department of Health and Social Care/GOV-Healthy Weight, Food & Nutrition Analysis - Documents/Data/HSfE/Raw/2019/UKDA-8860-spss/spss/spss25/hse_2019_eul_20211006.sav")

# Get the column names
#column_names <- colnames(hse_2019_in)

# Save the column names to a text file
#write(column_names, file = "column_names_hse_2019.txt")

# print(head(hse_2019_in))

# Reduce to data we want --------------------------------------------
# Select the specific columns
hse_2019_in_reduced <- hse_2019_in %>%
  select(Sex, ag16g10, HSEYR, BMIvg5, ThCoAny, SCSatis, origin2, LifeSatG, wemwbs, IllAff7, ILL12m, MENHTAKg2, AntiDepTakg2, SCOFF2, qimd19)

#print(head(hse_2019_in_reduced))
#print(summary(hse_2019_in_reduced))
