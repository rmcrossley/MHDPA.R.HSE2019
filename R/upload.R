
# Load required packages --------------------------------------------------
library(tidyverse)
library(haven)

hse_2019_in <- read_sav("C:/Users/Becky.Crossley/Department of Health and Social Care/GOV-Healthy Weight, Food & Nutrition Analysis - Documents/Data/HSfE/Raw/2019/UKDA-8860-spss/spss/spss25/hse_2019_eul_20211006.sav")

# Get the column names
column_names <- colnames(hse_2019_in)

# Save the column names to a text file
write(column_names, file = "column_names_hse_2019.txt")
