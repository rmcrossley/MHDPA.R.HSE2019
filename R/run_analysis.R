
# Load required packages --------------------------------------------------
library(tidyverse)
library(vcd)


# Source other scripts ----------------------------------------------------
# download <- new.env(); source("./R/download.R", local = download)
# process <- new.env(); source("./R/process.R", local = process)
# visualise <- new.env(); source("./R/visualise.R", local = visualise)

upload <- new.env(); source("./R/upload.R", local = upload)


# Main function to run analysis -------------------------------------------------------------------------------------------------------------------------------------------------

#' Run analysis
#'
#' Main function used to run analysis. This function is called from
#' the main.R script which should be used to run the project.
#'
#' @export
#'

run_analysis <- function() {
  print("Found the run_analysis function!")
  #log action
  logger$info("Running analysis...")

  # Load in data
  df <- upload$hse_2019_in_lab

  # Loop through factors for plots --------------------------------------------------------------------------------------
  # BMI first
  # List of factors to iterate through
  factors <- c("ag16g10", "Sex", "ThCoAny", "origin2",
               "LifeSatG", "wemwbs", "IllAff7", "ILL12m", "MENHTAKg2",
               "AntiDepTakg2", "SCOFF2", "qimd19", "AntiDepM2", "topqual3", "RELIGSC", "HHINC3", "eqv5", "totalwug_19")

  # Initialize a data frame to store results
  results <- data.frame(Factor = character(), Chi_Square_p_value = numeric(),
                        Cramers_V = numeric(), stringsAsFactors = FALSE)

  # Loop through each factor and perform the analysis
  for (factor in factors) {
    print(paste("Analysing", factor, "vs BMI"))

    # Clean out NAs for both Sex and the current factor
    df_clean <- df[complete.cases(df$BMIvg5, df[[factor]]), ]

    # Create a contingency table
    tbl <- table(df_clean$BMIvg5, df_clean[[factor]])

    # Perform chi-square test
    chi_square_result <- chisq.test(tbl)

    # Calculate Cramér's V
    cramers_v <- assocstats(tbl)$cramer

    # Save the results
    results <- rbind(results, data.frame(Factor = factor,
                                         Chi_Square_p_value = chi_square_result$p.value,
                                         Cramers_V = cramers_v))

    # Visualize with a mosaic plot
    mosaicplot(tbl, main = paste("Mosaic Plot of", factor, "and BMI"),
               ylab = factor, xlab = "BMI", color = TRUE)
  }

  # Print or save the results
  print(results)

  # Optionally, save results to a CSV or document
  write.csv(results, "BMI_chi_square_results.csv", row.names = FALSE)

  # BMI first
  # List of factors to iterate through
  factors <- c("ag16g10", "Sex", "qimd19", "origin2", "topqual3", "RELIGSC", "HHINC3", "eqv5", "totalwug_19")

  # Initialize a data frame to store results
  results <- data.frame(Factor = character(), Chi_Square_p_value = numeric(),
                        Cramers_V = numeric(), stringsAsFactors = FALSE)

  # Loop through each factor and perform the analysis
  for (factor in factors) {
    print(paste("Analysing", factor, "vs Life Satisfaction"))

    # Clean out NAs for both Sex and the current factor
    df_clean <- df[complete.cases(df[[factor]], df$LifeSatG), ]

    # Create a contingency table
    tbl <- table(df_clean[[factor]], df_clean$LifeSatG)

    # Perform chi-square test
    chi_square_result <- chisq.test(tbl)

    # Calculate Cramér's V
    cramers_v <- assocstats(tbl)$cramer

    # Save the results
    results <- rbind(results, data.frame(Factor = factor,
                                         Chi_Square_p_value = chi_square_result$p.value,
                                         Cramers_V = cramers_v))

    # Visualize with a mosaic plot
    mosaicplot(tbl, main = paste("Mosaic Plot of", factor, "and Life Satisfaction"),
               xlab = factor, ylab = "Life Satisfaction", color = TRUE)
  }

  # Print or save the results
  print(results)

  # Optionally, save results to a CSV or document
  write.csv(results, "LifeSatG_chi_square_results.csv", row.names = FALSE)

}

# run_analysis <- function() {
#   # log action
#   logger$info("Running analysis...")
#
#   # read in configuration
#   config_path <- file.path("input", "config.yml")
#   config <- yaml::read_yaml(config_path)
#
#   # get file date and time stamp
#   config$date_stamp <- format(Sys.time(), "%Y%m%d-%H%M")
#
#   # download summary file
#   attendance_data <-
#     download$download_nhsd_data(
#       config$source_url,
#       config$summary$regex,
#       file.path(
#         "input",
#         sprintf("%s_%s", config$date_stamp, config$summary$filename)
#       )
#     ) %>%
#     process$load_attendance_data()
#
#   visualise$save_plot(
#     visualise$plot_attendance_proportions(attendance_data),
#     file.path(
#       "output",
#       sprintf("%s_%s", config$date_stamp, "attendance_plot.svg")
#     )
#   )
#
#   # download summary file
#   nims_data <-
#     download$download_nhsd_data(
#       config$source_url,
#       config$nims$regex,
#       file.path(
#         "input",
#         sprintf("%s_%s", config$date_stamp, config$nims$filename)
#       )
#     ) %>%
#     process$load_nims_data()
#
#   # pick region of interest and only select full years
#   nims_monthly_eng <-
#     process$get_monthly_totals(
#       nims_data %>% filter(area_name == "England")
#     ) %>%
#     group_by(year) %>%
#     filter(n() == 12) %>%
#     ungroup()
#
#   visualise$save_plot(
#     visualise$plot_nims_monthly(nims_monthly_eng),
#     file.path(
#       "output",
#       sprintf("%s_%s", config$date_stamp, "nims_monthly_eng.svg")
#     )
#   )
#
#   # NIMS data uses old ICB codes so need to update to new 2023
#   # codes
#   nims_icb_data <- nims_data %>%
#     filter(
#       area_type == "ICB",
#       date == as.Date("2023-01-01")
#     ) %>%
#     mutate(
#       ons_code = case_when(
#         ons_code == "E54000053" ~ "E54000064",
#         ons_code == "E54000052" ~ "E54000063",
#         TRUE ~ ons_code
#       )
#     ) %>%
#     select(ons_code, total)
#
#
#   visualise$save_plot(
#     visualise$plot_nims_icb_map(nims_icb_data),
#     file.path(
#       "output",
#       sprintf("%s_%s", config$date_stamp, "nims_icb_map_20230101.svg")
#     )
#   )
#
#   # copy config to output
#   fs::file_copy(
#     file.path("input", "config.yml"),
#     file.path(
#       "output",
#       sprintf("%s_config.yml", config$date_stamp)
#     ),
#     overwrite = TRUE
#   )
#
#   logger$info("Finished")
# }
