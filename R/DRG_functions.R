# Load packages
library(readr)
library(tidyverse)
library(devtools)

# Load data
load("Data/DRG_data.RData")

# Processing data
names(drg) <- gsub(" ", "_", names(drg)) # Rename the variables
drg <- drg %>%
  mutate(DRG_Code = substr(DRG_Definition, 0, 3)) # Extract the first three characters from DRG Definition

# Function 1

#' Make A Boxplot of Payments By DRG Code
#'
#' @param pmt A payment category in  Medicare Severity Diagnosis Related Group data (DRG data)
#'
#' @return A boxplot of payments by DRG code
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#' @importFrom tidyverse
#'
#' @examples
#' boxplot_pmt('Average_Covered_Charges')
#'

boxplot_pmt <- function(pmt) {
  # Give the option when the payment is the average covered charges
  if (pmt == 'Average_Covered_Charges') {
    ggplot(drg, aes(x = DRG_Code, y = Average_Covered_Charges, fill = DRG_Code)) +
      geom_boxplot() +
      labs(title = 'Boxplot of Average Covered Charges by DRG Code',
           x = 'DRG Code',
           y = 'Average Covered Charges') +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 5, angle = 90), legend.position = "none")
    # Give the option when the payment is the average total payments
  }else if (pmt == 'Average_Total_Payments') {
    ggplot(drg, aes(x = DRG_Code, y = Average_Total_Payments, fill =  DRG_Code)) +
      geom_boxplot() +
      labs(title = 'Boxplot of Average Total Payments by DRG Code',
           x = 'DRG Code',
           y = 'Average Total Payments') +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 5, angle = 90), legend.position = "none")
    # Give the option when the payment is the average medicare payments
  }else if (pmt == 'Average_Medicare_Payments') {
    options(repr.plot.width = 2, repr.plot.height =3)
    ggplot(drg, aes(x = DRG_Code, y = Average_Medicare_Payments, fill = DRG_Code)) +
      geom_boxplot() +
      labs(title = 'Boxplot of Average Medicare Payments by DRG Code',
           x = 'DRG Code',
           y = 'Average Medicare Payments') +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 5, angle = 90), legend.position = "none")
    # Give a warning when the name does not match with the options
  } else {
    print('Wrong Name')
  }
}

# Function 2

#' Calculate A Statistics of Average Medicare Payments
#'
#' @param option A statistics (mean, median, or standard deviation) that is required to be calculated
#'
#' @return A mean, median, or standard deviation of average Medicare payments
#' @export
#'
#' @importFrom tidyverse
#' @examples makeBox("Mean")

makeBox <- function(option){

  if(option == "Mean"){
    # If choose to calculate the mean
    drg %>%
      # Group with same DRG Code
      group_by(DRG_Definition) %>%
      # Calculate its mean and form a table
      summarise(mean(Average_Medicare_Payments))
  }
  else if(option == "Median"){
    # If choose to calculate the median
    drg %>%
      # Group with same DRG Code
      group_by(DRG_Definition) %>%
      # Calculate its mean and form a table
      summarise(median(Average_Medicare_Payments))
  }
  else{
    # If choose to calculate the sd
    drg %>%
      # Group with same DRG Code
      group_by(DRG_Definition) %>%
      # Calculate its mean and form a table
      summarise(sd(Average_Medicare_Payments))
  }
}


