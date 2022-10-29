
library(readr)
library(tidyverse)
library(devtools)
drg <- read_csv('D:\\Data Science 1\\Projects\\Lab2-Group9\\DRG_data.csv')
names(drg) <- gsub(" ", "_", names(drg))
drg <- drg %>%
  mutate(DRG_Code = substr(DRG_Definition, 0, 3))

setwd('..')
save(drg, file = 'D:\\Data Science 1\\Test\\Lab2Test\\Data\\DRG_data.RData')

# data(drg)
load("D:/Data Science 1/Test/Lab2-Group9/Data/DRG_data.RData")
boxplot_pmt <- function(pmt) {
  if (pmt == 'Average_Covered_Charges') {
    ggplot(drg, aes(x = DRG_Code, y = Average_Covered_Charges, fill = DRG_Code)) +
      geom_boxplot() +
      labs(title = 'Boxplot of Average Covered Charges by DRG Code',
           x = 'DRG Code',
           y = 'Average Covered Charges') +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 5, angle = 90), legend.position = "none")
  }else if (pmt == 'Average_Total_Payments') {
    ggplot(drg, aes(x = DRG_Code, y = Average_Total_Payments, fill =  DRG_Code)) +
      geom_boxplot() +
      labs(title = 'Boxplot of Average Total Payments by DRG Code',
           x = 'DRG Code',
           y = 'Average Total Payments') +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 5, angle = 90), legend.position = "none")
  }else if (pmt == 'Average_Medicare_Payments') {
    options(repr.plot.width = 2, repr.plot.height =3)
    ggplot(drg, aes(x = DRG_Code, y = Average_Medicare_Payments, fill = DRG_Code)) +
      geom_boxplot() +
      labs(title = 'Boxplot of Average Medicare Payments by DRG Code',
           x = 'DRG Code',
           y = 'Average Medicare Payments') +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 5, angle = 90), legend.position = "none")
  } else {
    print('Wrong Name')
  }

}


makeBox <- function(option){
  if(option == "Mean"){
    # If choose to calculate the mean
    drg %>%
      # group with same DRG Code
      group_by(DRG_Definition) %>%
      # Calculate its mean and form a table
      summarise(mean(Average_Medicare_Payments))
  }
  else if(option == "Median"){
    # If choose to calculate the median
    drg %>%
      # group with same DRG Code
      group_by(DRG_Definition) %>%
      # Calculate its mean and form a table
      summarise(median(Average_Medicare_Payments))
  }
  else{
    # If choose to calculate the sd
    drg %>%
      # group with same DRG Code
      group_by(DRG_Definition) %>%
      # Calculate its mean and form a table
      summarise(sd(Average_Medicare_Payments))
  }
}
# Use map function to form three tables at a time
map(c("Mean", "Median", "Sd"), makeBox)
```
