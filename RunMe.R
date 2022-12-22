#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Country Reports - RunMe File
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 17th, 2022
##
## This version:      December 1st, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Presettings                                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
source("Code/settings.R")

# Loading functions for sections
source("Code/S01.R")
source("Code/S02.R")
source("Code/S03.R")

# Loading plotting functions from GitHub
source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/loading.R")
loadVIZ(set = "LAC")

# Loading data
master_data.df <- read_dta("Data/LAC - Merged.dta")

# Defining group of countries to work with
if (args[1] == "andean") {
  group <- andeanCountries.ls
}
if (args[1] == "south") {
  group <- southCone.ls
}
if (args[1] == "carib1") {
  group <- westCaribbean_and_guianas.ls
}
if (args[1] == "carib2") {
  group <- eastCaribbean.ls
}
if (args[1] == "central") {
  group <- centralAmerica.ls
}

# Subsetting data for the current report
data_subset.df <- master_data.df %>%
  filter(country %in% group) %>%
  
  # Latest year is different for Paraguay
  mutate(latestYear = if_else(country == "Paraguay", 2021, 2022))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Loop Prologue                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

for (mainCountry in group) {
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
  print(paste("Currently printing plots for",
              mainCountry))
  
  # Which country group is gonna be the comparison group?
  comparison_countries.ls <- group[-match(mainCountry, group)]

  # Country Set to include in the reports
  countrySet <- c(mainCountry, comparison_countries.ls)

  # Updating the country color palette to match the length of the group of countries
  countryPalette <- countryPalette[1:length(countrySet)]
  names(countryPalette) <- countrySet
  
  # Cleaning the Outputs directory for this country
  ordnung.fn(targetCountry = mainCountry)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Section 1                                                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # All the figure functions from this section are contained in the "Code/S01.R" file. Please refer to it
  # for more information.
  
  # Figure 1:
  print("Designing Figure 1")
  figure01.fn()
  
  # Figure 2:
  print("Designing Figure 2")
  figure02.fn()
  
  # Figure 3:
  print("Designing Figure 3")
  if (mainCountry != "Paraguay") {
    figure03.fn()
  }
  
  # Figure 4:
  print("Designing Figure 4")
  if (mainCountry != "Paraguay") {
    figure04.fn()
  } else {
    print("We forgot to add Paraguay")
  }
  
  # Figure 5:
  print("Designing Figure 5")
  figure05.fn()
  
  # Figure 6:
  print("Designing Figure 6")
  if (mainCountry != "Paraguay") {
    figure06.fn()
  } else {
    figure06_1_PARAGUAY.fn()
  }
  
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##   Section 2                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # All the figure functions from this section are contained in the "Code/S02.R" file. Please refer to it
  # for more information.
  
  # Figure 7:
  print("Designing Figure 7")
  figure07.fn()
  
  # Figure 8:
  print("Designing Figure 8")
  figure08.fn()
  
  # Figure 9:
  print("Designing Figure 9")
  figure09.fn()
  
  # Figure 10:
  print("Designing Figure 10")
  figure10.fn()
  
  # Figure 11:
  print("Designing Figure 11")
  figure11.fn()
  
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##   Section 3                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # All the figure functions from this section are contained in the "Code/S03.R" file. Please refer to it
  # for more information.
  
  print("Designing Figure 12")
  # Figure 12.1:
  figure12_1.fn()
  # Figure 12.2:
  figure12_2.fn()
  
  print("Designing Figure 13")
  # Figure 13.1:
  figure13_1.fn()
  # Figure 13.2:
  # figure13_2.fn() // NOT WORKING FOR BOLIVIA
  
  # Figure 14:
  print("Designing Figure 14")
  figure14.fn()
  
  # Figure 15:
  print("Designing Figure 15")
  figure15.fn()
  
  # Figure 16:
  print("Designing Figure 16")
  figure_16.fn()
  
  # Figure 17:
  print("Designing Figure 17")
  figure17.fn()
  
  # Figure 18:
  print("Designing Figure 18")
  figure18.fn()

}
