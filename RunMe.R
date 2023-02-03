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
source("Code/methodology.R")

# Loading plotting functions from GitHub
source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/loading.R")
loadVIZ(set = "LAC")

# Loading data
master_data.df <- read_dta("Data/LAC - Merged.dta")
method_data.ls <- list(
  "sf" = read_excel("Data/method_summaryData.xlsx",
                    sheet = "Sampling_Frame"),
  "af" = read_excel("Data/method_summaryData.xlsx",
                    sheet = "AdminDivs"),
  "sd" = read_excel("Data/method_summaryData.xlsx",
                    sheet = "SampleDesc"),
  "tA" = read_excel("Data/method_summaryData.xlsx",
                    sheet = "Table_A"),
  "tB" = read_excel("Data/method_summaryData.xlsx",
                    sheet = "Table_B")
)

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
  if (mainCountry != "Paraguay") {
    figure01.fn()
  }
  
  # Figure 2:
  print("Designing Figure 2")
  if (mainCountry != "Paraguay") {
    figure02.fn()
  }
  
  # Figure 3:
  print("Designing Figure 3")
  if (mainCountry != "Paraguay") {
    figure03.fn()
  } else {
    figure03.fn(nchart = 1)
  }

  # Figure 4:
  print("Designing Figure 4")
  if (mainCountry != "Paraguay") {
    figure04.fn()
  }
  
  # Figure 5:
  print("Designing Figure 5")
  if (mainCountry != "Paraguay") {
    figure05.fn()
  } else {
    figure05.fn(nchart = 2)
  }
  
  # Figure 6:
  print("Designing Figure 6")
  if (mainCountry != "Paraguay") {
    figure06.fn()
  } else {
    figure06.fn(nchart = 3)
  }
  
  # Figure 4 for Paraguay
  if (mainCountry == "Paraguay") {
    figure04_PRY.fn()
  }
  
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##   Section 2                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # All the figure functions from this section are contained in the "Code/S02.R" file. Please refer to it
  # for more information.
  
  # Figure 5 for Paraguay
  if (mainCountry == "Paraguay") {
    figure05_A_PRY.fn()
    figure05_B_PRY.fn()
  }
  
  # Figure 7:
  print("Designing Figure 7")
  if (mainCountry != "Paraguay") {
    figure07.fn()
  }
  
  # Figure 8:
  print("Designing Figure 8")
  if (mainCountry != "Paraguay") {
    figure08.fn()
  }
  
  # Figure 9:
  print("Designing Figure 9")
  if (mainCountry != "Paraguay") {
    figure09.fn()
  }
  
  # Figure 10:
  print("Designing Figure 10")
  if (mainCountry != "Paraguay" & args[1] != "carib1" & args[1] != "carib2") {
    figure10.fn()
  } 
  if (args[1] == "carib1") { 
    figure10.fn(carib = TRUE)
  }
  if (mainCountry == "Paraguay") {
    figure10.fn(nchart = 6)
  }
  
  # Figure 11:
  print("Designing Figure 11")
  if (mainCountry != "Paraguay" & args[1] != "carib2") {
    figure11.fn()
  }
  if (args[1] == "carib2") {
    figure11.fn(nchart = 10)
  }
  
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##   Section 3                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # All the figure functions from this section are contained in the "Code/S03.R" file. Please refer to it
  # for more information.
  
  # Figure 12:
  print("Designing Figure 12")
  if (mainCountry != "Paraguay" & args[1] != "carib2") {
    figure12_1.fn()
    figure12_2.fn()
    
  } 
  if (args[1] == "carib2") {
    figure12_1.fn(nchart = 11)
    figure12_2.fn(nchart = 11)
  }
  if (mainCountry == "Paraguay") {
     figure12_1.fn(nchart = 7) 
     figure12_2.fn(nchart = 7)               
  }
  
  # Figure 13:
  print("Designing Figure 13")
  if (mainCountry != "Paraguay" & args[1] != "carib2") {
    figure13_1.fn()
    figure13_2.fn()
    
  }
  if (args[1] == "carib2") {
    figure13_1.fn(nchart = 12)
    figure13_2.fn(nchart = 12)
  }
  if (mainCountry == "Paraguay") {
    figure08_A_PRY.fn()
    figure13_2.fn(nchart = 8) 
  }
  
  # Figure 14:
  print("Designing Figure 14")
  if (mainCountry != "Paraguay" & args[1] != "carib2") {
    figure14.fn()
  }
  if (args[1] == "carib2") {
    figure14.fn(nchart = 13)
  }
  if (mainCountry == "Paraguay") {
    figure09_PRY.fn()
  }
  
  # Figure 15:
  print("Designing Figure 15")
  if (mainCountry != "Paraguay" & args[1] != "carib2") {
    figure15.fn()
  }
  if (args[1] == "carib2") {
    figure15.fn(nchart = 14)
  }
  if (mainCountry == "Paraguay") {
    figure10_PRY.fn()
  }
  
  # Figure 16:
  print("Designing Figure 16")
  if (mainCountry != "Paraguay" & args[1] != "carib2") {
    figure16.fn()
  }
  if (args[1] == "carib2") {
    figure16.fn(nchart = 15)
  }
  if (mainCountry == "Paraguay") {
    figure11_PRY.fn()
  }
  
  # Figure 17:
  print("Designing Figure 17")
  if (mainCountry != "Paraguay" & args[1] != "carib2") {
    figure17.fn()
  }
  if (args[1] == "carib2") {
    figure17.fn(nchart = 16)
  }
  
  # Figure 18:
  print("Designing Figure 18")
  if (mainCountry != "Paraguay" & args[1] != "carib1" & args[1] != "carib2") {
    figure18.fn()
  }
  if (mainCountry == "Paraguay") {
    figure18.fn(nchart = 12)
  }
  
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##   Methodology                                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # Printing Methodology page
  print("Compiling Methodology Page")
  create_methodPage.fn()
  
  # Moving HTML to corresponding directory
  file.copy(from = "Code/method.md", 
            to   = file.path("Outputs", 
                             str_replace(mainCountry, " ", "_"),
                             "Method",
                             paste0(mainCountry, "_method.md"),
                             fsep = "/"), 
            overwrite = T)
  unlink("Code/method.md")

}
