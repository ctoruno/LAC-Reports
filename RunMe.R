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
source("Code/S05.R")
source("Code/methodology.R")

# Loading plotting functions from GitHub
source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/loading.R")
loadVIZ(set = "LAC")

# Loading data
if (args[1] == "central") {
  master_data.df <- read_dta("Data/LAC - Merged (with CA).dta")
  boundaries.sf  <- st_read(paste0(path2SP,
                                   "Data/GeoBoundaries/geoBoundariesCGAZ_ADM1.shp"))
  map_data.ls  <- list("USA_map"      = st_read("Data/USA_boundaries.csv",
                                                crs = 4326),
                       "BorderPoints" = st_read("Data/USA_borderPoints.csv",
                                                crs = 4326),
                       "CAmapdata"    = st_read("Data/CA_data4maps.csv",
                                                crs = 4326))
    
} else {
  master_data.df <- read_dta("Data/LAC - Merged.dta")
}

# Loading Metholodology values
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
  if (args[1] == "andean") {
    names(countryPalette) <- c("Colombia", "Bolivia", "Ecuador", "Peru")
  } else if (args[1] == "south") {
    names(countryPalette) <- c("Paraguay", "Argentina", "Brazil")
  } else {
    names(countryPalette) <- group
  }
  
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
  if (! mainCountry %in% c("Paraguay", "Suriname", "Nicaragua")) {
    print("Designing Figure 1")
    figure01.fn()
  }
  
  # Figure 2:
  if (! mainCountry %in% c("Paraguay", "Suriname", "Nicaragua")) {
    print("Designing Figure 2")
    figure02.fn()
  }
  
  # Figure 3:
  if (! mainCountry %in% c("Paraguay", "Suriname", "Nicaragua")) {
    print("Designing Figure 3")
    figure03.fn()
  } 
  if (mainCountry == "Paraguay") {
    print("Designing Figure 1")
    figure03.fn(nchart = 1)
  }

  # Figure 4:
  if (! mainCountry %in% c("Paraguay", "Suriname", "Nicaragua")) {
    print("Designing Figure 4")
    figure04.fn()
  }
  if (mainCountry %in% c("Suriname", "Nicaragua")) {
    print("Designing Figure 1")
    figure04.fn(nchart = 1)
  }
  
  # Figure 5:
  if (! mainCountry %in% c("Paraguay", "Suriname", "Nicaragua")) {
    print("Designing Figure 5")
    figure05.fn()
  } else {
    print("Designing Figure 2")
    figure05.fn(nchart = 2)
  }
  
  # Figure 6:
  if (! mainCountry %in% c("Paraguay", "Suriname", "Nicaragua")) {
    print("Designing Figure 6")
    figure06.fn()
  } else {
    print("Designing Figure 3")
    figure06.fn(nchart = 3)
  }
  
  # Figure 4 for Paraguay and Suriname
  if (mainCountry %in% c("Paraguay", "Suriname")) {
    print("Designing Figure 4")
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
    print("Designing Figure 5")
    figure05_A_PRY.fn()
    figure05_B_PRY.fn()
  }
  
  # Figure 7:
  if (! mainCountry %in% c("Paraguay", "Suriname", "Nicaragua")) {
    print("Designing Figure 7")
    figure07.fn()
  }
  if (mainCountry == "Suriname") {
    print("Designing Figure 5")
    figure07.fn(nchart = 5)
  }
  if (mainCountry == "Nicaragua") {
    print("Designing Figure 4")
    figure07.fn(nchart = 4)
  }
  
  # Figure 8:
  if (! mainCountry %in% c("Paraguay", "Suriname", "Nicaragua")) {
    print("Designing Figure 8")
    figure08.fn()
  }
  if (mainCountry == "Suriname") {
    print("Designing Figure 6")
    figure08.fn(nchart = 6)
  }
  if (mainCountry == "Nicaragua") {
    print("Designing Figure 5")
    figure08.fn(nchart = 5)
  }
  
  # Figure 9:
  if (! mainCountry %in% c("Paraguay", "Suriname", "Nicaragua")) {
    print("Designing Figure 9")
    figure09.fn()
  }
  if (mainCountry == "Suriname") {
    print("Designing Figure 7")
    figure09.fn(nchart = 7)
  }
  if (mainCountry == "Nicaragua") {
    print("Designing Figure 6")
    figure09.fn(nchart = 6)
  }
  
  # Figure 10:
  if (mainCountry != "Paraguay" & args[1] != "carib1" & args[1] != "carib2" & mainCountry != "Nicaragua") {
    print("Designing Figure 10")
    figure10.fn()
  } 
  if (args[1] == "carib1" & mainCountry != "Suriname") { 
    print("Designing Figure 10")
    figure10.fn(carib = TRUE)
  }
  if (mainCountry == "Suriname") { 
    print("Designing Figure 8")
    figure10.fn(nchart = 8, 
                carib  = TRUE)
  }
  if (mainCountry == "Paraguay") {
    print("Designing Figure 6")
    figure10.fn(nchart = 6)
  }
  if (mainCountry == "Nicaragua") {
    print("Designing Figure 7")
    figure10.fn(nchart = 7)
  }
  
  # Figure 11:
  if (mainCountry != "Paraguay" & mainCountry != "Suriname" & args[1] != "carib2" & mainCountry != "Nicaragua") {
    print("Designing Figure 11")
    figure11.fn()
  }
  if (args[1] == "carib2") {
    print("Designing Figure 10")
    figure11.fn(nchart = 10)
  }
  if (mainCountry == "Suriname") {
    print("Designing Figure 9")
    figure11.fn(nchart = 9)
  }
  
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##   Section 3                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # All the figure functions from this section are contained in the "Code/S03.R" file. Please refer to it
  # for more information.
  
  # Figure 12:
  if (mainCountry != "Paraguay" & mainCountry != "Suriname" & args[1] != "carib2" & args[1] != "central") {
    print("Designing Figure 12")
    figure12_1.fn()
    figure12_2.fn()
  } 
  if(args[1] == "central" & mainCountry != "Nicaragua") {
    print("Designing Figure 12")
    figure12_1.fn(nchart = 12)
    figure12_2_CA.fn(nchart = 12)
  }
  if (args[1] == "carib2") {
    print("Designing Figure 11")
    figure12_1.fn(nchart = 11)
    figure12_2.fn(nchart = 11)
  }
  if (mainCountry == "Paraguay") {
    print("Designing Figure 7")
     figure12_1.fn(nchart = 7) 
     figure12_2.fn(nchart = 7)               
  }
  if (mainCountry == "Suriname") {
    print("Designing Figure 10")
    figure12_1.fn(nchart = 10) 
    figure12_2.fn(nchart = 10)               
  }
  if (mainCountry == "Nicaragua") {
    print("Designing Figure 8")
    figure12_1.fn(nchart = 8) 
    figure12_2_CA.fn(nchart = 8)               
  }
  
  # Figure 13:
  if (mainCountry != "Paraguay" & mainCountry != "Suriname" & args[1] != "carib2" & mainCountry != "Nicaragua") {
    print("Designing Figure 13")
    figure13_1.fn()
    figure13_2.fn()
  }
  if (args[1] == "carib2") {
    print("Designing Figure 12")
    figure13_1.fn(nchart = 12)
    figure13_2.fn(nchart = 12)
  }
  if (mainCountry == "Paraguay") {
    print("Designing Figure 8")
    figure08_A_PRY.fn()
    figure13_2.fn(nchart = 8) 
  }
  if (mainCountry == "Suriname") {
    print("Designing Figure 11")
    figure13_1.fn(nchart = 11)
    figure13_2.fn(nchart = 11)
  }
  if (mainCountry == "Nicaragua") {
    print("Designing Figure 9")
    figure13_1.fn(nchart = 9)
    figure13_2.fn(nchart = 9)
  }
  
  # Figure 14:
  if (mainCountry != "Paraguay" & mainCountry != "Suriname" & args[1] != "carib2" & mainCountry != "Nicaragua") {
    print("Designing Figure 14")
    figure14.fn()
  }
  if (args[1] == "carib2") {
    print("Designing Figure 13")
    figure14.fn(nchart = 13)
  }
  if (mainCountry == "Paraguay") {
    print("Designing Figure 9")
    figure09_PRY.fn()
  }
  if (mainCountry == "Suriname") {
    print("Designing Figure 12")
    figure14.fn(nchart = 12)
  }
  
  # Figure 15:
  if (mainCountry != "Paraguay" & mainCountry != "Suriname" & args[1] != "carib2" & mainCountry != "Nicaragua") {
    print("Designing Figure 15")
    figure15.fn()
  }
  if (args[1] == "carib2") {
    print("Designing Figure 14")
    figure15.fn(nchart = 14)
  }
  if (mainCountry == "Paraguay") {
    print("Designing Figure 10")
    figure10_PRY.fn()
  }
  if (mainCountry == "Suriname") {
    print("Designing Figure 13")
    figure15.fn(nchart = 13)
  }
  
  # Figure 16:
  if (mainCountry != "Paraguay" & mainCountry != "Suriname" & args[1] != "carib2" & args[1] != "central" & mainCountry != "Nicaragua") {
    print("Designing Figure 16")
    figure16.fn()
  }
  if (args[1] == "central"){
    print("Designing Figure 16")
    figure16.fn()
    figure16B_CA.fn()
  }
  if (args[1] == "carib2") {
    print("Designing Figure 15")
    figure16.fn(nchart = 15)
  }
  if (mainCountry == "Paraguay") {
    print("Designing Figure 11")
    figure11_PRY.fn()
  }
  if (mainCountry == "Suriname") {
    print("Designing Figure 14")
    figure16.fn(nchart = 14)
  }
  
  # Figure 17:
  print("Designing Figure 17")
  if (mainCountry != "Paraguay" & mainCountry != "Suriname" & args[1] != "carib2" & mainCountry != "Nicaragua") {
    figure17.fn()
  }
  if (args[1] == "carib2") {
    figure17.fn(nchart = 16)
  }
  if (mainCountry == "Suriname") {
    print("Designing Figure 15")
    figure17.fn(nchart = 15)
  }
  
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##   Section 4                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # All the figure functions from this section are contained in the "Code/S03.R" file. Please refer to it
  # for more information.
  
  # Figure 18:
  if (mainCountry != "Paraguay" & args[1] != "carib1" & args[1] != "carib2" & mainCountry != "Nicaragua") {
    print("Designing Figure 18")
    figure18.fn()
  }
  if (mainCountry == "Paraguay") {
    print("Designing Figure 12")
    figure18.fn(nchart = 12)
  }
  
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##   Section 5                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # All the figure functions from this section are contained in the "Code/S05.R" file. Please refer to it
  # for more information.  
  
  if (args[1] == "central" & mainCountry != "Costa Rica" & mainCountry != "Nicaragua") {
    
    # Figure 19
    print("Designing Figure 19")
    figure19A.fn()
    figure19B.fn()
    
    # Figure 20
    print("Designing Figure 20")
    figure20A.fn()
    figure20B.fn()
    figure20C.fn()
    
    # Figure 21
    
    print("Designing Figure 21")
    figure21A.fn()
    figure21B.fn()
    
    # Figure 22
    print("Designing Figure 22")
    figure22A.fn()
    figure22B.fn()
    figure22C.fn()
    
  }
  
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##   Methodology                                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # All the figure functions from this section are contained in the "Code/methodology.R" file. Please refer to
  # it for more information.
  
  # Printing Methodology page
  print("Compiling Methodology Page")
  create_methodPage.fn()
  
  # Moving HTML to corresponding directory
  file.copy(from = "Code/method.md", 
            to   = file.path("Outputs", 
                             str_replace_all(mainCountry, " ", "_"),
                             "Method",
                             paste0(mainCountry, "_method.md"),
                             fsep = "/"), 
            overwrite = T)
  unlink("Code/method.md")

}
