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
## This version:      March 23rd, 2023
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
} else if (args[1] == "usa") {
  master_data.df <- read_dta("Data/G7 - Merged.dta")
  
} else {
  master_data.df <- read_dta("Data/LAC - Merged.dta")
}
    
# Loading Methodology values
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
if (args[1] == "usa") {
  group <- g7.ls
}

# Subsetting data for the current report
if (args[1] != "usa"){
  data_subset.df <- master_data.df %>%
    filter(country %in% group) %>%
    
    # Latest year is different for Paraguay
    mutate(latestYear = if_else(country == "Paraguay", 2021, 2022)) %>%
    mutate(year = if_else(country == "Nicaragua" & year == 2021, NA_real_, year))
  
} else {
  data_subset.df <- master_data.df %>%
    mutate(latestYear = case_when(
      country == "United States" ~ 2021,
      country %in% c("Japan", "France", "Germany", "United Kingdom") ~ 2018,
      country %in% c("Canada", "Italy") ~ 2017
    ))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Loop Prologue                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

for (mainCountry in group) {
  
  if (mainCountry %in% c("Japan", "France", "Germany", "United Kingdom", "Canada", "Italy")) {
    next
  }
  
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
  
  # Figure 1: Perceptions of Authoritarian Behaviors
  if (! mainCountry %in% c("Paraguay", "Suriname", "Nicaragua", "United States")) {
    print("Designing Figure 1")
    figure01.fn()
  }
  
  # Figure 2: Perceptions of Authoritarian Behavior, by Support for the Current Administration
  if (! mainCountry %in% c("Paraguay", "Suriname", centralAmerica.ls, "United States")) {
    print("Designing Figure 2")
    figure02.fn()
  }
  
  # Figure 3: Attitudes Towards Authoritarianism and Rule of Law
  if (! mainCountry %in% c("Paraguay", "Suriname", "Nicaragua", "United States")) {
    print("Designing Figure 3")
    figure03.fn()
  } 
  if (mainCountry %in% c("Paraguay", "United States")) {
    print("Designing Figure 1")
    figure03.fn(nchart = 1, PAR = T)
  }

  # Figure 4: Fundamental Freedoms Over Time
  if (! mainCountry %in% c("Paraguay", "Suriname", "Nicaragua", "United States")) {
    print("Designing Figure 4")
    figure04.fn()
  }
  if (mainCountry %in% c("Suriname", "Nicaragua")) {
    print("Designing Figure 1")
    figure04.fn(nchart = 1)
  }
  if (mainCountry %in% c("United States")) {
    print("Designing Figure 2")
    figure04.fn(nchart = 2)
  }
  
  # Figure 5: Perceptions of Fundamental Freedoms in [REGION]
  if (! mainCountry %in% c("Paraguay", "Suriname", "Nicaragua", "United States")) {
    print("Designing Figure 5")
    figure05.fn()
  }
  if (mainCountry %in% c("Paraguay", "Suriname", "Nicaragua")) {
    print("Designing Figure 2")
    figure05.fn(nchart = 2)
  }
  if (mainCountry %in% c("United States")) {
    print("Designing Figure 3")
    figure05.fn(nchart = 3)
  }
  
  # Figure 6: Perceptions of Accountability in [REGION]
  if (mainCountry != "Nicaragua"){
    if (! mainCountry %in% c("Paraguay", "Suriname", "United States")) {
      print("Designing Figure 6")
      figure06.fn()
    }
    if (mainCountry %in% c("Paraguay", "Suriname")){
      print("Designing Figure 3")
      figure06.fn(nchart = 3)
    } 
    if (mainCountry %in% c("United States")){
      print("Designing Figure 4")
      figure06.fn(nchart = 4)
    } 
  }
  
  # ALT Figure for Paraguay, Suriname and the USA: Perceptions of Constraints on Government Powers
  if (mainCountry %in% c("Paraguay", "Suriname")) {
    print("Designing Figure 4")
    figure04_PRY.fn()
  }
  if (mainCountry %in% c("United States")) {
    print("Designing Figure 5")
    figure04_PRY.fn(nchart = 5)
  }
  
  # ALT Figure for the USA: Government Accountability and Rule of Law, by Political Affiliation
  if (mainCountry %in% c("United States")) {
    print("Designing Figure 6")
    # figure06_USA.fn()
  }
  
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##   Section 2                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # All the figure functions from this section are contained in the "Code/S02.R" file. Please refer to it
  # for more information.
  
  # ALT Figure 5 for Paraguay: Trust in Country Members
  if (mainCountry == "Paraguay") {
    print("Designing Figure 5")
    figure05_A_PRY.fn()
    figure05_B_PRY.fn()
  }
  
  # Figure 7: Perceptions of Corruption by Institution Over Time
  if (! mainCountry %in% c("Paraguay", "Suriname", "Nicaragua")) {
    print("Designing Figure 7")
    figure07.fn()
  }
  if (mainCountry == "Nicaragua") {
    print("Designing Figure 4")
    figure07.fn(nchart = 4)
  }
  if (mainCountry == "Suriname") {
    print("Designing Figure 5")
    figure07.fn(nchart = 5)
  }
  
  # Figure 8: Perceptions of Corruption in [REGION], by Institution
  if (! mainCountry %in% c("Paraguay", "Suriname", "Nicaragua")) {
    print("Designing Figure 8")
    figure08.fn()
  }
  if (mainCountry == "Nicaragua") {
    print("Designing Figure 5")
    figure08.fn(nchart = 5)
  }
  if (mainCountry == "Suriname") {
    print("Designing Figure 6")
    figure08.fn(nchart = 6)
  }
  
  # Figure 9: Attitudes Towards Corrupt Behaviors in [REGION]
  if (! mainCountry %in% c("Paraguay", "Suriname", "Nicaragua", "United States")) {
    print("Designing Figure 9")
    figure09.fn()
  }
  if (mainCountry == "Nicaragua") {
    print("Designing Figure 6")
    figure09.fn(nchart = 6)
  }
  if (mainCountry == "Suriname") {
    print("Designing Figure 7")
    figure09.fn(nchart = 7)
  }
  
  # Figure 10: Bribery Victimization
  if (args[1] != "carib1" & args[1] != "carib2") {
    if (mainCountry != "Paraguay" & mainCountry != "Nicaragua" & mainCountry != "United States") {
      print("Designing Figure 10")
      figure10.fn()
    }
    if (mainCountry == "Paraguay") {
      print("Designing Figure 6")
      figure10.fn(nchart = 6)
    }
    if (mainCountry == "Nicaragua") {
      print("Designing Figure 7")
      figure10.fn(nchart = 7)
    }
    if (mainCountry == "United States") {
      print("Designing Figure 9")
      figure10.fn(nchart = 9)
    }
    
  } else {
    if (args[1] == "carib1" & mainCountry != "Suriname") { 
      print("Designing Figure 10")
      figure10.fn(carib = TRUE)
    }
    if (mainCountry == "Suriname") { 
      print("Designing Figure 8")
      figure10.fn(nchart = 8, 
                  carib  = TRUE)
    }
  }
  
  # Figure 11: Trust in Institutions Over Time
  if (args[1] != "carib2" & mainCountry != "United States") {
    if (! mainCountry %in% c("Paraguay", "Suriname", "Nicaragua")) {
      print("Designing Figure 11")
      figure11.fn()
    }
    if (mainCountry == "Suriname") {
      print("Designing Figure 9")
      figure11.fn(nchart = 9)
    }
    
  } else {
    print("Designing Figure 10")
    figure11.fn(nchart = 10)
  }
  
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##   Section 3                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # All the figure functions from this section are contained in the "Code/S03.R" file. Please refer to it
  # for more information.
  
  # Figure 12: Crime Victimization
  if (args[1] != "carib2" & args[1] != "central" & args[1] != "usa"){
    if (mainCountry != "Paraguay" & mainCountry != "Suriname") {
      print("Designing Figure 12")
      figure12_1.fn()
      figure12_2.fn()
    } 
  }
  if (mainCountry == "Costa Rica") {
    print("Designing Figure 12")
    figure12_1.fn()
    figure12_2.fn()
  }
  if(args[1] == "central" & mainCountry != "Nicaragua" & mainCountry != "Costa Rica") {
    print("Designing Figure 12")
    figure12_1.fn(nchart = 12)
    figure12_2_CA.fn(nchart = 12)
  }
  if (args[1] == "carib2" | mainCountry == "United States") {
    print("Designing Figure 11")
    figure12_1.fn(nchart = 11)
    # figure12_2.fn(nchart = 11)
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
  if (mainCountry == "Paraguay") {
    print("Designing Figure 7")
    figure12_1.fn(nchart = 7) 
    figure12_2.fn(nchart = 7)               
  }
  
  # Figure 13: Security
  if (args[1] != "carib2" & args[1] != "usa"){
    if (mainCountry != "Paraguay" & mainCountry != "Suriname" & mainCountry != "Nicaragua") {
      print("Designing Figure 13")
      figure13_1.fn()
      figure13_2.fn()
    } 
  }
  if (args[1] == "carib2" | mainCountry == "United States") {
    print("Designing Figure 12")
    figure13_1.fn(nchart = 12)
    figure13_2.fn(nchart = 12)
  }
  if (mainCountry == "Suriname") {
    print("Designing Figure 11")
    figure13_1.fn(nchart = 11)
    figure13_2.fn(nchart = 11)
  }
  if (mainCountry == "Paraguay") {
    print("Designing Figure 8")
    figure08_A_PRY.fn()
    figure13_2.fn(nchart = 8) 
  }
  if (mainCountry == "Nicaragua") {
    print("Designing Figure 9")
    figure13_1.fn(nchart = 9)
    figure13_2.fn(nchart = 9)
  }
  
  # Figure 14: Perceptions of the Criminal Justice System in the USA
  if (args[1] != "carib2" & args[1] != "usa"){
    if (mainCountry != "Paraguay" & mainCountry != "Suriname" & mainCountry != "Nicaragua") {
      print("Designing Figure 14")
      figure14.fn()
    }
  }
  if (args[1] == "carib2" | mainCountry == "United States") {
    print("Designing Figure 13")
    figure14.fn(nchart = 13)
  }
  if (mainCountry == "Suriname") {
    print("Designing Figure 12")
    figure14.fn(nchart = 12)
  }
  if (mainCountry == "Paraguay") {
    print("Designing Figure 9")
    figure09_PRY.fn()
  }
  
  # Figure 15: Criminal Justice Actors
  if (args[1] != "carib2" & args[1] != "usa"){
    if (mainCountry != "Paraguay" & mainCountry != "Suriname" & mainCountry != "Nicaragua") {
      print("Designing Figure 15")
      figure15.fn()
    }
  }
  if (args[1] == "carib2" | mainCountry == "United States") {
    print("Designing Figure 14")
    figure15.fn(nchart = 14)
  }
  if (mainCountry == "Suriname") {
    print("Designing Figure 13")
    figure15.fn(nchart = 13)
  }
  if (mainCountry == "Paraguay") {
    print("Designing Figure 10")
    figure10_PRY.fn()
  }
  
  # Figure 16: Perceptions of the Police
  if (args[1] != "carib2" & args[1] != "usa"){
    if (mainCountry != "Paraguay" & mainCountry != "Suriname" & mainCountry != "Nicaragua") {
      print("Designing Figure 16")
      figure16.fn()
    }
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
  
  # Figure 16 - Police Interactions for Central America
  if (args[1] == "central" & mainCountry != "Nicaragua"){
    print("Designing Second Part of Figure 16")
    figure16B_CA.fn()
  }
  
  # Figure 17: Perceptions of the Treatment of Crime Victims
  if (args[1] != "carib2" & args[1] != "usa"){
    if (mainCountry != "Paraguay" & mainCountry != "Suriname" & mainCountry != "Nicaragua") {
      print("Designing Figure 17")
      figure17.fn()
    } 
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
  if (args[1] != "carib1" & args[1] != "carib2" & args[1] != "usa"){
    if (mainCountry != "Paraguay" & mainCountry != "Nicaragua") {
      print("Designing Figure 18")
      figure18.fn()
    } 
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
    
    # Figure 19: Internal Migration
    print("Designing Figure 19")
    figure19A.fn()
    figure19B.fn()
    
    # Figure 20: International Migration
    print("Designing Figure 20")
    figure20A.fn()
    figure20B.fn()
    figure20C.fn()
    
    # Figure 21: Path to the USA
    print("Designing Figure 21")
    figure21A.fn()
    figure21B.fn()
    
    # Figure 22: Migartion to the USA
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
  
  if (args[1] != "usa"){
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

}
