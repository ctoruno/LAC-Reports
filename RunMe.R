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
## This version:      November 28th, 2022
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

# Just write down the name of the country and run the whole script
mainCountry <- "Colombia"

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

# Subsetting data for the current report
data_subset.df <- master_data.df %>%
  filter(country %in% countrySet) %>%
  
  # Latest year is different for Paraguay
  mutate(latestYear = if_else(country == "Paraguay", 2021, 2022))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Section 1                                                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# All the figure functions from this section are contained in the "Code/S01.R" file. Please refer to it
# for more information.

# Figure 1:
figure01.fn()

# Figure 2:
# Dropped from Outline

# Figure 3:
figure03.fn()

# Figure 4:
figure04.fn()

# Figure 5:
figure05.fn()

# Figure 6:
figure06.fn()

# Figure 7:
figure07.fn()


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##   Section 2                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# All the figure functions from this section are contained in the "Code/S02.R" file. Please refer to it
# for more information.

# Figure 8:
figure08.fn()

# Figure 9:
figure09.fn()

# Figure 10:

# Figure 11:
figure11.fn()

# Figure 12:
figure12.fn()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##   Section 3                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# All the figure functions from this section are contained in the "Code/S03.R" file. Please refer to it
# for more information.

# Figure 13.1:
figure13_1.fn()

# Figure 13.2:
figure13_2.fn()

# Figure 14.1:

# Figure 14.2:
figure14_2.fn()

# Figure 15:
figure15.fn()

# Figure 16:
figure16.fn()

# Figure 17:

# Figure 18:
figure18.fn()


