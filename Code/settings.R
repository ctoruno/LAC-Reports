## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Country Reports - Settings
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 17th, 2022
##
## This version:      December 7th, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Required packages                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required packages
library(pacman)

# Notes: ggradar2  and ggsankey need to be installed from Github's developer version. Run the following 
# lines of code in order to install:
# remotes::install_github("xl0418/ggradar2@9ebc7b6a34462b356e2893d4c77725b70bed5546")
# devtools::install_github("davidsjoberg/ggsankey")

p_load(char = c(
  # Visualizations
  "showtext", "ggtext", "ggsankey", "ggrepel", "ggplotify", "gridExtra", "ggradar2",
  
  # Data Loading
  "haven", "readxl", "writexl",
  
  # Good 'ol Tidyverse
  "tidyverse",
  
  # Logit
  
  "margins",
  
  #Patchwork
  
  "patchwork"
  
  
))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  SharePoint Path                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SharePoint path
if (Sys.info()["user"] == "carlostorunopaniagua") {
  path2SP <- paste0("/Users/carlostorunopaniagua/OneDrive - World Justice Project/Data Analytics/")
  
} else if (Sys.info()["user"] == "santiagopardo") {
  path2SP <- paste0("/Users/santiagopardo/OneDrive - World Justice Project/Data Analytics/")
  
} else if (Sys.info()["user"] == "jaeheelee"){
  path2SP <- paste0("/Users/jaeheelee/Library/CloudStorage/OneDrive-SharedLibraries-WorldJusticeProject/",
                    "Research - Data Analytics/")
  
} else{
  path2SP <- "PLEASE INSERT YOUR PERSONAL PATH TO THE  WJP - DATA ANALYTICS DIRECTORY"
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Fonts                                                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Loading fonts
path2fonts<- paste0(path2SP, "6. Country Reports/0. Fonts/")
font_add(family     = "Lato Full",
         regular    = paste0(path2fonts, "Lato-Regular.ttf"),
         italic     = paste0(path2fonts, "Lato-LightItalic.ttf"),
         bold       = paste0(path2fonts, "Lato-Bold.ttf"),
         bolditalic = paste0(path2fonts, "Lato-BoldItalic.ttf"))
font_add(family  = "Lato Light",
         regular = paste0(path2fonts, "Lato-Light.ttf"))
font_add(family  = "Lato Black",
         regular = paste0(path2fonts, "Lato-Black.ttf"))
font_add(family  = "Lato Black Italic",
         regular = paste0(path2fonts, "Lato-BlackItalic.ttf"))
font_add(family  = "Lato Medium",
         regular = paste0(path2fonts, "Lato-Medium.ttf"))
showtext_auto()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  WJP theme                                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Defining a ggplot WJP theme
WJP_theme <- function() {
  theme(panel.background   = element_rect(fill = "white",
                                          size = 2),
        panel.grid.major   = element_line(size     = 0.25,
                                          colour   = "#5e5c5a",
                                          linetype = "dashed"),
        panel.grid.minor   = element_blank(),
        axis.title.y       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(0, 10, 0, 0)),
        axis.title.x       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(10, 0, 0, 0)),
        axis.text.y        = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C"),
        axis.text.x = element_text(family = "Lato Full",
                                   face   = "plain",
                                   size   = 3.514598*.pt,
                                   color  = "#524F4C"),
        axis.ticks  = element_blank(),
        plot.margin  = unit(c(0, 0, 0, 0), "points")
  ) 
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5.  Color Palette                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

mainCOLOR      <- c("#2a2a9A")
countryPalette <- c("#2a2a94", "#a90099", "#3273ff", "#f0585f", "#b374ff", "#43a9a7", "#efa700", "#2c6d4f")
binPalette     <- c("#003b8a", "#fa4d57")
barsPalette    <- c("#2a2a9A", "#E2E2F7")
glinesPalette  <- c("#2a2a94", "#a90099", "#3273ff")
rosePalette    <- c("#20204a", "#12006b", "#2e2e95", "#4e43dd", "#756ef9", "#9c94ff", "#b1a6ff",
                    "#cfb3ff", "#e2a4ff", "#f2aadc", "#ffd7f5")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 6.  Groups of countries                                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Defining country groups
eastCaribbean.ls <- c(
  "Antigua and Barbuda", 
  "Barbados",
  "Dominica",
  "Grenada",
  "St. Kitts and Nevis",
  "St. Lucia",
  "St. Vincent and the Grenadines",
  "Trinidad and Tobago"
)

westCaribbean_and_guianas.ls <- c(
  "Bahamas",
  "Dominican Republic",
  "Guyana",
  "Haiti",
  "Jamaica",
  "Suriname"
)

southCone.ls <- c(
  "Argentina",
  "Brazil",
  "Paraguay"
)

andeanCountries.ls <- c(
  "Bolivia",
  "Colombia",
  "Ecuador",
  "Peru"
)

centralAmerica.ls <- c(
  "Belize",
  "Costa Rica",
  "El Salvador",
  "Guatemala",
  "Honduras",
  "Nicaragua",
  "Panama"
)

# Which country group is gonna be the comparison group?
map(
  # We define a list of lists with all possible groups of countries
  list(eastCaribbean.ls,
       westCaribbean_and_guianas.ls,
       southCone.ls,
       andeanCountries.ls,
       centralAmerica.ls),
  
  # We define a matching function
  function(country_group){
    
    # IF the mainCountry is within the corresponding list of countries then
    if (!is.na(match(mainCountry, country_group))) {
      
      # We set the corresponding list as the comparison group (minus the mainCountry value)
      comparison_countries.ls <<- country_group[-match(mainCountry, country_group)]
      
    }
  }
)

# Country Set to include in the reports
countrySet <- c(mainCountry, comparison_countries.ls)

# Updating the country color palette to match the length of the group of countries
countryPalette <- countryPalette[1:4]
names(countryPalette) <- countrySet

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 7.  Creating directories for Outputs                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Defining a function to delete previous outputs and create the country directory
ordnung.fn <- function(targetCountry){
  
  # Defining the country directory in the Outputs
  outPath <- file.path("Outputs", 
                       str_replace(targetCountry, " ", "_"))
  
  # Listing previous outputs
  prevOutputs <- list.files(outPath, 
                            include.dirs = F, 
                            full.names   = T, 
                            recursive    = T)
  
  # Deleting previous outputs
  file.remove(prevOutputs)
  
  # Creating country folder within the Outputs directory
  dir.create(file.path("Outputs", 
                       str_replace(mainCountry, " ", "_")), 
             showWarnings = FALSE)
  
  # Creating folders for each chart output within the country directory
  for (plot in 1:19) {
    dir.create(file.path("Outputs", 
                         str_replace(mainCountry, " ", "_"),
                         paste0("figure_", plot),
                         fsep = "/"), 
               showWarnings = FALSE)
  }
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 8.  Creating a saving function                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

saveIT.fn <- function(chart, n, suffix = NULL, w, h) {
  ggsave(plot   = chart,
         file   = file.path("Outputs", 
                            str_replace(mainCountry, " ", "_"),
                            paste0("figure_", n),
                            paste0("figure_", n, suffix, ".svg"),
                            fsep = "/"), 
         width  = w, 
         height = h,
         units  = "mm",
         dpi    = 72,
         device = "svg")
} 

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 9.  Creating a to_percentage function                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

to_percentage.fn <- function(value){
  perc <- paste0(format(round(value, 0),
                        nsmall = 0),
                 "%")
  
  return(perc)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 10.  Incumbent political parties                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

incumbentPP.ls <- list(
  "Antigua and Barbuda"              = "Antigua and Barbuda Labor Party or ABLP", 
  "Barbados"                         = "Barbados Labour Party (BLP)",
  "Dominica"                         = "The Dominica Labour Party (DLP)",
  "Grenada"                          = "National Democratic Congress (NDC)",
  "St. Kitts and Nevis"              = "Labour Party  (SKNLP)",
  "St. Lucia"                        = "St. Lucia Labour Party (SLP)",
  "St. Vincent and the Grenadines"   = "Unity Labour Party (ULP)",
  "Trinidad and Tobago"              = "People National Movement",
  "Bahamas"                          = "Progressive Liberal Party (PLP)",
  "Dominican Republic"               = "Partido Revolucionario Moderno PRM",
  "Guyana"                           = "People's Progressive Party",
  "Haiti"                            = "Parti haïtien Tèt Kale (PHTK)",         # 18/507
  "Jamaica"                          = "Jamaica Labour Party (JLP)",
  "Suriname"                         = "VHP",                                   # 10/502
  "Argentina"                        = "IN FRONT OF EVERYONE|Peronism",
  "Brazil"                           = "PL- Partido Liberal",                   # 29/1,109
  "Paraguay"                         = "Partido Colorado (ANR)",
  "Bolivia"                          = "Movimiento al Socialismo",
  "Colombia"                         = "Partido Centro Democr",                 # 31/1,000
  "Ecuador"                          = paste0("CREO Movement|",
                                              "Christian Social Party (PSC)|",
                                              "Patriotic Society Party (PSP)"),
  "Peru"                             = "Party 1",                               # No info about political parties
  "Belize"                           = "PUP: People's United Party",            # No Data, re-check
  "Costa Rica"                       = paste0("Partido Social Democrático (PSD)|",
                                              "Unidos Podemos (UP)"),
  "El Salvador"                      = "Nuevas Ideas|GANA",                     # No Data, re-check
  "Guatemala"                        = "VAMOS",                                 # No Data re-check
  "Honduras"                         = "Partido Libertad y Refundación",        # No Data, re-check
  "Nicaragua"                        = "FSLN",                                  # No Data, re-check
  "Panama"                           = "Partido Revolucionario Democrático PRD" # No Data, re-check
)

  