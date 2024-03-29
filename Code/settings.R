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
  "showtext", "ggtext", "ggsankey", "ggrepel", "ggplotify", "gridExtra", "ggradar2", "patchwork", 
  "waffle", "ggh4x",
  
  # Data Loading
  "haven", "readxl", "writexl", "openxlsx",
  
  # GIS
  "tmaptools", "rmapshaper", "sf", "units", "rgeos",
  
  # Utilities
  "margins", "english", "quarto", "kableExtra",
  
  # Good 'ol Tidyverse
  "tidyverse"
  
))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  SharePoint Path                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SharePoint path
if (Sys.info()["user"] == "ctoruno") {
  path2SP <- paste0("/Users/ctoruno/OneDrive - World Justice Project/Data Analytics/")

} else if (Sys.info()["user"] == "santiagopardo") {
  path2SP <- paste0("/Users/santiagopardo/OneDrive - World Justice Project/Data Analytics/")
  
} else if (Sys.info()["user"] == "jaeheelee"){
  path2SP <- paste0("/Users/jaeheelee/Library/CloudStorage/OneDrive-SharedLibraries-WorldJusticeProject/",
                    "Research - Data Analytics/")
  
} else if (Sys.info()["user"] == "macbookprosolido") {
  path2SP <- paste0("/Users/macbookprosolido/Documents/WJP/")
  
} else{
  path2SP <- "PLEASE INSERT YOUR PERSONAL PATH TO THE  WJP - DATA ANALYTICS DIRECTORY"
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Fonts                                                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Loading fonts
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
font_add(family = "FontAwesome", 
         regular = "../../Fonts/fontawesome-webfont.ttf")
font_add(family = "FontAwesome5Free-Solid", 
         regular = "../../Fonts/fa-solid-900.ttf")
font_add(family = "FontAwesome 5 Brands", 
         regular = "../../Fonts/fa-brands-400.ttf")
showtext_auto()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  WJP theme                                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Defining a ggplot WJP theme
WJP_theme <- function() {
  theme(panel.background   = element_blank(),
        plot.background    = element_blank(),
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
countryPalette <- c("#2a2a94", "#a90099", "#3273ff", "#fa4d57", "#9d61f2", "#43a9a7", "#efa700", "#2c6d4f")
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
  #"Antigua and Barbuda", 
  "Barbados",
  "Dominica",
  "Grenada",
  #"St. Kitts and Nevis",
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

g7.ls <- c(
  "Canada",
  "France",
  "Germany",
  "Italy",
  "Japan",
  "United Kingdom",
  "United States"
)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 7.  Creating a function that will reset the Outputs directory                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Defining a function to delete previous outputs and create the country directory
ordnung.fn <- function(targetCountry){
  
  # Defining the country directory in the Outputs
  outPath <- file.path("Outputs", 
                       str_replace_all(targetCountry, " ", "_"))
  
  # Listing previous outputs
  prevOutputs <- list.files(outPath, 
                            include.dirs = F, 
                            full.names   = T, 
                            recursive    = T)
  
  # Deleting previous outputs
  file.remove(prevOutputs)
  
  # Creating country folder within the Outputs directory
  dir.create(file.path("Outputs", 
                       str_replace_all(mainCountry, " ", "_")), 
             showWarnings = FALSE)
  
  # Creating folders for each chart output within the country directory
  for (plot in 1:23) {
    dir.create(file.path("Outputs", 
                         str_replace_all(mainCountry, " ", "_"),
                         paste0("imgChart", plot),
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
                            str_replace_all(mainCountry, " ", "_"),
                            paste0("imgChart", n),
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
## 10.  Preparing order data in the logit                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

condition_categories <- function(main_data, group_var, name_var) {
  
  condition <-  main_data %>%
    group_by({{group_var}}) %>%
    summarise(N_obs = sum(counter, na.rm = T)) %>%
    ungroup() %>%
    mutate(variable = as.character({{name_var}})) %>%
    rename(category = {{group_var}}) %>%
    drop_na()
  
  return(condition)
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



  
