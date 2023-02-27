## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Country Reports - GIS & Geocoding
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     February 24th, 2023
##
## This version:      February 24th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  USA MAP                                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Subsetting and simplifying USA boundaries
USA.sf <- boundaries.sf %>%
  filter(shapeGroup == "USA") %>%
  filter(! shapeName %in% c("Alaska", "American Samoa", "Commonwealth of the Northern Mariana Islands",
                            "Guam", "United States Virgin Islands", "Puerto Rico", "Hawaii")) %>%
  rmapshaper::ms_simplify(keep = 0.1) %>%
  group_by(shapeGroup) %>%
  summarise()

# Saving simple feature
st_write(USA.sf, 
         "Data/USA_boundaries.csv",
         layer_options = "GEOMETRY=AS_WKT")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Central America Locations                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Estimating migration data per location
central_america.df <- master_data.df %>% 
  filter(country %in% c("Belize", "Guatemala", "Honduras", "El Salvador", "Panama")) %>%
  filter(year == 2022) %>%
  select(country, city, PSU, EXP_q31a, EXP22_q26b, EXP_q31d, EXP_q31f) %>%
  mutate(
    location = case_when(
      country == "Belize"      ~ paste0(city, ", ", country),
      country == "El Salvador" ~ paste0(str_squish(str_extract(PSU, "(?<=Seg\\s\\d{1,3}\\s-.{1,50}-).+?(?=-)")), 
                                        ", ", country),
      country == "Guatemala"   ~ paste0(PSU, ", ", country),
      # country == "Honduras"    ~ paste0(city, ", ", country),
      country == "Panama"      ~ paste0(str_squish(str_to_title(str_extract(PSU, "(?<=Seg\\s\\d{1,3}\\s-.{1,50}-).+?(?=-)"))), 
                                        ", ", country)
    ),
    EXP_q31a = case_when(
      EXP_q31a == 0 ~ 1,
      EXP_q31a == 1 ~ 0
    ),
    EXP22_q26b = case_when(
      EXP22_q26b == 1 ~ 1,
      EXP22_q26b == 0 ~ 0
    ),
    EXP_q31d = case_when(
      EXP_q31d == 1  ~ 1,
      EXP_q31d == 2  ~ 0
    ),
    EXP_q31f = case_when(
      EXP_q31f == 1  ~ 1,
      EXP_q31f == 0  ~ 0
    )
  ) %>% 
  group_by(location) %>%
  summarise(
    intmig_person = mean(EXP_q31a, na.rm = T),
    recent_intmig = mean(EXP22_q26b, na.rm = T),
    desire2emig   = mean(EXP_q31d, na.rm = T),
    plans2emig    = mean(EXP_q31f, na.rm = T)
  )

# Geocoding locations
coordinates.sf <- geocode_OSM(central_america.df$location,
                              projection = 4326,
                              as.sf = T)

# Creating simple feature for data
central_america.sf <- central_america.df %>%
  left_join(coordinates.sf %>%
              st_drop_geometry() %>%
              select(query, x, y),
            by = c("location" = "query")) %>%
  st_as_sf(coords  = c("x", "y"),
           remove  = F,
           na.fail = F)
  
