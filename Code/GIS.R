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

# Geocoding USA entry points
entry_points <- c("San Diego, United States", 
                  "El Centro, Imperial County, United States",
                  "Yuma, United States",
                  "Tucson, United States",
                  "El Paso, United States",
                  "Big Bend National Park",
                  "Del Rio, United States",
                  "Laredo, United States",
                  "Rio Grande City, United States")

entry_points.sf <- geocode_OSM(entry_points,
                               projection = 4326,
                               as.sf = T)


entry_points.sf <- entry_points.sf %>%
  select(location = query, x, y)

# Saving simple feature
st_write(entry_points.sf, 
         "Data/USA_borderPoints.csv",
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
    location = if_else(location == "San Cristóbal, Guatemala", 
                       "Cd. San Cristóbal, Guatemala",
                       location),
    
    # Taking into account metro area localities
    location = case_when(
      location == "Roaring Creek, Belize"            ~ "Belmopan, Belize",
      location == "Santa Elena, Belize"              ~ "San Ignacio, Belize",
      location == "Delgado, El Salvador"             ~ "San Salvador, El Salvador",
      location == "Mejicanos, El Salvador"           ~ "San Salvador, El Salvador",
      location == "Mejicanos, El Salvador"           ~ "San Salvador, El Salvador",
      location == "Nueva San Salvador, El Salvador"  ~ "San Salvador, El Salvador",
      location == "Soyapango, El Salvador"           ~ "San Salvador, El Salvador",
      location == "Cuscatancingo, El Salvador"       ~ "San Salvador, El Salvador",
      location == "Ayutuxtepeque, El Salvador"       ~ "San Salvador, El Salvador",
      location == "San Marcos, El Salvador"          ~ "San Salvador, El Salvador",
      location == "Antiguo Cuscatlán, El Salvador"   ~ "San Salvador, El Salvador",
      location == "Ciudad De Guatemala, Guatemala"   ~ "Ciudad De Guatemala, Guatemala",
      location == "Ilopango, El Salvador"            ~ "San Salvador, El Salvador",
      location == "Mixco, Guatemala"                 ~ "Ciudad De Guatemala, Guatemala",
      location == "Villa Nueva, Guatemala"           ~ "Ciudad De Guatemala, Guatemala",
      TRUE ~ location
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
    country = first(country),
    intmig_person = mean(EXP_q31a, na.rm = T),
    recent_intmig = mean(EXP22_q26b, na.rm = T),
    desire2emig   = mean(EXP_q31d, na.rm = T),
    plans2emig    = mean(EXP_q31f, na.rm = T),
    nobs          = n()
  ) %>%
  filter(!is.na(location))

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
           na.fail = F,
           crs     = 4326) %>%
  filter(!is.na(x) & !is.na(y)) 

# Subsetting metro areas
m.areas <- c("Ciudad De Guatemala, Guatemala",
             "Quetzaltenango, Guatemala",
             "Huehuetenango, Guatemala",
             "Belmopan, Belize",
             "Belize, Belize",
             "San Ignacio, Belize",
             "Panamá, Panama",
             "Colón, Panama",
             "David, Panama",
             "San Salvador, El Salvador",
             "Santa Ana, El Salvador",
             "San Miguel, El Salvador")

data4maps.sf <- central_america.sf %>%
  filter(location %in% m.areas)

# Saving simple feature
st_write(data4maps.sf, 
         "Data/CA_data4maps.csv",
         layer_options = "GEOMETRY=AS_WKT")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Auxiliary routine to group localities within metro areas                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# We need a mollweide projection to get distances in kilometers
# Get the simple feature from the previous section
central_america.sf <- central_america.sf %>%
  st_transform("+proj=moll")

# Subsetting metro areas
metro_areas.sf <- central_america.sf %>%
  filter(location %in% m.areas)

# Estimating distance to metro areas
potential_metro.sf <- central_america.sf %>%
  st_nearest_points(metro_areas.sf %>% st_combine())
potential_metro.sf <- st_sf(central_america.sf, 
                            geometry = potential_metro.sf) %>%
  mutate(dist2metro = set_units(st_length(.),
                                "km")) %>%
  filter(dist2metro < set_units(15, "km") & dist2metro > set_units(0, "km")) %>%
  st_drop_geometry()

# Now you will have to manually check localities: Viele Spaß!!




