## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Country Reports - Methodology Parameters
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     January 17th, 2023
##
## This version:      January 17th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

gen.tableB <- function(data) {
  
  # Producing HTML table using Knittr
  html <- data %>% 
    filter(Country %in% countrySet) %>% 
    kable(format = "html")
  
  # Adjusting HTML code
  html <- str_remove_all(html, 
                         pattern  = "<table>|</table>|<tbody>|</tbody>|<thead>|</thead>")
  html <- str_replace_all(html,
                          pattern     = "\\\n",
                          replacement = "")
  html <- str_replace_all(html,
                          pattern     = '<th.+?>', 
                          replacement = "<td>")
  html <- str_replace_all(html,
                          pattern     = "</th>",
                          replacement = "</td>")
  html <- str_replace_all(html,
                          pattern     = '<td style.+?>', 
                          replacement = "<td>")
  html <- str_replace(html, 
                      pattern         = "<tr>", 
                      replacement     = "<tr class=\\'bg-purple text-white\\'>")
  
  return(html)
  
}


create_methodPage.fn <- function(){
  
  # Filtering data
  data4quarto.ls <- list(
    "sf" = method_data.ls[["sf"]] %>% filter(Country == mainCountry),
    "af" = method_data.ls[["af"]] %>% filter(country == mainCountry),
    "sd" = method_data.ls[["sd"]] %>% filter(country == mainCountry),
    "tA" = method_data.ls[["tA"]] %>%
      filter(country == mainCountry) %>%
      pivot_longer(!country,
                   names_to  = "category",
                   values_to = "value") %>%
      select(!country) %>%
      mutate(value = format(value, big.mark = ",")),
    "tB" = method_data.ls[["tB"]]
  )
  
  # Defining Parameters
  method_input.ls <- list(
    
    # Sampling Frame
    "sf_country"         = mainCountry, 
    "sf_year"            = data_subset.df %>% 
                              group_by(country) %>% 
                              summarise(latestYear = first(latestYear)) %>%
                              filter(country == mainCountry) %>% 
                              pull(latestYear),
    "sf_perception"      = data4quarto.ls[["sf"]] %>% pull(`Perception`),
    "sf_experience"      = data4quarto.ls[["sf"]] %>% pull(`Experience`),
    "sf_ssize"           = data4quarto.ls[["sf"]] %>% 
                              mutate(`Sample Size` = format(`Sample Size`, 
                                                            big.mark = ",")) %>%
                              pull(`Sample Size`),           
    "sf_company"         = data4quarto.ls[["sf"]] %>% pull(`Polling Company`),         
    "sf_dates"           = data4quarto.ls[["sf"]] %>% pull(`FW Dates`),           
    "sf_nationality"     = data4quarto.ls[["sf"]] %>% pull(`Sample Size`),     
    "sf_location"        = data4quarto.ls[["sf"]] %>% pull(`Polling Company Location`),
    "sf_projPop"        = data4quarto.ls[["sf"]] %>% pull(`ProjPop`),
    "sf_comparison"      = str_replace(paste(comparison_countries.ls, collapse = ", "), 
                                       "(,)(?!.*\\1)", 
                                       " and"),  
    "sf_surveyLang"      = data4quarto.ls[["sf"]] %>% pull(`SurveyLang`),
    "sf_sampling"        = data4quarto.ls[["sf"]] %>% pull(`Sampling`),
    "sf_ethnicity"       = data4quarto.ls[["sf"]] %>% pull(`Ethnicity`),
    "sf_qcontrol"        = data4quarto.ls[["sf"]] %>% pull(`Quality Control`),
    "sf_education"       = data4quarto.ls[["sf"]] %>% pull(`Education`),
    "sf_total"           = data4quarto.ls[["sf"]] %>% pull(`Total`),
    "sf_female"          = data4quarto.ls[["sf"]] %>% pull(`Female`),
    "sf_OptEnumerators"  = data4quarto.ls[["sf"]] %>% pull(`OptEnumerators`),
    "sf_language"        = data4quarto.ls[["sf"]] %>% pull(`Language`),
    "sf_probSample"      = data4quarto.ls[["sf"]] %>% 
                                    mutate(`Prob Sample` = format(`Prob Sample`, 
                                                                  big.mark = ",")) %>%
                                    pull(`Prob Sample`),
    "sf_historical"      = data4quarto.ls[["sf"]] %>% pull(`Historical Data`),
    "sf_region"          = tolower(data4quarto.ls[["sf"]] %>% pull(`Region`)),
    
    # Administrative Divisions
    "af_sunit_term"      = data4quarto.ls[["af"]] %>% 
                              filter(`Administration Divisions` == "Sample Units") %>% 
                              pull(`Term`),    
    "af_sunit_number"    = data4quarto.ls[["af"]] %>% 
                              filter(`Administration Divisions` == "Sample Units") %>% 
                              pull(`Number`),     
    "af_region_term"     = data4quarto.ls[["af"]] %>% 
                              filter(`Administration Divisions` == "region") %>% 
                              pull(`Number`),     
    "af_region_number"   = data4quarto.ls[["af"]] %>% 
                              filter(`Administration Divisions` == "region") %>% 
                              pull(`Number`),   
    
    # Sample Description
    "sd_region1_name"    = data4quarto.ls[["sd"]] %>% 
                              filter(item == "Region1") %>% 
                              pull(name),    
    "sd_region1_value"   = paste0(format(data4quarto.ls[["sd"]] %>% 
                                           filter(item == "Region1") %>% 
                                           mutate(value = round(value*100, 0)) %>%
                                           pull(value),
                                         nsmall = 0),
                                  "%"),   
    "sd_region2_name"    = data4quarto.ls[["sd"]] %>% 
                              filter(item == "Region2") %>% 
                              pull(name),    
    "sd_region2_value"   = paste0(format(data4quarto.ls[["sd"]] %>% 
                                           filter(item == "Region2") %>% 
                                           mutate(value = round(value*100, 0)) %>%
                                           pull(value),
                                         nsmall = 0),
                                  "%"),   
    "sd_region3_name"    = data4quarto.ls[["sd"]] %>% 
                              filter(item == "Region3") %>% 
                              pull(name),    
    "sd_region3_value"   = paste0(format(data4quarto.ls[["sd"]] %>% 
                                           filter(item == "Region3") %>% 
                                           mutate(value = round(value*100, 0)) %>%
                                           pull(value),
                                         nsmall = 0),
                                  "%"),   
    "sd_rural_valueTXT"  = str_to_sentence(english(data4quarto.ls[["sd"]] %>% 
                                                  filter(name == "Rural") %>%
                                                  mutate(value = round(value*100, 0)) %>%
                                                  pull(value))),  
    "sd_rural_value"     = paste0(format(data4quarto.ls[["sd"]] %>% 
                                           filter(name == "Rural") %>%
                                           mutate(value = round(value*100, 0)) %>%
                                           pull(value),
                                         nsmall = 0),
                                  "%"),     
    "sd_urban_value"     = paste0(format(data4quarto.ls[["sd"]] %>% 
                                           filter(name == "Urban") %>%
                                           mutate(value = round(value*100, 0)) %>%
                                           pull(value),
                                         nsmall = 0),
                                  "%"),     
    "sd_female_valueTXT" = str_to_sentence(english(data4quarto.ls[["sd"]] %>% 
                                                  filter(name == "Female") %>%
                                                  mutate(value = round(value*100, 0)) %>%
                                                  pull(value))), 
    "sd_female_value"    = paste0(format(data4quarto.ls[["sd"]] %>% 
                                           filter(name == "Female") %>%
                                           mutate(value = round(value*100, 0)) %>%
                                           pull(value),
                                         nsmall = 0),
                                  "%"),    
    "sd_male_value"      = paste0(format(data4quarto.ls[["sd"]] %>% 
                                           filter(name == "Male") %>%
                                           mutate(value = round(value*100, 0)) %>%
                                           pull(value),
                                         nsmall = 0),
                                  "%"),      
    "sd_highedu_value"   = paste0(format(data4quarto.ls[["sd"]] %>% 
                                           filter(str_detect(name, "at least high")) %>%
                                           mutate(value = round(value*100, 0)) %>%
                                           pull(value),
                                         nsmall = 0),
                                  "%"),   
    "sd_lowedu_value"    = paste0(format(data4quarto.ls[["sd"]] %>% 
                                           filter(str_detect(name, "diploma or less")) %>%
                                           mutate(value = round(value*100, 0)) %>%
                                           pull(value),
                                         nsmall = 0),
                                  "%"),    
    
    # Table A
    "tA_elegible"      = data4quarto.ls[["tA"]] %>% 
                              filter(category == "elegible_household") %>% 
                              pull(value),
    "tA_refusals"      = data4quarto.ls[["tA"]] %>% 
                              filter(category == "refusals") %>% 
                              pull(value),
    "tA_breaks"        = data4quarto.ls[["tA"]] %>% 
                              filter(category == "break_off") %>% 
                              pull(value),
    "tA_nocont"        = data4quarto.ls[["tA"]] %>% 
                              filter(category == "non_contact") %>% 
                              pull(value),
    "tA_inelegible"    = data4quarto.ls[["tA"]] %>% 
                              filter(category == "inelegible_household") %>% 
                              pull(value),
    "tA_noele"         = data4quarto.ls[["tA"]] %>% 
                              filter(category == "no_elegible") %>% 
                              pull(value),
    "tA_quotafil"      = data4quarto.ls[["tA"]] %>% 
                              filter(category == "quota_filled") %>% 
                              pull(value),
    
    # Table B
    "tB"  =  gen.tableB(data4quarto.ls[["tB"]])
    
  )
  
  # Modifying specific changes
  # if (mainCountry == "Colombia"){
  #   method_input.ls[["af_sub1_term"]] <- data4quarto.ls[["af"]] %>% 
  #     filter(`Administration Divisions` == "Sample Units") %>% 
  #     pull(`Term`)
  #   method_input.ls[["af_sub1_number"]] <- data4quarto.ls[["af"]] %>% 
  #     filter(`Administration Divisions` == "Sample Units") %>% 
  #     pull(`Number`)
  # }
  
  # Creating Directory to store Methodology files
  dir.create(file.path("Outputs", 
                       str_replace(mainCountry, " ", "_"),
                       "Method",
                       fsep = "/"), 
             showWarnings = FALSE)
  
  # Rendering quarto document
  quarto::quarto_render(
    input        = "Code/method.qmd",
    execute_dir  = file.path("Outputs", 
                            str_replace(mainCountry, " ", "_"),
                            "Method",
                            fsep = "/"),
    execute_params = method_input.ls
    )
}
