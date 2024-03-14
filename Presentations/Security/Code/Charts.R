
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Country Reports - Presentations: Security.
##
## Author(s):         A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     March 13th, 2024
##
## This version:      March 13th, 2024
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

# Loading plotting functions from GitHub
source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/loading.R")
loadVIZ(set = "LAC")

master_data_CAR.df <- read_dta("Data/LAC - Merged (with CA).dta") %>%
  select(country, year, id,
         q9, 
         q1d, q2d,
         # All variables related with security
         starts_with("EXP_q8"), starts_with("q8"), CAR_q47a_12, CAR_q47b_5,
         EXP_q22e_G1, q48c_G1, 
         COLOR, age, fin, Urban, gend, edu)
master_data_LAC.df <- read_dta("Data/LAC - Merged.dta") %>%
  select(country, year, id,
         q9, 
         q1d, q2d,
         # All variables related with security
         starts_with("EXP_q8"), starts_with("q8"), CAR_q47a_12, CAR_q47b_5,
         EXP_q22e_G1, q48c_G1, 
         COLOR, age, fin, Urban, gend, edu)

"%!in%" <- compose("!", "%in%")

master_data.df <- rbind(master_data_CAR.df, master_data_LAC.df) %>%
  group_by(country) %>%
  mutate(last_year = max(year)) %>%
  ungroup() %>%
  mutate(
    across(all_of(c("q9", "q1d", "q48c_G1")),
           ~case_when(
             .x == 1 | .x == 2 ~ 1,
             .x == 3 | .x == 4 ~ 0,
             q9 == 99 | is.na(q9) ~ NA_real_,
           )),
    across(all_of(c("q2d", "EXP_q22e_G1")),
           ~case_when(
             .x == 1 | .x == 2 ~ 0,
             .x == 3 | .x == 4 ~ 1,
             q9 == 99 | is.na(q9) ~ NA_real_,
           )),
    victim = if_else(EXP_q8a_1 == 1 | EXP_q8a_2 == 1 | EXP_q8a_3 == 1 | EXP_q8a_4 == 1 | EXP_q8a_5 == 1 | EXP_q8a_6 == 1 | EXP_q8a_7 == 1 |
                       EXP_q8a_8 == 1 | EXP_q8a_9 == 1 | EXP_q8a_10 == 1 | EXP_q8a_11 == 1 | EXP_q8a_12 == 1 | EXP_q8a_13 == 1|
                       EXP_q8b_1 == 1 | EXP_q8b_2 == 1 | EXP_q8b_3 == 1 | EXP_q8b_4 == 1 |  CAR_q47a_12 == 1 | CAR_q47b_5 == 1|
                       q8b_1 == 1 | q8b_2 == 1 | q8b_3 == 1 | q8b_4 == 1 | q8b_5 == 1 | q8b_6 == 1 | q8b_7 == 1 | q8b_8 == 1 | q8b_9 == 1 |
                       q8b_10 == 1 | q8b_11 == 1 | q8b_12 == 1 | q8b_13 == 1 | q8b_14 == 1 | q8b_15 == 1 | q8b_16 == 1 | q8b_17 == 1, 1, 0, 0)
  ) %>%
  filter(country %!in% c("Antigua and Barbuda", "Mexico", "St. Kitts and Nevis", "Venezuela", "Nicaragua", "Chile", "Uruguay"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Charts                                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
outPath <- "/Users/santiagopardo/Documents/GitHub/LAC-Reports/Presentations/Security/Outputs"
vars2plot <- c("q9", 
               "q1d", 
               "q2d", 
               "victim", 
               "q48c_G1", 
               "EXP_q22e_G1")
vars2names <- c("security", 
               "trust_police", 
               "corruption_police", 
               "victims", 
               "rights_suspects", 
               "not_excessive_force")
titles <- c("Perceptions of Security in the Region",
            "Trust in Police",
            "Perceptions of Corruption in the Police",
            "Crime Victimization Rates in the Region",
            "Perceptions of the Police Due Process",
            "Perceptions of the Police Due Process")
subtitles <- c("Percentage of respondents who reported that they feel safe or very safe walking in their neighborhood at night",
               "Percentage of respondents who have a lot or some trust in...",
               "Percentage of respondents who believe that most or all people working in the following institutions are corrupt",
               "Data on crime victimization",
               "Percentage of respondents who believe that the police respect the rights of suspects",
               "Percentage of respondents who believe that the police do not use excessive force"
               )
for (var in vars2plot) {
  
  var_name <-   master_data.df %>%
    select(var) %>%
    pivot_longer(cols = all_of(var), names_to = "variable", values_to = "values") %>%
    select(variable) %>%
    distinct() %>%
    mutate(
      title = 
        case_when(
          variable %in% vars2plot[1] ~ vars2names[1],
          variable %in% vars2plot[2] ~ vars2names[2],
          variable %in% vars2plot[3] ~ vars2names[3],
          variable %in% vars2plot[4] ~ vars2names[4],
          variable %in% vars2plot[5] ~ vars2names[5],
          variable %in% vars2plot[6] ~ vars2names[6],
          variable %in% vars2plot[7] ~ vars2names[7],
          variable %in% vars2plot[8] ~ vars2names[8],
        ),
    ) %>%
    pull()
    
  title <- master_data.df %>%
    select(var) %>%
    pivot_longer(cols = all_of(var), names_to = "variable", values_to = "values") %>%
    select(variable) %>%
    distinct() %>%
    mutate(
      title = 
        case_when(
          variable %in% vars2plot[1] ~ titles[1],
          variable %in% vars2plot[2] ~ titles[2],
          variable %in% vars2plot[3] ~ titles[3],
          variable %in% vars2plot[4] ~ titles[4],
          variable %in% vars2plot[5] ~ titles[5],
          variable %in% vars2plot[6] ~ titles[6],
          variable %in% vars2plot[7] ~ titles[7],
          variable %in% vars2plot[8] ~ titles[8],
          ),
    ) %>%
    pull()
  
  subtitle <-  master_data.df %>%
    pivot_longer(cols = var, names_to = "variable", values_to = "values")  %>%
    select(variable) %>%
    distinct() %>%
    mutate(
      title = 
        case_when(
          variable %in% vars2plot[1] ~ subtitles[1],
          variable %in% vars2plot[2] ~ subtitles[2],
          variable %in% vars2plot[3] ~ subtitles[3],
          variable %in% vars2plot[4] ~ subtitles[4],
          variable %in% vars2plot[5] ~ subtitles[5],
          variable %in% vars2plot[6] ~ subtitles[6],
          variable %in% vars2plot[7] ~ subtitles[7],
          variable %in% vars2plot[8] ~ subtitles[8],
        ),
    ) %>%
    pull()
    
  data2table <- master_data.df %>%
    rename(var = all_of(var)) %>%
    filter(year == last_year) %>%
    group_by(country) %>%
    summarise(value2plot = mean(var, na.rm = TRUE)) %>%
    mutate(
      value2plot = value2plot * 100,
      ranking = rank(-value2plot),
      ranking = if_else(is.na(ranking), 99, ranking)
      ) %>%
    filter(ranking != 99) %>%
    filter(!is.nan(value2plot)) %>%
    mutate(
      length_lowest = length(country) - 3,
      category = if_else(ranking < 4, "Best 3",
                         if_else(ranking > length_lowest, "Lowest 3", "No"))
      ) %>%
    select(!length_lowest) %>%
    rbind(master_data.df %>%
            rename(var = all_of(var)) %>%
            filter(year == last_year) %>%
            group_by(country) %>%
            summarise(value2plot = mean(var, na.rm = TRUE)) %>%
            ungroup() %>%
            summarise(value2plot = mean(value2plot, na.rm = T)) %>%
            mutate(
              value2plot = value2plot*100,
              country = "Regional Average",
              ranking = 5,
              category = "Regional Average"
            ))  %>%
    mutate(
      figure   = paste0(round(value2plot, 0), "%")
    ) %>%
    arrange(ranking) %>%
    filter(category != "No") 
  
  if(var %in% c("q48c_G1", "EXP_q22e_G1", "q1d", "q9")) {
    
    colors4plot <- c("Lowest 3" = "#F69191", 
                     "Regional Average" = "#5B0252", 
                     "Best 3" = "#7FA7FE")
  } else {
    
    colors4plot <- c("Best 3" = "#F69191", 
                     "Regional Average" = "#5B0252", 
                     "Lowest 3" = "#7FA7FE")
    
  }

  
  p <- ggplot(data2table,
              aes(
                x     = reorder(country, ranking),
                y     = value2plot,
                fill  = category,
                label = figure
              )) +
    geom_bar(stat = "identity",
             show.legend = FALSE,
             position = position_dodge(width = 0.9)) +
    geom_text(aes(y    = value2plot + 5), 
              position = position_dodge(width = 0.9),
              color    = "black",
              family   = "Lato Full",
              fontface = "bold", 
              size = 4.217518) +
    scale_fill_manual(values = colors4plot) +
    scale_y_continuous(limits = c(0, 100),
                       breaks = seq(0, 100, 20),
                       labels = paste0(seq(0, 100, 20), "%"),
                       position = "left") + 
    labs(
      title = title,
      subtitle = subtitle
    ) +
    theme(
      panel.background   = element_blank(),
      plot.background    = element_blank(),
      panel.grid.major   = element_line(size     = 0.25,
                                        colour   = "#5e5c5a",
                                        linetype = "dashed"),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#D0D1D3")) +
    theme(
      axis.title.y       = element_blank(),
      axis.title.x       = element_blank(),
      axis.ticks         = element_blank(),
      axis.text.y        = element_markdown(family   = "Lato Full",
                                            face     = "plain",
                                            size     = 3.514598 * .pt,
                                            color    = "#524F4C",
                                            margin   = margin(0, 10, 0, 0),
                                            hjust = 0), 
      plot.caption = element_markdown(family   = "Lato Full",
                                      face     = "plain",
                                      size     = 3.514598 * .pt,
                                      color    = "#524F4C", 
                                      vjust    = 0, 
                                      hjust    = 0, 
                                      margin = margin(20, 0, 0, 0)),
      axis.text.x        = element_markdown(family   = "Lato Full",
                                            face     = "bold",
                                            size     = 4.217518 * .pt,
                                            color    = "#524F4C"),
      plot.title          = element_text(family   = "Lato Full",
                                         face     = "bold",
                                         size     = 4.217518 * .pt,
                                         color    = "black",
                                         margin   = margin(0, 0, 10, 0),
                                         hjust    = 0), 
      plot.subtitle      = element_text(family   = "Lato Full",
                                        face     = "plain",
                                        size     = 4.217518 * .pt,
                                        color    = "black",
                                        margin   = margin(2.5, 0, 20, 0),
                                        hjust    = 0),
      legend.text        =  element_markdown(family   = "Lato Full",
                                             face     = "plain",
                                             size     = 4.217518 * .pt,
                                             color    = "#524F4C"),
      legend.title       = element_markdown(family   = "Lato Full",
                                            face     = "plain",
                                            size     = 3.514598 * .pt,
                                            color    = "#524F4C")
    )
  
  ggsave(
    plot = p,
    filename = paste0(outPath,"/chart_", var_name, ".svg"), 
    width =  12,
    height = 8)
}

# LOGIT

data_subset.df <- master_data.df %>%
  mutate(
         white         =  if_else(COLOR == 1 | COLOR == 2 | COLOR == 3 | COLOR == 4, "White" , 
                                  if_else(COLOR == 5 | COLOR == 6 | COLOR == 7 | COLOR == 8 | COLOR == 9 | COLOR == 10, "No White", NA_character_)),
         poor          =  if_else(fin == 1 | fin == 2, "Poor",
                                  if_else(fin == 3 | fin == 4 | fin == 5, "No Poor", NA_character_)),
         area          =  if_else(Urban == 1, "Urban", "Rural"),
         gender        =  if_else(gend == 1, "Male", "Female"),
         diploma       =  if_else(edu == 4 | edu == 5 | edu == 6| edu == 7, "High Education Level", 
                                  if_else(edu < 4, "No High Education Level", NA_character_))) %>%
  mutate(
    young = if_else(country %in% c("Bahamas",
                                   "Peru",
                                   "Barbados",
                                   "Dominica","
                                   St. Lucia",
                                   "St. Vincent and the Grenadines",
                                   "Grenada"), 
                    if_else(age < 3, "Less than 35 years", 
                            if_else(age > 2, "More than 35 years", NA_character_)),
                    if_else(age < 35, "Less than 35 years", 
                            if_else(age > 34, "More than 35 years", NA_character_)),
                    NA_character_)
  ) %>%
  select(id, country, year,victim, white, poor, area, gender, diploma, young) %>%
  filter(year == max(year))

condition <- data_subset.df %>%
  select(white, young, poor, area, gender, diploma) %>%
  mutate(counter = 1)

color   <- condition_categories(main_data = condition, group_var = white, name_var = "white")
age     <- condition_categories(main_data = condition, group_var = young, name_var = "young")
income  <- condition_categories(main_data = condition, group_var = poor, name_var = "poor")
area    <- condition_categories(main_data = condition, group_var = area, name_var = "area")
gender  <- condition_categories(main_data = condition, group_var = gender, name_var = "gender")
diploma <- condition_categories(main_data = condition, group_var = diploma, name_var = "diploma")

selectables <- rbind(color, age, income, area, gender, diploma) %>%
  group_by(variable) %>%
  summarise(min_group = min(N_obs, na.rm = T),
            total_group = sum(N_obs, na.rm = T)) %>%
  filter(min_group > 30) %>%
  filter(min_group != total_group) %>%
  pull(variable)


logit_data <- data_subset.df %>%
  select(victim, all_of(selectables)) %>%
  rowid_to_column("id") %>%
  pivot_longer(cols = !c(victim, id), names_to = "categories", values_to = "values") %>%
  mutate(values = if_else(categories %in% "young" & values %in% "More than 35 years", "1More than 35 years", values),
         values = if_else(categories %in% "gender" & values %in% "Male", "1Male", values)) %>%
  pivot_wider(id_cols = c(victim, id), names_from = categories, values_from = values)  

logit_data<- logit_data %>%
  select(all_of(selectables),
         victim) # We didn't include the non answer

formula <- selectables %>%
  t() %>%
  as.data.frame() %>%
  unite(., formula, sep = "+") %>%
  as.character()

models <- lapply(list("victim"), 
                 function(depVar) {
                   formula  <- as.formula(paste(depVar, "~", formula))
                   logit  <- glm(formula,  
                                 data   = logit_data, 
                                 family = "binomial")})

summaryreg <- bind_rows(as.data.frame(coef(summary(models[[1]]))))

margEff    <- margins_summary(models[[1]], data = models[[1]]$model)

data2plot <- margEff

data2plot$factor <- recode(data2plot$factor, "genderFemale" = "Female", "poorPoor" = "Financially \ninsecure",
                           "areaUrban" = "Urban", "whiteWhite" = "Light skin \ntone", "youngLess than 35 years" = "Younger than 35",
                           "diplomaNo High Education Level" = "No high school \ndiploma")

data2plot <- data2plot %>%
  mutate(order_variable = if_else(factor %in% "Female", 1,
                                  if_else(factor %in% "White", 2,
                                          if_else(factor %in% "Poor", 3,
                                                          if_else(factor %in% "Urban", 4, 
                                                                  if_else(factor %in% "Young", 5, 6)))))) 

plot <- logit_demo_panel(mainData = data2plot, line_size = 1.5) +
  scale_y_continuous(limits = c(-0.175, 0.175),
                     breaks = seq(-0.15, 0.15, by = 0.075),
                     expand = expansion(mult = 0.025), position = "right",
                     labels = c("-15 p.p.", "-7.5 p.p.", "0 p.p.", "+7.5 p.p.", "+15 p.p.")) +
  labs(title = "Impact of Sociodemographic Characteristics on Being Victim of Crime",
       subtitle = "Likelihood that people suffer a victimization in the region",
       caption = "This regression was made with 24 countries of the Latin American and the Caribbean")+
  theme(
    axis.text.y        = element_markdown(family   = "Lato Full",
                                          face     = "bold",
                                          size     = 4.217518 * .pt,
                                          color    = "#524F4C",
                                          margin   = margin(0, 10, 0, 0),
                                          hjust = 0), 
    plot.caption = element_markdown(family   = "Lato Full",
                                    face     = "plain",
                                    size     = 3.514598 * .pt,
                                    color    = "#524F4C", 
                                    vjust    = 0, 
                                    hjust    = 0, 
                                    margin = margin(20, 0, 0, 0)),
    axis.text.x        = element_markdown(family   = "Lato Full",
                                          face     = "bold",
                                          size     = 4.217518 * .pt,
                                          color    = "#524F4C"),
    plot.title          = element_text(family   = "Lato Full",
                                       face     = "bold",
                                       size     = 4.217518 * .pt,
                                       color    = "black",
                                       margin   = margin(0, 0, 10, 0),
                                       hjust    = 0), 
    plot.subtitle      = element_text(family   = "Lato Full",
                                      face     = "plain",
                                      size     = 4.217518 * .pt,
                                      color    = "black",
                                      margin   = margin(2.5, 0, 20, 0),
                                      hjust    = 0),
    legend.text        =  element_markdown(family   = "Lato Full",
                                           face     = "plain",
                                           size     = 3.514598 * .pt,
                                           color    = "#524F4C"),
    legend.title       = element_markdown(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598 * .pt,
                                          color    = "#524F4C")
  )

  
ggsave(
  plot = plot,
  filename = paste0(outPath,"/logit_victim.svg"), 
  width =  12,
  height = 8)


# Feel safe

data_subset.df <- master_data.df %>%
  mutate(
    unsafe_bin    = q9,
    victim        =  if_else(victim == 1, "Victim", "Non Victim", NA_character_),
    white         =  if_else(COLOR == 1 | COLOR == 2 | COLOR == 3 | COLOR == 4, "White" , 
                             if_else(COLOR == 5 | COLOR == 6 | COLOR == 7 | COLOR == 8 | COLOR == 9 | COLOR == 10, "No White", NA_character_)),
    poor          =  if_else(fin == 1 | fin == 2, "Poor",
                             if_else(fin == 3 | fin == 4 | fin == 5, "No Poor", NA_character_)),
    area          =  if_else(Urban == 1, "Urban", "Rural"),
    gender        =  if_else(gend == 1, "Male", "Female"),
    diploma       =  if_else(edu == 4 | edu == 5 | edu == 6| edu == 7, "High Education Level", 
                             if_else(edu < 4, "No High Education Level", NA_character_))) %>%
  mutate(
    young = if_else(country %in% c("Bahamas",
                                   "Peru",
                                   "Barbados",
                                   "Dominica","
                                   St. Lucia",
                                   "St. Vincent and the Grenadines",
                                   "Grenada"), 
                    if_else(age < 3, "Less than 35 years", 
                            if_else(age > 2, "More than 35 years", NA_character_)),
                    if_else(age < 35, "Less than 35 years", 
                            if_else(age > 34, "More than 35 years", NA_character_)),
                    NA_character_)
  ) %>%
  select(id, unsafe_bin, country, year,victim, white, poor, area, gender, diploma, young) %>%
  filter(year == max(year))

condition <- data_subset.df %>%
  select(victim, white, young, poor, area, gender, diploma) %>%
  mutate(counter = 1)

victim  <- condition_categories(main_data = condition, group_var = victim, name_var = "victim")
color   <- condition_categories(main_data = condition, group_var = white, name_var = "white")
age     <- condition_categories(main_data = condition, group_var = young, name_var = "young")
income  <- condition_categories(main_data = condition, group_var = poor, name_var = "poor")
area    <- condition_categories(main_data = condition, group_var = area, name_var = "area")
gender  <- condition_categories(main_data = condition, group_var = gender, name_var = "gender")
diploma <- condition_categories(main_data = condition, group_var = diploma, name_var = "diploma")

selectables <- rbind(victim, color, age, income, area, gender, diploma) %>%
  group_by(variable) %>%
  summarise(min_group = min(N_obs, na.rm = T),
            total_group = sum(N_obs, na.rm = T)) %>%
  filter(min_group > 30) %>%
  filter(min_group != total_group) %>%
  pull(variable)


logit_data <- data_subset.df %>%
  select(unsafe_bin, all_of(selectables)) %>%
  rowid_to_column("id") %>%
  pivot_longer(cols = !c(unsafe_bin, id), names_to = "categories", values_to = "values") %>%
  mutate(values = if_else(categories %in% "young" & values %in% "More than 35 years", "1More than 35 years", values),
         values = if_else(categories %in% "gender" & values %in% "Male", "1Male", values)) %>%
  pivot_wider(id_cols = c(unsafe_bin, id), names_from = categories, values_from = values)  

logit_data<- logit_data %>%
  select(all_of(selectables),
         unsafe_bin) # We didn't include the non answer

formula <- selectables %>%
  t() %>%
  as.data.frame() %>%
  unite(., formula, sep = "+") %>%
  as.character()

models <- lapply(list("unsafe_bin"), 
                 function(depVar) {
                   formula  <- as.formula(paste(depVar, "~", formula))
                   logit  <- glm(formula,  
                                 data   = logit_data, 
                                 family = "binomial")})

summaryreg <- bind_rows(as.data.frame(coef(summary(models[[1]]))))

margEff    <- margins_summary(models[[1]], data = models[[1]]$model)

data2plot <- margEff

data2plot$factor <- recode(data2plot$factor, "genderFemale" = "Female", "poorPoor" = "Financially \ninsecure", "victimVictim" = "Previous crime \nvictimization",
                           "areaUrban" = "Urban", "whiteWhite" = "Light skin \ntone", "youngLess than 35 years" = "Younger than 35",
                           "diplomaNo High Education Level" = "No high school \ndiploma")

data2plot <- data2plot %>%
  mutate(order_variable = if_else(factor %in% "Female", 1,
                                  if_else(factor %in% "White", 2,
                                          if_else(factor %in% "Poor", 3,
                                                  if_else(factor %in% "Victim", 4,
                                                          if_else(factor %in% "Urban", 5, 
                                                                  if_else(factor %in% "Young", 6, 7)))))))

plot <- logit_demo_panel(mainData = data2plot, line_size = 1.5) +
  scale_y_continuous(limits = c(-0.175, 0.175),
                     breaks = seq(-0.15, 0.15, by = 0.075),
                     expand = expansion(mult = 0.025), position = "right",
                     labels = c("-15 p.p.", "-7.5 p.p.", "0 p.p.", "+7.5 p.p.", "+15 p.p.")) +
  labs(title = "Impact of Sociodemographic Characteristics on Perceptions of Safety",
       subtitle = "Likelihood that respondents feel safe or very safe walking in their neighborhood at night",
       caption = "This regression was made with 24 countries of the Latin American and the Caribbean")+
  theme(
    axis.text.y        = element_markdown(family   = "Lato Full",
                                          face     = "bold",
                                          size     = 4.217518 * .pt,
                                          color    = "#524F4C",
                                          margin   = margin(0, 10, 0, 0),
                                          hjust = 0), 
    plot.caption = element_markdown(family   = "Lato Full",
                                    face     = "plain",
                                    size     = 3.514598 * .pt,
                                    color    = "#524F4C", 
                                    vjust    = 0, 
                                    hjust    = 0, 
                                    margin = margin(20, 0, 0, 0)),
    axis.text.x        = element_markdown(family   = "Lato Full",
                                          face     = "bold",
                                          size     = 4.217518 * .pt,
                                          color    = "#524F4C"),
    plot.title          = element_text(family   = "Lato Full",
                                       face     = "bold",
                                       size     = 4.217518 * .pt,
                                       color    = "black",
                                       margin   = margin(0, 0, 10, 0),
                                       hjust    = 0), 
    plot.subtitle      = element_text(family   = "Lato Full",
                                      face     = "plain",
                                      size     = 4.217518 * .pt,
                                      color    = "black",
                                      margin   = margin(2.5, 0, 20, 0),
                                      hjust    = 0),
    legend.text        =  element_markdown(family   = "Lato Full",
                                           face     = "plain",
                                           size     = 3.514598 * .pt,
                                           color    = "#524F4C"),
    legend.title       = element_markdown(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598 * .pt,
                                          color    = "#524F4C")
  )


ggsave(
  plot = plot,
  filename = paste0(outPath,"/logit_perception_safe.svg"), 
  width =  12,
  height = 8)
