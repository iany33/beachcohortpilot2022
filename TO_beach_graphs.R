
pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics, 
  gtsummary,    # summary statistics and tests
  rstatix,      # statistics
  corrr,        # correlation analysis for numeric variables
  janitor,      # adding totals and percents to tables
  flextable,     # converting tables to HTML
  lubridate,     # working with date
  ggridges,
  ggforce,
  forcats       # factors
)


### Turn illness variables into factors for graphing

data <- data %>% mutate(agi2 = case_when(agi == 1 ~ "Yes", TRUE ~ "No")) %>% 
  mutate(agi2 = as.factor(agi2))

data <- data %>% mutate(respiratory2 = case_when(respiratory == 1 ~ "Yes", TRUE ~ "No")) %>% 
  mutate(respiratory2 = as.factor(respiratory2))

data <- data %>% mutate(eye_infection2 = case_when(eye_infection == 1 ~ "Yes", TRUE ~ "No")) %>% 
  mutate(eye_infection2 = as.factor(eye_infection2))

data <- data %>% mutate(ear_infection2 = case_when(ear_infection == 1 ~ "Yes", TRUE ~ "No")) %>% 
  mutate(ear_infection2 = as.factor(ear_infection2))

data <- data %>% mutate(skin_infection2 = case_when(skin_infection == 1 ~ "Yes", TRUE ~ "No")) %>% 
  mutate(skin_infection2 = as.factor(skin_infection2))

data <- data %>% mutate(diarrhea2 = case_when(diarrhea == 1 ~ "Yes", TRUE ~ "No")) %>% 
  mutate(diarrhea2 = as.factor(diarrhea2))


### E. coli levels by illness

data %>% 
  ggplot(aes(x = respiratory2, y = e_coli, fill = respiratory2)) +
  geom_boxplot(width = 0.2) +                    
  geom_jitter(size = 0.5) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    x = "Respiratory illness",
    y = "E. coli geometric mean (CFU / 100 mL)" )

data %>% 
  ggplot(aes(y = respiratory2, x = e_coli, fill = respiratory2)) +
  geom_density_ridges() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    y = "Respiratory illness",
    x = "E. coli geometric mean (CFU / 100 mL)" )

data %>% 
  ggplot(aes(x = respiratory2, y = e_coli, fill = respiratory2)) +
  geom_violin(alpha = 0.2) +                    
  geom_sina(aes(colour = respiratory2)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    x = "Respiratory illness",
    y = "E. coli geometric mean (CFU / 100 mL)" )
