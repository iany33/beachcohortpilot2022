
pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  Matrix,
  tidyverse,    # data management + ggplot2 graphics, 
  gtsummary,    # summary statistics and tests
  rstatix,      # statistics
  corrr,        # correlation analysis for numeric variables
  janitor,      # adding totals and percents to tables
  flextable,     # converting tables to HTML
  lubridate,     # working with dates
  forcats       # factors
)


# Create variable to determine if follow-up was complete or not

data  <- data  %>% 
  mutate(follow = case_when(
    (rec_act1 == 1 | rec_act1 == 0) ~ "Yes", 
    TRUE ~ "No")) %>% 
  mutate(follow = as.factor(follow)) 


# Format date variables

data <- data %>% 
  mutate(symp_date_cramps = as.Date(symp_date_cramps, format = "%Y-%m-%d")) %>% 
  mutate(symp_date_diar = as.Date(symp_date_diar, format = "%Y-%m-%d")) %>% 
  mutate(symp_date_vomit = as.Date(symp_date_vomit, format = "%Y-%m-%d")) %>% 
  mutate(symp_date_naus = as.Date(symp_date_naus, format = "%Y-%m-%d")) %>% 
  mutate(symp_date_fever = as.Date(symp_date_fever, format = "%Y-%m-%d")) %>% 
  mutate(symp_date_throat = as.Date(symp_date_throat, format = "%Y-%m-%d")) %>% 
  mutate(symp_date_nose = as.Date(symp_date_nose, format = "%Y-%m-%d")) %>% 
  mutate(symp_date_cough = as.Date(symp_date_cough, format = "%Y-%m-%d")) %>% 
  mutate(symp_date_ear = as.Date(symp_date_ear, format = "%Y-%m-%d")) %>% 
  mutate(symp_date_eye = as.Date(symp_date_eye, format = "%Y-%m-%d")) %>% 
  mutate(symp_date_rash = as.Date(symp_date_rash, format = "%Y-%m-%d"))


# Create exposure and outcome variables of interest

data <- data %>% 
  mutate(base_agi = case_when(
    base_symp_diar == "diarrhea" ~ 1,
    base_symp_vomit == "vomiting" ~ 1,
    (base_symp_cramps == "cramps" & base_symp_naus == "nausea") ~ 1,
    TRUE ~ 0)) %>%
  mutate(base_agi = as.numeric(base_agi))

data <- data %>% 
  mutate(base_resp = case_when(
    (base_symp_fever == "fever" & base_symp_throat == "throat") ~ 1,
    (base_symp_fever == "fever" & base_symp_nose == "nose") ~ 1,
    base_symp_cough == "cough" ~ 1,
    TRUE ~ 0)) %>%
  mutate(base_resp = as.numeric(base_resp))

data <- data %>% 
  mutate(base_ear = case_when(
    base_symp_ear == "ear" ~ 1,
    TRUE ~ 0)) %>%
  mutate(base_ear = as.numeric(base_ear))

data <- data %>% 
  mutate(base_eye = case_when(
    base_symp_eye == "eye" ~ 1,
    TRUE ~ 0)) %>%
  mutate(base_eye = as.numeric(base_eye))

data <- data %>% 
  mutate(base_skin = case_when(
    base_symp_rash == "rash" ~ 1,
    TRUE ~ 0)) %>%
  mutate(base_skin = as.numeric(base_skin))


# Create 7-day outcome/illness variables 

data <- data %>% 
  mutate(cramps_inc = symp_date_cramps-date) %>% 
  mutate(diar_inc = symp_date_diar-date) %>% 
  mutate(vomit_inc = symp_date_vomit-date) %>% 
  mutate(naus_inc = symp_date_naus-date) %>% 
  mutate(fever_inc = symp_date_fever-date) %>% 
  mutate(throat_inc = symp_date_throat-date) %>% 
  mutate(nose_inc = symp_date_nose-date) %>% 
  mutate(cough_inc = symp_date_cough-date) %>% 
  mutate(ear_inc = symp_date_ear-date) %>% 
  mutate(eye_inc = symp_date_eye-date) %>% 
  mutate(rash_inc = symp_date_rash-date)

data <- data %>% 
  mutate(agi = case_when(
    (symptoms_diar == "diarrhea" & diar_inc <= 7) ~ 1,
    (symptoms_vomit == "vomiting" & vomit_inc <= 7) ~ 1,
    (symptoms_cramps == "cramps" & symptoms_naus == "nausea" & cramps_inc <=7 & naus_inc <= 7) ~ 1,
    (symptoms_cramps == "cramps" & cramps_inc <=7 & misswork == 1) ~ 1,
    (symptoms_naus == "nausea" & naus_inc <=7 & misswork == 1) ~ 1,
    TRUE ~ 0)) %>%
  mutate(agi = as.numeric(agi))

data <- data %>% 
  mutate(respiratory = case_when(
    (symptoms_cough == "cough" & cough_inc <= 7) ~ 1,
    (symptoms_fever == "fever" & symptoms_throat == "throat" & fever_inc <=7 & throat_inc <= 7) ~ 1,
    (symptoms_fever == "fever" & symptoms_nose == "nose" & fever_inc <=7 & nose_inc <= 7) ~ 1,
    TRUE ~ 0)) %>%
  mutate(respiratory = as.numeric(respiratory))

data <- data %>% 
  mutate(ear_infection = case_when(
    (symptoms_ear == "ear" & ear_inc <= 7) ~ 1,
    TRUE ~ 0)) %>%
  mutate(ear_infection = as.numeric(ear_infection))

data <- data %>% 
  mutate(eye_infection = case_when(
    (symptoms_eye == "eye" & eye_inc <= 7) ~ 1,
    TRUE ~ 0)) %>%
  mutate(eye_infection = as.numeric(eye_infection))

data <- data %>% 
  mutate(skin_infection = case_when(
    (symptoms_rash == "rash" & rash_inc <= 7) ~ 1,
    TRUE ~ 0)) %>%
  mutate(skin_infection = as.numeric(skin_infection))

data <- data %>% 
  mutate(diarrhea = case_when(
    (symptoms_diar == "diarrhea" & diar_inc <= 7) ~ 1,
    TRUE ~ 0)) %>%
  mutate(diarrhea = as.numeric(diarrhea))

# Create Yes/No versions of outcome variables

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

# Create outcome versions that exclude those with same baseline symptoms

data <- data %>% 
  mutate(agi3 = case_when(
    (agi == 1 & base_agi == 0) ~ "Yes",
    TRUE ~ "No")) %>%
  mutate(agi3 = as.factor(agi3))

data <- data %>% 
  mutate(respiratory3 = case_when(
    (respiratory == 1 & base_resp == 0) ~ "Yes",
    TRUE ~ "No")) %>%
  mutate(respiratory3 = as.factor(respiratory3))

data <- data %>% 
  mutate(skin_infection3 = case_when(
    (skin_infection == 1 & base_skin == 0) ~ "Yes",
    TRUE ~ "No")) %>%
  mutate(skin_infection3 = as.factor(skin_infection3))

data <- data %>% 
  mutate(ear_infection3 = case_when(
    (ear_infection == 1 & base_ear == 0) ~ "Yes",
    TRUE ~ "No")) %>%
  mutate(ear_infection3 = as.factor(ear_infection3))

data <- data %>% 
  mutate(eye_infection3 = case_when(
    (eye_infection == 1 & base_eye == 0) ~ "Yes",
    TRUE ~ "No")) %>%
  mutate(eye_infection3 = as.factor(eye_infection3))

data <- data %>% 
  mutate(diarrhea3 = case_when(
    (diarrhea == 1 & base_symp_diar == 0) ~ "Yes",
    TRUE ~ "No")) %>%
  mutate(diarrhea3 = as.factor(diarrhea3))



# Convert various variables from text and NA to 1/0 entries

data <- data %>% mutate(cond_GI = case_when(cond_GI == "GI" ~ "Yes", TRUE ~ "No")) %>%
  mutate(cond_GI = as.factor(cond_GI))

data <- data %>% mutate(cond_resp = case_when(cond_resp == "respiratory" ~ "Yes", TRUE ~ "No")) %>%
  mutate(cond_resp = as.factor(cond_resp))

data <- data %>% mutate(cond_skin = case_when(cond_skin == "cond_skin" ~ "Yes", TRUE ~ "No")) %>%
  mutate(cond_skin = as.factor(cond_skin))

data <- data %>% mutate(cond_allergy = case_when(cond_allergy == "allergies" ~ "Yes", TRUE ~ "No")) %>%
  mutate(cond_allergy = as.factor(cond_allergy))

data <- data %>% mutate(cond_immune = case_when(cond_immune == "immune" ~ "Yes", TRUE ~ "No")) %>%
  mutate(cond_immune = as.factor(cond_immune))

data <- data %>% mutate(cond_none = case_when(cond_none == "none" ~ "Yes", TRUE ~ "No")) %>%
  mutate(cond_none = as.factor(cond_none))

data <- data %>% mutate(eth_arab = case_when(ethnicity_arab == "arab" ~ "Yes", TRUE ~ "No")) %>%
  mutate(eth_arab = as.factor(eth_arab))

data <- data %>% mutate(eth_black = case_when(ethnicity_black == "black" ~ "Yes", TRUE ~ "No")) %>%
  mutate(eth_black = as.factor(eth_black))

data <- data %>% mutate(eth_east_asian = case_when(ethnicity_east_asian == "east_asian" ~ "Yes", TRUE ~ "No")) %>%
  mutate(eth_east_asian = as.factor(eth_east_asian))

data <- data %>% mutate(eth_indigenous = case_when(ethnicity_indigenous == "indigenous" ~ "Yes", TRUE ~ "No")) %>%
  mutate(eth_indigenous = as.factor(eth_indigenous))

data <- data %>% mutate(eth_latin = case_when(ethnicity_latin == "latin" ~ "Yes", TRUE ~ "No")) %>%
  mutate(eth_latin = as.factor(eth_latin))

data <- data %>% mutate(eth_south_asian = case_when(ethnicity_south_asian == "south_asian" ~ "Yes", TRUE ~ "No")) %>%
  mutate(eth_south_asian = as.factor(eth_south_asian))

data <- data %>% mutate(eth_se_asian = case_when(ethnicity_se_asian == "southeast_asian" ~ "Yes", TRUE ~ "No")) %>%
  mutate(eth_se_asian = as.factor(eth_se_asian))

data <- data %>% mutate(eth_white = case_when(ethnicity_white == "white" ~ "Yes", TRUE ~ "No")) %>%
  mutate(eth_white = as.factor(eth_white))

data <- data %>% mutate(eth_other = case_when(ethnicity_other == "other_eth" ~ "Yes", TRUE ~ "No")) %>%
  mutate(eth_other = as.factor(eth_other))

data <- data %>% mutate(prev_act1 = case_when(prev_act1 == 1 ~ "Yes", prev_act1 == "NA" ~ NA_character_, TRUE ~ "No")) %>%
  mutate(prev_act1 = as.factor(prev_act1))

data <- data %>% mutate(water_contact = case_when(water_contact == 1 ~ "Yes", water_contact == "NA" ~ NA_character_, TRUE ~ "No")) %>%
  mutate(water_contact = as.factor(water_contact))

data <- data %>% mutate(sand1 = case_when(sand1 == 1 ~ "Yes", sand1 == "NA" ~ NA_character_, TRUE ~ "No")) %>%
  mutate(sand1 = as.factor(sand1))

data <- data %>% mutate(sand_mouth1 = case_when(sand_mouth1 == 1 ~ "Yes", sand_mouth1 == "NA" ~ NA_character_, TRUE ~ "No")) %>%
  mutate(sand_mouth1 = as.factor(sand_mouth1))

data <- data %>% mutate(water_act_swim = case_when(water_act_swim == "swim" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_act_swim = as.factor(water_act_swim))

data <- data %>% mutate(water_act_surf = case_when(water_act_surf == "surf" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_act_surf = as.factor(water_act_surf))

data <- data %>% mutate(water_act_kite = case_when(water_act_kite == "kite" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_act_kite = as.factor(water_act_kite))

data <- data %>% mutate(water_act_wind = case_when(water_act_wind == "wind" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_act_wind = as.factor(water_act_wind))

data <- data %>% mutate(water_act_wake = case_when(water_act_wake == "wake" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_act_wake = as.factor(water_act_wake))

data <- data %>% mutate(water_act_ski = case_when(water_act_ski == "ski" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_act_ski = as.factor(water_act_ski))

data <- data %>% mutate(water_act_paddle = case_when(water_act_paddle == "paddle" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_act_paddle = as.factor(water_act_paddle))

data <- data %>% mutate(water_act_snorkel = case_when(water_act_snorkel == "snorkel" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_act_snorkel = as.factor(water_act_snorkel))

data <- data %>% mutate(water_act_dive = case_when(water_act_dive == "dive" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_act_dive = as.factor(water_act_dive))

data <- data %>% mutate(water_act_wade = case_when(water_act_wade == "wade" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_act_wade = as.factor(water_act_wade))

data <- data %>% mutate(water_act_sail = case_when(water_act_sail == "sail" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_act_sail = as.factor(water_act_sail))

data <- data %>% mutate(water_act_boat = case_when(water_act_boat == "boat" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_act_boat = as.factor(water_act_boat))

data <- data %>% mutate(water_act_fish = case_when(water_act_fish == "fish" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_act_fish = as.factor(water_act_fish))

data <- data %>% mutate(water_act_canoe = case_when(water_act_canoe == "canoe" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_act_canoe = as.factor(water_act_canoe))

data <- data %>% mutate(water_act_kayak = case_when(water_act_kayak == "kayak" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_act_kayak = as.factor(water_act_kayak))

data <- data %>% mutate(water_act_row = case_when(water_act_row == "row" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_act_row = as.factor(water_act_row))

data <- data %>% mutate(water_act_other = case_when(water_act_other == "other" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_act_other = as.factor(water_act_other))

data <- data %>% mutate(water_exp_face = case_when(water_exp_face == "face" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_exp_face = as.factor(water_exp_face))

data <- data %>% mutate(water_exp_mouth = case_when(water_exp_mouth == "mouth" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_exp_mouth = as.factor(water_exp_mouth))

data <- data %>% mutate(water_exp_neither = case_when(water_exp_neither == "neither" ~ "Yes", TRUE ~ "No")) %>%
  mutate(water_exp_neither = as.factor(water_exp_neither))

data <- data %>% mutate(beach_exp_algae = case_when(beach_exp_algae == "algae" ~ "Yes", TRUE ~ "No")) %>%
  mutate(beach_exp_algae = as.factor(beach_exp_algae))

data <- data %>% mutate(beach_exp_sunscreen = case_when(beach_exp_sun == "sun" ~ "Yes", TRUE ~ "No")) %>%
  mutate(beach_exp_sunscreen = as.factor(beach_exp_sunscreen))

data <- data %>% mutate(beach_exp_repellent = case_when(beach_exp_rep == "repellent" ~ "Yes", TRUE ~ "No")) %>%
  mutate(beach_exp_repellent = as.factor(beach_exp_repellent))

data <- data %>% mutate(beach_exp_food = case_when(beach_exp_food == "food" ~ "Yes", TRUE ~ "No")) %>%
  mutate(beach_exp_food = as.factor(beach_exp_food))

data <- data %>% mutate(sand_act_dig = case_when(sand_act_dig == "dig" ~ "Yes", TRUE ~ "No")) %>%
  mutate(sand_act_dig = as.factor(sand_act_dig))

data <- data %>% mutate(sand_act_bury = case_when(sand_act_bury == "bury" ~ "Yes", TRUE ~ "No")) %>%
  mutate(sand_act_bury = as.factor(sand_act_bury))

data <- data %>% mutate(sand_act_other = case_when(sand_act_other == "other" ~ "Yes", TRUE ~ "No")) %>%
  mutate(sand_act_other = as.factor(sand_act_other))


# Examine distribution above to determine how best to categorize key variables

data <- data %>% 
  mutate(age1 = fct_relevel(age1, "5-9", after = 1)) %>% 
  mutate(income = fct_relevel(income, "<20,000", "20000-39999", "40000-59999", "60000-79999", "80000-99999", "100000-149999", "150000+"))  %>% 
  mutate(education = fct_relevel(education, "none", "secondary", "apprenticeship", "college", "bachleors", "graduate"))

levels(data$age1)
levels(data$income)
levels(data$education)

data <- data %>% 
  mutate(education2 = case_when(
    (education == "none" | education == "secondary") ~ "high school or less",
    (education == "apprenticeship" | education == "college") ~ "college/trades",
    (education == "bachleors" | education == "bachelors") ~ "bachelors",
    education == "graduate" ~ "post-graduate",
        TRUE ~ NA_character_))  %>%
  mutate(education2 = fct_relevel(education2, "high school or less", "college/trades", "bachelors", "post-graduate"))

data <- data %>% 
  mutate(income2 = case_when(
    (income == "<20,000" | income == "20000-39999") ~ "<40,000",
    (income == "40000-59999" | income == "60000-79999") ~ "40,000-79,999",
    (income == "80000-99999" | income == "100000-149999") ~ "80,000-149,999",
    income == "150000+" ~ "150,000+",
    TRUE ~ NA_character_))  %>%
  mutate(income2 = fct_relevel(income2, "<40,000", "40,000-79,999", "80,000-149,999", "150,000+"))

data <- data %>% 
  mutate(gender = case_when(
    ((gender1 == "woman" | gender1 == "girl") & sex1 == "female") ~ "woman/girl",
    ((gender1 == "man" | gender1 == "boy") & sex1 == "male") ~ "man/boy",
    (gender1 == "nonbinary" | gender1 == "other_gender") ~ "fluid/trans",
    ((gender1 == "woman" | gender1 == "girl") & sex1 == "male") ~ "fluid/trans",
    ((gender1 == "man" | gender1 == "boy") & sex1 == "female") ~ "fluid/trans",   
    TRUE ~ NA_character_))  %>%
  mutate(gender = fct_relevel(gender, "woman/girl", "man/boy", "fluid/trans"))

data <- data %>% mutate(age1 = as.factor(age1))
data$age1 <- factor(data$age1, exclude = "NA")

data <- data %>% mutate(income = as.factor(income))
data$income <- factor(data$income, exclude = "NA")

data <- data %>% 
  mutate(age2 = case_when(
    (age1 == "0-4" | age1 == "5-9" | age1 == "10-14") ~ "0-14",
    age1 == "15-19" ~ "15-19",
    age1 == "20-39" ~ "20-39",
    (age1 == "40-59" | age1 == "60+") ~ "40+",
    TRUE ~ NA_character_))  %>%
  mutate(age2 = fct_relevel(age2, "0-14", "15-19", "20-39", "40+"))

data <- data %>% 
  mutate(age3 = case_when(
    (age1 == "0-4" | age1 == "5-9" | age1 == "10-14") ~ "0-14",
    (age1 == "15-19" | age1 == "20-39" | age1 == "40-59" | age1 == "60+") ~ "15+",   
    TRUE ~ NA_character_))  %>%
  mutate(age3 = as.factor(age3)) %>%
  mutate(age3 = fct_relevel(age3, "15+", "0-14"))

data <- data %>% 
  mutate(ethnicity = case_when(
    ethnicity_other == "other_eth" ~ "Multiple ethnicities",
    (eth_white == "Yes" & (eth_south_asian == "Yes" | eth_arab == "Yes" | eth_se_asian == "Yes" | 
                             eth_east_asian == "Yes" | eth_black == "Yes" | 
                             eth_latin == "Yes" | eth_indigenous == "Yes")) ~ "Multiple ethnicities",
    (eth_arab == "Yes" & (eth_south_asian == "Yes" | eth_se_asian == "Yes" | 
                             eth_east_asian == "Yes" | eth_black == "Yes" | 
                             eth_latin == "Yes" | eth_indigenous == "Yes")) ~ "Multiple ethnicities",
    eth_white == "Yes" ~ "White",
    eth_south_asian == "Yes" ~ "South Asian",
    eth_se_asian == "Yes" ~ "Southeast Asian",
    eth_arab == "Yes" ~ "Arab",
    eth_east_asian == "Yes" ~ "East Asian",
    eth_black == "Yes" ~ "Black",
    eth_latin == "Yes" ~ "Latin",
    eth_indigenous == "Yes" ~ "Indigenous",
    TRUE ~ NA_character_)) 

data %>% tabyl(ethnicity)  
    

# Create water sports and other minimal contact variables

data <- data %>% 
  mutate(water_sports = case_when(
    (water_act_surf == "Yes" | water_act_kite == "Yes" | water_act_wind == "Yes" | water_act_wake == "Yes" | 
       water_act_ski == "Yes" | water_act_snorkel == "Yes" | water_act_dive == "Yes") ~ "Yes",
    TRUE ~ "No")) %>%
  mutate(prim_contact = as.factor(water_sports))

data <- data %>% 
  mutate(minimal_contact = case_when(
    (water_act_sail == "Yes" | water_act_boat == "Yes" | water_act_fish == "Yes" | 
       water_act_canoe == "Yes" | water_act_kayak == "Yes" | water_act_row == "Yes") ~ "Yes",
    TRUE ~ "No")) %>%
  mutate(minimal_contact = as.factor(minimal_contact))


# Define sand contact as only digging or burying - excluding others like tanning, laying in sand

data <- data %>% 
  mutate(sand_contact = case_when(
    (sand_act_dig == "Yes" | sand_act_bury == "Yes") ~ "Yes",
    TRUE ~ "No")) %>%
  mutate(sand_contact = as.factor(sand_contact))


# Create log E. coli variable

data <- data %>% 
  mutate(log_ecoli =  log(e_coli))






