


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
  lubridate,     # working with dates
  forcats       # factors
)


# Descriptive stats for socio-demographic variables and baseline variables


data %>% tabyl(age1)
data %>% tabyl(sex1)
data %>% tabyl(gender)
data %>% tabyl(income)
data %>% tabyl(education2)
data %>% tabyl(residence)
data %>% tabyl(ethnicity_arab)
data %>% tabyl(ethnicity_black)
data %>% tabyl(ethnicity_east_asian)
data %>% tabyl(ethnicity_indigenous)
data %>% tabyl(ethnicity_latin)
data %>% tabyl(ethnicity_south_asian)
data %>% tabyl(ethnicity_se_asian)
data %>% tabyl(ethnicity_white)
data %>% tabyl(ethnicity_other)
data %>% tabyl(ethnicity_other_s)


data %>% 
  select(age1, gender, income, education2, residence, ethnicity_arab, ethnicity_black, 
         ethnicity_east_asian, ethnicity_indigenous, ethnicity_latin, ethnicity_south_asian, 
         ethnicity_se_asian, ethnicity_white) %>% 
  tbl_summary()  

data %>% 
  select(base_agi, base_resp, base_ear, base_eye, base_skin, cond_GI, cond_resp, cond_skin, 
         cond_allergy, cond_immune, cond_none, prev_act1, beach_exp_algae, beach_exp_sunscreen, 
         beach_exp_repellent, beach_exp_food) %>% 
  tbl_summary()

data %>% 
  select(water_contact, water_act_swim, water_act_surf, water_act_kite, water_act_wind, water_act_wake, 
         water_act_ski, water_act_paddle, water_act_snorkel, water_act_dive, water_act_wade, 
         water_act_sail, water_act_boat, water_act_fish, water_act_canoe, water_act_kayak, 
         water_act_row, water_act_other, water_exp_face, water_exp_mouth, sand1, sand_act_dig, 
         sand_act_bury, sand_act_other, sand_mouth1) %>% 
  tbl_summary()
```

## Summarize follow-up survey results


data_grouped <- data %>% group_by(house_id) %>% 
  summarize(n_cases  = n(), n_follow = sum(follow==1, na.rm=T),
            follow_percent = sum((n_follow/n_cases)*100))

data_follow <- data[ which(data$follow == 1), ]

data %>% tabyl(follow)

data_follow %>% 
  select(agi, diarrhea, respiratory, ear_infection, eye_infection, skin_infection, misswork, 
         med_antibiotics, med_otc, med_none, healthcare1, emergency, hospital) %>% 
  tbl_summary()

## Compare sociodemographics of those who completed follow-up survey vs. not

data %>% 
  select(age1, gender, income, education2, residence, ethnicity_arab, ethnicity_black, 
         ethnicity_east_asian, ethnicity_indigenous, ethnicity_latin, ethnicity_south_asian, 
         ethnicity_se_asian, ethnicity_white, follow) %>% 
  tbl_summary(by = follow) %>% add_p()

data %>% 
  select(base_agi, base_resp, base_ear, base_eye, base_skin, cond_GI, cond_resp, cond_skin, 
         cond_allergy, cond_immune, cond_none, prev_act1, beach_exp_algae, beach_exp_sunscreen, 
         beach_exp_repellent, beach_exp_food, follow) %>% 
  tbl_summary(by = follow) %>% add_p()

data %>% 
  select(water_contact, water_act_swim, water_act_surf, water_act_kite, water_act_wind, water_act_wake, 
         water_act_ski, water_act_paddle, water_act_snorkel, water_act_dive, water_act_wade, 
         water_act_sail, water_act_boat, water_act_fish, water_act_canoe, water_act_kayak, 
         water_act_row, water_act_other, water_exp_face, water_exp_mouth, sand1, sand_act_dig, 
         sand_act_bury, sand_act_other, sand_mouth1, follow) %>% 
  tbl_summary(by = follow) %>% add_p()


## Cross-tab water activities with face and mouth contact

data %>% 
  select(water_act_swim, water_act_surf, water_act_kite, water_act_wind, water_act_wake, 
         water_act_ski, water_act_paddle, water_act_snorkel, water_act_dive, water_act_wade, 
         water_act_sail, water_act_boat, water_act_fish, water_act_canoe, water_act_kayak, 
         water_act_row, water_act_other, water_exp_face) %>% 
  tbl_summary(by = water_exp_face)

data %>% 
  select(water_act_swim, water_act_surf, water_act_kite, water_act_wind, water_act_wake, 
         water_act_ski, water_act_paddle, water_act_snorkel, water_act_dive, water_act_wade, 
         water_act_sail, water_act_boat, water_act_fish, water_act_canoe, water_act_kayak, 
         water_act_row, water_act_other, water_exp_mouth) %>% 
  tbl_summary(by = water_exp_mouth)


