
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

entrance <- import("G:/My Drive/Research/Projects-TMU/Beach water illness/Toronto pilot study/Results/June 4 and 6 2022-entrance.xlsx")
exit <- import("G:/My Drive/Research/Projects-TMU/Beach water illness/Toronto pilot study/Results/June 4 and 6 2022-exit.xlsx")

follow <- import("G:/My Drive/Research/Projects-TMU/Beach water illness/Toronto pilot study/Results/2022-follow.xlsx")
beach <- import("G:/My Drive/Research/Projects-TMU/Beach water illness/Toronto pilot study/Results/2022-beach.xlsx")

e_coli <- import("G:/My Drive/Research/Projects-TMU/Beach water illness/Toronto pilot study/Results/E. coli data-2022.xlsx")

entrance <- entrance %>% janitor::clean_names()
exit <- exit %>% janitor::clean_names()
follow <- follow %>% janitor::clean_names()
beach <- beach %>% janitor::clean_names()


# First upload and combine data from original entrance and exit surveys, then upload combined beach data

# Combine household member answers together then split into multiple rows per respondent

entrance <- entrance %>% 
  unite(name, c("firstname", "lastname"), sep=" ") %>% 
  unite(name1, c("name", "name2", "name3"), sep=",") %>% 
  unite(age1, c("age", "age2", "age3"), sep=",") %>% 
  unite(sex1, c("sex", "sex2", "sex3"), sep=",") %>%
  unite(sex_other, c("sexother_sex", "sex2other_sex", "sex3other_sex"), sep=",") %>%  
  unite(gender1, c("gender", "gender2", "gender3"), sep=",") %>%
  unite(gender_other, c("genderother_gender", "gender2other_gender", "gender3other_gender"), sep=",") %>%  
  unite(ethnicity_arab, c("ethnicityarab", "ethnicity2arab", "ethnicity3arab"), sep=",") %>% 
  unite(ethnicity_black, c("ethnicityblack", "ethnicity2black", "ethnicity3black"), sep=",") %>% 
  unite(ethnicity_east_asian, c("ethnicityeast_asian", "ethnicity2east_asian", "ethnicity3east_asian"), sep=",") %>% 
  unite(ethnicity_indigenous, c("ethnicityindigenous", "ethnicity2indigenous", "ethnicity3indigenous"), sep=",") %>% 
  unite(ethnicity_latin, c("ethnicitylatin", "ethnicity2latin", "ethnicity3latin"), sep=",") %>% 
  unite(ethnicity_south_asian, c("ethnicitysouth_asian", "ethnicity2south_asian", "ethnicity3south_asian"), sep=",") %>% 
  unite(ethnicity_se_asian, c("ethnicitysoutheast_asian", "ethnicity2southeast_asian", "ethnicity3southeast_asian"), sep=",") %>% 
  unite(ethnicity_white, c("ethnicitywhite", "ethnicity2white", "ethnicity3white"), sep=",") %>% 
  unite(ethnicity_other, c("ethnicityother_eth_29", "ethnicity2other_eth_66", "ethnicity3other_eth_103"), sep=",") %>% 
  unite(ethnicity_other_s, c("ethnicityother_eth_30", "ethnicity2other_eth_67", "ethnicity3other_eth_104"), sep=",") %>% 
  unite(ethnicity_na, c("ethnicity_na", "ethnicity2na", "ethnicity3na"), sep=",") %>% 
  unite(base_symp_diar, c("symptomsdiarrhea", "symptoms2diarrhea", "symptoms3diarrhea"), sep=",") %>% 
  unite(base_symp_vomit, c("symptomsvomiting", "symptoms2vomiting", "symptoms3vomiting"), sep=",") %>% 
  unite(base_symp_cramps, c("symptomscramps", "symptoms2cramps", "symptoms3cramps"), sep=",") %>% 
  unite(base_symp_naus, c("symptomsnausea", "symptoms2nausea", "symptoms3nausea"), sep=",") %>% 
  unite(base_symp_fever, c("symptomsfever", "symptoms2fever", "symptoms3fever"), sep=",") %>% 
  unite(base_symp_throat, c("symptomsthroat", "symptoms2throat", "symptoms3throat"), sep=",") %>% 
  unite(base_symp_nose, c("symptomsnose", "symptoms2nose", "symptoms3nose"), sep=",") %>% 
  unite(base_symp_cough, c("symptomscough", "symptoms2cough", "symptoms3cough"), sep=",") %>% 
  unite(base_symp_ear, c("symptomsear", "symptoms2ear", "symptoms3ear"), sep=",") %>% 
  unite(base_symp_eye, c("symptomseye", "symptoms2eye", "symptoms3eye"), sep=",") %>% 
  unite(base_symp_rash, c("symptomsrash", "symptoms2rash", "symptoms3rash"), sep=",") %>% 
  unite(base_symp_none, c("symptomsnone", "symptoms2none", "symptoms3none"), sep=",") %>% 
  unite(cond_GI, c("conditions_gi", "conditions2gi", "conditions3gi"), sep=",") %>% 
  unite(cond_resp, c("conditionsrespiratory", "conditions2respiratory", "conditions3respiratory"), sep=",") %>% 
  unite(cond_skin, c("conditionsskin", "conditions2skin", "conditions3skin"), sep=",") %>% 
  unite(cond_allergy, c("conditionsallergies", "conditions2allergies", "conditions3allergies"), sep=",")  %>% 
  unite(cond_immune, c("conditionsimmune", "conditions2immune", "conditions3immune"), sep=",")  %>%
  unite(cond_none, c("conditionsnone", "conditions2none", "conditions3none"), sep=",")  %>%
  unite(prev_act1, c("prev_act", "prev_act2", "prev_act3"), sep=",") %>%
  unite(others_ent, c("others", "others2", "others3"), sep=",")

entrance <- entrance %>% 
  separate_rows(name1, age1, sex1, sex_other, gender1, gender_other, ethnicity_arab, ethnicity_black, 
                ethnicity_east_asian, ethnicity_indigenous, ethnicity_latin, ethnicity_south_asian, 
                ethnicity_se_asian, ethnicity_white, ethnicity_other, ethnicity_other_s, ethnicity_na,
                base_symp_diar, base_symp_vomit, base_symp_cramps, base_symp_naus, base_symp_fever, 
                base_symp_throat, base_symp_nose, base_symp_cough, base_symp_ear, base_symp_eye, 
                base_symp_rash, base_symp_none, cond_GI, cond_resp, cond_skin, cond_allergy, 
                cond_immune, cond_none, prev_act1, others_ent, sep=",") 

entrance <- entrance[!(entrance$name1=="NA"),]
entrance <- select(entrance, -name4:-prev_act10)

exit <- exit %>% 
  unite(name, c("fname", "lname"), sep=" ") %>% 
  unite(name1, c("name", "name2", "name3"), sep=",") %>% 
  unite(water_contact, c("swam", "swam2", "swam3"), sep=",") %>% 
  unite(water_act_swim, c("water_actswim", "water_act2swim", "water_act3swim"), sep=",") %>% 
  unite(water_act_surf, c("water_actsurf", "water_act2surf", "water_act3surf"), sep=",") %>% 
  unite(water_act_kite, c("water_actkite", "water_act2kite", "water_act3kite"), sep=",") %>% 
  unite(water_act_wind, c("water_actwind", "water_act2wind", "water_act3wind"), sep=",") %>% 
  unite(water_act_wake, c("water_actwake", "water_act2wake", "water_act3wake"), sep=",") %>% 
  unite(water_act_ski, c("water_actski", "water_act2ski", "water_act3ski"), sep=",") %>% 
  unite(water_act_paddle, c("water_actpaddle", "water_act2paddle", "water_act3paddle"), sep=",") %>% 
  unite(water_act_snorkel, c("water_actsnorkel", "water_act2snorkel", "water_act3snorkel"), sep=",") %>% 
  unite(water_act_dive, c("water_actdive", "water_act2dive", "water_act3dive"), sep=",") %>% 
  unite(water_act_wade, c("water_actwade", "water_act2wade", "water_act3wade"), sep=",") %>% 
  unite(water_act_sail, c("water_actsail", "water_act2sail", "water_act3sail"), sep=",") %>% 
  unite(water_act_boat, c("water_actboat", "water_act2boat", "water_act3boat"), sep=",") %>% 
  unite(water_act_fish, c("water_actfish", "water_act2fish", "water_act3fish"), sep=",") %>% 
  unite(water_act_canoe, c("water_actcanoe", "water_act2canoe", "water_act3canoe"), sep=",") %>% 
  unite(water_act_kayak, c("water_actkayak", "water_act2kayak", "water_act3kayak"), sep=",") %>% 
  unite(water_act_other, c("water_actother_23", "water_act2other_57", "water_act3other_91"), sep=",") %>%   
  unite(water_act_other_s, c("water_actother_24", "water_act2other_58", "water_act3other_92"), sep=",") %>%   
  unite(water_act_row, c("water_actrow", "water_act2row", "water_act3row"), sep=",") %>% 
  unite(water_exp_face, c("water_expface", "water_exp2face", "water_exp3face"), sep=",") %>% 
  unite(water_exp_mouth, c("water_expmouth", "water_exp2mouth", "water_exp3mouth"), sep=",") %>% 
  unite(water_exp_neither, c("water_expneither", "water_exp2neither", "water_exp3neither"), sep=",") %>% 
  unite(beach_exp_algae, c("beach_expalgae", "beach_exp2algae", "beach_exp3algae"), sep=",") %>% 
  unite(beach_exp_sun, c("beach_expsun", "beach_exp2sun", "beach_exp3sun"), sep=",") %>% 
  unite(beach_exp_rep, c("beach_exprepellent", "beach_exp2repellent", "beach_exp3repellent"), sep=",") %>% 
  unite(beach_exp_food, c("beach_expfood", "beach_exp2food", "beach_exp3food"), sep=",") %>% 
  unite(sand1, c("sand", "sand2", "sand3"), sep=",") %>% 
  unite(sand_act_dig, c("sand_actdig", "sand_act2dig", "sand_act3dig"), sep=",") %>% 
  unite(sand_act_bury, c("sand_actbury", "sand_act2bury", "sand_act3bury"), sep=",") %>%
  unite(sand_act_other, c("sand_actother_35", "sand_act2other_69", "sand_act3other_103"), sep=",") %>%   
  unite(sand_act_other_s, c("sand_actother_36", "sand_act2other_70", "sand_act3other_104"), sep=",") %>% 
  unite(sand_mouth1, c("sand_mouth", "sand_mouth2", "sand_mouth3"), sep=",")  %>% 
  unite(others, c("others", "others2", "others3"), sep=",") 

exit <- exit %>% 
  separate_rows(name1, water_contact, water_act_swim, water_act_surf, water_act_kite,
                water_act_wind, water_act_wake, water_act_ski, water_act_paddle, 
                water_act_snorkel, water_act_dive, water_act_wade,
                water_act_sail, water_act_boat, water_act_fish, water_act_canoe,
                water_act_kayak, water_act_row, water_act_other, water_act_other_s,
                water_exp_face, water_exp_mouth, water_exp_neither, beach_exp_algae, 
                beach_exp_sun, beach_exp_rep, beach_exp_food, sand1, sand_act_dig, 
                sand_act_bury, sand_act_other, sand_act_other_s, sand_mouth1,
                others, sep=",") 

exit <- exit[!(exit$name1=="NA"),]
exit <- select(exit, -name4:-sand_mouth10)


beach <- beach %>% 
  unite(name, c("firstname", "lastname"), sep=" ") %>% 
  unite(name1, c("name", "name2", "name3", "name4", "name5", "name6"), sep=",") %>% 
  unite(age1, c("age", "age2", "age3", "age4", "age5", "age6"), sep=",") %>% 
  unite(sex1, c("sex", "sex2", "sex3", "sex4", "sex5", "sex6"), sep=",") %>%
  unite(sex_other, c("sex_other_sex", "sex2_other_sex", "sex3_other_sex", "sex4_other_sex", "sex5_other_sex", "sex6_other_sex"), sep=",") %>%  
  unite(gender1, c("gender", "gender2", "gender3", "gender4", "gender5", "gender6"), sep=",") %>%
  unite(gender_other, c("gender_other_gender", "gender2_other_gender", "gender3_other_gender", "gender4_other_gender", "gender5_other_gender", "gender6_other_gender"), sep=",") %>%  
  unite(ethnicity_arab, c("ethnicity_arab", "ethnicity2_arab", "ethnicity3_arab", "ethnicity4_arab", "ethnicity5_arab", "ethnicity6_arab"), sep=",") %>% 
  unite(ethnicity_black, c("ethnicity_black", "ethnicity2_black", "ethnicity3_black", "ethnicity4_black", "ethnicity5_black", "ethnicity6_black"), sep=",") %>% 
  unite(ethnicity_east_asian, c("ethnicity_east_asian", "ethnicity2_east_asian", "ethnicity3_east_asian", "ethnicity4_east_asian", "ethnicity5_east_asian", "ethnicity6_east_asian"), sep=",") %>% 
  unite(ethnicity_indigenous, c("ethnicity_indigenous", "ethnicity2_indigenous", "ethnicity3_indigenous", "ethnicity4_indigenous", "ethnicity5_indigenous", "ethnicity6_indigenous"), sep=",") %>% 
  unite(ethnicity_latin, c("ethnicity_latin", "ethnicity2_latin", "ethnicity3_latin", "ethnicity4_latin", "ethnicity5_latin", "ethnicity6_latin"), sep=",") %>% 
  unite(ethnicity_south_asian, c("ethnicity_south_asian", "ethnicity2_south_asian", "ethnicity3_south_asian", "ethnicity4_south_asian", "ethnicity5_south_asian", "ethnicity6_south_asian"), sep=",") %>% 
  unite(ethnicity_se_asian, c("ethnicity_southeast_asian", "ethnicity2_southeast_asian", "ethnicity3_southeast_asian", "ethnicity4_southeast_asian", "ethnicity5_southeast_asian", "ethnicity6_southeast_asian"), sep=",") %>% 
  unite(ethnicity_white, c("ethnicity_white", "ethnicity2_white", "ethnicity3_white", "ethnicity4_white", "ethnicity5_white", "ethnicity6_white"), sep=",") %>% 
  unite(ethnicity_other, c("ethnicity_other_eth_29", "ethnicity2_other_eth_98", "ethnicity3_other_eth_167", "ethnicity4_other_eth_236", "ethnicity5_other_eth_305", "ethnicity6_other_eth_374"), sep=",") %>% 
  unite(ethnicity_other_s, c("ethnicity_other_eth_30", "ethnicity2_other_eth_99", "ethnicity3_other_eth_168", "ethnicity4_other_eth_237", "ethnicity5_other_eth_306", "ethnicity6_other_eth_375"), sep=",") %>% 
  unite(ethnicity_na, c("ethnicity_na", "ethnicity2_na", "ethnicity3_na", "ethnicity4_na", "ethnicity5_na", "ethnicity6_na"), sep=",") %>% 
  unite(base_symp_diar, c("symptoms_diarrhea", "symptoms2_diarrhea", "symptoms3_diarrhea", "symptoms4_diarrhea", "symptoms5_diarrhea", "symptoms6_diarrhea"), sep=",") %>% 
  unite(base_symp_vomit, c("symptoms_vomiting", "symptoms2_vomiting", "symptoms3_vomiting", "symptoms4_vomiting", "symptoms5_vomiting", "symptoms6_vomiting"), sep=",") %>% 
  unite(base_symp_cramps, c("symptoms_cramps", "symptoms2_cramps", "symptoms3_cramps", "symptoms4_cramps", "symptoms5_cramps", "symptoms6_cramps"), sep=",") %>% 
  unite(base_symp_naus, c("symptoms_nausea", "symptoms2_nausea", "symptoms3_nausea", "symptoms4_nausea", "symptoms5_nausea", "symptoms6_nausea"), sep=",") %>% 
  unite(base_symp_fever, c("symptoms_fever", "symptoms2_fever", "symptoms3_fever", "symptoms4_fever", "symptoms5_fever", "symptoms6_fever"), sep=",") %>% 
  unite(base_symp_throat, c("symptoms_throat", "symptoms2_throat", "symptoms3_throat", "symptoms4_throat", "symptoms5_throat", "symptoms6_throat"), sep=",") %>% 
  unite(base_symp_nose, c("symptoms_nose", "symptoms2_nose", "symptoms3_nose", "symptoms4_nose", "symptoms5_nose", "symptoms6_nose"), sep=",") %>% 
  unite(base_symp_cough, c("symptoms_cough", "symptoms2_cough", "symptoms3_cough", "symptoms4_cough",  "symptoms5_cough", "symptoms6_cough"), sep=",") %>% 
  unite(base_symp_ear, c("symptoms_ear", "symptoms2_ear", "symptoms3_ear", "symptoms4_ear", "symptoms5_ear", "symptoms6_ear"), sep=",") %>% 
  unite(base_symp_eye, c("symptoms_eye", "symptoms2_eye", "symptoms3_eye", "symptoms4_eye", "symptoms5_eye", "symptoms6_eye"), sep=",") %>% 
  unite(base_symp_rash, c("symptoms_rash", "symptoms2_rash", "symptoms3_rash", "symptoms4_rash", "symptoms5_rash", "symptoms6_rash"), sep=",") %>% 
  unite(base_symp_none, c("symptoms_none", "symptoms2_none", "symptoms3_none", "symptoms4_none", "symptoms5_none", "symptoms6_none"), sep=",") %>% 
  unite(cond_GI, c("conditions_gi", "conditions2_gi", "conditions3_gi", "conditions4_gi", "conditions5_gi", "conditions6_gi"), sep=",") %>% 
  unite(cond_resp, c("conditions_respiratory", "conditions2_respiratory", "conditions3_respiratory", "conditions4_respiratory", "conditions5_respiratory", "conditions6_respiratory"), sep=",") %>% 
  unite(cond_skin, c("conditions_skin", "conditions2_skin", "conditions3_skin", "conditions4_skin", "conditions5_skin", "conditions6_skin"), sep=",") %>% 
  unite(cond_allergy, c("conditions_allergies", "conditions2_allergies", "conditions3_allergies", "conditions4_allergies", "conditions5_allergies", "conditions6_allergies"), sep=",")  %>% 
  unite(cond_immune, c("conditions_immune", "conditions2_immune", "conditions3_immune", "conditions4_immune", "conditions5_immune", "conditions6_immune"), sep=",")  %>%
  unite(cond_none, c("conditions_none", "conditions2_none", "conditions3_none", "conditions4_none", "conditions5_none", "conditions6_none"), sep=",")  %>%
  unite(prev_act1, c("prev_act", "prev_act2", "prev_act3", "prev_act4", "prev_act5", "prev_act6"), sep=",") %>%
  unite(water_contact, c("swam", "swam2", "swam3", "swam4", "swam5", "swam6"), sep=",") %>% 
  unite(water_act_swim, c("water_act_swim", "water_act2_swim", "water_act3_swim", "water_act4_swim", "water_act5_swim", "water_act6_swim"), sep=",") %>% 
  unite(water_act_surf, c("water_act_surf", "water_act2_surf", "water_act3_surf", "water_act4_surf", "water_act5_surf", "water_act6_surf"), sep=",") %>% 
  unite(water_act_kite, c("water_act_kite", "water_act2_kite", "water_act3_kite", "water_act4_kite", "water_act5_kite", "water_act6_kite"), sep=",") %>% 
  unite(water_act_wind, c("water_act_wind", "water_act2_wind", "water_act3_wind", "water_act4_wind", "water_act5_wind", "water_act6_wind"), sep=",") %>% 
  unite(water_act_wake, c("water_act_wake", "water_act2_wake", "water_act3_wake", "water_act4_wake", "water_act5_wake", "water_act6_wake"), sep=",") %>% 
  unite(water_act_ski, c("water_act_ski", "water_act2_ski", "water_act3_ski", "water_act4_ski", "water_act5_ski", "water_act6_ski"), sep=",") %>% 
  unite(water_act_paddle, c("water_act_paddle", "water_act2_paddle", "water_act3_paddle", "water_act4_paddle", "water_act5_paddle", "water_act6_paddle"), sep=",") %>% 
  unite(water_act_snorkel, c("water_act_snorkel", "water_act2_snorkel", "water_act3_snorkel", "water_act4_snorkel", "water_act5_snorkel", "water_act6_snorkel"), sep=",") %>% 
  unite(water_act_dive, c("water_act_dive", "water_act2_dive", "water_act3_dive", "water_act4_dive", "water_act5_dive", "water_act6_dive"), sep=",") %>% 
  unite(water_act_wade, c("water_act_wade", "water_act2_wade", "water_act3_wade", "water_act4_wade", "water_act5_wade", "water_act6_wade"), sep=",") %>% 
  unite(water_act_sail, c("water_act_sail", "water_act2_sail", "water_act3_sail", "water_act4_sail", "water_act5_sail", "water_act6_sail"), sep=",") %>% 
  unite(water_act_boat, c("water_act_boat", "water_act2_boat", "water_act3_boat", "water_act4_boat", "water_act5_boat", "water_act6_boat"), sep=",") %>% 
  unite(water_act_fish, c("water_act_fish", "water_act2_fish", "water_act3_fish", "water_act4_fish", "water_act5_fish", "water_act6_fish"), sep=",") %>% 
  unite(water_act_canoe, c("water_act_canoe", "water_act2_canoe", "water_act3_canoe", "water_act4_canoe", "water_act5_canoe", "water_act6_canoe"), sep=",") %>% 
  unite(water_act_kayak, c("water_act_kayak", "water_act2_kayak", "water_act3_kayak", "water_act4_kayak", "water_act5_kayak", "water_act6_kayak"), sep=",") %>% 
  unite(water_act_row, c("water_act_row", "water_act2_row", "water_act3_row", "water_act4_row", "water_act5_row", "water_act6_row"), sep=",") %>% 
  unite(water_act_other, c("water_act_other_68", "water_act2_other_137", "water_act3_other_206", "water_act4_other_275", "water_act5_other_344", "water_act6_other_413"), sep=",") %>%   
  unite(water_act_other_s, c("water_act_other_69", "water_act2_other_138", "water_act3_other_207", "water_act4_other_276", "water_act5_other_345", "water_act6_other_414"), sep=",") %>% 
  unite(water_exp_face, c("water_exp_face", "water_exp2_face", "water_exp3_face", "water_exp4_face", "water_exp5_face", "water_exp6_face"), sep=",") %>% 
  unite(water_exp_mouth, c("water_exp_mouth", "water_exp2_mouth", "water_exp3_mouth", "water_exp4_mouth", "water_exp5_mouth", "water_exp6_mouth"), sep=",") %>% 
  unite(water_exp_neither, c("water_exp_neither", "water_exp2_neither", "water_exp3_neither", "water_exp4_neither", "water_exp5_neither", "water_exp6_neither"), sep=",") %>% 
  unite(beach_exp_algae, c("beach_exp_algae", "beach_exp2_algae", "beach_exp3_algae", "beach_exp4_algae", "beach_exp5_algae", "beach_exp6_algae"), sep=",") %>% 
  unite(beach_exp_sun, c("beach_exp_sun", "beach_exp2_sun", "beach_exp3_sun", "beach_exp4_sun", "beach_exp5_sun", "beach_exp6_sun"), sep=",") %>% 
  unite(beach_exp_rep, c("beach_exp_repellent", "beach_exp2_repellent", "beach_exp3_repellent", "beach_exp4_repellent", "beach_exp5_repellent", "beach_exp6_repellent"), sep=",") %>% 
  unite(beach_exp_food, c("beach_exp_food", "beach_exp2_food", "beach_exp3_food", "beach_exp4_food", "beach_exp5_food", "beach_exp6_food"), sep=",") %>% 
  unite(sand1, c("sand", "sand2", "sand3", "sand4", "sand5", "sand6"), sep=",") %>% 
  unite(sand_act_dig, c("sand_act_dig", "sand_act2_dig", "sand_act3_dig", "sand_act4_dig", "sand_act5_dig", "sand_act6_dig"), sep=",") %>% 
  unite(sand_act_bury, c("sand_act_bury", "sand_act2_bury", "sand_act3_bury", "sand_act4_bury", "sand_act5_bury", "sand_act6_bury"), sep=",") %>% 
  unite(sand_act_other, c("sand_act_other_80", "sand_act2_other_149", "sand_act3_other_218", "sand_act4_other_287", "sand_act5_other_356", "sand_act6_other_425"), sep=",") %>%   
  unite(sand_act_other_s, c("sand_act_other_81", "sand_act2_other_150", "sand_act3_other_219", "sand_act4_other_288", "sand_act5_other_357", "sand_act6_other_426"), sep=",") %>% 
  unite(sand_mouth1, c("sand_mouth", "sand_mouth2", "sand_mouth3", "sand_mouth4", "sand_mouth5", "sand_mouth6"), sep=",")  %>% 
  unite(others, c("others", "others2", "others3", "others4", "others5", "others6"), sep=",")

beach <- beach %>% 
  separate_rows(name1, age1, sex1, sex_other, gender1, gender_other, ethnicity_arab, ethnicity_black, 
                ethnicity_east_asian, ethnicity_indigenous, ethnicity_latin, ethnicity_south_asian, 
                ethnicity_se_asian, ethnicity_white, ethnicity_other, ethnicity_other_s, ethnicity_na, 
                base_symp_diar, base_symp_vomit, base_symp_cramps, base_symp_naus, base_symp_fever, 
                base_symp_throat, base_symp_nose, base_symp_cough, base_symp_ear, base_symp_eye, 
                base_symp_rash, base_symp_none, cond_GI, cond_resp, cond_skin, cond_allergy, 
                cond_immune, cond_none, prev_act1, water_contact, water_act_swim, water_act_surf, 
                water_act_kite, water_act_wind, water_act_wake, water_act_ski, water_act_paddle, 
                water_act_snorkel, water_act_dive, water_act_wade, water_act_sail, water_act_boat, 
                water_act_fish, water_act_canoe, water_act_kayak, water_act_row, water_act_other, 
                water_act_other_s, water_exp_face, water_exp_mouth, water_exp_neither, beach_exp_algae, 
                beach_exp_sun, beach_exp_rep, beach_exp_food, sand1, sand_act_dig, 
                sand_act_bury, sand_act_other, sand_act_other_s, sand_mouth1,
                others, sep=",") 

beach <- beach[!(beach$name1=="NA"),]
beach <- select(beach, -name7:-sand_mouth10)


follow <- follow %>% 
  unite(name, c("fname", "lname"), sep=" ") %>% 
  unite(name1, c("name", "name2", "name3", "name4", "name5", "name6"), sep=",") %>% 
  unite(rec_act1, c("rec_act", "rec_act2", "rec_act3", "rec_act4", "rec_act5", "rec_act6"), sep=",") %>% 
  unite(symptoms_diar, c("symptoms_diarrhea", "symptoms2_diarrhea", "symptoms3_diarrhea", "symptoms4_diarrhea", "symptoms5_diarrhea", "symptoms6_diarrhea"), sep=",") %>% 
  unite(symptoms_vomit, c("symptoms_vomiting", "symptoms2_vomiting", "symptoms3_vomiting", "symptoms4_vomiting", "symptoms5_vomiting", "symptoms6_vomiting"), sep=",") %>% 
  unite(symptoms_cramps, c("symptoms_cramps", "symptoms2_cramps", "symptoms3_cramps", "symptoms4_cramps", "symptoms5_cramps", "symptoms6_cramps"), sep=",") %>% 
  unite(symptoms_naus, c("symptoms_nausea", "symptoms2_nausea", "symptoms3_nausea", "symptoms4_nausea", "symptoms5_nausea", "symptoms6_nausea"), sep=",") %>% 
  unite(symptoms_fever, c("symptoms_fever", "symptoms2_fever", "symptoms3_fever", "symptoms4_fever", "symptoms5_fever", "symptoms6_fever"), sep=",") %>% 
  unite(symptoms_throat, c("symptoms_throat", "symptoms2_throat", "symptoms3_throat", "symptoms4_throat", "symptoms5_throat", "symptoms6_throat"), sep=",") %>% 
  unite(symptoms_nose, c("symptoms_nose", "symptoms2_nose", "symptoms3_nose", "symptoms4_nose", "symptoms5_nose", "symptoms6_nose"), sep=",") %>% 
  unite(symptoms_cough, c("symptoms_cough", "symptoms2_cough", "symptoms3_cough", "symptoms4_cough", "symptoms5_cough","symptoms6_cough"), sep=",") %>% 
  unite(symptoms_ear, c("symptoms_ear", "symptoms2_ear", "symptoms3_ear", "symptoms4_ear", "symptoms5_ear", "symptoms6_ear"), sep=",") %>% 
  unite(symptoms_eye, c("symptoms_eye", "symptoms2_eye", "symptoms3_eye", "symptoms4_eye", "symptoms5_eye", "symptoms6_eye"), sep=",") %>% 
  unite(symptoms_rash, c("symptoms_rash", "symptoms2_rash", "symptoms3_rash", "symptoms4_rash", "symptoms5_rash", "symptoms6_rash"), sep=",") %>% 
  unite(symptoms_none, c("symptoms_none", "symptoms2_none", "symptoms3_none", "symptoms4_none", "symptoms5_none", "symptoms6_none"), sep=",") %>% 
  unite(symp_date_diar, c("symp_date_diarrhea_date", "symp_date2_diarrhea_date", "symp_date3_diarrhea_date", "symp_date4_diarrhea_date", "symp_date5_diarrhea_date","symp_date6_diarrhea_date"), sep=",") %>% 
  unite(symp_date_vomit, c("symp_date_vomiting_date", "symp_date2_vomiting_date", "symp_date3_vomiting_date", "symp_date4_vomiting_date", "symp_date5_vomiting_date", "symp_date6_vomiting_date"), sep=",") %>% 
  unite(symp_date_cramps, c("symp_date_cramps_date", "symp_date2_cramps_date", "symp_date3_cramps_date", "symp_date4_cramps_date", "symp_date5_cramps_date", "symp_date6_cramps_date"), sep=",") %>% 
  unite(symp_date_naus, c("symp_date_nausea_date", "symp_date2_nausea_date", "symp_date3_nausea_date", "symp_date4_nausea_date", "symp_date5_nausea_date", "symp_date6_nausea_date"), sep=",") %>% 
  unite(symp_date_fever, c("symp_date_fever_date", "symp_date2_fever_date", "symp_date3_fever_date", "symp_date4_fever_date", "symp_date5_fever_date", "symp_date6_fever_date"), sep=",") %>% 
  unite(symp_date_throat, c("symp_date_throat_date", "symp_date2_throat_date", "symp_date3_throat_date", "symp_date4_throat_date", "symp_date5_throat_date", "symp_date6_throat_date"), sep=",") %>% 
  unite(symp_date_nose, c("symp_date_nose_date", "symp_date2_nose_date", "symp_date3_nose_date", "symp_date4_nose_date", "symp_date5_nose_date", "symp_date6_nose_date"), sep=",") %>% 
  unite(symp_date_cough, c("symp_date_cough_date", "symp_date2_cough_date", "symp_date3_cough_date", "symp_date4_cough_date", "symp_date5_cough_date", "symp_date6_cough_date"), sep=",") %>% 
  unite(symp_date_ear, c("symp_date_ear_date", "symp_date2_ear_date", "symp_date3_ear_date", "symp_date4_ear_date", "symp_date5_ear_date", "symp_date6_ear_date"), sep=",") %>% 
  unite(symp_date_eye, c("symp_date_eye_date", "symp_date2_eye_date", "symp_date3_eye_date", "symp_date4_eye_date", "symp_date5_eye_date", "symp_date6_eye_date"), sep=",") %>% 
  unite(symp_date_rash, c("symp_date_rash_date", "symp_date2_rash_date", "symp_date3_rash_date", "symp_date4_rash_date", "symp_date5_rash_date", "symp_date6_rash_date"), sep=",") %>% 
  unite(misswork, c("misswork", "misswork2", "misswork3", "misswork4", "misswork5", "misswork6"), sep=",") %>% 
  unite(misswork_days, c("misswork_1", "misswork2_1", "misswork3_1", "misswork4_1", "misswork5_1", "misswork6_1"), sep=",") %>% 
  unite(med_antibiotics, c("medication_antibiotics", "medication2_antibiotics", "medication3_antibiotics", "medication4_antibiotics", "medications5_antibiotics", "medication6_antibiotics"), sep=",") %>% 
  unite(med_otc, c("medication_otc_drugs", "medication2_otc_drugs", "medication3_otc_drugs", "medication4_otc_drugs", "medications5_otc_drugs", "medication6_otc_drugs"), sep=",") %>% 
  unite(med_none, c("medication_none", "medication2_none", "medication3_none", "medication4_none", "medications5_none", "medication6_none"), sep=",")  %>% 
  unite(healthcare1, c("healthcare", "healthcare2", "healthcare3", "healthcare4", "healthcare5", "healthcare6"), sep=",")  %>%
  unite(emergency, c("ed_visit", "ed_visit2", "ed_visit3", "ed_visit4", "ed_visit5", "ed_visit6"), sep=",")  %>%
  unite(hospital, c("hospitalized", "hospitalized2", "hospitalized3", "hospitalized4", "hospitalized5", "hospitalized6"), sep=",") %>%
  unite(others_follow, c("others", "others2", "others3", "others4", "others5", "others6"), sep=",")

follow <- follow %>% 
  separate_rows(name1, rec_act1, symptoms_diar, symptoms_vomit, symptoms_cramps, 
                symptoms_naus, symptoms_fever, symptoms_throat, symptoms_nose, 
                symptoms_cough, symptoms_ear, symptoms_eye, symptoms_rash,
                symptoms_none, symp_date_diar, symp_date_vomit, symp_date_cramps, 
                symp_date_naus, symp_date_fever, symp_date_throat, symp_date_nose, 
                symp_date_cough, symp_date_ear, symp_date_eye, symp_date_rash,
                misswork, misswork_days, med_antibiotics, med_otc, med_none, healthcare1, 
                emergency, hospital, others_follow, sep=",") 

follow <- follow[!(follow$name1=="NA"),]


# Merge survey datasets

pacman::p_load(data.table)

beach_separate <- left_join(entrance, exit, by = "name1")

beach_separate <- beach_separate %>%
  rename(house_id = internal_id.x) %>% 
  rename(date = submitted_date.x) 

beach_separate <- subset(beach_separate, verify!="NA")

beach <- beach %>% 
  rename(house_id = internal_id) %>% 
  rename(date = submitted_date) 

beach_surveys <- rbindlist(list(beach, beach_separate), fill=TRUE) 

survey_data <- left_join(beach_surveys, follow, by = "name1")


# Check for duplicate names
survey_data %>% group_by(name1) %>% filter(n()>1) 


## Check for any follow-up participants that did not match to beach participants

follow %>% anti_join(beach_surveys, by = "name1")
investigate <- follow %>% anti_join(beach_surveys, by = "name1")


## Merge E. coli data

e_coli <- e_coli %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) 

survey_data <- survey_data %>% 
  mutate(date = as.Date(date))  %>% 
  mutate(month = as.factor(month(date))) %>% 
  mutate(dow = as.factor(wday(date))) # Sunday is 1, Sat. is 7

survey_data <- left_join(survey_data, e_coli, by = "date")


### Replace name column for confidential reasons with unique ID - drop email, phone

pacman::p_load(uuid)

survey_data$row_id <- 1:nrow(survey_data)

survey_data <- survey_data %>% 
  group_by(name1) %>% 
  mutate(
    id = uuid::UUIDgenerate(use.time = FALSE)
  ) %>% 
  ungroup() %>% 
  select(-name1, participant_id = id) %>% 
  relocate(participant_id)

survey_data <- survey_data %>% 
  relocate(participant_id, .after = house_id)  %>% 
  relocate(row_id, .after = participant_id)

data <- subset(survey_data, select = -c(email.x, email.y, phone))

remove(beach, beach_separate, beach_surveys, entrance, exit, follow, investigate, survey_data)



