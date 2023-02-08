remove(list = ls())

library(pollster)
library(data.table)
library(tidyverse)
library(ggsci)
library(sjPlot)
library(jtools)
library(srvyr)
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(urbnmapr)
library(RColorBrewer)
library(scales)
library(tidycensus)
library(weights)
library(dplyr)
library(officer)
library(rvg)
library(stargazer)
library(sjPlot)
library(gtsummary)
theme_gtsummary_compact()
theme_gtsummary_journal(journal = "jama")
library(ggeffects)
library(lavaan)
library(tidySEM)
library(psych)

# Note, that data.csv uploaded to Github is saved after the following lines of code (saved at line 582)


# 
# df1<-fread("CovidNearYou Datafile 08 May 10PM.csv")
# 
# df1$date <- as.Date(df1$start_date)
# 
# df2 <- df1 %>%
#   filter(date >= as.Date("2022-01-01")) %>%
#   filter(age >=18) %>%
#   filter(!is.na(race_recode)) %>%
#   filter(have_children_mc_5_to_11 == 1 | have_children_mc_12_to_15 == 1 | have_children_mc_16_to_17 == 1)  %>%
#   filter(have_children_mc_under5 != 1) %>% 
#   dplyr::select(response_id,date,race_recode,
#                 get_vaccine,
#                 party_id,
#                 gender,
#                 education,
#                 employment_status,
#                 income,
#                 friends_or_family_at_risk_covid19,
#                 religion,
#                 get_vaccine_yesno,
#                 received_all_covid19_required_doses,
#                 educ4,
#                 is_essential_worker,
#                 age,
#                 have_children_mc_none,
#                 have_children_mc_under5,
#                 have_children_mc_5_to_11,
#                 have_children_mc_12_to_15,
#                 have_children_mc_16_to_17,
#                 willing_children_get_covid19_vaccine_mx_12_to_15,
#                 willing_children_get_covid19_vaccine_mx_16_to_17,
#                 willing_children_get_covid19_vaccine_mx_5_to_11,
#                 willing_children_get_covid19_vaccine_mx_under5,
#                 where_willing_children_get_vaccine_mc_pharmacy,
#                 where_willing_children_get_vaccine_mc_other,
#                 where_willing_children_get_vaccine_mc_primary_health_care_sites,
#                 where_willing_children_get_vaccine_mc_health_centers,
#                 where_willing_children_get_vaccine_mc_hospital,
#                 where_willing_children_get_vaccine_mc_mass_vaccine_sites,
#                 where_willing_children_get_vaccine_mc_mobile_vaccine_sites,
#                 where_willing_children_get_vaccine_mc_retail_sites,
#                 where_willing_children_get_vaccine_mc_place_of_worship,
#                 where_willing_get_vaccine_mc_pharmacy_other_text,
#                 where_willing_children_get_vaccine_mc_other_text,
#                 self_report_health,
#                 likely_wear_mask_exercising_outside,
#                 likely_wear_mask_grocery_shopping,
#                 likely_wear_mask_visit_family_friends,
#                 likely_wear_mask_workplace,
#                 flu_vaccine_since_june_2021,
#                 flu_vaccine_since_june_2020,
#                 plan_flu_vaccine_mc_always_get,
#                 plan_flu_vaccine_mc_other,
#                 plan_flu_vaccine_mc_never_get,
#                 plan_flu_vaccine_mc_covid19_pandemic,
#                 plan_flu_vaccine_mc_waitsee__how_bad_flu_season,
#                 plan_flu_vaccine_mc_wait_see_effectiveness,
#                 plan_flu_vaccine_mc_close_someone_high_risk,
#                 plan_flu_vaccine_mc_not_recommended_health,
#                 get_flu_vaccine_before_jan2020,
#                 get_vaccine_upcoming_flu_season,
#                 getting_covid19_booster,
#                 plan_to_get_covid19_vaccine,
#                 comfortable_children_attend_school_mc_12to15,
#                 comfortable_children_attend_school_mc_16to17,
#                 comfortable_children_attend_school_mc_5to11,
#                 comfortable_children_attend_school_mc_under5,
#                 children_going_to_school_mc_12to15,
#                 children_going_to_school_mc_16to17,
#                 children_going_to_school_mc_5to11,
#                 children_going_to_school_mc_under5,
#                 covid19_booster_likely_to_get,
#                 why_not_planning_get_vaccine_mc_already_received_flu_vaccine,
#                 why_not_planning_get_vaccine_child_mc_too_new,
#                 why_not_planning_get_vaccine_child_mc_other,
#                 why_not_planning_get_vaccine_child_mc_too_young,
#                 why_not_planning_get_vaccine_child_mc_let_them_decide,
#                 why_not_planning_get_vaccine_child_mc_side_effect,
#                 why_not_planning_get_vaccine_child_mc_threat_exaggerated,
#                 why_not_planning_get_vaccine_child_mc_lack_trust_government,
#                 why_not_planning_get_vaccine_child_mc_lack_trust_scientist,
#                 why_not_planning_get_vaccine_child_mc_politics,
#                 why_not_planning_get_vaccine_child_mc_already_had_covid,
#                 why_not_planning_get_vaccine_child_mc_never_get_vaccine,
#                 why_not_planning_get_vaccine_child_mc_health_history,
#                 why_not_planning_get_vaccine_child_mc_worried_get_covid,
#                 why_not_planning_get_vaccine_child_mc_not_at_risk,
#                 why_not_planning_get_vaccine_child_mc_risk_higher_infection,
#                 why_not_planning_get_vaccine_child_mc_afraid_needles,
#                 why_not_planning_get_vaccine_child_mc_contr_religious_belief,
#                 why_not_planning_get_vaccine_child_mc_wait_herd_immunity,
#                 why_not_planning_get_vaccine_child_mc_others_before_me,
#                 why_not_planning_get_vaccine_child_mc_already_recvd_flu_vaccine,
#                 why_not_sure_get_vaccine_child_mc_too_new,
#                 why_not_sure_get_vaccine_child_mc_other,
#                 why_not_sure_get_vaccine_child_mc_too_young,
#                 why_not_sure_get_vaccine_child_mc_let_them_decide,
#                 why_not_sure_get_vaccine_child_mc_side_effect,
#                 why_not_sure_get_vaccine_child_mc_threat_exaggerated,
#                 why_not_sure_get_vaccine_child_mc_lack_trust_government,
#                 why_not_sure_get_vaccine_child_mc_lack_trust_scientist,
#                 why_not_sure_get_vaccine_child_mc_politics,
#                 why_not_sure_get_vaccine_child_mc_already_had_covid,
#                 why_not_sure_get_vaccine_child_mc_never_get_vaccine,
#                 why_not_sure_get_vaccine_child_mc_health_history,
#                 why_not_sure_get_vaccine_child_mc_worried_get_covid,
#                 why_not_sure_get_vaccine_child_mc_not_at_risk,
#                 why_not_sure_get_vaccine_child_mc_risk_higher_infection,
#                 why_not_sure_get_vaccine_child_mc_afraid_needles,
#                 why_not_sure_get_vaccine_child_mc_contr_religious_belief,
#                 why_not_sure_get_vaccine_child_mc_wait_herd_immunity,
#                 why_not_sure_get_vaccine_child_mc_others_before_me,
#                 why_not_sure_get_vaccine_child_mc_already_recvd_flu_vaccine,
#                 why_not_planning_get_vaccine_child_mc_other_text,
#                 why_not_sure_get_vaccine_child_mc_other_text,
#                 how_long_covid19_pandemic_over,
#                 which_covid19_vaccine,
#                 vaccine_side_effect_within_24hr_mc_covid_arm,
#                 vaccine_side_effect_within_24hr_mc_nausea_vomiting,
#                 vaccine_side_effect_within_24hr_mc_muscle_ache,
#                 vaccine_side_effect_within_24hr_mc_something_else,
#                 vaccine_side_effect_within_24hr_mc_pain_in_arm,
#                 vaccine_side_effect_within_24hr_mc_swelling_in_arm,
#                 vaccine_side_effect_within_24hr_mc_fever,
#                 vaccine_side_effect_within_24hr_mc_chills,
#                 vaccine_side_effect_within_24hr_mc_tiredness,
#                 vaccine_side_effect_within_24hr_mc_headache,
#                 vaccine_side_effect_within_24hr_mc_other_text,
#                 industry,
#                 employment_status,
#                 source_insurance,
#                 self_report_health,
#                 condition_mc_asthma,
#                 condition_mc_lung_disease,
#                 condition_mc_heart_disease,
#                 condition_mc_diabetes,
#                 condition_mc_kidney_disease,
#                 condition_mc_cancer_last_year,
#                 condition_mc_immunosuppressive,
#                 reason_not_get_covid19_booster_mc_inconvenient,
#                 reason_not_get_covid19_booster_mc_something_else,
#                 reason_not_get_covid19_booster_mc_too_new,
#                 reason_not_get_covid19_booster_mc_side_effect,
#                 reason_not_get_covid19_booster_mc_threat_exaggerated,
#                 reason_not_get_covid19_booster_mc_distrust_government,
#                 reason_not_get_covid19_booster_mc_distrust_scientist,
#                 reason_not_get_covid19_booster_mc_too_political,
#                 reason_not_get_covid19_booster_mc_got_covid19_and_vaccinated,
#                 reason_not_get_covid19_booster_mc_reaction,
#                 reason_not_get_covid19_booster_mc_not_at_risk,
#                 reason_not_get_covid19_booster_mc_others_before_me,
#                 why_not_planning_get_vaccine_mc_too_new,
#                 why_not_planning_get_vaccine_mc_other,
#                 why_not_planning_get_vaccine_mc_side_effect,
#                 why_not_planning_get_vaccine_mc_threat_exaggerated,
#                 why_not_planning_get_vaccine_mc_lack_trust_government,
#                 why_not_planning_get_vaccine_mc_lack_trust_scientist,
#                 why_not_planning_get_vaccine_mc_politics,
#                 why_not_planning_get_vaccine_mc_already_had_covid,
#                 why_not_planning_get_vaccine_mc_never_get_vaccine,
#                 why_not_planning_get_vaccine_mc_health_history,
#                 why_not_planning_get_vaccine_mc_worried_get_covid,
#                 why_not_planning_get_vaccine_mc_not_at_risk,
#                 why_not_planning_get_vaccine_mc_higher_risk_infection,
#                 why_not_planning_get_vaccine_mc_afraid_needles,
#                 why_not_planning_get_vaccine_mc_contradict_religious_belief,
#                 why_not_planning_get_vaccine_mc_wait_herd_immunity,
#                 why_not_planning_get_vaccine_mc_others_before_me,
#                 covid19_vaccine_not_sure_mc_too_new,
#                 covid19_vaccine_not_sure_mc_something_else,
#                 covid19_vaccine_not_sure_mc_side_effects,
#                 covid19_vaccine_not_sure_mc_threat_exaagerated,
#                 covid19_vaccine_not_sure_mc_no_trust_government,
#                 covid19_vaccine_not_sure_mc_no_trust_science,
#                 covid19_vaccine_not_sure_mc_politics,
#                 covid19_vaccine_not_sure_mc_already_vaccinated,
#                 covid19_vaccine_not_sure_mc_never_vaccinated,
#                 covid19_vaccine_not_sure_mc_health_history,
#                 covid19_vaccine_not_sure_mc_get_covid19,
#                 covid19_vaccine_not_sure_mc_not_at_risk,
#                 covid19_vaccine_not_sure_mc_high_risk,
#                 covid19_vaccine_not_sure_mc_needles,
#                 covid19_vaccine_not_sure_mc_religious_beliefs,
#                 covid19_vaccine_not_sure_mc_wait_herd_immunity,
#                 covid19_vaccine_not_sure_mc_others_should_get_first,
#                 covid19_vaccine_not_sure_mc_other_text,
#                 where_willing_get_vaccine_mc_pharmacy,
#                 where_willing_get_vaccine_mc_other,
#                 where_willing_get_vaccine_mc_primary_health_care_sites,
#                 where_willing_get_vaccine_mc_health_centers,
#                 where_willing_get_vaccine_mc_hospital,
#                 where_willing_get_vaccine_mc_mass_vaccine_sites,
#                 where_willing_get_vaccine_mc_mobile_vaccine_sites,
#                 where_willing_get_vaccine_mc_retail_sites,
#                 where_willing_get_vaccine_mc_place_of_worship,
#                 )
# 
# 
# 
# weights1<-fread("CovidNearYou Weight 08 May 10PM.csv")
# 
# df3<-merge(df2,weights1,by="response_id", all.x = TRUE, all.y = FALSE)  %>%
#   filter(!(is.na(weight_daily_national_13plus)))
# 
# 
# df4 <- df3
# 
# df4$party<- factor(df4$party_id, levels = c(1,2,3), labels = c("Republican",
#                                                                "Democrat",
#                                                                "Independent"))
# 
# 
# df4$edu<- factor(df4$educ4, levels = c(1,2,3,4,5), labels = c("High School or Less",
#                                                               "Some College",
#                                                               "College or More", 
#                                                               "Post Graduate Degree","Did Not Respond"))
# 
# df4$race_recode <- ifelse(df4$race_recode==6,2,df4$race_recode)
# 
# df4$race<- factor(df4$race_recode, levels = c(1,2,3,4,5,7,8,9,10), labels = c("White, not Hispanic","Single Other Race","Hispanic or Latino/a",
#                                                                               "Black or African American", "Asian",
#                                                                               "Native Hawaiian or Other Pacific Islander", "American Indian or Alaska Native", "Did Not Respond", "Multi-Racial"))
# 
# df4$race <- factor(df4$race, levels = c("American Indian or Alaska Native","Asian","Black or African American","Hispanic or Latino/a",
#                                         "Native Hawaiian or Other Pacific Islander","White, not Hispanic","Multi-Racial","Single Other Race","Did Not Respond"))
# 
# df4$is_essential_worker <- ifelse(is.na(df4$is_essential_worker),3,df4$is_essential_worker)
# 
# df4$essential_worker<- factor(df4$is_essential_worker, levels = c(1,2,3), labels = c("Yes",
#                                                                                      "No",
#                                                                                      "Did Not Respond"))
# 
# 
# df4$household_income<- factor(df4$income, levels = c(1,2,3,4,5,6,7,8), labels = c("Under $15,000",
#                                                                                   "Between $15,000 and $29,999",
#                                                                                   "Between $30,000 and $49,999",
#                                                                                   "Between $50,000 and $74,999",
#                                                                                   "Between $75,000 and $99,999",
#                                                                                   "Between $100,000 and $150,000",
#                                                                                   "Over $150,000","Did Not Respond"))
# 
# 
# 
# 
# df4$booster <- ifelse(df4$covid19_booster_likely_to_get==4,1,
#                       ifelse(df4$covid19_booster_likely_to_get<=3,0,"Did Not Respond"))
# 
# df4$booster <- ifelse(is.na(df4$booster),0,df4$booster)
# 
# df4$vaccination_status <- ifelse(df4$get_vaccine_yesno == 2, "Unvaccinated",
#                                  ifelse(df4$get_vaccine_yesno == 3, "Did Not Respond",
#                                         ifelse(df4$received_all_covid19_required_doses == 2, "Partially Vaccinated",
#                                                ifelse(df4$received_all_covid19_required_doses == 1 & df4$booster == 1, "Fully Vaccinated and Boosted",
#                                                       ifelse(df4$received_all_covid19_required_doses == 1,"Fully Vaccinated","Did Not Respond")))))
# 
# df4$vaccination_status <- factor(df4$vaccination_status, levels=c("Unvaccinated",
#                                                                   "Partially Vaccinated",
#                                                                   "Fully Vaccinated",
#                                                                   "Fully Vaccinated and Boosted","Did Not Respond"))
# 
# df4$age_group <- cut(df4$age, breaks = c(17.99,29,39,49,64,74,Inf))
# 
# df4$age_group <- fct_explicit_na(df4$age_group, "Did Not Respond")
# 
# levels(df4$age_group) <-c("18-29 years",
#                           "30-39 years",
#                           "40-49 years",
#                           "50-64 years",
#                           "65-74 years",
#                           "75+ years")
# 
# 
# df4$gender <- factor(df4$gender, levels = c(1,2,3,4), labels = c("Male","Female","Transgender or Nonbinary","No answer"))
# 
# df4$gender <- factor(df4$gender, levels = c("Female","Male","Transgender or Nonbinary","No answer"))
# 
# df4$race <- relevel(df4$race, ref = "White, not Hispanic")
# 
# 
# df4 <- df4 %>% mutate(pandemic_over = case_when(how_long_covid19_pandemic_over == 1 ~ "Already Over",
#                                                 how_long_covid19_pandemic_over == 2 ~ "Less than one month",
#                                                 how_long_covid19_pandemic_over == 3 ~ "Less than three months",
#                                                 how_long_covid19_pandemic_over == 4 ~ "Less than six months",
#                                                 how_long_covid19_pandemic_over == 5 ~ "Less than one year",
#                                                 how_long_covid19_pandemic_over == 6 ~ "More than one year",
#                                                 TRUE ~ "No Answer"
# ))
# df4$pandemic_over <- factor(df4$pandemic_over, levels=c("Already Over", "Less than one month","Less than three months","Less than six months","Less than one year","More than one year","No Answer"))
# 
# 
# df4 <- df4 %>% mutate(which_covid19_vaccine = case_when(which_covid19_vaccine == 1 ~ "Moderna",
#                                                         which_covid19_vaccine == 2 ~ "Pfizer",
#                                                         which_covid19_vaccine == 3 ~ "Johnson",
#                                                         which_covid19_vaccine == 4 ~ "don't know",
#                                                         which_covid19_vaccine == 5 ~ "No answer",
#                                                 TRUE ~ "No answer"
# ))
# df4$which_covid19_vaccine <- factor(df4$which_covid19_vaccine, levels=c("Moderna","Pfizer","Johnson","dont know","No answer"))
# 
# 
# 
# df4 <- df4 %>% rename(rash = vaccine_side_effect_within_24hr_mc_covid_arm) %>%  rename(nausea_vomiting = vaccine_side_effect_within_24hr_mc_nausea_vomiting) %>% 
#   rename(muscle_ache = vaccine_side_effect_within_24hr_mc_muscle_ache) %>% rename(something_else = vaccine_side_effect_within_24hr_mc_something_else) %>% 
#   rename(pain_in_arm = vaccine_side_effect_within_24hr_mc_pain_in_arm) %>% rename(swelling_in_arm = vaccine_side_effect_within_24hr_mc_swelling_in_arm) %>% 
#   rename(fever = vaccine_side_effect_within_24hr_mc_fever) %>% rename(chills = vaccine_side_effect_within_24hr_mc_chills) %>% 
#   rename(tiredness = vaccine_side_effect_within_24hr_mc_tiredness) %>% rename(headache = vaccine_side_effect_within_24hr_mc_headache)
# 
# 
# df4$rash <- factor(df4$rash)
# df4$nausea_vomiting <- factor(df4$nausea_vomiting)
# df4$muscle_ache <- factor(df4$muscle_ache)
# df4$something_else <- factor(df4$something_else)
# df4$pain_in_arm <- factor(df4$pain_in_arm)
# df4$swelling_in_arm <- factor(df4$swelling_in_arm)
# df4$fever <- factor(df4$fever)
# df4$chills <- factor(df4$chills)
# df4$tiredness <- factor(df4$tiredness)
# df4$headache <- factor(df4$headache)
# 
# df4 <- df4 %>% mutate(local_adverse_effect = case_when(rash == 1 | pain_in_arm == 1 | swelling_in_arm == 1 ~ "yes",
#                                            TRUE ~ "no"
# )) 
# 
# df4 <- df4 %>% mutate(systemic_adverse_effect = case_when(nausea_vomiting == 1 | muscle_ache == 1 | fever == 1 | chills == 1 | tiredness == 1 | headache == 1 ~ "yes",
#                                                        TRUE ~ "no"
# )) 
# 
# df4 <- df4 %>% mutate(no_side_effect = case_when(local_adverse_effect == 0 | systemic_adverse_effect == 0 | something_else==0 ~ 1,
#                                                           TRUE ~ 1
# )) 
# 
# df4$local_adverse_effect <- factor(df4$local_adverse_effect)
# df4$local_adverse_effect <- relevel(df4$local_adverse_effect, ref = "yes")
# df4$systemic_adverse_effect <- factor(df4$systemic_adverse_effect)
# df4$systemic_adverse_effect <- relevel(df4$systemic_adverse_effect, ref = "yes")
# 
# 
# 
# df4 <- df4 %>% mutate(religous = case_when(religion == 9 | religion == 10 ~ "Atheist/Agnostic",
#                                            religion == 1 | religion == 2 | religion == 3 | religion == 4 | religion == 5 | religion == 6 | religion == 7 | religion == 8 ~ "Religous",
#                                                     religion == 12 ~ "no answer",
#                                                     TRUE ~ "no answer"
#                                                     )) 
# 
# df4$religous <- factor(df4$religous, levels=c("Atheist/Agnostic", "no answer", "Religous"))
# df4$religous <- relevel(df4$religous, ref = "Religous")
# 
# 
# 
# 
# df4 <- df4 %>% mutate(flu_vaccine_since_jun_2021 = case_when(flu_vaccine_since_june_2021 == 1 ~ "yes",
#                                               flu_vaccine_since_june_2021 == 2 ~ "no",
#                                               flu_vaccine_since_june_2021 == 3~ "no answer",
#                                               TRUE ~ "no answer"))
# 
# df4$flu_vaccine_since_jun_2021 <- factor(df4$flu_vaccine_since_jun_2021)
# 
# 
# df4 <- df4 %>% mutate(self_report_health = case_when(self_report_health == 1 ~ "Excellent",
#                                                      self_report_health == 2 ~ "Very good",
#                                                      self_report_health == 3~ "Good",
#                                                      self_report_health == 4~ "Fair",
#                                                      self_report_health == 5~ "Poor",
#                                                      self_report_health == 6~ "No answer",
#                                                              TRUE ~ "No answer"))
# df4$self_report_health <- factor(df4$self_report_health)
# 
# df4 <- df4 %>% mutate(employment_status = case_when(employment_status == 1 ~ "Full Time",
#                                                     employment_status == 2 ~ "Part Time",
#                                                     employment_status == 3~ "Student",
#                                                     employment_status == 4~ "Retired",
#                                                     employment_status == 5~ "Looking for work",
#                                                     employment_status == 6~ "Not looking for work",
#                                                     employment_status == 7~ "No answer",
#                                                      TRUE ~ "No answer"))
# df4$employment_status <- factor(df4$employment_status)
# 
# 
# df4 <- df4 %>% mutate(source_insurance = case_when(source_insurance == 1 ~ "Plan through spouse or parents employer",
#                                                    source_insurance == 2 ~ "Other",
#                                                    source_insurance == 3~ "Self-purchased",
#                                                    source_insurance == 4~ "Medicare",
#                                                    source_insurance == 5~ "Medicaid",
#                                                    source_insurance == 6~ "TRICARE",
#                                                    source_insurance == 7~ "Uninsured",
#                                                    source_insurance == 8~ "No answer",
#                                                     TRUE ~ "No answer"))
# df4$source_insurance <- factor(df4$source_insurance)
# 
# 
# 
# df4 <- rename(df4, Gender = gender)
# df4 <- rename(df4, 'Age.Group' = age_group)
# df4 <- rename(df4, 'Household.Income' = household_income)
# df4 <- rename(df4, Race = race)
# df4 <- rename(df4, 'Education' = edu)
# df4 <- rename(df4, 'Party' = party)
# df4 <- rename(df4, 'Religous' = religous)
# df4 <- rename(df4, 'Pandemic.Over' = pandemic_over)
# df4 <- rename(df4, 'COVID19.Vaccine' = which_covid19_vaccine)
# df4 <- rename(df4, 'Flu.Vaccine.Since.June.2021' = flu_vaccine_since_jun_2021)
# df4 <- rename(df4, 'Self.Reported.Health' = self_report_health)
# df4 <- rename(df4, 'Employment.Status' = employment_status)
# df4 <- rename(df4, 'Insurance' = source_insurance)
# 
# 
# 
# df5 <- df4
# 
# levels(df5$`Age.Group`) <- c("18-29 years", "30-39 years", "40-49 years", "50-64 years", "65+ years", "65+ years")
# levels(df5$`Household.Income`) <- c("Under $30,000", "Under $30,000", "Between $30,000 and $49,999", "Between $50,000 and $74,999", "Between $75,000 and $99,999", "Over $100,000", "Over $100,000", "Did Not Respond")
# levels(df5$`Insurance`) <- c("Insured", "Insured", "No answer", "Insured", "Insured", "Insured", "Insured", "Uninsured")
# levels(df5$`Employment.Status`) <- c("Employed", "Unemployed", "No answer", "Unemployed", "Employed", "Unemployed", "Unemployed")
# levels(df5$`Self.Reported.Health`) <- c("Excellent", "Fair/Poor", "Good", "No answer", "Fair/Poor", "Very good")
# df5$`Self.Reported.Health` <- factor(df5$`Self.Reported.Health`, levels=c('Fair/Poor', 'Good', 'Very good', 'Excellent'))
# 
# levels(df5$Race) <- c("White, not Hispanic", "Other", "Asian", "Black or African American", "Hispanic or Latino/a", "Other", "Other", "Other", "Did Not Respond")
# levels(df5$Education) <- c("High School or Less", "Some College", "College Graduate", "College Graduate", "Did Not Respond")
# levels(df5$`Household.Income`)  <- c("Under $49,999", "Under $49,999", "$50,000 and $99,999", "$50,000 and $99,999", "Over $100,000", "Did Not Respond")
# levels(df5$`Pandemic.Over`)  <- c("Already Over", "Less than three months", "Less than three months", "Three months to one year", "Three months to one year", "More than one year", "No Answer" )
# 
# 
# 
# df5 <- df5 %>% mutate(parent_vaccination = case_when(vaccination_status == "Partially Vaccinated" ~ "Vaccinated",
#                                                      vaccination_status == "Fully Vaccinated" ~ "Vaccinated",
#                                                      vaccination_status == "Fully Vaccinated and Boosted" ~ "Vaccinated",
#                                                      vaccination_status == "Unvaccinated" ~ "Unvaccinated",
#                                                      vaccination_status == "Did Not Respond" ~ "No answer",
#                                                      TRUE ~ "No answer"))
# df5$parent_vaccination <- factor(df5$parent_vaccination)
# df5$parent_vaccination <- droplevels(df5$parent_vaccination)
# 
# df5 <- df5 %>% mutate(mRNA_Johnson = case_when(COVID19.Vaccine == "Moderna" ~ "mRNA",
#                       COVID19.Vaccine == "Pfizer" ~ "mRNA",
#                       COVID19.Vaccine == "Johnson" ~ "Johnson",
#                       COVID19.Vaccine == "don't know" ~ "don't know",
#                       COVID19.Vaccine == "No answer" ~ "No answer",
#                       TRUE ~ "No answer"))
# df5 <- df5 %>% mutate(mRNA_Johnson = case_when(mRNA_Johnson == "No answer" & parent_vaccination == "Vaccinated" ~ "Vaccinated, unknown vaccine",
#                                                mRNA_Johnson == "No answer" & parent_vaccination == "Unvaccinated" ~ "Unvaccinated",
#                                                mRNA_Johnson == "Johnson" ~ "Johnson",
#                                                mRNA_Johnson == "mRNA" ~ "mRNA",
#                                                TRUE ~ "No answer"))
# 
# 
# df5$mRNA_Johnson <- factor(df5$mRNA_Johnson)
# 
# df5 <- df5 %>% filter(mRNA_Johnson != "Vaccinated, unknown vaccine")
# 
# 
# df5<-df5[complete.cases(df5[,
#                        c("have_children_mc_5_to_11", "have_children_mc_12_to_15", "have_children_mc_16_to_17", 
#                          "willing_children_get_covid19_vaccine_mx_5_to_11", 
#                          "willing_children_get_covid19_vaccine_mx_12_to_15", "willing_children_get_covid19_vaccine_mx_16_to_17")])]
# 
# 
# df5 <- df5 %>% mutate(always_unwilling = ifelse(
#   ( (have_children_mc_5_to_11 == 1 & willing_children_get_covid19_vaccine_mx_5_to_11 == 2) | (have_children_mc_5_to_11 == 0) ) &
#     ( (have_children_mc_12_to_15 == 1 & willing_children_get_covid19_vaccine_mx_12_to_15 == 2) | (have_children_mc_12_to_15 == 0) ) &
#     ( (have_children_mc_16_to_17 == 1 & willing_children_get_covid19_vaccine_mx_16_to_17 == 2) | (have_children_mc_16_to_17 == 0) ),1,0))
# 
# df5 <- df5 %>% mutate(always_willing = ifelse(
#   ( (have_children_mc_5_to_11 == 1 & willing_children_get_covid19_vaccine_mx_5_to_11 == 1) | (have_children_mc_5_to_11 == 0) ) &
#     ( (have_children_mc_12_to_15 == 1 & willing_children_get_covid19_vaccine_mx_12_to_15 == 1) | (have_children_mc_12_to_15 == 0) ) &
#     ( (have_children_mc_16_to_17 == 1 & willing_children_get_covid19_vaccine_mx_16_to_17 == 1) | (have_children_mc_16_to_17 == 0) ) ,
#   1,0))
# 
# df5 <- df5 %>% mutate(always_unsure = ifelse(
#   ( (have_children_mc_5_to_11 == 1 & willing_children_get_covid19_vaccine_mx_5_to_11 == 3) | (have_children_mc_5_to_11 == 0) ) &
#     ( (have_children_mc_12_to_15 == 1 & willing_children_get_covid19_vaccine_mx_12_to_15 == 3) | (have_children_mc_12_to_15 == 0) ) &
#     ( (have_children_mc_16_to_17 == 1 & willing_children_get_covid19_vaccine_mx_16_to_17 == 3) | (have_children_mc_16_to_17 == 0) ) ,
#   1,0))
# 
# 
# 
# df5 <- df5 %>% mutate(unsure_at_least_once = ifelse(
#   ( willing_children_get_covid19_vaccine_mx_5_to_11 == 3) |
#     ( willing_children_get_covid19_vaccine_mx_12_to_15 == 3) |
#     ( willing_children_get_covid19_vaccine_mx_16_to_17 == 3) ,
#   1,0))
# 
# df5 <- df5 %>% mutate(unwilling_at_least_once = ifelse(
#   ( willing_children_get_covid19_vaccine_mx_5_to_11 == 2) |
#     ( willing_children_get_covid19_vaccine_mx_12_to_15 == 2) |
#     ( willing_children_get_covid19_vaccine_mx_16_to_17 == 2) ,
#   1,0))
# 
# 
# ##################################################
# ##################################################
# ##################################################
# ##################################################
# Summary Statistics
# ##################################################
# ##################################################
# ##################################################
# ##################################################
# 
# 
# df5 <- df5 %>% filter(parent_vaccination != "No answer") %>% filter(Household.Income != "Did Not Respond") %>% 
#   filter(Religous != 'no answer') %>% filter(Pandemic.Over != 'No Answer') %>% filter(Flu.Vaccine.Since.June.2021 != 'no answer') %>% 
#   filter(Self.Reported.Health != 'No answer') %>% filter(Employment.Status != 'No answer') %>% filter(Insurance != 'No answer')
# 
# df5$Gender <- droplevels(df5$Gender)
# df5$Household.Income <- droplevels(df5$Household.Income)
# df5$Race <- droplevels(df5$Race)
# df5$Education <- droplevels(df5$Education)
# df5$Religous <- droplevels(df5$Religous)
# df5$Pandemic.Over <- droplevels(df5$Pandemic.Over)
# df5$Flu.Vaccine.Since.June.2021 <- droplevels(df5$Flu.Vaccine.Since.June.2021)
# df5$Self.Reported.Health <- droplevels(df5$Self.Reported.Health)
# df5$Employment.Status <- droplevels(df5$Employment.Status)
# df5$Insurance <- droplevels(df5$Insurance)
# # df5$child_vaccinated <- droplevels(df5$child_vaccinated)
# df5$parent_vaccination <- droplevels(df5$parent_vaccination)
# df5$mRNA_Johnson <- droplevels(df5$mRNA_Johnson)
# 
# df5$Insurance <- relevel(df5$Insurance, ref = "Insured")
# df5$Flu.Vaccine.Since.June.2021 <- relevel(df5$Flu.Vaccine.Since.June.2021, ref = "no")
# 
# 
# 
# 
# 
# df5 <- df5[complete.cases(df5[,c("Gender", "Age.Group", "Household.Income", "Race", "Education", "Party", "Religous", 
#                                  "Pandemic.Over", "Flu.Vaccine.Since.June.2021", "Self.Reported.Health", 
#                                  "Employment.Status", "Insurance")])] # Remember already dropped some people with no weights at the beginning (df3)
# 
# 
# 
# 
# df5 <- df5 %>%
#   group_by(date) %>%
#   dplyr::mutate(weight_daily_national_18plus = weight_daily_national_13plus * (n()/sum(weight_daily_national_13plus)))
# 
# df5$fpc1<-63000000
# 
# df_always_unsure <- df5 %>% filter(always_unsure==1)
# df_mix <- df5 %>% filter(always_willing==0 & always_unwilling==0 & always_unsure==0)
# 
# df_always_unsure_survey <- survey::svydesign(ids = ~1, weights = df_always_unsure$weight_daily_national_18plus, fpc=df_always_unsure$fpc1, data=df_always_unsure)
# df_mix_survey <- survey::svydesign(ids = ~1, weights = df_mix$weight_daily_national_18plus, fpc=df_mix$fpc1, data=df_mix)
# 
# # drop those with mixed preferences
# df5 <- df5 %>% filter(always_willing==1 | always_unwilling==1)
# 
# df5 %>% write.csv('data.csv')
# ## NOTE, data.csv file is saved here

df5 <- fread('data.csv')


df5 <- df5 %>% mutate(child_vaccinated = ifelse(always_willing==1, 1, 0))
df5$child_vaccinated <- factor(df5$child_vaccinated, labels=c('unvaccinated', 'vaccinated'))



df_survey <- survey::svydesign(ids = ~1, weights = df5$weight_daily_national_18plus, fpc=df5$fpc1, data=df5)


table1_unweighted <- tbl_summary(
  df5,
  by = NULL,
  label = list(have_children_mc_5_to_11 ~ 'Have Child Age 5 to 11 Years',
               have_children_mc_12_to_15 ~ 'Have Child Age 12 to 15 Years',
               have_children_mc_16_to_17 ~ 'Have Child Age 16 to 17 Years',
               mRNA_Johnson ~ 'Parent COVID-19 Vaccination',
               Age.Group ~ 'Age Group',
               Household.Income ~ 'Household Income',
               Pandemic.Over ~ 'When Will the Pandemic End?',
               Flu.Vaccine.Since.June.2021 ~ 'Flu Vaccine Since June 2021',
               Self.Reported.Health	 ~ 'Self Reported Health',
               Employment.Status ~ 'Employment Status	',
               Race ~ 'Race/Ethnicity',
               Insurance ~ 'Health Insurance',
               Party ~ 'Political Party Affiliation'),
  statistic = list(all_categorical() ~ "{n} ({p}%)"),
  digits = everything() ~ 0,
  type = NULL,
  value = NULL,
  missing = "no",
  missing_text = NULL,
  sort = NULL,
  percent = "col",
  include = c(Gender, Age.Group, Household.Income, Race, Education, Employment.Status, Insurance, Self.Reported.Health, Religous,
              mRNA_Johnson,
              have_children_mc_5_to_11,
              have_children_mc_12_to_15,
              have_children_mc_16_to_17, 
              Party, Pandemic.Over, Flu.Vaccine.Since.June.2021, vaccination_status)) %>% 
  modify_caption("Table 1. Characteristics of Parents") %>% modify_header(label ~ "", stat_0 ~ '**n (%)**') %>% modify_footnote(all_stat_cols() ~ NA)

table1 <- tbl_svysummary(
  df_survey,
  by = NULL,
  label = list(have_children_mc_5_to_11 ~ 'Have Child Age 5 to 11 Years',
               have_children_mc_12_to_15 ~ 'Have Child Age 12 to 15 Years',
               have_children_mc_16_to_17 ~ 'Have Child Age 16 to 17 Years',
               mRNA_Johnson ~ 'Parent COVID-19 Vaccination',
               Age.Group ~ 'Age Group',
               Household.Income ~ 'Household Income',
               Pandemic.Over ~ 'When Will the Pandemic End?',
               Flu.Vaccine.Since.June.2021 ~ 'Flu Vaccine Since June 2021',
               Self.Reported.Health	 ~ 'Self Reported Health',
               Employment.Status ~ 'Employment Status	',
               Race ~ 'Race/Ethnicity',
               Insurance ~ 'Health Insurance',
               Party ~ 'Political Party Affiliation'),
  statistic = list(all_categorical() ~ "{n} ({p}%)"),
  digits = everything() ~ 0,
  type = NULL,
  value = NULL,
  missing = "no",
  missing_text = NULL,
  sort = NULL,
  percent = "col",
  include = c(Gender, Age.Group, Household.Income, Race, Education, Employment.Status, Insurance, Self.Reported.Health, Religous,
              mRNA_Johnson,
              have_children_mc_5_to_11,
              have_children_mc_12_to_15,
              have_children_mc_16_to_17, 
              Party, Pandemic.Over, Flu.Vaccine.Since.June.2021, vaccination_status)) %>% 
  modify_caption("Table 1. Weighted Characteristics of 31,074 Parents") %>% modify_header(label ~ "", stat_0 ~ '**n (%)**') %>% modify_footnote(all_stat_cols() ~ NA)


merged_tbl1 <- tbl_merge(tbls=list(table1, table1_unweighted), tab_spanner = c("**Weighted (N=29,583)**", "**Unweighted (N=30,140)**"))



table1_always_unsure <- tbl_svysummary(
  df_always_unsure_survey,
  by = NULL,
  label = list(have_children_mc_5_to_11 ~ 'Have Child Age 5 to 11 Years',
               have_children_mc_12_to_15 ~ 'Have Child Age 12 to 15 Years',
               have_children_mc_16_to_17 ~ 'Have Child Age 16 to 17 Years',
               mRNA_Johnson ~ 'Parent COVID-19 Vaccination',
               Age.Group ~ 'Age Group',
               Household.Income ~ 'Household Income',
               Pandemic.Over ~ 'When Will the Pandemic End?',
               Flu.Vaccine.Since.June.2021 ~ 'Flu Vaccine Since June 2021',
               Self.Reported.Health	 ~ 'Self Reported Health',
               Employment.Status ~ 'Employment Status	',
               Race ~ 'Race/Ethnicity',
               Insurance ~ 'Health Insurance',
               Party ~ 'Political Party Affiliation'),
  statistic = list(all_categorical() ~ "{n} ({p}%)"),
  digits = everything() ~ 0,
  type = NULL,
  value = NULL,
  missing = "no",
  missing_text = NULL,
  sort = NULL,
  percent = "col",
  include = c(Gender, Age.Group, Household.Income, Race, Education, Employment.Status, Insurance, Self.Reported.Health, Religous,
              mRNA_Johnson,
              have_children_mc_5_to_11,
              have_children_mc_12_to_15,
              have_children_mc_16_to_17, 
              Party, Pandemic.Over, Flu.Vaccine.Since.June.2021, vaccination_status)) %>% 
  modify_caption("Table 1. Weighted Characteristics of 31,074 Parents") %>% modify_header(label ~ "", stat_0 ~ '**n (%)**') %>% modify_footnote(all_stat_cols() ~ NA)

table1_mix <- tbl_svysummary(
  df_mix_survey,
  by = NULL,
  label = list(have_children_mc_5_to_11 ~ 'Have Child Age 5 to 11 Years',
               have_children_mc_12_to_15 ~ 'Have Child Age 12 to 15 Years',
               have_children_mc_16_to_17 ~ 'Have Child Age 16 to 17 Years',
               mRNA_Johnson ~ 'Parent COVID-19 Vaccination',
               Age.Group ~ 'Age Group',
               Household.Income ~ 'Household Income',
               Pandemic.Over ~ 'When Will the Pandemic End?',
               Flu.Vaccine.Since.June.2021 ~ 'Flu Vaccine Since June 2021',
               Self.Reported.Health	 ~ 'Self Reported Health',
               Employment.Status ~ 'Employment Status	',
               Race ~ 'Race/Ethnicity',
               Insurance ~ 'Health Insurance',
               Party ~ 'Political Party Affiliation'),
  statistic = list(all_categorical() ~ "{n} ({p}%)"),
  digits = everything() ~ 0,
  type = NULL,
  value = NULL,
  missing = "no",
  missing_text = NULL,
  sort = NULL,
  percent = "col",
  include = c(Gender, Age.Group, Household.Income, Race, Education, Employment.Status, Insurance, Self.Reported.Health, Religous,
              mRNA_Johnson,
              have_children_mc_5_to_11,
              have_children_mc_12_to_15,
              have_children_mc_16_to_17, 
              Party, Pandemic.Over, Flu.Vaccine.Since.June.2021, vaccination_status)) %>% 
  modify_caption("Table 1. Weighted Characteristics of 31,074 Parents") %>% modify_header(label ~ "", stat_0 ~ '**n (%)**') %>% modify_footnote(all_stat_cols() ~ NA)

appendix_tbl1 <- tbl_merge(tbls=list(table1, table1_always_unsure, table1_mix), tab_spanner = c("**Always Unsure or Sure (n=29,583)**", "**Always Unsure (n=3,951)**", "**Mixed (n=1,067)**"))


##################################################
##################################################
##################################################
##################################################
# Univariate Regression - Two Separate Regressions
##################################################
##################################################
##################################################
##################################################


add_percentage <- function(tbl){
  t <- tbl %>% 
  add_n(location = "level") %>% 
    add_nevent(location = "level") %>% 
    # adding event rate
    modify_table_body(
      ~ .x %>% 
        dplyr::mutate(
          stat_nevent_rate = 
            ifelse(
              !is.na(stat_nevent),
              paste0(style_sigfig(stat_nevent / stat_n, scale = 100), "%"),
              NA
            ), 
          .after = stat_nevent
        )
    ) %>%
    # merge the colums into a single column
    modify_cols_merge(
      pattern = "{stat_nevent} / {stat_n} ({stat_nevent_rate})",
      rows = !is.na(stat_nevent)
    ) %>% modify_header(stat_nevent = "**n/N  (%)**")
  return(t)
}



levels(df5$rash) <- c('no', 'yes')
df5$rash <- relevel(df5$rash, ref = "yes")
levels(df5$nausea_vomiting) <- c('no', 'yes')
df5$nausea_vomiting <- relevel(df5$nausea_vomiting, ref = "yes")
levels(df5$muscle_ache) <- c('no', 'yes')
df5$muscle_ache <- relevel(df5$muscle_ache, ref = "yes")
levels(df5$something_else) <- c('no', 'yes')
df5$something_else <- relevel(df5$something_else, ref = "yes")
levels(df5$pain_in_arm) <- c('no', 'yes')
df5$pain_in_arm <- relevel(df5$pain_in_arm, ref = "yes")
levels(df5$swelling_in_arm) <- c('no', 'yes')
df5$swelling_in_arm <- relevel(df5$swelling_in_arm, ref = "yes")
levels(df5$fever) <- c('no', 'yes')
df5$fever <- relevel(df5$fever, ref = "yes")
levels(df5$chills) <- c('no', 'yes')
df5$chills <- relevel(df5$chills, ref = "yes")
levels(df5$tiredness) <- c('no', 'yes')
df5$tiredness <- relevel(df5$tiredness, ref = "yes")
levels(df5$headache) <- c('no', 'yes')
df5$headache <- relevel(df5$headache, ref = "yes")

df5$have_children_mc_5_to_11 <- factor(df5$have_children_mc_5_to_11)
levels(df5$have_children_mc_5_to_11) <- c('no', 'yes')
df5$have_children_mc_12_to_15 <- factor(df5$have_children_mc_12_to_15)
levels(df5$have_children_mc_12_to_15) <- c('no', 'yes')
df5$have_children_mc_16_to_17 <- factor(df5$have_children_mc_16_to_17)
levels(df5$have_children_mc_16_to_17) <- c('no', 'yes')


df5$fpc1<-332403650
df_parent_vaccinated <- filter(df5, parent_vaccination == "Vaccinated" & 
                                 child_vaccinated!="No answer" &
                                 vaccination_status!='Partially Vaccinated')
df_parent_unvaccinated <- filter(df5, vaccination_status == "Unvaccinated" &
                                   child_vaccinated!="No answer")
df_parent_vaccinated$child_vaccinated <- droplevels(df_parent_vaccinated$child_vaccinated)
df_parent_unvaccinated$child_vaccinated <- droplevels(df_parent_unvaccinated$child_vaccinated)


df_parent_overall <- filter(df5, child_vaccinated!="No answer")
df_parent_overall$child_vaccinated <- droplevels(df_parent_overall$child_vaccinated)


df_parent_vaccinated_survey <- survey::svydesign(ids = ~1, weights = df_parent_vaccinated$weight_daily_national_18plus, fpc=df_parent_vaccinated$fpc1, data=df_parent_vaccinated)
df_parent_unvaccinated_survey <- survey::svydesign(ids = ~1, weights = df_parent_unvaccinated$weight_daily_national_18plus, fpc=df_parent_unvaccinated$fpc1, data=df_parent_unvaccinated)
df_parent_overall_survey <- survey::svydesign(ids = ~1, weights = df_parent_overall$weight_daily_national_18plus, fpc=df_parent_overall$fpc1, data=df_parent_overall)

df_parent_vaccinated_surv <- df_parent_vaccinated %>% as_survey_design(ids = 1, weights = weight_daily_national_18plus, fpc=fpc1)


parent_vaccinated_overall_tbl <- tbl_svysummary(
  df_parent_vaccinated_survey,
  by = NULL,
  label = NULL,
  statistic = list(all_categorical() ~ "{n}/{N} ({p}%)"),
  digits = everything() ~ 0,
  type = NULL,
  value = NULL,
  missing = NULL,
  missing_text = NULL,
  sort = NULL,
  percent = "col",
  include = c(child_vaccinated)
)  %>% modify_header(stat_0 = "**Child+**") %>% modify_footnote(update = everything() ~ NA)

parent_unvaccinated_overall_tbl <- tbl_svysummary(
  df_parent_unvaccinated_survey,
  by = NULL,
  label = NULL,
  statistic = list(all_categorical() ~ "{n}/{N} ({p}%)"),
  digits = everything() ~ 0,
  type = NULL,
  value = NULL,
  missing = NULL,
  missing_text = NULL,
  sort = NULL,
  percent = "column",
  include = c(child_vaccinated)
)  %>% modify_header(stat_0 = "**Child+**") %>% modify_footnote(update = everything() ~ NA)
# Run Regression for vaccinated parents
vax_five_model = glm(child_vaccinated ~ have_children_mc_5_to_11, family = "binomial",data = df_parent_vaccinated ,weights = weight_daily_national_18plus)
vax_twelve_model = glm(child_vaccinated ~ have_children_mc_12_to_15, family = "binomial",data = df_parent_vaccinated ,weights = weight_daily_national_18plus)
vax_sixteen_model = glm(child_vaccinated ~ have_children_mc_16_to_17, family = "binomial",data = df_parent_vaccinated ,weights = weight_daily_national_18plus)
vax_gender_model = glm(child_vaccinated ~ Gender, family = "binomial",data = df_parent_vaccinated ,weights = weight_daily_national_18plus)
vax_age_model = glm(child_vaccinated~ Age.Group, family = "binomial",data = df_parent_vaccinated,weights = weight_daily_national_18plus)
vax_household_income_model = glm(child_vaccinated~ `Household.Income`, family = "binomial",data = filter(df_parent_vaccinated, `Household.Income`!="Did Not Respond"),weights = weight_daily_national_18plus)
vax_race_model = glm(child_vaccinated~ `Race`, family = "binomial",data = df_parent_vaccinated,weights = weight_daily_national_18plus)
vax_edu_model = glm(child_vaccinated~ `Education`, family = "binomial",data = df_parent_vaccinated,weights = weight_daily_national_18plus)
vax_party_model = glm(child_vaccinated~ `Party`, family = "binomial",data = df_parent_vaccinated,weights = weight_daily_national_18plus)
vax_religous_model = glm(child_vaccinated~ `Religous`, family = "binomial",data = filter(df_parent_vaccinated, `Religous` !="no answer"),weights = weight_daily_national_18plus)
vax_pandemic_over_model = glm(child_vaccinated~ `Pandemic.Over`, family = "binomial",data = filter(df_parent_vaccinated, `Pandemic.Over` !="No Answer"),weights = weight_daily_national_18plus)
# which_covid19_vaccine_model = glm(child_vaccinated~ `COVID19.Vaccine`, family = "binomial",data = filter(df_parent_vaccinated, `COVID19.Vaccine` !="No answer"),weights = weight_daily_national_18plus)
vax_flu_vaccine_since_jun_2021_model = glm(child_vaccinated~ `Flu.Vaccine.Since.June.2021`, family = "binomial",data = filter(df_parent_vaccinated, Flu.Vaccine.Since.June.2021 !="no answer"),weights = weight_daily_national_18plus)
vax_self_report_health_model = glm(child_vaccinated~ `Self.Reported.Health`, family = "binomial",data = filter(df_parent_vaccinated, `Self.Reported.Health` !="No answer"),weights = weight_daily_national_18plus)
vax_employment_status_model = glm(child_vaccinated~ `Employment.Status`, family = "binomial",data = filter(df_parent_vaccinated, `Employment.Status` !="No answer"),weights = weight_daily_national_18plus)
vax_source_insurance_model = glm(child_vaccinated~ `Insurance`, family = "binomial",data = filter(df_parent_vaccinated, `Insurance` !="No answer"),weights = weight_daily_national_18plus)
vax_rash_model = glm(child_vaccinated ~ rash, family = "binomial",data = df_parent_vaccinated,weights = weight_daily_national_18plus)
vax_nausea_vomiting_model = glm(child_vaccinated ~ nausea_vomiting, family = "binomial",data = df_parent_vaccinated,weights = weight_daily_national_18plus)
vax_muscle_ache_model = glm(child_vaccinated ~ muscle_ache, family = "binomial",data = df_parent_vaccinated,weights = weight_daily_national_18plus)
vax_pain_in_arm_model = glm(child_vaccinated ~ pain_in_arm, family = "binomial",data = df_parent_vaccinated,weights = weight_daily_national_18plus)
vax_swelling_in_arm_model = glm(child_vaccinated ~ swelling_in_arm, family = "binomial",data = df_parent_vaccinated,weights = weight_daily_national_18plus)
vax_fever_model = glm(child_vaccinated ~ fever, family = "binomial",data = df_parent_vaccinated,weights = weight_daily_national_18plus)
vax_chills_model = glm(child_vaccinated ~ chills, family = "binomial",data = df_parent_vaccinated,weights = weight_daily_national_18plus)
vax_tiredness_model = glm(child_vaccinated ~ tiredness, family = "binomial",data = df_parent_vaccinated,weights = weight_daily_national_18plus)
vax_headache_model = glm(child_vaccinated ~ headache, family = "binomial",data = df_parent_vaccinated,weights = weight_daily_national_18plus)
vax_something_else_model = glm(child_vaccinated ~ something_else, family = "binomial",data = df_parent_vaccinated,weights = weight_daily_national_18plus)
vax_local_adverse_effect_model = glm(child_vaccinated ~ local_adverse_effect, family = "binomial",data = df_parent_vaccinated,weights = weight_daily_national_18plus)
vax_systemic_adverse_effect_model = glm(child_vaccinated ~ systemic_adverse_effect, family = "binomial",data = df_parent_vaccinated,weights = weight_daily_national_18plus)
vax_which_covid19_vaccine_model = glm(child_vaccinated~ mRNA_Johnson, family = "binomial",data = df_parent_vaccinated,weights = weight_daily_national_18plus)

# Create Individual Tables
vax_five_tbl <- tbl_regression(vax_five_model, exponentiate = TRUE) %>% add_percentage()
vax_twelve_tbl <- tbl_regression(vax_twelve_model, exponentiate = TRUE) %>% add_percentage()
vax_sixteen_tbl <- tbl_regression(vax_sixteen_model, exponentiate = TRUE) %>% add_percentage()
vax_gender_tbl <- tbl_regression(vax_gender_model, exponentiate = TRUE) %>% add_percentage()
vax_age_tbl <- tbl_regression(vax_age_model, exponentiate = TRUE) %>% add_percentage()
vax_household_income_model_tbl <- tbl_regression(vax_household_income_model, exponentiate = TRUE) %>% add_percentage()
vax_race_model_tbl <- tbl_regression(vax_race_model, exponentiate = TRUE) %>% add_percentage()
vax_edu_model_tbl <- tbl_regression(vax_edu_model, exponentiate = TRUE) %>% add_percentage()
vax_party_model_tbl <- tbl_regression(vax_party_model, exponentiate = TRUE) %>% add_percentage()
vax_religous_model_tbl <- tbl_regression(vax_religous_model, exponentiate = TRUE) %>% add_percentage()
vax_pandemic_over_model_tbl <- tbl_regression(vax_pandemic_over_model, exponentiate = TRUE) %>% add_percentage()
vax_which_covid19_vaccine_model_tbl <- tbl_regression(vax_which_covid19_vaccine_model, exponentiate = TRUE) %>% add_percentage()
vax_flu_vaccine_since_jun_2021_model_tbl <- tbl_regression(vax_flu_vaccine_since_jun_2021_model, exponentiate = TRUE) %>% add_percentage()
vax_self_report_health_model_tbl <- tbl_regression(vax_self_report_health_model, exponentiate = TRUE) %>% add_percentage()
vax_employment_status_model_tbl <- tbl_regression(vax_employment_status_model, exponentiate = TRUE) %>% add_percentage()
vax_source_insurance_model_tbl <- tbl_regression(vax_source_insurance_model, exponentiate = TRUE) %>% add_percentage()
vax_rash_model_tbl <- tbl_regression(vax_rash_model, exponentiate = TRUE) %>% add_percentage() #%>%   remove_row_type(type = "reference")
vax_nausea_vomiting_model_tbl <- tbl_regression(vax_nausea_vomiting_model, exponentiate = TRUE) %>% add_percentage() #%>%   remove_row_type(type = "reference")
vax_muscle_ache_model_tbl <- tbl_regression(vax_muscle_ache_model, exponentiate = TRUE) %>% add_percentage() #%>%   remove_row_type(type = "reference")
vax_pain_in_arm_model_tbl <- tbl_regression(vax_pain_in_arm_model, exponentiate = TRUE) %>% add_percentage() #%>%   remove_row_type(type = "reference")
vax_swelling_in_arm_model_tbl <- tbl_regression(vax_swelling_in_arm_model, exponentiate = TRUE) %>% add_percentage() #%>%   remove_row_type(type = "reference")
vax_fever_model_tbl <- tbl_regression(vax_fever_model, exponentiate = TRUE) %>% add_percentage() #%>%   remove_row_type(type = "reference")
vax_chills_model_tbl <- tbl_regression(vax_chills_model, exponentiate = TRUE) %>% add_percentage() #%>%   remove_row_type(type = "reference")
vax_tiredness_model_tbl <- tbl_regression(vax_tiredness_model, exponentiate = TRUE) %>% add_percentage() #%>%   remove_row_type(type = "reference")
vax_headache_model_tbl <- tbl_regression(vax_headache_model, exponentiate = TRUE) %>% add_percentage() #%>%   remove_row_type(type = "reference")
vax_something_else_model_tbl <- tbl_regression(vax_something_else_model, exponentiate = TRUE) %>% add_percentage() #%>%   remove_row_type(type = "reference")
vax_local_model_tbl <- tbl_regression(vax_local_adverse_effect_model, exponentiate = TRUE) %>% add_percentage()
vax_systemic_model_tbl <- tbl_regression(vax_systemic_adverse_effect_model, exponentiate = TRUE) %>% add_percentage()


# Combine into one
parent_vaccinated_univariate_table <- tbl_stack(tbls=list(#parent_vaccinated_overall_tbl,
                                                          vax_five_tbl,
                                                          vax_twelve_tbl,
                                                          vax_sixteen_tbl,
                                                          vax_gender_tbl, 
                                                          vax_age_tbl,
                                                          vax_household_income_model_tbl,
                                                          vax_race_model_tbl,
                                                          vax_edu_model_tbl,
                                                          vax_party_model_tbl,
                                                          vax_religous_model_tbl,
                                                          vax_pandemic_over_model_tbl,
                                                          vax_flu_vaccine_since_jun_2021_model_tbl,
                                                          vax_self_report_health_model_tbl,
                                                          vax_employment_status_model_tbl,
                                                          vax_source_insurance_model_tbl,
                                                          vax_which_covid19_vaccine_model_tbl,
                                                          vax_rash_model_tbl,
                                                          vax_nausea_vomiting_model_tbl,
                                                          vax_muscle_ache_model_tbl,
                                                          vax_pain_in_arm_model_tbl,
                                                          vax_swelling_in_arm_model_tbl,
                                                          vax_fever_model_tbl,
                                                          vax_chills_model_tbl,
                                                          vax_tiredness_model_tbl,
                                                          vax_headache_model_tbl,
                                                          vax_something_else_model_tbl,
                                                          vax_local_model_tbl,
                                                          vax_systemic_model_tbl
                                                          )) %>% modify_caption("**Parent Vaccinated**")





# Run Regression for unvaccinated parents
unvax_five_model = glm(child_vaccinated ~ have_children_mc_5_to_11, family = "binomial",data = df_parent_unvaccinated ,weights = weight_daily_national_18plus)
unvax_twelve_model = glm(child_vaccinated ~ have_children_mc_12_to_15, family = "binomial",data = df_parent_unvaccinated ,weights = weight_daily_national_18plus)
unvax_sixteen_model = glm(child_vaccinated ~ have_children_mc_16_to_17, family = "binomial",data = df_parent_unvaccinated ,weights = weight_daily_national_18plus)
unvax_gender_model = glm(child_vaccinated ~ Gender, family = "binomial",data = df_parent_unvaccinated ,weights = weight_daily_national_18plus)
unvax_age_model = glm(child_vaccinated~ `Age.Group`, family = "binomial",data = df_parent_unvaccinated,weights = weight_daily_national_18plus)
unvax_household_income_model = glm(child_vaccinated~ `Household.Income`, family = "binomial",data = filter(df_parent_unvaccinated, `Household.Income`!="Did Not Respond"),weights = weight_daily_national_18plus)
unvax_race_model = glm(child_vaccinated~ `Race`, family = "binomial",data = df_parent_unvaccinated,weights = weight_daily_national_18plus)
unvax_edu_model = glm(child_vaccinated~ `Education`, family = "binomial",data = df_parent_unvaccinated,weights = weight_daily_national_18plus)
unvax_party_model = glm(child_vaccinated~ `Party`, family = "binomial",data = df_parent_unvaccinated,weights = weight_daily_national_18plus)
unvax_religous_model = glm(child_vaccinated~ `Religous`, family = "binomial",data = filter(df_parent_unvaccinated, `Religous` !="no answer"),weights = weight_daily_national_18plus)
unvax_pandemic_over_model = glm(child_vaccinated~ `Pandemic.Over`, family = "binomial",data = filter(df_parent_unvaccinated, `Pandemic.Over` !="No Answer"),weights = weight_daily_national_18plus)
unvax_flu_vaccine_since_jun_2021_model = glm(child_vaccinated~ `Flu.Vaccine.Since.June.2021`, family = "binomial",data = filter(df_parent_unvaccinated, `Flu.Vaccine.Since.June.2021` !="no answer"),weights = weight_daily_national_18plus)
unvax_self_report_health_model = glm(child_vaccinated~ `Self.Reported.Health`, family = "binomial",data = filter(df_parent_unvaccinated, `Self.Reported.Health` !="No answer"),weights = weight_daily_national_18plus)
unvax_employment_status_model = glm(child_vaccinated~ `Employment.Status`, family = "binomial",data = filter(df_parent_unvaccinated, `Employment.Status` !="No answer"),weights = weight_daily_national_18plus)
unvax_source_insurance_model = glm(child_vaccinated~ `Insurance`, family = "binomial",data = filter(df_parent_unvaccinated, `Insurance` !="No answer"),weights = weight_daily_national_18plus)

# Create Individual Tables
unvax_five_tbl <- tbl_regression(unvax_five_model, exponentiate = TRUE) %>% add_percentage()
unvax_twelve_tbl <- tbl_regression(unvax_twelve_model, exponentiate = TRUE) %>% add_percentage()
unvax_sixteen_tbl <- tbl_regression(unvax_sixteen_model, exponentiate = TRUE) %>% add_percentage()
unvax_gender_tbl <- tbl_regression(unvax_gender_model, exponentiate = TRUE) %>% add_percentage()
unvax_age_tbl <- tbl_regression(unvax_age_model, exponentiate = TRUE) %>% add_percentage()
unvax_household_income_model_tbl <- tbl_regression(unvax_household_income_model, exponentiate = TRUE) %>% add_percentage()
unvax_race_model_tbl <- tbl_regression(unvax_race_model, exponentiate = TRUE) %>% add_percentage()
unvax_edu_model_tbl <- tbl_regression(unvax_edu_model, exponentiate = TRUE) %>% add_percentage()
unvax_party_model_tbl <- tbl_regression(unvax_party_model, exponentiate = TRUE) %>% add_percentage()
unvax_religous_model_tbl <- tbl_regression(unvax_religous_model, exponentiate = TRUE) %>% add_percentage()
unvax_pandemic_over_model_tbl <- tbl_regression(unvax_pandemic_over_model, exponentiate = TRUE) %>% add_percentage()
unvax_flu_vaccine_since_jun_2021_model_tbl <- tbl_regression(unvax_flu_vaccine_since_jun_2021_model, exponentiate = TRUE) %>% add_percentage()
unvax_self_report_health_model_tbl <- tbl_regression(unvax_self_report_health_model, exponentiate = TRUE) %>% add_percentage()
unvax_employment_status_model_tbl <- tbl_regression(unvax_employment_status_model, exponentiate = TRUE) %>% add_percentage()
unvax_source_insurance_model_tbl <- tbl_regression(unvax_source_insurance_model, exponentiate = TRUE) %>% add_percentage()
# Combine into one
parent_unvaccinated_univariate_table <- tbl_stack(tbls=list(#parent_unvaccinated_overall_tbl,
                                                            unvax_five_tbl,
                                                            unvax_twelve_tbl,
                                                            unvax_sixteen_tbl,
                                                            unvax_gender_tbl, 
                                                            unvax_age_tbl,
                                                            unvax_household_income_model_tbl,
                                                            unvax_race_model_tbl,
                                                            unvax_edu_model_tbl,
                                                            unvax_party_model_tbl,
                                                            unvax_religous_model_tbl,
                                                            unvax_pandemic_over_model_tbl,
                                                            unvax_flu_vaccine_since_jun_2021_model_tbl,
                                                            unvax_self_report_health_model_tbl,
                                                            unvax_employment_status_model_tbl,
                                                            unvax_source_insurance_model_tbl)) %>% modify_caption("**Parent Not Vaccinated**")


parent_overall_tbl <- tbl_svysummary(
  survey::svydesign(ids = ~1, weights = df_parent_overall$weight_daily_national_18plus, fpc=df_parent_overall$fpc1, data=df_parent_overall),
  by = NULL,
  label = NULL,
  statistic = list(all_categorical() ~ "{n}/{N} ({p}%)"),
  digits = everything() ~ 0,
  type = NULL,
  value = NULL,
  missing = NULL,
  missing_text = NULL,
  sort = NULL,
  percent = "col",
  include = c(child_vaccinated)
)  %>% modify_header(stat_0 = "**Child+**") %>% modify_footnote(update = everything() ~ NA)

# Run Regression for all parents
overall_five_model = glm(child_vaccinated ~ have_children_mc_5_to_11, family = "binomial",data = df_parent_overall ,weights = weight_daily_national_18plus)
overall_twelve_model = glm(child_vaccinated ~ have_children_mc_12_to_15, family = "binomial",data = df_parent_overall ,weights = weight_daily_national_18plus)
overall_sixteen_model = glm(child_vaccinated ~ have_children_mc_16_to_17, family = "binomial",data = df_parent_overall ,weights = weight_daily_national_18plus)
overall_gender_model = glm(child_vaccinated ~ Gender, family = "binomial",data = df_parent_overall ,weights = weight_daily_national_18plus)
overall_age_model = glm(child_vaccinated~ `Age.Group`, family = "binomial",data = df_parent_overall,weights = weight_daily_national_18plus)
overall_household_income_model = glm(child_vaccinated~ `Household.Income`, family = "binomial",data = filter(df_parent_overall, `Household.Income`!="Did Not Respond"),weights = weight_daily_national_18plus)
overall_race_model = glm(child_vaccinated~ `Race`, family = "binomial",data = df_parent_overall,weights = weight_daily_national_18plus)
overall_edu_model = glm(child_vaccinated~ `Education`, family = "binomial",data = df_parent_overall,weights = weight_daily_national_18plus)
overall_party_model = glm(child_vaccinated~ `Party`, family = "binomial",data = df_parent_overall,weights = weight_daily_national_18plus)
overall_religous_model = glm(child_vaccinated~ `Religous`, family = "binomial",data = filter(df_parent_overall, `Religous` !="no answer"),weights = weight_daily_national_18plus)
overall_pandemic_over_model = glm(child_vaccinated~ `Pandemic.Over`, family = "binomial",data = filter(df_parent_overall, `Pandemic.Over` !="No Answer"),weights = weight_daily_national_18plus)
overall_flu_vaccine_since_jun_2021_model = glm(child_vaccinated~ `Flu.Vaccine.Since.June.2021`, family = "binomial",data = filter(df_parent_overall, `Flu.Vaccine.Since.June.2021` !="no answer"),weights = weight_daily_national_18plus)
overall_self_report_health_model = glm(child_vaccinated~ `Self.Reported.Health`, family = "binomial",data = filter(df_parent_overall, `Self.Reported.Health` !="No answer"),weights = weight_daily_national_18plus)
overall_employment_status_model = glm(child_vaccinated~ `Employment.Status`, family = "binomial",data = filter(df_parent_overall, `Employment.Status` !="No answer"),weights = weight_daily_national_18plus)
overall_source_insurance_model = glm(child_vaccinated~ `Insurance`, family = "binomial",data = filter(df_parent_overall, `Insurance` !="No answer"),weights = weight_daily_national_18plus)

overall_vaccination_model = glm(child_vaccinated~ parent_vaccination, family = "binomial",data = df_parent_overall,weights = weight_daily_national_18plus)



# Create Individual Tables
overall_five_tbl <- tbl_regression(overall_five_model, exponentiate = TRUE) %>% add_percentage()
overall_twelve_tbl <- tbl_regression(overall_twelve_model, exponentiate = TRUE) %>% add_percentage()
overall_sixteen_tbl <- tbl_regression(overall_sixteen_model, exponentiate = TRUE) %>% add_percentage()
overall_gender_tbl <- tbl_regression(overall_gender_model, exponentiate = TRUE) %>% add_percentage()
overall_age_tbl <- tbl_regression(overall_age_model, exponentiate = TRUE) %>% add_percentage()
overall_household_income_model_tbl <- tbl_regression(overall_household_income_model, exponentiate = TRUE) %>% add_percentage()
overall_race_model_tbl <- tbl_regression(overall_race_model, exponentiate = TRUE) %>% add_percentage()
overall_edu_model_tbl <- tbl_regression(overall_edu_model, exponentiate = TRUE) %>% add_percentage()
overall_party_model_tbl <- tbl_regression(overall_party_model, exponentiate = TRUE) %>% add_percentage()
overall_religous_model_tbl <- tbl_regression(overall_religous_model, exponentiate = TRUE) %>% add_percentage()
overall_pandemic_over_model_tbl <- tbl_regression(overall_pandemic_over_model, exponentiate = TRUE) %>% add_percentage()
overall_flu_vaccine_since_jun_2021_model_tbl <- tbl_regression(overall_flu_vaccine_since_jun_2021_model, exponentiate = TRUE) %>% add_percentage()
overall_self_report_health_model_tbl <- tbl_regression(overall_self_report_health_model, exponentiate = TRUE) %>% add_percentage()
overall_employment_status_model_tbl <- tbl_regression(overall_employment_status_model, exponentiate = TRUE) %>% add_percentage()
overall_source_insurance_model_tbl <- tbl_regression(overall_source_insurance_model, exponentiate = TRUE) %>% add_percentage()

overall_vaccination_model_tbl <- tbl_regression(overall_vaccination_model, exponentiate = TRUE) %>% add_percentage()

# Combine into one
parent_overall_univariate_table <- tbl_stack(tbls=list(overall_gender_tbl,
                                                       overall_age_tbl,
                                                       overall_household_income_model_tbl,
                                                       overall_race_model_tbl,
                                                       overall_edu_model_tbl,
                                                       overall_employment_status_model_tbl,
                                                       overall_source_insurance_model_tbl,
                                                       overall_self_report_health_model_tbl,
                                                       overall_religous_model_tbl,
                                                       overall_five_tbl,
                                                       overall_twelve_tbl,
                                                       overall_sixteen_tbl,
                                                       overall_party_model_tbl,
                                                       overall_pandemic_over_model_tbl,
                                                       overall_flu_vaccine_since_jun_2021_model_tbl,
                                                       overall_vaccination_model_tbl)) %>% modify_caption("**Overall**")




# Merge
##################################################
two_univariate_table <- tbl_merge(tbls=list(parent_overall_univariate_table,
                                            parent_unvaccinated_univariate_table,
                                            parent_vaccinated_univariate_table),
                                    tab_spanner = c("**Overall**", "**Unvaccinated Parents**", "**Vaccinated Parents**")) %>% 
  modify_caption("**Table 2. Univariate Relationship between Parent Characteristics and Willingness to Vaccinate Children**") %>% 
  modify_column_hide(columns = c(p.value_1, p.value_2, p.value_3)) %>% 
  modify_header(
      estimate_1 = "**Odds Ratio** **(95% confidence interval)**",
      estimate_2 = "**Odds Ratio** **(95% confidence interval)**",
      estimate_3 = "**Odds Ratio** **(95% confidence interval)**",
      ) %>%  modify_footnote(all_stat_cols() ~ NA)



two_univariate_table %>% 
  as_gt() %>%
  gt::gtsave(filename = "Table2.png")


##################################################
##################################################
##################################################
##################################################
# Table 3 - Reasons for not vaccinating children
##################################################
##################################################
##################################################
##################################################

df_child_unvaccinated <- filter(df5, always_unwilling==1)


df_child_unvaccinated <- df_child_unvaccinated %>% mutate(other = case_when(why_not_planning_get_vaccine_child_mc_other_text == "" ~ 0,
                                                        TRUE ~ 1))

df_child_unvaccinated <- df_child_unvaccinated %>% mutate(already_received_flu_vaccine = case_when(why_not_planning_get_vaccine_mc_already_received_flu_vaccine == 1 ~ 1,
                                                                                                  TRUE ~ 0))

df_child_unvaccinated <- df_child_unvaccinated %>% mutate(too_new = case_when(why_not_planning_get_vaccine_child_mc_too_new == 1 ~ 1,
                                                                                                   TRUE ~ 0))

df_child_unvaccinated <- df_child_unvaccinated %>% mutate(too_young = case_when(why_not_planning_get_vaccine_child_mc_too_young == 1 ~ 1,
                                                                              TRUE ~ 0))
df_child_unvaccinated <- df_child_unvaccinated %>% mutate(let_them_decide = case_when(why_not_planning_get_vaccine_child_mc_let_them_decide == 1 ~ 1,
                                                                                TRUE ~ 0))
df_child_unvaccinated <- df_child_unvaccinated %>% mutate(side_effects = case_when(why_not_planning_get_vaccine_child_mc_side_effect == 1 ~ 1,
                                                                                      TRUE ~ 0))
df_child_unvaccinated <- df_child_unvaccinated %>% mutate(covid_threat_exaggerated = case_when(why_not_planning_get_vaccine_child_mc_threat_exaggerated == 1 ~ 1,
                                                                                   TRUE ~ 0))

df_child_unvaccinated <- df_child_unvaccinated %>% mutate(lack_trust_government = case_when(why_not_planning_get_vaccine_child_mc_lack_trust_government	 == 1 ~ 1,
                                                                                               TRUE ~ 0))

df_child_unvaccinated <- df_child_unvaccinated %>% mutate(lack_trust_scientists = case_when(why_not_planning_get_vaccine_child_mc_lack_trust_scientist	 == 1 ~ 1,
                                                                                            TRUE ~ 0))
df_child_unvaccinated <- df_child_unvaccinated %>% mutate(vaccine_development_too_political = case_when(why_not_planning_get_vaccine_child_mc_politics	 == 1 ~ 1,
                                                                                            TRUE ~ 0))
df_child_unvaccinated <- df_child_unvaccinated %>% mutate(child_already_had_covid = case_when(why_not_planning_get_vaccine_child_mc_already_had_covid	 == 1 ~ 1,
                                                                                                        TRUE ~ 0))
df_child_unvaccinated <- df_child_unvaccinated %>% mutate(never_get_any_vaccine = case_when(why_not_planning_get_vaccine_child_mc_never_get_vaccine	 == 1 ~ 1,
                                                                       TRUE ~ 0))
df_child_unvaccinated <- df_child_unvaccinated %>% mutate(vaccine_not_recommended_for_childs_health_history = case_when(why_not_planning_get_vaccine_child_mc_health_history	 == 1 ~ 1,
                                                                       TRUE ~ 0))
df_child_unvaccinated <- df_child_unvaccinated %>% mutate(worried_get_covid_from_vaccine = case_when(why_not_planning_get_vaccine_child_mc_worried_get_covid	 == 1 ~ 1,
                                                                       TRUE ~ 0))
df_child_unvaccinated <- df_child_unvaccinated %>% mutate(not_at_risk = case_when(why_not_planning_get_vaccine_child_mc_not_at_risk	 == 1 ~ 1,
                                                                       TRUE ~ 0))
df_child_unvaccinated <- df_child_unvaccinated %>% mutate(risk_higher_infection = case_when(why_not_planning_get_vaccine_child_mc_risk_higher_infection	 == 1 ~ 1,
                                                                       TRUE ~ 0))
df_child_unvaccinated <- df_child_unvaccinated %>% mutate(afraid_needles = case_when(why_not_planning_get_vaccine_child_mc_afraid_needles	 == 1 ~ 1,
                                                                       TRUE ~ 0))
df_child_unvaccinated <- df_child_unvaccinated %>% mutate(contr_religious_belief = case_when(why_not_planning_get_vaccine_child_mc_contr_religious_belief	 == 1 ~ 1,
                                                                       TRUE ~ 0))
df_child_unvaccinated <- df_child_unvaccinated %>% mutate(wait_herd_immunity = case_when(why_not_planning_get_vaccine_child_mc_wait_herd_immunity	 == 1 ~ 1,
                                                                       TRUE ~ 0))
df_child_unvaccinated <- df_child_unvaccinated %>% mutate(others_before_me = case_when(why_not_planning_get_vaccine_child_mc_others_before_me	 == 1 ~ 1,
                                                                       TRUE ~ 0))


df_child_unvaccinated_survey <- survey::svydesign(ids = ~1, weights = df_child_unvaccinated$weight_daily_national_18plus, fpc=df_child_unvaccinated$fpc1, data=df_child_unvaccinated)


table3 <- tbl_svysummary(
  df_child_unvaccinated_survey,
  by = parent_vaccination,
  label = NULL,
  statistic = list(all_categorical() ~ "{n}/{N} ({p}%)"),
  digits = everything() ~ 0,
  type = NULL,
  value = NULL,
  missing = "no",
  missing_text = NULL,
  sort = NULL,
  percent = "col",
  include = c(already_received_flu_vaccine,
              too_new,
              too_young,
              let_them_decide,
              side_effects,
              covid_threat_exaggerated,
              lack_trust_government,
              lack_trust_scientists,
              vaccine_development_too_political,
              child_already_had_covid,
              never_get_any_vaccine,
              vaccine_not_recommended_for_childs_health_history,
              worried_get_covid_from_vaccine,
              not_at_risk,
              risk_higher_infection,
              afraid_needles,
              contr_religious_belief,
              wait_herd_immunity,
              others_before_me,
              other)) %>% 
  modify_caption("Table 3. Reasons for Child Vaccination Attitudes among Unwilling Parents") %>% 
  add_difference()

table3 %>% 
  as_gt() %>%
  gt::gtsave(filename = "table3.png")

##################################################
##################################################
##################################################
##################################################
# Table 3 by Child Age
##################################################
##################################################
##################################################
##################################################

df_5_11 <- df_child_unvaccinated %>% filter(have_children_mc_5_to_11 == 'yes')
df_12_15 <- df_child_unvaccinated %>% filter(have_children_mc_12_to_15 == 'yes')
df_16_17 <- df_child_unvaccinated %>% filter(have_children_mc_16_to_17 == 'yes')

df_5_11_surv <- survey::svydesign(ids = ~1, weights = df_5_11$weight_daily_national_18plus, fpc=df_5_11$fpc1, data=df_5_11)
df_12_15_surv <- survey::svydesign(ids = ~1, weights = df_12_15$weight_daily_national_18plus, fpc=df_12_15$fpc1, data=df_12_15)
df_16_17_surv <- survey::svydesign(ids = ~1, weights = df_16_17$weight_daily_national_18plus, fpc=df_16_17$fpc1, data=df_16_17)

table5_11 <- tbl_svysummary(
  df_5_11_surv,
  by = NULL,
  label = NULL,
  statistic = list(all_categorical() ~ "{n}/{N} ({p}%)"),
  digits = everything() ~ 0,
  type = NULL,
  value = NULL,
  missing = "no",
  missing_text = NULL,
  sort = NULL,
  percent = "col",
  include = c(already_received_flu_vaccine,
              too_new,
              too_young,
              let_them_decide,
              side_effects,
              covid_threat_exaggerated,
              lack_trust_government,
              lack_trust_scientists,
              vaccine_development_too_political,
              child_already_had_covid,
              never_get_any_vaccine,
              vaccine_not_recommended_for_childs_health_history,
              worried_get_covid_from_vaccine,
              not_at_risk,
              risk_higher_infection,
              afraid_needles,
              contr_religious_belief,
              wait_herd_immunity,
              others_before_me,
              other)) %>% 
  modify_caption("Table 3. Reasons for Child Vaccination Attitudes among Unwilling Parents")

table12_15 <- tbl_svysummary(
  df_12_15_surv,
  by = NULL,
  label = NULL,
  statistic = list(all_categorical() ~ "{n}/{N} ({p}%)"),
  digits = everything() ~ 0,
  type = NULL,
  value = NULL,
  missing = "no",
  missing_text = NULL,
  sort = NULL,
  percent = "col",
  include = c(already_received_flu_vaccine,
              too_new,
              too_young,
              let_them_decide,
              side_effects,
              covid_threat_exaggerated,
              lack_trust_government,
              lack_trust_scientists,
              vaccine_development_too_political,
              child_already_had_covid,
              never_get_any_vaccine,
              vaccine_not_recommended_for_childs_health_history,
              worried_get_covid_from_vaccine,
              not_at_risk,
              risk_higher_infection,
              afraid_needles,
              contr_religious_belief,
              wait_herd_immunity,
              others_before_me,
              other)) %>% 
  modify_caption("Table 3. Reasons for Child Vaccination Attitudes among Unwilling Parents")

table16_17 <- tbl_svysummary(
  df_16_17_surv,
  by = NULL,
  label = NULL,
  statistic = list(all_categorical() ~ "{n}/{N} ({p}%)"),
  digits = everything() ~ 0,
  type = NULL,
  value = NULL,
  missing = "no",
  missing_text = NULL,
  sort = NULL,
  percent = "col",
  include = c(already_received_flu_vaccine,
              too_new,
              too_young,
              let_them_decide,
              side_effects,
              covid_threat_exaggerated,
              lack_trust_government,
              lack_trust_scientists,
              vaccine_development_too_political,
              child_already_had_covid,
              never_get_any_vaccine,
              vaccine_not_recommended_for_childs_health_history,
              worried_get_covid_from_vaccine,
              not_at_risk,
              risk_higher_infection,
              afraid_needles,
              contr_religious_belief,
              wait_herd_immunity,
              others_before_me,
              other)) %>% 
  modify_caption("Table 3. Reasons for Child Vaccination Attitudes among Unwilling Parents")

child_age_table3 <- tbl_merge(tbls=list(table5_11, table12_15, table16_17), tab_spanner = c("**Age 5-11**", "**Age 12-15**", "**Age 16-17**")) %>% modify_caption("**Table X. Reasons for Hesitancy by Child Age**")



##################################################
##################################################
##################################################
##################################################
# CLUSTERING
##################################################
##################################################
##################################################
##################################################
survey_questions <- c("already_received_flu_vaccine",
                      "too_new",
                      "too_young",
                      "let_them_decide",
                      "side_effects",
                      "covid_threat_exaggerated",
                      "lack_trust_government",
                      "lack_trust_scientists",
                      "vaccine_development_too_political",
                      "child_already_had_covid",
                      "never_get_any_vaccine",
                      "vaccine_not_recommended_for_childs_health_history",
                      "worried_get_covid_from_vaccine",
                      "not_at_risk",
                      "risk_higher_infection",
                      "afraid_needles",
                      "contr_religious_belief",
                      "wait_herd_immunity",
                      "others_before_me",
                      "other")


library(factoextra)
library(cluster)
library('klaR')

set.seed(1234)

df_child_unvaccinated <- df_child_unvaccinated %>% mutate(num_reasons_given = already_received_flu_vaccine +
                                                          too_new +
                                                          too_young +
                                                          let_them_decide +
                                                          side_effects +
                                                          covid_threat_exaggerated +
                                                          lack_trust_government +
                                                          lack_trust_scientists +
                                                          vaccine_development_too_political +
                                                          child_already_had_covid +
                                                          never_get_any_vaccine +
                                                          vaccine_not_recommended_for_childs_health_history +
                                                          worried_get_covid_from_vaccine +
                                                          not_at_risk +
                                                          risk_higher_infection +
                                                          afraid_needles +
                                                          contr_religious_belief +
                                                          wait_herd_immunity +
                                                          others_before_me +
                                                          other)
df_child_unvaccinated$num_reasons_given <- as.numeric(df_child_unvaccinated$num_reasons_given)
df_child_unvaccinated$fpc1<-63000000


kmode2 <- kmodes(df_child_unvaccinated[, survey_questions], 3, iter.max = 10, weighted = FALSE, fast = TRUE)

df_child_unvaccinated <- cbind(df_child_unvaccinated, cluster4 = kmode2$cluster)






df_child_unvaccinated_survey <- survey::svydesign(ids = ~1, weights = df_child_unvaccinated$weight_daily_national_18plus, fpc=df_child_unvaccinated$fpc1, data=df_child_unvaccinated)

df_child_unvaccinated_parents_vaxxed <- df_child_unvaccinated %>% filter(parent_vaccination=='Vaccinated') 
df_child_unvaccinated_parents_vaxxed_survey <- survey::svydesign(ids = ~1, weights = df_child_unvaccinated_parents_vaxxed$weight_daily_national_18plus, fpc=df_child_unvaccinated_parents_vaxxed$fpc1, data=df_child_unvaccinated_parents_vaxxed)

df_child_unvaccinated_parents_unvaxxed <- df_child_unvaccinated %>% filter(parent_vaccination=='Unvaccinated')
df_child_unvaccinated_parents_unvaxxed_survey <- survey::svydesign(ids = ~1, weights = df_child_unvaccinated_parents_unvaxxed$weight_daily_national_18plus, fpc=df_child_unvaccinated_parents_unvaxxed$fpc1, data=df_child_unvaccinated_parents_unvaxxed)


table_vaccinated <- tbl_svysummary(
  df_child_unvaccinated_parents_vaxxed_survey,
  by = cluster4,
  label = list(have_children_mc_5_to_11 ~ 'Have child age 5 to 11',
               have_children_mc_12_to_15 ~ 'Have child age 12 to 15',
               have_children_mc_16_to_17 ~ 'Have child age 16 to 17',
               mRNA_Johnson ~ 'Vaccine type',
               Age.Group ~ 'Age Group',
               Household.Income ~ 'Household Income',
               Pandemic.Over ~ 'When will the Pandemic end?',
               Flu.Vaccine.Since.June.2021 ~ 'Flu vaccine since 6/21',
               Self.Reported.Health	 ~ 'Self Reported Health',
               Employment.Status ~ 'Employment Status',
               child_vaccinated ~ 'Child vaccination',
               too_new ~ 'Vaccine too new',
               too_young ~ 'Child too young',
               let_them_decide ~ 'Let child decide when older',
               side_effects ~ 'Side Effects',
               covid_threat_exaggerated ~ 'COVID-19 threat exaggerated',
               lack_trust_government ~ 'Lack trust in government',
               lack_trust_scientists ~ 'Lack trust in scientists',
               vaccine_development_too_political ~ 'Vaccine development too political',
               child_already_had_covid ~ 'Child already had COVID-19',
               never_get_any_vaccine ~ 'Child never gets any vaccine',
               vaccine_not_recommended_for_childs_health_history ~ "Vaccine not recommended for child's health history",
               worried_get_covid_from_vaccine ~ 'Worried child will get COVID-19 from vaccine',
               not_at_risk ~ 'Child is not at risk',
               risk_higher_infection ~ 'Risk from vaccine greater than risk from COVID-19',
               afraid_needles ~ 'Child is afraid of needles',
               contr_religious_belief ~ 'Vaccine is contrary to religous beliefs',
               wait_herd_immunity ~ 'Prefer to wait for herd immunity for protection',
               others_before_me ~ 'There are other people who should get it first',
               already_received_flu_vaccine ~ 'Child recieved flu vaccine and is protected',
               other ~ 'Other'),
  statistic = list(all_categorical() ~ "{n}/{N} ({p}%)"),
  digits = list(num_reasons_given ~ c(1)),
  type = NULL,
  value = NULL,
  missing = "no",
  missing_text = NULL,
  sort = NULL,
  percent = "col",
  include = c(have_children_mc_5_to_11,
              have_children_mc_12_to_15,
              have_children_mc_16_to_17,
              mRNA_Johnson, child_vaccinated, Gender, Age.Group, Household.Income, Race, Education, Party, Religous, Pandemic.Over, Flu.Vaccine.Since.June.2021, Self.Reported.Health, Employment.Status, Insurance, 
              have_children_mc_5_to_11,
              have_children_mc_12_to_15,
              have_children_mc_16_to_17,
              local_adverse_effect,
              systemic_adverse_effect,
              rash,
              nausea_vomiting,
              muscle_ache,
              pain_in_arm,
              swelling_in_arm,
              fever,
              chills,
              tiredness,
              headache,
              something_else,
              already_received_flu_vaccine,
              too_new,
              too_young,
              let_them_decide,
              side_effects,
              covid_threat_exaggerated,
              lack_trust_government,
              lack_trust_scientists,
              vaccine_development_too_political,
              child_already_had_covid,
              never_get_any_vaccine,
              vaccine_not_recommended_for_childs_health_history,
              worried_get_covid_from_vaccine,
              not_at_risk,
              risk_higher_infection,
              afraid_needles,
              contr_religious_belief,
              wait_herd_immunity,
              others_before_me,
              other,
              num_reasons_given,
              vaccination_status
              )) %>% 
  modify_caption("Table X. Four Cluster Summary of Parents Unwilling to Vaccinate their Children") %>% add_overall()

table_unvaccinated <- tbl_svysummary(
  df_child_unvaccinated_parents_unvaxxed_survey,
  by = cluster4,
  label = list(have_children_mc_5_to_11 ~ 'Have child age 5 to 11',
               have_children_mc_12_to_15 ~ 'Have child age 12 to 15',
               have_children_mc_16_to_17 ~ 'Have child age 16 to 17',
               Age.Group ~ 'Age Group',
               Household.Income ~ 'Household Income',
               Pandemic.Over ~ 'When will the Pandemic end?',
               Flu.Vaccine.Since.June.2021 ~ 'Flu vaccine since 6/21',
               Self.Reported.Health	 ~ 'Self Reported Health',
               Employment.Status ~ 'Employment Status',
               too_new ~ 'Vaccine too new',
               too_young ~ 'Child too young',
               let_them_decide ~ 'Let child decide when older',
               side_effects ~ 'Side Effects',
               covid_threat_exaggerated ~ 'COVID-19 threat exaggerated',
               lack_trust_government ~ 'Lack trust in government',
               lack_trust_scientists ~ 'Lack trust in scientists',
               vaccine_development_too_political ~ 'Vaccine development too political',
               child_already_had_covid ~ 'Child already had COVID-19',
               never_get_any_vaccine ~ 'Child never gets any vaccine',
               vaccine_not_recommended_for_childs_health_history ~ "Vaccine not recommended for child's health history",
               worried_get_covid_from_vaccine ~ 'Worried child will get COVID-19 from vaccine',
               not_at_risk ~ 'Child is not at risk',
               risk_higher_infection ~ 'Risk from vaccine greater than risk from COVID-19',
               afraid_needles ~ 'Child is afraid of needles',
               contr_religious_belief ~ 'Vaccine is contrary to religous beliefs',
               wait_herd_immunity ~ 'Prefer to wait for herd immunity for protection',
               already_received_flu_vaccine ~ 'Child recieved flu vaccine and is protected',
               others_before_me ~ 'There are other people who should get it first',
               other ~ 'Other'),
  statistic = list(all_categorical() ~ "{n}/{N} ({p}%)"),
  digits = list(num_reasons_given ~ c(1)),
  type = NULL,
  value = NULL,
  missing = "no",
  missing_text = NULL,
  sort = NULL,
  percent = "col",
  include = c(already_received_flu_vaccine,
              too_new,
              too_young,
              let_them_decide,
              side_effects,
              covid_threat_exaggerated,
              lack_trust_government,
              lack_trust_scientists,
              vaccine_development_too_political,
              child_already_had_covid,
              never_get_any_vaccine,
              vaccine_not_recommended_for_childs_health_history,
              worried_get_covid_from_vaccine,
              not_at_risk,
              risk_higher_infection,
              afraid_needles,
              contr_religious_belief,
              wait_herd_immunity,
              others_before_me,
              other,
              num_reasons_given,
              Gender, Age.Group, Household.Income, Race, Education, Employment.Status, Insurance, Self.Reported.Health, Religous,
              have_children_mc_5_to_11,
              have_children_mc_12_to_15,
              have_children_mc_16_to_17, 
              Party, Pandemic.Over, Flu.Vaccine.Since.June.2021,
              vaccination_status
              )) %>% 
  modify_caption("Table 3. Cluster Results for Parents Unwilling to Vaccinate their Children") %>% add_overall()

overall <- tbl_svysummary(
  df_child_unvaccinated_survey,
  by = parent_vaccination,
  label = list(have_children_mc_5_to_11 ~ 'Have child age 5 to 11',
               have_children_mc_12_to_15 ~ 'Have child age 12 to 15',
               have_children_mc_16_to_17 ~ 'Have child age 16 to 17',
               Age.Group ~ 'Age Group',
               Household.Income ~ 'Household Income',
               Pandemic.Over ~ 'When will the Pandemic end?',
               Flu.Vaccine.Since.June.2021 ~ 'Flu vaccine since 6/21',
               Self.Reported.Health	 ~ 'Self Reported Health',
               Employment.Status ~ 'Employment Status',
               too_new ~ 'Vaccine too new',
               too_young ~ 'Child too young',
               let_them_decide ~ 'Let child decide when older',
               side_effects ~ 'Side Effects',
               covid_threat_exaggerated ~ 'COVID-19 threat exaggerated',
               lack_trust_government ~ 'Lack trust in government',
               lack_trust_scientists ~ 'Lack trust in scientists',
               vaccine_development_too_political ~ 'Vaccine development too political',
               child_already_had_covid ~ 'Child already had COVID-19',
               never_get_any_vaccine ~ 'Child never gets any vaccine',
               vaccine_not_recommended_for_childs_health_history ~ "Vaccine not recommended for child's health history",
               worried_get_covid_from_vaccine ~ 'Worried child will get COVID-19 from vaccine',
               not_at_risk ~ 'Child is not at risk',
               risk_higher_infection ~ 'Risk from vaccine greater than risk from COVID-19',
               afraid_needles ~ 'Child is afraid of needles',
               contr_religious_belief ~ 'Vaccine is contrary to religous beliefs',
               wait_herd_immunity ~ 'Prefer to wait for herd immunity for protection',
               others_before_me ~ 'There are other people who should get it first',
               already_received_flu_vaccine ~ 'Child recieved flu vaccine and is protected',
               other ~ 'Other'
               ),
  statistic = list(all_categorical() ~ "{n}/{N} ({p}%)"),
  digits = list(num_reasons_given ~ c(1)),
  type = NULL,
  value = NULL,
  missing = "no",
  missing_text = NULL,
  sort = NULL,
  percent = "col",
  include = c(already_received_flu_vaccine,
              too_new,
              too_young,
              let_them_decide,
              side_effects,
              covid_threat_exaggerated,
              lack_trust_government,
              lack_trust_scientists,
              vaccine_development_too_political,
              child_already_had_covid,
              never_get_any_vaccine,
              vaccine_not_recommended_for_childs_health_history,
              worried_get_covid_from_vaccine,
              not_at_risk,
              risk_higher_infection,
              afraid_needles,
              contr_religious_belief,
              wait_herd_immunity,
              others_before_me,
              other,
              num_reasons_given,
              Gender, Age.Group, Household.Income, Race, Education, Employment.Status, Insurance, Self.Reported.Health, Religous,
              have_children_mc_5_to_11,
              have_children_mc_12_to_15,
              have_children_mc_16_to_17, 
              Party, Pandemic.Over, Flu.Vaccine.Since.June.2021,
              vaccination_status
              )) %>% 
  modify_caption("Table 3. Reasons for Child Vaccination Attitudes among All Parents") %>% add_overall() %>% 
  add_difference() 

overall <- overall %>% modify_column_hide(stat_1) %>% modify_column_hide(stat_2) %>% modify_column_hide(estimate) %>% modify_column_hide(p.value)


cluster_new <- tbl_merge(tbls=list(overall, table_unvaccinated,
                                              table_vaccinated),
                                    tab_spanner = c("**Overall**", "**Parent unvaccinated**", "**Parent vaccinated**")) %>% 
  modify_caption("**Table 3. Cluster Results for Parents Unwilling to Vaccinate their Children**")





overall_no_strata <- tbl_svysummary(
  df_child_unvaccinated_survey,
  by = cluster4,
  label = list(have_children_mc_5_to_11 ~ 'Have child age 5 to 11',
               have_children_mc_12_to_15 ~ 'Have child age 12 to 15',
               have_children_mc_16_to_17 ~ 'Have child age 16 to 17',
               Age.Group ~ 'Age Group',
               Household.Income ~ 'Household Income',
               Pandemic.Over ~ 'When will the Pandemic end?',
               Flu.Vaccine.Since.June.2021 ~ 'Flu vaccine since 6/21',
               Self.Reported.Health	 ~ 'Self Reported Health',
               Employment.Status ~ 'Employment Status',
               too_new ~ 'Vaccine too new',
               too_young ~ 'Child too young',
               let_them_decide ~ 'Let child decide when older',
               side_effects ~ 'Side Effects',
               covid_threat_exaggerated ~ 'COVID-19 threat exaggerated',
               lack_trust_government ~ 'Lack trust in government',
               lack_trust_scientists ~ 'Lack trust in scientists',
               vaccine_development_too_political ~ 'Vaccine development too political',
               child_already_had_covid ~ 'Child already had COVID-19',
               never_get_any_vaccine ~ 'Child never gets any vaccine',
               vaccine_not_recommended_for_childs_health_history ~ "Vaccine not recommended for child's health history",
               worried_get_covid_from_vaccine ~ 'Worried child will get COVID-19 from vaccine',
               not_at_risk ~ 'Child is not at risk',
               risk_higher_infection ~ 'Risk from vaccine greater than risk from COVID-19',
               afraid_needles ~ 'Child is afraid of needles',
               contr_religious_belief ~ 'Vaccine is contrary to religous beliefs',
               wait_herd_immunity ~ 'Prefer to wait for herd immunity for protection',
               others_before_me ~ 'There are other people who should get it first',
               already_received_flu_vaccine ~ 'Child recieved flu vaccine and is protected',
               other ~ 'Other'
  ),
  statistic = list(all_categorical() ~ "{n}/{N} ({p}%)"),
  digits = list(num_reasons_given ~ c(1)),
  type = NULL,
  value = NULL,
  missing = "no",
  missing_text = NULL,
  sort = NULL,
  percent = "col",
  include = c(already_received_flu_vaccine,
              too_new,
              too_young,
              let_them_decide,
              side_effects,
              covid_threat_exaggerated,
              lack_trust_government,
              lack_trust_scientists,
              vaccine_development_too_political,
              child_already_had_covid,
              never_get_any_vaccine,
              vaccine_not_recommended_for_childs_health_history,
              worried_get_covid_from_vaccine,
              not_at_risk,
              risk_higher_infection,
              afraid_needles,
              contr_religious_belief,
              wait_herd_immunity,
              others_before_me,
              other,
              num_reasons_given,
              Gender, Age.Group, Household.Income, Race, Education, Employment.Status, Insurance, Self.Reported.Health, Religous,
              have_children_mc_5_to_11,
              have_children_mc_12_to_15,
              have_children_mc_16_to_17, 
              Party, Pandemic.Over, Flu.Vaccine.Since.June.2021,
              vaccination_status)) %>% add_overall() %>% 
  modify_caption("Table 3. Reasons for Child Vaccination Attitudes among All Parents") 

overall_no_strata %>% as_gt() %>% gt::gtsave(filename="Cluster4.png")


cluster_new %>%
  as_gt() %>%
  gt::gtsave(filename = "Table3.png", vwidth = 1500, vheight = 1000)

ft <- cluster_new %>% as_flex_table()

my_doc <- officer::read_docx()

flextable::body_add_flextable(
  my_doc, ft)
print(my_doc, target =
        "my_doc.docx")


# OVERALL TABLE FOR CALCULATING PVALUES
df_child_unvaccinated$c2<-ifelse(df_child_unvaccinated$cluster4==2,1,0)

df_child_unvaccinated$c3<-ifelse(df_child_unvaccinated$cluster4==3,1,0)

df_child_unvaccinated$c1<-ifelse(df_child_unvaccinated$cluster4==1,1,0)

df_child_unvaccinated$num_reasons_given <- as.numeric(df_child_unvaccinated$num_reasons_given)
df_c1 <- df_child_unvaccinated %>% filter(cluster4==1)
df_c2 <- df_child_unvaccinated %>% filter(cluster4==2)
df_c3 <- df_child_unvaccinated %>% filter(cluster4==3)
df_c3$num_reasons_given <- as.numeric(df_c3$num_reasons_given)
c1_surv <- survey::svydesign(ids = ~1, weights = df_c1$weight_daily_national_18plus, fpc=df_c1$fpc1, data=df_c1)
c2_surv <- survey::svydesign(ids = ~1, weights = df_c2$weight_daily_national_18plus, fpc=df_c2$fpc1, data=df_c2)
c3_surv <- survey::svydesign(ids = ~1, weights = df_c3$weight_daily_national_18plus, fpc=df_c3$fpc1, data=df_c3)

c1_tbl <- tbl_svysummary(
  c1_surv,
  by = parent_vaccination,
  label = list(have_children_mc_5_to_11 ~ 'Have child age 5 to 11',
               have_children_mc_12_to_15 ~ 'Have child age 12 to 15',
               have_children_mc_16_to_17 ~ 'Have child age 16 to 17',
               Age.Group ~ 'Age Group',
               Household.Income ~ 'Household Income',
               Pandemic.Over ~ 'When will the Pandemic end?',
               Flu.Vaccine.Since.June.2021 ~ 'Flu vaccine since 6/21',
               Self.Reported.Health	 ~ 'Self Reported Health',
               Employment.Status ~ 'Employment Status',
               too_new ~ 'Vaccine too new',
               too_young ~ 'Child too young',
               let_them_decide ~ 'Let child decide when older',
               side_effects ~ 'Side Effects',
               covid_threat_exaggerated ~ 'COVID-19 threat exaggerated',
               lack_trust_government ~ 'Lack trust in government',
               lack_trust_scientists ~ 'Lack trust in scientists',
               vaccine_development_too_political ~ 'Vaccine development too political',
               child_already_had_covid ~ 'Child already had COVID-19',
               never_get_any_vaccine ~ 'Child never gets any vaccine',
               vaccine_not_recommended_for_childs_health_history ~ "Vaccine not recommended for child's health history",
               worried_get_covid_from_vaccine ~ 'Worried child will get COVID-19 from vaccine',
               not_at_risk ~ 'Child is not at risk',
               risk_higher_infection ~ 'Risk from vaccine greater than risk from COVID-19',
               afraid_needles ~ 'Child is afraid of needles',
               contr_religious_belief ~ 'Vaccine is contrary to religous beliefs',
               wait_herd_immunity ~ 'Prefer to wait for herd immunity for protection',
               others_before_me ~ 'There are other people who should get it first',
               already_received_flu_vaccine ~ 'Child recieved flu vaccine and is protected',
               other ~ 'Other'
  ),
  statistic = list(all_categorical() ~ "{n}/{N} ({p}%)"),
  digits = list(num_reasons_given ~ c(1)),
  type = NULL,
  value = NULL,
  missing = "no",
  missing_text = NULL,
  sort = NULL,
  percent = "col",
  include = c(already_received_flu_vaccine,
              too_new,
              too_young,
              let_them_decide,
              side_effects,
              covid_threat_exaggerated,
              lack_trust_government,
              lack_trust_scientists,
              vaccine_development_too_political,
              child_already_had_covid,
              never_get_any_vaccine,
              vaccine_not_recommended_for_childs_health_history,
              worried_get_covid_from_vaccine,
              not_at_risk,
              risk_higher_infection,
              afraid_needles,
              contr_religious_belief,
              wait_herd_immunity,
              others_before_me,
              other,
              num_reasons_given,
              Gender, Age.Group, Household.Income, Race, Education, Employment.Status, Insurance, Self.Reported.Health, Religous,
              have_children_mc_5_to_11,
              have_children_mc_12_to_15,
              have_children_mc_16_to_17, 
              Party, Pandemic.Over, Flu.Vaccine.Since.June.2021)) %>% 
  modify_caption("Table 3. Reasons for Child Vaccination Attitudes among All Parents") %>% add_p()
c2_tbl <- tbl_svysummary(
  c2_surv,
  by = parent_vaccination,
  label = list(have_children_mc_5_to_11 ~ 'Have child age 5 to 11',
               have_children_mc_12_to_15 ~ 'Have child age 12 to 15',
               have_children_mc_16_to_17 ~ 'Have child age 16 to 17',
               Age.Group ~ 'Age Group',
               Household.Income ~ 'Household Income',
               Pandemic.Over ~ 'When will the Pandemic end?',
               Flu.Vaccine.Since.June.2021 ~ 'Flu vaccine since 6/21',
               Self.Reported.Health	 ~ 'Self Reported Health',
               Employment.Status ~ 'Employment Status',
               too_new ~ 'Vaccine too new',
               too_young ~ 'Child too young',
               let_them_decide ~ 'Let child decide when older',
               side_effects ~ 'Side Effects',
               covid_threat_exaggerated ~ 'COVID-19 threat exaggerated',
               lack_trust_government ~ 'Lack trust in government',
               lack_trust_scientists ~ 'Lack trust in scientists',
               vaccine_development_too_political ~ 'Vaccine development too political',
               child_already_had_covid ~ 'Child already had COVID-19',
               never_get_any_vaccine ~ 'Child never gets any vaccine',
               vaccine_not_recommended_for_childs_health_history ~ "Vaccine not recommended for child's health history",
               worried_get_covid_from_vaccine ~ 'Worried child will get COVID-19 from vaccine',
               not_at_risk ~ 'Child is not at risk',
               risk_higher_infection ~ 'Risk from vaccine greater than risk from COVID-19',
               afraid_needles ~ 'Child is afraid of needles',
               contr_religious_belief ~ 'Vaccine is contrary to religous beliefs',
               wait_herd_immunity ~ 'Prefer to wait for herd immunity for protection',
               others_before_me ~ 'There are other people who should get it first',
               already_received_flu_vaccine ~ 'Child recieved flu vaccine and is protected',
               other ~ 'Other'
  ),
  statistic = list(all_categorical() ~ "{n}/{N} ({p}%)"),
  digits = list(num_reasons_given ~ c(1)),
  type = NULL,
  value = NULL,
  missing = "no",
  missing_text = NULL,
  sort = NULL,
  percent = "col",
  include = c(already_received_flu_vaccine,
              too_new,
              too_young,
              let_them_decide,
              side_effects,
              covid_threat_exaggerated,
              lack_trust_government,
              lack_trust_scientists,
              vaccine_development_too_political,
              child_already_had_covid,
              never_get_any_vaccine,
              vaccine_not_recommended_for_childs_health_history,
              worried_get_covid_from_vaccine,
              not_at_risk,
              risk_higher_infection,
              afraid_needles,
              contr_religious_belief,
              wait_herd_immunity,
              others_before_me,
              other,
              num_reasons_given,
              Gender, Age.Group, Household.Income, Race, Education, Employment.Status, Insurance, Self.Reported.Health, Religous,
              have_children_mc_5_to_11,
              have_children_mc_12_to_15,
              have_children_mc_16_to_17, 
              Party, Pandemic.Over, Flu.Vaccine.Since.June.2021)) %>% 
  modify_caption("Table 3. Reasons for Child Vaccination Attitudes among All Parents") %>% add_p()
c3_tbl <- tbl_svysummary(
  c3_surv,
  by = parent_vaccination,
  label = list(have_children_mc_5_to_11 ~ 'Have child age 5 to 11',
               have_children_mc_12_to_15 ~ 'Have child age 12 to 15',
               have_children_mc_16_to_17 ~ 'Have child age 16 to 17',
               Age.Group ~ 'Age Group',
               Household.Income ~ 'Household Income',
               Pandemic.Over ~ 'When will the Pandemic end?',
               Flu.Vaccine.Since.June.2021 ~ 'Flu vaccine since 6/21',
               Self.Reported.Health	 ~ 'Self Reported Health',
               Employment.Status ~ 'Employment Status',
               too_new ~ 'Vaccine too new',
               too_young ~ 'Child too young',
               let_them_decide ~ 'Let child decide when older',
               side_effects ~ 'Side Effects',
               covid_threat_exaggerated ~ 'COVID-19 threat exaggerated',
               lack_trust_government ~ 'Lack trust in government',
               lack_trust_scientists ~ 'Lack trust in scientists',
               vaccine_development_too_political ~ 'Vaccine development too political',
               child_already_had_covid ~ 'Child already had COVID-19',
               never_get_any_vaccine ~ 'Child never gets any vaccine',
               vaccine_not_recommended_for_childs_health_history ~ "Vaccine not recommended for child's health history",
               worried_get_covid_from_vaccine ~ 'Worried child will get COVID-19 from vaccine',
               not_at_risk ~ 'Child is not at risk',
               risk_higher_infection ~ 'Risk from vaccine greater than risk from COVID-19',
               afraid_needles ~ 'Child is afraid of needles',
               contr_religious_belief ~ 'Vaccine is contrary to religous beliefs',
               wait_herd_immunity ~ 'Prefer to wait for herd immunity for protection',
               others_before_me ~ 'There are other people who should get it first',
               already_received_flu_vaccine ~ 'Child recieved flu vaccine and is protected',
               other ~ 'Other'
  ),
  statistic = list(all_categorical() ~ "{n}/{N} ({p}%)"),
  digits = list(num_reasons_given ~ c(1)),
  type = list(num_reasons_given ~ "continuous"),
  value = NULL,
  missing = "no",
  missing_text = NULL,
  sort = NULL,
  percent = "col",
  include = c(already_received_flu_vaccine,
              too_new,
              too_young,
              let_them_decide,
              side_effects,
              covid_threat_exaggerated,
              lack_trust_government,
              lack_trust_scientists,
              vaccine_development_too_political,
              child_already_had_covid,
              never_get_any_vaccine,
              vaccine_not_recommended_for_childs_health_history,
              worried_get_covid_from_vaccine,
              not_at_risk,
              risk_higher_infection,
              afraid_needles,
              contr_religious_belief,
              wait_herd_immunity,
              others_before_me,
              other,
              num_reasons_given,
              Gender, Age.Group, Household.Income, Race, Education, Employment.Status, Insurance, Self.Reported.Health, Religous,
              have_children_mc_5_to_11,
              have_children_mc_12_to_15,
              have_children_mc_16_to_17, 
              Party, Pandemic.Over, Flu.Vaccine.Since.June.2021)) %>% 
  modify_caption("Table 3. Reasons for Child Vaccination Attitudes among All Parents") %>% add_p()





cluster4_surv <- survey::svydesign(ids = ~1, weights = df_child_unvaccinated$weight_daily_national_18plus, fpc=df_child_unvaccinated$fpc1, data=df_child_unvaccinated)


pval <- tbl_svysummary(
  cluster4_surv,
  by = c3,
  label = list(have_children_mc_5_to_11 ~ 'Have child age 5 to 11',
               have_children_mc_12_to_15 ~ 'Have child age 12 to 15',
               have_children_mc_16_to_17 ~ 'Have child age 16 to 17',
               Age.Group ~ 'Age Group',
               Household.Income ~ 'Household Income',
               Pandemic.Over ~ 'When will the Pandemic end?',
               Flu.Vaccine.Since.June.2021 ~ 'Flu vaccine since 6/21',
               Self.Reported.Health	 ~ 'Self Reported Health',
               Employment.Status ~ 'Employment Status',
               too_new ~ 'Vaccine too new',
               too_young ~ 'Child too young',
               let_them_decide ~ 'Let child decide when older',
               side_effects ~ 'Side Effects',
               covid_threat_exaggerated ~ 'COVID-19 threat exaggerated',
               lack_trust_government ~ 'Lack trust in government',
               lack_trust_scientists ~ 'Lack trust in scientists',
               vaccine_development_too_political ~ 'Vaccine development too political',
               child_already_had_covid ~ 'Child already had COVID-19',
               never_get_any_vaccine ~ 'Child never gets any vaccine',
               vaccine_not_recommended_for_childs_health_history ~ "Vaccine not recommended for child's health history",
               worried_get_covid_from_vaccine ~ 'Worried child will get COVID-19 from vaccine',
               not_at_risk ~ 'Child is not at risk',
               risk_higher_infection ~ 'Risk from vaccine greater than risk from COVID-19',
               afraid_needles ~ 'Child is afraid of needles',
               contr_religious_belief ~ 'Vaccine is contrary to religous beliefs',
               wait_herd_immunity ~ 'Prefer to wait for herd immunity for protection',
               others_before_me ~ 'There are other people who should get it first',
               already_received_flu_vaccine ~ 'Child recieved flu vaccine and is protected',
               other ~ 'Other'
  ),
  statistic = list(all_categorical() ~ "{n}/{N} ({p}%)"),
  digits = list(num_reasons_given ~ c(1)),
  type = NULL,
  value = NULL,
  missing = "no",
  missing_text = NULL,
  sort = NULL,
  percent = "col",
  include = c(already_received_flu_vaccine,
              too_new,
              too_young,
              let_them_decide,
              side_effects,
              covid_threat_exaggerated,
              lack_trust_government,
              lack_trust_scientists,
              vaccine_development_too_political,
              child_already_had_covid,
              never_get_any_vaccine,
              vaccine_not_recommended_for_childs_health_history,
              worried_get_covid_from_vaccine,
              not_at_risk,
              risk_higher_infection,
              afraid_needles,
              contr_religious_belief,
              wait_herd_immunity,
              others_before_me,
              other,
              num_reasons_given,
              Gender, Age.Group, Household.Income, Race, Education, Employment.Status, Insurance, Self.Reported.Health, Religous,
              have_children_mc_5_to_11,
              have_children_mc_12_to_15,
              have_children_mc_16_to_17, 
              Party, Pandemic.Over, Flu.Vaccine.Since.June.2021)) %>% 
  modify_caption("Table 3. Reasons for Child Vaccination Attitudes among All Parents") %>% add_p(pvalue_fun = fmt_pvalue_with_stars)


df_child_unvaccinated_parent_vac <- df_child_unvaccinated %>% filter(parent_vaccination=='Vaccinated')
df_child_unvaccinated_parent_vac$mRNA_Johnson <- droplevels(df_child_unvaccinated_parent_vac$mRNA_Johnson)
cluster4_vacc_surv <- survey::svydesign(ids = ~1, weights = df_child_unvaccinated_parent_vac$weight_daily_national_18plus, fpc=df_child_unvaccinated_parent_vac$fpc1, data=df_child_unvaccinated_parent_vac)

pval_vacc <- tbl_svysummary(
  cluster4_vacc_surv,
  by = c1,
  label = list(have_children_mc_5_to_11 ~ 'Have child age 5 to 11',
               have_children_mc_12_to_15 ~ 'Have child age 12 to 15',
               have_children_mc_16_to_17 ~ 'Have child age 16 to 17',
               mRNA_Johnson ~ 'Vaccine type',
               Age.Group ~ 'Age Group',
               Household.Income ~ 'Household Income',
               Pandemic.Over ~ 'When will the Pandemic end?',
               Flu.Vaccine.Since.June.2021 ~ 'Flu vaccine since 6/21',
               Self.Reported.Health	 ~ 'Self Reported Health',
               Employment.Status ~ 'Employment Status',
               child_vaccinated ~ 'Child vaccination',
               too_new ~ 'Vaccine too new',
               too_young ~ 'Child too young',
               let_them_decide ~ 'Let child decide when older',
               side_effects ~ 'Side Effects',
               covid_threat_exaggerated ~ 'COVID-19 threat exaggerated',
               lack_trust_government ~ 'Lack trust in government',
               lack_trust_scientists ~ 'Lack trust in scientists',
               vaccine_development_too_political ~ 'Vaccine development too political',
               child_already_had_covid ~ 'Child already had COVID-19',
               never_get_any_vaccine ~ 'Child never gets any vaccine',
               vaccine_not_recommended_for_childs_health_history ~ "Vaccine not recommended for child's health history",
               worried_get_covid_from_vaccine ~ 'Worried child will get COVID-19 from vaccine',
               not_at_risk ~ 'Child is not at risk',
               risk_higher_infection ~ 'Risk from vaccine greater than risk from COVID-19',
               afraid_needles ~ 'Child is afraid of needles',
               contr_religious_belief ~ 'Vaccine is contrary to religous beliefs',
               wait_herd_immunity ~ 'Prefer to wait for herd immunity for protection',
               others_before_me ~ 'There are other people who should get it first',
               already_received_flu_vaccine ~ 'Child recieved flu vaccine and is protected',
               other ~ 'Other'),
  statistic = list(all_categorical() ~ "{n}/{N} ({p}%)"),
  digits = list(num_reasons_given ~ c(1)),
  type = NULL,
  value = NULL,
  missing = "no",
  missing_text = NULL,
  sort = NULL,
  percent = "col",
  include = c(have_children_mc_5_to_11,
              have_children_mc_12_to_15,
              have_children_mc_16_to_17,
              mRNA_Johnson, child_vaccinated, Gender, Age.Group, Household.Income, Race, Education, Party, Religous, Pandemic.Over, Flu.Vaccine.Since.June.2021, Self.Reported.Health, Employment.Status, Insurance, 
              have_children_mc_5_to_11,
              have_children_mc_12_to_15,
              have_children_mc_16_to_17,
              local_adverse_effect,
              systemic_adverse_effect,
              rash,
              nausea_vomiting,
              muscle_ache,
              pain_in_arm,
              swelling_in_arm,
              fever,
              chills,
              tiredness,
              headache,
              something_else,
              already_received_flu_vaccine,
              too_new,
              too_young,
              let_them_decide,
              side_effects,
              covid_threat_exaggerated,
              lack_trust_government,
              lack_trust_scientists,
              vaccine_development_too_political,
              child_already_had_covid,
              never_get_any_vaccine,
              vaccine_not_recommended_for_childs_health_history,
              worried_get_covid_from_vaccine,
              not_at_risk,
              risk_higher_infection,
              afraid_needles,
              contr_religious_belief,
              wait_herd_immunity,
              others_before_me,
              other,
              num_reasons_given)) %>% 
  modify_caption("Table X. Four Cluster Summary of Parents Unwilling to Vaccinate their Children") %>%  add_p(pvalue_fun = fmt_pvalue_with_stars)

fmt_pvalue_with_stars <- function(x) {
  dplyr::case_when(
    x < 0.001 ~ paste0(style_pvalue(x), "***"),
    x < 0.01 ~ paste0(style_pvalue(x), "**"),
    x < 0.05 ~ paste0(style_pvalue(x), "*"),
    TRUE ~ style_pvalue(x)
  )
}


##################################################
##################################################
##################################################
##################################################
# Multivariate Regression - TWO SEPARATE
##################################################
##################################################
##################################################
##################################################

##################################################
# Parent Plus No Flu Shot
##################################################
df_parent_plus_no_flu <- filter(df5, 
                                  child_vaccinated != "No answer" &
                                  parent_vaccination == "Vaccinated") %>% filter(vaccination_status!='Partially Vaccinated')

parent_plus_no_flu = glm(child_vaccinated ~ have_children_mc_5_to_11 + have_children_mc_12_to_15 + have_children_mc_16_to_17 + Gender + Age.Group + Employment.Status +
                           Household.Income + Race + Education + Party + Religous +
                          `Self.Reported.Health` + `Insurance` + mRNA_Johnson +
                           rash + nausea_vomiting + muscle_ache + pain_in_arm + swelling_in_arm +
                          fever + chills + tiredness + headache + something_else,
                          family = 'binomial', data = df_parent_plus_no_flu,
                         weights = weight_daily_national_18plus)

library(car)
vif_values <- vif(parent_plus_no_flu)
vif_values <- vif_values[,3]
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)




parent_plus_no_flu_tbl <- tbl_regression(parent_plus_no_flu, exponentiate = TRUE) #%>% add_vif("aGVIF")


##################################################
# Parent Negative No Flu Shot
##################################################
df_parent_neg <- filter(df5, 
                                  child_vaccinated != "No answer" &
                          parent_vaccination == "Unvaccinated")
                               

parent_neg = glm(child_vaccinated ~ have_children_mc_5_to_11 + have_children_mc_12_to_15 + have_children_mc_16_to_17 + Gender + `Age.Group` + `Employment.Status` +
                           `Household.Income` + `Race` + `Education` + 
                           `Party` + `Religous` + `Self.Reported.Health` + `Insurance`
                         , family = "binomial",data = df_parent_neg ,weights = weight_daily_national_18plus)
parent_neg_tbl <- tbl_regression(parent_neg, exponentiate = TRUE) #%>% add_vif("aGVIF")


##################################################
# Not separating vaccination status out
##################################################
df_multi <- df5 %>% filter(child_vaccinated != 'No answer')
parent_both = glm(child_vaccinated ~ Gender + Age.Group + Household.Income + Race + Education + Employment.Status +
                    Insurance + Self.Reported.Health + Religous + have_children_mc_5_to_11 + have_children_mc_12_to_15 +
                    have_children_mc_16_to_17 + Party + vaccination_status
                 , family = "binomial",data = df_multi ,weights = weight_daily_national_18plus)
parent_both_tbl <- tbl_regression(parent_both, exponentiate = TRUE) #%>% add_vif("aGVIF")

parent_both_no_vax = glm(child_vaccinated ~ Gender + Age.Group + Household.Income + Race + Education + Employment.Status +
                    Insurance + Self.Reported.Health + Religous + have_children_mc_5_to_11 + have_children_mc_12_to_15 +
                    have_children_mc_16_to_17 + Party
                  , family = "binomial",data = df_multi ,weights = weight_daily_national_18plus)
parent_both_no_vax_tbl <- tbl_regression(parent_both_no_vax, exponentiate = TRUE)


plot_model(parent_both, type='pred', terms=c("vaccination_status"), title='', wrap.labels = 5) + 
  theme_classic() +
  xlab('Parent Vaccination Status') +
  ylab('Probability of Child Vaccinated') +
  font_size(axis_title.x = 15, axis_title.y = 15, labels.x=10, labels.y=10) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle=20, hjust=1))




library(ggforestplot)
library(ggforce)
df_parent_plus_no_flu <- data.frame(summary(parent_plus_no_flu)$coefficients)
df_parent_plus_no_flu <- cbind(name = rownames(df_parent_plus_no_flu), df_parent_plus_no_flu)
rownames(df_parent_plus_no_flu) <- NULL
df_parent_plus_no_flu$Model <- 'Vaccinated Parents'

df_parent_neg <- data.frame(summary(parent_neg)$coefficients)
df_parent_neg <- cbind(name = rownames(df_parent_neg), df_parent_neg)
rownames(df_parent_neg) <- NULL
df_parent_neg$Model <- 'Unvaccinated Parents'

df_both <- data.frame(summary(parent_both)$coefficients)
df_both <- cbind(name = rownames(df_both), df_both)
rownames(df_both) <- NULL
df_both$Model <- 'All Parents'

both_models_df <- rbind(df_both, df_parent_plus_no_flu, df_parent_neg)
  

both_models_df$group <- 'NULL'
both_models_df[both_models_df$name=='have_children_mc_5_to_11',]$group <- 'Have Children'
both_models_df[both_models_df$name=='have_children_mc_12_to_15',]$group <- 'Have Children'
both_models_df[both_models_df$name=='have_children_mc_16_to_17',]$group <- 'Have Children'
both_models_df[both_models_df$name=='GenderMale',]$group <- 'Gender'
both_models_df[both_models_df$name=='GenderTransgender or Nonbinary',]$group <- 'Gender'
both_models_df[both_models_df$name=='Age.Group30-39 years',]$group <- 'Age'
both_models_df[both_models_df$name=='Age.Group40-49 years',]$group <- 'Age'
both_models_df[both_models_df$name=='Age.Group50-64 years',]$group <- 'Age'
both_models_df[both_models_df$name=='Age.Group65+ years',]$group <- 'Age'
both_models_df[both_models_df$name=='Employment.StatusUnemployed',]$group <- 'Employment Status'
both_models_df[both_models_df$name=='Household.Income$50,000 and $99,999',]$group <- 'Household Income'
both_models_df[both_models_df$name=='Household.IncomeOver $100,000',]$group <- 'Household Income'
both_models_df[both_models_df$name=='RaceOther',]$group <- 'Race/Ethnicity'
both_models_df[both_models_df$name=='RaceAsian',]$group <- 'Race/Ethnicity'
both_models_df[both_models_df$name=='RaceBlack or African American',]$group <- 'Race/Ethnicity'
both_models_df[both_models_df$name=='RaceHispanic or Latino/a',]$group <- 'Race/Ethnicity'
both_models_df[both_models_df$name=='EducationSome College',]$group <- 'Education'
both_models_df[both_models_df$name=='EducationCollege Graduate',]$group <- 'Education'
both_models_df[both_models_df$name=='PartyDemocrat',]$group <- 'Political Party Affiliation'
both_models_df[both_models_df$name=='PartyIndependent',]$group <- 'Political Party Affiliation'
both_models_df[both_models_df$name=='ReligousAtheist/Agnostic',]$group <- 'Religious Status'
both_models_df[both_models_df$name=='Self.Reported.HealthGood',]$group <- 'Self Reported Health'
both_models_df[both_models_df$name=='Self.Reported.HealthVery good',]$group <- 'Self Reported Health'
both_models_df[both_models_df$name=='Self.Reported.HealthExcellent',]$group <- 'Self Reported Health'
both_models_df[both_models_df$name=='InsuranceUninsured',]$group <- 'Health Insurance'
both_models_df[both_models_df$name=='mRNA_JohnsonmRNA',]$group <- 'Parent Vaccination Type'
both_models_df[both_models_df$name=='rash1',]$group <- 'Vaccine Side Effect'
both_models_df[both_models_df$name=='nausea_vomiting1',]$group <- 'Vaccine Side Effect'
both_models_df[both_models_df$name=='muscle_ache1',]$group <- 'Vaccine Side Effect'
both_models_df[both_models_df$name=='pain_in_arm1',]$group <- 'Vaccine Side Effect'
both_models_df[both_models_df$name=='swelling_in_arm1',]$group <- 'Vaccine Side Effect'
both_models_df[both_models_df$name=='fever1',]$group <- 'Vaccine Side Effect'
both_models_df[both_models_df$name=='chills1',]$group <- 'Vaccine Side Effect'
both_models_df[both_models_df$name=='tiredness1',]$group <- 'Vaccine Side Effect'
both_models_df[both_models_df$name=='headache1',]$group <- 'Vaccine Side Effect'
both_models_df[both_models_df$name=='something_else1',]$group <- 'Vaccine Side Effect'

both_models_df[both_models_df$name=='have_children_mc_5_to_11',]$name <- 'Have Children Ages 5 to 11'
both_models_df[both_models_df$name=='have_children_mc_12_to_15',]$name <- 'Have Children Ages 12 to 15'
both_models_df[both_models_df$name=='have_children_mc_16_to_17',]$name <- 'Have Children Ages 16 to 17'
both_models_df[both_models_df$name=='GenderMale',]$name <- 'Male'
both_models_df[both_models_df$name=='GenderTransgender or Nonbinary',]$name <- 'Transgender or Nonbinary'
both_models_df[both_models_df$name=='Age.Group30-39 years',]$name <- '30-39 years'
both_models_df[both_models_df$name=='Age.Group40-49 years',]$name <- '40-49 years'
both_models_df[both_models_df$name=='Age.Group50-64 years',]$name <- '50-64 years'
both_models_df[both_models_df$name=='Age.Group65+ years',]$name <- '65+ years'
both_models_df[both_models_df$name=='Employment.StatusUnemployed',]$name <- 'Unemployed'
both_models_df[both_models_df$name=='Household.Income$50,000 and $99,999',]$name <- 'Between $50,000 and $99,999'
both_models_df[both_models_df$name=='Household.IncomeOver $100,000',]$name <- 'Over $100,000'
both_models_df[both_models_df$name=='RaceOther',]$name <- 'Other'
both_models_df[both_models_df$name=='RaceAsian',]$name <- 'Asian'
both_models_df[both_models_df$name=='RaceBlack or African American',]$name <- 'Black or African American'
both_models_df[both_models_df$name=='RaceHispanic or Latino/a',]$name <- 'Hispanic or Latino/a'
both_models_df[both_models_df$name=='EducationSome College',]$name <- 'Some College'
both_models_df[both_models_df$name=='EducationCollege Graduate',]$name <- 'College Graduate'
both_models_df[both_models_df$name=='PartyDemocrat',]$name <- 'Democrat'
both_models_df[both_models_df$name=='PartyIndependent',]$name <- 'Independent'
both_models_df[both_models_df$name=='ReligousAtheist/Agnostic',]$name <- 'Atheist/Agnostic'
both_models_df[both_models_df$name=='Self.Reported.HealthGood',]$name <- 'Good'
both_models_df[both_models_df$name=='Self.Reported.HealthVery good',]$name <- 'Very good'
both_models_df[both_models_df$name=='Self.Reported.HealthExcellent',]$name <- 'Excellent'
both_models_df[both_models_df$name=='InsuranceUninsured',]$name <- 'Uninsured'
both_models_df[both_models_df$name=='mRNA_JohnsonmRNA',]$name <- 'mRNA'
both_models_df[both_models_df$name=='rash1',]$name <- 'Rash'
both_models_df[both_models_df$name=='nausea_vomiting1',]$name <- 'Nausea/Vomiting'
both_models_df[both_models_df$name=='muscle_ache1',]$name <- 'Muscle Ache'
both_models_df[both_models_df$name=='pain_in_arm1',]$name <- 'Pain in Arm'
both_models_df[both_models_df$name=='swelling_in_arm1',]$name <- 'Swelling in Arm'
both_models_df[both_models_df$name=='fever1',]$name <- 'Fever'
both_models_df[both_models_df$name=='chills1',]$name <- 'Chills'
both_models_df[both_models_df$name=='tiredness1',]$name <- 'Tiredness'
both_models_df[both_models_df$name=='headache1',]$name <- 'Headache'
both_models_df[both_models_df$name=='something_else1',]$name <- 'Something else'
both_models_df <- both_models_df %>% filter(name!='(Intercept)')
both_models_df <- both_models_df %>% filter(name!='vaccination_statusFully Vaccinated')
both_models_df <- both_models_df %>% filter(name!='vaccination_statusPartially Vaccinated')
both_models_df <- both_models_df %>% filter(name!='vaccination_statusFully Vaccinated and Boosted')



p <- ggforestplot::forestplot(
  df = both_models_df,
  name = name,
  estimate = Estimate,
  se = Std..Error,
  xlab = "Odds Ratio",
  colour = Model,
  logodds = TRUE
  
) +
  ggforce::facet_col(
    facets = ~group,
    scales = "free_y",
    space = "free"
  ) + 
  ggforestplot::scale_colour_ng_d(palette = 'pesto')
  


grDevices::pdf("biomarker_linear_associations.pdf", 15, 15)
p
grDevices::dev.off()



plot_models(parent_plus_no_flu, parent_neg, grid = FALSE, 
            m.labels = c("Vaccinated Parents", "Unvaccinated Parents"),
            axis.labels = c('have_children_mc_5_to_11' = 'Have Child Age 5 to 11',
                               'have_children_mc_12_to_15' = 'Have Child Age 12 to 15',
                               'have_children_mc_16_to_17' = 'Have Child Age 16 to 17',
                                'GenderMale' = 'Male',
                            'GenderTransgender or Nonbinary' = 'Transgender or Nonbinary',
                            'Age.Group30-39 years' = '30-39 years',
                            'Age.Group40-49 years' = '40-49 years',
                            'Age.Group50-64 years' = '50-64 years',
                            'Age.Group65+ years' = '65+ years',
                            'Employment.StatusUnemployed' = 'Unemployed',
                            'Household.Income$50,000 and $99,999' = 'Income $50,000-$99,999',
                            'Household.IncomeOver $100,000' = 'Income >$100,000',
                            'RaceOther' = 'Other Race',
                            'RaceAsian' = 'Asian Race',
                            'RaceBlack or African American' = 'Black or African American',
                            'RaceHispanic or Latino/a' = 'Hispanic or Latino/a',
                            'EducationSome College' = 'Some College',
                            'EducationCollege Graduate' = 'College Graduate',
                            'PartyDemocrat' = 'Democrat',
                            'PartyIndependent' = 'Independent',
                            'ReligousAtheist/Agnostic' = 'Atheist/Agnostic',
                            'Self.Reported.HealthGood' = 'Good Health',
                            'Self.Reported.HealthVery good' = 'Very Good health',
                            'Self.Reported.HealthExcellent' = 'Excellent Health',
                            'InsuranceUninsured' = 'Uninsured',
                            'mRNA_JohnsonmRNA' = 'mRNA',
                            'rash1' = 'Rash',
                            'nausea_vomiting1' = 'Nausea/Vomiting',
                            'muscle_ache1' = 'Muscle Ache',
                            'pain_in_arm1' = 'Arm Pain',
                            'swelling_in_arm1' = 'Arm Swelling',
                            'fever1' = 'Fever',
                            'chills1' = 'Chills',
                            'tiredness1' = 'Tiredness',
                            'headache1' = 'Headache',
                            'something_else1' = 'Something Else'
                            )) + 
                          ylim(0, 6.1)+
                          theme_sjplot2()+
                          scale_y_continuous(breaks = 0:6.1, limits = c(0, NA))


##################################################
# Using all Four vaccination statuses 
##################################################
parent_four = glm(child_vaccinated ~ Gender + Age.Group + Household.Income + Race + Education + Employment.Status +
                    Insurance + Self.Reported.Health + Religous + have_children_mc_5_to_11 + have_children_mc_12_to_15 +
                    have_children_mc_16_to_17 + Party + vaccination_status
                  , family = "binomial",data = df_multi ,weights = weight_daily_national_18plus)
parent_four_tbl <- tbl_regression(parent_four, exponentiate = TRUE) #%>% add_vif("aGVIF")

plot_model(parent_four, type='pred', terms=c("vaccination_status"))




##################################################
# Merge
##################################################
two_multivariate_table <- tbl_merge(tbls=list(parent_both_tbl %>% add_significance_stars(pattern = "{estimate} ({conf.low}, {conf.high}){stars}",hide_ci = TRUE, hide_se = TRUE),
                                              parent_both_no_vax_tbl %>% add_significance_stars(pattern = "{estimate} ({conf.low}, {conf.high}){stars}",hide_ci = TRUE, hide_se = TRUE),
                                              parent_neg_tbl %>% add_significance_stars(pattern = "{estimate} ({conf.low}, {conf.high}){stars}",hide_ci = TRUE, hide_se = TRUE),
                                              parent_plus_no_flu_tbl %>% add_significance_stars(pattern = "{estimate} ({conf.low}, {conf.high}){stars}",hide_ci = TRUE, hide_se = TRUE)
                                              ),
                                tab_spanner = c("**Overall**", "**Overall No Vaccine**", "**Unvaccinated Parents**", "**Vaccinated Parents**")) %>% 
  modify_caption("**Table 3. Multivariate Relationship between Parent Characteristics and Willingness to Vaccinate Children**") %>% 
  modify_column_hide(columns = c(p.value_1, p.value_2, p.value_3, p.value_4)) %>% 
  modify_header(
    estimate_1 = "**Odds Ratio** **(95% confidence interval)**",
    estimate_2 = "**Odds Ratio** **(95% confidence interval)**",
    estimate_3 = "**Odds Ratio** **(95% confidence interval)**",
    estimate_4 = "**Odds Ratio** **(95% confidence interval)**",
  ) %>% modify_footnote(all_stat_cols() ~ NA) 





two_multivariate_table %>% 
  as_gt() %>%
  gt::gtsave(filename = "two_multivariate.png")

vif(parent_both_no_vax)
vif(parent_both)
vif(parent_neg)
vif(parent_plus_no_flu)

### APPENDIX TABLE


appendix_multivariate_table <- tbl_merge(tbls=list(parent_both_tbl,parent_four_tbl),
                                         tab_spanner = c("**Overall Aggregated**", "**Overall Disaggregated**")) %>% 
  modify_caption("**Appendix Table 1. Multivariate Relationship between Parent Characteristics and Willingness to Vaccinate Children**") %>% 
  modify_column_hide(columns = c(p.value_1, p.value_2)) %>% 
  modify_header(
    estimate_1 = "**Odds Ratio** **(95% confidence interval)**",
    estimate_2 = "**Odds Ratio** **(95% confidence interval)**",
  ) %>% modify_footnote(all_stat_cols() ~ NA)



appendix_multivariate_table %>% 
  as_gt() %>%
  gt::gtsave(filename = "appendix_table1.png")

plot_models(parent_both, parent_four, grid = FALSE,
            m.labels = c("Aggregated", "Disaggregated")) + ylim(0, 5)


##################################################
# Predicted Effects
##################################################
plot_models(parent_plus_no_flu, parent_neg, grid = FALSE,
            m.labels = c("Vaccinated Parents", "Unvaccinated Parents")) + ylim(0, 5)



##################################################
##################################################
##################################################
##################################################
# Stratified Analysis
##################################################
##################################################
##################################################
##################################################
df_democrat <- df5 %>% filter(child_vaccinated != 'No answer') %>% filter(Party == 'Democrat')
df_not_democrat <- df5 %>% filter(child_vaccinated != 'No answer') %>% filter(Party!='Democrat')

parent_democrat = glm(child_vaccinated ~ Gender + Age.Group + Household.Income + Race + Education + Employment.Status +
                    Insurance + Self.Reported.Health + Religous + have_children_mc_5_to_11 + have_children_mc_12_to_15 +
                    have_children_mc_16_to_17 + vaccination_status
                  , family = "binomial",data = df_democrat ,weights = weight_daily_national_18plus)
parent_democrat_tbl <- tbl_regression(parent_democrat, exponentiate = TRUE) 

parent_not_democrat = glm(child_vaccinated ~ Gender + Age.Group + Household.Income + Race + Education + Employment.Status +
                        Insurance + Self.Reported.Health + Religous + have_children_mc_5_to_11 + have_children_mc_12_to_15 +
                        have_children_mc_16_to_17 + vaccination_status
                      , family = "binomial",data = df_not_democrat ,weights = weight_daily_national_18plus)
parent_not_democrat_tbl <- tbl_regression(parent_not_democrat, exponentiate = TRUE) 

party_table <- tbl_merge(tbls=list(parent_democrat_tbl %>% add_significance_stars(pattern = "{estimate} ({conf.low}, {conf.high}){stars}",hide_ci = TRUE, hide_se = TRUE),
                                   parent_not_democrat_tbl %>% add_significance_stars(pattern = "{estimate} ({conf.low}, {conf.high}){stars}",hide_ci = TRUE, hide_se = TRUE)
),
tab_spanner = c("**Democrat**", "**Not Democrat**")) %>% 
  modify_caption("**Dem vs Not Dem**") %>% 
  modify_column_hide(columns = c(p.value_1, p.value_2)) %>% 
  modify_header(
    estimate_1 = "**Odds Ratio** **(95% confidence interval)**",
    estimate_2 = "**Odds Ratio** **(95% confidence interval)**",
  ) %>% modify_footnote(all_stat_cols() ~ NA) 




df_high_income <- df5 %>% filter(child_vaccinated != 'No answer') %>% filter(Household.Income == 'Over $100,000')
df_low_income <- df5 %>% filter(child_vaccinated != 'No answer') %>% filter(Household.Income!='Over $100,000')

parent_high_income = glm(child_vaccinated ~ Gender + Age.Group + Race + Education + Employment.Status +
                        Insurance + Self.Reported.Health + Religous + have_children_mc_5_to_11 + have_children_mc_12_to_15 +
                        have_children_mc_16_to_17 + Party + vaccination_status
                      , family = "binomial",data = df_high_income ,weights = weight_daily_national_18plus)
parent_high_income_tbl <- tbl_regression(parent_high_income, exponentiate = TRUE) 

parent_low_income = glm(child_vaccinated ~ Gender + Age.Group + Race + Education + Employment.Status +
                            Insurance + Self.Reported.Health + Religous + have_children_mc_5_to_11 + have_children_mc_12_to_15 +
                            have_children_mc_16_to_17 + Party + vaccination_status
                          , family = "binomial",data = df_low_income ,weights = weight_daily_national_18plus)
parent_low_income_tbl <- tbl_regression(parent_low_income, exponentiate = TRUE) 

income_table <- tbl_merge(tbls=list(parent_high_income_tbl %>% add_significance_stars(pattern = "{estimate} ({conf.low}, {conf.high}){stars}",hide_ci = TRUE, hide_se = TRUE),
                                   parent_low_income_tbl %>% add_significance_stars(pattern = "{estimate} ({conf.low}, {conf.high}){stars}",hide_ci = TRUE, hide_se = TRUE)
),
tab_spanner = c("**High Income**", "**Low Income**")) %>% 
  modify_caption("**Income**") %>% 
  modify_column_hide(columns = c(p.value_1, p.value_2)) %>% 
  modify_header(
    estimate_1 = "**Odds Ratio** **(95% confidence interval)**",
    estimate_2 = "**Odds Ratio** **(95% confidence interval)**",
  ) %>% modify_footnote(all_stat_cols() ~ NA) 




df_college <- df5 %>% filter(child_vaccinated != 'No answer') %>% filter(Education == 'College Graduate')
df_not_college <- df5 %>% filter(child_vaccinated != 'No answer') %>% filter(Education!='College Graduate')

parent_college = glm(child_vaccinated ~ Gender + Age.Group + Household.Income + Race + Employment.Status +
                        Insurance + Self.Reported.Health + Religous + have_children_mc_5_to_11 + have_children_mc_12_to_15 +
                        have_children_mc_16_to_17 + Party + vaccination_status
                      , family = "binomial",data = df_college ,weights = weight_daily_national_18plus)
parent_college_tbl <- tbl_regression(parent_college, exponentiate = TRUE) 

parent_not_college = glm(child_vaccinated ~ Gender + Age.Group + Household.Income + Race + Employment.Status +
                            Insurance + Self.Reported.Health + Religous + have_children_mc_5_to_11 + have_children_mc_12_to_15 +
                            have_children_mc_16_to_17 + Party + vaccination_status
                          , family = "binomial",data = df_not_college ,weights = weight_daily_national_18plus)
parent_not_college_tbl <- tbl_regression(parent_not_college, exponentiate = TRUE) 

college_table <- tbl_merge(tbls=list(parent_college_tbl %>% add_significance_stars(pattern = "{estimate} ({conf.low}, {conf.high}){stars}",hide_ci = TRUE, hide_se = TRUE),
                                   parent_not_college_tbl %>% add_significance_stars(pattern = "{estimate} ({conf.low}, {conf.high}){stars}",hide_ci = TRUE, hide_se = TRUE)
),
tab_spanner = c("**College**", "**Not College**")) %>% 
  modify_caption("**College vs Not College**") %>% 
  modify_column_hide(columns = c(p.value_1, p.value_2)) %>% 
  modify_header(
    estimate_1 = "**Odds Ratio** **(95% confidence interval)**",
    estimate_2 = "**Odds Ratio** **(95% confidence interval)**",
  ) %>% modify_footnote(all_stat_cols() ~ NA) 



df_old <- df5 %>% filter(child_vaccinated != 'No answer') %>% filter(Age.Group == '50-64 years' | Age.Group == '65+ years')
df_not_old <- df5 %>% filter(child_vaccinated != 'No answer') %>% filter(Age.Group!='50-64 years' & Age.Group != '65+ years')

parent_old = glm(child_vaccinated ~ Gender + Household.Income + Race + Education + Employment.Status +
                        Insurance + Self.Reported.Health + Religous + have_children_mc_5_to_11 + have_children_mc_12_to_15 +
                        have_children_mc_16_to_17 + Party + vaccination_status
                      , family = "binomial",data = df_old ,weights = weight_daily_national_18plus)
parent_old_tbl <- tbl_regression(parent_old, exponentiate = TRUE) 

parent_not_old = glm(child_vaccinated ~ Gender + Household.Income + Race + Education + Employment.Status +
                            Insurance + Self.Reported.Health + Religous + have_children_mc_5_to_11 + have_children_mc_12_to_15 +
                            have_children_mc_16_to_17 + Party + vaccination_status
                          , family = "binomial",data = df_not_old ,weights = weight_daily_national_18plus)
parent_not_old_tbl <- tbl_regression(parent_not_old, exponentiate = TRUE) 

age_table <- tbl_merge(tbls=list(parent_old_tbl %>% add_significance_stars(pattern = "{estimate} ({conf.low}, {conf.high}){stars}",hide_ci = TRUE, hide_se = TRUE),
                                   parent_not_old_tbl %>% add_significance_stars(pattern = "{estimate} ({conf.low}, {conf.high}){stars}",hide_ci = TRUE, hide_se = TRUE)
),
tab_spanner = c("**Old**", "**Not Old**")) %>% 
  modify_caption("**Age**") %>% 
  modify_column_hide(columns = c(p.value_1, p.value_2)) %>% 
  modify_header(
    estimate_1 = "**Odds Ratio** **(95% confidence interval)**",
    estimate_2 = "**Odds Ratio** **(95% confidence interval)**",
  ) %>% modify_footnote(all_stat_cols() ~ NA) 



##################################################
##################################################
##################################################
##################################################
# 80/20 Test Train Split
##################################################
##################################################
##################################################
##################################################
set.seed(1)

df_multi <- df5 %>% filter(child_vaccinated != 'No answer')


#use 80% of dataset as training set and 20% as test set
sample <- sample(c(TRUE, FALSE), nrow(df_multi), replace=TRUE, prob=c(0.8,0.2))
train  <- df_multi[sample, ]
test   <- df_multi[!sample, ]



parent_both_train = glm(child_vaccinated ~ Gender + Age.Group + Household.Income + Race + Education + Employment.Status +
                    Insurance + Self.Reported.Health + Religous + have_children_mc_5_to_11 + have_children_mc_12_to_15 +
                    have_children_mc_16_to_17 + Party + vaccination_status
                  , family = "binomial",data = train ,weights = weight_daily_national_18plus)
# parent_both_tbl <- tbl_regression(parent_both_train, exponentiate = TRUE) #%>% add_vif("aGVIF")

logitModelPred <- predict(parent_both_train, test, type = "response")
plot(logitModelPred, 
     main = "Scatterplot of Probabilities of Default (test data)", 
     xlab = "Customer ID", ylab = "Predicted Probability of Default")

# setting the cut-off probablity
classify50 <- ifelse(logitModelPred > 0.5,"vaccinated","unvaccinated")

# ordering the levels
classify50 <- ordered(classify50, levels = c("vaccinated", "unvaccinated"))
test$child_vaccinated <- ordered(test$child_vaccinated, levels = c("vaccinated", "unvaccinated"))

# confusion matrix
cm <- table(Predicted = classify50, Actual = test$child_vaccinated)
cm
library(caret)
confusionMatrix(cm)




library(ROCR)
PredLR <- predict(parent_both_train, test,type = "response")
lgPredObj <- prediction((1-PredLR),test$child_vaccinated)
lgPerfObj <- performance(lgPredObj, "tpr","fpr")
# plotting ROC curve
plot(lgPerfObj,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")

aucLR <- performance(lgPredObj, measure = "auc")
aucLR <- aucLR@y.values[[1]]



##################################################
##################################################
##################################################
##################################################
# Response Table
##################################################
##################################################
##################################################
##################################################

survey_multi <- survey::svydesign(ids = ~1, weights = df_multi$weight_daily_national_18plus, fpc=df_multi$fpc1, data=df_multi)
survey_unvaccinated <- survey::svydesign(ids = ~1, weights = df_parent_neg$weight_daily_national_18plus, fpc=df_parent_neg$fpc1, data=df_parent_neg)
survey_vaccinated <- survey::svydesign(ids = ~1, weights = df_parent_plus_no_flu$weight_daily_national_18plus, fpc=df_parent_plus_no_flu$fpc1, data=df_parent_plus_no_flu)

all_parents_table <- tbl_svysummary(
    survey_multi,
    by = child_vaccinated,
    label = list(have_children_mc_5_to_11 ~ 'Have Child Age 5 to 11 Years',
                 have_children_mc_12_to_15 ~ 'Have Child Age 12 to 15 Years',
                 have_children_mc_16_to_17 ~ 'Have Child Age 16 to 17 Years',
                 mRNA_Johnson ~ 'Parent COVID-19 Vaccination',
                 Age.Group ~ 'Age Group',
                 Household.Income ~ 'Household Income',
                 Pandemic.Over ~ 'When Will the Pandemic End?',
                 Flu.Vaccine.Since.June.2021 ~ 'Flu Vaccine Since June 2021',
                 Self.Reported.Health	 ~ 'Self Reported Health',
                 Employment.Status ~ 'Employment Status	',
                 Race ~ 'Race/Ethnicity',
                 Insurance ~ 'Health Insurance',
                 Party ~ 'Political Party Affiliation'),
    statistic = list(all_categorical() ~ "{n}/{N} ({p}%)"),
    digits = everything() ~ 0,
    type = list(everything() ~ 'categorical'),
    value = NULL,
    missing = "no",
    missing_text = NULL,
    sort = NULL,
    percent = "row",
    include = c(Gender, Age.Group, Household.Income, Race, Education, Employment.Status, Insurance, Self.Reported.Health, Religous,
                mRNA_Johnson,
                have_children_mc_5_to_11,
                have_children_mc_12_to_15,
                have_children_mc_16_to_17,
                Party, Pandemic.Over, Flu.Vaccine.Since.June.2021, vaccination_status)) %>% modify_caption("Table X. Willingness to Vaccinate")  %>% modify_footnote(all_stat_cols() ~ NA)
unvaccinated_table <- tbl_svysummary(
  survey_unvaccinated,
  by = child_vaccinated,
  label = list(have_children_mc_5_to_11 ~ 'Have Child Age 5 to 11 Years',
               have_children_mc_12_to_15 ~ 'Have Child Age 12 to 15 Years',
               have_children_mc_16_to_17 ~ 'Have Child Age 16 to 17 Years',
               mRNA_Johnson ~ 'Parent COVID-19 Vaccination',
               Age.Group ~ 'Age Group',
               Household.Income ~ 'Household Income',
               Pandemic.Over ~ 'When Will the Pandemic End?',
               Flu.Vaccine.Since.June.2021 ~ 'Flu Vaccine Since June 2021',
               Self.Reported.Health	 ~ 'Self Reported Health',
               Employment.Status ~ 'Employment Status	',
               Race ~ 'Race/Ethnicity',
               Insurance ~ 'Health Insurance',
               Party ~ 'Political Party Affiliation'),
  statistic = list(all_categorical() ~ "{n}/{N} ({p}%)"),
  digits = everything() ~ 0,
  list(everything() ~ 'categorical'),
  value = NULL,
  missing = "no",
  missing_text = NULL,
  sort = NULL,
  percent = "row",
  include = c(Gender, Age.Group, Household.Income, Race, Education, Employment.Status, Insurance, Self.Reported.Health, Religous,
              mRNA_Johnson,
              have_children_mc_5_to_11,
              have_children_mc_12_to_15,
              have_children_mc_16_to_17,
              Party, Pandemic.Over, Flu.Vaccine.Since.June.2021, vaccination_status)) %>% modify_caption("Table X. Willingness to Vaccinate")  %>% modify_footnote(all_stat_cols() ~ NA)
vaccinated_table <- tbl_svysummary(
  survey_vaccinated,
  by = child_vaccinated,
  label = list(have_children_mc_5_to_11 ~ 'Have Child Age 5 to 11 Years',
               have_children_mc_12_to_15 ~ 'Have Child Age 12 to 15 Years',
               have_children_mc_16_to_17 ~ 'Have Child Age 16 to 17 Years',
               mRNA_Johnson ~ 'Parent COVID-19 Vaccination',
               Age.Group ~ 'Age Group',
               Household.Income ~ 'Household Income',
               Pandemic.Over ~ 'When Will the Pandemic End?',
               Flu.Vaccine.Since.June.2021 ~ 'Flu Vaccine Since June 2021',
               Self.Reported.Health	 ~ 'Self Reported Health',
               Employment.Status ~ 'Employment Status	',
               Race ~ 'Race/Ethnicity',
               Insurance ~ 'Health Insurance',
               Party ~ 'Political Party Affiliation'),
  statistic = list(all_categorical() ~ "{n}/{N} ({p}%)"),
  digits = everything() ~ 0,
  type = list(everything() ~ 'categorical'),
  value = NULL,
  missing = "no",
  missing_text = NULL,
  sort = NULL,
  percent = "row",
  include = c(Gender, Age.Group, Household.Income, Race, Education, Employment.Status, Insurance, Self.Reported.Health, Religous,
              mRNA_Johnson,
              have_children_mc_5_to_11,
              have_children_mc_12_to_15,
              have_children_mc_16_to_17,
              Party, Pandemic.Over, Flu.Vaccine.Since.June.2021, vaccination_status)) %>% modify_caption("Table X. Willingness to Vaccinate")  %>% modify_footnote(all_stat_cols() ~ NA)



##################################################
##################################################
##################################################
##################################################
# Releveling
##################################################
##################################################
##################################################
##################################################
df_relevel <- df5 %>% filter(child_vaccinated != 'No answer')

df_relevel$vaccination_status <- relevel(df_relevel$vaccination_status, ref = "Partially Vaccinated")

partial = glm(child_vaccinated ~ Gender + Age.Group + Household.Income + Race + Education + Employment.Status +
                    Insurance + Self.Reported.Health + Religous + have_children_mc_5_to_11 + have_children_mc_12_to_15 +
                    have_children_mc_16_to_17 + Party + vaccination_status
                  , family = "binomial",data = df_relevel ,weights = weight_daily_national_18plus)
partial_tbl <- tbl_regression(partial, exponentiate = TRUE) %>% add_significance_stars(pattern = "{estimate} ({conf.low}, {conf.high}){stars}",hide_ci = TRUE, hide_se = TRUE)

df_relevel$vaccination_status <- relevel(df_relevel$vaccination_status, ref = "Fully Vaccinated")
full = glm(child_vaccinated ~ Gender + Age.Group + Household.Income + Race + Education + Employment.Status +
                Insurance + Self.Reported.Health + Religous + have_children_mc_5_to_11 + have_children_mc_12_to_15 +
                have_children_mc_16_to_17 + Party + vaccination_status
              , family = "binomial",data = df_relevel ,weights = weight_daily_national_18plus)
full_tbl <- tbl_regression(full, exponentiate = TRUE) %>% add_significance_stars(pattern = "{estimate} ({conf.low}, {conf.high}){stars}",hide_ci = TRUE, hide_se = TRUE)

df_relevel$vaccination_status <- relevel(df_relevel$vaccination_status, ref = "Fully Vaccinated and Boosted")
boosted = glm(child_vaccinated ~ Gender + Age.Group + Household.Income + Race + Education + Employment.Status +
             Insurance + Self.Reported.Health + Religous + have_children_mc_5_to_11 + have_children_mc_12_to_15 +
             have_children_mc_16_to_17 + Party + vaccination_status
           , family = "binomial",data = df_relevel ,weights = weight_daily_national_18plus)
boosted_tbl <- tbl_regression(boosted, exponentiate = TRUE) %>% add_significance_stars(pattern = "{estimate} ({conf.low}, {conf.high}){stars}",hide_ci = TRUE, hide_se = TRUE)


