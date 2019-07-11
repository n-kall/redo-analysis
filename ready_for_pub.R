# getting read files for publication

library(tidyverse)

# VR study

load("vr_carsac_glmm_redo_trialint_bootstrap_cov.RData")
load("vr_child_glmm_redo_bootstrap.RData")
load("vr_sidewalk_glmm_redo_bootstrap.RData")

                                        # anonymous participant IDs
vr_participants_anon  <- fct_anon(child.sub$participant.ID)

children_vs_adults_vr <- as_tibble(child.sub) %>%
  select(-passedSanCheck, - SecondSanCheck, -age_c, -prevVRExp, -heardBoutAV, -perceivedCar,
         -recAccidents) %>%
    rename(participant = participant.ID,
           gender = gender,
         age = age,
         visual_acuity = visImpairment,
         driving_experience = drivExperience,
         opinion_of_self_driving_cars = opinAV,
         identification = perceivedIden,
         judgement = decision) %>%
  mutate(driving_experience = fct_recode(driving_experience,
                                         none = "Ich fahre nicht Auto",
                                         "1 to 5 years" = "1-5 Jahre",
                                         "5 to 10 years" = "5-10",
                                         "more than 10 years" = "10+"),
         perspective = fct_recode(perspective,
                                  "pedestrian (larger group/adults)" = "PedLarge",
                                  "pedestrian (smaller group/children)" = "PedSmall",
                                  passenger = "Passenger",
                                  observer = "Bystander"),
         identification = fct_recode(identification,
                                     pedestrian = "Pedestrian",
                                     observer = "Bystander",
                                     passenger = "Passenger"),
         judgement = fct_recode(judgement,
                                towards_adults = "hitAdults",
                                towards_children = "hitChildren"),
         trial = fct_recode(trial,
                            smaller_groups = "smallGroups",
                            larger_groups = "largeGroups"),
         education = fct_recode(education,
                                "no higher education" = "keinen Hocschulabschluss",
                                "undergraduate studies" = "Hochschulabschluss (Bachelor, Vordiplom,etc.)",
                                "graduate studies" = "Hochschulabschluss (Master, Diplom, oder höher)"),
         opinion_of_self_driving_cars = fct_recode(opinion_of_self_driving_cars,
                                                   positive = "Positiv",
                                                   "very positive" = "Sehr positiv",
                                                   neutral = "Neutral",
                                                   negative = "Negativ",
                                                   "very negative" = "Sehr negativ"),
         visual_acuity = fct_recode(visual_acuity, "no impairment" = "Nein",
                                    "corrected vision" = "Ja, korrigiert durch Brille/ Kontaktlinsen",
                                    "uncorrected vision" = "Ja"),
         participant = vr_participants_anon)


sidewalk_vs_road_vr <- as_tibble(sidewalk.sub) %>%
  select(-passedSanCheck, - SecondSanCheck, -age_c, -prevVRExp, -heardBoutAV, -perceivedCar,
         -recAccidents) %>%
  rename(participant = participant.ID,
         gender = gender,
         age = age,
         visual_acuity = visImpairment,
         driving_experience = drivExperience,
         opinion_of_self_driving_cars = opinAV,
         identification = perceivedIden,
         judgement = decision) %>%
  mutate(driving_experience = fct_recode(driving_experience,
                                         none = "Ich fahre nicht Auto",
                                         "1 to 5 years" = "1-5 Jahre",
                                         "5 to 10 years" = "5-10",
                                         "more than 10 years" = "10+"),
         perspective = fct_recode(perspective,
                                  "pedestrian (larger group/road)" = "PedLarge",
                                  "pedestrian (smaller group/sidewalk)" = "PedSmall",
                                  passenger = "Passenger",
                                  observer = "Bystander"),
         identification = fct_recode(identification,
                                     pedestrian = "Pedestrian",
                                     observer = "Bystander",
                                     passenger = "Passenger"),
         judgement = fct_recode(judgement,
                                endanger_road = "hitStreet",
                                endanger_sidewalk = "hitSidewalk"),
         trial = fct_recode(trial,
                            smaller_groups = "smallGroups",
                            larger_groups = "largeGroups"),
         education = fct_recode(education,
                                "no higher education" = "keinen Hocschulabschluss",
                                "undergraduate studies" = "Hochschulabschluss (Bachelor, Vordiplom,etc.)",
                                "graduate studies" = "Hochschulabschluss (Master, Diplom, oder höher)"),
         opinion_of_self_driving_cars = fct_recode(opinion_of_self_driving_cars,
                                                   positive = "Positiv",
                                                   "very positive" = "Sehr positiv",
                                                   neutral = "Neutral",
                                                   negative = "Negativ",
                                                   "very negative" = "Sehr negativ"),
         visual_acuity = fct_recode(visual_acuity, "no impairment" = "Nein",
                                    "corrected vision" = "Ja, korrigiert durch Brille/ Kontaktlinsen",
                                    "uncorrected vision" = "Ja"),
                  participant = vr_participants_anon)




car_vs_pedestrians_vr <- as_tibble(carsac.sub) %>%
  select(-passedSanCheck, - SecondSanCheck, -age_c, -prevVRExp, -heardBoutAV, -perceivedCar,
         -recAccidents) %>%
  rename(participant = participant.ID,
         gender = gender,
         age = age,
         visual_acuity = visImpairment,
         driving_experience = drivExperience,
         opinion_of_self_driving_cars = opinAV,
         identification = perceivedIden,
         judgement = decision) %>%
  mutate(driving_experience = fct_recode(driving_experience,
                                         none = "Ich fahre nicht Auto",
                                         "1 to 5 years" = "1-5 Jahre",
                                         "5 to 10 years" = "5-10",
                                         "more than 10 years" = "10+"),
         perspective = fct_recode(perspective,
                                  pedestrian = "Pedestrian",
                                  passenger = "Passenger",
                                  observer = "Bystander"),
         identification = fct_recode(identification,
                                     pedestrian = "Pedestrian",
                                     observer = "Bystander",
                                     passenger = "Passenger"),
         judgement = fct_recode(judgement,
                                endanger_pedestrians = "hitPedestrians",
                                endanger_car_occupants = "selfSacrifice"),
         trial = fct_recode(trial,
                            parked_van = "cityR",
                            cliff = "mountain"),
         education = fct_recode(education,
                                "no higher education" = "keinen Hocschulabschluss",
                                "undergraduate studies" = "Hochschulabschluss (Bachelor, Vordiplom,etc.)",
                                "graduate studies" = "Hochschulabschluss (Master, Diplom, oder höher)"),
         opinion_of_self_driving_cars = fct_recode(opinion_of_self_driving_cars,
                                                   positive = "Positiv",
                                                   "very positive" = "Sehr positiv",
                                                   neutral = "Neutral",
                                                   negative = "Negativ",
                                                   "very negative" = "Sehr negativ"),
         visual_acuity = fct_recode(visual_acuity, "no impairment" = "Nein",
                                    "corrected vision" = "Ja, korrigiert durch Brille/ Kontaktlinsen",
                                    "uncorrected vision" = "Ja"),
                  participant = vr_participants_anon)

# Online study

load("online_study_redo2.RData")

pedestrians_vs_pedestrians_online <- pedped.sub %>%
  as_tibble() %>%
  select(-ratio_c, -ratio, -country) %>%
  rename(lives_at_risk = "ratio_f",
         positive_opinion_of_self_driving_cars = "better_place",
         identification = "identify",
         knowledge_of_self_driving_cars = "knowledge_of_av",
         road_type = "scenario",
         judgement = "response") %>%
  mutate(
    identification = fct_recode(identification,
                               car_occupant = "car",
                               pedestrian = "ped"),
    perspective = fct_recode(perspective,
                             car_occupant = "car"),
    driving_experience = fct_recode(driving_experience,
                                    none = "0",
                                    "6 to 10 years" = "6-10",
                                    "less than 5 years" = "5",
                                    "more than 10 years" = "10+"))

car_occupants_vs_pedestrians_online <- carped.sub %>%
  as_tibble() %>%
  select(-ratio_c, -ratio, -country, -scenario) %>%
  rename(lives_at_risk = "ratio_f",
         positive_opinion_of_self_driving_cars = "better_place",
         identification = "identify",
         knowledge_of_self_driving_cars = "knowledge_of_av",
         judgement = "response") %>%
  mutate(
    identification = fct_recode(identification,
                                car_occupant = "car",
                                pedestrian = "ped"),
    perspective = fct_recode(perspective,
                             car_occupant = "car"),
    driving_experience = fct_recode(driving_experience,
                                    none = "0",
                                    "6 to 10 years" = "6-10",
                                    "less than 5 years" = "5",
                                    "more than 10 years" = "10+"))

                                        # anonymize


write_csv(children_vs_adults_vr, "Study1a.csv")
write_csv(sidewalk_vs_road_vr, "Study1b.csv")
write_csv(car_vs_pedestrians_vr, "Study1c.csv")

write_csv(pedestrians_vs_pedestrians_online, "Study2a.csv")
write_csv(car_occupants_vs_pedestrians_online, "Study2b.csv")
