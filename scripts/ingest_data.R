# METADATA ====
# Description: Data ingestion
# Created: 2022-05-21 (Emma Terblanche)
# Refactored: 2023-04-04 (Rulof Burger)
# Reviewed: NA

# SUMMARY: This script opens the raw GHS data files downloaded from DataFirst.
# It then proceeds to filter on the working age population, and harmonise the
# variable names and labels. Finally, the harmonised data frames are joined into
# a single analysis data frame.

# INITIALISE ====

# Load packages ----
library(tidyverse)

# LOAD DATA ====

#> 2002 data ----
worker_2002 <- read.csv("data/raw/GHS 2002 Data/ghs-2002-worker-v1.3.csv", header = TRUE) %>% rename_with(tolower)
person_2002 <- read.csv("data/raw/GHS 2002 Data/ghs-2002-person-v1.3.csv", header = TRUE) %>% rename_with(tolower) 

#> 2003 data ----




# WRANGLE DATA ====

#> 2002 data ----

pooled_2002 <- person_2002 %>% 
  filter(age > 14 & age < 65) %>% 
  left_join(worker_2002 %>% select(c('uqnr', 'personnr', 'status1', 'status2', 'occup', 'indus', 'q27salto', 'q28salpe')), by = c('uqnr', 'personnr')) %>% 
  mutate(q27salto = as.numeric(q27salto)) %>% 
  mutate(msal = case_when(
    q28salpe == "Per week" ~ q27salto*4.2,
    q28salpe == "Annually" ~ q27salto/12,
    q28salpe == "Per month" ~ q27salto,
    TRUE ~ NA_real_
  )) %>% 
  mutate(logsal = log(msal)) %>% 
  mutate(female = recode(gender, "Male" = 0L, "Female" = 1L)) %>% 
  mutate(educ = recode(q110hied,
                          "No schooling" = "0",
                          "Grade R/0" = "0",
                          "Grade 1/Sub A/Class 1" = "1",
                          "Sub A/Grade 1" = "1",
                          "Grade 2/Sub B/Class 2" = "2",
                          "Sub B/Grade 2" = "2",
                          "Grade 3/Standard 1/ABET/AET 1" = "3",
                          "Grade 3/Standard 1" = "3",
                          "Grade 4/Standard 2" = "4",
                          "Grade 5/Standard 3/ABET/AET 2" = "5",
                          "Grade 5/Standard 3" = "5",
                          "Grade 6/Standard 4" = "6",
                          "Grade 7/Standard 5" = "7",
                          "Grade 7/Standard 5/ABET/AET 3" = "7",
                          "Grade 8/Standard 6/Form 1" = "8",
                          "Grade 9/Standard 7/Form 2" = "9",
                          "Grade 9/Standard 7/Form 2/ABET/AET 4/NCV Level 1/Occupational Certificate-NQF Level 1" = "9",
                          "Grade 10/Standard 8/Form 3/NCV Level 2/Occupational Certificate-NQF Level 2" = "10",
                          "Grade 10/Standard 8/Form 3" = "10",
                          "Grade 11/Standard 9/Form 4" = "11",
                          "Grade 11/Standard 9/Form 4/NCV Level 3/Occupational Certificate-NQF Level 3" = "11",
                          "Grade 12/Standard 10/Form 5/National Senior Certificate/Matric/ NCV Level 4/Occupational Certificate-NQF Level 4" = "12",
                          "Grade 12/Standard 10/Form 5/Matric" = "12",
                          "NTC I" = "10",
                          "NTC l" = "10",
                          "NTC I/N1" = "10",
                          "NTC II" = "11",
                          "NTC II/N2" = "11",
                          "NTC III" = "11",
                          "NTC III/N3" = "11",
                          "N4/NTC 4/Occupational Certificate-NQF Level 5" = "13",
                          "N5/NTC 5/Occupational Certificate-NQF Level 5" = "13",
                          "N6/NTC 6/Occupational Certificate-NQF Level 5" = "13",
                          "Diploma/certificate with less than Grade 12/Std 10" = "11",
                          "Diploma with less than Grade 12/Standard 10" = "11",
                          "Certificate with less than Grade 12/Standard 10" = "11",
                          "Diploma/certificate with Grade 12/Std 10" = "13",
                          "Diploma with Grade 12/Standard 10/Occupational Certificate-NQF Level 6" = "14",
                          "Higher/National/Advance certificate with Grade 12/Std 10/Occupational Certificate-NQF Level 5" = "13",
                          "Higher Diploma/Occupational Certificate (B-Tech Diploma)-NQF Level 7" = "15",
                          "Degree" = "15",
                          "Bachelors Degree/Occupational Certificate-NQF Level 7" = "15",
                          "Postgraduate degree or diploma" = "16",
                          "Honours Degree/Postgraduate Diploma/Occupational Certificate-NQF Level 8" = "16",
                          "Post Higher Diploma (Masters Diploma and Masters Degree)-NQF Level 9" = "17",
                          "Doctoral Degrees (Doctoral Diploma and PhD)-NQF Level 10" = "18",
                          "Other" = "NA",
                          "Don't know" = "NA",
                          "Do not know" = "NA",
                          "Unspecified" = "NA")
         ) %>% 
  mutate(educ = as.numeric(educ)) %>% 
  mutate(age_sq = age^2)

#> 2003 data ----



# TEST REGRESSION ====

#> 2002 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2002) %>%
  summary()



