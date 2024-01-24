#> 2002 data ----

pooled_2002_1 <- person_2002 %>%
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
worker_2003$uqnr <- as.character(as.numeric(worker_2003$uqnr))
person_2003$uqnr <- as.character(as.numeric(person_2003$uqnr))

1011001002201

pooled_2003_1 <- person_2003 %>%
    filter(age > 14 & age < 65) %>%
    inner_join(worker_2003 %>% select(c('uqnr', 'personnr', 'status1', 'status2', 'occup', 'indus', 'q28salto', 'q29salpe')), by = c('uqnr', 'personnr')) %>%
    mutate(q28salto = as.numeric(q28salto)) %>%
    mutate(msal = case_when(
        q29salpe == "Per week" ~ q28salto*4.2,
        q29salpe == "Annually" ~ q28salto/12,
        q29salpe == "Per month" ~ q28salto,
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
                         "Diploma/Certificate with less than grade 12/STD 10 " = "11",
                         "Certificate with less than Grade 12/Standard 10" = "11",
                         "Diploma/certificate with Grade 12/Std 10" = "13",
                         "Diploma/Certificate with grade 12/STD 10" = "13",
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
                         "Other " = "NA",
                         "Don't know" = "NA",
                         "Do not know" = "NA",
                         "Unspecified" = "NA")
    ) %>%
    mutate(educ = as.numeric(educ)) %>%
    mutate(age_sq = age^2)

#> 2004 data ----

pooled_2004 <- person_2004 %>%
    filter(age > 14 & age < 65) %>%
    left_join(worker_2004 %>% select(c('uqnr', 'personnr', 'status1', 'status2', 'occup', 'indus', 'q28salto', 'q29salpe')), by = c('uqnr', 'personnr')) %>%
    mutate(q28salto = as.numeric(q28salto)) %>%
    mutate(age = as.numeric(age)) %>%
    mutate(msal = case_when(
        q29salpe == "Per week" ~ q28salto*4.2,
        q29salpe == "Annually" ~ q28salto/12,
        q29salpe == "Per month" ~ q28salto,
        TRUE ~ NA_real_
    )) %>%
    mutate(logsal = log(msal)) %>%
    mutate(female = recode(gender, "Male" = 0L, "Female" = 1L)) %>%
    mutate(educ = recode(q19hiedu,
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
                         "Diploma/Certificate with less than grade 12/STD 10 " = "11",
                         "Certificate with less than Grade 12/Standard 10" = "11",
                         "Diploma/certificate with Grade 12/Std 10" = "13",
                         "Diploma/Certificate with grade 12/STD 10" = "13",
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
                         "Other " = "NA",
                         "Don't know" = "NA",
                         "Do not know" = "NA",
                         "Unspecified" = "NA",
                         "Grade 7 / Standard 5" = "7",
                         "Grade 8 / Standard 6 / Form 1" = "8",
                         "Grade 10 / Standard 8 / Form 3" = "10",
                         "Grade 12 / Standard 10 / Form 5 / Matric" = "12",
                         "Grade 11 / Standard 9 / Form 4" = "11",
                         "Honors Degree" = "16",
                         "Higher Degree ( Masters, Doctorate)" = "18",
                         "Certificate with Grade 12 / Std 10" = "13",
                         "Grade 5 / Standard 3" = "5",
                         "Bachelors Degree" = "15",
                         "Bachelors Degree and Diploma" = "16",
                         "Grade 9 / Standard 7 / Form 2" = "9",
                         "Grade 6 / Standard 4" = "6",
                         "Grade 2 / Sub B" = "2",
                         "Grade 3 / Standard 1" = "3",
                         "Grade 4 / Standard 2" = "4",
                         "Diploma with Grade 12 / Std 10" = "13",
                         "Diploma with less than Grade 12 / Std 10" = "11",
                         "Certificate with less than Grade 12 / Std 10" = "11",
                         "Grade 1 / Sub A" ="1")
    ) %>%
    mutate(educ = as.numeric(educ)) %>%
    mutate(age_sq = age^2)

#> 2005 data ----

pooled_2005 <- person_2005 %>%
    filter(age > 14 & age < 65) %>%
    left_join(worker_2005 %>% select(c('uqnr', 'personnr', 'status1', 'status2', 'occup', 'indus', 'q29salto', 'q210salp')), by = c('uqnr', 'personnr')) %>%
    mutate(q29salto = as.numeric(q29salto)) %>%
    mutate(msal = case_when(
        q210salp == "Per week" ~ q29salto*4.2,
        q210salp == "Annually" ~ q29salto/12,
        q210salp == "Per month" ~ q29salto,
        TRUE ~ NA_real_
    )) %>%
    mutate(logsal = log(msal)) %>%
    mutate(female = recode(gender, "Male" = 0L, "Female" = 1L)) %>%
    mutate(educ = recode(q19hiedu,
                         "No schooling" = "0",
                         "Grade R / 0" = "0",
                         "Grade 1/Sub A/Class 1" = "1",
                         "Sub A/Grade 1" = "1",
                         "Grade 1 / Sub A" = "1",
                         "Grade 2/Sub B/Class 2" = "2",
                         "Sub B/Grade 2" = "2",
                         "Grade 2 / Sub B" = "2",
                         "Grade 3/Standard 1/ABET/AET 1" = "3",
                         "Grade 3 / Standard 1" = "3",
                         "Grade 4 / Standard 2" = "4",
                         "Grade 5/Standard 3/ABET/AET 2" = "5",
                         "Grade 5 / Standard 3" = "5",
                         "Grade 6 / Standard 4" = "6",
                         "Grade 7 / Standard 5" = "7",
                         "Grade 7/Standard 5/ABET/AET 3" = "7",
                         "Grade 8 / Standard 6 / Form 1" = "8",
                         "Grade 9 / Standard 7 / Form 2" = "9",
                         "Grade 9/Standard 7/Form 2/ABET/AET 4/NCV Level 1/Occupational Certificate-NQF Level 1" = "9",
                         "Grade 10/Standard 8/Form 3/NCV Level 2/Occupational Certificate-NQF Level 2" = "10",
                         "Grade 10 / Standard 8 / Form 3" = "10",
                         "Grade 11 / Standard 9 / Form 4" = "11",
                         "Grade 11/Standard 9/Form 4/NCV Level 3/Occupational Certificate-NQF Level 3" = "11",
                         "Grade 12/Standard 10/Form 5/National Senior Certificate/Matric/ NCV Level 4/Occupational Certificate-NQF Level 4" = "12",
                         "Grade 12 / Standard 10 / Form 5/Matric" = "12",
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
                         "Diploma with less than Grade 12 / Std 10" = "11",
                         "Diploma with less than Grade 12/Standard 10" = "11",
                         "Certificate with less than Grade 12/Standard 10" = "11",
                         "Diploma/certificate with Grade 12/Std 10" = "13",
                         "Certificate with Grade 12 / Std 10" = "13",
                         "Diploma with Grade 12 / Std 10" = "12",
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
                         "Unspecified" = "NA",
                         "Honours Degree" = "16",
                         "Certificate with less than grade 12/STD 10" = "11",
                         "Bachelors Degree" = "15",
                         "Bachelors Degree and Diploma" = "16",
                         "Higher Degree (Masters, Doctorate)" = "17",
                         "Diploma with less than grade 12 / STD 10" = "11",
                         "Diploma / certificate with less than Grade 12 / Std 10" = "11",
                         "Certificate with grade 12 / STD 10" = "13",
                         "Diploma with grade 12 / STD 10" = "13")
    ) %>%
    mutate(educ = as.numeric(educ)) %>%
    mutate(age_sq = age^2)

#> 2006 data ----

pooled_2006 <- person_2006 %>%
    filter(age > 14 & age < 65) %>%
    left_join(worker_2006 %>% select(c('uqnr', 'personnr', 'status1', 'status2', 'occup', 'indus', 'q29salto', 'q210salp')), by = c('uqnr', 'personnr')) %>%
    mutate(q29salto = as.numeric(q29salto)) %>%
    mutate(msal = case_when(
        q210salp == "Per week" ~ q29salto*4.2,
        q210salp == "Annually" ~ q29salto/12,
        q210salp == "Per month" ~ q29salto,
        TRUE ~ NA_real_
    )) %>%
    mutate(logsal = log(msal)) %>%
    mutate(female = recode(gender, "Male" = 0L, "Female" = 1L)) %>%
    mutate(educ = recode(q19hiedu,
                         "No schooling" = "0",
                         "Grade R / 0" = "0",
                         "Grade 1/Sub A/Class 1" = "1",
                         "Sub A/Grade 1" = "1",
                         "Grade 1 / Sub A" = "1",
                         "Grade 2/Sub B/Class 2" = "2",
                         "Sub B/Grade 2" = "2",
                         "Grade 2 / Sub B" = "2",
                         "Grade 3/Standard 1/ABET/AET 1" = "3",
                         "Grade 3 / Standard 1" = "3",
                         "Grade 4 / Standard 2" = "4",
                         "Grade 5/Standard 3/ABET/AET 2" = "5",
                         "Grade 5 / Standard 3" = "5",
                         "Grade 6 / Standard 4" = "6",
                         "Grade 7 / Standard 5" = "7",
                         "Grade 7/Standard 5/ABET/AET 3" = "7",
                         "Grade 8 / Standard 6 / Form 1" = "8",
                         "Grade 9 / Standard 7 / Form 2" = "9",
                         "Grade 9/Standard 7/Form 2/ABET/AET 4/NCV Level 1/Occupational Certificate-NQF Level 1" = "9",
                         "Grade 10/Standard 8/Form 3/NCV Level 2/Occupational Certificate-NQF Level 2" = "10",
                         "Grade 10 / Standard 8 / Form 3" = "10",
                         "Grade 11 / Standard 9 / Form 4" = "11",
                         "Grade 11/Standard 9/Form 4/NCV Level 3/Occupational Certificate-NQF Level 3" = "11",
                         "Grade 12/Standard 10/Form 5/National Senior Certificate/Matric/ NCV Level 4/Occupational Certificate-NQF Level 4" = "12",
                         "Grade 12 / Standard 10 / Form 5/Matric" = "12",
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
                         "Diploma with less than Grade 12 / Std 10" = "11",
                         "Diploma with less than Grade 12/Standard 10" = "11",
                         "Certificate with less than Grade 12/Standard 10" = "11",
                         "Diploma/certificate with Grade 12/Std 10" = "13",
                         "Certificate with Grade 12 / Std 10" = "13",
                         "Diploma with Grade 12 / Std 10" = "12",
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
                         "Unspecified" = "NA",
                         "Honours Degree" = "16",
                         "Certificate with less than grade 12/STD 10" = "11",
                         "Bachelors Degree" = "15",
                         "Bachelors Degree and Diploma" = "16",
                         "Higher Degree (Masters, Doctorate)" = "17",
                         "Diploma with less than grade 12 / STD 10" = "11",
                         "Diploma / certificate with less than Grade 12 / Std 10" = "11",
                         "Certificate with grade 12 / STD 10" = "13",
                         "Diploma with grade 12 / STD 10" = "13",
                         "Grade 7/Standard 5" = "7",
                         "Grade 8/Standard 6/Form 1" = "8",
                         "Grade 12/Standard 10/Form 5/Matric" = "12",
                         "Grade 10/Standard 8/Form 3" = "10",
                         "Grade 9/Standard 7/Form 2" = "9",
                         "Grade 6/Standard 4" = "6",
                         "Grade 11/Standard 9/Form 4" = "11",
                         "Diploma with grade 12/STD 10" = "13",
                         "Grade 3/Standard 1" = "3",
                         "Grade 4/Standard 2" = "4",
                         "Grade R/0" = "0",
                         "Grade 5/Standard 3" = "5",
                         "Bachelor's Degree and Diploma" = "16",
                         "Bachelor's Degree" = "15",
                         "Certificate with grade 12/STD 10" = "13",
                         "Diploma with less than grade 12/STD 10" = "11")
    ) %>%
    mutate(educ = as.numeric(educ)) %>%
    mutate(age_sq = age^2)

#> 2007 data ----

pooled_2007 <- person_2007 %>%
    filter(age > 14 & age < 65) %>%
    left_join(worker_2007 %>% select(c('uqnr', 'personnr', 'status1', 'status2', 'occup', 'indus', 'q29salto', 'q210salp')), by = c('uqnr', 'personnr')) %>%
    mutate(q29salto = as.numeric(q29salto)) %>%
    mutate(msal = case_when(
        q210salp == "Per week" ~ q29salto*4.2,
        q210salp == "Annually" ~ q29salto/12,
        q210salp == "Per month" ~ q29salto,
        TRUE ~ NA_real_
    )) %>%
    mutate(logsal = log(msal)) %>%
    mutate(female = recode(gender, "Male" = 0L, "Female" = 1L)) %>%
    mutate(educ = recode(q19hiedu,
                         "No schooling" = "0",
                         "Grade R / 0" = "0",
                         "Grade 1/Sub A/Class 1" = "1",
                         "Sub A/Grade 1" = "1",
                         "Grade 1 / Sub A" = "1",
                         "Grade 2/Sub B/Class 2" = "2",
                         "Sub B/Grade 2" = "2",
                         "Grade 2 / Sub B" = "2",
                         "Grade 3/Standard 1/ABET/AET 1" = "3",
                         "Grade 3 / Standard 1" = "3",
                         "Grade 4 / Standard 2" = "4",
                         "Grade 5/Standard 3/ABET/AET 2" = "5",
                         "Grade 5 / Standard 3" = "5",
                         "Grade 6 / Standard 4" = "6",
                         "Grade 7 / Standard 5" = "7",
                         "Grade 7/Standard 5/ABET/AET 3" = "7",
                         "Grade 8 / Standard 6 / Form 1" = "8",
                         "Grade 9 / Standard 7 / Form 2" = "9",
                         "Grade 9/Standard 7/Form 2/ABET/AET 4/NCV Level 1/Occupational Certificate-NQF Level 1" = "9",
                         "Grade 10/Standard 8/Form 3/NCV Level 2/Occupational Certificate-NQF Level 2" = "10",
                         "Grade 10 / Standard 8 / Form 3" = "10",
                         "Grade 11 / Standard 9 / Form 4" = "11",
                         "Grade 11/Standard 9/Form 4/NCV Level 3/Occupational Certificate-NQF Level 3" = "11",
                         "Grade 12/Standard 10/Form 5/National Senior Certificate/Matric/ NCV Level 4/Occupational Certificate-NQF Level 4" = "12",
                         "Grade 12 / Standard 10 / Form 5/Matric" = "12",
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
                         "Diploma with less than Grade 12 / Std 10" = "11",
                         "Diploma with less than Grade 12/Standard 10" = "11",
                         "Certificate with less than Grade 12/Standard 10" = "11",
                         "Diploma/certificate with Grade 12/Std 10" = "13",
                         "Certificate with Grade 12 / Std 10" = "13",
                         "Diploma with Grade 12 / Std 10" = "12",
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
                         "Unspecified" = "NA",
                         "Honours Degree" = "16",
                         "Certificate with less than grade 12/STD 10" = "11",
                         "Bachelors Degree" = "15",
                         "Bachelors Degree and Diploma" = "16",
                         "Higher Degree (Masters, Doctorate)" = "17",
                         "Diploma with less than grade 12 / STD 10" = "11",
                         "Diploma / certificate with less than Grade 12 / Std 10" = "11",
                         "Certificate with grade 12 / STD 10" = "13",
                         "Diploma with grade 12 / STD 10" = "13",
                         "Grade 7/Standard 5" = "7",
                         "Grade 12/Standard 10/Form 5/Matric" = "12",
                         "Grade 8/Standard 6/Form 1" = "8",
                         "Grade 9/Standard 7/Form 2" = "9",
                         "Diploma with grade 12/STD 10" = "13",
                         "Grade 6/Standard 4" = "6",
                         "Certificate with grade 12/STD 10" = "13",
                         "Grade 10/Standard 8/Form 3"  = "10",
                         "Grade 3/Standard 1" = "3",
                         "Grade 4/Standard 2"  = "4",
                         "Grade 11/Standard 9/Form 4" = "11",
                         "Grade 5/Standard 3" = "5",
                         "Diploma with less than grade 12/STD 10" = "11",
                         "Bachelor's Degree" = "15",
                         "Bachelor's Degree and Diploma" = "16",
                         "Grade R/0" = "0")
    ) %>%
    mutate(educ = as.numeric(educ)) %>%
    mutate(age_sq = age^2)

#> 2008 data ----
pooled_2008 <- person_2008 %>%
    filter(age > 14 & age < 65) %>%
    left_join(worker_2008 %>% select(c('uqnr', 'personnr', 'status1', 'status2', 'occup', 'indus', 'q29salto', 'q210salp')), by = c('uqnr', 'personnr')) %>%
    mutate(q29salto = as.numeric(q29salto)) %>%
    mutate(msal = case_when(
        q210salp == "Per week" ~ q29salto*4.2,
        q210salp == "Annually" ~ q29salto/12,
        q210salp == "Per month" ~ q29salto,
        TRUE ~ NA_real_
    )) %>%
    mutate(logsal = log(msal)) %>%
    mutate(female = recode(gender, "Male" = 0L, "Female" = 1L)) %>%
    mutate(educ = recode(q19hiedu,
                         "No schooling" = "0",
                         "Grade R / 0" = "0",
                         "Grade 1/Sub A/Class 1" = "1",
                         "Sub A/Grade 1" = "1",
                         "Grade 1 / Sub A" = "1",
                         "Grade 2/Sub B/Class 2" = "2",
                         "Sub B/Grade 2" = "2",
                         "Grade 2 / Sub B" = "2",
                         "Grade 3/Standard 1/ABET/AET 1" = "3",
                         "Grade 3 / Standard 1" = "3",
                         "Grade 4 / Standard 2" = "4",
                         "Grade 5/Standard 3/ABET/AET 2" = "5",
                         "Grade 5 / Standard 3" = "5",
                         "Grade 6 / Standard 4" = "6",
                         "Grade 7 / Standard 5" = "7",
                         "Grade 7/Standard 5/ABET/AET 3" = "7",
                         "Grade 8 / Standard 6 / Form 1" = "8",
                         "Grade 9 / Standard 7 / Form 2" = "9",
                         "Grade 9/Standard 7/Form 2/ABET/AET 4/NCV Level 1/Occupational Certificate-NQF Level 1" = "9",
                         "Grade 10/Standard 8/Form 3/NCV Level 2/Occupational Certificate-NQF Level 2" = "10",
                         "Grade 10 / Standard 8 / Form 3" = "10",
                         "Grade 11 / Standard 9 / Form 4" = "11",
                         "Grade 11/Standard 9/Form 4/NCV Level 3/Occupational Certificate-NQF Level 3" = "11",
                         "Grade 12/Standard 10/Form 5/National Senior Certificate/Matric/ NCV Level 4/Occupational Certificate-NQF Level 4" = "12",
                         "Grade 12 / Standard 10 / Form 5/Matric" = "12",
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
                         "Diploma with less than Grade 12 / Std 10" = "11",
                         "Diploma with less than Grade 12/Standard 10" = "11",
                         "Certificate with less than Grade 12/Standard 10" = "11",
                         "Diploma/certificate with Grade 12/Std 10" = "13",
                         "Certificate with Grade 12 / Std 10" = "13",
                         "Diploma with Grade 12 / Std 10" = "12",
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
                         "Unspecified" = "NA",
                         "Honours Degree" = "16",
                         "Certificate with less than grade 12/STD 10" = "11",
                         "Bachelors Degree" = "15",
                         "Bachelors Degree and Diploma" = "16",
                         "Higher Degree (Masters, Doctorate)" = "17",
                         "Diploma with less than grade 12 / STD 10" = "11",
                         "Diploma / certificate with less than Grade 12 / Std 10" = "11",
                         "Certificate with grade 12 / STD 10" = "13",
                         "Diploma with grade 12 / STD 10" = "13",
                         "Grade 7/Standard 5" = "7",
                         "Grade 12/Standard 10/Form 5/Matric" = "12",
                         "Grade 8/Standard 6/Form 1" = "8",
                         "Grade 9/Standard 7/Form 2" = "9",
                         "Diploma with grade 12/STD 10" = "13",
                         "Grade 6/Standard 4" = "6",
                         "Certificate with grade 12/STD 10" = "13",
                         "Grade 10/Standard 8/Form 3"  = "10",
                         "Grade 3/Standard 1" = "3",
                         "Grade 4/Standard 2"  = "4",
                         "Grade 11/Standard 9/Form 4" = "11",
                         "Grade 5/Standard 3" = "5",
                         "Diploma with less than grade 12/STD 10" = "11",
                         "Bachelor's Degree" = "15",
                         "Bachelor's Degree and Diploma" = "16",
                         "Grade R/0" = "0")
    ) %>%
    mutate(educ = as.numeric(educ)) %>%
    mutate(age_sq = age^2)


#> 2009 data ----
pooled_2009 <- person_2009 %>%
    filter(age > 14 & age < 65) %>%
    mutate(q141asto = as.numeric(q141asto)) %>%
    mutate(msal = case_when(
        q141bsp == "Per week" ~ q141asto*4.2,
        q141bsp == "Annually" ~ q141asto/12,
        q141bsp == "Per month" ~ q141asto,
        TRUE ~ NA_real_
    )) %>%
    mutate(logsal = log(msal)) %>%
    mutate(female = recode(gender, "Male" = 0L, "Female" = 1L)) %>%
    mutate(educ = recode(q16hiedu,
                         "No schooling" = "0",
                         "Grade R/0" = "0",
                         "Grade 1/ Sub A/Class 1" = "1",
                         "Grade 2 / Sub B/Class 2" = "2",
                         "Grade 3/Standard 1/ ABET 1(Kha Ri Gude, Sanli)" = "3",
                         "Grade 4/ Standard 2" = "4",
                         "Grade 5/ Standard 3/ ABET 2" = "5",
                         "Grade 6/Standard 4" = "6",
                         "Grade 7/Standard 5/ ABET 3" = "7",
                         "Grade 8/Standard 6/Form 1" = "8",
                         "Grade 9/Standard 7/Form 2/ ABET 4" = "9",
                         "Grade 10/ Standard 8/ Form 3" = "10",
                         "Grade 11/ Standard 9/ Form 4" = "11",
                         "Grade 12/Standard 10/Form 5/Matric (Exemption *)" = "12",
                         "Grade 12/Standard 10/Form 5/Matric (No Exemption" = "12",
                         "NTC 1/ N1/NC (V) Level 2" = "10",
                         "NTC 2/ N2/ NC (V) Level 3" = "11",
                         "NTC 3/ N3/NC (V)/Level 4" = "11",
                         "N4/NTC 4" = "13",
                         "N5/NTC 5" = "13",
                         "N6/NTC 6" = "13",
                         "Diploma with less than Grade 12/Std 10" = "11",
                         "Certificate with less than Grade 12/Std 10" = "11",
                         "Certificate with Grade 12/Std 10" = "13",
                         "Diploma with Grade 12/Std 10" = "13",
                         "Bachelors Degree" = "15",
                         "Bachelors Degree and post-graduate diploma" = "16",
                         "Honours Degree" = "16",
                         "Higher Diploma (Technikon/University of Technology)" = "13",
                         "Post Higher Diploma (Technikon/University of Technology Masters, Doctoral)" = "15",
                         "Doctoral Degrees (Doctoral Diploma and PhD)-NQF Level 10" = "18",
                         "Higher degree (Masters, Doctorate)" = "17",
                         "Other" = "NA",
                         "Do not know" = "NA",
                         "Unspecified" = "NA")
    ) %>%
    mutate(educ = as.numeric(educ)) %>%
    mutate(age_sq = age^2)

#> 2010 data ----
pooled_2010 <- person_2010 %>%
    filter(age > 14 & age < 65) %>%
    mutate(q22asto = as.numeric(q22asto)) %>%
    mutate(msal = case_when(
        q22bsp == "Per week" ~ q22asto*4.2,
        q22bsp == "Annually" ~ q22asto/12,
        q22bsp == "Per month" ~ q22asto,
        TRUE ~ NA_real_
    )) %>%
    mutate(logsal = log(msal)) %>%
    mutate(female = recode(gender, "Male" = 0L, "Female" = 1L)) %>%
    mutate(educ = recode(q16hiedu,
                         "No schooling" = "0",
                         "Grade R/0" = "0",
                         "Grade 1/ Sub A/Class 1" = "1",
                         "Grade 2 / Sub B/Class 2" = "2",
                         "Grade 3/Standard 1/ ABET 1(Kha Ri Gude, Sanli)" = "3",
                         "Grade 4/ Standard 2" = "4",
                         "Grade 5/ Standard 3/ ABET 2" = "5",
                         "Grade 6/Standard 4" = "6",
                         "Grade 7/Standard 5/ ABET 3" = "7",
                         "Grade 8/Standard 6/Form 1" = "8",
                         "Grade 9/Standard 7/Form 2/ ABET 4" = "9",
                         "Grade 10/ Standard 8/ Form 3" = "10",
                         "Grade 11/ Standard 9/ Form 4" = "11",
                         "Grade 12/Standard 10/Form 5/Matric (Exemption *)" = "12",
                         "Grade 12/Standard 10/Form 5/Matric (No Exemption" = "12",
                         "NTC 1/ N1/NC (V) Level 2" = "10",
                         "NTC 2/ N2/ NC (V) Level 3" = "11",
                         "NTC 3/ N3/NC (V)/Level 4" = "11",
                         "N4/NTC 4" = "13",
                         "N5/NTC 5" = "13",
                         "N6/NTC 6" = "13",
                         "Diploma with less than Grade 12/Std 10" = "11",
                         "Certificate with less than Grade 12/Std 10" = "11",
                         "Certificate with Grade 12/Std 10" = "13",
                         "Diploma with Grade 12/Std 10" = "13",
                         "Bachelors Degree" = "15",
                         "Bachelors Degree and post-graduate diploma" = "16",
                         "Honours Degree" = "16",
                         "Higher Diploma (Technikon/University of Technology)" = "13",
                         "Post Higher Diploma (Technikon/University of Technology Masters, Doctoral)" = "15",
                         "Doctoral Degrees (Doctoral Diploma and PhD)-NQF Level 10" = "18",
                         "Higher degree (Masters, Doctorate)" = "17",
                         "Other" = "NA",
                         "Do not know" = "NA",
                         "Unspecified" = "NA")
    ) %>%
    mutate(educ = as.numeric(educ)) %>%
    mutate(age_sq = age^2)

#> 2011 data ----

pooled_2011 <- person_2011 %>%
    filter(age > 14 & age < 65) %>%
    mutate(q22asto = as.numeric(q22asto)) %>%
    mutate(msal = case_when(
        q22bsp == "Per week" ~ q22asto*4.2,
        q22bsp == "Annually" ~ q22asto/12,
        q22bsp == "Per month" ~ q22asto,
        TRUE ~ NA_real_
    )) %>%
    mutate(logsal = log(msal)) %>%
    mutate(female = recode(gender, "Male" = 0L, "Female" = 1L)) %>%
    mutate(educ = recode(q16hiedu,
                         "NTC 3/ N3/NC (V)/Level 4" = "11",
                         "Grade 9/Standard 7/Form 2/AET 4" = "9",
                         "Grade 6/Standard 4" = "6",
                         "Grade 2/Sub B/Class 2" = "2",
                         "N6/NTC 6" = "13",
                         "Grade 12/Standard 10/Form 5/Matric (No Exemption)" = "12",
                         "Grade 8/Standard 6/Form 1" = "8",
                         "Diploma with Grade 12/Std 10" = "13",
                         "Grade 11/Standard 9/ Form 4" = "11",
                         "Bachelor's Degree and post-graduate diploma" ="16",
                         "No schooling" = "0",
                         "Grade 5/Standard 3/AET 2" = "5",
                         "Grade 10/Standard 8/ Form 3" = "10",
                         "Grade 4/Standard 2" = "4",
                         "Grade 7/Standard 5/AET 3" = "7",
                         "Grade 3/Standard 1/AET 1(Kha Ri Gude, Sanli)" = "3",
                         "Bachelor's Degree" = "15",
                         "Higher Diploma (Technikon/University of Technology)" = "15",
                         "Grade 12/Standard 10/Form 5/Matric (Exemption *)" = "12",
                         "Do not know" = "0",
                         "Diploma with less than Grade 12/Std 10" = "11",
                         "Grade 1/Sub A/Class 1" = "1",
                         "Certificate with less than Grade 12/Std 10" = "11",
                         "Certificate with Grade 12/Std 10" = "13",
                         "NTC 1 N1/NC (V) Level 2" = "10",
                         "Unspecified" = "0",
                         "Post Higher Diploma (Technikon/University of Technology Masters, Doctoral)" = "17",
                         "Other (specify in the box below)" = "0",
                         "Honours Degree" = "16",
                         "N4/NTC 4" = "13",
                         "Higher degree (Masters, Doctorate)" = "17",
                         "Grade R/0" = "0",
                         "NTC 2/N2/ NC (V) Level 3" = "11",
                         "N5/NTC 5" = "13",
                         "Grade 5/ Standard 3/ABET 2" = "5",
                         "Grade 9/Standard 7/Form 2/ABET 4" = "9",
                         "Grade 4/ Standard 2" = "4",
                         "Bachelors Degree" = "15",
                         "Grade 3/Standard 1/ABET 1(Kha Ri Gude; Sanli)" = "3",
                         "Grade 2 / Sub B/Class 2" = "2",
                         "Grade 10/ Standard 8/ Form 3" = "10",
                         "Grade 7/Standard 5/ABET 3" = "7",
                         "Grade 11/ Standard 9/ Form 4" = "11",
                         "NTC 1/ N1/NC (V) Level 2" = "10",
                         "Bachelors Degree and post-graduate diploma" = "16",
                         "Grade 1/ Sub A/Class 1" = "1",
                         "Post Higher Diploma (Technikon/University of Technology Masters; Doctoral)" = "17",
                         "Higher degree (Masters; Doctorate)" = "18",
                         "NTC 2/ N2/ NC (V) Level 3" = "11",
                         "Other" = "NA")
    ) %>%
    mutate(educ = as.numeric(educ)) %>%
    mutate(age_sq = age^2)

#> 2012 data ----
pooled_2012 <- person_2012 %>%
    filter(age > 14 & age < 65) %>%
    mutate(q22asto = as.numeric(q22asto)) %>%
    mutate(msal = case_when(
        q22bsp == "Per week" ~ q22asto*4.2,
        q22bsp == "Annually" ~ q22asto/12,
        q22bsp == "Per month" ~ q22asto,
        TRUE ~ NA_real_
    )) %>%
    mutate(logsal = log(msal)) %>%
    mutate(female = recode(gender, "Male" = 0L, "Female" = 1L)) %>%
    mutate(educ = recode(q16hiedu,
                         "NTC 3/ N3/NC (V)/Level 4" = "11",
                         "Grade 9/Standard 7/Form 2/AET 4" = "9",
                         "Grade 6/Standard 4" = "6",
                         "Grade 2/Sub B/Class 2" = "2",
                         "N6/NTC 6" = "13",
                         "Grade 12/Standard 10/Form 5/Matric (No Exemption)" = "12",
                         "Grade 8/Standard 6/Form 1" = "8",
                         "Diploma with Grade 12/Std 10" = "13",
                         "Grade 11/Standard 9/ Form 4" = "11",
                         "Bachelor's Degree and post-graduate diploma" ="16",
                         "No schooling" = "0",
                         "Grade 5/Standard 3/AET 2" = "5",
                         "Grade 10/Standard 8/ Form 3" = "10",
                         "Grade 4/Standard 2" = "4",
                         "Grade 7/Standard 5/AET 3" = "7",
                         "Grade 3/Standard 1/AET 1(Kha Ri Gude, Sanli)" = "3",
                         "Bachelor's Degree" = "15",
                         "Higher Diploma (Technikon/University of Technology)" = "15",
                         "Grade 12/Standard 10/Form 5/Matric (Exemption *)" = "12",
                         "Do not know" = "0",
                         "Diploma with less than Grade 12/Std 10" = "11",
                         "Grade 1/Sub A/Class 1" = "1",
                         "Certificate with less than Grade 12/Std 10" = "11",
                         "Certificate with Grade 12/Std 10" = "13",
                         "NTC 1 N1/NC (V) Level 2" = "10",
                         "Unspecified" = "0",
                         "Post Higher Diploma (Technikon/University of Technology Masters, Doctoral)" = "17",
                         "Other (specify in the box below)" = "0",
                         "Honours Degree" = "16",
                         "N4/NTC 4" = "13",
                         "Higher degree (Masters, Doctorate)" = "17",
                         "Grade R/0" = "0",
                         "NTC 2/N2/ NC (V) Level 3" = "11",
                         "N5/NTC 5" = "13",
                         "Grade 5/ Standard 3/ABET 2" = "5",
                         "Grade 9/Standard 7/Form 2/ABET 4" = "9",
                         "Grade 4/ Standard 2" = "4",
                         "Bachelors Degree" = "15",
                         "Grade 3/Standard 1/ABET 1(Kha Ri Gude; Sanli)" = "3",
                         "Grade 2 / Sub B/Class 2" = "2",
                         "Grade 10/ Standard 8/ Form 3" = "10",
                         "Grade 7/Standard 5/ABET 3" = "7",
                         "Grade 11/ Standard 9/ Form 4" = "11",
                         "NTC 1/ N1/NC (V) Level 2" = "10",
                         "Bachelors Degree and post-graduate diploma" = "16",
                         "Grade 1/ Sub A/Class 1" = "1",
                         "Post Higher Diploma (Technikon/University of Technology Masters; Doctoral)" = "17",
                         "Higher degree (Masters; Doctorate)" = "18",
                         "NTC 2/ N2/ NC (V) Level 3" = "11",
                         "Other" = "NA")
    ) %>%
    mutate(educ = as.numeric(educ)) %>%
    mutate(age_sq = age^2)

#> 2013 data ----
pooled_2013 <- person_2013 %>%
    filter(age > 14 & age < 65) %>%
    mutate(q42asto = as.numeric(q42asto)) %>%
    mutate(msal = case_when(
        q42bsp == "Per week" ~ q42asto*4.2,
        q42bsp == "Annually" ~ q42asto/12,
        q42bsp == "Per month" ~ q42asto,
        TRUE ~ NA_real_
    )) %>%
    mutate(logsal = log(msal)) %>%
    mutate(female = recode(gender, "Male" = 0L, "Female" = 1L)) %>%
    mutate(educ = recode(q16hiedu,
                         "NTC 3/ N3/NC (V)/Level 4" = "11",
                         "Grade 9/Standard 7/Form 2/AET 4" = "9",
                         "Grade 6/Standard 4" = "6",
                         "Grade 2/Sub B/Class 2" = "2",
                         "N6/NTC 6" = "13",
                         "Grade 12/Standard 10/Form 5/Matric (No Exemption)" = "12",
                         "Grade 8/Standard 6/Form 1" = "8",
                         "Diploma with Grade 12/Std 10" = "13",
                         "Grade 11/Standard 9/ Form 4" = "11",
                         "Bachelor's Degree and post-graduate diploma" ="16",
                         "No schooling" = "0",
                         "Grade 5/Standard 3/AET 2" = "5",
                         "Grade 10/Standard 8/ Form 3" = "10",
                         "Grade 4/Standard 2" = "4",
                         "Grade 7/Standard 5/AET 3" = "7",
                         "Grade 3/Standard 1/AET 1(Kha Ri Gude, Sanli)" = "3",
                         "Bachelor's Degree" = "15",
                         "Higher Diploma (Technikon/University of Technology)" = "15",
                         "Grade 12/Standard 10/Form 5/Matric (Exemption *)" = "12",
                         "Do not know" = "0",
                         "Diploma with less than Grade 12/Std 10" = "11",
                         "Grade 1/Sub A/Class 1" = "1",
                         "Certificate with less than Grade 12/Std 10" = "11",
                         "Certificate with Grade 12/Std 10" = "13",
                         "NTC 1 N1/NC (V) Level 2" = "10",
                         "Unspecified" = "0",
                         "Post Higher Diploma (Technikon/University of Technology Masters, Doctoral)" = "17",
                         "Other (specify in the box below)" = "0",
                         "Honours Degree" = "16",
                         "N4/NTC 4" = "13",
                         "Higher degree (Masters, Doctorate)" = "17",
                         "Grade R/0" = "0",
                         "NTC 2/N2/ NC (V) Level 3" = "11",
                         "N5/NTC 5" = "13")
    ) %>%
    mutate(educ = as.numeric(educ)) %>%
    mutate(age_sq = age^2)


#> 2014 data ----
pooled_2014 <- person_2014 %>%
    filter(age > 14 & age < 65) %>%
    mutate(q42asto = as.numeric(q42asto)) %>%
    mutate(msal = case_when(
        q42bsp == "1" ~ q42asto*4.2,
        q42bsp == "3" ~ q42asto/12,
        q42bsp == "2" ~ q42asto,
        TRUE ~ NA_real_
    )) %>%
    mutate(logsal = log(msal)) %>%
    mutate(female = recode(gender, "1" = 0L, "2" = 1L)) %>%
    mutate(educ = recode(q15hiedu,
                         "98" = "0",
                         "00" = "0",
                         "01" = "1",
                         "02" = "2",
                         "03" = "3",
                         "04" = "4",
                         "05" = "5",
                         "06" = "6",
                         "07" = "7",
                         "08" = "8",
                         "09" = "9",
                         "10" = "10",
                         "11" = "11",
                         "12" = "12",
                         "13" = "12",
                         "14" = "10",
                         "15" = "11",
                         "16" = "11",
                         "17" = "13",
                         "18" = "13",
                         "19" = "13",
                         "20" = "11",
                         "21" = "11",
                         "22" = "13",
                         "23" = "13",
                         "24" = "15",
                         "25" = "17",
                         "26" = "15",
                         "27" = "16",
                         "28" = "16",
                         "29" = "17",
                         "30" = "NA",
                         "31" = "NA")
    ) %>%
    mutate(educ = as.numeric(educ)) %>%
    mutate(age_sq = age^2)

#> 2015 data ----
pooled_2015 <- person_2015 %>%
    filter(age > 14 & age < 65) %>%
    mutate(q42asto = as.numeric(q42asto)) %>%
    mutate(msal = case_when(
        q42bsp == "1" ~ q42asto*4.2,
        q42bsp == "3" ~ q42asto/12,
        q42bsp == "2" ~ q42asto,
        TRUE ~ NA_real_
    )) %>%
    mutate(logsal = log(msal)) %>%
    mutate(female = recode(gender, "1" = 0L, "2" = 1L)) %>%
    mutate(educ = recode(q15hiedu,
                         "98" = "0",
                         "00" = "0",
                         "01" = "1",
                         "02" = "2",
                         "03" = "3",
                         "04" = "4",
                         "05" = "5",
                         "06" = "6",
                         "07" = "7",
                         "08" = "8",
                         "09" = "9",
                         "10" = "10",
                         "11" = "11",
                         "12" = "12",
                         "13" = "12",
                         "14" = "10",
                         "15" = "11",
                         "16" = "11",
                         "17" = "13",
                         "18" = "13",
                         "19" = "13",
                         "20" = "11",
                         "21" = "11",
                         "22" = "13",
                         "23" = "13",
                         "24" = "15",
                         "25" = "17",
                         "26" = "15",
                         "27" = "16",
                         "28" = "16",
                         "29" = "17",
                         "30" = "NA",
                         "31" = "NA")
    ) %>%
    mutate(educ = as.numeric(educ)) %>%
    mutate(age_sq = age^2)

#> 2016 data ----
pooled_2016 <- person_2016 %>%
    filter(age > 14 & age < 65) %>%
    mutate(q42asto = as.numeric(q42asto)) %>%
    mutate(msal = case_when(
        q42bsp == "1" ~ q42asto*4.2,
        q42bsp == "3" ~ q42asto/12,
        q42bsp == "2" ~ q42asto,
        TRUE ~ NA_real_
    )) %>%
    mutate(logsal = log(msal)) %>%
    mutate(female = recode(gender, "1" = 0L, "2" = 1L)) %>%
    mutate(educ = recode(q15hiedu,
                         "0" = "0",
                         "1" = "1",
                         "2" = "2",
                         "3" = "3",
                         "4" = "4",
                         "5" = "5",
                         "6" = "6",
                         "7" = "7",
                         "8" = "8",
                         "9" = "9",
                         "10" = "10",
                         "11" = "11",
                         "12" = "12",
                         "13" = "12",
                         "14" = "10",
                         "15" = "11",
                         "16" = "11",
                         "17" = "13",
                         "18" = "13",
                         "19" = "13",
                         "20" = "11",
                         "21" = "11",
                         "22" = "13",
                         "23" = "13",
                         "24" = "13",
                         "25" = "15",
                         "26" = "15",
                         "27" = "15",
                         "28" = "16",
                         "29" = "17",
                         "30" = "NA",
                         "31" = "NA",
                         "98" = "0",
                         "99" = "NA")
    ) %>%
    mutate(educ = as.numeric(educ)) %>%
    mutate(age_sq = age^2)

#> 2017 data ----
pooled_2017 <- person_2017 %>%
    filter(age > 14 & age < 65) %>%
    mutate(q42asto = as.numeric(q42asto)) %>%
    mutate(msal = case_when(
        q42bsp == "1" ~ q42asto*4.2,
        q42bsp == "3" ~ q42asto/12,
        q42bsp == "2" ~ q42asto,
        TRUE ~ NA_real_
    )) %>%
    mutate(logsal = log(msal)) %>%
    mutate(female = recode(gender, "1" = 0L, "2" = 1L)) %>%
    mutate(educ = recode(q15hiedu,
                         "0" = "0",
                         "1" = "1",
                         "2" = "2",
                         "3" = "3",
                         "4" = "4",
                         "5" = "5",
                         "6" = "6",
                         "7" = "7",
                         "8" = "8",
                         "9" = "9",
                         "10" = "10",
                         "11" = "11",
                         "12" = "12",
                         "13" = "10",
                         "14" = "11",
                         "15" = "11",
                         "16" = "13",
                         "17" = "13",
                         "18" = "13",
                         "19" = "11",
                         "20" = "11",
                         "21" = "13",
                         "22" = "13",
                         "23" = "15",
                         "24" = "17",
                         "25" = "15",
                         "26" = "16",
                         "27" = "18",
                         "28" = "NA",
                         "29" = "NA",
                         "98" = "0",
                         "99" = "NA")
    ) %>%
    mutate(educ = as.numeric(educ)) %>%
    mutate(age_sq = age^2)

#> 2018 data ----
pooled_2018 <- person_2018 %>%
    filter(age > 14 & age < 65) %>%
    mutate(q42asto = as.numeric(q42asto)) %>%
    mutate(msal = case_when(
        q42bsp == "1" ~ q42asto*4.2,
        q42bsp == "3" ~ q42asto/12,
        q42bsp == "2" ~ q42asto,
        TRUE ~ NA_real_
    )) %>%
    mutate(logsal = log(msal)) %>%
    mutate(female = recode(gender, "1" = 0L, "2" = 1L)) %>%
    mutate(educ = recode(q15hiedu,
                         "0" = "0",
                         "1" = "1",
                         "2" = "2",
                         "3" = "3",
                         "4" = "4",
                         "5" = "5",
                         "6" = "6",
                         "7" = "7",
                         "8" = "8",
                         "9" = "9",
                         "10" = "10",
                         "11" = "11",
                         "12" = "12",
                         "13" = "10",
                         "14" = "11",
                         "15" = "11",
                         "16" = "13",
                         "17" = "13",
                         "18" = "13",
                         "19" = "11",
                         "20" = "11",
                         "21" = "13",
                         "22" = "13",
                         "23" = "15",
                         "24" = "17",
                         "25" = "15",
                         "26" = "16",
                         "27" = "18",
                         "28" = "NA",
                         "29" = "NA",
                         "98" = "0",
                         "99" = "NA")
    ) %>%
    mutate(educ = as.numeric(educ)) %>%
    mutate(age_sq = age^2)

#> 2019 data ----
pooled_2019 <- person_2019 %>%
    filter(age > 14 & age < 65) %>%
    mutate(lab_sto = as.numeric(lab_sto)) %>%
    mutate(msal = case_when(
        lab_salper == "Per week" ~ lab_sto*4.2,
        lab_salper == "Annually" ~ lab_sto/12,
        lab_salper == "Per month" ~ lab_sto,
        TRUE ~ NA_real_
    )) %>%
    mutate(logsal = log(msal)) %>%
    mutate(female = recode(sex, "Male" = 0L, "Female" = 1L)) %>%
    mutate(educ = recode(education,
                         "NTC 3/ N3/NC (V)/Level 4" = "11",
                         "Grade 9/Standard 7/Form 2/AET 4" = "9",
                         "Grade 6/Standard 4" = "6",
                         "Grade 2/Sub B/Class 2" = "2",
                         "N6/NTC 6" = "13",
                         "Grade 12/Standard 10/Form 5/Matric (No Exemption)" = "12",
                         "Grade 8/Standard 6/Form 1" = "8",
                         "Diploma with Grade 12/Std 10" = "13",
                         "Grade 11/Standard 9/ Form 4" = "11",
                         "Bachelor's Degree and post-graduate diploma" ="16",
                         "No schooling" = "0",
                         "Grade 5/Standard 3/AET 2" = "5",
                         "Grade 10/Standard 8/ Form 3" = "10",
                         "Grade 4/Standard 2" = "4",
                         "Grade 7/Standard 5/AET 3" = "7",
                         "Grade 3/Standard 1/AET 1(Kha Ri Gude, Sanli)" = "3",
                         "Bachelor's Degree" = "15",
                         "Higher Diploma (Technikon/University of Technology)" = "15",
                         "Grade 12/Standard 10/Form 5/Matric (Exemption *)" = "12",
                         "Do not know" = "0",
                         "Diploma with less than Grade 12/Std 10" = "11",
                         "Grade 1/Sub A/Class 1" = "1",
                         "Certificate with less than Grade 12/Std 10" = "11",
                         "Certificate with Grade 12/Std 10" = "13",
                         "NTC 1 N1/NC (V) Level 2" = "10",
                         "Unspecified" = "0",
                         "Post Higher Diploma (Technikon/University of Technology Masters, Doctoral)" = "17",
                         "Other (specify in the box below)" = "0",
                         "Honours Degree" = "16",
                         "N4/NTC 4" = "13",
                         "Higher degree (Masters, Doctorate)" = "17",
                         "Grade R/0" = "0",
                         "NTC 2/N2/ NC (V) Level 3" = "11",
                         "N5/NTC 5" = "13",
                         "Grade 5/ Standard 3/ABET 2" = "5",
                         "Grade 9/Standard 7/Form 2/ABET 4" = "9",
                         "Grade 4/ Standard 2" = "4",
                         "Bachelors Degree" = "15",
                         "Grade 3/Standard 1/ABET 1(Kha Ri Gude; Sanli)" = "3",
                         "Grade 2 / Sub B/Class 2" = "2",
                         "Grade 10/ Standard 8/ Form 3" = "10",
                         "Grade 7/Standard 5/ABET 3" = "7",
                         "Grade 11/ Standard 9/ Form 4" = "11",
                         "NTC 1/ N1/NC (V) Level 2" = "10",
                         "Bachelors Degree and post-graduate diploma" = "16",
                         "Grade 1/ Sub A/Class 1" = "1",
                         "Post Higher Diploma (Technikon/University of Technology Masters; Doctoral)" = "17",
                         "Higher degree (Masters; Doctorate)" = "18",
                         "NTC 2/ N2/ NC (V) Level 3" = "11",
                         "Other" = "NA",
                         "Grade 10/Standard 8/Form 3/NCV Level 2/Occupational Certificate \x96 NQF Level 2" = "10",
                         "Grade 7/Standard 5/ABET/AET 3" = "7",
                         "Grade 12/Standard 10/Form 5/National Senior Certificate/Matric/ NCV Level 4/Occupational Certificate \x96 NQF Level 4" = "12",
                         "Grade 9/Standard 7/Form 2/ABET/AET 4/NCV Level 1/Occupational Certificate \x96 NQF Level 1" = "9",
                         "Bachelor\x92s Degree/Occupational Certificate \x96 NQF Level 7" = "15",
                         "N6/NTC 6/Occupational Certificate \x96 NQF Level 5" = "13",
                         "NTC lll/N3/NQF 3" = "11",
                         "Grade 11/Standard 9/Form 4/NCV Level 3/Occupational Certificate \x96 NQF Level 3" = "11",
                         "NTC l/N1/NQF 1" = "10",
                         "Grade 5/Standard 3/ABET/AET 2" = "5",
                         "Diploma with Grade 12/Standard 10/Occupational Certificate \x96 NQF Level 6" = "13",
                         "Honours Degree/Postgraduate Diploma/Occupational Certificate \x96 NQF Level 8" = "16",
                         "N4/NTC 4/Occupational Certificate \x96 NQF Level 5" = "13",
                         "Certificate with less than Grade 12/Standard 10" = "11",
                         "Grade 3/Standard 1/ABET/AET 1" = "3",
                         "DO NOT KNOW" = "NA",
                         "Higher/National/Advance certificate with Grade 12/Std 10/Occupational Certificate \x96 NQF Level 5" = "13",
                         "Higher Diploma/Occupational Certificate (B-Tech Diploma) \x96 NQF Level 7" = "15",
                         "Post Higher Diploma (M-Tech and Master's Degree) \x96 NQF Level 9" = "17",
                         "Diploma with less than Grade 12/Standard 10" = "11",
                         "N5/NTC 5/Occupational Certificate \x96 NQF Level 5" = "13",
                         "NTC ll/N2/NQF 2" = "11",
                         "Doctoral Degrees (D-Tech and PhD) \x96 NQF Level 10" = "18")
    ) %>%
    mutate(educ = as.numeric(educ)) %>%
    mutate(age_sq = age^2)

#> 2020 data ----
pooled_2020 <- person_2020 %>%
    filter(age > 14 & age < 65) %>%
    mutate(lab_sto = as.numeric(lab_sto)) %>%
    mutate(msal = case_when(
        lab_salper == "Per week" ~ lab_sto*4.2,
        lab_salper == "Annually" ~ lab_sto/12,
        lab_salper == "Per month" ~ lab_sto,
        TRUE ~ NA_real_
    )) %>%
    mutate(logsal = log(msal)) %>%
    mutate(female = recode(sex, "Male" = 0L, "Female" = 1L)) %>%
    mutate(educ = recode(education,
                         "Grade 12/Standard 10/Form 5/National Senior Certificate/Matric/ NCV Level 4/Occupational Certificate \x96 NQF Level 4" = "12",
                         "Grade 9/Standard 7/Form 2/ABET/AET 4/NCV Level 1/Occupational Certificate \x96 NQF Level 1" = "9",
                         "Grade 7/Standard 5/ABET/AET 3" = "7",
                         "Grade 10/Standard 8/Form 3/NCV Level 2/Occupational Certificate \x96 NQF Level 2" = "10",
                         "Bachelor\x92s Degree/Occupational Certificate \x96 NQF Level 7" = "15",
                         "Diploma with Grade 12/Standard 10/Occupational Certificate \x96 NQF Level 6" = "13",
                         "Grade 4/Standard 2" = "4",
                         "Grade 5/Standard 3/ABET/AET 2" = "5",
                         "Grade 11/Standard 9/Form 4/NCV Level 3/Occupational Certificate \x96 NQF Level 3" = "11",
                         "Grade 6/Standard 4" = "6",
                         "Grade 8/Standard 6/Form 1" = "8",
                         "No schooling" = "0",
                         "Grade 3/Standard 1/ABET/AET 1" = "3",
                         "Higher Diploma/Occupational Certificate (B-Tech Diploma) \x96 NQF Level 7" = "15",
                         "N6/NTC 6/Occupational Certificate \x96 NQF Level 5" = "13",
                         "DO NOT KNOW" = "NA",
                         "Higher/National/Advance certificate with Grade 12/Std 10/Occupational Certificate \x96 NQF Level 5" = "13",
                         "Honours Degree/Postgraduate Diploma/Occupational Certificate \x96 NQF Level 8" = "16",
                         "N5/NTC 5/Occupational Certificate \x96 NQF Level 5" = "13",
                         "Doctoral Degrees (D-Tech and PhD) \x96 NQF Level 10" = "18",
                         "Other" = "NA",
                         "Grade 2/Sub B/Class 2" = "2",
                         "Certificate with less than Grade 12/Standard 10" = "10",
                         "N4/NTC 4/Occupational Certificate \x96 NQF Level 5" = "13",
                         "Grade 1/Sub A/Class 1" = "1",
                         "NTC lll/N3/NQF 3" = "11",
                         "Post Higher Diploma (M-Tech and Master's Degree) \x96 NQF Level 9" = "17",
                         "NTC ll/N2/NQF 2" = "11",
                         "Diploma with less than Grade 12/Standard 10" = "10")
    ) %>%
    mutate(educ = as.numeric(educ)) %>%
    mutate(age_sq = age^2)


#> 2021 data ----
pooled_2021 <- person_2021 %>%
    filter(age > 14 & age < 65) %>%
    mutate(lab_sto = as.numeric(lab_sto)) %>%
    mutate(msal = case_when(
        lab_salper == "Per week" ~ lab_sto*4.2,
        lab_salper == "Annually" ~ lab_sto/12,
        lab_salper == "Per month" ~ lab_sto,
        TRUE ~ NA_real_
    )) %>%
    mutate(logsal = log(msal)) %>%
    mutate(female = recode(sex, "Male" = 0L, "Female" = 1L)) %>%
    mutate(educ = recode(education,
                         "Grade 12/Standard 10/Form 5/National Senior Certificate/Matric/ NCV Level 4/Occupational Certificate \x96 NQF Level 4" = "12",
                         "Grade 9/Standard 7/Form 2/ABET/AET 4/NCV Level 1/Occupational Certificate \x96 NQF Level 1" = "9",
                         "Grade 7/Standard 5/ABET/AET 3" = "7",
                         "Grade 10/Standard 8/Form 3/NCV Level 2/Occupational Certificate \x96 NQF Level 2" = "10",
                         "Bachelor\x92s Degree/Occupational Certificate \x96 NQF Level 7" = "15",
                         "Diploma with Grade 12/Standard 10/Occupational Certificate \x96 NQF Level 6" = "13",
                         "Grade 4/Standard 2" = "4",
                         "Grade 5/Standard 3/ABET/AET 2" = "5",
                         "Grade 11/Standard 9/Form 4/NCV Level 3/Occupational Certificate \x96 NQF Level 3" = "11",
                         "Grade 6/Standard 4" = "6",
                         "Grade 8/Standard 6/Form 1" = "8",
                         "No schooling" = "0",
                         "Grade 3/Standard 1/ABET/AET 1" = "3",
                         "Higher Diploma/Occupational Certificate (B-Tech Diploma) \x96 NQF Level 7" = "15",
                         "N6/NTC 6/Occupational Certificate \x96 NQF Level 5" = "13",
                         "DO NOT KNOW" = "NA",
                         "Higher/National/Advance certificate with Grade 12/Std 10/Occupational Certificate \x96 NQF Level 5" = "13",
                         "Honours Degree/Postgraduate Diploma/Occupational Certificate \x96 NQF Level 8" = "16",
                         "N5/NTC 5/Occupational Certificate \x96 NQF Level 5" = "13",
                         "Doctoral Degrees (D-Tech and PhD) \x96 NQF Level 10" = "18",
                         "Other" = "NA",
                         "Grade 2/Sub B/Class 2" = "2",
                         "Certificate with less than Grade 12/Standard 10" = "10",
                         "N4/NTC 4/Occupational Certificate \x96 NQF Level 5" = "13",
                         "Grade 1/Sub A/Class 1" = "1",
                         "NTC lll/N3/NQF 3" = "11",
                         "Post Higher Diploma (M-Tech and Master's Degree) \x96 NQF Level 9" = "17",
                         "NTC ll/N2/NQF 2" = "11",
                         "Diploma with less than Grade 12/Standard 10" = "10",
                         "Grade R/0" = "0",
                         "NTC l/N1/NQF 1" = "10")
    ) %>%
    mutate(educ = as.numeric(educ)) %>%
    mutate(age_sq = age^2)
