#> 2002 to 2008 ----

ingest_2002_2008 <- function(df1, df2, age, salary, salary_categories_table, salperiod, educvar, recoding_table, incomecategory) {

    pooled <- df1 %>%
        filter(age > 14 & age < 65) %>%
        left_join(df2 %>% select(c('uqnr', 'personnr', 'status1', 'status2', 'occup', 'indus', 'salary', 'salperiod', 'incomecategory')), by = c('uqnr', 'personnr'))%>%
        mutate(salarynum = as.numeric(salary))%>%
        mutate(race = factor(race)) %>% 
        left_join(salary_categories_table, by = c("incomecategory")) %>%
        mutate(msal = case_when(
            salperiod == "Per week" ~ salarynum*4.2,
            salperiod == "Annually" ~ salarynum/12,
            salperiod == "Per month" ~ salarynum,
            TRUE ~ NA_real_
        ))%>%
        mutate(logsal = log(msal)) %>%
        mutate(female = recode(gender, "Male" = 0L, "Female" = 1L))%>%
        mutate(educ = dplyr::recode(educvar, !!!setNames(recoding_table$new_value, recoding_table$old_value)))%>%
        mutate(educ = as.numeric(educ)) %>%
        mutate(age_sq = age^2) %>%
        mutate(imputedsal = case_when(
            is.na(msal) ~ monthlymidpoint,
            TRUE ~ msal
        ))

    return(pooled)}

#test
pooled_2002_2 <- ingest_2002_2008(person_2002, worker_2002, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2002)

#> 2009 to 2014 ----

ingest_2009_2014 <- function(df1, age, salary, salary_categories_table, salperiod, educvar, recoding_table) {

    pooled <- df1 %>%
        filter(age > 14 & age < 65) %>%
        mutate(salarynum = as.numeric(salary)) %>%
        mutate(incomecategory = as.character(incomecategory)) %>% 
        mutate(race = factor(race)) %>% 
        left_join(salary_categories_table, by = c("incomecategory")) %>%
        mutate(msal = case_when(
            salperiod == "Per week" ~ salarynum*4.2,
            salperiod == "Annually" ~ salarynum/12,
            salperiod == "Per month" ~ salarynum,
            TRUE ~ NA_real_
        ))%>%
        mutate(logsal = log(msal)) %>%
        mutate(female = recode(gender, "Male" = 0L, "Female" = 1L))%>%
        mutate(educ = dplyr::recode(educvar, !!!setNames(recoding_table$new_value, recoding_table$old_value)))%>%
        mutate(educ = as.numeric(educ)) %>%
        mutate(age_sq = age^2) %>%
        mutate(imputedsal = case_when(
          is.na(msal) ~ monthlymidpoint,
          TRUE ~ msal
        ))

    return(pooled)}

pooled_2009_2 <- ingest_2009_2014(person_2009, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2009)

#> 2015 to 2018 ----

ingest_2015_2018 <- function(df1, age, salary, salperiod, salary_categories_table, educvar, recoding_table) {

    pooled <- df1 %>%
        filter(age > 14 & age < 65) %>%
        mutate(salarynum = as.numeric(salary)) %>%
        mutate(incomecategory = as.character(incomecategory)) %>% 
        mutate(race = factor(race)) %>% 
        left_join(salary_categories_table, by = c("incomecategory")) %>%
        mutate(msal = case_when(
            salperiod == 1 ~ salarynum*4.2,
            salperiod == 3 ~ salarynum/12,
            salperiod == 2 ~ salarynum,
            TRUE ~ NA_real_
        ))%>%
        mutate(logsal = log(msal)) %>%
        mutate(female = recode(gender, "Male" = 0L, "Female" = 1L))%>%
        mutate(educ = dplyr::recode(educvar, !!!setNames(recoding_table$new_value, recoding_table$old_value)))%>%
        mutate(educ = as.numeric(educ)) %>%
        mutate(age_sq = age^2)%>%
        mutate(imputedsal = case_when(
          is.na(msal) ~ monthlymidpoint,
          TRUE ~ msal
       ))

    return(pooled)}

pooled_2015_2 <- ingest_2015_2018(person_2015, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2015)

#> 2019 to 2021 ----

ingest_2019_2021 <- function(df1, age, salary, salperiod, salary_categories_table, educvar, recoding_table) {

    pooled <- df1 %>%
        filter(age > 14 & age < 65) %>%
        mutate(salarynum = as.numeric(salary)) %>%
        mutate(race = factor(race)) %>% 
        left_join(salary_categories_table, by = c("incomecategory")) %>%
        mutate(msal = case_when(
            salperiod == "Per week" ~ salarynum*4.2,
            salperiod == "Annually" ~ salarynum/12,
            salperiod == "Per month" ~ salarynum,
            TRUE ~ NA_real_
        ))%>%
        mutate(logsal = log(msal)) %>%
        mutate(female = recode(gender, "Male" = 0L, "Female" = 1L))%>%
        mutate(educ = dplyr::recode(educvar, !!!setNames(recoding_table$new_value, recoding_table$old_value)))%>%
        mutate(educ = as.numeric(educ)) %>%
        mutate(age_sq = age^2) %>%
        mutate(imputedsal = case_when(
          is.na(msal) ~ monthlymidpoint,
          TRUE ~ msal
       ))

    return(pooled)}

pooled_2019_2 <- ingest_2019_2021(person_2019, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2019)

