ingest <- function(df1, df2, age, salary, salperiod, educvar, recoding_table) {

    pooled <- df1 %>%
        filter(age > 14 & age < 65) %>%
        left_join(df2 %>% select(c('uqnr', 'personnr', 'status1', 'status2', 'occup', 'indus', 'salary', 'salperiod')), by = c('uqnr', 'personnr'))%>%
        mutate(salarynum = as.numeric(salary))%>%
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
        mutate(age_sq = age^2)

    return(pooled)}

pooled_2006_2 <- ingest(person_2006, worker_2006, age, salary, salperiod, educvar, recoding_table = recoding_educ_2006)


## rename the salary and education variables when reading in the data.
## see if it is necessary to adapt the ingest functions for the different years
## check education tables