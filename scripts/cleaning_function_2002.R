# Cleaning function

clean_2002 <- function(df1, df2, recoding_table) {

    df1 %>%
        filter(age > 14 & age < 65) %>%
        left_join(df2 %>% select(c('uqnr', 'personnr', 'status1', 'status2', 'occup', 'indus', 'q27salto', 'q28salpe')), by = c('uqnr', 'personnr')) %>%
        mutate(q27salto = as.numeric(q27salto)) %>%
        mutate(msal = case_when(
            q28salpe == "Per week" ~ q27salto*4.2,
            q28salpe == "Annually" ~ q27salto/12,
            q28salpe == "Per month" ~ q27salto,
            TRUE ~ NA_real_
        )) %>%
        mutate(logsal = log(msal)) %>%
        mutate(female = recode(gender, "Male" = 0L, "Female" = 1L)) %>%
        mutate(educ = dplyr::recode(q110hied, !!!setNames(recoding_table$new_value, recoding_table$old_value))) %>%
        mutate(educ = as.numeric(educ)) %>%
        mutate(age_sq = age^2)

    }

pooled_2002_2 <- clean_2002(person_2002, worker_2002, recoding_educ_2002)


is_identical2 <- identical(pooled_2002_1, pooled_2002_2)

# Print the result
if (is_identical2) {
    print("The data frames are identical.")
} else {
    print("The data frames are not identical.")
}



columns_pooled2002_1 <- colnames(pooled_2002_1)
columns_pooled2002_2 <- colnames(pooled_2002_2)

# Compare the columns and find the ones that are not identical
different_columns <- columns_pooled2002_1[!identical(pooled_2002_1, pooled_2002_2)]

# Print the different columns
print(different_columns)


library(dplyr)

# Compare two data frames and find non-identical observations
non_identical_rows <- anti_join(pooled_2002_1, pooled_2002_2, by = "educ")

# View the non-identical observations
View(non_identical_rows)
