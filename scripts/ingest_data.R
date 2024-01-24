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
library(dplyr)

# LOAD DATA ====

#> 2002 data ----
worker_2002 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2002_data/ghs-2002-worker-v1.3.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q27salto) %>%
    mutate(salperiod = q28salpe) %>%
    mutate(incomecategory = q29salca)

person_2002 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2002_data/ghs-2002-person-v1.3.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(educvar = q110hied)

#> 2003 data ----
worker_2003 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2003_data/ghs-2003-worker-v1.4.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q28salto) %>%
    mutate(salperiod = q29salpe) %>%
    mutate(incomecategory = q210salc)

person_2003 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2003_data/ghs-2003-person-v1.4.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(educvar = q110hied)

#> 2004 data ----
worker_2004 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2004_data/ghs-2004-worker-v1.4.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q28salto) %>%
    mutate(salperiod = q29salpe) %>%
    mutate(incomecategory = q210salc)

person_2004 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2004_data/ghs-2004-person-v1.4.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(educvar = q19hiedu) %>%
    mutate(age = as.numeric(age))

#> 2005 data ----
worker_2005 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2005_data/ghs-2005-worker-v1.4.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q29salto) %>%
    mutate(salperiod = q210salp) %>%
    mutate(incomecategory = q211salc)

person_2005 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2005_data/ghs-2005-person-v1.4.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(educvar = q19hiedu)

#> 2006 data ----
worker_2006 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2006_data/ghs-2006-worker-v1.4.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q29salto) %>%
    mutate(salperiod = q210salp) %>%
    mutate(incomecategory = q211salc)

person_2006 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2006_data/ghs-2006-person-v1.4.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(educvar = q19hiedu)

#> 2007 data ----
worker_2007 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2007_data/ghs-2007-worker-v1.4.csv", header = TRUE) %>% rename_with(tolower)%>%
    mutate(salary = q29salto) %>%
    mutate(salperiod = q210salp) %>%
    mutate(incomecategory = q211salc)

person_2007 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2007_data/ghs-2007-person-v1.4.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(educvar = q19hiedu)

#> 2008 data ----
worker_2008 <- read_csv("data/raw/ghs_2008_data/ghs-2008-worker-v1.4.csv", col_types = cols(Age_grp = col_skip())) %>% rename_with(tolower) %>%
    mutate(salary = q29salto) %>%
    mutate(salperiod = q210salp) %>%
    mutate(incomecategory = q211salc)

person_2008 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2008_data/ghs-2008-person-v1.4.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(educvar = q19hiedu)

#> 2009 data ----
person_2009 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2009_data/GHS2009Person.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q141asto) %>%
    mutate(salperiod = q141bsp) %>%
    mutate(educvar = q16hiedu)%>%
    mutate(incomecategory = q142salc)

#> 2010 data ----
person_2010 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2010_data/zaf-statssa-ghs-2010-person-v2.2.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q22asto) %>%
    mutate(salperiod = q22bsp) %>%
    mutate(educvar = q16hiedu)%>%
    mutate(incomecategory = q23salc)

#> 2011 data ----
person_2011 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2011_data/zaf-statssa-ghs-2011-person-v1.2.csv", header = TRUE) %>% rename_with(tolower)%>%
    mutate(salary = q22asto) %>%
    mutate(salperiod = q22bsp) %>%
    mutate(educvar = q16hiedu) %>%
    mutate(incomecategory = q23salc)

#> 2012 data ----
person_2012 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2012_data/zaf-statssa-ghs-2012-person-v2.1.csv", header = TRUE) %>% rename_with(tolower)%>%
    mutate(salary = q22asto) %>%
    mutate(salperiod = q22bsp) %>%
    mutate(educvar = q16hiedu)%>%
    mutate(incomecategory = q23salc)

#> 2013 data ----
person_2013 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2013_data/zaf-statssa-ghs-2013-person-v1.1.csv", header = TRUE) %>% rename_with(tolower)%>%
    mutate(salary = q42asto) %>%
    mutate(salperiod = q42bsp) %>%
    mutate(educvar = q16hiedu) %>%
    mutate(incomecategory = q43salc)

#> 2014 data ----
person_2014 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2014_data/GHS 2014 Person v1.1 CSV.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q42asto) %>%
    mutate(salperiod = q42bsp) %>%
    mutate(educvar = q15hiedu)%>%
    mutate(incomecategory = q43salc)

#> 2015 data ----
person_2015 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2015_data/GHS 2015 Person v1.2 CSV.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q42asto) %>%
    mutate(salperiod = q42bsp) %>%
    mutate(educvar = q15hiedu)%>%
    mutate(incomecategory = q43salc)

#> 2016 data ----
person_2016 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2016_data/GHS2016Person.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q42asto) %>%
    mutate(salperiod = q42bsp) %>%
    mutate(educvar = q15hiedu)%>%
    mutate(incomecategory = q43salc)

#> 2017 data ----
person_2017 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2017_data/GHS 2017 Person v1.0 CSV.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q42asto) %>%
    mutate(salperiod = q42bsp) %>%
    mutate(educvar = q15hiedu)%>%
    mutate(incomecategory = q43salc)

#> 2018 data ----
person_2018 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2018_data/ghs-2018-person-1.0-csv.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q42asto) %>%
    mutate(salperiod = q42bsp) %>%
    mutate(educvar = q15hiedu)%>%
    mutate(incomecategory = q43salc)

#> 2019 data ----
person_2019 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2019_data/zaf-statssa-ghs-2019-person-v1-csv.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = lab_sto) %>%
    mutate(salperiod = lab_salper) %>%
    mutate(educvar = education) %>%
    mutate(gender = sex) %>%
    mutate(race = population) %>%
    mutate(incomecategory = lab_salc)

#> 2020 data ----
person_2020 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2020_data/ghs-2020-person-v1.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = lab_sto) %>%
    mutate(salperiod = lab_salper) %>%
    mutate(educvar = education) %>%
    mutate(gender = sex) %>%
    mutate(race = population) %>%
    mutate(incomecategory = lab_salc)

#> 2021 data ----
person_2021 <- read.csv("/Users/mac/Desktop/race_wage_gap_ghs/data/raw/ghs_2021_data/ghs-2021-person-v1.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = lab_sto) %>%
    mutate(salperiod = lab_salper) %>%
    mutate(educvar = education) %>%
    mutate(gender = sex) %>%
    mutate(race = population) %>%
    mutate(incomecategory = lab_salc)


# WRANGLE DATA ====

pooled_2002 <- ingest_2002_2008(person_2002, worker_2002, age, salary, salary_categories_table = salarycategories_2002_2012, salperiod, educvar, recoding_table = recoding_educ_2002)

pooled_2003 <- ingest_2002_2008(person_2003, worker_2003, age, salary, salary_categories_table = salarycategories_2002_2012, salperiod, educvar, recoding_table = recoding_educ_2003)

pooled_2004 <- ingest_2002_2008(person_2004, worker_2004, age, salary, salary_categories_table = salarycategories_2002_2012, salperiod, educvar, recoding_table = recoding_educ_2004)

pooled_2005 <- ingest_2002_2008(person_2005, worker_2005, age, salary, salary_categories_table = salarycategories_2002_2012, salperiod, educvar, recoding_table = recoding_educ_2005)

pooled_2006 <- ingest_2002_2008(person_2006, worker_2006, age, salary, salary_categories_table = salarycategories_2002_2012, salperiod, educvar, recoding_table = recoding_educ_2006)

pooled_2007 <- ingest_2002_2008(person_2007, worker_2007, age, salary, salary_categories_table = salarycategories_2002_2012, salperiod, educvar, recoding_table = recoding_educ_2007)

pooled_2008 <- ingest_2002_2008(person_2008, worker_2008, age, salary, salary_categories_table = salarycategories_2002_2012, salperiod, educvar, recoding_table = recoding_educ_2008)

# Uses new function from here
pooled_2009 <- ingest_2009_2014(person_2009, age, salary, salary_categories_table = salarycategories_2002_2012, salperiod, educvar, recoding_table = recoding_educ_2009)

pooled_2010 <- ingest_2009_2014(person_2010, age, salary, salary_categories_table = salarycategories_2002_2012, salperiod, educvar, recoding_table = recoding_educ_2010)

pooled_2011 <- ingest_2009_2014(person_2011, age, salary, salary_categories_table = salarycategories_2002_2012, salperiod, educvar, recoding_table = recoding_educ_2011)

pooled_2012 <- ingest_2009_2014(person_2012, age, salary, salary_categories_table = salarycategories_2002_2012, salperiod, educvar, recoding_table = recoding_educ_2012)

pooled_2013 <- ingest_2009_2014(person_2013, age, salary, salperiod, educvar, recoding_table = recoding_educ_2013)

pooled_2014 <- ingest_2009_2014(person_2014, age, salary, salperiod, educvar, recoding_table = recoding_educ_2014)

pooled_2015 <- ingest_2015_2018(person_2015, age, salary, salperiod, educvar, recoding_table = recoding_educ_2015)

pooled_2016 <- ingest_2015_2018(person_2016, age, salary, salperiod, educvar, recoding_table = recoding_educ_2016)

pooled_2017 <- ingest_2015_2018(person_2017, age, salary, salperiod, educvar, recoding_table = recoding_educ_2017)

pooled_2018 <- ingest_2015_2018(person_2018, age, salary, salperiod, educvar, recoding_table = recoding_educ_2018)

pooled_2019 <- ingest_2019_2021(person_2019, age, salary, salperiod, educvar, recoding_table = recoding_educ_2019)

pooled_2020 <- ingest_2019_2021(person_2020, age, salary, salperiod, educvar, recoding_table = recoding_educ_2020)

pooled_2021 <- ingest_2019_2021(person_2021, age, salary, salperiod, educvar, recoding_table = recoding_educ_2021)

# TEST REGRESSION ====

#> 2002 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2002) %>%
    summary()

#> 2003 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2003) %>%
    summary()

#> 2004 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2004) %>%
    summary()

#> 2005 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2005) %>%
    summary()

#> 2006 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2006) %>%
    summary()

#> 2007 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2007) %>%
    summary()

#> 2008 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2008) %>%
    summary()

#> 2009 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2009) %>%
    summary()

#> 2010 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2010) %>%
    summary()

#> 2011 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2011) %>%
    summary()
## ERROR
## Log(0) = -Inf

#> 2012 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2012) %>%
    summary()
## ERROR
## Log(0) = -Inf

#> 2013 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2013) %>%
    summary()
## ERROR
## Log(0) = -Inf

#> 2014 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2014) %>%
    summary()
## ERROR
## Log(0) = -Inf

#> 2015 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2015) %>%
    summary()
## ERROR
## Log(0) = -Inf

#> 2016 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2016) %>%
    summary()

#> 2017 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2017) %>%
    summary()
## ERROR
## Log(0) = -Inf

#> 2018 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2018) %>%
    summary()

#> 2019 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2019) %>%
    summary()

#> 2020 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2020) %>%
    summary()

#> 2021 data ----
lm(logsal ~ race + educ + age + age_sq + female, data = pooled_2021) %>%
    summary()
