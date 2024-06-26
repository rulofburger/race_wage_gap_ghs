# METADATA ====
# Description: Data ingestion
# Created: 2022-05-21 (Emma Terblanche)
# Refactored: 2023-04-04 (Rulof Burger)
# Reviewed: NA

# SUMMARY: This script opens the raw GHS data files downloaded from DataFirst.
# It then proceeds to filter on the working age population, and harmonise the
# variable names and labels. Finally, the harmonised data frames are joined into
# a single analysis data frame.


# PROCESS ====
# Run salary_category_tables.R
# Run educ_tables.R
# Run function_ingest_overall.R
# Run this script.
source('scripts/salary_category_tables.R')
source('scripts/educ_tables.R')
source('scripts/function_ingest_overall.R')

# INITIALISE ====

# Load packages ----
library(tidyverse)
library(dplyr)

# LOAD DATA ====

#> 2002 data ----
worker_2002 <- read.csv("data/raw/ghs_2002_worker.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q27salto) %>%
    mutate(salperiod = q28salpe) %>%
    mutate(incomecategory = q29salca) %>% 
    mutate(status = status1) %>% 
    mutate(weight = worker_wgt) 

person_2002 <- read.csv("data/raw/ghs_2002_person.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(educvar = q110hied) %>% 
    mutate(year = c("2002"))

#> 2003 data ----

worker_2003 <- read_csv("data/raw/ghs_2003_worker.csv", 
                            col_types = cols(Stratum = col_skip())) %>% rename_with(tolower) %>%
              mutate(salary = q28salto) %>%
              mutate(salperiod = q29salpe) %>%
              mutate(incomecategory = q210salc) %>%
              mutate(status = status1) %>% 
              mutate(weight = worker_wgt) 

person_2003 <- read.csv("data/raw/ghs_2003_person.csv", header = TRUE) %>% rename_with(tolower) %>%
     mutate(educvar = q110hied) %>%
     mutate(age = as.numeric(age)) %>% 
     mutate(year = c("2003"))


#> 2004 data ----
worker_2004 <- read.csv("data/raw/ghs_2004_worker.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q28salto) %>%
    mutate(salperiod = q29salpe) %>%
    mutate(incomecategory = q210salc) %>% 
    mutate(status = status1) %>% 
    mutate(weight = worker_wgt) 

person_2004 <- read.csv("data/raw/ghs_2004_person.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(educvar = q19hiedu) %>%
    mutate(age = as.numeric(age)) %>% 
  mutate(year = c("2004"))

#> 2005 data ----
worker_2005 <- read.csv("data/raw/ghs_2005_worker.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q29salto) %>%
    mutate(salperiod = q210salp) %>%
    mutate(incomecategory = q211salc)  %>% 
    mutate(status = status1) %>% 
    mutate(weight = worker_wgt) 

person_2005 <- read.csv("data/raw/ghs_2005_person.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(educvar = q19hiedu) %>% 
    mutate(stratum = "") %>% 
  mutate(year = c("2005"))
# NOTE: There is no stratum in the 2005 dataset. 

#> 2006 data ----
worker_2006 <- read.csv("data/raw/ghs_2006_worker.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q29salto) %>%
    mutate(salperiod = q210salp) %>%
    mutate(incomecategory = q211salc) %>% 
    mutate(status = status1) %>% 
    mutate(weight = worker_wgt) 

person_2006 <- read.csv("data/raw/ghs_2006_person.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(educvar = q19hiedu) %>% 
    mutate(stratum = "")%>% 
  mutate(year = c("2006"))
# NOTE: There is no stratum in the 2006 dataset. 

#> 2007 data ----
worker_2007 <- read.csv("data/raw/ghs_2007_worker.csv", header = TRUE) %>% rename_with(tolower)%>%
    mutate(salary = q29salto) %>%
    mutate(salperiod = q210salp) %>%
    mutate(incomecategory = q211salc) %>% 
    mutate(status = status1) %>% 
    mutate(weight = worker_wgt) 

person_2007 <- read.csv("data/raw/ghs_2007_person.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(educvar = q19hiedu) %>% 
    mutate(stratum = "")%>% 
    mutate(year = c("2007"))
# NOTE: There is no stratum in the 2007 dataset. 

#> 2008 data ----
worker_2008 <- read_csv("data/raw/ghs_2008_worker.csv", col_types = cols(Age_grp = col_skip())) %>% rename_with(tolower) %>%
    mutate(salary = q29salto) %>%
    mutate(salperiod = q210salp) %>%
    mutate(incomecategory = q211salc) %>% 
    mutate(status = status1) %>% 
    mutate(weight = worker_wgt) 

person_2008 <- read.csv("data/raw/ghs_2008_person.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(educvar = q19hiedu)%>% 
  mutate(year = c("2008"))

#> 2009 data ----
person_2009 <- read.csv("data/raw/ghs_2009_person.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q141asto) %>%
    mutate(salperiod = q141bsp) %>%
    mutate(educvar = q16hiedu)%>%
    mutate(incomecategory = q142salc) %>% 
    mutate(status = q140awge)%>% 
    mutate(year = c("2009")) %>% 
    mutate(weight = person_wgt) 

#> 2010 data ----
person_2010 <- read.csv("data/raw/ghs_2010_person.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q22asto) %>%
    mutate(salperiod = q22bsp) %>%
    mutate(educvar = q16hiedu) %>%
    mutate(incomecategory = q23salc) %>% 
    mutate(status = q21awge)%>% 
    mutate(year = c("2010")) %>% 
    mutate(weight = person_wgt) 

#> 2011 data ----
person_2011 <- read.csv("data/raw/ghs_2011_person.csv", header = TRUE) %>% rename_with(tolower)%>%
    mutate(salary = q22asto) %>%
    mutate(salperiod = q22bsp) %>%
    mutate(educvar = q16hiedu) %>%
    mutate(incomecategory = q23salc) %>% 
    mutate(status = q21awge) %>% 
    mutate(year = c("2011")) %>% 
    mutate(weight = person_wgt) 

#> 2012 data ----
person_2012 <- read.csv("data/raw/ghs_2012_person.csv", header = TRUE) %>% rename_with(tolower)%>%
    mutate(salary = q22asto) %>%
    mutate(salperiod = q22bsp) %>%
    mutate(educvar = q16hiedu) %>%
    mutate(incomecategory = q23salc) %>% 
    mutate(status = q21awge) %>% 
    mutate(year = c("2012")) %>% 
    mutate(weight = person_wgt) 

#> 2013 data ----
person_2013 <- read.csv("data/raw/ghs_2013_person.csv", header = TRUE) %>% rename_with(tolower)%>%
    mutate(salary = q42asto) %>%
    mutate(salperiod = q42bsp) %>%
    mutate(educvar = q16hiedu) %>%
    mutate(incomecategory = q43salc) %>% 
    mutate(status = q41awge) %>% 
    mutate(year = c("2013")) %>% 
    mutate(weight = person_wgt) 

#> 2014 data ----
person_2014 <- read.csv("data/raw/ghs_2014_person.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q42asto) %>%
    mutate(salperiod = q42bsp) %>%
    mutate(educvar = q15hiedu)%>%
    mutate(incomecategory = q43salc) %>% 
    mutate(status = employ_status1) %>% 
    mutate(year = c("2014")) %>% 
    mutate(weight = person_wgt) 

#> 2015 data ----
person_2015 <- read.csv("data/raw/ghs_2015_person.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q42asto) %>%
    mutate(salperiod = q42bsp) %>%
    mutate(educvar = q15hiedu) %>%
    mutate(incomecategory = q43salc) %>% 
    mutate(status = employ_status1) %>% 
    mutate(year = c("2015")) %>% 
    mutate(weight = person_wgt) 

#> 2016 data ----
person_2016 <- read.csv("data/raw/ghs_2016_person.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q42asto) %>%
    mutate(salperiod = q42bsp) %>%
    mutate(educvar = q15hiedu) %>%
    mutate(incomecategory = q43salc) %>% 
    mutate(status = employ_status1)%>% 
    mutate(year = c("2016")) %>% 
    mutate(weight = person_wgt) 

#> 2017 data ----
person_2017 <- read.csv("data/raw/ghs_2017_person.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q42asto) %>%
    mutate(salperiod = q42bsp) %>%
    mutate(educvar = q15hiedu) %>%
    mutate(incomecategory = q43salc) %>% 
    mutate(status = employ_status1)%>% 
    mutate(year = c("2017")) %>% 
    mutate(weight = person_wgt) 

#> 2018 data ----
person_2018 <- read.csv("data/raw/ghs_2018_person.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = q42asto) %>%
    mutate(salperiod = q42bsp) %>%
    mutate(educvar = q15hiedu) %>%
    mutate(incomecategory = as.character(q43salc)) %>% 
    mutate(status = employ_status1) %>% 
    mutate(year = c("2018")) %>% 
    mutate(weight = person_wgt) 

#> 2019 data ----
person_2019 <- read.csv("data/raw/ghs_2019_person.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = lab_sto) %>%
    mutate(salperiod = lab_salper) %>%
    mutate(educvar = education) %>%
    mutate(gender = sex) %>%
    mutate(race = population) %>%
    mutate(incomecategory = lab_salc) %>% 
    mutate(status = lab_wge) %>% 
    mutate(year = c("2019")) %>% 
    mutate(weight = person_wgt) 

#> 2020 data ----
person_2020 <- read.csv("data/raw/ghs_2020_person.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = lab_sto) %>%
    mutate(salperiod = lab_salper) %>%
    mutate(educvar = education) %>%
    mutate(gender = sex) %>%
    mutate(race = population) %>%
    mutate(incomecategory = lab_salc) %>% 
    mutate(status = lab_wge) %>% 
    mutate(year = c("2020")) %>% 
    mutate(weight = person_wgt) 
  
#> 2021 data ----
person_2021 <- read.csv("data/raw/ghs_2021_person.csv", header = TRUE) %>% rename_with(tolower) %>%
    mutate(salary = lab_sto) %>%
    mutate(salperiod = lab_salper) %>%
    mutate(educvar = education) %>%
    mutate(gender = sex) %>%
    mutate(race = population) %>%
    mutate(incomecategory = lab_salc) %>% 
    mutate(status = lab_wge) %>% 
    mutate(year = c("2021")) %>% 
    mutate(weight = person_wgt) 


# WRANGLE DATA ====

pooled_2002 <- ingest_2002_2008(person_2002, worker_2002, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2002)

pooled_2003 <- ingest_2002_2008(person_2003, worker_2003, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2003)

pooled_2004 <- ingest_2002_2008(person_2004, worker_2004, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2004)

pooled_2005 <- ingest_2002_2008(person_2005, worker_2005, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2005)

pooled_2006 <- ingest_2002_2008(person_2006, worker_2006, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2006)

pooled_2007 <- ingest_2002_2008(person_2007, worker_2007, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2007)

pooled_2008 <- ingest_2002_2008(person_2008, worker_2008, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2008)

pooled_2009 <- ingest_2009_2014(person_2009, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2009)

pooled_2010 <- ingest_2009_2014(person_2010, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2010)

pooled_2011 <- ingest_2009_2014(person_2011, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2011)

pooled_2012 <- ingest_2009_2014(person_2012, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2012)

pooled_2013 <- ingest_2009_2014(person_2013, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2013)

pooled_2014 <- ingest_2009_2014(person_2014, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2014)

pooled_2015 <- ingest_2015_2018(person_2015, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2015)

pooled_2016 <- ingest_2015_2018(person_2016, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2016)

pooled_2017 <- ingest_2015_2018(person_2017, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2017)

pooled_2018 <- ingest_2015_2018(person_2018, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2018)

pooled_2019 <- ingest_2019_2021(person_2019, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2019)

pooled_2020 <- ingest_2019_2021(person_2020, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2020)

pooled_2021 <- ingest_2019_2021(person_2021, age, salary, salary_categories_table = salarycategories_2002_2021, salperiod, educvar, recoding_table = recoding_educ_2021)

# TEST REGRESSION ====

#> 2002 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2002 %>% filter(imputedsal > 0)) %>%
    summary()

#> 2003 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2003 %>% filter(imputedsal > 0)) %>%
    summary()


#> 2004 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2004 %>% filter(imputedsal > 0)) %>%
    summary()

#> 2005 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2005 %>% filter(imputedsal > 0)) %>%
    summary()

#> 2006 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2006 %>% filter(imputedsal > 0)) %>%
    summary()

#> 2007 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2007 %>% filter(imputedsal > 0)) %>%
    summary()

#> 2008 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2008 %>% filter(imputedsal > 0)) %>%
    summary()

#> 2009 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2009 %>% filter(imputedsal > 0)) %>%
    summary()

#> 2010 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2010 %>% filter(imputedsal > 0)) %>%
    summary()

#> 2011 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2011 %>% filter(imputedsal > 0)) %>%
    summary()

#> 2012 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2012 %>% filter(imputedsal > 0)) %>%
    summary()

#> 2013 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2013 %>% filter(imputedsal > 0)) %>%
    summary()

#> 2014 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2014 %>% filter(imputedsal > 0)) %>%
    summary()

#> 2015 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2015 %>% filter(imputedsal > 0)) %>%
    summary()

#> 2016 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2016 %>% filter(imputedsal > 0)) %>%
    summary()

#> 2017 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2017 %>% filter(imputedsal > 0)) %>%
    summary()

#> 2018 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2018 %>% filter(imputedsal > 0)) %>%
    summary()

#> 2019 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2019 %>% filter(imputedsal > 0)) %>%
    summary()

#> 2020 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2020 %>% filter(imputedsal > 0)) %>%
    summary()

#> 2021 data ----
lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_2021 %>% filter(imputedsal > 0)) %>%
    summary()



#> APPEND DATASETS ====
pooled_ghs <- bind_rows(pooled_2002, pooled_2003, pooled_2004, pooled_2005, pooled_2006, pooled_2007, pooled_2008, pooled_2009, pooled_2010, pooled_2011, pooled_2012, pooled_2013, pooled_2014, pooled_2015, pooled_2016, pooled_2017, pooled_2018, pooled_2019, pooled_2020, pooled_2021)

#> WRANGLE POOLED DATA ====
source('scripts/wrangling_pooled.R')

#> GENERATE GRAPHS ====
source('scripts/wage_gaps.R')
source('scripts/birth_cohort_males.R')
source('scripts/birth_cohort_females.R')
