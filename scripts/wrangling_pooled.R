#> APPEND DATASETS ====
 pooled_ghs <- bind_rows(pooled_2002, pooled_2003, pooled_2004, pooled_2005, pooled_2006, pooled_2007, pooled_2008, pooled_2009, pooled_2010, pooled_2011, pooled_2012, pooled_2013, pooled_2014, pooled_2015, pooled_2016, pooled_2017, pooled_2018, pooled_2019, pooled_2020, pooled_2021)


# GHS Wrangling ====
pooled_ghs <- pooled_ghs %>% 
  mutate(exp = age - educ - 6, # Experience variables
         exp_sq = exp^2) %>% 
  mutate(gfc = case_when( # Global Financial Crisis
    year > 2007 & year < 2014 ~ 1,
    TRUE ~ 0
  ) ) %>%
  mutate(aapre2003 = case_when( ## Pre-2003:
    year == 2002 ~ "0",
    year == 2003 ~ "1",
    year > 2003 ~ "1"
  )) %>%
  mutate(aapre2003 = as.numeric(aapre2003)) %>% 
  mutate(aapre2008 = case_when( ## 2003: Employment Equity Act + Skills Development Act
    year < 2003 ~ "0",
    year == 2003 ~ "1",
    year == 2004 ~ "2",
    year == 2005 ~ "3",
    year == 2006 ~ "4",
    year == 2007 ~ "5",
    year > 2007 ~ "5"
  )) %>%
  mutate(aapre2008 = as.numeric(aapre2008)) %>% 
  mutate(aapre2014 = case_when( ## 2008: Codes of Good Practice
    year < 2008 ~ "0",
    year == 2008 ~ "1",
    year == 2009 ~ "2",
    year == 2010 ~ "3",
    year == 2011 ~ "4",
    year == 2012 ~ "5",
    year == 2013 ~ "6",
    year > 2013 ~ "6"
  )) %>%
  mutate(aapre2014 = as.numeric(aapre2014)) %>% 
  mutate(aapre2019 = case_when( ## 2014: Simplification of BEE scorecard
    year < 2014 ~ "0",
    year == 2014 ~ "1",
    year == 2015 ~ "2",
    year == 2016 ~ "3",
    year == 2017 ~ "4",
    year == 2018 ~ "5",
    year > 2018 ~ "5"
  )) %>%
  mutate(aapre2019 = as.numeric(aapre2019)) %>% 
  mutate(aapost2019 = case_when( ## 2019: Significant changes
    year < 2019 ~ "0",
    year == 2019 ~ "1",
    year == 2020 ~ "2",
    year == 2021 ~ "3",
    year > 2021 ~ "3"
  )) %>% 
  mutate(aapost2019 = as.numeric(aapost2019)) %>% 
  mutate(y2002 = ifelse(year == "2002", 1, 0), # Year dummies
         y2003 = ifelse(year == "2003", 1, 0),
         y2004 = ifelse(year == "2004", 1, 0),
         y2005 = ifelse(year == "2005", 1, 0),
         y2006 = ifelse(year == "2006", 1, 0),
         y2007 = ifelse(year == "2007", 1, 0),
         y2008 = ifelse(year == "2008", 1, 0),
         y2009 = ifelse(year == "2009", 1, 0),
         y2010 = ifelse(year == "2010", 1, 0),
         y2011 = ifelse(year == "2011", 1, 0),
         y2012 = ifelse(year == "2012", 1, 0),
         y2013 = ifelse(year == "2013", 1, 0),
         y2014 = ifelse(year == "2014", 1, 0),
         y2015 = ifelse(year == "2015", 1, 0),
         y2016 = ifelse(year == "2016", 1, 0),
         y2017 = ifelse(year == "2017", 1, 0),
         y2018 = ifelse(year == "2018", 1, 0),
         y2019 = ifelse(year == "2019", 1, 0),
         y2020 = ifelse(year == "2020", 1, 0),
         y2021 = ifelse(year == "2021", 1, 0)) %>% 
  mutate(prispline = ifelse(educ < 8, educ, 7)) %>% # Education splines
  mutate(secspline = ifelse(
    educ < 8, 0, ifelse(educ == 8, 1, ifelse(educ == 9, 2, ifelse(educ == 10, 3, ifelse(educ == 11, 4, ifelse(educ == 12, 5, 5))))))) %>%
  mutate(terspline = ifelse(
    educ < 13, 0, ifelse(educ == 13, 1, ifelse(educ == 14, 2, ifelse(educ == 15, 3, ifelse(educ == 16, 4, ifelse(educ == 17, 5, 5)))))))


# Ingest CPI and create Real Log Wages ---
cpi <- read.csv("data/cpi_data.csv") %>%
  rename(cpi = ZAFCPIALLMINMEI) %>%
  mutate(year = year(DATE)) %>%
  mutate(cpi = cpi*100/131.70924) %>%
  select(year, cpi) %>% 
  mutate(year = as.factor(year))

pooled_ghs <- pooled_ghs %>%
  left_join(cpi, by = "year") %>% 
  mutate(logrealsal = log(imputedsal*100/cpi)) %>% 
  mutate(year = as.numeric(year))

# Create birth years and subset for birth year data ----
ghs_birthyear <- pooled_ghs %>%
  mutate(birthyear = year - age) %>%
  filter(birthyear >= 1940 & birthyear <= 2000) %>%
  mutate(
    by1940 = if_else(birthyear < 1950, birthyear - 1940, 10),
    by1950 = if_else(birthyear < 1950, 0, if_else(birthyear < 1960, birthyear - 1950, 10)),
    by1960 = if_else(birthyear < 1960, 0, if_else(birthyear < 1970, birthyear - 1960, 10)),
    by1970 = if_else(birthyear < 1970, 0, if_else(birthyear < 1980, birthyear - 1970, 10)),
    by1980 = if_else(birthyear < 1980, 0, if_else(birthyear < 1990, birthyear - 1980, 10)),
    by1990 = if_else(birthyear < 1990, 0, if_else(birthyear < 2000, birthyear - 1990, 10))
  )



