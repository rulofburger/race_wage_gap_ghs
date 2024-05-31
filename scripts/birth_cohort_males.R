#> Time Cohort Regression: Males ====
ghs_birthyear_male <- ghs_birthyear %>% 
  filter(female == 0)

# regression with age and birthyear
bymalereg <- lm(formula = logrealsal ~ race + prispline + secspline + terspline + age + gfc + gfc*prispline + gfc*secspline + exp + exp_sq + aapre2008 + aapre2014 + aapre2019 + aapost2019 + aapre2008*race + aapre2014*race + aapre2019*race + aapost2019*race + by1940 + by1950 + by1960 + by1970 + by1980 + by1990 + by1940*race + by1950*race + by1960*race + by1970*race + by1980*race + by1990*race, data = ghs_birthyear_male %>% filter(imputedsal > 0), weights = weight)
bymalereg %>%
  summary()

# RACE AND AGE ====
# fake dataset where everything except for race and age are zero
male_x_age <- ghs_birthyear_male %>%
  select(race, age) %>%
  unique() %>%
  add_column(exp = 0) %>%
  add_column(exp_sq = 0) %>%
  add_column(prispline = 0) %>%
  add_column(secspline = 0) %>%
  add_column(terspline = 0) %>%
  add_column(gfc = 0) %>%
  add_column(aapre2008 = 0) %>%
  add_column(aapre2014 = 0) %>%
  add_column(aapre2019 = 0) %>%
  add_column(aapost2019 = 0) %>% 
  add_column(by1940 = 0) %>%
  add_column(by1950 = 0) %>%
  add_column(by1960 = 0) %>%
  add_column(by1970 = 0) %>%
  add_column(by1980 = 0) %>%
  add_column(by1990 = 0)


male_x_age$y <- predict.lm(object = bymalereg, newdata = male_x_age)


male_blacksal <- male_x_age %>% 
  filter(race == "African/Black") %>%
  select(age, y)

male_x_age <- male_x_age %>%
  left_join(male_blacksal, by = "age", suffix = c("", "_black")) %>%
  mutate(wagegap = y - y_black) %>% 
  filter(race != "African/Black")


male_age_gapgraph <- ggplot(data=male_x_age, aes(x = age, y = wagegap, group = race)) +
  geom_line(aes(color = race)) +
  geom_point(aes(color = race)) +
  labs(x='age', y='wage gap') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))
male_age_gapgraph


# RACE AND BIRTHYEAR ====
# fake dataset where everything except for race and birthyear are zero
male_x_birthyear <- ghs_birthyear_male %>%
  select(race, birthyear, by1940, by1950, by1960, by1970, by1980, by1990) %>%
  unique() %>%
  add_column(exp = 0) %>%
  add_column(exp_sq = 0) %>%
  add_column(age = 0) %>%
  add_column(prispline = 0) %>%
  add_column(secspline = 0) %>%
  add_column(terspline = 0) %>%
  add_column(gfc = 0) %>%
  add_column(aapre2008 = 0) %>%
  add_column(aapre2014 = 0) %>%
  add_column(aapre2019 = 0) %>%
  add_column(aapost2019 = 0) 

male_x_birthyear$y <- predict.lm(object = bymalereg, newdata = male_x_birthyear)


male_blacksal_by <- male_x_birthyear %>% 
  filter(race == "African/Black") %>%
  select(birthyear, y)

male_x_birthyear <- male_x_birthyear %>%
  left_join(male_blacksal_by, by = "birthyear", suffix = c("", "_black")) %>%
  mutate(wagegap = y - y_black) %>% 
  filter(race != "African/Black")


male_by_gapgraph <- ggplot(data=male_x_birthyear, aes(x = birthyear, y = wagegap, group = race)) +
  geom_line(aes(color = race)) +
#  geom_point(aes(color = race)) +
  labs(x='birthyear', y='wage gap') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))
male_by_gapgraph
