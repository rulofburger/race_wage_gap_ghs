# REGRESSIONS ====
# Description: Mincer regressions
# Created: 2024-05-27 (Emma Terblanche)

library(sjPlot)
library(coefplot)

mincer1 <- lm(logimputedsal ~ race + educ + age + age_sq + female, data = pooled_ghs %>% filter(imputedsal > 0), weights = weight)
mincer1 %>%
  summary()

mincer2 <- lm(logimputedsal ~ educ + age + age_sq + female + year*race, data = pooled_ghs %>% filter(imputedsal > 0), weights = weight)
mincer2 %>%
  summary()

pooled_ghs %>% ggplot(aes(x = educ, y = logimputedsal)) + geom_smooth()

# Regressions with experience ----
mincer3 <- lm(logimputedsal ~ exp + exp_sq + age + age_sq + female + year*race, data = pooled_ghs %>% filter(imputedsal > 0), weights = weight)
mincer3 %>%
  summary()

# Regressions with splines ----
mincer4 <- lm(logimputedsal ~ race + prispline + secspline + terspline + age_sq + age + female, data = pooled_ghs %>% filter(imputedsal > 0), weights = weight)
mincer4 %>%
  summary()

# Regressions with year dummies ----
mincer5 <- lm(logimputedsal ~ prispline + secspline + terspline + age_sq + age + female + 
                y2004*race + y2005*race + y2006*race + y2007*race + y2008*race + 
                y2009*race + y2010*race + y2011*race + y2012*race + y2013*race + y2014*race + 
                y2015*race + y2016*race + y2017*race + y2018*race + y2019*race + y2020*race + 
                y2021*race, data = pooled_ghs %>% filter(imputedsal > 0), weights = weight)
mincer5 %>%
  summary()

coefplot(mincer5)


