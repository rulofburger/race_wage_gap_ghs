# Female Wage Gap graph =====

female_ghs <- pooled_ghs[pooled_ghs$female == 1,]

femalemincer <- lm(logrealsal ~ prispline + secspline + terspline + exp + exp_sq + as.factor(year)*race, data = female_ghs %>% filter(imputedsal > 0), weights = weight)

femalemincer %>%
  summary()

female_x <- female_ghs %>%
  select(race, year) %>%
  unique() %>%
  add_column(exp = 0) %>%
  add_column(exp_sq = 0) %>%
  add_column(age_sq = 0) %>% 
  add_column(age = 0) %>% 
  add_column(female = 0) %>%
  add_column(prispline = 0) %>%
  add_column(secspline = 0) %>%
  add_column(terspline = 0)


female_x$y <- predict.lm(object = femalemincer, newdata = female_x)


female_blacksal <- female_x[ which(female_x$race=='African/Black'), ] %>%
  select(year, y)

female_x <- female_x %>%
  left_join(female_blacksal, by = "year", suffix = c("", "_black")) %>%
  mutate(wagegap = y - y_black) %>% 
  filter(race != "African/Black")


fem_gapgraph <- ggplot(data=fem_gapgraphdf, aes(x = year, y = wagegap, group = race)) +
  geom_line(aes(color = race)) +
  geom_point(aes(color = race)) +
  labs(x='year', y='wage gap') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))
fem_gapgraph



# Male Wage Gap graph =====

male_ghs <- male_ghs %>% 
  filter(female == 0) %>% 
  mutate(year = as.factor(year))

malemincer <- lm(logrealsal ~  prispline + secspline + terspline + exp + exp_sq + as.factor(year)*race, data = male_ghs %>% filter(imputedsal > 0), weights = weight)

malemincer %>%
  summary()


male_x <- male_ghs %>%
  select(race, year) %>%
  unique() %>%
  add_column(exp = 0) %>%
  add_column(exp_sq = 0) %>%
  add_column(age_sq = 0) %>%  
  add_column(age = 0) %>% 
  add_column(female = 0) %>%
  add_column(prispline = 0) %>%
  add_column(secspline = 0) %>%
  add_column(terspline = 0)


male_x$y <- predict.lm(object = malemincer, newdata = male_x)


male_blacksal <- male_x %>%
  filter(race == "African/Black") %>% 
  select(year, y)

male_x <- male_x %>%
  left_join(male_blacksal, by = "year", suffix = c("", "_black")) %>%
  mutate(wagegap = y - y_black)  %>% 
  filter(race != "African/Black")


male_gapgraph <- ggplot(data=male_x, aes(x = year, y = wagegap, group = race)) +
  geom_line(aes(color = race)) +
  geom_point(aes(color = race)) +
  labs(x='year', y='wage gap') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))
male_gapgraph



# Everyone Wage Gap graph =====
mincer <- lm(logrealsal ~  prispline + secspline + terspline + exp + exp_sq + female + 
               as.factor(year)*race, data = pooled_ghs %>% filter(imputedsal > 0)) 

mincer %>%
  summary()


x <- pooled_ghs %>%
  select(race, year) %>%
  unique() %>%
  add_column(exp = 0) %>%
  add_column(exp_sq = 0) %>%
  add_column(age_sq = 0) %>% 
  add_column(age = 0) %>% 
  add_column(female = 0) %>%
  add_column(prispline = 0) %>%
  add_column(secspline = 0) %>%
  add_column(terspline = 0)


x$y <- predict.lm(object = mincer, newdata = x)


blacksal <- x[ which(x$race=='African/Black'), ] %>%
  select(year, y)

x <- x %>%
  left_join(blacksal, by = "year", suffix = c("", "_black")) %>%
  mutate(wagegap = y - y_black) %>% 
  filter(race != "African/Black")

gapgraph <- ggplot(data= x, aes(x = year, y = wagegap, group = race)) +
  geom_line(aes(color = race)) +
  geom_point(aes(color = race)) +
  labs(x='year', y='Wage Gap') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))
gapgraph
