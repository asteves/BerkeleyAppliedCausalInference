library(tidyverse)
library(estimatr)

data <- read_csv("WP8Data.csv")

m1 <- lm_robust(diff ~ treat, data = data, clusters = vilno)%>%
  tidy()%>%
  mutate(model = "m1")
m2 <- lm_robust(diff ~ treat + lpop2000 + poverty + tariq + iso + lnn + garrison + reb + pret + swept + lelev, data = data, cluster = vilno)%>%
  tidy()%>%
  mutate(model = "m2")
m3 <- lm_robust(diff ~ treat, data = data %>% filter(groznyy == 0), cluster = vilno)%>%
  tidy()%>%
  mutate(model = "m3")
m4 <- lm_robust(diff ~ treat + lpop2000 + poverty + tariq + iso + lnn + garrison + reb + pret + swept + lelev, data = data %>% filter(groznyy == 0), cluster = vilno)%>%
  tidy()%>%
  mutate(model = "m4")


### coefficient plots 
models <- bind_rows(m1, m2, m3, m4)%>%
  filter(term == "treat")%>%
  ggplot(aes(y = model, x = estimate))+
  geom_vline(xintercept = 0, linetype = 2) + 
  geom_point() + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = 0.1))+ 
  theme_bw()
models 