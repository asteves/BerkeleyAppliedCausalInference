#### Libraries #### 

# If you do not have these libraries install them
library(estimatr)
library(fabricatr)
library(randomizr)
library(tidyverse)


#### Data Generation #### 

## This is the dataset we are going to use for today 
set.seed(42)
dat <- fabricate(
  N = 100,                        # sample size
  x = runif(N, 0, 1),             # pre-treatment covariate
  y0 = rnorm(N, mean = x),        # control potential outcome
  y1 = y0 + 0.35,                 # treatment potential outcome
  z = complete_ra(N),             # complete random assignment to treatment
  y = ifelse(z, y1, y0),          # observed outcome
  
  # We will also consider clustered data
  clust = sample(rep(letters[1:20], each = 5)),
  z_clust = cluster_ra(clust),
  y_clust = ifelse(z_clust, y1, y0)
)


###### Regression in R ###### 

m <- lm(y~z + x, data = dat)
tidy(m)

# The correct default 
m <- lm_robust(y~z + x, data = dat, se_type = "HC0")
tidy(m)

# With cluster assignment 
m2 <- lm_robust(
  y_clust ~ z_clust + x,
  data = dat,
  clusters = clust
)
tidy(m2)


# The Lin Estimator. Good to know, but not likely we will use it often 
m3 <- lm_lin(
  y~z,
  covariates = ~x,
  data = dat
)
tidy(m3)


##### Plotting Regression Coefficients ##### 
m %>%
  tidy %>%
  filter(term != "(Intercept)")%>%
  ggplot(aes(x = term, y = estimate))+
  geom_point()+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high))+
  geom_hline(yintercept = 0)+
  coord_flip()
