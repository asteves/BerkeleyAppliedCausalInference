###### Section 2 ##### 

#### Learning Objectives ###### 

### Learn the pattern to create a data frame 
### See how to create functions in R 
### See how to use a simulation to understand unbiasedness of an estimator

###### Libraries ###### 
library(tidyverse) 

## Alternatively if you want to call only the packages we use here explicitly. You only need to run line 10 or lines 13-15
#library(dplyr)
#library(ggplot2)
#library(tibble)



##### Learning Objective 1: Creating our own Data Frame ##### 
## Base R way 
# R doesn't care about the space so we write to make it 
# readable for us
DF_NAME <- data.frame(
  variable1 = c(VALUES),
  variable2 = c(VALUES),
  ... # additional variables defined the same way
  , row.names = FALSE
)

## tidyverse way 
# A tibble is a data frame with some more sensible defaults 
# They work much the same way as data frames with some exceptions
# A tibble requires loading dplyr (or loading tidyverse)
DF_NAME <- tibble(
  variable1 = c(VALUES),
  variable2 = c(VALUES),
  ... # additional variables defined the same way 
)


### TODO Challenge 1 
### Create a data frame with three variables both ways

myDf <- ... 


myTibble <- ... 


# A nice aspect of tibble is that we can define a variable 
# inside the tibble and then use it to make other variables 

### TODO Challenge 2 
### Create a tibble with three variables. Make the second variable equivalent to the first variable + 10 and the third variable equivalent to the first variable + 100 

myTibble2 <- ... 

##### Random Numbers ####### 

## RNG Patterns 
# Random normal distribution
# n is the number of observations you want 
# mean is the center of the distribution 
# sd is the spread 
rnorm(n = N, mean = 0, sd = 1)

# You specify each argument
rnorm(n = N, mean = 200, sd = 47)

# Random uniform distribution 
# n is the same as before 
# min is the start of the distribution 
# max is the end of the distribution  
runif(n = N, min = 0, max = 1)

# Random Binomial distribution 
# n is the same as before 
# size is the number of trials 
# prob is the probability of success on each trial
rbinom(n = N, size = 1, prob = c(0.5, 0.5))


### TODO Challenge 3 
# Generate two different random normal variables. For the first one, draw 100 observations from a standard normal distribution. For the second, draw a different number of observations from a different normal distribution. 

rand1 <- ... 

rand2 <- ... 


########### Dataframes and Research Design Simulations ##### 

### TODO Challenge 4
## Fill in the following tibble to create a data frame for simulation

research_df <- tibble(
  # Create a variable id. Generate 50 sequential units from 1 to 50 
  id = ...,
  
  # Create a variable Y0
  # Generate Y_0 outcomes randomly from a normal distribution centered at 50 with a standard deviation of 10
  # 
  Y0 = ..., 
  
  # Create a variable Y1 
  # Generate Y_1 outcomes as a constant positive treatment effect of 5 
  Y1 = ...,
  
  # Create a variable ITE 
  # Generate the outcome as the Individual Treatment effect
  # For each observation 
  ITE = ..., 
  
  # Create a variable treat 
  # Make all even variables assigned to 1 and all odds 
  # assigned to 0.
  # What do you think the arguments for the function on the
  # RHS of the assignment are? 
  treat = if_else(id %% 2 == 0, 1, 0),
  
  # Create a variable Y_obs 
  # What do you think the function is doing here 
  YObs = if_else(treat == 1, Y1, Y0)
  
)


#### Learning Objective 2: Functions ######

### Function pattern 
myFunc <- function(arg1, arg2, ...){
  ### stuff your function does 
  
  ## what your function outputs
  # R functions return one object 
  return(...)
}


### TODO Challenge 5 
## a) Fill in the function addition so that it runs correctly on test data 

addition <- function(arg1, arg2){
  
  return(out)
}

## Test by running this line. Should return TRUE if correct 
addition(2,4) == 6


## b) Fill in the arguments for add4 so that it runs correctly on test data 

addNumber <- function(){
  
  return(start + toAdd)
}

## Test by running this line. Should return TRUE if correct
addNumber(0,4) == 4

## c) Write your own function to multiply two numbers together. Call this function multNum 




## Test by running these three line. Should return TRUE if correct 
multNum(2,2) == 4 
multNum(0, 100) == 0
multNum(100, 0) == 0


###### 3. See how a simulation works to test unbiasedness ##### 

### Create a data frame with a potential outcomes schedule
research_df <- tibble(
  id = 1:50,
  Y0 = rnorm(50, 50, 10),
  Y1 = Y0 + 5, 
  ITE = Y1 - Y0
)

### Write simulation functions 
# Wrapper function to get treatment assignments 
get_treatment_assignment <- function(N){
  random_treat <- sample(
    x = c(rep(1, N/2), 
          rep(0, N/2)), 
    size = N, 
    replace = F)
}

get_ate <- function(df, y1, y0, d){
  
  ## Get groups 
  y1 <- df[[y1]][d == 1]
  y0 <- df[[y0]][d == 0]
  
  ## Conditional Expected Values 
  E_Y1 <- mean(y1, na.rm = T)
  E_Y0 <- mean(y0, na.rm = T)
  
  # Return the difference in means 
  return(E_Y1 - E_Y0)
}

## Wrapper function to simulate an experiment 
sim_dm <- function(df, to, co){
  d <- get_treatment_assignment(nrow(df))
  get_ate(df, to, co, d = d)
}

set.seed(8675309)
dm <- NULL
for(i in 1:10000){
  dm[i] <- sim_dm(research_df_randomized, "Y1", "Y0")
}
mean(dm) # close to 5. This is what we will mean when we talk about unbiasedness as a procedure
sd(dm) # variability of our estimates. 


## Get the true ATE programmatically 
## This will be equal to 5 
true_ate <- research_df %>% 
  summarise(ate = mean(Y1, na.rm = T) - mean(Y0, na.rm = T))%>%
  pull()

## Make a plot of the sampling variability 
tibble(dm = dm)%>% 
  ggplot()+
  geom_histogram(mapping = aes(x = dm), bins = 30)+
  geom_vline(xintercept = true_ate)

### TODO Challenge 6 

## a) Feel comfortable explaining what each step of this simulation is doing. 

## b) Change different aspects of the simulation and see what happens when you rerun the code. 