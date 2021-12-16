library(tidyverse)

df <- read_csv("YOUR FILE PATH HERE")


## If you're having trouble reading in the data frame
## This code will create it 
df <- tibble(
  id = 1:7,
  y0 = c(NA,15,20,20,10,15,NA),
  y1 = c(15,NA,NA,NA,NA,NA,30),
  assign = c(1,0,0,0,0,0,1)
)

## Subsetting a data frame 
df$id

df[["id"]]

df[,1]

## Subsetting a data frame based on another column 

df$id[df$assign == 1]

df[["id"]][df[["assign"]]==1]

df[,1][df[,4]==1]

## Subsetting a data frame based on another vector 
## outside of a data frame 

example_vec <- c(0,0,0,1,1,0,0)
df$id[example_vec == 1]
df[["id"]][example_vec==1]
df[,1][example_vec==1]

## incidentally the vector does not have be the same length 
e <- c(1,0)

df$id[e == 1]

## For loop examples 
for(i in 1:10){
  print(i)
}

## For loop and store the results in a vector 
val <- NULL 
for(i in 1:10){
  val[i] <- i
}
val


## For loop, store the results, and then call a function on the vector
val <- NULL 
for(i in 1:10){
  val[i] <- i
}
sum(val)

########### Randomization Inference ############

## Getting ATE with subsetting 
treat <- mean(df[["y1"]][df[["assign"]]==1], na.rm = T)
control <- mean(df[["y0"]][df[["assign"]]==0], na.rm = T)
est_ATE <- treat - control

## Getting all possible treatment assignments 

# Line 74 creates a empty matrix that is 100x7 
# to store all the possible assignment vectors 
all_possible_assign <- matrix(nrow = 100, ncol = 7)
dim(all_possible_assign)

for(i in 1:100){
  all_possible_assign[i,] <- sample(c(rep(1,2), rep(0,5)), 7, 
                                    replace = F)
}

# Cut down to just the unique rows 
unique_treats <- unique(all_possible_assign)

# Confirm that we have 252 assignments 
dim(unique_treats)[1] == 21


### Writing a function to get the ATE 
get_ate <- function(df, y1, 
                    y0, d){
  ## Get groups 
  y1 <- df[[y1]][d == 1]
  y0 <- df[[y0]][d == 0]
  
  ## Conditional Expected Values 
  E_Y1 <- mean(y1, na.rm = T)
  E_Y0 <- mean(y0, na.rm = T)
  
  # Return 
  #the difference in means 
  return(E_Y1 - E_Y0)
}

## Example of running a user created function 
example_vec <- c(0,0,0,0, 1,1,1,1)
ex_df <- tibble(
  id = 1:8,
  y0 = seq(from = 1, to = 15, by =2),
  y1 = y0 + 2
)
get_ate(df = ex_df, y1= "y1", 
        y0="y0", d = example_vec)

## Make the Null distribution 
ex1 <- tibble(
  id = 1:7,
  y0 = c(15,15,20,20,10,15,30),
  y1 = y0
)

## The tidy way with mutate 
ex2 <- df %>% 
  mutate(y0 = ifelse(is.na(y0), 
                     y1, 
                     y0),
         y1 = ifelse(is.na(y1), 
                     y0, 
                     y1))

## The base R way 

ex3 <- df 

ex3$y0 <- ifelse(is.na(ex3$y0), 
                 ex3$y1, ex3$y0)
ex3$y1 <- ifelse(is.na(ex3$y1), 
                 ex3$y0, ex3$y0)


## Our Null distribution 
null_df <- df %>% 
  mutate(y0 = ifelse(is.na(y0), 
                     y1, 
                     y0),
         y1 = ifelse(is.na(y1), 
                     y0, 
                     y1))%>%
  # remove the assign column
  select(-assign) 


### Run the randomization distribution 
obs_ate <- 6.5 # our experiment
dm <- NULL 
## Randomize over all possible assignments 
for(i in 1:nrow(unique_treats)){
  dm[i] <- get_ate(null_df, "y1", "y0", d=unique_treats[i,])
}
## p-value 
sum(dm >= obs_ate)/nrow(unique_treats) 


### Visualize the simulation runs 
tibble(dm = dm,
       x = 1:21)%>%
  ggplot()+
  geom_point(aes(x = x,y=dm))+
  geom_hline(yintercept = obs_ate)+
  xlab("Simulation Run")+
  ylab("Estimated ATE")