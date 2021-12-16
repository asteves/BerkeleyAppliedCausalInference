######### Section 1 #######

# This is a comment. R ignores everything after the # that you type on a single line without a return 
# Comments are useful as notes to your future self 


### Libraries 
### We make use of the following libraries for this script 
### **Intermediate point of clarification** if you know you only want to use a single package within the tidyverse, you can call it separately. 
library(tidyverse)
library(palmerpenguins)


## Assign the variable df to the value of penguins 
## Practically, this copies the penguins data frame to a new variable 
df <- penguins


##### Make Plots ####### 

### Make a basic scatterplot using the penguins data 
ggplot(data = df) +
  geom_point(mapping = aes(x = flipper_length_mm, y = bill_length_mm))

### Make a scatterplot using the penguins data with 
### square blue points
ggplot(data = df) +
geom_point(mapping = aes(x = flipper_length_mm, y = bill_length_mm), color = "blue", shape = 15)


###### Some practice take aways ###### 

### What happens if we do this? 
ggplot(data = df, aes(x = flipper_length_mm, y = bill_length_mm))+
  geom_point(color = "blue", shape = 15)
### Why do you think the output is the result? 

### Explore some other ggplot() aesthetics 
### available here https://ggplot2.tidyverse.org/articles/ggplot2-specs.html 


### Variable assignment 
# What error message do you get when running the following code? 
2length <- 4 

# What error message do you get when running the following code? 
ggplot(data = dff)+
  geom_point(mapping = aes(x = flipper_length_mm, y = bill_length_mm), color = "blue", shape = 15)

# What error message do you get when running the following code?  
bill_length_mm <- df$bill_length_mm
flipper_length_mm <- df$flipper_length_mm

ggplot(data) + 
  geom_point(mapping = aes(x = flipper_length_mm, y = bill_length_mm), color = "blue", shape = 15)

# Note that we did not explain in class what lines 51 and 52 do explicitly. Can you figure out what they do based on examples you've seen before? 
