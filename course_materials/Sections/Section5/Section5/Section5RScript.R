library(tidyverse)
library(estimatr)
library(haven)
library(sf)
library(patchwork)

world <- st_read("world_countries_boundary_file_world_2002.shp")
mapnames <- read_csv("mapnames_filled2.csv")

jensenData <- read_csv("jensenData.csv")



effectiveWeights <- function(Y, controls, data){
  # Make the OLS formula call 
  treat_formula <- reformulate(termlabels = c(controls), 
                               response = Y)
  # Run a regression of the treatment on the controls 
  treat.model <- lm(as.formula(treat_formula), data = data)
  # Extract the residuals from that regression 
  d.tilde <- as.numeric(residuals(treat.model))
  
  # Square the residuals. 
  weights <- d.tilde^2
  return(weights)
}


### Variables Used 
X.vars <- c("var5",
            "market","lgdppc",
            "gdpgrowt","tradeofg",
            "overallb","generalg",
            "country", "d2","d3")

w <- effectiveWeights(Y = "regime", controls =X.vars, data = jensenData)


df <- tibble(weight = w, 
             country = jensen.cc$country)


## Step 5: Getting the average of weights by country 
weights <- df %>% 
  group_by(country)%>%
  summarise(avg = round(mean(weight),4))


## Getting map weights requires joining our map data with our weights 
mapWeights <- mapnames %>% 
  left_join(weights, by = c("jensen"="country"))%>%
  na.omit()

output <- world %>% 
  left_join(mapWeights, by = c("NAME"="mapname"))%>%
  filter(NAME != "Antarctica")%>%
  mutate(weight = if_else(is.na(avg), 0,avg),
         expW = if_else(is.na(avg), 0, 1))


## Make plots 

## Nominal plot 
ns <- ggplot(output)+
  geom_sf(aes(fill = expW))+
  scale_fill_gradient(low = "white", high = "black")+
  theme_void()+
  theme(legend.position = "none")+
  ggtitle("Nominal Sample")

## Effective Sample plot 
es <- ggplot(output)+
  geom_sf(aes(fill = weight))+
  scale_fill_gradient(low = "white", high = "black")+
  theme_void()+
  theme(legend.position = "none")+
  ggtitle("Effective Sample")

## This comes from patchwork 
## It is a way of aligning ggplots
plots <- ns / es
plots + plot_annotation(
  title = "The difference between Effective and Nominal Samples"
)
