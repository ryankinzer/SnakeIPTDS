# Load Packages ---- 
library(shiny)
library(shinydashboard)
library(tidyverse)
library(sf)
library(leaflet)
library(leafpop)
library(htmlTable)
library(shinyjs)
library(DT)

# GitHub packages
#library(cdmsR)
#library(cuyem)

# Source
source('./R/iptds_ui.R')
source('./R/iptds_server.R')
source('./R/mapFun.R')

# Javascript for "Enter" button ----
jscode <- '
$(document).keyup(function(event) {
if ((event.keyCode == 13)) {
$("#login").click();
}
});
'

# load points, rivers and polygons
load("./data/large_rivers.rda")
load("./data/stream_layer.rda")
load('./data/iptds_map.rda')

#load("./data/SR_pops.rda")
#load("./data/site_config.rda")

stadem_data <- stadem_data %>%
  mutate(across(c('spawn_year', 'estimate', 'lower_ci', 'sd', 'upper_ci'), as.numeric)) %>%
  mutate(species = ifelse(species == 'Steelhead', species, 'Chinook salmon'))

spsm_pop <- spsm_pop %>%
  mutate(TRT_POPID = ifelse(grepl('SEM|SEU', TRT_POPID),'SEUMA/SEMEA/SEMOO',TRT_POPID),
         POP_NAME = ifelse(grepl('SEM|SEU', TRT_POPID),'Selway Aggregate',POP_NAME),
         MPG = ifelse(grepl('SEM|SEU', TRT_POPID),'Wet Clearwater',MPG)) %>%
  group_by(MPG, POP_NAME, TRT_POPID) %>%
  summarise(area_sq = sum(Shape_Area)) %>%
  ungroup()

krs <- site_est %>% filter(SiteID == 'KRS', species == 'Chinook salmon') %>%
  mutate(TRT_POPID = 'SFSMA',
         valid_estimate = '1') %>%
  mutate(across(c('spawn_year', 'n_tags', 'estimate', 'lower_ci', 'upper_ci'), as.integer)) %>%
  mutate(across(c('sd', 'cv'), as.numeric)) %>%
  select(species, run, spawn_year, TRT_POPID, valid_estimate,
         n_tags, estimate, lower_ci, upper_ci, sd, cv) %>%
  st_set_geometry(NULL)

pop_est <- bind_rows(pop_est, krs)

c_theme <- function(){
  theme(text = element_text(size = 16))
}

# Function to create list of population graphs
popGraph<- function(id){
  
  popFiltered <- pop_est %>%
    #filter(TRT_POPID == 'SFSEC-s') %>%
    filter(TRT_POPID == id) #%>%
  #mutate(year5 = zoo::rollmean(estimate, 2, fill = NA))
  
  yint <- popFiltered %>%
    arrange(TRT_POPID, desc(spawn_year)) %>%
    group_by(TRT_POPID) %>%
    mutate(rank = 1:n(),
           n = n()) %>%
    filter(rank <= 5) %>%
    group_by(TRT_POPID) %>%
    summarise(avg5 = round(mean(estimate))) %>%
    ungroup() %>%
    pull(avg5)
  
  p <- ggplot(popFiltered, aes(x = spawn_year)) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = .5) +
    geom_line(aes(y = estimate)) +
    #geom_line(aes(y = year5)) +
    geom_hline(yintercept = yint, linetype = 2, colour = 'blue') +
    # stat_summary(fun.y = mean, geom = 'line', aes(group = 1)) +
    geom_point(aes(y = estimate)) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    theme_bw() +
    labs(title = paste0(id,': last five-year average = ',yint),
         x = 'Spawn Year',
         y = 'Estimate')
  
  return(p)
}