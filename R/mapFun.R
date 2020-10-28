# gather plotting data----
# get population polygons, graphs and set colors

mapFun <- function(spp){

if(spp == 'Steelhead'){
  full_pop <- sth_pop
} else {
  full_pop <- spsm_pop
}
  
  pop_layer <- full_pop %>%
    inner_join(pop_est %>%
                 arrange(TRT_POPID, desc(spawn_year)) %>%
                 group_by(TRT_POPID) %>%
                 mutate(rank = 1:n(),
                        n = n()) %>%
                 filter(rank <= 5) %>%
                 group_by(TRT_POPID) %>%
                 summarise(avg5 = round(mean(estimate))) %>%
                 ungroup()
    )
  
  p_all <- lapply(unique(pop_layer$TRT_POPID), popGraph)

# get site point estimates, graphs and set colors

site_est_spp <- site_est %>%
  filter(species == spp) %>%
  select(SiteID, SiteName, species, run, spawn_year, estimate, lower_ci, upper_ci, sd, cv) %>%
  st_set_geometry(NULL)

siteTable <- function(x){
  htmlTable(site_est_spp[site_est_spp$SiteID==x,5:10],
            rnames = FALSE,
            header = c('Spawn Year', 'Estimate', 'Lower 95% CI', 'Upper 95% CI',
                       'Std. Dev.', 'CV'),
            caption = paste0(site_est_spp[site_est_spp$SiteID==x,2][1],
                             " (",
                             x,
                             ")")
  )
}

site_tab <- lapply(unique(site_est_spp$SiteID), siteTable)

site_est_layer <- site_est %>%
  filter(species == spp) %>%
  distinct(SiteID, SiteName, SiteTypeName)


  savedlist = list(full_pop, pop_layer, p_all, site_est_spp, site_tab, site_est_layer)
  names(savedlist) = c('full_pop','pop_layer', 'p_all', 'site_est_spp', 'site_tab', 'site_est_layer')
  return(savedlist) 
}