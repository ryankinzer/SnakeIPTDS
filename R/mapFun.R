# gather plotting data----
# get population polygons, graphs and set colors

mapFun <- function(spp, basemap){

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

  pop_copop <- colorFactor(palette = 'viridis', domain = pop_layer$MPG)

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

pal <- colorFactor(
  palette = topo.colors(9)[1:6],
  domain = site_est_layer$SiteTypeName
)

# map----

 m <- basemap %>%
    addPolygons(data = pop_layer,
                label = ~TRT_POPID,
                layerId = ~TRT_POPID,
                group = 'Populations',
                labelOptions = labelOptions(noHide = F, textsize = 12),
                fillColor = ~pop_copop(MPG), weight = 1, fillOpacity = .5) %>%
    addPopupGraphs(p_all, group = 'Populations') %>%
    addPolygons(data = full_pop, group = 'Populations', fill = NA, stroke = TRUE,
                color = 'black', weight = 2, opacity = 1, fillOpacity = 0) %>%
    addCircleMarkers(data = site_est_layer, radius = 5,
                     group = 'Sites',
                     popup = site_tab,
                     color = ~pal(site_est_layer$SiteTypeName),
                     label = ~SiteID,
                     layerId = ~SiteID,
                     labelOptions = labelOptions(noHide = F),
                     popupOptions = popupOptions(noHide = F, minWidth = 400, maxWidth = 400)) %>%
    addLegend(data = site_est_layer, position = "bottomleft", pal = pal, values = ~SiteTypeName,
              title = "Observation Type",
              opacity = 1) %>%
  addLegend(data = pop_layer, position = "bottomleft", pal = pop_copop, values = ~MPG,
            title = "Major Population Group",
            opacity = .5) %>%
    addLayersControl(
      overlayGroups = c("Populations", "Sites"),
      options = layersControlOptions(collapsed = FALSE)
    )
 
  return(m) 
}