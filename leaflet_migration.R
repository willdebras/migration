# library(dplyr)
# 
# set.seed(1983)
# 
# df <- data_frame(origins = sample(c('Florida', 'Mississippi', 'Washington', 'California'), 
#                                   size = 100, replace = TRUE), 
#                  destinations = sample(c('Texas', 'New Jersey', 'Colorado', 'Minnesota'), 
#                                        size = 100, replace = TRUE))

library(openxlsx)
library(dplyr)

migration <- openxlsx::read.xlsx("H:\\New fldr\\migration_flow_clean.xlsx")

mig2 <- migration[,-c(2:9)]
mig3 <- mig2[,c(FALSE, TRUE)]

colnames(mig3) <- mig3[1,]

mig4 <- mig3[-c(1,2),]

states <- as.data.frame(migration[-c(1,2),1])

colnames(states) <- "destinations"

migration2 <- cbind(states, mig4)

mig_long <- migration2 %>%
  tidyr::pivot_longer(-destinations, names_to = "origins", values_to = "count")

mig_un <- mig_long[-c(1:55),]

mig_states <- mig_un %>%
  dplyr::select(origins, destinations, count)


# head(df)
# 
# df2 <- df %>%
#   group_by(origins, destinations) %>%
#   summarize(counts = n()) %>%
#   ungroup() %>%
#   arrange(desc(counts))

library(leaflet)
library(RColorBrewer)

library(rnaturalearth) # devtools::install_github('ropenscilabs/rnaturalearth')

# countries <- ne_countries()

states <- ne_states(iso_a2 = 'US')

states_xy <- states@data %>%
  select(name, longitude, latitude)


library(rgdal)

mig_lat <- mig_states %>%
  left_join(states_xy, by = c('origins' = 'name')) %>%
  left_join(states_xy, by = c('destinations' = 'name'))

mig_clean <- mig_lat %>%
  dplyr::filter(!is.na(longitude.x),
                !is.na(longitude.y),
                !count=="N/A",
                !count==0)

mig_weight <- mig_clean %>%
  mutate(weight = scales::rescale(as.numeric(count), to = c(0.02, 10)),
         count = as.numeric(count),
         origins = as.factor(origins),
         destinations = as.factor(destinations))

mig_ori <- mig_weight %>%
  group_by(origins) %>%
  summarise(state_ori = sum(count))

mig_dest <- mig_weight %>%
  group_by(destinations) %>%
  summarise(state_dest = sum(count))

mig_diff <- left_join(mig_dest, mig_ori, by=c("destinations" = "origins")) %>%
  mutate(diff = abs(state_ori - state_dest))

mig_weight_top <- mig_weight %>%
  dplyr::filter(weight > 2.19)

mig_weight_bot <- mig_weight %>%
  dplyr::filter(weight <= 2.19)



# countries$longitude <- coordinates(countries)[,1]
# 
# countries$latitude <- coordinates(countries)[,2]
# 
# countries_xy <- countries@data %>%
#   select(admin, longitude, latitude)
# 



# df3 <- df2 %>%
#   left_join(states_xy, by = c('origins' = 'name')) %>%
#   left_join(states_xy, by = c('destinations' = 'name'))
# 
# df3$longitude.y <- as.numeric(as.character(df3$longitude.y))
# 
# df3$latitude.y <- as.numeric(as.character(df3$latitude.y))
# 
# head(df3)


library(geosphere)

flows <- gcIntermediate(mig_weight[,4:5], mig_weight[,6:7], sp = TRUE, addStartEnd = TRUE)

# flows2 <- gcIntermediate(df3[,4:5], df3[,6:7], sp = TRUE, addStartEnd = TRUE)
# flows2counts <- df3$counts
# flows2origins <- df3$origins


flows$counts <- mig_weight$count
flows$origins <- mig_weight$origins
flows$destinations <- mig_weight$destinations
flows$weight <- mig_weight$weight

hover <- paste0(flows$origins, " to ", 
                flows$destinations, ': ', 
                as.character(flows$counts))



flows_top <- gcIntermediate(mig_weight_top[,4:5], mig_weight_top[,6:7], sp = TRUE, addStartEnd = TRUE)

flows_top$counts <- mig_weight_top$count
flows_top$origins <- mig_weight_top$origins
flows_top$destinations <- mig_weight_top$destinations
flows_top$weight <- mig_weight_top$weight

flows_bot <- gcIntermediate(mig_weight_bot[,4:5], mig_weight_bot[,6:7], sp = TRUE, addStartEnd = TRUE)

flows_bot$counts <- mig_weight_bot$count
flows_bot$origins <- mig_weight_bot$origins
flows_bot$destinations <- mig_weight_bot$destinations
flows_bot$weight <- mig_weight_bot$weight


hover_top <- paste0(flows_top$origins, " to ", 
                    flows_top$destinations, ': ', 
                    as.character(flows_top$counts))

hover_bot <- paste0(flows_bot$origins, " to ", 
                    flows_bot$destinations, ': ', 
                    as.character(flows_bot$counts))

pal <- colorFactor(brewer.pal(4, 'Set2'), flows$origins)

library(shiny)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 26px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Most Common Migrations in the U.S. in 2018")
)  


leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolylines(data = flows_bot, weight = ~weight,
               fillOpacity = .2,
               group = ~origins, color = "#d3d3d3") %>%
  addPolylines(data = flows_top, label = hover_top, 
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "15px",
                 direction = "auto"),
               weight = ~weight,
               opacity = .6,
               group = ~origins, color = "#1167b1",
               highlight = highlightOptions(opacity = 0.8, color = "red", bringToFront = TRUE)) %>%
  # addLayersControl(overlayGroups = unique(flows$destinations),
  #                  #baseGroups = c("origin", "destination"),
  #                  options = layersControlOptions(collapsed = FALSE)) %>%
  setView(-98.5795, 39.8283, zoom = 3.5) %>%
  addControl(title, position = "topleft", className="map-title")# %>%
  #addCircles(data = mig_weight_top, lng = ~longitude.x, lat = ~latitude.x, color = "#1167b1", weight = 12)
  
  
