library(shiny)
library(shinyjs)
library(shticky)
library(sigmajs)
library(waypointer)
library(graphTweets)
library(spdplyr)
library(sp)
library(leaflet.extras)

# source("./data/network.R")
# source("functions.R")

OFFSET <- "50%"
ANIMATION <- "slideInUp"


# creates 100vh div
longdiv <- function(...){
  div(
    ...,
    class = "container",
    style = "height:100vh;z-index:500;"
  )
}

library(openxlsx)
library(dplyr)

# edge_colors <- colorRampPalette(c("#575b73", "#8592b0", "#8da0bb"))(9)
# 
# months <- c("April", "May", "June", "July", "August", "September", "October", "November", "December")

# add data.
# add_data <- function(wp){
#   
#   n <- nodes %>% 
#     filter(appear == wp)
#   
#   e <- edges %>% 
#     filter(appear == wp)
#   
#   ec <- edges %>% 
#     filter(appear <= wp) %>% 
#     mutate(
#       new_color = edge_colors[wp]
#     )
#   
#   sigmajsProxy("graph") %>% 
#     sg_force_kill_p() %>% 
#     sg_read_nodes_p(n, id, label, color, size) %>% 
#     sg_read_edges_p(e, id, source, target, weight) %>% 
#     sg_read_exec_p() %>% 
#     sg_change_edges_p(ec, new_color, "color") %>% 
#     sg_refresh_p() %>% 
#     sg_force_start_p(strongGravityMode = TRUE, slowDown = 5)
# }
# 
# render_month <- function(wp, cl = "dark"){
#   tagList(
#     h1(months[wp], class = paste(cl, "big")),
#     render_count(wp)
#   )
# }
# 
# .get_tweet_count <- function(wp){
#   n_tweets %>% 
#     filter(created_at == wp) %>% 
#     pull(cs) %>% 
#     prettyNum(big.mark = ",") %>% 
#     span(
#       class = "sg-dark emph"
#     )
# }
# 
# render_count <- function(wp, cl = "dark"){
#   p(
#     "By", months[wp], .get_tweet_count(wp), "Twitter users have tweeted about #tidytuesday.",
#     class = cl
#   )
# }



migration <- openxlsx::read.xlsx("./data/migration_flow_clean.xlsx")
# migration <- openxlsx::read.xlsx("scrolly/data/migration_flow_clean.xlsx")

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

# library(rnaturalearth) # devtools::install_github('ropenscilabs/rnaturalearth')
# 
# # countries <- ne_countries()
# 
# states <- ne_states(iso_a2 = 'US')
# 
# states_xy <- states@data %>%
#   select(name, longitude, latitude)


# save(states_xy, file = "C:\\Users\\Bonnell-William\\Documents\\migration\\scrolly\\data\\states_xy.rda")

# states_xy <- load(file = "./data/states_latlona.rda")
# load(file = "scrolly/data/states_xy.rda")

load(file = "./data/states_xy.rda")


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
  mutate(weight = scales::rescale(as.numeric(count), to = c(0.02, 10)))

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

hover_fl <- subset(hover, stringr::str_detect(hover, "to Florida"))
hover_ny <- subset(hover, stringr::str_detect(hover, "New York to"))

pal <- colorFactor(brewer.pal(4, 'Set2'), flows$origins)


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

tags$head(
  tags$style(HTML(".leaflet-container { background: #f00; }"))
)





ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$link(
      rel = "stylesheet",
      href = "https://use.fontawesome.com/releases/v5.8.1/css/all.css", 
      integrity = "sha384-50oBUHEmvpQ+1lW4y57PTFmhCaXp0ML5d60M1M7uH2+nqUivzIebhndOJK28anvf",
      crossorigin = "anonymous"
    )
  ),
  use_shticky(),
  use_waypointer(),
  div(
    id = "bg",
    div(
      id = "stick",
      style = "position:fixed;width:100%;",
      fluidRow(
        column(5),
        # column(8, sigmajsOutput("graph", width = "100%", height = "100vh"))
        # column(8, leafletOutput("leaf", width = "100%", height = "100vh"))
        column(7, leafletOutput("leaf", width = "100%", height = "100vh"))
      )
    ),

    # longdiv(
    #   h1("Recent Migration Patterns in the U.S.", class = "title"),
    #   br(),
    #   br(),
    #   h1(
    #     class = "subtitle",
    #     "Each ", tags$i(class = "fas fa-slash sg"), "represents a flow in migration from state to state and the width represents the level of flow."
    #   ),
    #   br(),
    #   p(
    #     style = "text-align:center;",
    #     "Using data from the U.S. Census", 
    #     tags$a(
    #       class = "sg",
    #       tags$i(class = "fas fa-external-link-alt"),
    #       target = "_blank",
    #       href = "https://www.census.gov/data/tables/time-series/demo/geographic-mobility/state-to-state-migration.html"
    #     )
    #   ),
    #   br(),
    #   br(),
    #   br(),
    #   p(
    #     style = "text-align:center;",
    #     tags$i(class = "fas fa-chevron-down fa-3x")
    #   ),
    #   br(),
    #   br()
    # 
    # ),
    
    longdiv(style = "width:100%;",
            id = "top",
            uiOutput("top")
      
    ),
    
    longdiv(style = "width:100%;",
            div(

              )

            ),
    
    
    longdiv(style = "width:100%;",
      div(
        id = "m1",
        fluidRow(
          column(1),
          column(4, uiOutput("1"))
          
        )
        

      )
    ),
    longdiv(style = "width:100%;",
            div(
              id = "m2",
              fluidRow(
                column(1),
                column(4, uiOutput("2"))
                
              )
              
              
            )
    ),
    
    longdiv(style = "width:100%;",
            div(
              id = "m3",
              fluidRow(
                column(1),
                column(4, uiOutput("3"))
                
              )
              
              
            )
    ),
    
    longdiv(style = "width:100%;",
            div(
              id = "m4",
              fluidRow(
                column(1),
                column(4, uiOutput("4"))
                
              )
              
              
            )
    ),
    
    longdiv(style = "width:100%;",
            div(
                id = "m5",
                fluidRow(
                  column(1),
                  column(4, uiOutput("5"))

                )


            )
    )#,

    # longdiv(style = "width:100%;",
    #         div(
    #           id="m6",
    #           fluidRow(
    #             column(1),
    #             column(4, uiOutput("6"))
    # 
    #           )
    # 
    # 
    #         )
    # )
    
    # longdiv(
    #   div(
    #     id = "m5",
    #     uiOutput("5"),
    #     uiOutput("aug")
    #   )
    # ),
    # 
    # longdiv(
    #   div(
    #     id = "m6",
    #     uiOutput("6"),
    #     uiOutput("sep")
    #   )
    # ),
    # 
    # longdiv(
    #   div(
    #     id = "m7",
    #     uiOutput("7"),
    #     uiOutput("oct")
    #   )
    # ),
    # 
    # longdiv(
    #   div(
    #     id = "m8",
    #     uiOutput("8"),
    #     uiOutput("nov")
    #   )
    # ),
    # 
    # longdiv(
    #   div(
    #     id = "m9",
    #     uiOutput("9"),
    #     uiOutput("dec")
    #   )
    # ),
    

    

  )
)

server <- function(input, output, session) {

  top <- Waypoint$
    new("top", offset = OFFSET, animate = TRUE, animation = ANIMATION)$
    start()
  
  w1 <- Waypoint$
    new("m1", offset = OFFSET, animate = TRUE, animation = ANIMATION)$
    start()
  w2 <- Waypoint$
    new("m2", offset = OFFSET, animate = TRUE, animation = ANIMATION)$
    start()
  w3 <- Waypoint$
    new("m3", offset = OFFSET, animate = TRUE, animation = ANIMATION)$
    start()
  w4 <- Waypoint$
    new("m4", offset = OFFSET, animate = TRUE, animation = ANIMATION)$
    start()
  
  w5 <- Waypoint$
    new("m5", offset = OFFSET, animate = TRUE, animation = ANIMATION)$
    start()
  
  # w6 <- Waypoint$
  #   new("m6", offset = OFFSET, animate = TRUE, animation = ANIMATION)$
  #   start()
  # w7 <- Waypoint$
  #   new("m7", offset = OFFSET, animate = TRUE, animation = ANIMATION)$
  #   start()
  # w8 <- Waypoint$
  #   new("m8", offset = OFFSET, animate = TRUE, animation = ANIMATION)$
  #   start()
  # w9 <- Waypoint$
  #   new("m9", offset = OFFSET, animate = TRUE, animation = ANIMATION)$
  #   start()
  
  # w10 <- Waypoint$
  #   new("m10", offset = OFFSET, animate = TRUE, animation = ANIMATION)$
  #   start()
  
  output$`top` <- renderUI({
    
    div(
      h1("Recent Migration Patterns in the U.S.", class = paste("title")),
      br(),
      br(),
      h1(
        class = "subtitle",
        "Each ", tags$i(class = "fas fa-slash sg"), "represents a flow in migration from state to state and the width represents the level of flow."
      ),
      br(),
      p(
        style = "text-align:center;",
        "Using data from the U.S. Census", 
        tags$a(
          class = "sg",
          tags$i(class = "fas fa-external-link-alt"),
          target = "_blank",
          href = "https://www.census.gov/data/tables/time-series/demo/geographic-mobility/state-to-state-migration.html"
        )
      ),
      br(),
      br(),
      br(),
      p(
        style = "text-align:center;",
        tags$i(class = "fas fa-chevron-down fa-3x")
      ),
      br(),
      br()
    )
    
  })

  output$`1` <- renderUI({
    req(w1$get_triggered())
    if(w1$get_triggered() == TRUE)
      
      tagList(
      h1("Migration in 2018", class = paste("dark", "big")),
      h4(
        HTML("<i>\"The migrant people, scuttling for work, scrabbling to live, looked always for pleasure, dug for pleasure, manufactured pleasure, and they were hungry for amusement.\"</i>"),
        class = "dark"
      ),
      br(),
      h3(HTML("In 2018, nearly <span>4,000,000</span> migrated from one state to another for various push and pull factors. Most people migrate for economic reasons, 
              often seeking new opportunities for themselves or their family."
              ), class = "dark"
        ),
      br(),
      h4(HTML("Scroll on the left side to continue reading. Scroll on the right side to view the map more closely."
      )
      )
      
      )
  })
  


  
  observeEvent(w1$get_direction(), {
    if(w1$get_direction() == "down") 
      
      output$`top` <- renderUI({
        
        div(class = "newtop",
            br(),
            br(),
            h1("Recent Migration Patterns in the U.S.", class = paste("dark", "big")),
            br(),
            br(),
            h3(
              "Each ", tags$i(class = "fas fa-slash sg"), "represents a flow in migration from state to state and the width represents the level of flow."
            ),
            br(),
            h3(
              "Using data from the U.S. Census", 
              tags$a(
                class = "sg",
                tags$i(class = "fas fa-external-link-alt"),
                target = "_blank",
                href = "https://www.census.gov/data/tables/time-series/demo/geographic-mobility/state-to-state-migration.html"
              )
            ),
            br(),
            br(),
            br(),
            p(
              tags$i(class = "fas fa-chevron-down fa-3x")
            ),
            br(),
            br()
        )
        
      })
    
  })
  
  
  output$`2` <- renderUI({
    req(w2$get_triggered())
    if(w2$get_triggered() == TRUE) 
      
      tagList(
        h1("Top Moves in Migration", class = paste("dark", "big")),
        h3(
          "Large groups of Americans moved up and down the East and West Coast. Some of the largest moves were from New York to Florida and New Jersey and California to Washington and Oregon. Others picked up their homes and moved entirely across regions, migrating from New York to California or California to Texas.",
          class = "dark"
        ),
        br(),
        h4(
           "Try hovering your cursor over the map to see some of the top 50 diadic moves in the U.S. in 2018.")
      )
      
  })
  
  output$`3` <- renderUI({
    req(w3$get_triggered())
    if(w3$get_triggered() == TRUE) 
      
      tagList(
        h1("The Florida Man", class = paste("dark", "big")),
        h4(
          HTML("<i>\"Most changes are not so swift as the fallen house.\", Lauren Groff, Florida</i>"),
          class = "dark"
        ),
        br(),
        h3(HTML("The largest flow in migration happened in Florida in 2018. Nearly <span>600,000</span> people moved to Florida. 
        About <span>1 in 10</span> of those immigres came from New York, twice as many as any other state. 
                "
        )),
        h3(HTML("Large outmigration from New York and large inmigration to Florida will likely impact congressional reapportionment, 
                leading to shift in seats in the House of Representatives."
        ))
       
        
      )
      
  })
  
  output$`4` <- renderUI({
    req(w4$get_triggered())
    if(w4$get_triggered() == TRUE)


      tagList(
        h1("From Empire to Garden", class = paste("dark", "big")),
        h4(
          HTML("<i>\"It is mid-morning here in Skyscraper National Park\", Kurt Vonnegut, Slapstick: Or, Lonesome No More!</i>"),
          class = "dark"
        ),
        br(),
        h3(HTML("New York was th state with the largest differential in migration in 2018 with over 450,000 New York residents leaving to pursue opportunities in other states. Since 2010, <span>1.4 million</span> people have <span>emigrated</span> away from the state.
                "
        )),

        h3(HTML("The largest move this year was to New Jersey, where transplants benefit from average real estate prices per sqft. that are, on average, 1/10th that of home."
        ))





      )

  })
  

  
  output$`5` <- renderUI({
    req(w5$get_triggered())
    if(w5$get_triggered() == TRUE)


      tagList(
        br(),
        br(),
        br(),
        h3("Thank you for reading.", class = "light"),
        h3(
          tags$a(
            "Explore the git repo.",
            class = "sg",
            href = "https://github.com/willdebras/migration"
          )
        )
      )

  })
  
  # output$`5` <- renderUI({
  #   req(w5$get_triggered())
  #   if(w5$get_triggered() == TRUE) render_month(5)
  # })
  # 
  # output$`6` <- renderUI({
  #   req(w6$get_triggered())
  #   if(w6$get_triggered() == TRUE) render_month(6)
  # })
  # 
  # output$`7` <- renderUI({
  #   req(w7$get_triggered())
  #   if(w7$get_triggered() == TRUE) render_month(7)
  # })
  # 
  # output$`8` <- renderUI({
  #   req(w8$get_triggered())
  #   if(w8$get_triggered() == TRUE) render_month(8)
  # })
  # 
  # output$`9` <- renderUI({
  #   req(w9$get_triggered())
  #   if(w9$get_triggered() == TRUE) render_month(9)
  # })



  # Our sticky plot
  shtick <- Shtick$
    new("#stick")$
    shtick()



  
  observeEvent(w1$get_direction(), {
    if(w1$get_direction() == "down") 
      
      output$leaf <- renderLeaflet({
        
        leaflet("leaf", options = leafletOptions(zoomControl = FALSE,
                                                 scrollWheelZoom = FALSE)) %>%
          fitBounds(-125, 16, -75, 49) %>%
          setMaxBounds(-125, 16, -75, 49)
        
        
      })
    
    leafletProxy("leaf") %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addProviderTiles(providers$Stamen.TonerLines,
                       options = providerTileOptions(opacity = 0.35)) %>%
      addPolylines(data = flows_bot, weight = ~weight,
                   fillOpacity = .2,
                   group = ~origins, color = "#d3d3d3")

    
  })
  
  observeEvent(w2$get_direction(), {
    if(w2$get_direction() == "up") 
      
    
    leafletProxy("leaf") %>%
      clearShapes() %>%
      addPolylines(data = flows_bot, weight = ~weight,
                   fillOpacity = .2,
                   group = ~origins, color = "#d3d3d3")
    
  })

  observeEvent(w2$get_direction(), {
    if(w2$get_direction() == "down")
      
      leafletProxy("leaf") %>%
      addPolylines(data = flows_top, label = hover_top,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto"),
                   weight = ~weight,
                   opacity = .6,
                   group = ~origins,
                   color = "#1167b1",
                   highlight = highlightOptions(opacity = 0.8, color = "red", bringToFront = TRUE))

      
  })
  
  observeEvent(w3$get_direction(), {
    if(w3$get_direction() == "up")
      
      leafletProxy("leaf") %>%
      addPolylines(data = flows_top, label = hover_top,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto"),
                   weight = ~weight,
                   opacity = .6,
                   group = ~origins,
                   color = "#1167b1",
                   highlight = highlightOptions(opacity = 0.8, color = "red", bringToFront = TRUE))
    
    
  })

  observeEvent(w3$get_direction(), {
    if(w3$get_direction() == "down")
      
    leafletProxy("leaf") %>%
    clearShapes() %>%
    addPolylines(data = dplyr::filter(flows, destinations %in% "Florida"), label = hover_fl,
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px"),
                   textsize = "15px",
                   direction = "auto"),
                 weight = ~weight,
                 opacity = .6,
                 group = ~origins, color = "#1167b1",
                 highlight = highlightOptions(opacity = 0.8, color = "red", bringToFront = TRUE))
      
      
  })
  
  observeEvent(w4$get_direction(), {
    if(w4$get_direction() == "up")
      
      leafletProxy("leaf") %>%
      clearShapes() %>%
      addPolylines(data = dplyr::filter(flows, destinations %in% "Florida"), label = hover_fl,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto"),
                   weight = ~weight,
                   opacity = .6,
                   group = ~origins, color = "#1167b1",
                   highlight = highlightOptions(opacity = 0.8, color = "red", bringToFront = TRUE))
    
    
  })

  observeEvent(w4$get_direction(), {
    if(w4$get_direction() == "down") 
      
      
      leafletProxy("leaf") %>%
      clearShapes() %>%
      addPolylines(data = dplyr::filter(flows, origins %in% "New York"), label = hover_ny,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto"),
                   weight = ~weight,
                   opacity = .6,
                   group = ~origins, color = "#1167b1",
                   highlight = highlightOptions(opacity = 0.8, color = "red", bringToFront = TRUE))
      
  })
  
  observeEvent(w5$get_direction(), {
    if(w5$get_direction() == "up") 
      
      
      leafletProxy("leaf") %>%
      clearShapes() %>%
      addPolylines(data = dplyr::filter(flows, origins %in% "New York"), label = hover_ny,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto"),
                   weight = ~weight,
                   opacity = .6,
                   group = ~origins, color = "#1167b1",
                   highlight = highlightOptions(opacity = 0.8, color = "red", bringToFront = TRUE))
    
  })
  
  

  # observeEvent(w5$get_direction(), {
  #   if(w5$get_direction() == "down") add_data(5)
  # })
  # 
  # observeEvent(w6$get_direction(), {
  #   if(w6$get_direction() == "down") add_data(6)
  # })
  # 
  # observeEvent(w7$get_direction(), {
  #   if(w7$get_direction() == "down") add_data(7)
  # })
  # 
  # observeEvent(w8$get_direction(), {
  #   if(w8$get_direction() == "down") add_data(8)
  # })


}

shinyApp(ui, server)