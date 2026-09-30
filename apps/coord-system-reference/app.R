library(shiny)
library(tidyverse)
library(sf)
# library(leaflet)
library(DT)
library(kableExtra)
library(htmltools)
library(leaflet.extras)
library(mapgl)

# Standard colors
nn_blue = "#007DB5"

fips_epsg_ref = read_rds("data/us_spcs_epsg_ref.rds")
us_spcs_shp = read_rds("data/us_spcs_shp.rds")

ui <- fluidPage(
    
    title = "NN Coordinate System Reference",
    
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    waiter::use_waiter(),
    shinyjs::useShinyjs(),

    sidebarLayout(
        sidebarPanel = sidebarPanel(
            width=6,
            h4("Click on a State Plane Coordinate System Boundary to See EPSG Codes and Other Details"),
            HTML("<p>We recommend you select the lowest numbered EPSG code that will work for your needs, based on units and location. All available EPSG code information sourced from <a href='https://wiki.spatialmanager.com/index.php/Coordinate_Systems_objects_list' target='_blank'>here.</a></p>"),
            h3(textOutput("spcs_name")),
            div(DTOutput("epsg_table"),style="font-size:80%")
        ),
        mainPanel = mainPanel(
            width=6,
            mapboxglOutput("coord_sys_map")
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$coord_sys_map = renderMapboxgl({
      
      mapboxgl(
        style = mapbox_style("light"),
        bounds = us_spcs_shp
      ) %>% 
        add_fill_layer(id = "uc_spcs_fill",
                       source = us_spcs_shp,
                       fill_color = nn_blue, fill_opacity = 0.5,
                       # fill_outline_color = "white", fill_outline_opacity = 1, fill_outline_width = 1,
                       hover_options = list(fill_opacity = 0.75),
                       tooltip = "zonename") %>% 
        add_line_layer(id = "uc_spcs_line",
                       source = us_spcs_shp,
                       line_color = "white",
                       line_width = 1,
                       line_opacity = 1,
                       hover_options = list(line_width = 3))
      
      
        # leaflet() %>%
        #     addProviderTiles("CartoDB.Positron") %>%
        #     addPolygons(data = us_spcs_shp,
        #                 fillColor = nn_blue,
        #                 color = "white",
        #                 fillOpacity = 0.5,opacity = 1,
        #                 weight=1,
        #                 highlightOptions = highlightOptions(fillOpacity = 0.75,weight=3),
        #                 label =~ zonename,
        #                 layerId =~ spcs_fips_code) %>%
        #     addSearchOSM()
    })
    
    clicked_fips_code <- reactive({
      click <- input$coord_sys_map_feature_click
      req(click, click$properties$spcs_fips_code)
      click$properties$spcs_fips_code
    })
    
    output$spcs_name <- renderText({
      fips <- clicked_fips_code()
      paste0(us_spcs_shp$zonename[us_spcs_shp$spcs_fips_code == fips], " EPSG Codes:")
    })
    
    # output$spcs_name = renderText({
    #     clicked_fips_code = input$coord_sys_map_shape_click$id
    #     
    #     req(!is.null(clicked_fips_code))
    #     
    #     out_string = paste0(
    #         us_spcs_shp$zonename[us_spcs_shp$spcs_fips_code == clicked_fips_code]," EPSG Codes:")
    #     
    #     out_string
    # })
    
    output$epsg_table = renderDT({
      fips <- clicked_fips_code()
      
      
        # clicked_fips_code = input$coord_sys_map_shape_click$id
        
        # req(!is.null(clicked_fips_code))
        #print(clicked_fips_code)
        
      sub_epsg_ref = fips_epsg_ref %>%
          filter(spcs_fips_code == fips) %>%
          mutate(numeric_epsg_code = as.numeric(epsg_code)) %>%
          arrange(units,numeric_epsg_code) %>%
          select(epsg_code,spcs_fips_code,units,desc_string) %>%
          rename(description_string=desc_string) %>%
          rename_with(function(x){
              
              str_replace_all(x,"_"," ") %>%
                  str_to_upper()
          })
      
      req(nrow(sub_epsg_ref)>0)
      
      sub_epsg_ref %>%
          datatable(
              #style = "bootstrap",class='table-bordered table-condensed',
                    options = list(paging=FALSE,scrollY="500px",
                                   scrollX="100%"),
                    rownames = FALSE)
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
