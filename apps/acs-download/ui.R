fluidPage(
  title = "NN Census Data Download",
  
  waiter::use_waiter(),
  shinyjs::useShinyjs(),
  
  absolutePanel(top=25,right=100,draggable = TRUE,
                width="15%", style = "z-index:5000; min-width: 300px;",
                
                wellPanel(
                  radioButtons("parent_geog_type_select","1) Select Parent Geographic Level",
                               choices = parent_geog_levels,
                               width = "100%")
                )
  ),
  
  
  
  absolutePanel(top=190,right=100,draggable = TRUE,
                width="15%", style = "z-index:3000; min-width: 300px;",
                wellPanel(
                  radioButtons("ct_year_select","2) Are you downloading data for Connecticut on and before 2021?",
                              choices = c("No", 
                                          "Yes"), selected = "No",
                              width = "100%"
                              )
                )
  ),
  
  
  
  absolutePanel(top=360,right=100,draggable = TRUE,
                width="15%", style = "z-index:5000; min-width: 300px;",
                
                wellPanel(
                  pickerInput("state_select","3) Select State(s)",
                              choices = state_meta$name,
                              multiple = TRUE,
                              width = "100%")
                )
  ),
  absolutePanel(top=500,right=100,draggable = TRUE,
                width="15%", style = "z-index:4000; min-width: 300px;",
                wellPanel(
                  pickerInput("parent_geog_select","4) Select Parent Geographies from Picker or Map",
                              choices = character(0), selected = NULL,
                              width = "100%",
                              multiple = TRUE,
                              options = list(
                                `actions-box` = TRUE,
                                size = 10
                              ))
                )
  ),
  
  fluidRow(mapboxglOutput("acs_download_map",height=700)),
  fluidRow(
    wellPanel(
      fluidRow(
        h4("5) Select Data Source, then Select Year(s), Geographic Level, and Variables of Interest. You can select Variables of Interest directly in the table or from the picker dropdown."),
        p("Connecticut changed its County definition to Planning Regions. 
          Please download data for CT before and after 2022 separately. 
          Please select 'Yes' for Question 2 if you need to download 2020 Decennial Census data for CT. 
          "),
        p("
          Note that all variables include multiple columns based on standard NN analysis procedures. 
          Note which years tables are available for. Contact Meng Gao if you have questions or want to add more variables.")
        #,
       # HTML("<p>Request additional features <a href = 'https://forms.gle/sEZT7CSUxSdQorJu9' target='_blank'>here.</a></p>")
      
      
      #,
      # HTML("<p>Request additional features <a href = 'https://forms.gle/sEZT7CSUxSdQorJu9' target='_blank'>here.</a></p>")
    )
      ,
      tabsetPanel(
        id = "tab_data_source",
        tabPanel(title = acs_year_name,
                 icon = icon("magnifying-glass"),
                 
                 br(),
                 em("Note: Block Group level data is only available from 2013 onward."),
                 
                 fluidRow(
                   column(3,
                          pickerInput("year_select","Select Year(s) of Estimates",
                                      choices = rev(years_available),
                                      selected = tail(years_available,n=1),
                                      multiple = TRUE,
                                      options = list(
                                        `actions-box` = TRUE,
                                        size = 10
                                      ))
                   ),
                   column(4,
                          radioGroupButtons("geog_level","Geographic Level of Estimates",
                                            choices = character(0),
                                            selected = character(0))
                   ),
                   column(5,
                          pickerInput("voi_select","Variables of Interest",
                                      choices = voi_ref %>%
                                        filter(block_groups_bool) %>%
                                        pull(voi_label),
                                      multiple = TRUE,
                                      options = list(
                                        `actions-box` = TRUE,
                                        size = 10
                                      ))
                   )
                 ),
                 fluidRow(DTOutput("voi_desc"))
                 
        ),
        tabPanel(title = "2020 Census Data",
                 icon = icon("book-atlas"),
                 
                 br(),
                 
                 fluidRow(
                   column(6,
                          radioGroupButtons("census_geog_level","Geographic Level of Estimates",
                                            choices = character(0),
                                            selected = character(0))
                   ),
                   column(6,
                          pickerInput("census_voi_select","Variables of Interest",
                                      choices = census_voi_ref %>%
                                        pull(voi_label),
                                      multiple = TRUE,
                                      options = list(
                                        `actions-box` = TRUE,
                                        size = 10
                                      ))
                   )
                 ),
                 fluidRow(DTOutput("census_voi_desc"))
        )
      ),
    )
  ),
  fluidRow(
    wellPanel(
      h4("5) Set Download Preferences and Download!"),
      textInput("email_input","Please add NN email for usage tracking. This data will be used to improve the tool.",
                placeholder = "jdoe@nelsonnygaard.com"),
      checkboxInput("include_geom_checkbox","Include Geometry in Download? (CAUTION: This will add time to complete query)",
                    value = FALSE),
      fluidRow(actionButton("prepare_download","Compile Dataset",icon = icon("filter")),
               downloadButton("download_uscb","Download Dataset", style = "background-color: #83F28F"))
    )
  )
  
)