shinyServer(function(input, output, session) {
  
  # Initialization -------
  shinyjs::disable("prepare_download")
  user_selection = reactiveValues() #Initialize user selections
  user_selection$parent_geog_geoids = character(0)
  compiled_download_vector = reactiveVal(NULL)
  
  observe({
    shinyjs::toggle("download_uscb", condition = !is.null(compiled_download_vector()))
  })
  
  sel_tab_data_source <- reactive({
    input$tab_data_source
  })
  
  sel_ct_year <- reactive({
    input$ct_year_select
  })
  
  
  sel_parent_geog_type_select <- reactive({
    input$parent_geog_type_select
  })
  
  # Update Geographic Level Selector based upon Parent Geography Type -----------
  observe({
    if(sel_parent_geog_type_select() == "Counties (includes tracts, block groups, & blocks)"){
      updateRadioGroupButtons(session = session,
                              inputId = "geog_level",
                              choices = c("Counties","Tracts","Block Groups"),
                              selected = "Block Groups")
      updateRadioGroupButtons(session = session,
                              inputId = "census_geog_level",
                              choices = c("Counties","Tracts","Block Groups","Blocks"),
                              selected = "Blocks")
    }else if(sel_parent_geog_type_select() == "Census Designated Places"){
      updateRadioGroupButtons(session = session,
                              inputId = "geog_level",
                              choices = "Census Designated Places",
                              selected = "Census Designated Places")
      updateRadioGroupButtons(session = session,
                              inputId = "census_geog_level",
                              choices = "Census Designated Places",
                              selected = "Census Designated Places")
    }else if(sel_parent_geog_type_select() == "Core Based Statistical Areas"){
      updateRadioGroupButtons(session = session,
                              inputId = "geog_level",
                              choices =  "Core Based Statistical Areas",
                              selected =  "Core Based Statistical Areas")
      updateRadioGroupButtons(session = session,
                              inputId = "census_geog_level",
                              choices =  "Core Based Statistical Areas",
                              selected =  "Core Based Statistical Areas")
    }
  })
  
  observe({
    user_selection$parent_geog_type <-  sel_parent_geog_type_select()
  })
  
  observe({
    user_selection$data_source <- sel_tab_data_source()
  })
  
  # Query Parent Geographies Based on State Selected, Update Parent Geography Picker and User Selections--------
  observe({
    
    sel_parent_geog_type_select()
    user_selection$state_names = input$state_select
    user_selection$parent_geog_geoids = character(0)
    
    updatePickerInput(session,
                      "parent_geog_select",
                      choices = character(0),
                      selected = character(0))
    
    if(length(user_selection$state_names)>0){
      
      sel_state_ids = state_meta %>%
        filter(name %in% user_selection$state_names) %>%
        pull(statefp)
      
      user_selection$state_ids = sel_state_ids
      
      if(sel_parent_geog_type_select() == "Counties (includes tracts, block groups, & blocks)" 
         & sel_ct_year() == "Yes"){
        county_query = paste0("SELECT * FROM county_allyear WHERE year IN (",
                              #paste(tail(years_available,n=1)), 
                              "2021",
                              ") AND statefp IN (",
                              paste(paste0("'",sel_state_ids,"'"),collapse = ", "),
                              ");")
        
        county_geoms = st_read(con,query = county_query) %>%
          st_transform(4326) %>%
          left_join(state_meta %>%
                      select(statefp,stusps)) %>%
          mutate(parent_geog_label = paste0(namelsad,", ",stusps))
        
        user_selection$state_parent_geog_geoms = county_geoms
        
        state_county_meta = tbl(con,"county_allyear") %>%
          # filter(year != case_when(!!sel_ct_year() =="Yes" ~ "CT2022",
          #                          TRUE ~ "CT2021")) %>%
          #filter(year %in% years_to_query) %>%
          filter(statefp %in% sel_state_ids) %>%
          distinct(countyfp,geoid,namelsad,statefp) %>%
          collect() %>%
          left_join(state_meta %>%
                      select(statefp,stusps)) %>%
          mutate(parent_geog_label = paste0(namelsad,", ",stusps))
        
        user_selection$state_parent_geog_meta = state_county_meta
        
        updatePickerInput(session,
                          "parent_geog_select",
                          choices = sort(state_county_meta$parent_geog_label))
      }else if (sel_parent_geog_type_select() == "Counties (includes tracts, block groups, & blocks)" 
                & sel_ct_year() == "No") {
        
        # county_query = paste0("SELECT * FROM counties_2022 WHERE year IN ('2021', 'CT2022') AND statefp IN (",
        #                       paste(paste0("'",sel_state_ids,"'"),collapse = ", "),
        #                       ");")
        
        county_query = paste0("SELECT * FROM county_allyear WHERE year IN (",
                              paste(tail(years_available,n=1)),  ") AND statefp IN (",
                              paste(paste0("'",sel_state_ids,"'"),collapse = ", "),
                              ");")
        
        
        county_geoms = st_read(con,query = county_query) %>%
          st_transform(4326) %>%
          left_join(state_meta %>%
                      select(statefp,stusps)) %>%
          mutate(parent_geog_label = paste0(namelsad,", ",stusps))
        
        user_selection$state_parent_geog_geoms = county_geoms
        
        state_county_meta = tbl(con,"county_allyear") %>%
         # filter(year != if (sel_ct_year() == "Yes") "2022" else "2021")%>%
        # state_county_meta = tbl(con,"counties_2022") %>%
        #   filter(year != case_when(!!sel_ct_year() =="Yes" ~ "CT2022",
        #                            TRUE ~ "CT2021")) %>%
          filter(statefp %in% sel_state_ids) %>%
          distinct(countyfp,geoid,namelsad,statefp) %>%
          collect() %>%
          left_join(state_meta %>%
                      select(statefp,stusps)) %>%
          mutate(parent_geog_label = paste0(namelsad,", ",stusps))
        
        user_selection$state_parent_geog_meta = state_county_meta
        
        updatePickerInput(session,
                          "parent_geog_select",
                          choices = sort(state_county_meta$parent_geog_label))
        
        
        
      }
      
      else if(sel_parent_geog_type_select() == "Census Designated Places"){
        cdp_query = paste0("SELECT * FROM places WHERE year = 2021 AND statefp IN (",  #Change 2022 to 2021 cuz errors when yes_geometry
                           paste(paste0("'",sel_state_ids,"'"),collapse = ", "),
                           ");")
        
        cdp_geoms = st_read(con,query = cdp_query) %>%
          st_transform(4326) %>%
          left_join(state_meta %>%
                      select(statefp,stusps)) %>%
          st_as_sf() %>%
          mutate(parent_geog_label = paste0(namelsad,", ",stusps))
        
        user_selection$state_parent_geog_geoms = cdp_geoms
        
        state_cdp_meta = cdp_geoms %>%
          st_drop_geometry() %>%
          distinct(geoid,name,namelsad,statefp) %>%
          left_join(state_meta %>%
                      select(statefp,stusps)) %>%
          mutate(parent_geog_label = paste0(namelsad,", ",stusps))
        
        user_selection$state_parent_geog_meta = state_cdp_meta
        
        updatePickerInput(session,
                          "parent_geog_select",
                          choices = sort(user_selection$state_parent_geog_meta$parent_geog_label))
      }else if(sel_parent_geog_type_select() == "Core Based Statistical Areas"){
        cbsa_query = paste0("SELECT * FROM cbsas_by_state WHERE year = 2021 AND statefp IN (", #Change 2022 to 2021 cuz errors when yes_geometry
                            paste(paste0("'",sel_state_ids,"'"),collapse = ", "),
                            ");")
        
        cbsa_geoms = st_read(con,query = cbsa_query) %>%
          st_transform(4326) %>%
          arrange(geoid) %>%
          group_by(geoid) %>%
          do(head(., n=1)) %>%
          st_as_sf()  %>%
          mutate(parent_geog_label = namelsad)
        
        user_selection$state_parent_geog_geoms = cbsa_geoms
        
        state_cbsa_meta <- cbsa_geoms %>%
          st_drop_geometry() %>%
          distinct(geoid,name,namelsad,lsad,memi,mtfcc,parent_geog_label,statefp) 
        
        user_selection$state_parent_geog_meta = state_cbsa_meta
        
        updatePickerInput(session,
                          "parent_geog_select",
                          choices = sort(user_selection$state_parent_geog_meta$parent_geog_label))
      }
    }
  })
  
  #Initial Plotting of parent geog map upon state selection -------
  output$acs_download_map = renderMapboxgl({
    
    sel_state_names = user_selection$state_names
    sel_parent_geog_type_select()
    
    geom <- user_selection$state_parent_geog_geoms
    
    if(length(sel_state_names)>0){
      
      mapboxgl(
        style = mapbox_style("light"),
        bounds = geom
      ) %>% 
        add_fill_layer(
          id = "geom_fill",
          source = geom,
          # fill_color = "grey",
          fill_color = selection_fill_color(isolate(user_selection$parent_geog_geoids)),
          fill_opacity = 0.7,
          tooltip = "parent_geog_label",
          hover_options = list(fill_opacity = 0.9)
        ) %>% 
        add_line_layer(
          id = "geom_line",
          source = geom,
          line_color = "white",
          line_width = 3,
          hover_options = list(line_width = 5)
        )
      
    }else{
      mapboxgl(
        style = mapbox_style("light"),
        center = c(-92.6983399, 50.3495809),
        zoom = 3
      )
      
    }
  })
  
  # Parent Geography Selection Reactivity ----------------------------
  
  ## Update county selection in picker based on map ------------------
  observeEvent(input$acs_download_map_feature_click, {
    
    click <- input$acs_download_map_feature_click
    clicked_geoid <- click$properties$geoid
    req(clicked_geoid)
    
    if(clicked_geoid %in% user_selection$parent_geog_geoids) {
      # Deselect
      user_selection$parent_geog_geoids = setdiff(user_selection$parent_geog_geoids,
                                                  clicked_geoid)
    } else {
      # Select
      user_selection$parent_geog_geoids = union(user_selection$parent_geog_geoids,
                                                clicked_geoid)
    }
    
    mapboxgl_proxy("acs_download_map") %>%
      set_paint_property("geom_fill", "fill-color",
                         selection_fill_color(user_selection$parent_geog_geoids))
    
    new_selected_parent_geog_names = user_selection$state_parent_geog_meta %>%
      filter(geoid %in% user_selection$parent_geog_geoids) %>%
      pull(parent_geog_label)
    
    updatePickerInput(session, "parent_geog_select", selected = new_selected_parent_geog_names)
    
  })
  
  ## Update parent geog selection in map based on picker --------------------
  observeEvent(input$parent_geog_select, {
    
    picker_labels = input$parent_geog_select
    
    if(length(picker_labels) > 0){
      picker_geoids = user_selection$state_parent_geog_meta %>%
        filter(parent_geog_label %in% picker_labels) %>%
        pull(geoid)
    } else {
      picker_geoids = character(0)
    }
    
    user_selection$parent_geog_geoids = picker_geoids
    
    mapboxgl_proxy("acs_download_map") %>%
      set_paint_property("geom_fill", "fill-color",
                         selection_fill_color(picker_geoids))
    
  }, ignoreNULL = FALSE)

  # Year Selection Reactivity -------------
  observeEvent(input$geog_level, {
    
    prev_year_select = input$year_select
    
    if(input$geog_level =="Block Groups"){

      if(length(prev_year_select)>0){
        clean_year_select = prev_year_select[as.numeric(prev_year_select)>=2013]
      }else{
        clean_year_select = character(0)
      }

      updatePickerInput(session,
                        "year_select",
                        choices = rev(years_available),
                        selected = clean_year_select)
    }else if(input$geog_level == "Census Designated Places" |
             input$geog_level == "Core Based Statistical Areas"){
      
      if(length(prev_year_select)>0){
        clean_year_select = prev_year_select[as.numeric(prev_year_select)>=2011]
      }else{
        clean_year_select = character(0)
      }
      
      updatePickerInput(session,
                        "year_select",
                        choices = rev(years_available),
                        selected = clean_year_select)
    }else{
      prev_year_select = input$year_select
      
      updatePickerInput(session,
                        "year_select",
                        choices = rev(years_available),
                        selected = prev_year_select)
    }
    
  }, ignoreInit = TRUE)
  
  # Variable of Interest Table ----------------
  output$voi_desc = renderDT({
    
    voi_ref %>%
      mutate(table_name = str_replace(table_name,"...... - ","")) %>%
      select(voi_id:universe,table_number,table_name,tracts_bool:years_available) %>%
      rename(`#` = `voi_id`,
             `Variable of Interest` = `variable_of_interest`,
             `VOI Type` = `voi_type`,
             `Units` = `units`, 
             `Universe` = `universe`,
             `Table Number` = `table_number`, 
             `Table Name` = `table_name`,
             `Available at Tract Level?`=`tracts_bool`,
             `Available at Block Group Level?`=`block_groups_bool`,
             `Notes`=`notes`,
             `Years Available` = `years_available`) %>%
      datatable(options = list(paging=FALSE, scrollY='500px'),
                rownames = FALSE, filter = 'top')
  })
  
  output$census_voi_desc = renderDT({
    
    census_voi_ref %>%
      mutate(table_name = str_replace(table_name,"...... - ","")) %>%
      select(voi_id:universe,table_number,table_name,notes) %>%
      rename(`#` = `voi_id`,
             `Variable of Interest` = `variable_of_interest`,
             `VOI Type` = `voi_type`,
             `Units` = `units`, 
             `Universe` = `universe`,
             `Table Number` = `table_number`, 
             `Table Name` = `table_name`,
             `Notes`=`notes`) %>%
      datatable(options = list(paging=FALSE),
                rownames = FALSE, filter = 'top')
  })
  
  ## Update VOI availability based on geography level selected -------
  observeEvent(input$geog_level,{
    if(input$geog_level %in% ("Block Groups")){
      var_choices = voi_ref %>%
        filter(block_groups_bool) %>%
        pull(voi_label)
    }else{
      var_choices = voi_ref %>%
        filter(tracts_bool) %>%
        pull(voi_label)
    }
    
    updatePickerInput(session,"voi_select",
                      choices = var_choices)
  })
  
  ## Update VOI selection in Picker based on table --------
  observe({
    #print(input$voi_desc_rows_selected)
    
    if(length(input$voi_desc_rows_selected)>0){
      table_voi_selection = voi_ref %>%
        filter(voi_id %in% input$voi_desc_rows_selected) %>%
        pull(voi_label)
      
      updatePickerInput(session,"voi_select",selected = table_voi_selection)
    }else{
      updatePickerInput(session,"voi_select",selected = character(0))
    }
  })
  
  observe({
    if(length(input$census_voi_desc_rows_selected)>0){
      census_rows_selected = paste0("D",input$census_voi_desc_rows_selected)
      
      table_voi_selection = census_voi_ref %>%
        filter(voi_id %in% census_rows_selected) %>%
        pull(voi_label)
      
      updatePickerInput(session,"census_voi_select",selected = table_voi_selection)
    }else{
      updatePickerInput(session,"census_voi_select",selected = character(0))
    }
  })
  
  ## Update VOI selection in table based on Picker ----------
  observeEvent(input$voi_select,{
    
    picker_voi_selection = voi_ref %>%
      filter(voi_label %in% input$voi_select) %>%
      pull(voi_id)
    
    dataTableProxy("voi_desc") %>%
      selectRows(picker_voi_selection)
  },ignoreNULL = FALSE)
  
  observeEvent(input$census_voi_select,{
    
    picker_voi_selection = census_voi_ref %>%
      filter(voi_label %in% input$census_voi_select) %>%
      pull(voi_id) %>%
      str_replace("D","") %>%
      as.numeric()
    
    dataTableProxy("census_voi_desc") %>%
      selectRows(picker_voi_selection)
  },ignoreNULL = FALSE)
  
  
  # Check email and other inputs before enabling download button ----------
  observeEvent(c(input$email_input,
                 user_selection$parent_geog_geoids,
                 user_selection$voi_selection,
                 user_selection$years_selected),{
                   
                   print(input$email_input)
                   print(user_selection$parent_geog_geoids)
                   print(user_selection$voi_selection)
                   print(user_selection$years_selected)
                   
                   if(nchar(input$email_input)>nchar("nelsonnygaard.com")){
                     email_test = isValidEmail(input$email_input)
                     if(email_test){
                       if(length(user_selection$parent_geog_geoids)>0 &
                          length(user_selection$voi_selection)>0 &
                          length(user_selection$years_selected)>0){
                         shinyjs::enable("prepare_download")
                         user_selection$user_email = input$email_input
                       }else{
                         shinyjs::disable("prepare_download")
                         showNotification("Please select at least one year, county, and variable of interest.",type = "warning")
                       }
                     }else{
                       shinyjs::disable("prepare_download")
                       showNotification("Please enter valid email address to download data.",type = "warning")
                     }
                   }
                 }, ignoreInit = TRUE)
  
  #Observer linking user selection reactiveValues to input -------
  
  observe({
    
    if(sel_tab_data_source()==acs_year_name){   #######################2024 end changed here
      user_selection$voi_selection = input$voi_select
      user_selection$geog_level = input$geog_level
      user_selection$geom_include = input$include_geom_checkbox
      user_selection$years_selected = input$year_select
    }else{
      user_selection$voi_selection = input$census_voi_select
      user_selection$geog_level = input$census_geog_level
      user_selection$geom_include = input$include_geom_checkbox
      user_selection$years_selected = 2020
    }
    
    test_user_selection <<- reactiveValuesToList(user_selection)
  })
  
  # Prepare Download button (triggers query and packaging of download) --------------
  observeEvent(input$prepare_download,{
    
    #Initialize waiter spinner
    waiter <- waiter::Waiter$new()
    waiter$show()
    on.exit(waiter$hide())
    
    user_selection$query_start_timestamp = Sys.time()
    
    pre_entry = generate_query_record_entry(user_selection) 
    
    predicted_completion_seconds = pre_entry %>%
      mutate(geog_level = factor(geog_level)) %>%
      predict(trained_rf_model,.) %>%
      pull(.pred)

    note_seconds = case_when(
      predicted_completion_seconds < 5 ~ predicted_completion_seconds,
      TRUE ~ 5
    )

    showNotification(paste0("Query estimated to take ",
                            round(predicted_completion_seconds),
                            " seconds to complete."),
                     duration = note_seconds)
    
    #Fetch data based on user selections
    query_result = fetch_query(user_selection)
    
    #Remove duplicates when downloading 2023 (fixed )
    
    #query_result = query_result %>% group_by(year, query_geog_geoid, survey) %>% slice(1)
    
    
    
    if (user_selection$geog_level == "Census Designated Places"){ #March 2026 fix duplicates in places table
      
      query_result <- query_result %>% filter(parent_geog_geoid==query_geog_geoid)
    }
    
    tmp_meta_lines = tempfile(pattern = "uscb-export-metadata-",fileext = ".txt")
    generate_query_meta_file(user_selection,tmp_meta_lines)
    
    #csv version of tabular data
    tmp_flat = tempfile(pattern = "uscb-export-",fileext = '.csv')
    write_csv(query_result,tmp_flat,na="")
    
    #Excel version of tabular data
    tmp_excel = tempfile(pattern = "uscb-export-excel-",fileext = '.xlsx')
    wb = createWorkbook()
    addWorksheet(wb,"data")
    textStyle = createStyle(numFmt = "TEXT")
    col_nums_for_style = which(colnames(query_result) %in% 
                                 c('parent_geog_geoid', 'statefp', 'countyfp', 'query_geog_geoid'))
    writeData(wb,sheet='data',x=query_result)
    addStyle(wb,sheet="data",style = textStyle, cols = col_nums_for_style, rows = 1:nrow(query_result),
             gridExpand = TRUE)
    saveWorkbook(wb,tmp_excel,overwrite = TRUE)
    
    #Package geometry for download (if necessary)
    if(user_selection$geom_include == TRUE){
      
      geog_tbl_name = case_when(
        user_selection$geog_level == "Blocks" ~ "blocks",
        user_selection$geog_level == "Block Groups" ~ "block_groups",
        user_selection$geog_level == "Tracts" ~ "tracts",
        user_selection$geog_level == "Counties" ~ "county_allyear",
        user_selection$geog_level == "Census Designated Places" ~ "places",
        user_selection$geog_level ==  "Core Based Statistical Areas" ~ "cbsas_by_state"
      )
      
      years_to_query = case_when(
        user_selection$geog_level == "Blocks" ~ 2020,

        user_selection$geog_level == "Block Groups" ~   as.numeric(user_selection$years_selected), 

        user_selection$geog_level == "Tracts" ~   as.numeric(user_selection$years_selected), 

        user_selection$geog_level == "Counties" ~ as.numeric(user_selection$years_selected), 
        user_selection$geog_level == "Census Designated Places" ~  as.numeric(user_selection$years_selected),  #changed from 2022 to 2021 due to error
        user_selection$geog_level ==  "Core Based Statistical Areas" ~  as.numeric(user_selection$years_selected)  #changed from 2022 to 2021 due to error
      )
      
      if(geog_tbl_name == "blocks"){
        uq_parent_geog_ids <- unique(query_result$parent_geog_geoid)
        
        geog_query = paste0("SELECT * FROM ",geog_tbl_name," WHERE SUBSTRING(geoid,1,5) IN (",
                            paste(paste0("'",uq_parent_geog_ids,"'"),collapse = ", "),
                            ") AND year = 2020;")
      }else{
        uq_query_geog_ids = unique(query_result$query_geog_geoid)
        
        geog_query = paste0("SELECT * FROM ",geog_tbl_name," WHERE geoid IN (",
                            paste(paste0("'",uq_query_geog_ids,"'"),collapse = ", "),
                            ") AND year IN (",
                            paste(paste0(years_to_query),collapse = ", "),
                            ");")
      }
      
      reconnect_db()
      sel_geom = st_read(con,query = geog_query) %>%
        st_transform(4326) %>%
        mutate(aland_sqmi = aland * 0.00000038610,
               aland_acre = aland * 0.000247105 ,
               #geoid_numeric = as.numeric(geoid)
             
                 
        ) %>%
        select(-aland,-awater)
      
      temp_shp_dir = tempdir(check=TRUE)
      st_write(obj = sel_geom %>% st_as_sf() ,
               dsn = paste0(temp_shp_dir,"/uscb-export-geom.shp"),
               layer = "uscb-export-geom",
               #overwrite_layer = TRUE, 
               append = FALSE,
               driver = "ESRI Shapefile")
      shp_files = list.files(temp_shp_dir,pattern = "uscb-export-geom",
                             full.names = TRUE)
      
      file_vec = c(tmp_meta_lines,tmp_flat, tmp_excel, shp_files)
    }else{
      #Otherwise, just package up tabular data
      file_vec = c(tmp_meta_lines,tmp_flat, tmp_excel)
    }
    
    #Write query metadata out to database table
    user_selection$query_end_timestamp = Sys.time()
    
    query_entry = generate_query_record_entry(user_selection)
    
    reconnect_db()
    dbWriteTable(con,"acs_query_records",query_entry,row.names=FALSE,append=TRUE)
    
    compiled_download_vector(file_vec)
  })
  
  output$download_uscb = downloadHandler(
    filename = function(){
      #Download filename
      
      fn = Sys.time() %>% make_clean_names() %>%
        str_replace("x","uscb_download_") %>%
        paste0(".zip")
      
      return(fn)
    },
    content = function(file){
      
      #Initialize waiter spinner
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      
      zip::zipr(file, files = compiled_download_vector())
      
    },
    contentType = "application/zip"
  )
})
