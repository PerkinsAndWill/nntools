#Query ACS Data based on user selections



fetch_query = function(user_selection){
  
  #Temporary storage of user selections in new variables
  selected_parent_geog_meta = user_selection$state_parent_geog_meta %>%
    filter(geoid %in% user_selection$parent_geog_geoids)
  
  sel_parent_geog_geoids = selected_parent_geog_meta %>%
    select(any_of(c("geoid","statefp","countyfp","parent_geog_label")))
  
  sel_geog_level = user_selection$geog_level
  sel_parent_geog_type <- user_selection$parent_geog_type
  sel_years = user_selection$years_selected
  
  if(user_selection$data_source == "5-year ACS Data (2010 - 2024)"){ #Update here
    #For loop calling fetch_acs_voi() for each variable of interest (VOI)
    selected_voi_ids = voi_ref %>%
      filter(voi_label %in% user_selection$voi_selection) %>%
      pull(voi_id)
    
    for(i in 1:length(selected_voi_ids)){
      
      sel_voi_id = selected_voi_ids[i]
      
      voi_res = fetch_acs_voi(sel_voi_id, 
                              sel_parent_geog_geoids,
                              sel_parent_geog_type,
                              sel_geog_level,
                              sel_years)
      
      if(i == 1){
        running_voi_df = voi_res
        rm(voi_res)
      }else{
        running_voi_df = running_voi_df %>% 
          full_join(voi_res, by = c("year","survey","parent_geog_geoid","parent_geog_label", "query_geog_geoid"))
        rm(voi_res)
      }
      
      #print(i)
    }
  }else{
    selected_voi_ids = census_voi_ref %>%
      filter(voi_label %in% user_selection$voi_selection) %>%
      pull(voi_id)
    
    #For loop calling fetch_decennial_census_voi() for each variable of interest (VOI)
    for(i in 1:length(selected_voi_ids)){
      
      sel_voi_id = selected_voi_ids[i]
      
      voi_res = fetch_decennial_census_voi(
        sel_voi_id, 
        sel_parent_geog_geoids,
        sel_parent_geog_type,
        sel_geog_level,
        sel_years) %>%
        select(-any_of(c("statefp","countyfp")))
      
      if(i == 1){
        running_voi_df <- voi_res
        rm(voi_res)
      }else{
        running_voi_df <- running_voi_df %>% 
          full_join(voi_res, by = c("year","parent_geog_geoid", 
                                    "parent_geog_label", 
                                    "query_geog_geoid"))
        rm(voi_res)
      }
      
      # check <- running_voi_df %>%
      #   group_by(year, parent_geog_geoid, parent_geog_label, query_geog_geoid) %>%
      #   summarise(n=n())
      # 
      # check_2 <- running_voi_df %>%
      #   filter(query_geog_geoid == check$query_geog_geoid[1])
      
      #print(i)
    }
  }
  
  return(running_voi_df)
  
}

#Fetch data using tidycensus based on parameters specific by fetch_query()
#Function called by fetch_query()
fetch_acs_voi = function(sel_voi_id, 
                         sel_parent_geog_geoids, 
                         sel_parent_geog_type,
                         sel_geog_level, 
                         sel_years, 
                         sel_survey = "acs5"){
  
  #Formatted string specifying geography level
  formatted_geog_level = case_when(
    sel_geog_level == "Block Groups" ~ "block group",
    sel_geog_level == "Tracts" ~ "tract",
    sel_geog_level == "Counties" ~ "county",
    sel_geog_level == "Census Designated Places" ~ "place",
    sel_geog_level ==  "Core Based Statistical Areas" ~ "metropolitan statistical area/micropolitan statistical area"
  )
  
  #Pull table number for VOI
  voi_table_id = voi_ref %>%
    filter(voi_id == sel_voi_id) %>%
    pull(table_number) %>%
    str_split(", ") %>%
    unlist()
  
  #Use purrr + tidycensus to download neccessary ACS table data separately for each county (or other parent geog) and year
  if(sel_parent_geog_type == "Counties (includes tracts, block groups, & blocks)"){
    raw = sel_parent_geog_geoids %>%
      expand_grid(year = as.integer(sel_years),
                  table_number = voi_table_id) %>%
      left_join(table_year_ref) %>%
      filter(available) %>%
      select(-available) %>%
      mutate(acs_result = pmap(.l = list(statefp,countyfp,year,table_number),
                               ~safe_get_acs(geography = formatted_geog_level,
                                        table = ..4,
                                        year = ..3, 
                                        survey = sel_survey,
                                        state = ..1,
                                        county = ..2))) %>%
      unnest(acs_result) %>%
      rename(parent_geog_geoid = geoid)
  }else if(sel_parent_geog_type == "Core Based Statistical Areas"){
    raw = expand_grid(year = as.integer(sel_years),
                      table_number = voi_table_id) %>%
      left_join(table_year_ref) %>%
      filter(available) %>%
      select(-available) %>%
      mutate(acs_result = pmap(.l = list(year,table_number),
                               ~safe_get_acs(geography = formatted_geog_level,
                                        table = ..2,
                                        year = ..1, 
                                        survey = sel_survey))) %>%
      unnest(acs_result) %>%
      filter(GEOID %in% sel_parent_geog_geoids$geoid) %>%
      rename(parent_geog_geoid = geoid)
  }else{
    raw = sel_parent_geog_geoids %>%
      expand_grid(year = as.integer(sel_years),
                  table_number = voi_table_id) %>%
      left_join(table_year_ref) %>%
      filter(available) %>%
      select(-available) %>%
      mutate(acs_result = pmap(.l = list(statefp,year,table_number),
                               ~safe_get_acs(geography = formatted_geog_level,
                                        table = ..3,
                                        year = ..2, 
                                        survey = sel_survey,
                                        state = ..1))) %>%
      unnest(acs_result) %>%
      filter(GEOID %in% sel_parent_geog_geoids$geoid) %>% 
      rename(parent_geog_geoid = geoid)
  }
  
  #Additional cleaning for each VOI before serving data back to user
  if(sel_voi_id == 1){
    ## 1) Total population (B01003) -------------------
    
    clean = raw %>%
      rename(query_geog_geoid = GEOID,
             total_pop = estimate) %>%
      select(-NAME, - moe, -variable)
    
  }else if(sel_voi_id == 2){
    ## 2) Age (Youth and Senior) (B01001) -------------------
    
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME, - moe) %>%
      left_join(acs_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      mutate(col_name = map_chr(label,function(label){
        
        split = str_split(label,"!!") %>% unlist()
        
        if(length(split)==2){
          return("age_total_pop")
        }else if (length(split)==3){
          return(NA)
        }else{
          if(split[4] == "Under 5 years"){
            return("age_under_5")
          }else if (split[4] == "85 years and over"){
            return("age_85_plus")
          }else{
            str_replace(split[4]," years","") %>%
              str_replace_all(" ","_") %>%
              paste0("age_",.)
          }
        }
        
      })) %>%
      filter(!is.na(col_name)) %>%
      group_by(survey,year,parent_geog_geoid, parent_geog_label,
               query_geog_geoid,col_name) %>%
      summarise(estimate = sum(estimate,na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(col_name = factor(col_name,
                               ordered=TRUE,
                               levels = c("age_under_5","age_5_to_9",
                                          "age_10_to_14","age_15_to_17","age_18_and_19",
                                          "age_20","age_21","age_22_to_24","age_25_to_29",
                                          "age_30_to_34","age_35_to_39","age_40_to_44",
                                          "age_45_to_49","age_50_to_54" ,"age_55_to_59",
                                          "age_60_and_61","age_62_to_64","age_65_and_66",
                                          "age_67_to_69","age_70_to_74","age_75_to_79",
                                          "age_80_to_84","age_85_plus","age_total_pop"))) %>%
      arrange(query_geog_geoid,col_name) %>%
      pivot_wider(names_from=col_name,values_from=estimate) %>%
      mutate(age_under_18 = age_under_5 + age_5_to_9 + age_10_to_14 +
               age_15_to_17,
             age_10_to_17 = age_10_to_14 + age_15_to_17,
             age_18_to_24 = age_18_and_19 + age_20 + age_21 + age_22_to_24,
             age_65_plus = age_65_and_66 + age_67_to_69 + age_70_to_74 + 
               age_75_to_79 + age_80_to_84 + age_85_plus)
    
  }else if(sel_voi_id == 3){
    ## 3) People with disabilities (B23024) --------------------
    
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME, - moe) %>%
      left_join(acs_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      mutate(col_name = case_when(
        variable == 'B23024_001' ~ "disability_total_pop",
        variable %in% c('B23024_003','B23024_018') ~ "disability_yes",
        variable %in% c('B23024_010','B23024_025') ~ "disability_no",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(col_name)) %>%
      group_by(survey,year,parent_geog_geoid, parent_geog_label,
               query_geog_geoid,col_name) %>%
      summarise(estimate = sum(estimate,na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(col_name = factor(col_name, ordered=TRUE,
                               levels = c("disability_yes","disability_no","disability_total_pop"))) %>%
      arrange(query_geog_geoid, col_name) %>%
      pivot_wider(names_from = col_name, values_from = estimate)
    
  }else if(sel_voi_id == 4){
    ## 4) People with disabilities (B18101) ----------------
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME, - moe) %>%
      left_join(acs_vars %>%
                  select(name,label) %>%
                  rename(variable = name))  %>%
      separate(label, into = c("temp_1","temp_2","temp_3","temp_4","temp_5"),
               sep="!!", remove = FALSE) %>%
      mutate(disability_status = case_when(
        temp_5 == "No disability" ~ "disability_no",
        temp_5 == "With a disability" ~ "disability_yes",
        TRUE ~ NA_character_
      ),
      age_handle = case_when(
        temp_4 == "Under 5 years:" ~ "age_under_5",
        temp_4 == "75 years and over:" ~ "age_75_plus",
        TRUE ~ str_replace(temp_4," years:","") %>%
          str_replace_all(" ","_") %>%
          paste0("age_",.)
      )) %>%
      filter(label == "Estimate!!Total:" | (!is.na(disability_status) & !is.na(age_handle))) %>%
      mutate(col_name = paste0(disability_status,"_",age_handle)) %>%
      mutate(col_name = case_when(label == "Estimate!!Total:" ~ "disability_total",
                                  TRUE ~ col_name)) %>%
      filter(!is.na(col_name)) %>%
      group_by(survey,year,parent_geog_geoid, parent_geog_label,
               query_geog_geoid,col_name) %>%
      summarise(estimate = sum(estimate,na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(col_name = factor(col_name, ordered=TRUE,
                               levels = c("disability_total",
                                          paste0(c("disability_yes_"),
                                                 c("age_under_5","age_5_to_17",
                                                   "age_18_to_34","age_35_to_64",
                                                   "age_65_to_74","age_75_plus")),
                                          paste0(c("disability_no_"),
                                                 c("age_under_5","age_5_to_17",
                                                   "age_18_to_34","age_35_to_64",
                                                   "age_65_to_74","age_75_plus")))
      )) %>%
      arrange(query_geog_geoid,col_name) %>% 
      pivot_wider(names_from = col_name, values_from = estimate)
    
  }else if(sel_voi_id == 5){
    ## 5) Zero vehicle households (B25044) ------------------
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME, - moe) %>%
      left_join(acs_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      mutate(temp = map_chr(label,function(string){return((str_split(string,"!!") %>% unlist())[4])})) %>%
      mutate(temp2 = map_chr(label,function(string){return((str_split(string,"!!") %>% unlist())[3])})) %>%
      mutate(col_name = case_when(
        str_sub(temp,1,2) == "No" ~ "veh_hh_0",
        TRUE ~ paste0("veh_hh_",str_sub(temp,1,1)))) %>%
      mutate(col_name = case_when(label == "Estimate!!Total:" ~ "veh_hh_total",
                                  label == "Estimate!!Total:!!Owner occupied:" ~ "veh_hh_total_own",
                                  label == "Estimate!!Total:!!Renter occupied:" ~ "veh_hh_total_rent",
                                  temp2 == "Owner occupied:" ~ paste0(col_name,"_own"),
                                  temp2 == "Renter occupied:" ~ paste0(col_name,"_rent"))) %>%
      filter(col_name == "veh_hh_total" | !is.na(temp)) %>%
      group_by(survey,year,parent_geog_geoid, parent_geog_label,
               query_geog_geoid,col_name) %>%
      summarise(estimate = sum(estimate,na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_wider(names_from = col_name, values_from = estimate)
  }else if(sel_voi_id == 6){
    ## 6) Renter / owners (B25003) --------------------
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME, - moe) %>%
      left_join(acs_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      mutate(col_name = case_when(
        label == "Estimate!!Total:" ~ "tenure_total_hh",
        label == "Estimate!!Total:!!Owner occupied" ~ "tenure_owner_hh",
        label == "Estimate!!Total:!!Renter occupied" ~ "tenure_renter_hh"
      )) %>%
      group_by(survey,year,parent_geog_geoid, parent_geog_label,
               query_geog_geoid,col_name) %>%
      summarise(estimate = sum(estimate,na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_wider(names_from = col_name, values_from = estimate)
  }else if(sel_voi_id == 7){
    ## 7) Language (B16007) ------------------------
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME, - moe) %>%
      left_join(acs_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      mutate(lang_temp = map_chr(label,function(label){
        (str_split(label,"!!") %>% unlist())[4]
      })) %>%
      left_join(tibble(
        lang_temp = c("Speak only English","Speak Spanish",
                      "Speak other Indo-European languages",
                      "Speak Asian and Pacific Island languages",
                      "Speak other languages"),
        col_name = c("lang_home_english","lang_home_spanish",
                     "lang_home_other_indo_euro","lang_home_asian_pac_isl",
                     "lang_home_all_other")
      )) %>%
      mutate(col_name = case_when(label == "Estimate!!Total:" ~ "lang_home_total",
                                  TRUE ~ col_name)) %>%
      filter(!is.na(col_name)) %>%
      group_by(survey,year,parent_geog_geoid, parent_geog_label,
               query_geog_geoid,col_name) %>%
      summarise(estimate = sum(estimate,na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_wider(names_from = col_name, values_from = estimate)
  }else if(sel_voi_id == 8){
    ## 8) English Proficiency (B16004) -------------
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME, - moe) %>%
      left_join(acs_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      mutate(temp = map_chr(label,function(label){
        (str_split(label,"!!") %>% unlist())[5]
      })) %>%
      mutate(col_name = case_when(
        label == "Estimate!!Total:" ~ "eng_prof_total",
        temp %in% c('Speak English "not well"',
                    'Speak English "not at all"') ~ "eng_prof_poor_none"
      )) %>%
      filter(!is.na(col_name)) %>%
      group_by(survey,year,parent_geog_geoid, parent_geog_label,
               query_geog_geoid,col_name) %>%
      summarise(estimate = sum(estimate,na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_wider(names_from = col_name, values_from = estimate)
  }else if(sel_voi_id == 9){
    ## 9) People in poverty (B17021) -----------------
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME, - moe) %>%
      left_join(acs_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      filter(variable %in% c("B17021_001","B17021_002")) %>%
      mutate(col_name = case_when(variable == "B17021_001" ~"poverty_ppl_total",
                                  variable == "B17021_002" ~"poverty_ppl_below")) %>%
      group_by(survey,year,parent_geog_geoid, parent_geog_label,
               query_geog_geoid,col_name) %>%
      summarise(estimate = sum(estimate,na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_wider(names_from = col_name, values_from = estimate)
  }else if(sel_voi_id == 10){
    ## 10) People whose household income is less than 150% of federal poverty line (C17002) -----------
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME, - moe) %>%
      left_join(acs_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      left_join(tibble(
        label = c("Estimate!!Total:","Estimate!!Total:!!Under .50",
                  "Estimate!!Total:!!.50 to .99",
                  "Estimate!!Total:!!1.00 to 1.24",
                  "Estimate!!Total:!!1.25 to 1.49","Estimate!!Total:!!1.50 to 1.84", 
                  "Estimate!!Total:!!1.85 to 1.99","Estimate!!Total:!!2.00 and over"),
        col_name = c("poverty_line_total",
                     "poverty_line_below_100","poverty_line_below_100",
                     "poverty_line_100_to_124",
                     "poverty_line_125_to_149",
                     "povery_line_150_to_184",
                     "poverty_line_185_to_199",
                     "poverty_line_200_plus")
      ))  %>%
      mutate(col_name = factor(col_name, ordered=TRUE, 
                               levels = c("poverty_line_total",
                                          "poverty_line_below_100",
                                          "poverty_line_100_to_124",
                                          "poverty_line_125_to_149",
                                          "povery_line_150_to_184",
                                          "poverty_line_185_to_199",
                                          "poverty_line_200_plus"))) %>%
      group_by(survey,year,parent_geog_geoid, parent_geog_label,
               query_geog_geoid,col_name) %>%
      summarise(estimate = sum(estimate,na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_wider(names_from = col_name, values_from = estimate)
  }else if(sel_voi_id == 11){
    ## 11) Household income (B19001) -----------------------
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME, - moe) %>%
      left_join(acs_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      mutate(temp = map_chr(label,function(label){
        (str_split(label,"!!") %>% unlist())[3]
      })) %>%
      filter(!is.na(temp)) %>%
      mutate(col_name = temp %>%
               str_replace(",000","k") %>%
               str_replace_all(fixed("$"),"") %>%
               str_to_lower() %>%
               str_replace_all(",","") %>%
               str_replace(" ","_") %>%
               paste0("hh_income_",.)) %>%
      mutate(col_name = case_when(label == "Estimate!!Total:" ~ "hh_income_total",
                                  TRUE ~ col_name)) %>%
      mutate(col_name = factor(col_name,ordered=TRUE,
                               levels = unique(col_name))) %>%
      group_by(survey,year,parent_geog_geoid, parent_geog_label,
               query_geog_geoid,col_name) %>%
      summarise(estimate = sum(estimate,na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_wider(names_from = col_name, values_from = estimate)
    
  }else if(sel_voi_id == 12){
    ## 12) Median household income (B19013) ----------------
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME, - moe) %>%
      left_join(acs_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      rename(median_hh_income = estimate) %>%
      select(-variable,-label)
    
  }else if(sel_voi_id == 13){
    ## 13) Race (B03002) ----------------
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME, - moe) %>%
      left_join(acs_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      mutate(hisp_temp = map_chr(label,function(label){
        (str_split(label,"!!") %>% unlist())[3]
      }),
      race_temp = map_chr(label,function(label){
        (str_split(label,"!!") %>% unlist())[4]
      })) %>%
      mutate(hisp_temp = case_when(
        hisp_temp == "Hispanic or Latino:" ~ "hispanic",
        hisp_temp == "Not Hispanic or Latino:" ~ "non_hispanic",
        TRUE ~ NA_character_
      )) %>%
      filter(!(variable %in% c("B03002_010","B03002_011",
                               "B03002_020","B03002_021"))) %>%
      left_join(tibble(
        race_temp = c("White alone","Black or African American alone",
                      "American Indian and Alaska Native alone","Asian alone", 
                      "Native Hawaiian and Other Pacific Islander alone",
                      "Some other race alone","Two or more races:"),
        race = c("white","black","native_ind_ak","asian", 
                 "native_hawaiian_islander", 
                 "other","two_plus")
      )) %>%
      mutate(col_name = paste0("race_",race,"_",hisp_temp),
             col_name = case_when(label == "Estimate!!Total:" ~ "race_total",
                                  TRUE ~ col_name)) %>%
      filter(col_name == "race_total" | (!is.na(hisp_temp) & !is.na(race))) %>%
      group_by(survey,year,parent_geog_geoid, parent_geog_label,
               query_geog_geoid,col_name) %>%
      summarise(estimate = sum(estimate,na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_wider(names_from = col_name, values_from = estimate) %>%
      mutate(race_hispanic_total = race_asian_hispanic + 
               race_native_hawaiian_islander_hispanic +
               race_black_hispanic + race_native_ind_ak_hispanic +
               race_other_hispanic + race_two_plus_hispanic +
               race_white_hispanic,
             race_poc_total = race_total - race_white_non_hispanic)
  }else if(sel_voi_id == 14){
    ## 14) Foreign Born (B99051) ---------------------
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME) %>%
      left_join(acs_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      mutate(col_name = case_when(
        label == "Estimate!!Total:!!Native:" ~ "native_born_pop",
        label == "Estimate!!Total:!!Foreign born:" ~ "foreign_born_pop",
        TRUE ~ NA_character_
      )) %>%
      mutate(col_name = case_when(label == "Estimate!!Total:" ~ "born_pop_total",
                                  TRUE ~ col_name)) %>%
      filter(!is.na(col_name)) %>%
      group_by(survey,year,parent_geog_geoid, parent_geog_label,
               query_geog_geoid,col_name) %>%
      summarise(estimate = sum(estimate,na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_wider(names_from = col_name, values_from = estimate)
    
  }else if(sel_voi_id == 15){
    ## 15) Non-traditional work hours (B08302) ------------------
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME) %>%
      left_join(acs_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      group_by(query_geog_geoid) %>%
      mutate(clean_label = make_clean_names(label) %>%
               str_replace("estimate_total","commute_to_work_depart")) %>%
      ungroup() %>%
      mutate(col_name = case_when(
        label == "Estimate!!Total:" ~ "commute_to_work_total",
        TRUE ~ clean_label
      )) %>%
      filter(!is.na(col_name)) %>%
      mutate(col_name = factor(col_name,ordered=TRUE,levels = unique(col_name))) %>%
      group_by(survey,year,parent_geog_geoid, parent_geog_label,
               query_geog_geoid,col_name) %>%
      summarise(estimate = sum(estimate,na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(query_geog_geoid,col_name) %>%
      pivot_wider(names_from = col_name, values_from = estimate)
  }else if(sel_voi_id == 16){
    ## 16) Average Household Size (B25010) -----------------------
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME) %>%
      left_join(acs_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      mutate(col_name = case_when(
        label == "Estimate!!Average household size --!!Total:" ~ "avg_hh_size_total",
        label == "Estimate!!Average household size --!!Total:!!Owner occupied" ~ "avg_hh_size_owned",
        label == "Estimate!!Average household size --!!Total:!!Renter occupied" ~ "avg_hh_size_rented"
      )) %>%
      group_by(survey,year,parent_geog_geoid, parent_geog_label,
               query_geog_geoid,col_name) %>%
      summarise(estimate = sum(estimate,na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(query_geog_geoid,col_name) %>%
      pivot_wider(names_from = col_name, values_from = estimate)
  }else if(sel_voi_id == 17){
    ## 17) Number of Housing Units (B25001) ------------------------
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME, - moe) %>%
      left_join(acs_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      rename(num_housing_units = estimate) %>%
      select(-variable,-label)
  }else if (sel_voi_id == 18){
    ## 18) Means of Transportation to Work (B08301) ----------------------
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME, - moe) %>%
      left_join(acs_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      left_join(tribble(
        ~label,     ~col_name,
        "Estimate!!Total:", 'commuters_total',                                                                                                       
        "Estimate!!Total:!!Car, truck, or van:", NA,                                                                                       
        "Estimate!!Total:!!Car, truck, or van:!!Drove alone", "commuters_drove_alone",                                                                          
        "Estimate!!Total:!!Car, truck, or van:!!Carpooled:", "commuters_carpooled",                                                                             
        "Estimate!!Total:!!Car, truck, or van:!!Carpooled:!!In 2-person carpool", NA,                                                        
        "Estimate!!Total:!!Car, truck, or van:!!Carpooled:!!In 3-person carpool", NA,                                                     
        "Estimate!!Total:!!Car, truck, or van:!!Carpooled:!!In 4-person carpool", NA,                                                        
        "Estimate!!Total:!!Car, truck, or van:!!Carpooled:!!In 5- or 6-person carpool", NA,                                                   
        "Estimate!!Total:!!Car, truck, or van:!!Carpooled:!!In 7-or-more-person carpool", NA,                                                 
        "Estimate!!Total:!!Public transportation (excluding taxicab):", "commuters_public_transit",                                                             
        "Estimate!!Total:!!Public transportation (excluding taxicab):!!Bus", NA,                                                              
        "Estimate!!Total:!!Public transportation (excluding taxicab):!!Subway or elevated rail", NA,                                          
        "Estimate!!Total:!!Public transportation (excluding taxicab):!!Long-distance train or commuter rail", NA,                             
        "Estimate!!Total:!!Public transportation (excluding taxicab):!!Light rail, streetcar or trolley (carro pÃºblico in Puerto Rico)", NA, 
        "Estimate!!Total:!!Public transportation (excluding taxicab):!!Ferryboat", NA,                                                        
        "Estimate!!Total:!!Taxicab", "commuters_taxi",                                                                                                    
        "Estimate!!Total:!!Motorcycle", "commuters_motorcycle",                                                                                                  
        "Estimate!!Total:!!Bicycle", "commuters_bicycle",                                                                                                     
        "Estimate!!Total:!!Walked", "commuters_walked",                                                                                                      
        "Estimate!!Total:!!Other means", "commuters_other",                                                                                                 
        "Estimate!!Total:!!Worked from home", "commuters_wfh" 
      )) %>%
      filter(!is.na(col_name)) %>%
      mutate(col_name = factor(col_name, ordered=TRUE, levels = unique(col_name))) %>%
      group_by(survey,year,parent_geog_geoid, parent_geog_label,
               query_geog_geoid,col_name) %>%
      summarise(estimate = sum(estimate,na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(query_geog_geoid,col_name) %>%
      pivot_wider(names_from = col_name, values_from = estimate)
  }else if (sel_voi_id == 19){
    # 19) Educational Attainment (B15003) ------------
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME, - moe) %>%
      left_join(acs_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      left_join(tribble(
        ~ label,      ~col_name,
        "Estimate!!Total:", "educ_total",
        "Estimate!!Total:!!No schooling completed", "educ_less_than_high_school",             
        "Estimate!!Total:!!Nursery school", "educ_less_than_high_school", 
        "Estimate!!Total:!!Kindergarten",  "educ_less_than_high_school",                          
        "Estimate!!Total:!!1st grade", "educ_less_than_high_school", 
        "Estimate!!Total:!!2nd grade", "educ_less_than_high_school",                           
        "Estimate!!Total:!!3rd grade", "educ_less_than_high_school", 
        "Estimate!!Total:!!4th grade", "educ_less_than_high_school",                               
        "Estimate!!Total:!!5th grade", "educ_less_than_high_school", 
        "Estimate!!Total:!!6th grade", "educ_less_than_high_school",                               
        "Estimate!!Total:!!7th grade", "educ_less_than_high_school", 
        "Estimate!!Total:!!8th grade", "educ_less_than_high_school",                               
        "Estimate!!Total:!!9th grade", "educ_less_than_high_school", 
        "Estimate!!Total:!!10th grade", "educ_less_than_high_school",                              
        "Estimate!!Total:!!11th grade", "educ_less_than_high_school", 
        "Estimate!!Total:!!12th grade, no diploma", "educ_less_than_high_school",               
        "Estimate!!Total:!!Regular high school diploma", "educ_high_school_or_equiv", 
        "Estimate!!Total:!!GED or alternative credential", "educ_high_school_or_equiv", 
        "Estimate!!Total:!!Some college, less than 1 year", "educ_some_college_or_assoc",
        "Estimate!!Total:!!Some college, 1 or more years, no degree", "educ_some_college_or_assoc",
        "Estimate!!Total:!!Associate's degree", "educ_some_college_or_assoc",
        "Estimate!!Total:!!Bachelor's degree", "educ_bachelors",           
        "Estimate!!Total:!!Master's degree", "educ_grad_or_prof",        
        "Estimate!!Total:!!Professional school degree", "educ_grad_or_prof",              
        "Estimate!!Total:!!Doctorate degree", "educ_grad_or_prof"
      )) %>%
      filter(!is.na(col_name)) %>%
      mutate(col_name = factor(col_name, ordered=TRUE, levels = unique(col_name))) %>%
      group_by(survey,year,parent_geog_geoid, parent_geog_label,
               query_geog_geoid,col_name) %>%
      summarise(estimate = sum(estimate,na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(query_geog_geoid,col_name) %>%
      pivot_wider(names_from = col_name, values_from = estimate)
  }else if (sel_voi_id == 20){
    
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME, - moe) %>%
      left_join(acs_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      left_join(voi_col_name_ref) %>%
      filter(!is.na(col_name)) %>%
      select(survey,year,parent_geog_geoid, parent_geog_label,query_geog_geoid,col_name,estimate) %>%
      ungroup() %>%
      arrange(query_geog_geoid,col_name) %>%
      pivot_wider(names_from = col_name, values_from = estimate)
    
  }
  
  rm(raw)
  
  return(clean %>% select(-any_of("table_number")))
  
}

fetch_decennial_census_voi = function(sel_voi_id, 
                                      sel_parent_geog_geoids, 
                                      sel_parent_geog_type,
                                      sel_geog_level, 
                                      sel_years){
  
  #Formatted string specifying geography level
  formatted_geog_level = case_when(
    sel_geog_level == "Blocks" ~ "block",
    sel_geog_level == "Block Groups" ~ "block group",
    sel_geog_level == "Tracts" ~ "tract",
    sel_geog_level == "Counties" ~ "county",
    sel_geog_level == "Census Designated Places" ~ "place",
    sel_geog_level ==  "Core Based Statistical Areas" ~ "metropolitan statistical area/micropolitan statistical area"
  )
  
  #Pull table number for VOI
  voi_table_id = census_voi_ref %>%
    filter(voi_id == sel_voi_id) %>%
    pull(table_number) %>%
    str_split(", ") %>%
    unlist()
  
  #Use purrr + tidycensus to download necessary census table data separately for each county (or other parent geog) and year
  if(sel_parent_geog_type == "Counties (includes tracts, block groups, & blocks)"){
    raw = sel_parent_geog_geoids %>%
      expand_grid(year = as.integer(sel_years),
                  table_number = voi_table_id) %>%
      mutate(result = pmap(.l = list(statefp,countyfp,year,table_number),
                           ~get_decennial(
                             geography = formatted_geog_level,
                             table = ..4,
                             year = ..3,
                             state = ..1,
                             county = ..2
                           )
      )) %>%
      unnest(result) %>%
      rename(parent_geog_geoid = geoid)
  }else if(sel_parent_geog_type == "Core Based Statistical Areas"){
    raw = expand_grid(year = as.integer(sel_years),
                      table_number = voi_table_id) %>%
      mutate(result = pmap(.l = list(year,table_number),
                           ~get_decennial(geography = formatted_geog_level,
                                          table = ..2,
                                          year = ..1))) %>%
      unnest(result)  %>%
      rename(parent_geog_geoid = geoid) %>%
      filter(GEOID %in% sel_parent_geog_geoids$geoid)
  }else{
    raw = sel_parent_geog_geoids %>%
      expand_grid(year = as.integer(sel_years),
                  table_number = voi_table_id) %>%
      mutate(result = pmap(.l = list(statefp,year,table_number),
                           ~get_decennial(geography = formatted_geog_level,
                                          table = ..3,
                                          year = ..2, 
                                          state = ..1))) %>%
      unnest(result) %>%
      rename(parent_geog_geoid = geoid) %>%
      filter(GEOID %in% sel_parent_geog_geoids$geoid)
  }
  
  ## D1) Total Population ----------
  if(sel_voi_id == "D1"){
    clean = raw %>%
      filter(variable == "P1_001N") %>%
      rename(query_geog_geoid = GEOID,
             total_pop = value) %>%
      select(-NAME, -variable)
    
  }else if(sel_voi_id == "D2"){
    ## D2) Race ------------
    
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME) %>%
      left_join(census_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      mutate(temp_split = map(label,~str_split(.x,"!!") %>% unlist())) %>%
      mutate(hisp_temp = map_chr(temp_split, ~.x[3]),
             race_count_temp = map_chr(temp_split, ~.x[4]),
             race_temp = map_chr(temp_split, ~.x[5])) %>%
      select(-temp_split) %>%
      mutate(hisp_temp = case_when(
        hisp_temp == "Hispanic or Latino:" ~ "hispanic",
        hisp_temp == "Not Hispanic or Latino:" ~ "non_hispanic",
        TRUE ~ NA_character_
      )) %>%
      left_join(
        tribble(
          ~race_temp, ~race,
          "White alone", "white",
          "Black or African American alone", "black",
          "American Indian and Alaska Native alone", "native_ind_ak",
          "Asian alone", "asian",
          "Native Hawaiian and Other Pacific Islander alone", "native_hi_pac_island",
          "Some Other Race alone", "other_alone"#,
          # "Population of two races:", "two_plus",                   
          # "Population of three races:", "two_plus",
          # "Population of four races:", "two_plus",                     
          # "Population of five races:", "two_plus",
          # "Population of six races:","two_plus"
        )
      ) %>%
      mutate(col_name = paste0("race_",race,"_",hisp_temp),
             col_name = case_when(label == " !!Total:" ~ "race_total",
                                  label == " !!Total:!!Hispanic or Latino" ~ "race_hispanic_total",
                                  TRUE ~ col_name)) %>%
      filter(col_name == "race_total" | col_name == "race_hispanic_total" | 
               (!is.na(hisp_temp) & !is.na(race))) %>%
      group_by(year,parent_geog_geoid,parent_geog_label,
               query_geog_geoid,col_name) %>%
      summarise(value = sum(value,na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_wider(names_from = col_name, values_from = value) %>%
      mutate(race_poc_total = race_total - race_white_non_hispanic,
             race_two_plus_non_hispanic = race_total - 
               race_asian_non_hispanic -
               race_black_non_hispanic - 
               race_hispanic_total -
               race_native_hi_pac_island_non_hispanic -
               race_native_ind_ak_non_hispanic -
               race_other_alone_non_hispanic -
               race_white_non_hispanic)
    
    # check <- clean %>%
    #   mutate(check_race_total = race_asian_non_hispanic +
    #            race_black_non_hispanic + 
    #            race_hispanic_total +
    #            race_native_hi_pac_island_non_hispanic +
    #            race_native_ind_ak_non_hispanic +
    #            race_other_alone_non_hispanic +
    #            race_two_plus_non_hispanic +
    #            race_white_non_hispanic)
    
  }else if(sel_voi_id == "D3"){
    ## D3) Housing Units ------------
    clean = raw %>%
      rename(query_geog_geoid = GEOID) %>%
      select(-NAME) %>%
      left_join(census_vars %>%
                  select(name,label) %>%
                  rename(variable = name)) %>%
      left_join(
        tribble(
          ~label, ~ col_name,
          " !!Total:", "total_housing_units",
          " !!Total:!!Occupied", "total_occupied_housing_units",
          " !!Total:!!Vacant", "total_vacant_housing_units"
        )
      ) %>%
      group_by(year,parent_geog_geoid,parent_geog_label,
               query_geog_geoid,col_name) %>%
      summarise(value = sum(value,na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_wider(names_from = col_name, values_from = value)
  }
  
  rm(raw)
  

  return(clean %>% select(-any_of("table_number")))
}

#Generate a record of the query for saving the SQL database for later use/monitoring
generate_query_record_entry = function(user_selection){
  
  if(user_selection$data_source == "2020 Census Data"){
    voi_ids = census_voi_ref %>%
      filter(voi_label %in% user_selection$voi_selection) %>%
      arrange(voi_id) %>%
      pull(voi_id)
    
    var_count = length(voi_ids)
  }else{
    voi_ids = voi_ref %>%
      filter(voi_label %in% user_selection$voi_selection) %>%
      arrange(voi_id) %>%
      pull(voi_id)
    
    if(20 %in% voi_ids){
      var_count = length(voi_ids) + 8
    }else{
      var_count = length(voi_ids)
    }
  }
  
  if(!is.null(user_selection$query_end_timestamp)){
    query_entry = tibble(
      user_email = user_selection$user_email,
      start_timestamp = user_selection$query_start_timestamp,
      end_timestamp = user_selection$query_end_timestamp,
      seconds_elapsed = difftime(user_selection$query_end_timestamp,user_selection$query_start_timestamp,
                                 units = "secs") %>% as.numeric(),
      geog_level = user_selection$geog_level,
      geom_include = user_selection$geom_include,
      num_counties = length(user_selection$parent_geog_geoids),
      num_states = user_selection$state_ids %>% n_distinct(),
      num_vars = var_count,
      voi_selection_string = voi_ids %>%
        paste(collapse=", "),
      num_years = length(user_selection$years_selected),
      year_selection_string = paste(sort(user_selection$years_selected), collapse = ", "),
      data_source = case_when(
        user_selection$data_source ==  "2020 Census Data" ~ "Decennial",
        TRUE ~ "ACS"
      )
    )
  }else{
    query_entry = tibble(
      user_email = user_selection$user_email,
      start_timestamp = user_selection$query_start_timestamp,
      geog_level = user_selection$geog_level,
      geom_include = user_selection$geom_include,
      num_counties = length(user_selection$parent_geog_geoids),
      num_states = user_selection$state_ids %>% n_distinct(),
      num_vars = var_count,
      voi_selection_string = voi_ids %>%
        paste(collapse=", "),
      num_years = length(user_selection$years_selected),
      year_selection_string = paste(sort(user_selection$years_selected), collapse = ", "),
      data_source = case_when(
        user_selection$data_source ==  "2020 Census Data" ~ "Decennial",
        TRUE ~ "ACS"
      )
    )
  }
  
  return(query_entry)
  
}

generate_query_meta_file = function(user_selection,temp_fh){
  
  line_vec = c(
    paste0("User Email: ",user_selection$user_email),
    paste0("Query Timestamp: ",user_selection$query_start_timestamp),
    paste0("Data Source: ",
           case_when(user_selection$data_source == "2020 Census Data" ~ "Decennial Census",
                     TRUE ~ "American Community Survey")),
    paste0("Geometry Included?: ",user_selection$geom_include),
    paste0("Geography Level: ",user_selection$geog_level),
    paste0("Year(s): ",paste(user_selection$years_selected,collapse = ", ")),
    paste0("Variables of Interest: ", paste(user_selection$voi_selection %>%
                                              str_replace_all(fixed("\r\n")," "),collapse = ", ")),
    paste0("State IDs: ",paste(user_selection$state_ids,collapse = ", ")),
    paste0("State Names: ",paste(user_selection$state_names,collapse = ", ")),
    paste0("Parent Geography Type: ",user_selection$parent_geog_type),
    paste0("Parent Geography IDs: ",paste(user_selection$parent_geog_geoids,collapse = ", ")),
    paste0("Parent Geography Names: ",paste(user_selection$state_parent_geog_meta %>% 
                                              filter(geoid %in% user_selection$parent_geog_geoids) %>% 
                                              pull(parent_geog_label),collapse = ", "))
  )
  
  write_lines(line_vec,temp_fh)
  
}

#Use regular expression to check if email address is valid
isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), 
        ignore.case=TRUE)
}
