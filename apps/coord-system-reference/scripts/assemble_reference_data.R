library(tidyverse)
library(sf)
library(httr)
library(rvest)
library(janitor)
library(xml2)
library(tigris)
library(aws.s3)
library(aws.signature)
library(aws.iam)

Sys.setenv("AWS_ACCESS_KEY_ID" = Sys.getenv(),
           "AWS_SECRET_ACCESS_KEY" = Sys.getenv(),
           "AWS_DEFAULT_REGION" = Sys.getenv())


us_spcs_shp = read_sf("data/spcszn83/spcszn83.shp") %>%
  st_transform(4326) %>%
  clean_names() %>%
  rename(spcs_fips_code = fipszone) %>%
  mutate(spcs_fips_code = str_pad(spcs_fips_code,width = 4,side="left",pad="0"))

raw_bullets = GET("https://wiki.spatialmanager.com/index.php/Coordinate_Systems_objects_list") %>%
  content() %>%
  xml_nodes("li") %>%
  xml_text() %>%
  as_tibble_col("raw_string") 

clean_bullets = raw_bullets %>%
  filter(str_sub(raw_string,1,4)=="EPSG") %>%
  mutate(epsg_code = str_extract(raw_string,"(?<=^EPSG:)\\d+(?=\\s-)"),
         fips_code = str_extract(raw_string,"(?<=FIPS_)\\d+"))

us_states = states()
st_two_letter_codes = unique(us_states$STUSPS)
st_names = unique(us_states$NAME)

fips_epsg_ref = clean_bullets %>%
  filter(!is.na(fips_code)) %>%
  separate(raw_string,into = c("temp_1","temp_2","temp_3","temp_4","temp_5","temp_6","temp_7","temp_8","temp_9"),
           sep="/", remove = FALSE) %>%
  select(epsg_code,fips_code,temp_3,temp_4,temp_5,raw_string) %>%
  mutate(meta_extract = pmap(.l = list(temp_3,temp_5),
                            .f = function(temp_3,temp_5){
                              
                              search_string = c(temp_3,temp_5)[!is.na(c(temp_3,temp_5))] %>%
                                str_trim() %>%
                                str_c(collapse = " ")
                              
                              #State
                              
                              #Check two letter code first, then full name
                              
                              state_query_code = str_extract(search_string,"\\b[:upper:]{2}\\b")
                              
                              if(!is.na(state_query_code)){
                                state_code = st_two_letter_codes[which(
                                  str_detect(st_two_letter_codes,state_query_code)
                                )]
                              }else{
                                full_state_match = st_names[which(str_detect(search_string,st_names))]
                                
                                state_code =us_states$STUSPS[us_states$NAME == full_state_match]
                                
                              }
                              
                              #Units
                              
                              detected_units = case_when(
                                str_detect(search_string,fixed("(m")) | str_detect(search_string,"Meter") ~ "meters",
                                str_detect(search_string,fixed("(ft"))  | str_detect(search_string,"Feet")~ "feet",
                                TRUE ~ NA_character_
                              )
                              
                              out_meta = tibble(
                                st_usps = state_code,
                                units = detected_units
                              )
                              
                              return(out_meta)
                              
                            })) %>%
  select(epsg_code,fips_code,raw_string,meta_extract) %>%
  unnest(meta_extract) %>%
  rename(desc_string = raw_string)  %>%
  rename(spcs_fips_code = fips_code) %>%
  left_join(us_states %>%
              st_drop_geometry() %>%
              select(STUSPS,STATEFP,NAME) %>%
              rename(st_usps=STUSPS,
                     state_fips_code = STATEFP,
                     state_name = NAME)) %>%
  select(epsg_code,spcs_fips_code,st_usps,state_fips_code,state_name,units,
         desc_string)


write_rds(fips_epsg_ref,"data/us_spcs_epsg_ref.rds")
write_rds(us_spcs_shp,"data/us_spcs_shp.rds")

aws_bucket_id = "nn-r-resources"
s3saveRDS(fips_epsg_ref,"gis-resources/us_spcs_epsg_ref.rds",bucket = aws_bucket_id)
s3saveRDS(us_spcs_shp,"gis-resources/us_spcs_shp.rds",bucket = aws_bucket_id)
