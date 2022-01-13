#' Create parallel lines at distance, assumes sf data frame input.
#'
#' @param line_obj `{sf}` data frame
#' @param coord_local EPSG code of local (planar) coordinate system
#' @param distance Distance of parallel line, in specified planar coordinate system
#' @param forward Direction of parallel line generation. Default is in forward direction, select FALSE if you want in other direction.
#'
#' @return `{sf}` data frame with transformed geometry (parallel lines per specifications)
#' @export
st_parallel_line <- function(line_obj, coord_local, distance, forward = TRUE){

  orig_line_obj = line_obj

  orig_crs = sf::st_crs(orig_line_obj)

  orig_line_coords = line_obj %>%
    sf::st_transform(coord_local) %>%
    sf::st_simplify(dTolerance = 5) %>%
    sf::st_coordinates() %>%
    tibble::as_tibble() %>%
    dplyr::rename(line_num = L1) %>%
    tidyr::nest(coords = c(X,Y))

  parallel_line_obj = orig_line_coords %>%
    dplyr::mutate(parallel_line = purrr::map(coords, function(coords){

      orig_coords = coords %>%
        dplyr::mutate(coord_num = seq_along(X))

      sub_line_segs = orig_coords %>%
        dplyr::mutate(x1 = lag(X), x2 = X,
               y1 = lag(Y), y2 = Y,
               from_coord = dplyr::lag(coord_num),
               to_coord = coord_num) %>%
        dplyr::filter(!is.na(x1)) %>%
        dplyr::select(x1:to_coord) %>%
        dplyr::mutate(line_seg_num = seq_along(x1)) %>%
        dplyr::mutate(p1 = purrr::map2(x1,y1,~sf::st_point(c(.x,.y))) %>% sf::st_sfc(crs=coord_local),
               p2 = purrr::map2(x2,y2,~sf::st_point(c(.x,.y))) %>% sf::st_sfc(crs=coord_local)) %>%
        dplyr::mutate(
          azi_degrees = nngeo::st_azimuth(p1,p2),
          azi_degrees = ifelse(azi_degrees>180,360-azi_degrees,azi_degrees),
          azi_radians = azi_degrees * (pi/180),
          x1_offset = dplyr::case_when(forward == TRUE ~ x1 + distance*cos(azi_radians),
                                TRUE ~ x1 - distance*cos(azi_radians)),
          x2_offset = dplyr::case_when(forward == TRUE ~ x2 + distance*cos(azi_radians),
                                TRUE ~ x2 - distance*cos(azi_radians)),
          y1_offset = dplyr::case_when(forward == TRUE ~ y1 + distance*sin(azi_radians),
                                TRUE ~ y1 - distance*sin(azi_radians)),
          y2_offset = dplyr::case_when(forward == TRUE ~ y2 + distance*sin(azi_radians),
                                TRUE ~ y2 - distance*sin(azi_radians)) #,
          # p1_offset = map2(x1_offset,y1_offset,~st_point(c(.x,.y))) %>% st_sfc(crs=coord_local),
          # p2_offset = map2(x2_offset,y2_offset,~st_point(c(.x,.y))) %>% st_sfc(crs=coord_local)
        )

      # leaflet() %>%
      #   addProviderTiles("CartoDB.Positron") %>%
      #   addCircles(data = sub_line_segs %>% select(p1) %>%
      #                st_as_sf() %>%
      #                st_transform(coord_global), color = "blue",
      #              label = sub_line_segs$line_seg_num) %>%
      #   addCircles(data = sub_line_segs %>% select(p1_offset) %>%
      #                st_as_sf() %>%
      #                st_transform(coord_global), color = "red",
      #              label = sub_line_segs$line_seg_num) %>%
      #   addMeasure()

      para_coords = dplyr::bind_rows(
        sub_line_segs %>%
          dplyr::select(from_coord,x1_offset,y1_offset) %>%
          dplyr::rename(coord_num = from_coord,
                 X = x1_offset,
                 Y = y1_offset) %>%
          dplyr::mutate(temp_num = 1),
        sub_line_segs %>%
          dplyr::select(to_coord,x2_offset,y2_offset) %>%
          dplyr::rename(coord_num = to_coord,
                 X = x2_offset,
                 Y = y2_offset) %>%
          dplyr::mutate(temp_num = 2)
      ) %>%
        dplyr::arrange(coord_num,temp_num) %>%
        dplyr::group_by(coord_num) %>%
        #do(head(.,n=1)) %>%
        dplyr::summarise(X = mean(X),
                  Y = mean(Y)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(coord_num)

      parallel_linestring = sf::st_linestring(
        para_coords %>%
          select(X,Y) %>%
          as.matrix()
      )

      # orig_line_sf = st_linestring(orig_coords %>%
      #                                select(X,Y) %>%
      #                                as.matrix()) %>%
      #   st_sfc(crs = coord_local)
      #
      # para_line_sf = parallel_linestring %>%
      #   st_sfc(crs = coord_local)
      #
      # leaflet() %>%
      #   addProviderTiles("CartoDB.Positron") %>%
      #   addPolylines(data = orig_line_sf %>%
      #                  st_transform(coord_global), color = "blue") %>%
      #   addPolylines(data = para_line_sf %>%
      #                st_transform(coord_global), color = "red") %>%
      #   addMeasure()

      return(parallel_linestring)
    }) %>% sf::st_sfc(crs = coord_local))

  # leaflet() %>%
  #   addProviderTiles("CartoDB.Positron") %>%
  #   addPolylines(data = orig_line_obj %>% st_transform(coord_global), color = "blue",
  #                label =~ segment_id) %>%
  #   addPolylines(data = parallel_line_obj$parallel_line %>%
  #                  st_transform(coord_global), color = "red")

  out_obj = orig_line_obj %>%
    dplyr::mutate(geometry = parallel_line_obj$parallel_line) %>%
    sf::st_as_sf()

  return(out_obj)
}
