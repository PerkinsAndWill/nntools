#' Compute a weighted average azimuth (using angular statistics) of multiple line segments
#'
#' See this slide deck where Bryan pulled this formula from: http://webspace.ship.edu/pgmarr/Geo441/Lectures/Lec%2016%20-%20Directional%20Statistics.pdf
#'
#' @param azi Azimuth of line in decimal degrees
#' @param len Length of line in planar unit (e.g., feet)
#'
#' @return Weighted average azmimuth of line segments in decimal degrees
#' @export
#'
#' @examples mean_azimuth(c(355,5),c(10,5))
mean_azimuth = function(azi,len){
  #Referenced this slide deck for this formula: http://webspace.ship.edu/pgmarr/Geo441/Lectures/Lec%2016%20-%20Directional%20Statistics.pdf

  azi_radians = azi*(pi/180)

  y = stats::weighted.mean(sin(azi_radians),len)
  x = stats::weighted.mean(cos(azi_radians),len)
  r = sqrt(x^2 + y^2)

  mean_bearing = atan2((y/r),(x/r))/(pi/180)

  mean_azi = ifelse(mean_bearing<0,360+mean_bearing,mean_bearing)

  return(mean_azi)

}

#' Return opposite azimuth in decimal degrees (for a given azimuth in decimal degrees)
#'
#' @param azi Input azimuth (decimal degrees)
#'
#' @return Opposite azimuth (decimal degrees)
#' @export
#'
#' @examples opposite_azimuth(270)
opposite_azimuth = function(azi){

  return_vec = numeric(length(azi))

  for(i in 1:length(azi)){
    if(azi[i] > 180){
      return_vec[i] = azi[i]-180
    }else{
      return_vec[i] = azi[i] + 180
    }
  }
  return(return_vec)
}

#' Compute a a difference between two azimuth using angular math
#'
#' See this slide deck where Bryan pulled this formula from: http://webspace.ship.edu/pgmarr/Geo441/Lectures/Lec%2016%20-%20Directional%20Statistics.pdf
#'
#' @param azi_1 First azimuth in decimal degrees
#' @param azi_2 Second azimuth in decimal degrees
#'
#' @return Difference in decimal degrees
#' @export
#'
#' @examples azimuth_difference(355,10)
azimuth_difference = function(azi_1,azi_2){

  azi_1_radians = azi_1 * (pi/180)
  azi_2_radians = azi_2 * (pi/180)

  (acos(cos(azi_1_radians) * cos(azi_2_radians) + sin(azi_1_radians) * sin(azi_2_radians)))*180/pi

}

#' Compute the average azimuth of a complex (typically more than two vertices) line segment
#'
#' Computes the azimuth and length for every sub-segment (i.e. the line between two successive vertices) and then takes a length-weighted average of all sub-segments.
#' Typically used within a {purrr} `map` function.
#'
#' @param linestring Input linestring (from {sf})
#' @param coord_local Local coordinate system EPSG code of input linestring
#'
#' @return Average azimuth in decimal degrees
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
st_avg_line_azimuth = function(linestring, coord_local){

  linestring %>%
    sf::st_coordinates() %>%
    tibble::as_tibble() %>%
    dplyr::select(.data$X,.data$Y) %>%
    dplyr::mutate(x1=dplyr::lag(.data$X), y1=dplyr::lag(.data$Y),
           x2=.data$X, y2=.data$Y) %>%
    dplyr::select(.data$x1:.data$y2) %>%
    dplyr::filter(!is.na(.data$x1),!is.na(.data$y1)) %>%
    dplyr::mutate(pt1 = purrr::map2(.data$x1,.data$y1,~sf::st_point(c(.x,.y))) %>% sf::st_sfc(crs=coord_local),
           pt2 = purrr::map2(.data$x2,.data$y2,~st_point(c(.x,.y))) %>% sf::st_sfc(crs=coord_local)) %>%
    dplyr::mutate(distance = purrr::map2_dbl(.data$pt1,.data$pt2,~sf::st_distance(.x,.y) %>% as.numeric()),
           azimuth = purrr::map2_dbl(.data$pt1,.data$pt2,~nngeo::st_azimuth(.x,.y))) %>%
    dplyr::summarise(x = mean_azimuth(.data$azimuth,.data$distance)) %>%
    dplyr::pull(.data$x)

}
