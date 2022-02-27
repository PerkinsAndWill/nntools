#' Return NN colors
#'
#' @param ... Can leave empty if you just want to return all colors, specify name(s) if you want to return those colors
#'
#' @return All colors, or requested colors (character vector of hex codes)
#' @export
#'
#' @examples
#' nn_colors()
#' nn_colors("NN Blue")
nn_colors <- function(...){

  cols <- c(...)

  if(is.null(cols)){
    return(nn_base_color_palette %>%
             dplyr::pull(hex_code) %>%
             purrr::set_names(nn_base_color_palette$color_name))
  }

  not_found <- which(!(cols %in% nn_base_color_palette$color_name))
  if(length(not_found) > 0){
    warning(paste0("Could not find colors ", paste0(cols[not_found], collapse = ", "), ". Returned NAs instead.\n"))
  }

  nn_base_color_palette %>%
    dplyr::filter(color_name %in% cols) %>%
    dplyr::pull(hex_code) %>%
    return()
}

#' Return a sequential, diverging, or qualitative palette/ramp of NN colors
#'
#' @param num_colors Number of colors you would like, within the bounds of # of colors available
#' @param type One of seq (sequential), div (diverging) or qual (qualitative)
#' @param palette_name String indicating the name of the palette (not case-sensitive)
#' @param direction Sets the order of colors in the scale. If 1, colors are ordered in the default order. If -1, the order of colors is reversed.
#'
#' @return Character vector of hex codes corresponding to queried colors
#' @export
#'
#' @examples nn_color_ramp(num_colors = 5, type = "seq", palette_name = "Blue", direction=1)
nn_color_ramp <- function(num_colors = 5, type="seq", palette_name = "Blue", direction=1){

  if(type=="seq"){

    #Check if palette name is available
    sub_pal_data <- nn_mono_pal_data %>%
      dplyr::filter(stringr::str_to_lower(color_palette_name) == stringr::str_to_lower(palette_name))

    if(nrow(sub_pal_data)>0){

      if(num_colors > nrow(sub_pal_data)){
        max_colors = nrow(sub_pal_data)
        stop(paste0("You asked for too many colors, there are a maximum of ",max_colors," available for this palette."))
      }

      if(direction==1){
        ordered_hex_codes = sub_pal_data %>%
          dplyr::arrange(sort_order) %>%
          dplyr::pull(hex_code)

        return(grDevices::colorRampPalette(ordered_hex_codes)(num_colors))

      }else if (direction == -1){
        ordered_hex_codes = sub_pal_data %>%
          dplyr::arrange(desc(sort_order)) %>%
          dplyr::pull(hex_code)

        return(grDevices::colorRampPalette(ordered_hex_codes)(num_colors))

      }else{
        stop("direction must be either 1 or -1.")
      }

    }else{
      uq_pal_names = unique(nn_mono_pal_data$color_palette_name)
      stop(
        paste0("The sequential/monochrome palette name you referenced is not available. Please select from the following palettes: ",
               stringr::str_flatten(uq_pal_names,collapse = ", "))
      )
    }

  }else if (type=="div"){
    stop("These palettes are not ready yet, stay tuned for updates!")
  }else if (type=="qual"){
    stop("These palettes are not ready yet, stay tuned for updates!")
  }else{
    stop("Type must be one of 'seq', 'div', or 'qual'")
  }
}
