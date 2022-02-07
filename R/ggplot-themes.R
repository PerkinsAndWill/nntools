#' Return NN colors
#'
#' @param ... Can leave empty if you just want to return all colors, specify name(s) if you want to return those colors
#'
#' @return All colors, or requested colors (hex codes)
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

#' Basic Nelson\\Nygaard Plot Theme
#'
#' @param legend_right Legend defaults to top, if you want on right, set this parameter
#' @param base_size Base font size, defaults to 12
#' @param base_family Base font family, defaults to Barlow
#' @param base_line_size Base grid line size
#' @param base_rect_size Base rectangle size
#'
#' @export
nn_basic_theme <- function(legend_right = FALSE,
                     base_size = 12,
                     base_family = "Barlow",
                     base_line_size = base_size / 170,
                     base_rect_size = base_size / 170) {

  if(base_family=="Barlow"){
    #library(showtext)
    avail_font_families = sysfonts::font_families()
    if(!("Barlow" %in% avail_font_families)){
      sysfonts::font_add_google(name = "Barlow", family = "Barlow")
    }
    showtext::showtext_auto()
  }

  half_line <- base_size/2
  grid_line_color <- nn_colors("NN Midnight")
  grid_line_size <- 0.2
  title_text_color <- nn_colors("NN Midnight")
  other_text_color <- nn_colors("NN Midnight")
  plot_bg_color <- nn_colors("NN Harbor")
  panel_bg_color <- nn_colors("NN Harbor")

  if(legend_right == TRUE){
    spec_legend_position <- "right"
    spec_legend_direction <- "vertical"
    legend_justification_spec <- "center"
    legend_box_spacing_spec = ggplot2::unit(2 * half_line, "pt")
  } else {
    spec_legend_position <- "top"
    spec_legend_direction <- "horizontal"
    legend_justification_spec <- c(0,0)
    legend_box_spacing_spec <- ggplot2::unit(0, "char")
  }

  ggplot2::theme_minimal(base_size = base_size,
                         base_family = base_family,
                         base_line_size = base_line_size) %+replace%
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = plot_bg_color, linetype = 0, colour = NA),
      panel.background = ggplot2::element_rect(fill = panel_bg_color, linetype = 0, colour = NA),
      plot.title = ggplot2::element_text(
        color = title_text_color,
        size = ggplot2::rel(1.2),
        face = "bold",
        hjust = 0,
        margin = ggplot2::margin(b = half_line)
      ),
      plot.subtitle = ggplot2::element_text(
        color = other_text_color,
        face = "bold",
        hjust = 0,
        margin = ggplot2::margin(b = half_line)
      ),
      plot.caption = ggplot2::element_text(
        color = other_text_color,
        hjust = 0,
        size = ggplot2::rel(0.8),
        margin = ggplot2::margin(t = half_line)
      ),
      axis.title = ggplot2::element_text(
        color = other_text_color,
        size = ggplot2::rel(0.9),
        face = "bold"
      ),
      axis.text = ggplot2::element_text(
        color = other_text_color,
        size = ggplot2::rel(0.8),
        margin = ggplot2::margin()
      ),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = -0.8 * half_line / 2), hjust = 1),
      axis.line = ggplot2::element_line(
        colour = grid_line_color,
        size = grid_line_size
      ),
      axis.line.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(
        color = grid_line_color,
        size = grid_line_size
      ),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.length = ggplot2::unit(0.5,"char"),
      panel.grid.major.y = ggplot2::element_line(
        color = grid_line_color,
        size = grid_line_size
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = spec_legend_position,
      legend.justification = legend_justification_spec,
      legend.direction = spec_legend_direction,
      legend.title = ggplot2::element_text(hjust = 0,
                                           color = other_text_color,
                                           size = ggplot2::rel(0.9),
                                           face = "bold"),
      legend.spacing.x = ggplot2::unit(1, "char"),
      legend.text = ggplot2::element_text(
        color = other_text_color,
        hjust = 0,
        size = ggplot2::rel(0.8)
      ),
      legend.margin = ggplot2::margin(),
      legend.box.spacing = legend_box_spacing_spec,
      plot.margin = ggplot2::margin(1,1,1,1, unit = "char"),
      plot.title.position = "plot",
      plot.caption.position = "plot",

      complete = TRUE
    )
}

# library(tidyverse)
# library(nycflights13)
#
# flight_demo = flights %>%
#   group_by(carrier) %>%
#   summarise(num_flights = n())
#
# ggplot(flight_demo, aes(x=carrier, y = num_flights))+
#   geom_col() +
#   coord_flip() +
#   nn_basic_theme()+
#   labs(y = "Number of Flights", x = "Carrier")
#
# scales::show_col(nn_colors())

