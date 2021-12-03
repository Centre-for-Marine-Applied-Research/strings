
#' Plot variables at depth with faceted ggplot
#'
#' @param dat.tidy Tidy data
#' @param color.pal Optional color palette.

#' @return ggplot object
#' @import ggplot2
#' @import dplyr
#'
#' @export
#'


ggplot_variables_at_depth <- function(dat.tidy, color.pal = NULL){

  dat.tidy <- dat.tidy %>%
    convert_depth_to_ordered_factor()

  if(is.null(color.pal)) color.pal <- get_colour_palette(dat.tidy)
  axis.breaks <- get_xaxis_breaks(dat.tidy)

  p <- ggplot(dat.tidy, aes(x = TIMESTAMP, y = VALUE, col = DEPTH)) +
    geom_point(size = 0.25) +
    scale_x_datetime(
      name = "Date",
      breaks = axis.breaks$date.breaks.major,
      minor_breaks = axis.breaks$date.breaks.minor,
      date_labels =  axis.breaks$date.labels.format
    ) +
    scale_colour_manual(name = "Depth",
                        values = color.pal,
                        drop = FALSE) +
    guides(color = guide_legend(override.aes = list(size = 4))) +
    theme_light()

  if("VARIABLE" %in% colnames(dat.tidy)){

    if(length(unique(dat.tidy$VARIABLE)) > 1) {

      p <- p + facet_wrap(~VARIABLE, ncol = 1, scales = "free_y",
                          strip.position = "left") +
        theme(
          strip.placement = "outside",
              strip.background = element_blank(),
              strip.text = element_text(colour = "black", size = 10)
          ) +
        scale_y_continuous(name = "")
    }
  }

  p


}


