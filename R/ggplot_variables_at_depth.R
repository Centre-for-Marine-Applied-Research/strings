
#' Plot variables at depth with faceted ggplot
#'
#' @param dat.tidy Tidy data
#' @param superchill Logical
#'
#' @return ggplot object
#' @import ggplot
#' @import dplyr
#'
#' @export
#'


ggplot_variables_at_depth <- function(dat.tidy, superchill = FALSE){

  dat.tidy <- dat.tidy %>%
    convert_depth_to_ordered_factor()

  color.pal <- get_colour_palette(dat.tidy)
  axis.breaks <- get_xaxis_breaks(dat.tidy)
  n.facet <- length(unique(dat.tidy$VARIABLE))

  p <- ggplot(dat.tidy, aes(x = TIMESTAMP, y = VALUE, col = DEPTH)) +
    geom_point(size = 0.25) +
    facet_wrap(~VARIABLE, ncol = 1, scales = "free_y") +
    scale_x_datetime(
      breaks = axis.breaks$date.breaks.major,
      minor_breaks = axis.breaks$date.breaks.minor,
      date_labels =  axis.breaks$date.labels.format
    ) +
    scale_colour_manual(name = "Depth",
                        values = color.pal,
                        drop = FALSE) +
    guides(color = guide_legend(override.aes = list(size = 4))) +
    theme_light()

  #if(n.facet == 1) p <- p + theme(strip.background = element_rect(NA))

  if(isTRUE(superchill)) p <- p + geom_hline(yintercept = -0.7, col = 2, lty = 2)

  p


}









