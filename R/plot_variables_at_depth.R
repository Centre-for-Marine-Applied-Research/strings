#' @title Exports ggplot2 object(s) of variables at depth over time
#'
#' @param dat.tidy Data in tidy format, as returned by the function
#'   \code{convert_to_tidydata()}. Must include four columns: \code{TIMESTAMP}
#'   (POSIXct), \code{VARIABLE} (character), \code{DEPTH} (ordered factor), and
#'   \code{VALUE} (numeric).
#'
#' @param plot.title Title for plot. Default is no title.
#'
#' @param color.palette Color palette of hex colors onto which \code{DEPTH} will
#'   be mapped. Required if there are more than 6 levels in \code{DEPTH}.
#'   Default is \code{color.palette = rev(viridis(6, option = "D"))}.
#'
#' @param date.breaks.major Intervals for major breaks. Default is selected by
#'   \code{get_xaxis_breaks()}.
#'
#' @param date.breaks.minor Intervals for minor breaks. Default is
#'   \code{date.breaks.minor = "1 month"}.
#'
#' @param date.labels.format Format for the date labels. Default is "\%y-\%b"
#'   (two-digit year-three-letter month).
#'
#' @param standard.DO.ylims If \code{TRUE}, the y-limits for dissolved oxygen
#'   are set to c(60, 130) \%. If \code{FALSE}, the y-limits are set to the
#'   \code{ggplot} default.
#'
#' @param alpha.points Value indicating the transparency of the points. 0 is
#'   most transparent; 1 is opaque.
#'
#' @param legend.name Name for the legend. Must be a character string. Default
#'   is \code{legend.name = "Depth (m)"}.
#'
#' @param legend.position Position for the legend. Passed to \code{ggpubr}.
#'   Default is \code{legend.position = "right"}.
#'
#' @return Returns ggplot2 object, a single figure with the plots for each
#'   variable in \code{tidy.data} stacked in a column is returned.
#'
#' @family plot
#' @author Danielle Dempsey
#' @importFrom viridis viridis
#' @importFrom dplyr filter
#' @importFrom ggpubr ggarrange
#' @import ggplot2
#' @export

plot_variables_at_depth <- function(dat.tidy,
                                    plot.title = "",
                                    color.palette = NULL,

                                    date.breaks.major = NULL,
                                    date.breaks.minor = NULL,
                                    date.labels.format = NULL,

                                    standard.DO.ylims = TRUE,

                                    alpha.points = 1,

                                    legend.name = "Depth (m)",
                                    legend.position = "right"
                                   ){


  theme_set(theme_light())

  dat.tidy <- convert_depth_to_ordered_factor(dat.tidy)

  # Common plot elements ----------------------------------------------------
  #  x-axis
  axis.breaks <- get_xaxis_breaks(dat.tidy)

  if(!is.null(date.breaks.major)) axis.breaks$date.breaks.major <- date.breaks.major
  if(!is.null(date.breaks.minor)) axis.breaks$date.breaks.minor <- date.breaks.minor
  if(!is.null(date.labels.format)) axis.breaks$date.labels.format <- date.labels.format

  date.min = min(na.omit(dat.tidy$TIMESTAMP))
  date.max = max(na.omit(dat.tidy$TIMESTAMP))

  x_axis_date <- scale_x_datetime(
    name = "Date",
    date_breaks = axis.breaks$date.breaks.major,          # major breaks
    date_minor_breaks =axis.breaks$date.breaks.minor,     # minor breaks
    date_labels = axis.breaks$date.labels.format,          # format for showing date
    limits = c(date.min, date.max)
  )

  # theme
  string_theme <- theme(
    plot.title = element_text(face = "bold"),                # plot title format
    axis.title = element_text(size = 10),                    # axis titles size & color
    axis.text = element_text(size = 9, colour = "black"),    # axis text size & color
    legend.title = element_text(size = 10) ,                 # legend title size
    legend.text = element_text(size = 10)                    # legend text size
  )

  # color scale
  if(is.null(color.palette)){
    color.palette <- get_colour_palette(dat.tidy)
  }
  string_color_scale <- scale_colour_manual(
    name = legend.name, values = color.palette, drop = FALSE
  )

  # size of the points in the legend
  legend_size <-  guides(color = guide_legend(override.aes = list(size = 5)))

  # superchill line
  super.chill <- geom_hline(yintercept = -0.7, linetype = 1, color = "red")


  # Loop over unique VARIABLE values --------------------------------------------------

  figs <- list(NULL)              # empty list for storing the figures

  # arrange variable to plot in consistent order
  vars.to.plot <- dat.tidy %>%
    select(VARIABLE) %>%
    distinct() %>%
    mutate(
      VARIABLE = factor(VARIABLE),
      VARIABLE = ordered(
        VARIABLE,
        levels = c("Temperature",
                   "Dissolved Oxygen",
                   "Dissolved Oxygen (%)",
                   "Dissolved Oxygen (mg/L)",
                   "Salinity")
      )
    ) %>%
    arrange(VARIABLE)
  vars.to.plot <- vars.to.plot$VARIABLE

  n.vars <- length(vars.to.plot)  # number of variables to plot

  for(i in 1:n.vars){

    var.i <- vars.to.plot[i]

    y.limits <- NULL

    # set y-label
    if(var.i == "Temperature"){
      y.lab <- expression(paste("Temperature (",degree,"C)"))
    }

    # set y-label and y limits
    if(var.i == "Dissolved Oxygen" | var.i == "Dissolved Oxygen (%)"){

      if(standard.DO.ylims == TRUE) y.limits <- c(60, 130)

      y.lab <- "Dissolved Oxygen (%)"
    }

    if(var.i == "Dissolved Oxygen (mg/L)"){

      if(standard.DO.ylims == TRUE) y.limits <- c(0, 15)

      y.lab <- "Dissolved Oxygen (mg/L)"
    }

    # set y-label
    if(var.i == "Salinity") y.lab <- "Salinity (PSU)"


    # # General y-label, supplied by user
    # if(!(var.i %in% c("Temperature", "Dissolved Oxygen", "Salinity"))){
    #   y.lab <- paste(var.i, ylab.units[i], sep = " ")
    # }

    # filter data for the variable of interest
    dat.i <- dat.tidy %>%  dplyr::filter(VARIABLE == var.i)

    # plot var.i
    plot.i <- ggplot(dat.i, aes(x = TIMESTAMP, y = VALUE, color = DEPTH)) +
      geom_point(size = 0.25, alpha = alpha.points) +
      scale_y_continuous(name = y.lab, limits = y.limits) +
      string_color_scale +
      x_axis_date +
      string_theme +
      legend_size

    if(vars.to.plot[i] == "Temperature" & min(na.omit(dat.i$VALUE)) < -0.7) {

      plot.i <- plot.i + super.chill

    }

    figs[[i]] <- plot.i

  } # end of for loop over n.vars


  # RETURN TO GLOBAL ENV ----------------------------------------------------

  # add plot title to the top plot (first variable in vars.to.plot)
  figs[[1]] <- figs[[1]] + labs(title = plot.title)

  # remove x-axis title from all except the bottom plot (last variable in vars.to.plot)
  if(n.vars > 1){

    for(j in 1:(n.vars - 1)) {

      figs[[j]] <- figs[[j]] + theme(axis.title.x = element_blank())

    }
  }

  # arrange figs using ggpubr::ggarrange
  if(n.vars == 1) figs.stacked <- figs[[1]]
  if(n.vars == 2) figs.stacked <- ggarrange(figs[[1]], figs[[2]], ncol = 1, common.legend = TRUE, legend = legend.position)
  if(n.vars == 3) figs.stacked <- ggarrange(figs[[1]], figs[[2]], figs[[3]], ncol = 1, common.legend = TRUE, legend = legend.position)
  if(n.vars == 4) figs.stacked <- ggarrange(figs[[1]], figs[[2]], figs[[3]], figs[[4]], ncol = 1, common.legend = TRUE, legend = legend.position)

  figs.stacked # export stacked figs

}






