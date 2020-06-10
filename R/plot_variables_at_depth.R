#'@title Exports ggplot2 objects for temperauture, dissolved oxygen, and
#'  salinity on separate panes
#'@details can only do 4 vars
#'@param dat.tidy Data in tidy format, as returned by the function
#'  \code{convert_to_tidydata()}. Must include four columns: \code{DATE}
#'  (POSIXct), \code{VALUE}, (numeric), \code{VARIABLE} (character),
#'  \code{DEPTH} (ordered factor).
#'@param plot.title Title for plot. Default "" (will this work for loop?)
#'@param vars.to.plot A character vector of variables for which to return plots.
#'  Options are any combination of "Temperature", "Dissolved Oxygen", and
#'  "Salinity". Default is \code{vars.to.plot = c("Temperature", "Dissolved
#'  Oxygen", "Salinity")}.
#'@param ylab.units Character vector of units. Must be in correct order!
#'@param color.palette Color palette of hex colors onto which \code{DEPTH} will
#'  be mapped. Default is \code{color.palette = rev(viridis(6, option = "D"))}.
#'@param date.breaks.major Intervals for major breaks. Default is
#'  \code{date.breaks.major = "2 month"}.
#'@param date.breaks.minor Intervals for minor breaks. Default is
#'  \code{date.breaks.minor = "1 month"}.
#'@param stacked If true;  returns 1 figure; if FALSE, returns each separately
#'@return Returns a list of ggplot2 plots of Temperature, Dissolved Oxygen,
#'  and/or Salinity.
#'@family plot
#'@author Danielle Dempsey
#'@importFrom viridis viridis
#'@importFrom dplyr filter
#'@importFrom ggpubr ggarrange
#'@import ggplot2
#'@export

plot_variables_at_depth <- function(dat.tidy,
                                    plot.title = "",
                                    vars.to.plot = c("Temperature", "Dissolved Oxygen", "Salinity"),
                                    ylab.units = c(expression(paste("(",degree,"C)")),
                                                   "(%)",
                                                   "(ppt)"),
                                    color.palette = rev(viridis(6, option = "D")),
                                    date.breaks.major = "2 month",
                                    date.breaks.minor = "1 month",
                                    stacked = TRUE){

  theme_set(theme_light())

  # Error message in case trying to plot a variable that is not in the dataframe OR
  # a variable is spelled wrong
  if(any(!(vars.to.plot %in%  unique(dat.tidy$VARIABLE)))){

    stop("You are trying to plot a variable that is not in this dataframe. Check spelling in vars.to.plot")
  }

  # Common plot elements ----------------------------------------------------
  # x range (must be POSIXct objects)
  x_date_min <- min(na.omit(dat.tidy$DATE))
  x_date_max <- max(na.omit(dat.tidy$DATE))

  # format for the x-axis
  x_axis_date <- scale_x_datetime(name = "Date",
                                  date_breaks = date.breaks.major,             # major breaks
                                  date_minor_breaks = date.breaks.minor,       # minor breaks
                                  date_labels = "%b-%y",                       # format for showing date
                                  limits = c(x_date_min, x_date_max))          # date range

  # theme
  string_theme <- theme(
    plot.title = element_text(face = "bold"),                # plot title format
    axis.title = element_text(size = 10),                    # axis titles size & color
    axis.text = element_text(size = 9, colour = "black"),    # axis text size & color
    legend.title = element_text(size = 10) ,                 # legend title size
    legend.text = element_text(size = 10)                    # legend text size
  )


  # color scale
  string_color_scale <- scale_colour_manual(name = "Depth (m)",
                                            values = color.palette,
                                            drop = FALSE)

  # size of the points in the legend
  legend_size <-  guides(color = guide_legend(override.aes = list(size = 5)))

  # superchill line
  super.chill <- geom_hline(yintercept = -0.7, linetype = 1, color = "red")


  # Loop over vars.to.plot --------------------------------------------------

  figs <- list(NULL)
  n.vars <- length(vars.to.plot)


  for(i in 1:n.vars){

    var.i <- vars.to.plot[i]

    if(vars.to.plot[i] == "Temperature"){

      y.lab <- expression(paste("Temperature (",degree,"C)"))
    } else  y.lab <-paste(vars.to.plot[i], ylab.units[i], sep = " ")

    dat.i <- dat.tidy %>%  dplyr::filter(VARIABLE == var.i)

    plot.i <- ggplot(dat.i, aes(x = DATE, y = VALUE, color = DEPTH)) +
      geom_point(size = 0.25) +
      scale_y_continuous(name = y.lab) +
      string_color_scale +
      x_axis_date +
      string_theme +
      legend_size

    if(vars.to.plot[i] == "Temperature" & min(na.omit(dat.i$VALUE)) < -0.7) plot.i <- plot.i + super.chill

    figs[[i]] <- plot.i

  }


  # RETURN TO GLOBAL ENV ----------------------------------------------------

  if(stacked == TRUE){

    # add plot title to the top plot (first variable in vars.to.plot)
    figs[[1]] <- figs[[1]] +
      labs(title = plot.title)

    # remove x-axis title from all except the bottom plot (last variable in vars.to.plot)
    for(j in 1:(n.vars - 1)){

      figs[[j]] <- figs[[j]] +
        theme(axis.title.x = element_blank())
    }

    # arrange figs using ggpubr::ggarrange
    if(n.vars == 2) figs.stacked <- ggarrange(figs[[1]], figs[[2]], ncol = 1, common.legend = TRUE, legend = "right")
    if(n.vars == 3) figs.stacked <- ggarrange(figs[[1]], figs[[2]], figs[[3]], ncol = 1, common.legend = TRUE, legend = "right")
    if(n.vars == 4) figs.stacked <- ggarrange(figs[[1]], figs[[2]], figs[[3]], figs[[4]], ncol = 1, common.legend = TRUE, legend = "right")

    figs.stacked # export stacked figs

  } else{
    # add plot.title to each figure
    for(j in 1:(n.vars)){

      figs[[j]] <- figs[[j]] +
        labs(title = plot.title)

    }

    figs # export list of individual figures
  }

}






