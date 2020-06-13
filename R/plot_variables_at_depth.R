#'@title Exports ggplot2 object(s) of variables at depth over time
#'@param dat.tidy Data in tidy format, as returned by the function
#'  \code{convert_to_tidydata()}. Must include four columns: \code{DATE}
#'  (POSIXct), \code{VARIABLE} (character), \code{DEPTH} (ordered factor), and
#'  \code{VALUE} (numeric).
#'@param plot.title Title for plot. Default is blank: \code{plot.title = ""}.
#'@param vars.to.plot A character vector of variables to plot. Each string must
#'  match an entry in the \code{VARIABLE} column of \code{dat.tidy}. Default is
#'  \code{vars.to.plot = c("Temperature", "Dissolved Oxygen", "Salinity")}.
#'@param ylab.units Character vector of units as they will appear in the y-axis
#'  title. Must be in the same order as the associated variable in
#'  \code{vars.to.plot}. Note: the units for \code{vars.to.plot = "Temperature"} are
#'  hard-coded to ensure the degree symbol renders.
#'@param color.palette Color palette of hex colors onto which \code{DEPTH} will
#'  be mapped. Default is \code{color.palette = rev(viridis(6, option = "D"))}.
#'@param date.breaks.major Intervals for major breaks. Default is
#'  \code{date.breaks.major = "2 month"}.
#'@param date.breaks.minor Intervals for minor breaks. Default is
#'  \code{date.breaks.minor = "1 month"}.
#'@param stacked Logical value indicating what figures to return.  If
#'  \code{stacked = TRUE}, a single figure with the plots for each variable in
#'  \code{vars.to.plot} stacked in a column is returned. Plots will be stacked
#'  in the order the variables appear in \code{vars.to.plot}. \code{plot.title}
#'  will be placed on the top plot. If \code{stacked = FALSE}, a list of the
#'  individual plot for each variable will be returned. Default is \code{stacked
#'  = TRUE}.
#'@return Returns ggplot2 object(s). If \code{stacked = TRUE}, a single figure
#'  with the plots for each variable in \code{vars.to.plot} stacked in a column
#'  is returned. Plots will be stacked in the order the variables appear in
#'  \code{vars.to.plot}. \code{plot.title} will be placed on the top plot. If
#'  \code{stacked = FALSE}, a list of the individual plot for each variable will
#'  be returned.
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
                                    ylab.units = c("deg C)",
                                                   "(%)",
                                                   "(ppt)"),
                                    color.palette = rev(viridis(6, option = "D")),
                                    date.breaks.major = "2 month",
                                    date.breaks.minor = "1 month",
                                    stacked = TRUE){

  #plot.new()

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






