#'@title Exports ggplot2 objects for temperauture, dissolved oxygen, and
#'  salinity on separate panes
#'@param dat.tidy Data in tidy format, as returned by the function
#'  \code{convert_to_tidydata()}. Must include four columns: \code{DATE}
#'  (POSIXct), \code{VALUE}, (numeric), \code{VARIABLE} (character),
#'  \code{DEPTH} (ordered factor).
#'@param vars.to.plot A character vector of variables for which to return plots.
#'  Options are any combination of "Temperature", "Dissolved Oxygen", and
#'  "Salinity". Default is \code{vars.to.plot = c("Temperature", "Dissolved
#'  Oxygen", "Salinity")}.
#'@param color.palette Color palette of hex colors onto which \code{DEPTH} will
#'  be mapped. Default is \code{color.palette = rev(viridis(6, option = "D"))}.
#'@param date.breaks.major Intervals for major breaks. Default is
#'  \code{date.breaks.major = "2 month"}.
#'@param date.breaks.minor Intervals for minor breaks. Default is
#'  \code{date.breaks.minor = "1 month"}.
#'@return Returns a list of ggplot2 plots of Temperature, Dissolved Oxygen, and/or Salinity.
#'@family plot
#'@author Danielle Dempsey
#'@importFrom viridis viridis
#'@importFrom dplyr filter
#'@import ggplot2
#'@export

plot_temp_DO_sal <- function(dat.tidy,
                             vars.to.plot = c("Temperature", "Dissolved Oxygen", "Salinity"),
                             color.palette = rev(viridis(6, option = "D")),
                             date.breaks.major = "2 month",
                             date.breaks.minor = "1 month"
){

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


  # TEMPERATURE -------------------------------------------------------------

  if("Temperature" %in% vars.to.plot){

    dat.temp <- dat.tidy %>%  dplyr::filter(VARIABLE == "Temperature")

    temp.plot <- ggplot(dat.temp, aes(x = DATE, y = VALUE, color = DEPTH)) +
      geom_point(size = 0.25) +
      scale_y_continuous(name = expression(paste("Temperature (",degree,"C)"))) +
      string_color_scale +
      x_axis_date +
      string_theme +
      legend_size

    if(min(na.omit(dat.temp$VALUE)) < -0.7) temp.plot <- temp.plot + super.chill

  } else temp.plot = NULL

  # DISSOLVED OXYGEN --------------------------------------------------------

  if("Dissolved Oxygen" %in% vars.to.plot){

    dat.DO <- dat.tidy %>%  dplyr::filter(VARIABLE == "Dissolved Oxygen")

    DO.plot <- ggplot(dat.DO, aes(x = DATE, y = VALUE, color = DEPTH)) +
      geom_point(size = 0.25) +
      string_color_scale +
      x_axis_date +
      scale_y_continuous(name = "Dissolved Oxygen (%)") +
      string_theme +
      legend_size

  } else DO.plot = NULL


  # SALINITY ----------------------------------------------------------------

  if("Salinity" %in% vars.to.plot){

    dat.sal <-  dat.tidy %>% dplyr::filter(VARIABLE == "Salinity")

    sal.plot <- ggplot(dat.sal, aes(x = DATE, y = VALUE, color = DEPTH)) +
      geom_point(size = 0.25) +
      scale_y_continuous(name =  "Salinity (ppt)") +
      string_color_scale +
      x_axis_date +
      string_theme +
      legend_size

  } else sal.plot = NULL


  # RETURN TO GLOBAL ENV ----------------------------------------------------

  list("Temp" = temp.plot,
       "DO" = DO.plot,
       "Sal" = sal.plot)

}






