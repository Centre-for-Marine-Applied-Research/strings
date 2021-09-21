
<!-- README.md is generated from README.Rmd. Please edit that file -->

# strings

<!-- badges: start -->

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![](https://img.shields.io/badge/devel%20version-1.6.2-blue.svg)](https://github.com/centre-for-marine-applied-research/strings)
[![CodeFactor](https://www.codefactor.io/repository/github/centre-for-marine-applied-research/strings/badge)](https://www.codefactor.io/repository/github/centre-for-marine-applied-research/strings)
[![R build
status](https://github.com/centre-for-marine-applied-research/strings/workflows/R-CMD-check/badge.svg)](https://github.com/centre-for-marine-applied-research/strings/actions)

<!-- badges: end -->

Compile, format, and visualize water quality (temperature, dissolved
oxygen, salinity) data measured by different brands of autonomous
sensors.

## Installation

You can install the development version of strings from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Centre-for-Marine-Applied-Research/strings")
```

## Background

The Centre for Marine Applied Research ([CMAR](https://cmar.ca/))
coordinates an extensive [Coastal Monitoring
Program](https://cmar.ca/2020/07/14/coastal-monitoring-program/) that
measures [Essential Ocean
Variables](https://www.goosocean.org/index.php?option=com_content&view=article&id=14&Itemid=114)
(e.g., temperature, dissolved oxygen, salinity, sea state, currents),
typically within 1 km of the coast of Nova Scotia, Canada. The `strings`
package is used to compile, format, and visualize water quality data
collected through this program.

Water quality data (temperature, dissolved oxygen, and salinity) is
collected using “sensor strings”. Each string is attached to the
seafloor by an anchor and suspended by a sub-surface buoy, with
autonomous sensors attached at various depths (Figure 1). A string
typically includes sensors from three manufacturers: Hobo (Onset?),
aquaMeasure (InnovaSea?), and Vemco (Table X). Strings are typically
deployed at a station for 6 – 12 months and data are measured every 1
minute to 1 hour, resulting in tens- to hundreds- of thousands of
observations for a single deployment.

<div class="figure" style="text-align: center">

<img src="inst/image/README_fig1.PNG" alt="Might crop out the text and put it in a table?" width="65%" />

<p class="caption">

Might crop out the text and put it in a table?

</p>

</div>

(After retrieval?) Data from each sensor is exported to a separate \*csv
file (using manufacturer-specific software). Each type of sensor
generates a data file with unique columns and header fields, which poses
a significant challenge for compiling all data from a deployment into a
single format for analysis.

The strings package was originally built to address this challenge, and
now offers functions to compile, format, visualize, and convert sensor
string data.

`strings` was developed specifically to streamline CMAR’s workflow, but
was designed to be flexible enough that other users can apply it to
process data from the accepted sensors. Refer to vignettes for more
detail.

Processed data from CMAR’s Coastal Monitoring Program can be viewed and
downloaded from …. \[cheat sheet\].

include example of compiled data here?

## Example

``` r
library(strings)

library(readr)
```

Consider a string deployed from May 31, 2019 to October 19, 2019 with
three sensors:

<table>

<thead>

<tr>

<th style="text-align:left;">

Sensor

</th>

<th style="text-align:center;">

Serial\#

</th>

<th style="text-align:center;">

Depth

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

HOBO Pro V2

</td>

<td style="text-align:center;">

10755220

</td>

<td style="text-align:center;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

aquaMeasure DOT

</td>

<td style="text-align:center;">

670364

</td>

<td style="text-align:center;">

5

</td>

</tr>

<tr>

<td style="text-align:left;">

VR2AR

</td>

<td style="text-align:center;">

547109

</td>

<td style="text-align:center;">

15

</td>

</tr>

</tbody>

</table>

### Title for this section (Raw data? Sensor export? Separate data files?)

The data from each sensor is exported to a separate \*csv file, each
with manufacturer-specific columns.

Import raw data files:

``` r
path <- system.file("extdata", package = "strings")

hobo_raw <- read_csv(paste0(path, "/HOBO/10755220.csv"))
#> 
#> -- Column specification --------------------------------------------------------
#> cols(
#>   `#` = col_double(),
#>   `Date Time, GMT+00:00` = col_character(),
#>   `Temp, °C (LGR S/N: 10755220, SEN S/N: 10755220)` = col_double(),
#>   X4 = col_logical(),
#>   `Coupler Attached (LGR S/N: 10755220)` = col_logical(),
#>   `Host Connected (LGR S/N: 10755220)` = col_logical(),
#>   `Stopped (LGR S/N: 10755220)` = col_logical(),
#>   `End Of File (LGR S/N: 10755220)` = col_logical()
#> )

aquaMeasure_raw <- read_csv(paste0(path, "/aquaMeasure/aquaMeasure-670364_2019-10-19_UTC.csv"))
#> 
#> -- Column specification --------------------------------------------------------
#> cols(
#>   `Timestamp(UTC)` = col_character(),
#>   Sensor = col_character(),
#>   `Record Type` = col_character(),
#>   `Dissolved Oxygen` = col_double(),
#>   Temperature = col_logical()
#> )

vemco_raw <-  read_csv(paste0(path, "/Vemco/Vemco_Borgles_Island_2019_05_30.csv"))
#> 
#> -- Column specification --------------------------------------------------------
#> cols(
#>   `Date and Time (UTC)` = col_character(),
#>   Receiver = col_character(),
#>   Description = col_character(),
#>   Data = col_double()
#> )
```

Raw Hobo data:

``` r
head(hobo_raw)
#> # A tibble: 6 x 8
#>     `#` `Date Time, GMT+0~ `Temp, °C (LGR S/N: 1075~ X4    `Coupler Attached (L~
#>   <dbl> <chr>                                  <dbl> <lgl> <lgl>                
#> 1     1 2019-05-30 18:00                       12.2  NA    NA                   
#> 2     2 2019-05-30 19:00                        7.87 NA    NA                   
#> 3     3 2019-05-30 20:00                        6.58 NA    NA                   
#> 4     4 2019-05-30 21:00                        6.66 NA    NA                   
#> 5     5 2019-05-30 22:00                        6.66 NA    NA                   
#> 6     6 2019-05-30 23:00                        7.29 NA    NA                   
#> # ... with 3 more variables: Host Connected (LGR S/N: 10755220) <lgl>,
#> #   Stopped (LGR S/N: 10755220) <lgl>, End Of File (LGR S/N: 10755220) <lgl>
```

Raw aquaMeasure data:

``` r
head(aquaMeasure_raw)
#> # A tibble: 6 x 5
#>   `Timestamp(UTC)`        Sensor     `Record Type`  `Dissolved Oxyg~ Temperature
#>   <chr>                   <chr>      <chr>                     <dbl> <lgl>      
#> 1 352s after startup (ti~ aquaMeasu~ Dissolved Oxy~             101. NA         
#> 2 1691s after startup (t~ aquaMeasu~ Dissolved Oxy~             100. NA         
#> 3 3015s after startup (t~ aquaMeasu~ Dissolved Oxy~             100. NA         
#> 4 4346s after startup (t~ aquaMeasu~ Dissolved Oxy~             101. NA         
#> 5 5690s after startup (t~ aquaMeasu~ Dissolved Oxy~             101. NA         
#> 6 364s after startup (ti~ aquaMeasu~ Dissolved Oxy~             101. NA
```

Raw Vemco data:

``` r
head(vemco_raw)
#> # A tibble: 6 x 4
#>   `Date and Time (UTC)` Receiver     Description  Data
#>   <chr>                 <chr>        <chr>       <dbl>
#> 1 2019-05-30 20:00      VR2AR-547109 Temperature   4.2
#> 2 2019-05-31 0:02       VR2AR-547109 Temperature   4.6
#> 3 2019-05-31 0:07       VR2AR-547109 Temperature   4.6
#> 4 2019-05-31 0:12       VR2AR-547109 Temperature   4.6
#> 5 2019-05-31 0:17       VR2AR-547109 Temperature   4.7
#> 6 2019-05-31 0:22       VR2AR-547109 Temperature   4.6
```

Something about how this data is messy / hard to work with in this
format

### Compile data

Compile data from the 3 sensors using `strings::compile_all_data()`:

``` r

# deployment information for metadata:

# deployment and retrieval dates
deployment <- data.frame("START" = "2019-05-30", "END" = "2019-10-19")

serial.table.HOBO <- data.frame("SENSOR" = "HOBO-10755220", "DEPTH" = "2m")
serial.table.aM <- data.frame("SENSOR" = "aquaMeasure-670364", "DEPTH" = "5m")
depth.vemco <- "15m"


ALL_data <- compile_all_data(path = path,
                             deployment.range = deployment,
                             area.name = area,
                             # hobo
                             serial.table.HOBO = serial.table.HOBO,
                             # aquaMeasure
                             serial.table.aM = serial.table.aM,
                             # vemco
                             depth.vemco = depth.vemco)
#> Warning: Missing column names filled in: 'X4' [4], 'X5' [5], 'X6' [6], 'X7' [7],
#> 'X8' [8]
#> Warning: Missing column names filled in: 'X4' [4]
#> [1] "HOBO data compiled"
#> [1] "found Temperature in file aquaMeasure-670364_2019-10-19_UTC.csv"     
#> [2] "found Dissolved Oxygen in file aquaMeasure-670364_2019-10-19_UTC.csv"
#> [1] "aquaMeasure data compiled"
#> [1] "Vemco data compiled: Temperature"
```

The data is compiled in a “wide” format, with metadata in the first four
rows indicating the deployment period, the sensor serial number, the
variable and depth of the sensor, and the timezone of the timestamps.
The first column is an index column, starting at -4 to account for the
metadata rows. The remaining columns alternate between timestamp (in the
format “Y-m-d H:M:S”) and variable value (rounded to three decimal
places). Note that because the sensors can be initialized at different
times and record on different intervals, values in a single row do not
necessarily correspond to the same timestamp.

This format is convenient for human readers, who can quickly scan the
metadata to determine the number of sensors deployed, the depths of
deployment, etc. However, this format is less convenient for analysis
(e.g., to include include the metadata, all values were converted to
class character). Prior to analysis, the dataframe should be converted
to a “long” (“tidy”) format, and the values should be converted to the
appropriate class (e.g., POSIXct for the timestamps and numeric for
variable values). This can be done using the `convert_to_tidydata()`
function, which returns a dataframe with the following columns:

  - `DEPLOYMENT_RANGE`: The deployment and retrieval dates (character)
  - `SENSOR`: The sensor that recorded the measurement (character)
  - `TIMESTAMP`: The timestamp of the measurement (POSIXct)
  - `VARIABLE`: The parameter measured (Temperature, Dissolved Oxygen,
    or Salinity) (character)
  - `DEPTH`: The depth of the sensor (ordered factor)
  - `VALUE:` The value of the measurement (numeric)
