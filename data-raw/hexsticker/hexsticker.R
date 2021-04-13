
library(hexSticker)
library(scales)

# colours in CMAR logo
cmar_cols <- c("#063e4d",  "#c7dfe7", "#04a4e4", "#4e7883", "#62aeca",
               "#2db5ec", "#15ace4", "#849ca4", "#7cd4f4", "#2088e8")

show_col(cmar_cols)


outline <-"#063e4d"
fill <-   "#7cd4f4"

hex_cols <- c(outline, fill)
show_col(hex_cols)


string_fig <- "data-raw/hexsticker/cmar5.png"

s <- sticker(string_fig,
             package = "strings",
             p_family = "mono",
             p_fontface = "bold",
             p_size = 65,               # for dpi = 1000; set to 20 otherwise
             p_color = outline,
             p_y = 1.55,

             h_size = 1.2,
             h_color = outline,
             h_fill = fill,

             s_x = 0.97,
             s_y = 0.8,

             #s_height = 1,
              s_width = 0.2,

             #asp = 0.5,

             spotlight = TRUE,
             dpi = 1000,

             filename= "data-raw/hexsticker/strings_hex6.png")


s


#h_color = "#1CADE4",
#h_fill = "#CFEDF9",




