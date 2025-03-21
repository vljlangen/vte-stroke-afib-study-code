
###########################################################################
###########################################################################
###                                                                     ###
###                            LOAD PACKAGES                            ###
###                                                                     ###
###########################################################################
###########################################################################

library(pacman)
p_load(here, ggplot2, ggthemes, dplyr, ggthemr, tidyverse, showtext, magick)


# Load specific font from Google Fonts
font_add_google("Rosario", family = "rosario")

# Invoke showtext
showtext_auto()

# To install the ggthemr (themes for ggplot2), run:
#
#devtools::install_github('Mikata-Project/ggthemr')




###########################################################################
###########################################################################
###                                                                     ###
###                     CREATE BOGUS DATA IF NEEDED                     ###
###                                                                     ###
###########################################################################
###########################################################################

# fig2_long <- tibble::tribble(
#   ~Status,            ~Year,        ~Statistic,    ~Value,
#   "Deep venous thromboembolism",    "2007-2010",  "estimate",    55,
#   "Deep venous thromboembolism",    "2007-2010",  "upper",       60,
#   "Deep venous thromboembolism",    "2007-2010",  "lower",       50,
#   "Deep venous thromboembolism",    "2011-2014",  "estimate",    42,
#   "Deep venous thromboembolism",    "2011-2014",  "upper",       47,
#   "Deep venous thromboembolism",    "2011-2014",  "lower",       37,
#   "Deep venous thromboembolism",    "2015-2018",  "estimate",    35,
#   "Deep venous thromboembolism",    "2015-2018",  "upper",       39,
#   "Deep venous thromboembolism",    "2015-2018",  "lower",       31,
#   
#   "No deep venous thromboembolism", "2007-2010",  "estimate",    33,
#   "No deep venous thromboembolism", "2007-2010",  "upper",       36,
#   "No deep venous thromboembolism", "2007-2010",  "lower",       30,
#   "No deep venous thromboembolism", "2011-2014",  "estimate",    28,
#   "No deep venous thromboembolism", "2011-2014",  "upper",       31,
#   "No deep venous thromboembolism", "2011-2014",  "lower",       25,
#   "No deep venous thromboembolism", "2015-2018",  "estimate",    22,
#   "No deep venous thromboembolism", "2015-2018",  "upper",       25,
#   "No deep venous thromboembolism", "2015-2018",  "lower",       19
# )



###########################################################################
###########################################################################
###                                                                     ###
###                              REAL DATA                              ###
###                                                                     ###
###########################################################################
###########################################################################

fig2_long <- 
  structure(list(Status = c("Deep venous thromboembolism", "Deep venous thromboembolism", "Deep venous thromboembolism", 
                            "Deep venous thromboembolism", "Deep venous thromboembolism", "Deep venous thromboembolism", "Deep venous thromboembolism", 
                            "Deep venous thromboembolism", "Deep venous thromboembolism", "No deep venous thromboembolism", "No deep venous thromboembolism", 
                            "No deep venous thromboembolism", "No deep venous thromboembolism", "No deep venous thromboembolism", "No deep venous thromboembolism", 
                            "No deep venous thromboembolism", "No deep venous thromboembolism", "No deep venous thromboembolism"), 
                 Year = c("2007-2010", "2007-2010", "2007-2010", "2011-2014", 
                          "2011-2014", "2011-2014", "2015-2018", "2015-2018", "2015-2018", 
                          "2007-2010", "2007-2010", "2007-2010", "2011-2014", "2011-2014", 
                          "2011-2014", "2015-2018", "2015-2018", "2015-2018"),
                 Statistic = c("estimate", 
                               "upper", "lower", "estimate", "upper", "lower", "estimate", 
                               "upper", "lower", "estimate", "upper", "lower", "estimate", 
                               "upper", "lower", "estimate", "upper", "lower"),
                 Value = c(3.310, 
                           4.308, 2.493, 2.514, 2.907, 2.162, 1.874, 2.098, 1.668, 
                           2.522, 2.609, 2.436, 1.858, 1.902, 1.815, 1.495, 1.526, 
                           1.463)), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA,  -18L))




# Convert long to wide using pivot_wider
fig2_wide <- fig2_long %>%
  pivot_wider(names_from = Statistic, values_from = Value)



############################################################################
############################################################################
###                                                                      ###
###                             SET UP STYLE                             ###
###                                                                      ###
############################################################################
############################################################################



# Set your own hand-picked colors
set.seed(123456)
my_colors <- c("#024B7B", "#5B7DB2", "#E19A3F", "#46BAC5", "#FF00FF",
               "#00FFFF", "#800080", "#008000", "#000080", "#FFA500")


# Create custom ggtherm style
my_style <- define_palette(
  swatch = my_colors,
  gradient = c(lower = my_colors[1L], upper = my_colors[2L])
)


# Set plot style
ggthemr(my_style)


# Create a mapping of shapes to levels in "Follow-up time"
shape_mapping <- c("Deep venous thromboembolism" = 19, "No deep venous thromboembolism" = 15)  # Replace with actual levels and corresponding shapes



###########################################################################
###########################################################################
###                                                                     ###
###                             CREATE PLOT                             ###
###                                                                     ###
###########################################################################
###########################################################################




# Create plot
ggplot(fig2_wide, aes(x=Year, y=estimate, group=Status, color=Status, shape=Status)) + 
  
  ggtitle("Ischemic stroke rate in atrial fibrillation\nby VTE status") +
  
  geom_line(linewidth=0.8) +
  geom_point(size = 3.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.07, size=0.8) +
  theme_classic(base_family = "rosario") +
  scale_shape_manual(values = shape_mapping) + # Apply the shape mapping
  
  labs(y = "Ischemic stroke rate (per 1000 patient years)",
       x =  "Calendar year") +
  
  theme(#plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 17),
    plot.margin = margin(20, 20, 20, 20),
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  
  theme(axis.title.x = element_text(
    margin = margin(t = 19, unit = "pt")),  # Adjust margin for x-axis title
    axis.title.y = element_text(
      margin = margin(r = 23, unit = "pt"))) +  # Adjust margin for y-axis title
  
  annotate("text", x = 2.2, y = 2.7, label = "Deep venous thromboembolism",
           size = 4.5, color="black", hjust = 0,
           family = "rosario") +
  annotate("text", x = 2.1, y = 1.2, label = "No deep venous thromboembolism",
           size = 4.5, color="black", hjust = 0,
           family = "rosario") +
  
  theme(legend.position = "none") +
  
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 70, by = 1),
                     limits=c(0, 5))

# labs(y = "Ischemic stroke rate\n(per 1000 patient years)",
#      x =  "Calendar year") +
# theme(legend.text = element_text(size = 16),  # Adjust the legend text size
#       legend.title = element_text(size = 18))   +

#   scale_x_discrete(limits = c("2007-2008", "2009-2010",
#                               "2011-2012", "2013-2014",
#                               "2015-2016", "2017-2018"))




############################################################################
############################################################################
###                                                                      ###
###                             EXPORT IMAGE                             ###
###                                                                      ###
############################################################################
############################################################################


#####################################################################################
##  Use package {here} to load data sets in a cross-platform - PC, Mac etc. - way  ##
#####################################################################################

# First, tell where you are right now - i.e. location of the file you are running right now

here::i_am("README.md")

# After that, you can check where the project root is by running "here()"
here()


# Use {here} package to refer to subfolders to avoid cross-platform hiccups

pdf_file <- here("article2", "Figures", "Supplementaryfigure3.pdf")
png_file <- here("article2", "Figures", "Supplementaryfigure3.png")
tiff_file <- here("article2", "Figures", "Supplementaryfigure3.tiff")
jpeg_file <- here("article2", "Figures", "Supplementaryfigure3.jpeg")




# max. 1920 x 1080

# # # Save as PDF with dpi specified
ggsave(pdf_file, dpi = 300, bg = "white", width = 7, height = 6)

# Load that pdf file with the magick package
pdf_image <- magick::image_read_pdf(pdf_file, density = 300)



# Save it as PNG
image_write(pdf_image,
            path = png_file,
            format = "png",
            density = 300)

# Save it as TIFF
image_write(pdf_image,
            path = tiff_file,
            format = "tiff",
            density = 300,
            compression = "LZW")

# Save it as JPEG
image_write(pdf_image,
            path = jpeg_file,
            format = "jpeg",
            density = 300,
            quality = 100)

# Clean up variables
rm(pdf_file,
   png_file,
   tiff_file,
   jpeg_file)






# # max. 1920 x 1080 for an abstract to EHRA
#  
# # # # Save as PDF with dpi specified
# ggsave(pdf_file, dpi = 170, bg = "white", width = 7, height = 6)
# 
# # Load that pdf file with the magick package
# pdf_image <- magick::image_read_pdf(pdf_file, density = 170)
# 
# 
# 
# # Save it as PNG
# image_write(pdf_image,
#             path = png_file,
#             format = "png",
#             density = 170)
# 
# # Save it as TIFF
# image_write(pdf_image,
#             path = tiff_file,
#             format = "tiff",
#             density = 170,
#             compression = "LZW")
# 
# # Save it as JPEG
# image_write(pdf_image,
#             path = jpeg_file,
#             format = "jpeg",
#             density = 170,
#             quality = 100)
# 
# # Clean up variables
# rm(pdf_file,
#    png_file,
#    tiff_file,
#    jpeg_file)
