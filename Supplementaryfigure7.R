library(pacman)
p_load(ggplot2, ggthemes, dplyr, ggthemr, tidyverse, showtext, magick)


# Load specific font from Google Fonts
font_add_google("Rosario", family = "rosario")

# Invoke showtext
showtext_auto()

# To install the ggthemr (themes for ggplot2), run:
#
#devtools::install_github('Mikata-Project/ggthemr')



# Real data

fig2_long <- tibble::tribble(
  ~Status,            ~Year,        ~Statistic,    ~Value,
  
  
  #  "Without anticoagulant", "2007-2010",  "estimate",    1.537229,
  # "Without anticoagulant", "2007-2010",  "upper",        1.749808,
  #  "Without anticoagulant", "2007-2010",  "lower",        1.3504754,
  #  
  #  "Without anticoagulant", "2011-2014",  "estimate",    1.190876,
  #  "Without anticoagulant", "2011-2014",  "upper",       1.369213,
  #  "Without anticoagulant", "2011-2014",  "lower",       1.0357665,
  #  
  #  "Without anticoagulant", "2015-2018",  "estimate",    1.132853,
  #  "Without anticoagulant", "2015-2018",  "upper",        1.308136,
  #  "Without anticoagulant", "2015-2018",  "lower",        0.9810564,
  
  
  
  "Deep venous thromboembolism",    "2007-2010",  "estimate",    1.045263,
  "Deep venous thromboembolism",    "2007-2010",  "upper",        1.364606,
  "Deep venous thromboembolism",    "2007-2010",  "lower",        0.8006514,
  
  "Deep venous thromboembolism",    "2011-2014",  "estimate",   1.097953,
  "Deep venous thromboembolism",    "2011-2014",  "upper",      1.488640,
  "Deep venous thromboembolism",    "2011-2014",  "lower",       0.8098008, 
  
  "Deep venous thromboembolism",    "2015-2018",  "estimate",    1.013593,
  "Deep venous thromboembolism",    "2015-2018",  "upper",        1.354890,
  "Deep venous thromboembolism",    "2015-2018",  "lower",        0.7582694
  
  
)





# Convert long to wide using pivot_wider
fig2_wide <- fig2_long %>%
  pivot_wider(names_from = Statistic, values_from = Value)




# Set your own hand-picked colors
set.seed(123456)
my_colors <- c("#024B7B", "#3F3538", "#AFB0B4", "#46BAC5", "#FF00FF",
               "#00FFFF", "#800080", "#008000", "#000080", "#FFA500")


# Create custom ggtherm style
my_style <- define_palette(
  swatch = my_colors,
  gradient = c(lower = my_colors[1L], upper = my_colors[2L])
)


# Set plot style
ggthemr(my_style)


# Create a mapping of shapes to levels in "Follow-up time"
shape_mapping <- c("Deep venous thromboembolism" = 19)  # Replace with actual levels and corresponding shapes


# Create plot
ggplot(fig2_wide, aes(x=Year, y=estimate, group=Status, color=Status, shape=Status)) + 
  
  ggtitle("IS risk in patients with a history of DVT") +
  
  geom_line(linewidth=0.8) +
  geom_point(size = 3.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.07, size=0.8) +
  theme_classic(base_family = "rosario") +
  scale_shape_manual(values = shape_mapping) + # Apply the shape mapping
  
  labs(y = "Adjusted incidence rate ratio",
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
  
  #  annotate("text", x = 1.6, y = 1.53, label = "Without",
  #           size = 4.5, color="black", hjust = 0.5,
  #           family = "rosario") +
  #  
  #  annotate("text", x = 1.6, y = 1.48, label = "anticoagulant",
  #           size = 4.5, color="black", hjust = 0.5,
  #           family = "rosario") +
  
  
  
  
  #  annotate("text", x = 1.5, y = 1.17, label = "Adjusted for",
  #          size = 4.5, color="black", hjust = 0.5,
  #          family = "rosario") +
  
  #  annotate("text", x = 1.5, y = 1.12, label = "anticoagulant use",
  #          size = 4.5, color="black", hjust = 0.5,
  #          family = "rosario") +
  
  
  # annotate("text", x = 0.90, y = 1.022, label = "Without heart failure",
  #          size = 4.5, color="black", hjust = 0.5,
  #          family = "rosario") +
  # 
  # annotate("text", x = 0.90, y = 0.983, label = "as reference",
  #          size = 4.5, color="black", hjust = 0.5,
  #          family = "rosario") +
  
  annotate("text", x = 0.70, y = 0.91, label = "Without DVT",
           size = 4.0, color="black", hjust = 0.5,
           family = "rosario") +
  
  annotate("text", x = 0.70, y = 0.866, label = "as reference",
           size = 4.0, color="black", hjust = 0.5,
           family = "rosario") +
  
  geom_segment(aes(x = 0.70, y = 0.929, xend = 0.80, yend = 0.993), 
               color = "black", 
               linetype = "solid", 
               size = 0.3) +
  
  theme(legend.position = "none") +
  
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0.6, 1.6, by = 0.2),
                     limits=c(0.5, 1.6)) +
  
  geom_hline(yintercept = 1.00, linetype = "dashed", color = "black")




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

pdf_file <- here("article2", "Figures", "Supplementaryfigure5.pdf")
png_file <- here("article2", "Figures", "Supplementaryfigure5.png")
tiff_file <- here("article2", "Figures", "Supplementaryfigure5.tiff")
jpeg_file <- here("article2", "Figures", "Supplementaryfigure5.jpeg")



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




# 
# ## Export image for the abstract for EHRA
# 
# 
# # max. 1920 x 1080
#  
# # # # Save as PDF with dpi specified
# ggsave("heart_failure_fig2.pdf", dpi = 170, bg = "white", width = 7, height = 6)
# 
# # Load that pdf file with the magick package
# pdf_image <- magick::image_read_pdf("heart_failure_fig2.pdf", density = 170)
# 
# # Save it as PNG
# image_write(pdf_image,
#             path = "heart_failure_fig2.png",
#             format = "png",
#             density = 170)
#  
# 
# # Save it as TIFF
# image_write(pdf_image,
#             path = "heart_failure_fig2.tiff",
#             format = "tiff",
#             density = 170,
#             compression = "LZW")
# 
# # Save it as JPEG
# image_write(pdf_image,
#             path = "heart_failure_fig2.jpeg",
#             format = "jpeg",
#             density = 170,
#             quality = 100)