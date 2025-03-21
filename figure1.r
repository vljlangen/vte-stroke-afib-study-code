
# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)
library(showtext)
library(magick)

# Constants
y_max <- 8
header_x <- 0.5
header_y <- 7.7
bar_width <- 0.5
custom_base_size <- 12
axis_text_size <- 16
axis_title_size <- 18
header_text_size <- 6
panel_distance <- 0.1
bar_thickness <- 0.2  
y_axis_distance <- 1.2  
legend_font_size <- 14

# Load specific font from Google Fonts
font_add_google("Rosario", family = "rosario")
# Invoke showtext
showtext_auto()

# Create the data frame with all values
vte_data <- data.frame(
  age_group = rep(c("Under 65 years", "From 65 to 74 years", "75 years or more", "Total"), each = 3),
  condition = rep(c("Any venous thromboembolism", "Pulmonary embolism", "Deep venous thrombosis"), 4),
  period_2007_2010_pct = c(
    1.6, 0.7, 1.1,  # Under 65
    2.4, 1.2, 1.4,  # 65-74
    3.6, 1.7, 2.0,  # 75+
    2.7, 1.3, 1.6   # Total
  ),
  period_2007_2010_n = c(
    376, 158, 245,   # Under 65
    490, 248, 274,   # 65-74
    1297, 629, 745,  # 75+
    2163, 1035, 1264 # Total
  ),
  period_2011_2014_pct = c(
    3.0, 1.2, 2.1,  # Under 65
    4.4, 1.9, 2.9,  # 65-74
    5.5, 2.3, 3.6,  # 75+
    4.6, 1.9, 3.0   # Total
  ),
  period_2011_2014_n = c(
    690, 278, 477,    # Under 65
    1132, 489, 734,   # 65-74
    2560, 1082, 1652, # 75+
    4382, 1849, 2863  # Total
  ),
  period_2015_2018_pct = c(
    4.3, 1.6, 3.1,  # Under 65
    5.7, 2.1, 4.0,  # 65-74
    7.4, 2.6, 5.3,  # 75+
    6.3, 2.3, 4.5   # Total
  ),
  period_2015_2018_n = c(
    873, 329, 627,    # Under 65
    1624, 586, 1140,  # 65-74
    3561, 1277, 2540, # 75+
    6058, 2192, 4307  # Total
  )
)

# View the first few rows
head(vte_data)


# Reshape the data to long format
vte_data_long <- vte_data %>%
  select(age_group, condition, 
         period_2007_2010_pct, period_2011_2014_pct, period_2015_2018_pct) %>%
  pivot_longer(
    cols = starts_with("period"),
    names_to = "period",
    values_to = "percentage"
  ) %>%
  mutate(
    period = case_when(
      period == "period_2007_2010_pct" ~ "2007-2010",
      period == "period_2011_2014_pct" ~ "2011-2014",
      period == "period_2015_2018_pct" ~ "2015-2018"
    ),
    age_group = factor(age_group, 
                      levels = c("Under 65 years", 
                               "From 65 to 74 years",
                               "75 years or more",
                               "Total"))
  )

# Create individual plots for each age group
plot_under65 <- ggplot(subset(vte_data_long, age_group == "Under 65 years"), 
                      aes(x = period, y = percentage, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge", width = bar_width, color = "black", linewidth = bar_thickness) +
  theme_classic(base_size = custom_base_size, base_family = "rosario") +
  labs(x = "Time Period",
       y = "Percentage") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(size = axis_text_size),
        axis.title.y = element_text(size = axis_title_size, face = "bold", margin = margin(r = y_axis_distance * 10))) +
  scale_fill_manual(values = c("#D8D4A7", "#E17A96", "#CEBECB")) +
  scale_y_continuous(limits = c(0, y_max), 
                    breaks = seq(0, y_max, by = 2),
                    #labels = paste0(seq(0, y_max, by = 2), "%"),
                    expand = c(0, 0)) +
  annotate("text", x = header_x, y = header_y, 
           label = "Under 65 years", hjust = 0, size = header_text_size, 
           fontface = "bold", family = "rosario")





plot_65to74 <- ggplot(subset(vte_data_long, age_group == "From 65 to 74 years"), 
                     aes(x = period, y = percentage, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge", width = bar_width, color = "black", linewidth = bar_thickness) +
  theme_classic(base_size = custom_base_size, base_family = "rosario") +
  labs(x = "Time Period",
       y = "Percentage") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(size = axis_text_size),
        axis.title.y = element_text(size = axis_title_size, face = "bold", margin = margin(r = y_axis_distance * 10))) +
  scale_fill_manual(values = c("#D8D4A7", "#E17A96", "#CEBECB")) +
  scale_y_continuous(limits = c(0, y_max), 
                    breaks = seq(0, y_max, by = 2),
                    #labels = paste0(seq(0, y_max, by = 2), "%"),
                    expand = c(0, 0)) +
  annotate("text", x = header_x, y = header_y, 
           label = "65-74 years", hjust = 0, size = header_text_size, 
           fontface = "bold", family = "rosario")





plot_75plus <- ggplot(subset(vte_data_long, age_group == "75 years or more"), 
                     aes(x = period, y = percentage, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge", width = bar_width, color = "black", linewidth = bar_thickness) +
  theme_classic(base_size = custom_base_size, base_family = "rosario") +
  labs(x = "Time Period",
       y = "Percentage") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(size = axis_text_size),
        axis.title.y = element_text(size = axis_title_size, face = "bold", margin = margin(r = y_axis_distance * 10))) +
  scale_fill_manual(values = c("#D8D4A7", "#E17A96", "#CEBECB")) +
  scale_y_continuous(limits = c(0, y_max), 
                    breaks = seq(0, y_max, by = 2),
                    #labels = paste0(seq(0, y_max, by = 2), "%"),
                    expand = c(0, 0)) +
  annotate("text", x = header_x, y = header_y, 
           label = "75 years or more", hjust = 0, size = header_text_size, 
           fontface = "bold", family = "rosario")




plot_total <- ggplot(subset(vte_data_long, age_group == "Total"), 
                    aes(x = period, y = percentage, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge", width = bar_width, color = "black", linewidth = bar_thickness) +
  theme_classic(base_size = custom_base_size, base_family = "rosario") +
  labs(x = "Time Period",
       y = "Percentage",
       fill = "") +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = axis_text_size),
        axis.title.y = element_text(size = axis_title_size, face = "bold", margin = margin(r = y_axis_distance * 10))) +
  scale_fill_manual(values = c("#D8D4A7", "#E17A96", "#CEBECB")) +
  scale_y_continuous(limits = c(0, y_max), 
                    breaks = seq(0, y_max, by = 2),
                    #labels = paste0(seq(0, y_max, by = 2), "%"),
                    expand = c(0, 0)) +
  annotate("text", x = header_x, y = header_y, 
           label = "Total", hjust = 0, size = header_text_size, 
           fontface = "bold", family = "rosario")



# The following plot will be made just for the legend, which will be cut and
# paste later on to the final panel


legend_total <- ggplot(subset(vte_data_long, age_group == "Total"), 
                       aes(x = period, y = percentage, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge", width = bar_width, color = "black", linewidth = bar_thickness) +
  theme_classic(base_size = custom_base_size, base_family = "rosario") +
  labs(x = "Time Period",
       y = "Percentage",
       fill = "") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = legend_font_size, family = "rosario"),  # Updated legend font size
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = axis_text_size),
        axis.title.y = element_text(size = axis_title_size, face = "bold", margin = margin(r = y_axis_distance * 10))) +
  scale_fill_manual(values = c("#D8D4A7", "#E17A96", "#CEBECB")) +
  scale_y_continuous(limits = c(0, y_max), 
                     breaks = seq(0, y_max, by = 2),
                     labels = paste0(seq(0, y_max, by = 2), "%"),
                     expand = c(0, 0))




# Save as PDF with dpi specified
ggsave("article2/Figures/legend.pdf",
       legend_total,
       dpi = 600,
       width = 12,
       height = 8)

# Load the PDF back with {magick} package
pdf_image <- magick::image_read_pdf("article2/Figures/legend.pdf", density = 600)

# Save it as PNG
image_write(pdf_image,
            path = "article2/Figures/legend.png",
            format = "png",
            density = 600)







# Combine plots using plot_grid
combined_plot <- plot_grid(
  plot_under65,
  NULL,
  plot_65to74,
  NULL, NULL, NULL,
  plot_75plus,
  NULL, 
  plot_total,
  ncol = 3,
  align = 'v',
  axis = 'lr',
  rel_heights = c(1, panel_distance, 1, panel_distance, panel_distance, panel_distance, 1, panel_distance, 1),
  rel_widths  = c(1, panel_distance, 1, panel_distance, panel_distance, panel_distance, 1, panel_distance, 1)
)

# Display the plot
combined_plot





# Save as PDF with dpi specified
ggsave("article2/Figures/figure1_top_part.pdf",
       combined_plot,
       dpi = 600,
       width = 12,
       height = 8)


# Load the PDF back with {magick} package
pdf_image <- magick::image_read_pdf("article2/Figures/figure1_top_part.pdf", density = 600)

# Save it as PNG
image_write(pdf_image,
            path = "article2/Figures/figure1_top_part.png",
            format = "png",
            density = 600)




# Do the cut and paste for adding a custom legend

# Read the images
image1 <- image_read("article2/Figures/figure1_top_part.png")
image2 <- image_read("article2/Figures/legend.png")

# Get the dimensions of the images
width1 <- image_info(image1)$width
height1 <- image_info(image1)$height

width2 <- image_info(image2)$width
height2 <- image_info(image2)$height

# Calculate the cropping dimensions
crop_top <- round(0.93 * height2)

# Calculate centering offset
legend_width <- width2 * 0.5  # Approximate width of the legend after cropping
x_offset <- round((width1 - legend_width) / 2)  # Center the legend

# Crop the legend
image2_cropped <- image_crop(image2, geometry = paste0("0x", height2 - crop_top, "+0+", crop_top))

# Create some empty space with a white box
white <- image_blank(width1, 100, "white")

# Append the white box to the panel
result <- image_append(c(image1, white), stack = TRUE)

# Append the legend to the panel
result <- image_append(c(result, image2_cropped), stack = TRUE)

# Save the final result
image_write(result, path = "article2/Figures/figure1_final.png")

