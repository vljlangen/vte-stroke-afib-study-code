
# Load packages
library(pacman)
p_load(forestplot)

library(dplyr)

library(showtext)


# Load specific font from Google Fonts
font_add_google("Rosario", family = "rosario")

# Invoke showtext
showtext_auto()



data <- tibble::tribble(
  ~labeltext, ~events, ~patient_years, ~incidence,    ~unadjusted_irr, ~adjusted_irr,  ~mean, ~lower, ~upper, ~group,
  
  "PE",        NA,       NA,         NA,                 NA,                NA,                  1,   1,    1,     "Unadjusted", 
  "  No",     "3906",  "4543.1", "0.86 (0.83-0.89)", "Ref.",            "Ref.",              1,   1,    1,   "Unadjusted", 
  "  Yes",    "39",     "38.9",    "1.00 (0.71-1.37)", "1.17 (0.85-1.60)","1.04 (0.76-1.42)",   1.17, 0.85, 1.60,   "Unadjusted",    
  "DVT",      NA,        NA,         NA,                 NA,                NA,                  1,   1,    1,      "Unadjusted", 
  "  No2",   "3874",   "4515.5", "0.86 (0.83-0.89)", "Ref.",            "Ref.",              1,   1,    1,   "Unadjusted", 
  "  Yes2",   "71",     "66.5",    "1.07 (0.83-1.35)", "1.24 (0.98-1.57)","1.16 (0.92-1.47)",    1.24, 0.98, 1.57,  "Unadjusted",    
  "Any VTE",   NA,       NA,         NA,                 NA,                NA,                  1,   1,    1,        "Unadjusted", 
  "  No3",    "3844",  "4484.7", "0.86 (0.83-0.89)", "Ref.",            "Ref.",              1,   1,    1,     "Unadjusted", 
  "  Yes3",  "101",      "97.3",    "1.04 (0.85-1.26)", "1.21 (0.99-1.48)","1.10 (0.90-1.34)",   1.21, 0.99, 1.48,  "Unadjusted",   
  
  "PE",        NA,      NA,         NA,                  NA,                 NA,                 1,   1,    1,         "Adjusted", 
  "  No",     "3906", "4543.1", "0.86 (0.83-0.89)",  "Ref.",             "Ref.",             1,   1,    1,       "Adjusted", 
  "  Yes",    "39",    "38.9",    "1.00 (0.71-1.37)",  "1.17 (0.85-1.60)", "1.04 (0.76-1.42)",   1.04, 0.76, 1.42, "Adjusted",    
  "DVT",      NA,       NA,         NA,                  NA,                 NA,                 1,   1,    1        ,"Adjusted", 
  "  No2",   "3874",  "4515.5", "0.86 (0.83-0.89)",  "Ref.",             "Ref.",             1,   1,    1,    "Adjusted", 
  "  Yes2",   "71",    "66.5",    "1.07 (0.83-1.35)",  "1.24 (0.98-1.57)", "1.16 (0.92-1.47)",    1.16, 0.92, 1.47, "Adjusted",   
  "Any VTE",   NA,      NA,         NA,                  NA,                 NA,                 1,   1,    1,       "Adjusted", 
  "  No3",    "3844", "4484.7", "0.86 (0.83-0.89)",  "Ref.",             "Ref.",             1,   1,    1,       "Adjusted", 
  "  Yes3",  "101",     "97.3",    "1.04 (0.85-1.26)",  "1.21 (0.99-1.48)", "1.10 (0.90-1.34)",    1.10, 0.90, 1.34, "Adjusted",    
)




# Possible X ticks:

#> log(0.5)
#[1] -0.6931472
# > log(0.75)
# [1] -0.2876821
# > log(0.125)
# [1] -2.079442
# > log(1.50)
# [1] 0.4054651

# > log(0.8)
# [1] -0.2231436
# > log(1.4)
# [1] 0.3364722

# > log(0.9)
# [1] -0.1053605

pdf(file='article2/Figures/forest_plot_lowormoderate.pdf',  height=6, width=12) # Open PDF device with specific file name


data |>
  group_by(group) |>
  forestplot(labeltext = c(labeltext, events, patient_years, incidence, unadjusted_irr, adjusted_irr),
             graph.pos=5,
             xticks = c(0.75, 1, 1.60),
             xlog = TRUE,
             zero=1, cex=0.9, lineheight = "auto", colgap=unit(8,"mm"),
             lwd.ci=2, ci.vertices=FALSE, ci.vertices.height = 0.2,
             txt_gp=fpTxtGp(label=gpar(cex=2.25),
                            title=gpar(cex = 1.2)),
             fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
             boxsize = .22, # We set the box size to better visualize the type
             line.margin = .2, # We need to add this to avoid crowding
             xlab = "IRR") |>
  fp_set_style(align = "lcccccccc",
               hrz_lines = "#999999",
               line = "black",
               
               txt_gp = fpTxtGp(title=gpar(cex = 1.8),
                                label = gpar(fontfamily = "rosario", cex = 1.2),
                                ticks = gpar(fontfamily = "rosario", cex = 1.2),
                                xlab  = gpar(fontfamily = "rosario", cex = 1.2)),
               
               box = c("white", "black") |> lapply(function(x) gpar(fill = x, col = "black")),
               default = gpar(vertices = TRUE)) |>
  fp_set_zebra_style("#F5F9F9") |>
  
  fp_add_header(labeltext = c("Subgroup"),
                events = c("Events"),
                patient_years = c("Patient years"),
                incidence = "Incidence",
                unadjusted_irr = "Unadjusted IRR",
                adjusted_irr = "Adjusted IRR")


dev.off()




