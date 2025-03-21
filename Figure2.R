
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
  "PE",        NA,      NA,         NA,                  NA,                 NA,                 1,   1,    1,         "Adjusted",
  "  OAC",     NA,      NA,         NA,                  NA,                 NA,               1,   1,    1,   "Unadjusted", 
  "  OAC",     NA,      NA,         NA,                  NA,                 NA,               1,   1,    1,   "Adjusted", 
  "    No",     "19 262",  "11 014.9", "1.75 (1.72-1.77)", "Ref.",            "Ref.",              1,   1,    1,   "Unadjusted",
  "    No",     "19 262", "11 014.9", "1.75 (1.72-1.77)",  "Ref.",             "Ref.",             1,   1,    1,       "Adjusted", 
  "    Yes",    "316",     "150.7",    "2.10 (1.87-2.34)", "1.20 (1.07-1.34)","1.01 (0.91-1.13)",   1.2, 1.07, 1.34,   "Unadjusted",    
  "    Yes",    "316",    "150.7",    "2.10 (1.87-2.34)",  "1.20 (1.07-1.34)", "1.01 (0.91-1.13)",   1.01, 0.91, 1.13, "Adjusted",    
  "  NoOAC",     NA,      NA,         NA,                  NA,                 NA,               1,   1,    1,   "Unadjusted", 
  "  NoOAC",     NA,      NA,         NA,                  NA,                 NA,               1,   1,    1,   "Adjusted", 
  "    No",     "6860",  "3080.2", "2.23 (2.18-2.28)", "Ref.",            "Ref.",              1,   1,    1,   "Unadjusted",
  "    No",     "6860",  "3080.2", "2.23 (2.18-2.28)",  "Ref.",             "Ref.",             1,   1,    1,       "Adjusted", 
  "    Yes",    "49",     "13.6",    "3.59 (2.66-4.75)", "1.61 (1.22-2.14)","0.98 (0.74-1.29)",   1.61, 1.22, 2.14,   "Unadjusted",    
  "    Yes",    "49",     "13.6",    "3.59 (2.66-4.75)",  "1.61 (1.22-2.14)","0.98 (0.74-1.29)",   0.98, 0.74, 1.29, "Adjusted",    
  
  "DVT",        NA,       NA,         NA,                 NA,                NA,                  1,   1,    1,     "Unadjusted", 
  "DVT",        NA,      NA,         NA,                  NA,                 NA,                 1,   1,    1,         "Adjusted",
  "  OAC1",     NA,      NA,         NA,                  NA,                 NA,               1,   1,    1,   "Unadjusted", 
  "  OAC1",     NA,      NA,         NA,                  NA,                 NA,               1,   1,    1,   "Adjusted", 
  "    No",   "19 040",   "10 915.9", "1.74 (1.72-1.77)", "Ref.",            "Ref.",              1,   1,    1,   "Unadjusted",
  "    No",   "19 040",  "10 915.9", "1.74 (1.72-1.77)",  "Ref.",             "Ref.",             1,   1,    1,    "Adjusted",
  "    Yes",   "538",     "249.7",    "2.16 (1.98-2.35)", "1.24 (1.13-1.35)","1.09 (1.00-1.18)",    1.24, 1.13, 1.35,  "Unadjusted",
  "    Yes",   "538",    "249.7",    "2.16 (1.98-2.35)",  "1.24 (1.13-1.35)", "1.09 (1.00-1.18)",    1.09, 1.00, 1.18, "Adjusted",
  "  NoOAC1",     NA,      NA,         NA,                  NA,                 NA,               1,   1,    1,   "Unadjusted", 
  "  NoOAC1",     NA,      NA,         NA,                  NA,                 NA,               1,   1,    1,   "Adjusted", 
  "    No",   "6797",   "3058.6", "2.22 (2.17-2.28)", "Ref.",            "Ref.",              1,   1,    1,   "Unadjusted",
  "    No",   "6797",  "3058.6", "2.22 (2.17-2.28)",  "Ref.",             "Ref.",             1,   1,    1,    "Adjusted",
  "    Yes",   "112",     "35.2",    "3.18 (2.62-3.83)", "1.43 (1.19-1.73)","1.00 (0.83-1.20)",    1.43, 1.19, 1.73,  "Unadjusted",
  "    Yes",   "112",    "35.2",    "3.18 (2.62-3.83)",  "1.43 (1.19-1.73)","1.00 (0.83-1.20)",    1.00, 0.83, 1.20, "Adjusted",
  
  "Any VTE",        NA,       NA,         NA,                 NA,                NA,                  1,   1,    1,     "Unadjusted", 
  "Any VTE",        NA,      NA,         NA,                  NA,                 NA,                 1,   1,    1,         "Adjusted",
  "  OAC2",     NA,      NA,         NA,                  NA,                 NA,               1,   1,    1,   "Unadjusted", 
  "  OAC2",     NA,      NA,         NA,                  NA,                 NA,               1,   1,    1,   "Adjusted", 
  "    No",    "18 781",  "10 791.1", "1.74 (1.72-1.77)", "Ref.",            "Ref.",              1,   1,    1,     "Unadjusted",
  "    No",    "18 781", "10 791.1", "1.74 (1.72-1.77)",  "Ref.",             "Ref.",             1,   1,    1,       "Adjusted",
  "    Yes",  "797",      "374.5",    "2.13 (1.98-2.28)", "1.22 (1.14-1.31)","1.05 (0.98-1.13)",   1.22, 1.14, 1.31,  "Unadjusted",
  "    Yes",  "797",     "374.5",    "2.13 (1.98-2.28)",  "1.22 (1.14-1.31)", "1.05 (0.98-1.13)",    1.05, 0.98, 1.13, "Adjusted",
  "  NoOAC2",     NA,      NA,         NA,                  NA,                 NA,               1,   1,    1,   "Unadjusted", 
  "  NoOAC2",     NA,      NA,         NA,                  NA,                 NA,               1,   1,    1,   "Adjusted", 
  "    No",   "6752",   "3046.5", "2.22 (2.16-2.27)", "Ref.",            "Ref.",              1,   1,    1,   "Unadjusted",
  "    No",   "6752",   "3046.5", "2.22 (2.16-2.27)", "Ref.",            "Ref.",           1,   1,    1,    "Adjusted",
  "    Yes",   "157",    "47.3",    "3.32 (2.82-3.88)",  "1.50 (1.28-1.75)", "1.00 (0.86-1.18)",    1.50, 1.28, 1.75,  "Unadjusted",
  "    Yes",   "157",    "47.3",    "3.32 (2.82-3.88)",  "1.50 (1.28-1.75)", "1.00 (0.86-1.18)",    1.00, 0.86, 1.18, "Adjusted"

)




 

# Possible X ticks:

#> log(0.5)
#[1] -0.6931472
# > log(0.75)
# [1] -0.2876821
# > log(0.125)
# [1] -2.079442
# > log(1)
# [1] 0
# > log(1.50)
# [1] 0.4054651
# > log(2)
# [1] 0.6931472
# > log(2.5)
# [1] 0.9162907
# > log(0.6)
# [1] -0.5108256
# > log(0.7)
# [1] -0.3566749
# > log(2.2)
# [1] 0.7884574

# > log(0.8)
# [1] -0.2231436
# > log(1.4)
# [1] 0.3364722

# > log(0.9)
# [1] -0.1053605

pdf(file='article2/Figures/forest_plot_yhdistetty.pdf',  height=10, width=12) # Open PDF device with specific file name


data |>
  group_by(group) |>
  forestplot(labeltext = c(labeltext, events, patient_years, incidence, unadjusted_irr, adjusted_irr),
             graph.pos=5,
             xticks = c(-0.6931472, 0, 0.9162907),
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

    fp_add_header(labeltext = c("","Subgroup"),
                  events = c("","Events"),
                  patient_years = c("Patient years","(1000 years)"),
                  incidence = c("Incidence per 1000","patient-years"),
                  unadjusted_irr = c("", "Unadjusted IRR"),
                  adjusted_irr = c("", "Adjusted IRR")) |>

fp_add_lines(h_3 = gpar(lty = 1, col = "black")) 



dev.off()

 


 

 