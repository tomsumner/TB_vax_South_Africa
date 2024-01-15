# 1.1  Load in the required packages
suppressPackageStartupMessages({
  rm(list=ls())
  library(here)
  library(data.table) 
  library(ggplot2)
  library(cowplot)
  theme_set(theme_minimal_grid() + panel_border(color = "black"))

  })

params <- fread("./processing_files/param_sets/IND_params.csv")

colnames(params) <- c("nhits", "uid", "chi",	"eta",
                      "j1A0", "j2A0", "j3A0",	"j4A0",
                      "kappa",	"muDc",	"muK",	"multiplier",	"omegaS0",	"pEhigh",	"pR",
                      "pT",	"rho",	"sageA15",	"sigma",	"theta",	"zeta")

params <- params[, !c("nhits")]

params <- melt(params, id.vars = "uid")

iplist <- split(params, by = "variable")
input <- fread("./countries/IND/parameters/input.csv")
prtb <- input[choose == TRUE]

opl <- list()

for (p in 1:length(iplist)) {
  pn <- names(iplist)[[p]]
  
  prior_ranges <- prtb[unique.name == pn, .(min, max)]

  opl[[pn]] <- ggplot(iplist[[pn]]) +
    scale_x_continuous(limits = c(prior_ranges$min, prior_ranges$max), name = pn) +
    scale_y_continuous(name = "") +
    theme(legend.position = "bottom") +
    scale_fill_viridis_d() +
    scale_colour_viridis_d() +
    geom_density(aes(x = value))
  
}


opl <- patchwork::wrap_plots(opl,  ncol = 4, nrow = 5)
opl


pdf(paste0("./INDscen2_1_posteriors.pdf"), width = 11.7, height = 8.3)
print(opl) 
dev.off()






# ----- end