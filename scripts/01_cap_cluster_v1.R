#' ---
#' title: "Cluster Cap Analysis"
#' author: "Darren Shoemaker"
#' ---

# Libraries ----

library(ggplot2)
library(BiodiversityR)
library(vegan)
library(vegan3d)
library(ggfortify)
library(ggradar)
library(ggpubr)
library(tidymodels)
library(svglite)

#[1] Pairwise Permanova by Cluster ----

# Morphology ----

#' Morph was transformed and missing data were removed. This uses a different structure than other analyses

# Morphology base permanova

morph_permanova <- adonis2(morph ~ dat3$Poll_clus, method = 'gower')
morph_permanova

# Morphology pairwise permanova

#' Note pairwise permanova function still calls adonis2 but pairwise.adonis2 does not allow p-value adjustment.

morph_pw_permanova <- pairwiseAdonis::pairwise.adonis(morph, dat3$Poll_clus, 
                                                   sim.method = 'gower', 
                                                   p.adjust.m = 'bonferroni')
morph_pw_permanova
morph_matrix <- morph_pw_permanova %>% 
  separate(col = pairs, into = c('clus_1', 'clus_2'), sep = 'vs', remove = T) 
morph_tab <- xtabs(p.value ~ clus_1 + clus_2, data = morph_matrix)

## Morphology significance matrix for clusters

morph_tab

# Land use ----

#' Land use also has missing data. Use dat2. 

# Base permanova

land_permanova <- adonis2(land ~ Poll_clus, data = dat2, method = 'gower')
land_permanova

# Pairwise permanova

land_pw_permanova <- pairwiseAdonis::pairwise.adonis(land, dat2$Poll_clus, 
                                                      sim.method = 'gower', 
                                                      p.adjust.m = 'bonferroni')
land_pw_permanova
land_matrix <- land_pw_permanova %>% 
  separate(col = pairs, into = c('clus_1', 'clus_2'), sep = 'vs', remove = T) 
land_tab <- xtabs(p.value ~ clus_1 + clus_2, data = land_matrix)

## Land significance matrix for clusters

land_tab

# Climate ----

clim_permanova <- adonis2(clim ~ Poll_clus, data = dat, method = 'gower')
clim_permanova

# Pairwise permanova

clim_pw_permanova <- pairwiseAdonis::pairwise.adonis(clim, dat$Poll_clus, 
                                                     sim.method = 'gower', 
                                                     p.adjust.m = 'bonferroni')
clim_pw_permanova
clim_matrix <- clim_pw_permanova %>% 
  separate(col = pairs, into = c('clus_1', 'clus_2'), sep = 'vs', remove = T) 
clim_tab <- xtabs(p.value ~ clus_1 + clus_2, data = clim_matrix)

## Climate significant matrix for clusters

clim_tab

# Overall

all_permanova <- adonis2(dat_all[3:24] ~ Poll_clus, data = dat_all, method = 'gower')
all_permanova

#[2] Cap Analysis by Cluster ----

#Impairment ----

imp_cap <- CAPdiscrim(impair ~ Poll_clus, data = dat, dist = 'gower', axes = 2, m = 0, add = F)
imp_cap <- add.spec.scores(imp_cap, comm = impair, method = 'cor.scores')
imp_plot <- ordiplot(imp_cap, choices = c(1 , 2), 
                     main = 'CAP Ordination Impairment by Lake Cluster')  
ordisymbol(imp_plot, y = dat, factor = 'Poll_clus', legend = T)

#' Some clusters are recognizable. Expected after cluster analysis.

# Morphology ----

# Use dat3 

morph_cap <- CAPdiscrim(morph ~ Poll_clus,
                        data = dat3,
                        dist = 'gower', axes = 2, m = 0, add = F)
morph_cap <- add.spec.scores(morph_cap, comm = morph, method = 'cor.scores')
morph_plot <- ordiplot(morph_cap, choices = c(1, 2), 
                       main = 'CAP Ordination Morphology by Lake Cluster')  
ordisymbol(morph_plot, y = dat3, factor = 'Poll_clus', legend = T)

#' Very few differences by morphology

# Land Use ----

#' Note land use missing values. Use dat2

land_cap <- CAPdiscrim(land ~ Poll_clus, data = dat2, dist = 'gower', axes = 2, m = 0, add = F)
land_cap <- add.spec.scores(land_cap, comm = land, method = 'cor.scores')
land_plot <- ordiplot(land_cap, choices = c(1 ,2), 
                     main = 'CAP Ordination Land Use by Lake Cluster')  
ordisymbol(land_plot, y = dat2, factor = 'Poll_clus', legend = T)

#' No visible clusters by land use

# Climate ----

clim_cap <- CAPdiscrim(clim ~ Poll_clus, data = dat, dist = 'gower', axes = 2, m = 0, add = F)
clim_cap <- add.spec.scores(clim_cap, comm = clim, method = 'cor.scores')
clim_plot <- ordiplot(clim_cap, choices = c(1 ,2), 
                      main = 'CAP Ordination Climate by Lake Cluster')  
ordisymbol(clim_plot, y = dat, factor = 'Poll_clus', legend = T)

#' Very few differences by climate

#[3] Radar plots by category ----

clus_table_data <- clus_table %>% 
  group_by(X) %>% 
  reframe(across(4:13))

clus_names <- clus_table[,14]

imp_plots <- list()

for(i in 1:8) {
imp_plots[[i]] <- ggradar(clus_table_data %>% 
          slice(i), 
        grid.label.size = 3,
        axis.label.size = 2.5,
        group.point.size = 3,
        fill = T,
        group.colours = 'skyblue',
        grid.min = 0,
        grid.mid = 0.5,
        grid.max = 1,
        values.radar = c('0', '0.5', '1'),
        fill.alpha = 0.5) +
  labs(title = clus_names[i]) +
  theme(
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.title.position = "plot",
    legend.position.inside = c(1, 0),
    legend.justification = c(1, 0.1),
    legend.text = element_text(size = 12),
    legend.key = element_rect(fill = NA, color = NA),
    legend.background = element_blank()
  )
}
imp_plots[[1]]

arranged_plots <- ggarrange(plotlist = imp_plots, nrow = 4, ncol = 2,
                            align = 'hv')

# ggsave(filename =  here('output'), 'lake_radar_plots.svg'), 
#        arranged_plots,
#        width = 6.5, height = 12, dpi = 1200)

print(imp_plots)

#[4] Statistical Analyses to Estimate Best Predictors ----

morph_mod <- multinom_reg(penalty = 1) %>% 
  set_engine('nnet') %>% 
  fit_xy(dat3$Poll_clus, x = morph)
morph_mod

#' Morphology model AIC = 3417.673

land_mod <- multinom_reg(penalty = 1) %>% 
  set_engine('nnet') %>% 
  fit_xy(x = land,
         y = dat2$Poll_clus) 
land_mod

#' Land use model AIC = 2447.663

clim_mod <- multinom_reg(penalty = 1) %>% 
  set_engine('nnet') %>% 
  fit_xy(x = clim,
         y = dat$Poll_clus) 
clim_mod

#' Climate model AIC = 4159.153

#rmarkdown::render(paste0(wd$scripts, '03_cap_cluster_v1.R'), output_dir = wd$output)