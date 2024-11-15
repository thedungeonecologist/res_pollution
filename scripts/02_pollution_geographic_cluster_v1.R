#' ---
#' title: "Geographic Clustering by Pollution"
#' author: "Darren Shoemaker"
#' ---

# Install Remote Packages 

# remotes::install_git("https://codeberg.org/mpadge/spatialcluster")
# remotes::install_git("https://git.sr.ht/~mpadge/spatialcluster")
# remotes::install_bitbucket("mpadge/spatialcluster")
# remotes::install_gitlab("mpadge/spatialcluster")
# remotes::install_github("mpadge/spatialcluster")

library (spatialcluster)

# Libraries 

library(maps)
library(sf)
library(cluster)
library(Rtsne)
library(mapproj)
library(ggspatial)
library(spatstat)
library(ggConvexHull)
library(here)
library(tidyverse)

# Read Data 

dat <- read.csv(here('data/raw', 'res_pollution_v4.csv'))

all_dat <- read.csv(here('data/raw', 'merge_krogman_rfhp_v6.csv'))

res <- dat %>% 
  select(c(1:4, 103:122)) %>% 
  mutate(across(c(5:24), ~factor(., order = T, levels = c('0', '1', '2', '3', '4', '5'))))

#[1] Gower Distance Cluster by Pollution Impairment

pollution_cluster <- cluster::daisy(res %>% select(-c(NIDID, State)), 
                                    metric = 'gower',
                                    type = list(logratio = 3))

pollution_sil_width <- c(NA)
for(i in 2:25) {
  pam_fit <- pam(pollution_cluster, 
                 diss = T,
                 k = i)
  pollution_sil_width[i] <- pam_fit$silinfo$avg.width
}
graphics::plot(1:25, pollution_sil_width,
               xlab = 'Number of Clusters',
               ylab = 'Silhouette Width',
               main = 'Pollution')
lines(1:25, pollution_sil_width)

pollution_pam_fit <- pam(pollution_cluster, diss = T, k = 12)

pollution_pam_results <- res %>% 
  mutate(cluster = pollution_pam_fit$clustering) %>% 
  group_by(cluster) %>% 
  do(the_summary = base::summary(.))

#pollution_pam_results$the_summary
#res[pam_fit$medoids, ]

pollution_tsne <- Rtsne(pollution_cluster, is_distance = T)

pollution_tsne_dat <- pollution_tsne$Y %>% 
  data.frame() %>% 
  setNames(c('X', 'Y')) %>% 
  mutate(cluster = factor(pollution_pam_fit$clustering),
         NIDID = res$NIDID)

pollution_cluster_dat <- res %>% 
  ungroup() %>% 
  dplyr::mutate(Cluster = pollution_tsne_dat$cluster)

ggplot(data = pollution_tsne_dat, aes(x = X, y = Y)) +
  geom_point(aes(color = cluster)) +
  labs(title = 'Cluster Analysis Pollution')

#climate_cluster_dat$Cluster <- recode(climate_cluster_dat$cluster, 
# '1' = 'name1', ...)

#Note: Rename clusters after identifying geographic patterns

# Example Clusters by Medoid ----

pollution_medoid <- pollution_cluster_dat %>% 
  mutate(Name = all_dat$Dam_Name) %>% 
  filter(row.names(res) %in% pollution_pam_fit$medoids) %>% 
  select(c(NIDID, Name, Cluster, Latitude, Longitude))

pollution_medoid

# Pollution Cluster Plot ----

state <- map_data('state')
sites <- st_as_sf(res, coords = c("Longitude", "Latitude"),
                  crs = 3174, agr = 'constant')

pollution_cluster_plot <- ggplot() +
  geom_polygon(
    data = state,
    aes(x = long, y = lat, group = group),
    fill = 'white',
    color = 'black'
  ) +
  geom_point(data = pollution_cluster_dat,
             aes(x = Longitude, y = Latitude, color = Cluster),
             size = 1) +
  #stat_ellipse(data = climate_cluster_dat, aes(x = Longitude, y = Latitude, group =     Cluster), type = 'norm') +
  coord_map('albers', lat0 = 39, lat1 = 45) +
  xlab('Longitude') +
  ylab('Latitude') +
  theme_bw() +
  theme(legend.title = element_blank(),
        text = element_text(family = 'Times New Roman')) +
  labs(title = 'Pollution Cluster Analysis') +
  geom_point(data = pollution_medoid, 
             aes(x = Longitude, y = Latitude),
             size = 2, shape = 17) +
  geom_convexhull(data = pollution_cluster_dat, alpha = 0.2, aes(x = Longitude, 
                                                               y = Latitude,
                                                               fill = Cluster),
                  color = 'black')
pollution_cluster_plot

#[2] Spatially Constrained Cluster Analysis ----

#' IMPORTANT!!! This algorithm CANNOT FAIL to identify spatial clusters, even if they are meaningless. I do not recommend this in this or any publication without extreme due diligence. Alternatively, just don't do it. 

#' This code is commented out to avoid running it accidentally. This is time/computationally intensive and does not produce meaningful results.

# xy <- as.matrix(res %>% 
#   select(c(Latitude, Longitude)))
# 
# spat_res <- as.matrix(dat %>% 
#                         select(c(103:122)))
# 
# spat_cluster <- scl_redcap(xy, spat_res, ncl = 12)