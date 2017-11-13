library(tidyverse)
library(readr)
library(forcats)
library(cluster)
library(dendextend)
library(colorspace)

ELO = read_csv("data/elo-ratings-2017-09-18.csv")
coach_info = read_csv("data/coaches_to2015.csv")


# transform
ELO_wide <- ELO %>%
  filter(Year > 2010) %>%
  select(Coach, Year, Rating, Rank) %>%
  gather(Rating:Rank, key = 'variable', value = 'value') %>%
  unite(col = 'variable', variable, Year) %>%
  spread(variable, value) %>%
  na.omit()

# Attempt cluster analysis
ward_clust <- ELO_wide %>%
  select(starts_with('Rating')) %>%
  daisy(., metric = 'euclidean') %>%
  agnes(., diss = TRUE, method = 'ward')

complete_clust <- ELO_wide %>%
  select(starts_with('Rating')) %>%
  daisy(., metric = 'euclidean') %>%
  agnes(., diss = TRUE, method = 'complete')

# Plot dendrogram
coach_names <- rev(levels(ELO_wide$Coach))
dend <- as.dendrogram(ward_clust)
# order it the closest we can to the order of the observations:
dend <- rotate(dend, 1:71)

# Color the branches based on the clusters:
dend <- color_branches(dend, k=3) #, groupLabels=iris_species)

# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(dend) <-
  rainbow_hcl(3)[sort_levels_values(
    as.numeric(ELO_wide$Coach)[order.dendrogram(dend)]
  )]

# We shall add the flower type to the labels:
labels(dend) <- paste(as.character(ELO_wide$Coach)[order.dendrogram(dend)],
                      "(",labels(dend),")", 
                      sep = "")
# We hang the dendrogram a bit:
dend <- hang.dendrogram(dend, hang_height=0.2)
# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", 0.5)
# And plot:
par(mar = c(3,3,3,7))
plot(dend, 
     main = "Clustered ELO data set
     (the labels give the Coach Name)", 
     horiz =  TRUE,  nodePar = list(cex = .007))
legend("topleft", legend = coach_names, fill = rainbow_hcl(3))

d3heatmap::d3heatmap(as.matrix(select(ELO_wide, starts_with('Rating'))),
                     dendrogram = "row",
                     Rowv = dend,
                     colors = "Greens",
                     # scale = "row",
                     width = 600,
                     show_grid = FALSE)
