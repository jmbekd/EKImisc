source("./R/functions.R")

# cpt <- load_CPT(name = "./data/FMW-CPT.csv")
# mip <- load_MIP(name = "./data/FMW-MIP.csv")
#
# bm <- load_bm(name = "./data/shapefile/_Selected_Parcels_NAD27.shp", layer = "_Selected_Parcels_NAD27")
# save(cpt, mip, bm, file = "./data/xs_data.RData")

load("./data/xs_data.RData")

map <- plot_bm(bm)
# siteb_limits <- subset_bm(bm)
siteb_limits <- structure(list(long = c(1482523.90414217, 1483501.27249086),
                               lat = c(492527.547193377, 491574.613053401)),
                          .Names = c("long", "lat"),
                          row.names = 1:2,
                          class = "data.frame")
map <- plot_bm(bm, limits = siteb_limits)

locs <- cpt[, .SD[.N], by = Loc_Orig, .SDcols = c("GS.Elev", "Depth.ft", "Easting", "Northing")]
setnames(locs, c("loc_id", "gs_elev", "depth", "x", "y"))


map <- plot_locs(locs, basemap = map)
A <- select_xs(name = "A", basemap = map)
B <- select_xs(name = "B", nodes = 4, basemap = map)

xs_list <- list_xs(list(A, B))

plot_xs(xs = A, basemap = map) # plots a line for a single cross section
map <- plot_xs(xs = xs_list, basemap = map) # plots lines for multiple cross sections

xs_locs <- calc_xs_locs(xs_list, locs, width = 50)

xs_locs_data <- xs_locs[, .(xs_name, seg_distance, location = loc_id, distance, gs_elev, depth,
                            type = "cpt", tos = NA, bos = NA)]

grid_xs(xs_locs_data[xs_name == "A", ]) # sets up the cross section
plot_xs_locs(xs_locs_data[xs_name == "A", ]) # plots the cross section locations

xs_locs_SBT <- merge(xs_locs_data[, .(xs_name, location, gs_elev, distance)],
                     cpt[, .(Loc_Orig, elev, SBT)], by.x = "location", by.y = "Loc_Orig")

plot_xs_SBT(xs_locs_SBT[xs_name == "A"]) # plots the SBT values on the cross section

## New test --

map <- plot_bm(bm, limits = siteb_limits)
map <- plot_locs(locs, basemap = map)
A <- select_xs(name = "A", basemap = map)
B <- select_xs(name = "B", nodes = 3, basemap = map)
C <- select_xs(name = "C", nodes = 5, basemap = map)

xs_list <- list_xs(list(A, B, C))
map <- plot_xs(xs = xs_list, basemap = map)
xs_locs <- calc_xs_locs(xs_list, locs, width = 50)
xs_locs_data <- xs_locs[, .(xs_name, seg_distance, location = loc_id, distance, gs_elev, depth,
                            type = "cpt", tos = NA, bos = NA)]
xs_locs_SBT <- merge(xs_locs_data[, .(xs_name, location, gs_elev, distance)],
                     cpt[, .(Loc_Orig, elev, SBT)], by.x = "location", by.y = "Loc_Orig")

for (i in unique(xs_locs_data[["xs_name"]])) {
  grid_xs(xs_locs_data[xs_name == i, ]) # sets up the cross section
  plot_xs_locs(xs_locs_data[xs_name == i, ]) # plots the cross section locations
  xs_locs_SBT <- merge(xs_locs_data[xs_name == i, .(xs_name, location, gs_elev, distance)],
                       cpt[, .(Loc_Orig, elev, SBT)], by.x = "location", by.y = "Loc_Orig")
  plot_xs_SBT(xs_locs_SBT[xs_name == i, ]) # plots the SBT data
}

for (i in unique(xs_locs_data[["xs_name"]])) {
  info <- xs_info(xs_locs_data[xs_name == i, ], xs_par = xs_params(h_scale = 10))
  pdf(paste0("./figure/xs_", i, ".pdf"),
             height = info[["fig_height"]] + 2,
             width = info[["fig_width"]] + 2)
  grid_xs(xs_locs_data[xs_name == i, ], xs_par = xs_params(h_scale = 10)) # sets up the cross section
  plot_xs_locs(xs_locs_data[xs_name == i, ]) # plots the cross section locations
  xs_locs_SBT <- merge(xs_locs_data[xs_name == i, .(xs_name, location, gs_elev, distance)],
                       cpt[, .(Loc_Orig, elev, SBT)], by.x = "location", by.y = "Loc_Orig")
  plot_xs_SBT(xs_locs_SBT[xs_name == i, ]) # plots the SBT data
  dev.off()
}
