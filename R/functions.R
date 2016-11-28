load_CPT <- function(name = NULL) {
  if(!require("data.table")) install.packages("data.table", repos = "http://cran.r-project.org")
  library(data.table)

  if (is.null(name)) {
    file <- file.choose()
    dir <- dirname(file)
  } else {
    file <- name
    dir <- dirname(file)
  }


  cpt <- as.data.table(read.csv(file, stringsAsFactors = FALSE))
  cpt <- cpt[order(Loc, Depth.ft)]
  cpt[, ID := seq(1, nrow(cpt))]

  t_elev <- copy(cpt)
  b_elev <- copy(cpt)
  t_elev <- t_elev[, elev := GS.Elev - c(0, Depth.ft[1:length(Depth.ft) - 1]), by = Loc]
  b_elev <- b_elev[, elev := GS.Elev - Depth.ft]

  segments <- rbindlist(list(t_elev, b_elev))
  segments <- segments[order(Loc, ID, -elev)]
  setkey(segments, "Loc")
  return(segments)
}

load_MIP <- function(name = NULL) {
  if(!require("data.table")) install.packages("data.table", repos = "http://cran.r-project.org")
  library(data.table)

  if (is.null(name)) {
    file <- file.choose()
    dir <- dirname(file)
  } else {
    file <- name
    dir <- dirname(file)
  }

  mip <- as.data.table(read.csv(file, stringsAsFactors = FALSE))
  mip <- mip[order(Loc, Depth_ft)]
  mip[, ID := seq(1, nrow(mip))]

  t_elev <- copy(mip)
  b_elev <- copy(mip)
  t_elev <- t_elev[, elev := GS.Elev - c(0, Depth_ft[1:length(Depth_ft) - 1]), by = Loc]
  b_elev <- b_elev[, elev := GS.Elev - Depth_ft]

  segments <- rbindlist(list(t_elev, b_elev))
  segments <- segments[order(Loc, ID, -elev)]
  setkey(segments, "Loc")
  return(segments)
}

## code below from: http://stackoverflow.com/questions/8175912/load-multiple-packages-at-once

ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  lapply(pkg, library, character.only = TRUE)
  invisible()
}

load_bm <- function(name = NULL, layer = NULL) {
  pkgs <- c("sp", "rgdal", "data.table", "ggplot2")
  ipak(pkgs)

  if (is.null(name)) {
    file <- file.choose()
    dir <- dirname(file)
  } else {
    file <- name
    dir <- dirname(file)
  }

  if (is.null(layer)) {
    layers <- ogrListLayers(file)
    print(layers[1])
    layer_no <- readline("Please enter the number of the desired layer.\n")
    if (layer_no == "") {break}
    layer <- layers[as.numeric(layer_no)]
  }

  bm <- readOGR(name, layer)
  bm <- as.data.table(fortify(bm))

  return(bm)
}

plot_bm <- function(bm = bm,
                    limits = NULL,
                    col = "black",
                    labels = list(title = "Basemap", x = "Easting", y = "Northing")) {
  pkgs <- c("ggplot2", "ggmap", "plyr")
  ipak(pkgs)

  map <- ggplot(bm, aes(long, lat, group = group)) +
    geom_polygon(fill = NA, color = col) +
    theme_bw() +
    labs(labels) +
    if (is.null(limits)) {
      coord_fixed()
    } else {
      coord_fixed(xlim = range(limits$long),
                  ylim = range(limits$lat))
    }
  plot(map)
  return(map)
}

subset_bm <- function(basemap = NULL, ...) {
  if (is.null(basemap)) stop("please supply the desired basemap.")

  plot(basemap)
  cat("Select two diagonal points along the bounding box of the desired area.\n")
  b_limits <- gglocator(n = 2)
  map <- basemap +
    coord_fixed(xlim = range(b_limits$long),
                ylim = range(b_limits$lat)) +
    ggtitle("Subsetted Basemap")
  plot(map)
  list("basemap" = map, "limits" = b_limits)
}

plot_locs <- function(locs,
                      col_pt = "red",
                      label_pts = TRUE,
                      basemap = NULL,
                      ...) {
  pkgs <- c("ggplot2", "ggrepel")
  ipak(pkgs)

  if (!is.null(basemap)) {
    map <- basemap +
      geom_point(data = locs,
                 aes(x = x, y = y, group = NULL),
                 color = col_pt)
  } else {
    map <- ggplot(locs, aes(x, y)) +
      labs(list(title = "Locations", x = "Easting", y = "Northing")) +
      geom_point(color = col_pt) +
      theme_bw() + coord_fixed()
  }
  if (label_pts) {
    map <- map +
      geom_text_repel(data = locs,
                      aes(x = x, y = y, label = loc_id, group = NULL),
                      color = "black",
                      size = 3,
                      max.iter = 50)
  }
  plot(map)
  return(map)
}


select_xs <- function(name, basemap, nodes = 2, col = "blue", lwd = 1.15) {
  plot(basemap)
  cat("click on the desired points along the cross section. click escape to exit.\n")
  xs <- data.table::as.data.table(gglocator(n = nodes, xexpand = c(0, 0), yexpand = c(0, 0)))
  xs[, xs_name := name]
  xs[, segment := 1:nrow(xs) - 1]

  map <- basemap +
    geom_path(data = xs,
              aes(x = long, y = lat, group = NULL),
              color = col,
              size = lwd) +
    geom_text(data = xs[, .SD[c(1, .N)]],
              aes(x = long, y = lat, group = NULL,
                  label = paste0(c("", "'"), xs_name)),
              color = col)
  plot(map)
  return(xs)
}

list_xs <- function(list_of_xs) {
  xs <- data.table::rbindlist(list_of_xs)
  return(xs)
}

plot_xs <- function(xs, basemap, col = "blue", lwd = 1.15) {
  map <- basemap +
    geom_path(data = xs,
              aes(x = long, y = lat, group = xs_name),
              color = col,
              size = lwd) +
    geom_text(data = xs[, .SD[c(1, .N)], by = xs_name],
              aes(x = long, y = lat, group = xs_name,
                  label = paste0(c("", "'"), xs_name)),
              color = col)
  plot(map)
  return(map)
}

calc_theta <- function(x, y) {
  theta <- atan(diff(y) / diff(x))
  ifelse(diff(x) < 0, pi + theta, theta)
}

width_xs <- function(xs, width) {
  xs_width <- data.table(xs_name = unique(xs[["xs_name"]]),
                         xs_width = width)
  merge(xs, xs_width, by = "xs_name")
}

rotate <- function(x, y, xo = 0, yo = 0, theta, units = "rad") {
  coords <- matrix(rbind(x, y), ncol = length(x))
  origin <- if (length(xo) == 1) {
    c(xo, yo)
  } else {
    matrix(rbind(xo, yo), ncol = length(xo))
  }
  theta <- -theta
  if (units == "deg") theta <- theta * pi / 180
  R <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)),
              nrow = 2, ncol = 2, byrow = TRUE)
  r_data <- t(R %*% (coords - origin))
  r_data <- as.data.frame(r_data)
  colnames(r_data) <- c("r_x", "r_y")
  r_data
}

calc_seg_distance <- function(x, y) {
  Mod(diff(x + 1i * y))
}

calc_xs_locs <- function(xs, locs, width) {
  ## in the future, would like to add the ability to have different widths for different cross sections
  ## Also, need to check for and remove any locations that might appear twice on a cross section. The
  ## location should only show up on the cross section that it is closest to...

  xs_segments <- xs[, .(segment = segment[-.N],
                        long = long[-.N],
                        lat = lat[-.N],
                        theta = calc_theta(long, lat),
                        seg_distance = calc_seg_distance(long, lat)),
                    by = xs_name]
  xs_segments[, distance := cumsum(seg_distance) - seg_distance, by = xs_name]
  r_coords <- rbindlist(lapply(1:nrow(xs_segments),
                               function(i) {
                                 locs[, rotate(x = x,
                                               y = y,
                                               xo = xs_segments$long[i],
                                               yo = xs_segments$lat[i],
                                               theta = xs_segments$theta[i])]
                               }))
  expand_xs_segments <- xs_segments[rep(seq_len(nrow(xs_segments)), each = nrow(locs))] # replicates each row in xs_segments, nlocations number of times
  expand_locs <- locs[rep(seq_len(nrow(locs)), times = nrow(xs_segments))]
  r_locs <- cbind(expand_xs_segments, expand_locs, r_coords)
  setorder(r_locs, xs_name, segment, r_x)
  r_locs <- r_locs[r_x >= 0 &
                     r_x < seg_distance &
                     abs(r_y) <= width, ]
  r_locs[, distance := distance + r_x]
  r_locs
}

#### xs_info ----
xs_info <- function(data = NULL,
                   seg_distance = NULL, distance = NULL, depth = NULL, gs_elev = NULL,
                   name = NULL, xs_par = xs_params()) {

  if (!is.null(data)) {
    names <- names(data)
    if (any(!(c("distance", "seg_distance", "depth", "gs_elev", "xs_name") %in% names))) {
      stop ("please provide a data.frame with columns identified as 'xs_name', 'seg_distance',
            'distance', 'depth', and 'gs_elev' or specify the 'name', 'seg_distance', distance',
            'depth',and 'gs_elev' parameters directly.")
    }
    }

  if (is.null(data) & any(is.null(name) & is.null(distance) & is.null(depth) & is.null(gs_elev))) {
    stop ("please provide a data.frame with columns identified as 'xs_name', 'seg_distance',
          'distance', 'depth', and 'gs_elev' or specify the 'name', 'seg_distance', distance',
          'depth',and 'gs_elev' parameters directly.")
  }

  if (length(distance) > 1) {
    distance <- max(distance)
  }
  if (length(depth) > 1) {
    depth <- max(depth)
  }
  if (length(gs_elev) > 1) {
    gs_elev <- max(gs_elev)
  }

  ## if seg_distance, distance, depth, and gs_elev parameters not specified, determine from data
  if (is.null(distance)) distance <- max(data[["distance"]])
  if (is.null(depth)) depth <- max(data[["depth"]])
  if (is.null(gs_elev)) gs_elev <- max(data[["gs_elev"]])
  if (is.null(name)) name <- unique(data[["xs_name"]])
  if (is.null(seg_distance)) seg_distance <- cumsum(unique(data[["seg_distance"]]))

  ## round the distance, depth, and gs_elev parameters
  distance <- plyr::round_any(distance, xs_par[["h_major"]], f = ceiling)
  depth <- plyr::round_any(depth, xs_par[["v_major"]], f = ceiling)
  gs_elev <- plyr::round_any(gs_elev, xs_par[["v_minor"]], f = ceiling)

  ## determine the figure height and width and x and y axes limits
  y_lim <- range(pretty(c(gs_elev, gs_elev - depth)))
  x_lim <- range(pretty(c(0, distance)))
  fig_height <- diff(y_lim) / xs_par[["v_scale"]]
  fig_width <- diff(x_lim) / xs_par[["h_scale"]]

  ## return the calculated values
  list("x_lim" = x_lim, "y_lim" = y_lim,
       "fig_height" = fig_height, "fig_width" = fig_width,
       "seg_distance" = seg_distance)
}

#### grid_xs ----

#' Set up the plotting region for the cross section
#'
#' This function sets up the plotting region for the cross section. Inputs are the total
#' length of the cross section, the depth below ground surface of the boreholes and/or wells
#' (or the maximum desired depth below ground surface that the cross section should extend),
#' and the ground surface elevation of the boreholes and/or wells. Using the supplied
#' information, and the cross section parameters set up using the xs_params function, an empty
#' gridded cross section will be plotted.
#'
#' @param data A data.frame or data.table with columns of "distance", "depth", and
#'   "gs_elev". For explanations of these parameters and their valid values, see below. The data
#'   parameter is not necessary if the distance, depth, and gs_elev parameters are individually
#'   specified. Defaults to NULL.
#' @param distance Horizontal distance of the boreholes and/or wells along the cross section or
#'   the maximum distance that the cross section should extend. Can be a single value or a
#'   vector of distances. If a vector of values is supplied, the maximum value is selected.
#'   Defaults to NULL.
#'   Note, the distance parameter is rounded to the nearest h_major unit (see ?xs_params) and
#'   the x-axis limits are determined using the 'pretty' function (see ?pretty) with an
#'   interval of 0 to the rounded distance value.
#' @param depth The total depth of the boreholes and/or wells below ground surface, or the
#'   the maximum distance below ground surface that the cross section should extend to. Can be
#'   a single value or a vector of depths. If a vector of depths is supplied, the maximum value
#'   is selected. Defaults to NULL.
#'   Note, the depth parameter is rounded to the nearest v_major unit (see ?xs_params)
#'   and the y-axis limits are determined using the 'pretty' function (see ?pretty) with
#'   the upper limit being the ground surface elevation (see gs_elev) and the lower limit
#'   being gs_elev - the rounded depth value.
#' @param gs_elev The ground surface elevation at each of the boreholes and/or wells along the
#'   cross section. Can be a single value or a vector of depths. If a vector of ground
#'   surface elevations is supplied, the maximum value is selected. Defaults to NULL.
#'   Note, the gs_elev parameter is rounded to the nearest v_minor unit (see ?xs_params)
#'   and the y-axis limits are determined using the 'pretty' function (see ?pretty) with
#'   the upper limit being the rounded gs_elev value and the lower limit
#'   being the rounded gs_elev - and the depth (see depth).
#' @param name The cross section ID (e.g., A, B, etc.). Defaults to NULL.
#' @param xs_par A list of cross section parameters from the xs_params function (see
#'   ?xs_params). Defaults to xs_params().
#'
#' @return A plot of an empty gridded cross section.
#'
#' @keywords xs, cross section, plot, gs_elev, depth, distance, xs_params, grid
#'
#' @export
#'
#' @examples
#' grid_xs(distance = c(100, 200, 600), depth = c(30, 15, 55), gs_elev = c(10, 12, 11),
#' name = "A", xs_params = xs_params())

grid_xs <- function(data = NULL,
                    seg_distance = NULL, distance = NULL, depth = NULL, gs_elev = NULL,
                    name = NULL, xs_par = xs_params()) {

  if (!is.null(data)) {
    names <- names(data)
    if (any(!(c("distance", "seg_distance", "depth", "gs_elev", "xs_name") %in% names))) {
      stop ("please provide a data.frame with columns identified as 'xs_name', 'seg_distance',
            'distance', 'depth', and 'gs_elev' or specify the 'name', 'seg_distance', distance',
            'depth',and 'gs_elev' parameters directly.")
    }
  }

  if (is.null(data) & any(is.null(name) & is.null(distance) & is.null(depth) & is.null(gs_elev))) {
    stop ("please provide a data.frame with columns identified as 'xs_name', 'seg_distance',
          'distance', 'depth', and 'gs_elev' or specify the 'name', 'seg_distance', distance',
          'depth',and 'gs_elev' parameters directly.")
  }

  if (length(distance) > 1) {
    distance <- max(distance)
  }
  if (length(depth) > 1) {
    depth <- max(depth)
  }
  if (length(gs_elev) > 1) {
    gs_elev <- max(gs_elev)
  }

  ## if seg_distance, distance, depth, and gs_elev parameters not specified, determine from data
  if (is.null(distance)) distance <- max(data[["distance"]])
  if (is.null(depth)) depth <- max(data[["depth"]])
  if (is.null(gs_elev)) gs_elev <- max(data[["gs_elev"]])
  if (is.null(name)) name <- unique(data[["xs_name"]])
  if (is.null(seg_distance)) seg_distance <- cumsum(unique(data[["seg_distance"]]))

  ## round the distance, depth, and gs_elev parameters
  distance <- plyr::round_any(distance, xs_par[["h_major"]], f = ceiling)
  depth <- plyr::round_any(depth, xs_par[["v_major"]], f = ceiling)
  gs_elev <- plyr::round_any(gs_elev, xs_par[["v_minor"]], f = ceiling)

  ## determine the figure height and width and x and y axes limits
  y_lim <- range(pretty(c(gs_elev, gs_elev - depth)))
  x_lim <- range(pretty(c(0, distance)))
  fig_height <- diff(y_lim) / xs_par[["v_scale"]]
  fig_width <- diff(x_lim) / xs_par[["h_scale"]]

  par(pin = c(fig_width, fig_height), yaxs = "i", xaxs = "i",
      mar = c(5, 5, 5, 5)) # a value of 5 lines was chosen as 5 lines = 1 inch

  ## set up plotting region
  plot(0, 0, type = "n",
       xlim = x_lim, ylim = y_lim, axes = FALSE,
       xlab = paste0("Distance (" , xs_par[["units"]], ")"),
       ylab = paste0("Elevation (" , xs_par[["units"]], ")"),
       main = paste0("Cross Section ", name, "-", name, "'"))

  ## add major and minor tick marks to x and y axes
  axis(1, at = seq(from = x_lim[1], to = x_lim[2],
                   by = xs_par[["h_minor"]]), label = FALSE, tck = -0.01)
  axis(1, at = seq(from = x_lim[1], to = x_lim[2],
                   by = xs_par[["h_major"]]))
  axis(2, at = seq(from = y_lim[1], to = y_lim[2],
                   by = xs_par[["v_minor"]]), label = FALSE, tck = -0.01)
  axis(2, at = seq(from = y_lim[1], to = y_lim[2],
                   by = xs_par[["v_major"]]), las = 2)
  axis(4, at = seq(from = y_lim[1], to = y_lim[2],
                   by = xs_par[["v_minor"]]), label = FALSE, tck = -0.01)
  axis(4, at = seq(from = y_lim[1], to = y_lim[2],
                   by = xs_par[["v_major"]]), las = 2)
  mtext(text = paste0("Elevation (" , xs_par[["units"]], ")"), side = 4,
        line = 3)

  ## add major and minor gridlines to x and y axes
  if (xs_par[["plot_h_minor"]]) {
    abline(v = seq(from = x_lim[1], to = x_lim[2], by = xs_par[["h_minor"]]),
           lty = 3, lwd = 0.3)
  }
  if (xs_par[["plot_h_major"]]) {
    abline(v = seq(from = x_lim[1], to = x_lim[2], by = xs_par[["h_major"]]),
           lty = 3, lwd = 0.5)
  }
  if (xs_par[["plot_v_minor"]]) {
    abline(h = seq(from = y_lim[1], to = y_lim[2], by = xs_par[["v_minor"]]),
           lty = 3, lwd = 0.3)
  }
  if (xs_par[["plot_v_major"]]) {
    abline(h = seq(from = y_lim[1], to = y_lim[2], by = xs_par[["v_major"]]),
           lty = 2, lwd = 0.5)
  }

  ## Add xs_node locations
  abline(v = seg_distance[-length(seg_distance)], lty = 2, lwd = 2)
}

#### xs_params ----

#' Specify the cross section parameters
#'
#' This function allows you to specify desired cross section parameters such as
#' horizontal and vertical scales and gridlines.
#'
#' @param v_scale Vertical scale of the y-axis. Defaults to 10 (i.e.,
#'   in the resulting plot, 10 feet on the y-axis will correspond to 1 inch printed).
#' @param h_scale Horizontal scale of the x-axis. Defaults to 100 (i.e.,
#'   in the resulting plot, 100 feet on the x-axis will correspond to 1 inch printed).
#' @param v_minor Interval for plotting minor horizontal grid lines. Defaults to 1 (i.e.,
#'   a minor gridline every foot).
#' @param v_major Interval for plotting major horizontal grid lines. Defaults to 10 (i.e.,
#'   a major gridline every 10 feet).
#' @param h_minor Interval for plotting minor vertical grid lines. Defaults to 20 (i.e.,
#'   a minor gridline every 20 feet).
#' @param h_major Interval for plotting major vertical grid lines. Defaults to 100 (i.e.,
#'   a major gridline every 100 feet).
#' @param h_major Interval for plotting major vertical grid lines. Defaults to 100 (i.e.,
#'   a major gridline every 100 feet).
#' @param units Unit of measurement for the x and y axes. Defaults to feet ("ft").
#' @param plot_v_minor Plot minor horizontal grid lines? Defaults to TRUE.
#' @param plot_v_major Plot major horizontal grid lines? Defaults to TRUE.
#' @param plot_h_minor Plot minor vertical grid lines? Defaults to FALSE.
#' @param plot_h_major Plot major vertical grid lines? Defaults to TRUE.
#'
#' @return List of parameters that will be used to set desired cross section parameters.
#'
#' @keywords xs, params, cross section
#'
#' @export
#'
#' @examples
#' xs_params()

xs_params <- function(v_scale = 10,
                      h_scale = 100,
                      v_minor = 1,
                      v_major = 10,
                      h_minor = 20,
                      h_major = 100,
                      units = "ft",
                      plot_v_minor = TRUE,
                      plot_v_major = TRUE,
                      plot_h_minor = FALSE,
                      plot_h_major = TRUE) {
  ## Sets up basic cross section parameters
  xs_params <- list("v_scale" = v_scale, "h_scale" = h_scale, "v_minor" = v_minor,
                    "v_major" = v_major, "h_minor" = h_minor, "h_major" = h_major,
                    "units" = units, "plot_v_minor" = plot_v_minor, "plot_v_major" = plot_v_major,
                    "plot_h_minor" = plot_h_minor, "plot_h_major" = plot_h_major)
}

##### plot_xs_locs ----

#' Plot wells/boreholes on the cross section
#'
#' This function plots wells and/or boreholes on the cross section. The function will plot a
#' rectangular screened interval for wells. The bottom of a well is denoted with a dash and
#' the bottom of a borehole is denoted with a point.
#'
#' @param dt A data.frame or data.table with columns of "location", "type", "distance",
#'   "gs_elev", "depth", "tos", and "bos". For explanations of these parameters and their
#'   valid values, see the definitions below. The data parameter is not necessary if the
#'   location, type, distance, gs_elev, depth, tos, and bos parameters are individually
#'   specified. Defaults to NULL.
#' @param location A unique identifier for each borehole and/or well (e.g., "MW-3", "BH-45",
#'   etc.). Should be a vector of character strings. Defaults to NULL.
#' @param type Is the specified location a well, borehole, etc.? Should be a vector of character
#'   strings such as "well", "borehole", "ssp", "svp", etc. Defaults to NULL.
#' @param distance Horizontal distance of each of the boreholes and/or wells along the cross
#'   section. Should be a vector of depths. Defaults to NULL.
#' @param gs_elev The ground surface elevation at each of the boreholes and/or wells along the
#'   cross section. Should be a vector of depths. Defaults to NULL.
#' @param depth The total depth (below ground surface) at each of the boreholes and/or wells
#'   along the cross section. Should be a vector of depths. Defaults to NULL.
#' @param tos The distance from the ground surface to the top of the screened interval for
#'   each well along the cross section. Should be a vector of depths. Defaults to NULL.
#' @param bos The distance from the ground surface to the tbottom of the screened interval for
#'   each well along the cross section. Should be a vector of depths. Defaults to NULL.
#' @param plot_gs. Should a line be plotted between the ground surface elevations (gs_elev)
#'   of the boreholes and wells? Defaults to TRUE.
#' @param w_width Width of the well screen in inches. Defaults to 0.1.
#' @param label_shift The vertical distance by which to shift the borehole and/or well
#'   labels. Defaults to 2.
#' @param xs_params A list of cross section parameters from the xs_params function (see
#'   ?xs_params). Defaults to xs_params().
#'
#' @return Plots the wells and boreholes on the cross section set up using grid_xs().
#'
#' @keywords xs, plot, cross section
#'
#' @export
#'
#' @examples
#' grid_xs(distance = c(100, 200, 550),
#'         depth = c(30, 15, 55),
#'         gs_elev = c(10, 12, 11),
#'         name = "A", xs_par = xs_params())
#' plot_xs_locs(location = c("MW-1", "BH-5", "CPT-3"),
#'              type = c("well", "borehole", "cpt"),
#'              distance = c(100, 200, 550),
#'              gs_elev = c(11, 8, 12),
#'              depth = c(60, 42, 90),
#'              tos = c(45, NA, NA),
#'              bos = c(55, NA, NA))

plot_xs_locs <- function(dt = NULL,
                         location = NULL, type = NULL, distance = NULL, gs_elev = NULL,
                         depth = NULL, tos = NULL, bos = NULL,
                         plot_gs = TRUE, w_width = 0.1, label_shift = 2,
                         xs_par = xs_params()) {

  ## data should be a data.frame or data.table with columns of "location", "type" (e.g., well,),
  ## borehole, etc.), "distance", "gs_elev" (ground surface elevation), "depth", "tos" (top of screen),
  ## and "bos" (bottom of screen). the data parameter is not necessary if the location, distance,
  ## gs_elev, depth, tos, and bos are supplied as separate vectors.
  ## w_width is the default width of the well on the output figure, in inches
  ## label_shift is the vertical distance to shift the well/borehole label above the top of the
  ## ground surface.

  if (!is.null(dt)) {
    names <- names(dt)
    if (any(!(c("location", "type", "distance", "gs_elev",
                "depth", "tos", "bos") %in% names))) {
      stop ("please provide a data.frame with columns identified as 'location', 'type',
            'distance', 'gs_elev', 'depth', 'tos', and 'bos' or specify the vectors of 'location',
            'type', 'distance', 'gs_elev', 'depth', 'tos', and 'bos' directly.")
    }
    location <- dt[["location"]]
    type <- dt[["type"]]
    distance <- dt[["distance"]]
    gs_elev <- dt[["gs_elev"]]
    depth <- dt[["depth"]]
    tos <- dt[["tos"]]
    bos <- dt[["bos"]]
    }

  if (is.null(dt) & any(is.null(location) & is.null(distance) & is.null(gs_elev) &
                        is.null(depth) & is.null(tos) & is.null(bos))) {
    stop ("please provide a data.frame or data.table with columns identified as 'location', 'type',
          'distance', 'gs_elev', 'depth', 'tos', and 'bos' or specify the vectors of 'location',
          'type', 'distance', 'gs_elev', 'depth', 'tos', and 'bos' directly.")
  }

  if (plot_gs) {
    lines(x = distance,
          y = gs_elev,
          lty = 1, lwd = 1.5)
  }

  ## plot a line from the ground surface to the bottom of the well/borehole
  segments(x0 = distance,
           y0 = gs_elev,
           x1 = distance,
           y1 = gs_elev - depth,
           lty = 1, lwd = 1, lend = "butt")

  wells <- tolower(type) == "well"

  if (any(wells) == TRUE) {
    ## plot a tick mark at the bottom of the well
    segments(x0 = distance[wells] - xs_par[["h_scale"]] * w_width / 2,
             y0 = gs_elev[wells] - depth[wells],
             x1 = distance[wells] + xs_par[["h_scale"]] * w_width / 2,
             y1 = gs_elev[wells] - depth[wells],
             lty = 1, lwd = 2, lend = "butt")

    ## plot a rectangle to the left of the well showing the screened interval
    rect(xleft = distance[wells] - xs_par[["h_scale"]] * w_width,
         ybottom = gs_elev[wells] - bos[wells],
         xright = distance[wells],
         ytop = gs_elev[wells] - tos[wells],
         density = 24, angle = 0)
  }

  ## plot a point at the total depth of each non-well borehole
  points(x = distance[!wells],
         y = gs_elev[!wells] - depth[!wells],
         pch = 19)

  ## add location labels
  text(x = distance,
       y = gs_elev + label_shift,
       labels = location,
       cex = 0.5, srt = 90, adj = c(0, 0.25), xpd = TRUE)
}

plot_xs_SBT <- function(data, line_thickness = 3) {
  SBT_cols <- data.table(SBT = c(1:13),
                         col = c(rgb(255, 0, 0, maxColorValue = 255),
                                 rgb(153, 76, 0, maxColorValue = 255),
                                 rgb(102, 153, 204, maxColorValue = 255),
                                 rgb(7, 63, 127, maxColorValue = 255),
                                 rgb(9, 127, 9, maxColorValue = 255),
                                 rgb(0, 255, 0, maxColorValue = 255),
                                 rgb(223, 255, 127, maxColorValue = 255),
                                 rgb(255, 255, 0, maxColorValue = 255),
                                 rgb(204, 178, 102, maxColorValue = 255),
                                 rgb(255, 127, 0, maxColorValue = 255),
                                 rgb(192, 192, 192, maxColorValue = 255),
                                 rgb(128, 128, 128, maxColorValue = 255),
                                 rgb(0, 0, 0, alpha = 0, maxColorValue = 255)))
  dt <- copy(data)
  dt[is.na(SBT), SBT := 13]
  dt[!(SBT %in% 1:13), SBT := 13]
  dt[, col := SBT_cols[dt[["SBT"]], col]]

  segments(x0 = dt[, .("distance" = distance[-1]), by = location][, distance],
           y0 = dt[, .("elev" = elev[-.N]), by = location][, elev],
           x1 = dt[, .("distance" = distance[-1]), by = location][, distance],
           y1 = dt[, .("elev" = elev[-1]), by = location][, elev],
           col = dt[, .("col" = col[-.N]), by = location][, col],
           lty = 1, lwd = line_thickness, lend = "butt")
}

