context("Zoning")

library(rgeos)

get_spatial <- function(wkt) {
  spatial <- readWKT(wkt)
  proj4string(spatial) <- CRS("+init=epsg:32631")
  return(spatial)
}

#' Build a 2X2 data source
#'
#'   +-------+
#'   ¦ 1 ¦ 2 ¦
#'   +---+---¦
#'   ¦ 3 ¦ 4 ¦
#'   +-------+
#'
#' @return SpatialPointsDataFrame
#'
get_source_2_2 <- function() {
  coords <- get_spatial("GEOMETRYCOLLECTION(
    POINT(0.5 1.5), POINT(1.5 1.5),
    POINT(0.5 0.5), POINT(1.5 0.5))")
  a <- c(1, 2, 3, 4)
  return(SpatialPointsDataFrame(coords = coords, data = as.data.frame(a)))
}

#' Build a 3X3 data source
#'
#'   +-----------+-----+
#'   ¦  1  ¦  2  ¦ 2.2 ¦
#'   +-----+-----+-----¦
#'   ¦ 1.1 ¦  8  ¦ 4.7 ¦
#'   +-----+-----+-----¦
#'   ¦ 1.2 ¦  4  ¦ 4.3 ¦
#'   +-----------------+
#'
#' @return SpatialPointsDataFrame
#'
get_source_3_3 <- function() {
  coords <- get_spatial("GEOMETRYCOLLECTION(
    POINT(0.5 2.5), POINT(1.5 2.5), POINT(2.5 2.5),
    POINT(0.5 1.5), POINT(1.5 1.5), POINT(2.5 1.5),
    POINT(0.5 0.5), POINT(1.5 0.5), POINT(2.5 0.5))")
  a <- c(1, 2, 2.2, 1.1, 8, 4.7, 1.2, 4, 4.3)
  return(SpatialPointsDataFrame(coords = coords, data = as.data.frame(a)))
}

#' Build a 2X2 multi points data source
#'
#'   +-------+
#'   ¦ 1 ¦ 1 ¦
#'   +---+---¦
#'   ¦ 2 ¦ 2 ¦
#'   +-------+
#'
#' @return SpatialMultiPointsDataFrame
#'
get_multi_points_source <- function() {
  points1 <- get_spatial("MULTIPOINT((0.5 1.5), (1.5 1.5))")
  points2 <- get_spatial("MULTIPOINT((0.5 0.5), (1.5 0.5))")
  coords <- SpatialMultiPoints(coords = list(points1, points2), proj4string = rebuild_CRS(points1@proj4string))
  a <- c(1, 2)
  return(SpatialMultiPointsDataFrame(coords = coords, data = as.data.frame(a)))
}

#' Build a 2X2 border
#'
#' @return SpatialPolygons
#'
get_border_2_2 <- function() {
  return(get_spatial("POLYGON((0 0, 0 2, 2 2, 2 0, 0 0))"))
}

#' Build a 3X3 border
#'
#' @return SpatialPolygons
#'
get_border_3_3 <- function() {
  return(get_spatial("POLYGON((0 0, 0 3, 3 3, 3 0, 0 0))"))
}

#' Compute the boost standard deviation (returned by zone get_variance)\n
#' The boost variance has denominator n while the R variance has n-1
#'
#' @param x numeric vector, the input
#'
#' @return numeric value, the boost variance
#'
bsd <- function(x) {
  length <- length(x)
  if (length == 1) {
    return(0)
  } else {
    var <- var(x)
    return(sqrt(var * (length - 1) / length))
  }
}

get_not_zonable_source <- function() {
  coords <- get_spatial("GEOMETRYCOLLECTION(
    POINT(0 1), POINT(1 1),
    POINT(0 0), POINT(1 0))")
  a <- c("a", "b", "c", "d")
  b <- c(1, 2, NA, 4)
  c <- c(1, 1, 1, 1)
  return(SpatialPointsDataFrame(coords = coords, data = data.frame(a, b, c)))
}

get_warn_zonable_source <- function() {
  coords <- get_spatial("GEOMETRYCOLLECTION(
    POINT(0 1), POINT(1 1),
    POINT(0 0), POINT(1 0))")
  a <- c("a", "b", "c", "d")
  b <- c(1, 2, 3, 4)
  c <- c(1, 1, 1, 1)
  return(SpatialPointsDataFrame(coords = coords, data = data.frame(a, b, c)))
}

expected_map <- function(wkt, id, a) {
  Sr <- get_spatial(wkt)
  size <- sapply(a, length)
  area <- sapply(Sr@polygons, FUN = function(Polygons) gArea(SpatialPolygons(Srl = list(Polygons))))
  a_mean <- sapply(a, mean)
  a_std <- sapply(a, bsd)
  data <- data.frame(id, size, area, a_mean, a_std)
  return(SpatialPolygonsDataFrame(Sr = Sr, data = data))
}

expected_neighborhood_map <- function(wkt, filtered) {
  sl <- get_spatial(wkt)
  return(SpatialLinesDataFrame(sl = sl, data = as.data.frame(filtered)))
}

expect_default_maps <- function(zoning) {
  expected_map4 <- expected_map("GEOMETRYCOLLECTION(
    POLYGON((1 2, 2 2, 2 1, 1 1, 1 2)),
    POLYGON((1 0, 1 1, 2 1, 2 2, 3 2, 3 1, 3 0, 2 0, 1 0)),
    POLYGON((0 0, 0 1, 0 2, 0 3, 1 3, 1 2, 1 1, 1 0, 0 0)),
    POLYGON((1 2, 1 3, 2 3, 3 3, 3 2, 2 2, 1 2)))",
    id = c("5", "6", "1", "2"),
    a = list(8, c(4, 4.3, 4.7), c(1, 1.1, 1.2), c(2, 2.2))
  )
  expect_map_equal(zoning$map(4), expected_map4)

  expected_map2 <- expected_map("GEOMETRYCOLLECTION(
    POLYGON((1 2, 2 2, 2 1, 1 1, 1 2)),
    POLYGON((0 0, 0 1, 0 2, 0 3, 1 3, 2 3, 3 3, 3 2, 3 1, 3 0, 2 0, 1 0, 0 0), (2 2, 2 1, 1 1, 1 2, 2 2)))",
    id = c("5", "1"),
    a = list(8, c(1, 2, 2.2, 1.1, 4.7, 1.2, 4, 4.3))
  )
  expect_map_equal(zoning$map(2), expected_map2)
}

###############################################################################

test_that("default zoning", {
  zoning <- NewZoning(get_source_3_3())
  zoning$border <- get_border_3_3()
  zoning$perform_zoning()
  expect_default_maps(zoning)
})

test_that("zonable", {
  expect_error(NewZoning(get_not_zonable_source()), "zoning data source must contains at least one zonable data")
  expect_warning(NewZoning(get_warn_zonable_source()), "attribute\\(s\\) \\[ a, c \\] not zonable")
})

test_that("zoning attribute distance", {
  zoning <- NewZoning(get_source_2_2())
  expect_error(zoning$attribute_distance <- NULL, "the attribute distance cannot be NULL")
  expect_error(zoning$attribute_distance <- list(EuclideanDistance(), EuclideanDistance()), "the number of attribute distances must be equal to the number of zonable attributes")
  expect_error(zoning$attribute_distance <- list(NULL), "at least one attribute distance must not be NULL")
  expect_no_error(zoning$attribute_distance <- EuclideanDistance())
  expect_no_error(zoning$attribute_distance <- list(EuclideanDistance()))
})

test_that("border check", {
  zoning <- NewZoning(get_source_2_2())
  expect_error(zoning$border <- get_spatial("POLYGON((3 3, 3 4, 4 4, 4 3, 3 3))"), "no points inside the border")
  expect_warning(zoning$border <- get_spatial("POLYGON((0 1, 0 3, 2 3, 2 1, 0 1))"), "2 points are outside the border")
})

test_that("zone size check", {
  zoning <- NewZoning(get_source_2_2())
  expect_error(zoning$smallest_zone <- ZoneSize(0), "smallest zone size must be in range \\[1,4\\]")
  expect_error(zoning$smallest_zone <- ZoneSize(5), "smallest zone size must be in range \\[1,4\\]")
})

test_that("zone area check", {
  zoning <- NewZoning(get_source_3_3())
  zoning$border <- get_border_3_3()
  expect_error(zoning$smallest_zone <- ZoneArea(10), "smallest zone area must be in range \\[0,9\\]")
  zoning$smallest_zone <- ZoneArea(9)
  expect_warning(zoning$border <- get_border_2_2(), "5 points are outside the border")
  expect_error(zoning$perform_zoning(), "smallest zone area must be in range \\[0,4\\]")
})

test_that("voronoi", {
  zoning <- NewZoning(get_source_2_2())
  zoning$border <- get_border_2_2()
  zoning$perform_voronoi()

  expected_voronoi_map <- get_spatial("GEOMETRYCOLLECTION(
    POLYGON((1 2, 1 1, 0 1, 0 2, 1 2)),
    POLYGON((2 2, 2 1, 1 1, 1 2, 2 2)),
    POLYGON((1 1, 1 0, 0 0, 0 1, 1 1)),
    POLYGON((2 1, 2 0, 1 0, 1 1, 2 1)))")
  expect_identical(zoning$voronoi_map(), expected_voronoi_map)
})

test_that("neighborhood", {
  zoning <- NewZoning(get_source_2_2())
  zoning$border <- get_border_2_2()
  zoning$perform_neighborhood()

  expected_neighborhood_map <- expected_neighborhood_map(
    "GEOMETRYCOLLECTION(
     LINESTRING(0.5 0.5, 0.5 1.5),
     LINESTRING(1.5 0.5, 0.5 1.5),
     LINESTRING(0.5 0.5, 1.5 0.5),
     LINESTRING(1.5 0.5, 1.5 1.5),
     LINESTRING(1.5 1.5, 0.5 1.5))",
    rep(FALSE, 5)
  )
  expect_identical(zoning$neighborhood_map(), expected_neighborhood_map)
})

test_that("edge length neighborhood", {
  zoning <- NewZoning(get_source_2_2())
  zoning$border <- get_border_2_2()
  zoning$neighborhood <- 0.5
  zoning$perform_neighborhood()

  expected_neighborhood_map <- expected_neighborhood_map(
    "GEOMETRYCOLLECTION(
     LINESTRING(0.5 0.5, 0.5 1.5),
     LINESTRING(0.5 0.5, 1.5 0.5),
     LINESTRING(1.5 0.5, 1.5 1.5),
     LINESTRING(1.5 1.5, 0.5 1.5),
     LINESTRING(1.5 0.5, 0.5 1.5))",
    c(FALSE, FALSE, FALSE, FALSE, TRUE)
  )
  expect_identical(zoning$neighborhood_map(), expected_neighborhood_map)
})

test_that("zone size zoning", {
  zoning <- NewZoning(get_source_3_3())
  zoning$border <- get_border_3_3()
  # ZoneSize(1) in neutral for zoning
  zoning$smallest_zone <- ZoneSize(1)
  zoning$perform_zoning()
  expect_default_maps(zoning)

  zoning$smallest_zone <- ZoneSize(2)
  zoning$perform_zoning()

  expected_map2 <- expected_map("GEOMETRYCOLLECTION(
    POLYGON((0 0, 0 1, 0 2, 0 3, 1 3, 2 3, 3 3, 3 2, 2 2, 1 2, 1 1, 1 0, 0 0)),
    POLYGON((1 0, 1 1, 1 2, 2 2, 3 2, 3 1, 3 0, 2 0, 1 0)))",
    id = c("1", "5"),
    a = list(c(1, 2, 2.2, 1.1, 1.2), c(8, 4.7, 4, 4.3))
  )
  expect_map_equal(zoning$map(2), expected_map2)
})

test_that("zone area zoning", {
  zoning <- NewZoning(get_source_3_3())
  zoning$border <- get_border_3_3()
  # ZoneArea(1) in neutral for zoning
  zoning$smallest_zone <- ZoneArea(1)
  zoning$perform_zoning()
  expect_default_maps(zoning)

  zoning$smallest_zone <- ZoneArea(2)
  zoning$perform_zoning()

  expected_map2 <- expected_map("GEOMETRYCOLLECTION(
    POLYGON((0 0, 0 1, 0 2, 0 3, 1 3, 2 3, 3 3, 3 2, 2 2, 1 2, 1 1, 1 0, 0 0)),
    POLYGON((1 0, 1 1, 1 2, 2 2, 3 2, 3 1, 3 0, 2 0, 1 0)))",
    id = c("1", "5"),
    a = list(c(1, 2, 2.2, 1.1, 1.2), c(8, 4.7, 4, 4.3))
  )
  expect_map_equal(zoning$map(2), expected_map2)
})

test_that("multi points zoning", {
  zoning <- NewZoning(get_multi_points_source())
  zoning$border <- get_border_2_2()
  zoning$perform_zoning()

  expected_map2 <- expected_map("GEOMETRYCOLLECTION(
    POLYGON((0 0, 0 1, 1 1, 2 1, 2 0, 1 0, 0 0)),
    POLYGON((0 1, 0 2, 1 2, 2 2, 2 1, 1 1, 0 1)))",
    id = c("2", "1"),
    a = list(c(2, 2), c(1, 1))
  )
  expect_map_equal(zoning$map(2), expected_map2)
})
