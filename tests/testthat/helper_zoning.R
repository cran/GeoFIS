
expect_map_equal <- function(map, expected_map) {
  actual <- quasi_label(rlang::enquo(map))
  expect <- quasi_label(rlang::enquo(expected_map))
  expect_identical_crs(map, expected_map, label = actual$lab, expected.label = expect$lab)
  expect_identical(map@polygons, expected_map@polygons, label = paste(actual$lab, "polygons"), expected.label = paste(expect$lab, "polygons"))
  expect_equal(map, expected_map, label = actual$lab, expected.label = expect$lab, tolerance = 1e-3)
}
