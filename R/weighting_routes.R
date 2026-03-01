library(sf)

## Function for calculating routes ----
segmentize_routes <- function(all, max_len = 10) {
  all %>%
    st_transform(3857) %>%
    st_segmentize(dfMaxLength = max_len) %>%
    st_cast("LINESTRING") %>%
    mutate(seg_id = row_number())
}
build_segment_grid <- function(segments, buffer = 50, cellsize = 10) {
  roi <- st_union(segments) |> st_buffer(buffer)
  
  grid <- st_make_grid(roi, cellsize = cellsize, square = TRUE)
  grid <- st_sf(
    grid_id = seq_len(length(grid)),
    geometry = grid
  )
  
  list(
    segments = segments,
    grid = grid
  )
}
compute_segment_density <- function(grid_segments) {
  segments <- grid_segments$segments
  grid     <- grid_segments$grid
  
  seg_grid <- st_intersection(segments, grid)
  
  grid_weights <- seg_grid %>%
    count(grid_id, name = "weight")
  
  weighted_segments <- seg_grid %>%
    left_join(st_drop_geometry(grid_weights), by = "grid_id") %>%
    st_transform(4326)
  
  return(weighted_segments)
}

## Caching workflow for computing only when added route ----
make_signature <- function(all, max_len, buffer, cellsize) {
  list(
    dates    = sort(all$date),
    n        = nrow(all),
    max_len  = max_len,
    buffer   = buffer,
    cellsize = cellsize
  )
}

sig_path <- "data/weighted_routes_signature.rds"
res_path <- "data/weighted_routes.rds"

signature <- make_signature(routes, 10, 50, 10)

if (
  !file.exists(sig_path) ||
  !file.exists(res_path) ||
  !identical(signature, readRDS(sig_path))
) {
  
  message("Recomputing weighted routes")
  
  weighted_routes <-
    segmentize_routes(routes, max_len = 10) %>%
    build_segment_grid(buffer = 50, cellsize = 10) %>%
    compute_segment_density()
  
  saveRDS(weighted_routes, res_path)
  saveRDS(signature, sig_path)
  
} else {
  message("Using cached weighted routes")
  weighted_routes <- readRDS(res_path)
}