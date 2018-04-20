# The file "crop_info.csv" was created using the following R code:

# Load the crop information from kwb.furain
crop_info <- kwb.furain:::getCropInfo()

# Simplify the crop information
crop_info <- cbind(crop_info, x_off = crop_info[, "x"] + crop_info[, "xoffset"])
crop_info <- cbind(crop_info, y_off = crop_info[, "y"] + crop_info[, "yoffset"])
crop_info <- crop_info[, c("width", "height", "x_off", "y_off")]

# Remove rows "DateTimeMin" and "DateTimeWithoutMin"
exclude <- c("DateTimeMin", "DateTimeWithoutMin")
crop_info <- crop_info[setdiff(rownames(crop_info), exclude), ]

# Create geometry strings
crop_info$geometry <- apply(crop_info, 1, function(x) geometry_area(
  
  x["width"], x["height"], x["x_off"], x["y_off"]
))

# Create named string vector gauge_geometry
gauge_geometry <- setNames(crop_info$geometry, rownames(crop_info))

# Define path to package
pkg <- "~/RProgramming/github/magickx"

# Write the simplified crop information to inst/extdata of magickx
write.table(crop_info, file.path(pkg, "inst/extdata/crop_info.csv"))

# Save gauge_geometry as dataset in the package
#devtools::use_data(gauge_geometry, pkg = pkg)
