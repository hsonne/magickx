# The file "crop_info.csv" was created using the following R code:

# Load the crop information from kwb.furain
crop_info <- kwb.furain:::getCropInfo()

# Simplify the crop information
crop_info <- cbind(crop_info, x_off = crop_info[, "x"] + crop_info[, "xoffset"])
crop_info <- cbind(crop_info, y_off = crop_info[, "y"] + crop_info[, "yoffset"])
crop_info <- crop_info[, c("width", "height", "x_off", "y_off")]

# Write the simplified crop information to inst/extdata of magickx
file <- "~/RProgramming/github/magickx/inst/extdata/crop_info.csv"
write.table(crop_info, file)
