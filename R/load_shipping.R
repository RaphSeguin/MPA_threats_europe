load_shipping <- function(){
  
  shipping <- raster("data/shapefiles/shipping_lanes/shipping.tif")
  
  return(shipping)
  
}