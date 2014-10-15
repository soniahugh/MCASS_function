#### shpfile.MCASS (DRAFT)
#### Author: Sonia Hugh
#### Contact: soniahugh@gmail.com
#### Date: 23/05/2014
####______________________________________________________________________________________________________________________
#### Function to change shape files to MCASS ready raster.  It needs a mask layer of study areas with binary values 
#### -9999 for NA values and any other value for the study area.
####
#### mask =       Raster object. Mask raster used as a base for the map and also as a template for resolution, extent and projection.
####              Must have -9999 values for NA regions.  Usually a binary map of study region and NA region.
####
#### shp =        points (a SpatialPoints* object, or a two-column matrix (or data.frame)), SpatialLines*, SpatialPolygons*, or an Extent object
#### mask.value = Numeric. Value to assigned to the region of the mask not equal to -9999.  The value of the study area can easily be changed in this function the default value for the study area is -888.
####
#### field =      numeric or character. The value(s) to be transferred. This can be a single number, or a vector of numbers 
####              that has the same length as the number of spatial features (points, lines, polygons). 
####              If x is a Spatial*DataFrame, this can be the column name of the variable to be transferred. If missing, the 
####              attribute index is used (i.e. numbers from 1 to the number of features). You can also provide a vector with 
####              the same length as the number of spatial features, or a matrix where the number of rows matches the number 
####              of spatial features
####
#### type =       numeric. There are two inputs 1, which will create a raster with only one value for all the fields and 2 (default)
####              which assigns a value for each unique value in the field.
####
#### value =      numeric. Value assigned to the features in type 1 that are not part of the mask layer.
####
#### mm =         Logical. To determine whether the raster output of the shapefile will be merged with the mask.  The default is TRUE.
#_________________________________________________________________________________________________________________________

shpfile.MCASS<-function(mask1,shp,field, type = 2,  mask.value = -888, value = 1, mm = 2){
  mask1[mask1!=-9999]<-mask.value
  shp<-spTransform(shp, CRS(projection(mask1)))
  if (type == 1) {
    if(missing(field)){
      rr<-rasterize(shp, mask1)
      rr[rr>0]<-value
    }else{
      rr<-rasterize(shp,mask1,field)
      rr[rr>0]<- value}
    
  }else{
    if(missing(field)){
      rr<-rasterize(shp, mask1)
    }else{
      rr<-rasterize(shp, mask1, field)
    }
  }
  if(mm==1){
    reclassify(rr, c(NA,NA, -9999))
  }else if(mm==2){
    merge(rr, mask1)
  }else{
    rr
  }
}


###########

# Function for table creation
##shp.field = the field used in the 'field' argument in the shapefile.MCASS function

mcass.shp.table<-function(shp.field, path = "./mcass.txt"){
  table.prep<-data.frame(as.character(unique(shp.field)))
  table.prep<-cbind(1:nrow(table.prep), rep("=",nrow(table.prep)), table.prep)
  write.table(table.prep, path, row.names = F, col.names = F, quote = FALSE, na = "")
}


#### raster.MCASS (DRAFT)
#### Author: Sonia Hugh
#### Contact: soniahugh@gmail.com
#### Date: 23/05/2014
####______________________________________________________________________________________________________________________
#### Function to change raster into MCASS ready raster.  It needs a mask layer of study areas with binary values 
#### -9999 for NA values and any other value for the study area (default value for the study area is -888).
####
#### mask =       Raster object. Mask raster used as a base for the map and also as a template for resolution, extent and projection.
####              Must have -9999 values for NA regions.  Usually a binary map of study region and NA region.
####
#### rast =       Raster object.  Raster to be converted into MCASS format.
####
#### mask.value = Numeric. Value to be assigned to the region of the mask not equal to -9999.  
####              The value of the study area can easily be changed in this function the default value for the study area is -888.
####
#### mm =         Logical. To determine whether the raster output of the input raster object will be merged with the mask.  
####              The default is FALSE.
#### proj =       The projection of the raster input if the current projection is NA.
#_________________________________________________________________________________________________________________________



raster.MCASS<-function(mask, rast,mask.value = -888, mm = FALSE, proj){
  if(is.na(projection(rast))){
    projection(rast)<-proj
  }
  if (projection(mask) != projection(rast)){
    rast<-projectRaster(rast,mask)
  }
  if(mm == 1L){
    mask[mask!=-9999]<-mask.value
    merge(rast,mask)
  }else{
    reclassify(rast, c(NA,NA, -9999))}
}


}