#' RunElecation
#' @description Visualize ...
#' @param run a run data frame returned by \code{import_run}.
#' @return A ggplot object.
#' @examples
#' run2profile(smooth = 0.5)
#' @section Warning:
#' Do not operate heavy machinery within 8 hours of using this function.
#' @export
run2profile <- function(run = import_run(),
                        smooth = 2,
                        # Plot settings
                        title = 'runR-Elevationplot',
                        subtitle = 'Untertitel',
                        xlab = 'Time',
                        ylab = 'Elevation (in meters)'
                        ){
  
  p <- ggplot(run, 
              aes(x=time, y=ele))+
    geom_point() + 
    geom_line() + 
    geom_smooth(method = "loess",span = smooth,color = "red")+
    labs(title = title,
         subtitle = subtitle,
         x=xlab, y= ylab)+
    theme_minimal() ;p
  return(p)
}


#' Run2Map
#' @description Visualize Run2Map
#' @param run a run data frame returned by \code{import_run}.
#' @return A ggplot object.
#' @examples
#' run <- import_run(file = 'data/2017-11-06.gpx',type = 'gpx')
#' head(run)
#' @section Warning:
#' Do not operate heavy machinery within 8 hours of using this function.
#' @export
run2map <- function(run_track = import_run(file = 'data/2017-11-06.gpx',
                                           layer = 'tracks')){
  
  track <- run_track
  
  l <- leaflet() %>% addTiles() %>% addPolylines(data=wp)
  
  return(l)
  }




# run2map()

# m <- leaflet() %>%
#   
#   # Add tiles
#   addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
#   addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
#   addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
#   addLegend(position = 'bottomright',opacity = 0.4, 
#             colors = 'blue', 
#             labels = 'Gimillan-Grausson',
#             title = 'Hikes Italy, region Aosta') %>%
#   
#   # Layers control
#   addLayersControl(position = 'bottomright',
#                    baseGroups = c("Topographical", "Road map", "Satellite"),
#                    overlayGroups = c("Hiking routes", "Photo markers"),
#                    options = layersControlOptions(collapsed = FALSE)) %>%
#   
#   addPolylines(data=track, color='blue', group='Hiking routes') 
# 
# 
# track <- readOGR('data/2017-11-06.gpx', layer = "tracks", verbose = FALSE)
# leaflet() %>% addTiles() %>% addPolylines(data=track)
# 
# 
