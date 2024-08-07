install.packages("ggmap")
install.packages("mapproj")
library(ggmap)
library(mapproj)
map <- get_map(location = 'Taiwan', zoom = 7)
ggmap(map)


position = 'I-Shou University'
position <- geocode(position)
position

map <- get_map(location = c(position$lon, position$lat),
               zoom = 16, language = "zh-TW", maptype = "hybrid")



ggmap(map) + geom_point(aes(position$lon, position$lat), shape=16,
                        size=5, color="red")





uv <-read.csv("C:/Users/PC/Downloads/UV_20151116152215.csv",fileEncoding =
           "UTF-8")

lon.deg <- sapply((strsplit(as.character(uv$WGS84Lon), ",")), as.numeric)
uv$lon <- lon.deg[1, ] + lon.deg[2, ]/60 + lon.deg[3, ]/3600
lat.deg <- sapply((strsplit(as.character(uv$WGS84Lat), ",")), as.numeric)
uv$lat <- lat.deg[1, ] + lat.deg[2, ]/60 + lat.deg[3, ]/3600

map <- get_map(location = 'Taiwan', zoom = 7)
map<-ggmap(map, darken = c(0.5, "white")) + geom_point(aes(x = lon, y = lat, size = UVI,color = UVI),data = uv)


map+scale_color_continuous(low="green",high="red")+theme_bw(16)+theme(legend.position = "right")+facet_grid(~PublishAgency)

