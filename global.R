library(shiny)
library(leaflet)
library(leaflet.extras)
library(shinyjs)
library(sf)
library(zip)
library(lwgeom)
library(DT)
library(dplyr)
library(shinyalert)
library(leaflet.esri)
library(plotly)
library(tractor.base)
library("V8")
library(shinyBS)
library(shinycssloaders)
library(future)
library(promises)
plan(multiprocess)


url<-c('https://docs.google.com/document/d/1m0a2Zizx0bnQTPzNHui38IHHKyaPqcd_sGEo-XiyT-0/edit?ts=5d092aed#bookmark=id.2aesa1i947i5',
       'https://docs.google.com/document/d/1m0a2Zizx0bnQTPzNHui38IHHKyaPqcd_sGEo-XiyT-0/edit?ts=5d092aed#heading=h.hjo99vrue5qu',
       'https://docs.google.com/document/d/1m0a2Zizx0bnQTPzNHui38IHHKyaPqcd_sGEo-XiyT-0/edit?ts=5d092aed#bookmark=id.mr8kw59dz03e',
       'https://docs.google.com/document/d/1m0a2Zizx0bnQTPzNHui38IHHKyaPqcd_sGEo-XiyT-0/edit?ts=5d092aed#heading=h.ufp2zv30nw7t',
       'https://docs.google.com/document/d/1m0a2Zizx0bnQTPzNHui38IHHKyaPqcd_sGEo-XiyT-0/edit?ts=5d092aed#bookmark=id.b5t9nitj6gy7',
       'https://docs.google.com/document/d/1m0a2Zizx0bnQTPzNHui38IHHKyaPqcd_sGEo-XiyT-0/edit?ts=5d092aed#bookmark=id.b0vxl54x87rd',
       'https://about:blank',
       'https://docs.google.com/document/d/1m0a2Zizx0bnQTPzNHui38IHHKyaPqcd_sGEo-XiyT-0/edit?ts=5d092aed#bookmark=id.wqdhwld55g2y',
       'https://docs.google.com/document/d/1m0a2Zizx0bnQTPzNHui38IHHKyaPqcd_sGEo-XiyT-0/edit?ts=5d092aed#bookmark=id.1g4it6y3t5n7',
       'https://docs.google.com/document/d/1m0a2Zizx0bnQTPzNHui38IHHKyaPqcd_sGEo-XiyT-0/edit?ts=5d092aed#bookmark=id.t9u3cxpm1y4',
       'https://docs.google.com/document/d/1m0a2Zizx0bnQTPzNHui38IHHKyaPqcd_sGEo-XiyT-0/edit?ts=5d092aed#bookmark=id.fvmnzke2x8q',
       'https://docs.google.com/document/d/1m0a2Zizx0bnQTPzNHui38IHHKyaPqcd_sGEo-XiyT-0/edit?ts=5d092aed#bookmark=id.fq7hcf44q2n5',
       'https://docs.google.com/document/d/1m0a2Zizx0bnQTPzNHui38IHHKyaPqcd_sGEo-XiyT-0/edit?ts=5d092aed#bookmark=id.os8jiyqn65g4',
       'https://docs.google.com/document/d/1m0a2Zizx0bnQTPzNHui38IHHKyaPqcd_sGEo-XiyT-0/edit?ts=5d092aed#bookmark=id.iv61wa2n1mv5',
       'https://docs.google.com/document/d/1m0a2Zizx0bnQTPzNHui38IHHKyaPqcd_sGEo-XiyT-0/edit?ts=5d092aed#bookmark=id.kn88sgbkdzfi',
       'https://docs.google.com/document/d/1m0a2Zizx0bnQTPzNHui38IHHKyaPqcd_sGEo-XiyT-0/edit?ts=5d092aed#bookmark=id.h9j7xkpnr0u6',
       'https://docs.google.com/document/d/1m0a2Zizx0bnQTPzNHui38IHHKyaPqcd_sGEo-XiyT-0/edit?ts=5d092aed#bookmark=id.53k7281cxoxq',
       'https://docs.google.com/document/d/1m0a2Zizx0bnQTPzNHui38IHHKyaPqcd_sGEo-XiyT-0/edit?ts=5d092aed#heading=h.nbg2pdoj4mb3',
       'https://about:blank'
)

descript<-c('The area of the proposed projects, or area of interest in Acres. ',
            'Hexagons that intersect or are within a 1-km2 buffer of a protected area are scored a 1 and are reported as yes (connected); all others are not connected to a protected area and score a  0, which is reported as No (not connected).  Protected areas are determined from the Protected Areas Database for the United States. This attribute prioritizes adjacency and proximity to protected areas. ',
            'A percent attribute which stands for the proportion (%) of area classified as Hub or Corridor. This attribute prioritizes large protected areas and critical corridor connections. ',
            'SLEUTH projections of urbanization by 2060 were used to identify threat of urbanization across the Gulf Coast Region. Threat of Urbanization is reported as: High (0 to 0.33) , Medium (0.34 to 0.66), and Low (0.67 to 1). This measure has an adjustable utility function. ',
            'This attribute prioritizes rare habitat types and those that have been identified as conservation priorities in state and regional plans. Scores reflect the proportion (%) of each area of interest that is covered by a priority land cover. ',
            'Scores range from 0 to 100 and reflect the percentage of the watershed that is listed as impaired on the  EPA 303(d) list. Watershed data are analyzed at the HUC12 level. Higher values reflect greater impairment. The model default is to give higher scores to areas with more impairment. However, this is an adjustable utility function. ',
            'This data provides spatial information of over 2.5 million streams and waterbodies in the contiguous U.S with stream order designations. ',
            'The biodiversity index measures unprotected (vulnerable) prioritized species richness for >1,200 endemic (to US) mammals, birds, and amphibians. Where diversity scores are equal to the proportion of the specie range that is unprotected divided by the area of the specie range. Scores increase as range size decrease. Conversely, if a greater proportion of a specie range is within protected areas, the score decreases accordingly.  The biodiversity index was classified into 10 equal-interval categories and converted into High ( 6 to 10), Medium (3 to 6 ),  and Low ( 0 to 3 ) ',
            'Scores reflect the total percentage of the area that is designated as critical habitat for any threatened or endangered species by the U.S. Fish & Wildlife Service. ',
            'Number of threatened and endangered species whose ranges overlap within the area of interest. The attribute is based on the U.S. Fish & Wildlife Service species range data. ',
            'Light Pollution is reported as High, Medium, and Low.  Scaled scores range from 0 to 1, with a score of 0 indicative of intense light pollution (bright night skies) and 1 indicative of dark skies. ',
            'Scores reflect the total count of Historic Places (point data) on the National Register contained within or intersecting the area of interest boundary. ', 
            'Scores reflect the percentage of area that is within a designated National Heritage Area. ',
            'SOVI. ',
            'NFWF Community Threat Index. ',
            'This measure prioritizes agricultural and silvicultural producing lands. Scores reflect the proportion (%) of each area of interest that is covered by: Evergreen (i.e., Pine), Cropland, Rangeland, and Pasture/Hay. Data was compiled from a mosaic of Texas Ecological Mapping Systems land cover, Florida Cooperative Land Cover,and GAP/LANDFIRE National Terrestrial Ecosystems land cover data and excludes areas that are already protected (PAD-US).  ',
            'Scores range from engagement level 0-4, where 4 represents the highest level of engagement in commercial fishing activity as an index of permits and vessel landings. High: 4; Medium-High: 3; Medium: 2; Low:1, 0 is reported as insufficient data. ',
            'This data measures the presence of commercial fishing activity  relation to the population of a community through fishing activity. A high rank indicates more reliance. Scores range from 0 to 4 and reflect recreational fishing activity as an index of: charter fishing pressure, private fishing pressure, and shore fishing pressure. ',
            'Access and recreation')

goal<-c("Habitat","Habitat","Habitat","Habitat","Habitat","Water Quality","Water Quality","LCMR","LCMR","LCMR","LCMR","Community Resilience","Community Resilience","Community Resilience","Community Resilience","Economy","Economy","Economy","Economy")

unitattr<-c("Acres","Index", "Percentage","Index","Percentage","Percentage","km","Index","Percentage","Count","0-1 Index","Count","Percentage","Index","Index","Percentage","Index","Index","Count")                      


scr <- tags$script(HTML(
  "
  Shiny.addCustomMessageHandler(
  'removeleaflet',
  function(x){
  console.log('deleting',x)
  // get leaflet map
  var map = HTMLWidgets.find('#' + x.elid).getMap();
  // remove
  map.removeLayer(map._layers[x.layerid])
  })
  "
))

coln<-c("Project Area ", "Connectivity to Existing Protected Area ","Structural Connectivity Index ","Threat of Urbanization ","Land Cover - Composition of Natural Lands ","Impaired Watershed Area -- EPA '303(d)' list ","Stream Abundance ","Biodiversity Index ", "Threatened and Endangered Species - Critical Habitat Area ","Threatened and Endangered Species - Number of Species ","Light Pollution Index ","National Register of Historic Places ","National Heritage Area ","Social Vulnerability Index ","Community Threat Index ", "Working Lands ","Commercial Fishery Index ", "Recreational Fishery Index ","Access & Recreation")


#changecolor <- "shinyjs.pageCol = function(id,color) {
#    $('id').css('color', color);
# }"
#changecolor<-"shinyjs.pageCol = function(params){
#      alert(params);
#    };"
#jsCode1 <- "shinyjs.pageCol = function(params){$('body').css('background', params);}"

jscode <- "shinyjs.refresh = function() { history.go(0); }
           shinyjs.pageCol = function(id,color) {$('id').css('color', color);}"

HU12 <- st_read(dsn = "./hex_tile/HU12_SCA.shp", layer = "HU12_SCA")
HU12<- st_transform(HU12, crs= 4326)
HU12$color<-"blue"

choice<-as.character(HU12$NAME)
choiceid<-as.character(HU12$HUC12)
Hex_tile <- st_read(dsn = "./hex_tile/Hex_tile.shp", layer = "Hex_tile")
Hex_tile<- st_transform(Hex_tile, crs= 4326)


SCA <- st_read(dsn = "./hex_tile/SCA.shp", layer = "SCA")
SCA<- st_transform(SCA, crs= 4326)

#padus <- st_read(dsn = "./hex_tile/padus.shp", layer = "padus")
#padus<- st_transform(padus, crs= 4326)

#HABITAT Priority Attributes and Measures
hab_PA_Table = data.frame("Measures"= c("Project Area", "Connectivity to Existing Protected Area ","Structural Connectivity Index","Threat of Urbanization","Land Cover - Composition of Natural Lands"))

#WATER QUALITY Priority Attributes and Measures
wq_PA_Table = data.frame( "Measures"= c("Impaired Watershed Area -- EPA '303(d)' list ","Stream Abundance"))

#LCMR Priority Attributes and Measures
lcmr_PA_Table = data.frame(  "Measures"= c("Biodiversity Index", "Threatened and Endangered Species - Critical Habitat Area ","Threatened and Endangered Species - Number of Species","Light Pollution Index"))

#COMMUNITY RESILIENCE Priority Attributes and Measures
commres_PA_Table = data.frame("Measures"= c("National Register of Historic Places ","National Heritage Area","Social Vulnerability Index","Community Threat Index"))


#GULF ECONOMY Priority Attributes and Measures
gulfecon_PA_Table = data.frame("Measures"= c( "Working Lands","Commercial Fishery Index", "Recreational Fishery Index","Access & Recreation"))

