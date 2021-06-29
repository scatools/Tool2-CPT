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
library(future)
library(promises)
plan(multiprocess)

load("./data/newdata.Rdata")

url<-c('https://scatoolsuite.gitbook.io/sca-tool-suite/support/habitat#project-area',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/habitat#connectivity-to-existing-protected-area',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/habitat#structural-connectivity',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/habitat#threat-of-urbanization',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/habitat#composition-of-natural-lands',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/waterquality_quantity#303d-impaired-watershed-area',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/waterquality_quantity#hydrologic-response-to-land-use-change',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/waterquality_quantity#hydrologic-response-to-land-use-change',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/waterquality_quantity#hydrologic-response-to-land-use-change',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/waterquality_quantity#hydrologic-response-to-land-use-change',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/lcmr#biodiversity-index',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/lcmr#threatened-and-endangered-species-critical-habitat-area',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/lcmr#threatened-and-endangered-species-number-of-species',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/lcmr#light-pollution-index',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/community_resilience#national-register-of-historic-places',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/community_resilience#national-heritage-area',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/community_resilience#proximity-to-socially-vulnerable-communities',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/community_resilience#community-threat-index',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/economy#working-lands-high-priority',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/economy#commercial-fishing-reliance',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/economy#recreational-fishing-engagement',
       'https://scatoolsuite.gitbook.io/sca-tool-suite/support/economy#access-and-recreation-number-of-access-points'
)

descript<-c('The area of the proposed projects, or area of interest in Acres. ',
            'Hexagons that intersect or are within a 1-km2 buffer of a protected area are scored a 1 and are reported as yes (connected); all others are not connected to a protected area and score a  0, which is reported as No (not connected).  Protected areas are determined from the Protected Areas Database for the United States. This attribute prioritizes adjacency and proximity to protected areas. ',
            'A percent attribute which stands for the proportion (%) of area classified as Hub or Corridor. This attribute prioritizes large protected areas and critical corridor connections. ',
            'SLEUTH projections of urbanization by 2060 were used to identify threat of urbanization across the Gulf Coast Region. Threat of Urbanization is reported as: High (0 to 0.33) , Medium (0.34 to 0.66), and Low (0.67 to 1). This measure has an adjustable utility function. ',
            'This attribute prioritizes rare habitat types and those that have been identified as conservation priorities in state and regional plans. Scores reflect the proportion (%) of each area of interest that is covered by a priority land cover. ',
            'A percent attribute that stands for the proportion of impaired watershed within each hexagon. The watershed data are analyzed based on the 12-digit hydrologic unit code (HUC-12) level. Any HUC-12 watershed that contains an Environmental Protection Agency (EPA) 303(d) listed impaired waterbody would be considered impaired. ',
            'The magnitude of change in peak flow due to Land-Use/Land-Cover change from 1996 to 2016, analyzed at the HUC12 watershed scale.  The magnitude of peak flow change is categorized in the following way under the default value system: 0, Very Significant (10-50% increase); 0.25, Significant (5-10% increase); 0.5, Moderate (1-5% increase); 0.75, Minimal (0-1% increase); 1, No Change or Decrease (0% or lower).  Watersheds comprising of 90% or more open water were excluded from analysis. ',
            'The percent of the area of interest that is covered by irrigated agriculture, according to the 2017 version of the Moderate Resolution Imaging Spectroradiometer (MODIS) Irrigated Agriculture Dataset for the United States (Pervez and Brown 2010). ',
            'The proportion of floodplain within the area of interest that is connected or disconnected if using negative utility function, using the EPAs 100 year floodplain and a measure of relative floodplain inundation frequency derived from Landsat imagery ',
            'An average index value of the composition of lands within a 100-meter buffer of streams.  The index score was calculated by first organizing the Land-Use/Land-Cover (LULC) classes from NLCD 2016 into three tiers: 1) Developed, 2) Agricultural, and 3) Natural ',
            'The Vulnerable Areas of Terrestrial Endemic Species measures unprotected (vulnerable) prioritized species richness for >1,200 endemic (to US) mammals, birds, and amphibians. Where diversity scores are equal to the proportion of the specie range that is unprotected divided by the area of the specie range. Scores increase as range size decrease. Conversely, if a greater proportion of a specie range is within protected areas, the score decreases accordingly.  The Vulnerable Areas of Terrestrial Endemic Species was classified into 10 equal-interval categories and converted into High ( 6 to 10), Medium (3 to 6 ),  and Low ( 0 to 3 ). ',
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

goal<-c("Habitat","Habitat","Habitat","Habitat","Habitat","Water Quality","Water Quality","Water Quality","Water Quality","Water Quality","LCMR","LCMR","LCMR","LCMR","Community Resilience","Community Resilience","Community Resilience","Community Resilience","Economy","Economy","Economy","Economy")

unitattr<-c("Acres","Index", "Percentage","Index","Percentage","Percentage","Index","Percentage","Percentage","Index","Index","Percentage","Count","0-1 Index","Count","Percentage","Index","Index","Percentage","Index","Index","Count")                      


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

coln<-c("Project Area ", "Connectivity to Existing Protected Area ","Connectivity of Natural Lands ","Threat of Urbanization ",
        "Composition of Priority Natural Lands ","303D: Impaired Watershed Area ","Hydrologic Response to Land-Use Change ",
        "Percent Irrigated Agriculture ","Lateral Connectivity of Floodplain ","Composition of Riparian Zone Lands ",
        "Vulnerable Areas of Terrestrial Endemic Species ", "Threatened and Endangered Species - Critical Habitat Area ",
        "Threatened and Endangered Species - Number of Species ","Light Pollution Index ","National Register of Historic Places ",
        "National Heritage Area ","Proximity to Socially Vulnerable Communities ","Community Threat Index ", "High Priority Working Lands ",
        "Commercial Fishing Reliance ", "Recreational Fishing Engagement ","Access & Recreation: Number of Access Points ")


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
hab_PA_Table = data.frame("Measures"= c("Project Area", "Connectivity to Existing Protected Area ","Connectivity of Natural Lands","Threat of Urbanization","Composition of Priority Natural Lands"))

#WATER QUALITY Priority Attributes and Measures
wq_PA_Table = data.frame( "Measures"= c("303D: Impaired Watershed Area","Hydrologic Response to Land-Use Change","Percent Irrigated Agriculture","Lateral Connectivity of Floodplain","Composition of Riparian Zone Lands"))

#LCMR Priority Attributes and Measures
lcmr_PA_Table = data.frame(  "Measures"= c("Vulnerable Areas of Terrestrial Endemic Species", "Threatened and Endangered Species - Critical Habitat Area","Threatened and Endangered Species - Number of Species","Light Pollution Index"))

#COMMUNITY RESILIENCE Priority Attributes and Measures
commres_PA_Table = data.frame("Measures"= c("National Register of Historic Places","National Heritage Area","Proximity to Socially Vulnerable Communities","Community Threat Index"))


#GULF ECONOMY Priority Attributes and Measures
gulfecon_PA_Table = data.frame("Measures"= c( "High Priority Working Lands","Commercial Fishing Reliance", "Recreational Fishing Engagement","Access & Recreation: Number of Access Points"))

