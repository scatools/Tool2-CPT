function(input, output, session) {
  #Structure: 1. needed data and reactive value
  #           2. feature and functions by page
  
  #page0: preload settings when launch
  #datasets preloaded:1. criteria_labels (labels for RESTORE goals)
  #                   2. colorlist (color for rank accept && spatial polygons)
  #                   3. colorlist1 (color for central weights)
  #                   4. proplist (default names for proposals)
  showModal(div(id="ModalDiv123", modalDialog(
    title = "Welcome to the SCA Conservation Prioritization Tool",
    HTML(c("<div align='left'>
             &nbsp; &nbsp;Using this tool, you can create a custom report on your areas of interest (up to 10), with our catalog of over 20 metrics and address particular conservation and restoration questions.<br/>
             Some key features:</br>
             <ul>
             <li>Quickly create custom prioritization maps</li>
             <li>HTML and CSV outputs</li>
             <li>Over 20 metrics</li>
             </ul>
             <b>Intended Use</b></br>
             &nbsp; &nbsp; The Conservation Prioritization Tool (CPT) is <b>not</b> intended to be prescriptive. Instead this tool was designed to provide data to <b>support</b> conservation planning efforts across the Gulf Coast Region. All users acknowledge that the CPT model is intended to <b>explore</b> ecological and socioeconomic co-benefits of proposed areas of land conservation, and should <B>not</b> be used in a decision making context.<br/>
             &nbsp; &nbsp; The flexibility of this tool enables a user to evaluate conservation alternatives using either a multi-criteria decision analysis (MCDA) framework, or user-defined values.</div>")),
    footer = tagList(
                     actionButton("oneprojectmode", "Single project mode"),
                     actionButton("multipleprojectmode", "Multiple project mode"),
                     actionButton("portfoliomode", "Portfolio mode",onclick = "window.open('https://drive.google.com/file/d/1YlOha1F9N1gpnLKePMQsGTdVMZHJAp6X/view',target='_blank')")),
    size = "l"
  )))
  

  #source("./reporttesting/report_table.R")
  criteria_labels = c('Habitat','Water Quality & Quantity','Living Coastal & Marine Resources','Community Resilience','Gulf Economy')
  colorlist=c("orange1","lightsteelblue2","indianred1","darkorange1","royalblue3","paleturquoise1","firebrick2","darkorchid2","palegreen2","lightpink2")
  collist_rgb<-c("rgb(255,165,0)","rgb(188,210,238)","rgb(255,106,106)","rgb(255,127,0)","rgb(58,95,205)","rgb(187,255,255)",
                 "rgb(238,44,44)","rgb(178,58,238)","rgb(144,238,144)","rgb(238,162,173)")
  colorlist1<-c("green","brown","gray","coral","aquamarine","chartreuse","cyan","chocolate","antiquewhite","cadetblue")
  proplist<-c("Area of Interest 1","Area of Interest 2","Area of Interest 3","Area of Interest 4","Area of Interest 5","Area of Interest 6","Area of Interest 7","Area of Interest 8","Area of Interest 9","Area of Interest 10")
  value1<-reactiveValues()
  proplist_os<-"Area of Interest 1"
  #update and hide tabs 
  observe({
    hide(selector = "#nav li a[data-value=MCDA]")
    hide(selector = "#nav li a[data-value=Result]")
    hide(selector = "#tabsResult li a[data-value=SMAA_result]")
    hide(selector = "#tabsResult li a[data-value=Weights_result]")
    hide(selector = "#tabsResult li a[data-value=defaultweight]")
    hide(selector = "#tabs li a[data-value=goalweight]")
    hide(selector = "#tabs li a[data-value=PAweight]")
    hide(selector = "#viewdata li a[data-value=scaledata]")
    hide(selector = "#viewdata li a[data-value=deselect]")
    hide(selector = "#viewdata li a[data-value=addattr]")
    hide(selector = "#viewdata li a[data-value=rename]")
    hide(selector = "#viewdata li a[data-value=utility]")
    
    hide(selector = "#viewdataos li a[data-value=renameos]")
    hide(selector = "#viewdataos li a[data-value=redefineos]")
    
    hide(selector = "#tabs li a[data-value=osdataoverview]")
    hide(selector = "#tabs li a[data-value=dataoverview]")
    hide(selector = "#tabs li a[data-value=portfoliooverview]")
    hide("weights_review_ua")
    #hide("osdataoverview")
    #hide("dataoverview")
    #hide(selector = "#tabs li a[data-value=wsboverview]")
    #hide("portfoliooverview")
    shinyjs::hide("gotorawtable")
    shinyjs::hide("gotodeselect")
    shinyjs::hide("updatetable")
    shinyjs::hide("gotoutility")
    shinyjs::hide("addweight")
    shinyjs::hide("renameproject")
    # shinyjs::hide("downloadData")
    # shinyjs::hide("download2")
    shinyjs::hide("numMCDA")
    shinyjs::hide("adjustmcdanumbers")
    shinyjs::hide("advancedoptions")
    shinyjs::hide("advancedoptionsos")
    shinyjs::hide("osupdatetable")
  })
  
  
  #page1: landing page
  
  output$goalsTable<-renderImage({
    
    filename <- normalizePath(file.path('./images',
                                        paste('RESTORE_goals', '.png', sep='')))
    return(list(
      src= filename, width=550))
  }, deleteFile = F) 
  
  # output$logo<-renderImage({
  # return(list(
  #  src= "images/msu_fws_logos.png",
  #  contentType = "image/png",
  #   width=400))
  # }, deleteFile = F) 
  
  
  observeEvent(input$oneprojectmode,{
    show("controls")
    value1$mode<-"os"
    removeModal(session = getDefaultReactiveDomain())
    show(selector = "#tabs li a[data-value=osdataoverview]")
    hide(selector = "#tabs li a[data-value=dataoverview]")
    hide(selector = "#tabs li a[data-value=portfoliodataoverview]")
    #show("osdataoverview")
    #hide("dataoverview")
    #hide(selector = "#tabs li a[data-value=wsbdataoverview]")
    #hide("portfoliodataoverview")
    click("learnmoreos")
  })
  observeEvent(input$multipleprojectmode,{
    show("controls")
    value1$mode<-"mcda"
    removeModal(session = getDefaultReactiveDomain())
    #show("dataoverview")
    #hide("osdataoverview")
    #hide(selector = "#tabs li a[data-value=wsbdataoverview]")
    #hide("portfoliodataoverview")
    show(selector = "#tabs li a[data-value=dataoverview]")
    hide(selector = "#tabs li a[data-value=osdataoverview]")
    hide(selector = "#tabs li a[data-value=portfoliodataoverview]")
  })
  
  observeEvent(input$portfoliomode,{
    show("controls")
    value1$mode<-"portfolio"
    removeModal(session = getDefaultReactiveDomain())
    #show("portfoliodataoverview")
    #hide("osdataoverview")
    #hide(selector = "#tabs li a[data-value=wsbdataoverview]")
    #hide("dataoverview")
    show(selector = "#tabs li a[data-value=portfoliodataoverview]")
    hide(selector = "#tabs li a[data-value=dataoverview]")
    hide(selector = "#tabs li a[data-value=osdataoverview]")
  })
  
  
  
  #page2: Spatial footprint
  
  output$map <- renderLeaflet({
    leaflet()%>% 
      addProviderTiles(providers$Esri.WorldStreetMap,group = "ESRI WorldStreetmap") %>%setView(-90.4369, 28.9072, 6) %>%
      addProviderTiles(providers$OpenTopoMap,group="Openstreetmap Topo Layer")%>%
      addEsriBasemapLayer(esriBasemapLayers$Imagery,group="ESRI World Imagery", autoLabels = TRUE)%>%
      addPolygons(data=SCA, fill = F , options = pathOptions(clickable = FALSE))%>%
      addEsriTiledMapLayer(
        url = "https://gis1.usgs.gov/arcgis/rest/services/padus2_1/FeeManagers/MapServer",
        options = providerTileOptions(opacity = 0.15),group = "PAD-US")%>%
      addEsriDynamicMapLayer(
        url = "https://gis.usgs.gov/sciencebase2/rest/services/Catalog/5da9e701e4b09fd3b0c9cb6a/MapServer",
        options = dynamicMapLayerOptions(opacity = 0.15),group = "SECAS")%>%
      addLayersControl(baseGroups=c("ESRI WorldStreetmap","Openstreetmap Topo Layer","ESRI World Imagery"),
                       overlayGroups = c("PAD-US","SECAS"),
                       position="bottomleft")%>%
      addDrawToolbar(
        targetGroup='drawn',
        polylineOptions=FALSE,
        circleOptions= FALSE ,
        rectangleOptions=FALSE,
        markerOptions=FALSE,
        circleMarkerOptions=FALSE,
        polygonOptions=drawPolygonOptions(
          shapeOptions = drawShapeOptions(clickable = F)
        ),
        editOptions = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()))
    
  })
  
  #input1: drawing
  #value input: 1. input$threshold (number of plans)
  #             2. input$button (finish one project)
  #             3. input$finish (finish all projects)
  #             4. input$map_draw_start && input$map_draw_new_feature (start of a new project footprint)
  #             5. input$text (custome name for the project)
  #value created: 1. value1$num (number of plans imported)
  #               2. value1$drawingpolygon (start drawing a footprint (0[drawing] or 10[done]))
  #               3. value1$test11 (finish drawing one project, save the data in)
  #               4. value1$nshape (total numbers of shape drawn)
  #               5. value1$dshape (total numbers of shape drawn before this one)
  #               6. ps_list$result (list of all the drawn polygons, list[sf])
  #               7. ps_list$hex (list of numeric array)
  #               8. ps_list$hex_final (numeric arry)
  #               9. ps_list$hex_merge (hex loaded & merged from hex_details)
  #               10. result$datatable (list of data by project, list[numeric array])
  #               11. result$matrix (matrix format from aggregating result$datatable)
  #value output: 1. output$value (number of plans left)
  #              2. output$map (update on main map)
  
  ps_list<-reactiveValues()
  ps_list_portfolio<-reactiveValues()
  ps_list$result<-list()
  ps_list$hex<-list()
  ps_list$hex_merge<-NULL
  ps_list$hex_select<-NULL
  ps_list_import<-reactiveValues()
  ps_list_import$result=NULL
  ps_list_import$result_1=NULL
  ps_list_select<-reactiveValues()
  ps_list_select$result<-NULL
  ps_list_os<-reactiveValues()
  ps_list_portfolio_import<-reactiveValues()
  ps_list_portfolio_import$result=NULL
  ps_list_portfolio_import$result_1=NULL
  
  #features 1: update the control panel 
  
  #default value with 0 plans included
  value1$num=0
  value1$num_portfolio=0
  value1$nshape = 0
  value1$dshape = 0
  value1$test11<-0
  value1$test12<-0
  value1$test_utility<-0
  value1$test14<-0
  value1$test15<-0
  value1$test16<-0
  value1$test17<-0
  value1$HU12color<-HU12$color
  value1$HU12color_mcda<-HU12$color
  value1$txtbackend<-0
  value1$test20<-0
  value1$test_portfolio<-0
  value1$test_portfolio2<-0
  value1$test_portfolio1<-0
  value1$test_portfolio3<-0
  value1$mcdarun<-10000
  value1$imported<-0
  value1$lastinput<-0
  value1$habitat<-20
  value1$habitat1<-20
  value1$water<-20
  value1$water1<-20
  value1$species<-20
  value1$species1<-20
  value1$resilience<-20
  value1$resilience1<-20
  value1$economy<-20
  value1$economy1<-20
  value1$imported_portfolio<-0
  
  
  #default value show no on-going draw
  value1$drawingpolygon=10
  observeEvent(input$map_draw_start,{
    value1$drawingpolygon <- 10
  })
  #update the number of plans left
  output$value <- renderText({ input$threshold - value1$num })
  output$value_import <- renderText({ value1$imported })
  output$value_import_portfolio<-renderText({value1$imported_portfolio})
  #update the number of plans left _ portfolio
  output$value_portfolio <- renderText({ input$threshold_portfolio - value1$num_portfolio })
  #update the value to current drawing
  observeEvent(input$map_draw_new_feature,{
    value1$drawingpolygon <- 0
  })
  output$drawingpolygon <- reactive({
    value1$drawingpolygon
  })
  outputOptions(output, 'drawingpolygon', suspendWhenHidden = FALSE)
  
  #feature2: finish one project input
  observeEvent(input$button, {
    #load in customized name for this project
    proplist[value1$num+1]<<-input$text
    #update the conditional panel
    value1$drawingpolygon <- 10
    #trigger the event to load that polygon in
    value1$test11<-10
  })
  
  observeEvent(input$button_portfolio, {
    #load in customized name for this project
    proplist[value1$num_portfolio+1]<<-input$text_portfolio
    #update the conditional panel
    value1$drawingpolygon <- 10
    #trigger the event to load that polygon in
    value1$test_portfolio<-10
  })
  
 
  
  
  observe({
    if(input$way3=='boundary'){
      withProgress(message = 'Loading dataset', value = 0, {
        
        labs <- lapply(seq(nrow(HU12)), function(i) {
          paste0( HU12$NAME[i], "<br>")
        })
        
      leafletProxy("map")%>%
        addPolygons(data=HU12,fillColor = HU12$color,fill=T,color="Black",fillOpacity = 0.5, 
                    label = lapply(labs, HTML),weight = 0.5, smoothFactor = 0.5,group = "hu12",layerId=as.character(HU12$OBJECTID),
                    highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE,fillColor = "white",fillOpacity = 0.1))
        incProgress(0.5, detail = paste("Fetching shape"))
        value1$mode1 <- 'wsb1' 
      
      })
      }
  })
  
  observe({
    if(input$way3!='boundary'){
      leafletProxy("map")%>%
        clearGroup("hu12")
    }
  })
  
  
  observe({
    if(input$way4=='boundary'){
      withProgress(message = 'Loading data', value = 0, {
        labs <- lapply(seq(nrow(HU12)), function(i) {
          paste0( HU12$NAME[i], "<br>")
        })
        
        
        
      leafletProxy("map")%>%
        addPolygons(data=HU12,fillColor = HU12$color,fill=T,color="Black",fillOpacity = 0.5, weight = 0.5,
                    label = lapply(labs, HTML),smoothFactor = 0.5,group = "hu12_mcda",layerId=paste0("mcda",as.character(HU12$OBJECTID)),
                    highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE,fillColor = "white",fillOpacity = 0.1))
        incProgress(0.5, detail = paste("Fetching shape"))
        value1$mode1 <- 'wsb2'
      
      })
      }
  })
  
  observe({
    if(input$way4!='boundary'){
      leafletProxy("map")%>%
        clearGroup("hu12_mcda")
    }
  })
  
  observeEvent(input$map_shape_click,{
    value1$clickid<-input$map_shape_click$id
    value1$clickid<-as.character(value1$clickid)
    #print(value1$mode1 =="wsb2")
    if(value1$mode1 =="wsb1"){
     if(value1$HU12color[HU12$OBJECTID==value1$clickid]=="blue"){
      value1$HU12color[HU12$OBJECTID==value1$clickid]<-"red"
      leafletProxy("map")%>%
        removeShape(as.character(value1$clickid))%>%
        addPolygons(data=HU12[HU12$OBJECTID==value1$clickid,], fillColor=value1$HU12color[HU12$OBJECTID==value1$clickid],fillOpacity = 0.5,weight =0.5,smoothFactor = 0.5,group ="hu12", layerId= as.character(value1$clickid))

     }
     else if(value1$HU12color[HU12$OBJECTID==value1$clickid]=="red"){
      value1$HU12color[HU12$OBJECTID==value1$clickid]<-"blue"
      leafletProxy("map")%>%
        removeShape(as.character(value1$clickid))%>%
        addPolygons(data=HU12[HU12$OBJECTID==value1$clickid,], fillColor = value1$HU12color[HU12$OBJECTID==value1$clickid],fill=T,color="Black",fillOpacity = 0.5, weight = 0.5, smoothFactor = 0.5,group = "hu12",layerId=as.character(value1$clickid),
                    highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE,fillColor = "white",fillOpacity = 0.1))
     }
    }
    else if(value1$mode1 =="wsb2"){
      if(value1$HU12color_mcda[HU12$OBJECTID==substr(value1$clickid,5,nchar(value1$clickid))]=="blue"){
        value1$HU12color_mcda[HU12$OBJECTID==as.character(substr(value1$clickid,5,nchar(value1$clickid)))]<-"red"
        leafletProxy("map")%>%
          removeShape(as.character(value1$clickid))%>%
          addPolygons(data=HU12[HU12$OBJECTID==substr(value1$clickid,5,nchar(value1$clickid)),], fillColor="red",fillOpacity = 0.5,weight =0.5,smoothFactor = 0.5,group ="hu12_mcda", layerId= as.character(value1$clickid))
      }
      else if(value1$HU12color_mcda[HU12$OBJECTID==substr(value1$clickid,5,nchar(value1$clickid))]=="red"){
        value1$HU12color_mcda[HU12$OBJECTID==substr(value1$clickid,5,nchar(value1$clickid))]<-"blue"
        leafletProxy("map")%>%
          removeShape(as.character(value1$clickid))%>%
          addPolygons(data=HU12[HU12$OBJECTID==substr(value1$clickid,5,nchar(value1$clickid)),], fillColor = "blue",fill=T,color="Black",fillOpacity = 0.5, weight = 0.5, smoothFactor = 0.5,group = "hu12_mcda",layerId=as.character(value1$clickid),
                      highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE,fillColor = "white",fillOpacity = 0.1))
      }
    }
  })
  observeEvent(input$osfinish, {
    
    #load in customized name for this project
    proplist_os<<-input$text1
    #update the conditional panel
    #value1$drawingpolygon <- 10
    #trigger the event to load that polygon in
    value1$test17<-10
    show(selector = "#nav li a[data-value=MCDA]")
    updateTabsetPanel(session = session, inputId = "nav", "MCDA")
    updateTabsetPanel(session = session, inputId = "tabs", "osdataoverview")
   
  })

  
  
  #observe({
  #  value1$numr<-input$threshold - value1$num
  #  if(value1$numr ==0){
  #    leafletProxy("map")%>%
  #    removeDrawToolbar(clearFeatures = T)
  #  }
  #})
  
  observe({
    withProgress(message = 'Processing...', value = 0, {
      if(value1$test11==10){
        #update the default name for next project
        updateTextInput(session, "text", value = proplist[value1$num+2])
        #determine how many shapes are drawn
        value1$nshape<-length(drawnshapes)
        sps<-NULL
        incProgress(0.1, detail = paste("Fetching shape"))
        #add the polygon in
        for(i in (value1$dshape+1):value1$nshape){
          polyg<<-input$map_draw_all_features$features[[i]]$geometry$coordinates[[1]]
          x<-numeric(0)
          y<-numeric(0)
          for(i in 1:length(polyg)){
            x[i]<-as.numeric(polyg[i][[1]][[1]][1])
            y[i]<-as.numeric(polyg[i][[1]][[2]][1])
          }
          xym <- cbind(x, y)
          pol<-st_sfc(st_polygon(list(xym)),crs = 4326)
          num<-value1$num+1
          if(length(sps)==0){
            sps<-st_sf(data.frame(geom=pol,proposal=num))
          }
          else{
            sps<-rbind(sps,st_sf(data.frame(geom=pol,proposal=num)))
            sps<-st_combine(sps)
            sps<-st_sf(data.frame(geom=sps,proposal=num))
          }
        }
        incProgress(0.3, detail = paste("Saving shape"))
        #save the polygon to ps_list
        ps_list$result[[num]]<<-sps
        value1$num<-num
        incProgress(0.2, detail = paste("Organizing shape"))
        #remove the polygon
        lapply(
          drawnshapes[(value1$dshape+1):value1$nshape],
          function(todelete) {
            session$sendCustomMessage(
              "removeleaflet",
              list(elid="map", layerid=todelete)
            )
          }
        )
        value1$dshape<-length(drawnshapes)
        incProgress(0.2, detail = paste("Removing draft shape"))
        #update the main map
        leafletProxy("map",data = sps)%>%
          addPolygons(color = "#e87f17", weight = 1)
        value1$test11<-0
        incProgress(0.2, detail = paste("Finished"))
      }
    })
  })
  
  observe({
    withProgress(message = 'Processing...', value = 0, {
      if(value1$test_portfolio==10){
        #update the default name for next project
        updateTextInput(session, "text_portfolio", value = proplist[value1$num_portfolio+2])
        #determine how many shapes are drawn
        value1$nshape<-length(drawnshapes)
        sps<-NULL
        incProgress(0.1, detail = paste("Fetching shape"))
        #add the polygon in
        for(i in (value1$dshape+1):value1$nshape){
          polyg<<-input$map_draw_all_features$features[[i]]$geometry$coordinates[[1]]
          x<-numeric(0)
          y<-numeric(0)
          for(i in 1:length(polyg)){
            x[i]<-as.numeric(polyg[i][[1]][[1]][1])
            y[i]<-as.numeric(polyg[i][[1]][[2]][1])
          }
          xym <- cbind(x, y)
          pol<-st_sfc(st_polygon(list(xym)),crs = 4326)
          num<-value1$num_portfolio+1
          if(length(sps)==0){
            sps<-st_sf(data.frame(geom=pol,proposal=num))
          }
          else{
            sps<-rbind(sps,st_sf(data.frame(geom=pol,proposal=num)))
            sps<-st_combine(sps)
            sps<-st_sf(data.frame(geom=sps,proposal=num))
          }
        }
        incProgress(0.3, detail = paste("Saving shape"))
        #save the polygon to ps_list
        ps_list_portfolio$result[[num]]<<-sps
        value1$num_portfolio<-num
        
        incProgress(0.2, detail = paste("Organizing shape"))
        #remove the polygon
        lapply(
          drawnshapes[(value1$dshape+1):value1$nshape],
          function(todelete) {
            session$sendCustomMessage(
              "removeleaflet",
              list(elid="map", layerid=todelete)
            )
          }
        )
        value1$dshape<-length(drawnshapes)
        incProgress(0.2, detail = paste("Removing draft shape"))
        #update the main map
        leafletProxy("map",data = sps)%>%
          addPolygons(color = "#e87f17", weight = 1)
        value1$test_portfolio<-0
        incProgress(0.2, detail = paste("Finished"))
      }
    })
  })
  
  
  #support function to get leafletid of drawn polygons
  observeEvent(
    input$map_draw_all_features,
    {
      drawnshapes <<- lapply(
        input$map_draw_all_features$features,
        function(ftr) {
          ftr$properties$`_leaflet_id`
        }
      )
      # seeing is believing
      str(drawnshapes)
    }
  )
  
  
  
  observe({
    withProgress(message = 'Processing...', value = 0, {
      if(value1$test17==10){
        #determine how many shapes are drawn
        value1$nshape<-length(drawnshapes)
        sps<-NULL
        incProgress(0.1, detail = paste("Fetching shape"))
        #add the polygon in
        for(i in (value1$dshape+1):value1$nshape){
          polyg<<-input$map_draw_all_features$features[[i]]$geometry$coordinates[[1]]
          x<-numeric(0)
          y<-numeric(0)
          for(i in 1:length(polyg)){
            x[i]<-as.numeric(polyg[i][[1]][[1]][1])
            y[i]<-as.numeric(polyg[i][[1]][[2]][1])
          }
          xym <- cbind(x, y)
          pol<-st_sfc(st_polygon(list(xym)),crs = 4326)
          if(length(sps)==0){
            sps<-st_sf(data.frame(geom=pol,proposal=1))
          }
          else{
            sps<-rbind(sps,st_sf(data.frame(geom=pol,proposal=1)))
            sps<-st_combine(sps)
            sps<-st_sf(data.frame(geom=sps,proposal=1))
          }
        }
        incProgress(0.3, detail = paste("Saving shape"))
        #save the polygon to ps_list
        ps_list_os$result<<-sps
        
        incProgress(0.2, detail = paste("Organizing shape"))
        #remove the polygon
        lapply(
          drawnshapes[(value1$dshape+1):value1$nshape],
          function(todelete) {
            session$sendCustomMessage(
              "removeleaflet",
              list(elid="map", layerid=todelete)
            )
          }
        )
        value1$dshape<-length(drawnshapes)
        incProgress(0.2, detail = paste("Removing draft shape"))
        #update the main map
        leafletProxy("map",data = sps)%>%
          addPolygons(color = "#e87f17", weight = 1)
        incProgress(0.2, detail = paste("Finished"))
        
        withProgress(message = 'Processing...', value = 0, {
          incProgress(0.2, detail = paste("Saving spatial data"))  
          
          data1<- ps_list_os$result
          join_result<-st_join(data1, Hex_tile, join = st_intersects)
          ps_list_os$hex<- join_result$Id-1
          ps_list_os$hex_final<-ps_list_os$hex
          leafletProxy("map",data = data1)%>%
            addPolygons(color = "#545454", weight = 1)
          
          #ps_list_os$hex_merge<-st_read("./hex_details/geojson/1.geojson") ### #
          ps_list_os$hex_merge<-st_read("./hex_details2/geojson/1.geojson")
          ps_list_os$hex_merge<-st_transform(ps_list_os$hex_merge, crs=4326)
          
          incProgress(0.2, detail = paste("Connecting database"))
          dir <- file.path(".", "hex_details2","geojson")  ### #
          
          #print(length(ps_list_os$hex_final))
          for(i in 1:length(ps_list_os$hex_final)){
            temp<-paste(c(as.character(ps_list_os$hex_final[[i]]),".geojson"),collapse ="")
            temp1<- st_read(dsn = file.path(dir,temp))
            temp1<- st_transform(temp1, crs=4326 )
            #print(length(temp1$OBJECTID))
            ps_list_os$hex_merge<- rbind(ps_list_os$hex_merge,temp1)
            ps_list_os$hex_merge<-ps_list_os$hex_merge[match(unique(ps_list_os$hex_merge$OBJECTID),ps_list_os$hex_merge$OBJECTID),]
            
          }
          
          incProgress(0.2, detail = paste("Performing spatial analysis"))
          
          data2<-st_join(ps_list_os$hex_merge,data1,join=st_intersects,left=F)
          print(data2)  ### #
          names(data2)[1]<-paste("OBJECTID")
          #print(as.numeric(st_area(data1)/1000000))
          #print(1-max(data2$Sleuth_v2))
          result_os$datatable<-c(as.numeric(st_area(data1)/1000000),max(data2$PADUS2),sum(data2$area_conne),(1-max(data2$Sleuth_v2)),sum(data2$conl_index),sum(data2$area_12_13),mean(data2$wq3),(sum(data2$wq4)/length(data2$wq4))*100,mean(data2[data2$wq5>-1,]$wq5)*100,mean(data2[data2$wq6>-1,]$wq6),max(data2$Index_cpt_),sum(data2$PA1),max(as.numeric(as.character(data2$statuscoun))),min(data2$area_light),max(as.numeric(as.character(data2$Join_Cou_2))),sum(data2$area_nha),max(data2$SOVInew),max(data2$THREATINDE),sum(data2$WORKINGLAN),max(data2$ComEng_ct),max(data2$RecEng_ct),max(data2$AR_boat)) ### #
          #print(result_os$datatable)
          result_os$datatable_context<-c(sum(data2$metal_cuzo),min(data2$X303d_desig),length(data2$area_conne))
          ##print("testing")
          ##print(result_os$datatable_context)
          tmpworking<-workingland3[which(workingland3$OBJECTID %in% data2$OBJECTID),]
          tmpte<-TE_index[which(TE_index$OBJECTID %in% data2$OBJECTID),]
          tmplanduse<-landuse[which(landuse$objectid %in% data2$OBJECTID),]
          
          if(length(tmpte$OBJECTID)==0){
            tmpte <- 0
          }else{
            tmpte<-paste0(tmpte$SPPCodeNew,collapse="")
            tmpte<-substring(tmpte,seq(1,nchar(tmpte)-3,4),seq(4,nchar(tmpte),4))
            tmpte<-unique(tmpte)
            tmpte<-TE_Code_Name[which(TE_Code_Name$SPCODE %in% tmpte),]
            tmpte<-paste0(tmpte$COMNAME,collapse = ",")
            #print(tmpte)
          }
          if(length(tmpworking$OBJECTID)==0){
            tmpworkingsum<-0
            tmpworkingclass<-0
          }else{
            tmpworkingsum<-mean(tmpworking$sum)
            tmpworkingclass<-3
            tmpworkingcode<-as.character(tmpworking$code_safe)
            
            tmpworkingcode<-substr(tmpworkingcode,2,13)
            #split the barcode into 3 columns
            #Evergreen = substr(tmpworkingcode, 1, 4)
            #Cropland = substr(tmpworkingcode, 5, 8)
            #Pasture = substr(tmpworkingcode, 9, 12)
            
            Evergreen<- paste0(substr(tmpworkingcode,1,3),".",substr(tmpworkingcode,4,4))
            Cropland<- paste0(substr(tmpworkingcode,5,7), ".", substr(tmpworkingcode,8,8))
            Pasture<- paste0(substr(tmpworkingcode,9,11), ".", substr(tmpworkingcode,12,12))
            
            #convert the 3 columns to numeric
            Evergreen<- as.numeric(Evergreen)
            Cropland<- as.numeric(Cropland)
            Pasture<-as.numeric(Pasture)
            
            #take the average for each of the 3 columns
            avg_evergreen<- round(sum(Evergreen)/length(Evergreen), digits = 1)
            avg_cropland<- round(sum(Cropland)/length(Cropland), digits = 1)
            avg_pasture<- round(sum(Pasture)/length(Pasture), digits = 1)
            
            #put the averages in a vector
            vector_avg<-paste0(avg_evergreen,",",avg_cropland,",",avg_pasture)
            sum_avg<-avg_evergreen + avg_cropland + avg_pasture
            #print(vector_avg)
            #print(sum_avg)
            if(sum(tmpworking$p1)==0){
              tmpworkingclass<-tmpworkingclass-1
            }
            if(sum(tmpworking$p2)==0){
              tmpworkingclass<-tmpworkingclass-1
            }
            if(sum(tmpworking$p3)==0){
              tmpworkingclass<-tmpworkingclass-1
            }
            
          }
          
          if(length(tmplanduse$objectid)==0){
            tmplanduse<-0
          }else{
            tmplanduse<-paste0(tmplanduse$classindex,collapse="")
            tmplanduse<-substring(tmplanduse,seq(1,nchar(tmplanduse)-3,4),seq(4,nchar(tmplanduse),4))
            tmplanduse<-unique(tmplanduse)
            tmplanduse<-length(tmplanduse)
          }
          
          result_os$datatable_additional<-c(tmpworkingsum,tmpte,tmpworkingclass,tmplanduse,vector_avg,sum_avg)
          
          result_os$matrix_context<-as.matrix(result_os$datatable_context)
          result_os$matrix<-as.matrix(result_os$datatable)
          ps_list_os$hex_merge_final<- data2
          incProgress(0.2, detail = paste("Calculating matrix "))
          
          
          
          rownames(result_os$matrix) <- coln
          result_os$showing_matrix<-round(result_os$matrix,2)
          result_os$showing_matrix[3]<-round(result_os$showing_matrix[3]/result_os$datatable_context[3]*100,0)
          result_os$showing_matrix[5]<-round(result_os$showing_matrix[5]/result_os$datatable_context[3]*100,0)
          result_os$showing_matrix[6]<-round(result_os$showing_matrix[6]/result_os$datatable_context[3]*100,0)
          result_os$showing_matrix[12]<-round(result_os$showing_matrix[12]/result_os$datatable_context[3]*100,0) ### #
          result_os$showing_matrix[16]<-round(result_os$showing_matrix[16]/result_os$datatable_context[3]*100,0) ### #
          result_os$showing_matrix[19]<-round(result_os$showing_matrix[19]/result_os$datatable_context[3]*100,0) ### #
          colnames(result_os$showing_matrix)<-proplist_os
          
          incProgress(0.2, detail = paste("Finished"))
        })
        #print(result_os$matrix)
        value1$test17<-0
      }
    })
  })
#feature2: finish all projects input
  
  observeEvent(input$finish, {
    if(value1$lastinput==1){
      showModal(div(id="ModalDiv", modalDialog(
        title = "There are areas of interest captured.",
        "Please use the Startover Feature to input others.",
        footer = tagList(modalButton("OK"))
      )))
    }
    else{
    show(selector = "#nav li a[data-value=MCDA]")
    updateTabsetPanel(session = session, inputId = "nav", "MCDA")
    updateTabsetPanel(session = session, inputId = "tabs", "dataoverview")
    value1$test12<-10
    }
  })
  
  observeEvent(input$finish_portfolio, {
    
    show(selector = "#nav li a[data-value=MCDA]")
    updateTabsetPanel(session = session, inputId = "nav", "MCDA")
    updateTabsetPanel(session = session, inputId = "tabs", "portfoliooverview")
    value1$test_portfolio3<-10
  })
  
  observe({
    if(value1$test12==10){
      withProgress(message = 'Processing...', value = 0, {
        for(i in 1:length(ps_list$result)){
          data1<- ps_list$result[[i]]
          join_result<-st_join(data1, Hex_tile, join = st_intersects)
          ps_list$hex[[i]]<- join_result$Id-1
          ps_list$hex_final<-c(ps_list$hex_final,ps_list$hex[[i]])
          leafletProxy("map",data = data1)%>%
            addPolygons(color = "#66ff66", weight = 1)
        }
        ps_list$hex_merge<-st_read("./hex_details2/geojson/1.geojson") ### #
        ps_list$hex_merge<-st_transform(ps_list$hex_merge, crs=4326)
        dir <- file.path(".", "hex_details2","geojson")  ### #
        for(i in 1:length(ps_list$hex_final)){
          temp<-paste(c(as.character(ps_list$hex_final[i]),".geojson"),collapse ="")
          temp1<- st_read(dsn = file.path(dir,temp))
          temp1<- st_transform(temp1, crs=4326 )
          ps_list$hex_merge<- rbind(ps_list$hex_merge,temp1)
          ps_list$hex_merge<-ps_list$hex_merge[match(unique(ps_list$hex_merge$OBJECTID),ps_list$hex_merge$OBJECTID),]
        }
        incProgress(0.2, detail = paste("Sending shape to server."))
        
        for(i in 1:length(ps_list$result)){
          data1<- ps_list$result[[i]]
          data2<-st_join(ps_list$hex_merge,data1,join=st_intersects,left=F)
          data2$appid<-i
          incProgress(0.4/length(ps_list$result), detail = paste("Handling shape: ", i))
          result$datatable[[i]]<-c(as.numeric(st_area(data1)/1000000),max(data2$PADUS2),sum(data2$area_conne),1-max(data2$Sleuth_v2),sum(data2$conl_index),sum(data2$area_12_13),mean(data2$wq3),(sum(data2$wq4)/length(data2$wq4))*100,mean(data2[data2$wq5>-1,]$wq5)*100,mean(data2[data2$wq6>-1,]$wq6),max(data2$Index_cpt_),sum(data2$PA1),max(as.numeric(as.character(data2$statuscoun))),min(data2$area_light),max(as.numeric(as.character(data2$Join_Cou_2))),sum(data2$area_nha),max(data2$SOVInew),max(data2$THREATINDE),sum(data2$WORKINGLAN),max(data2$ComEng_ct),max(data2$RecEng_ct),max(data2$AR_boat)) ### #
          result$datatable_context[[i]]<-c(length(data2$area_conne)) #detects the number of hexagons for each area of interest
          result$datatable[[i]][3]<-result$datatable[[i]][3]/result$datatable_context[[i]][1]*100 
          result$datatable[[i]][5]<-result$datatable[[i]][5]/result$datatable_context[[i]][1]*100 
          result$datatable[[i]][6]<-result$datatable[[i]][6]/result$datatable_context[[i]][1]*100
          result$datatable[[i]][12]<-result$datatable[[i]][12]/result$datatable_context[[i]][1]*100 ### #
          result$datatable[[i]][16]<-result$datatable[[i]][16]/result$datatable_context[[i]][1]*100 ### #
          result$datatable[[i]][19]<-result$datatable[[i]][19]/result$datatable_context[[i]][1]*100 ### #
          if(is.null(ps_list$hex_merge_final)){
            ps_list$hex_merge_final<- data2
          }else{ps_list$hex_merge_final<-rbind(ps_list$hex_merge_final,data2)}
          
          if(is.null(result$matrix)){result$matrix<-result$datatable[[i]]}
          else{result$matrix<-cbind(result$matrix,result$datatable[[i]])}
        }
        result$matrix_raw<-result$matrix
        incProgress(0.2, detail = paste("Calculating matrix "))
        #Maximum area of 303D, 
        #to be used in calculating utility for Impaired watershed
        print(result$datatable[[1]])
        print(result$datatable_context[[1]][1])
        print(result$datatable_context[[2]][1])
        
        
        #COMPREHENSIVE FOR LOOP
        for (i in 1:ncol(result$matrix)){
          #1. Threat of Urbanization
          result$matrix[4,i]<-result$matrix[4,i]
          #2. Connectivity with PAD-US
          result$matrix[2,i]<-result$matrix[2,i]
          #3. Connectivity of Natural Lands 
          result$matrix[3,i]<-result$matrix[3,i]/100
          #4. Proposed Area of Conservation
          result$matrix[1,i]<-ifelse(result$matrix[1,i]==0,0,
                                     ifelse(result$matrix[1,i]<=.4,.3,
                                            ifelse(result$matrix[1,i]<=.8,.75,
                                                   ifelse(result$matrix[1,i]<=2,.9,1))))
          #5. Composition of Natural Lands
          result$matrix[5,i]<-result$matrix[5,i]/100
          #6. 303D - Impaired watershed area
          result$matrix[6,i]<-(result$matrix[6,i]/100) 
          #7. Hydrologic Response to Land-Use Change
          result$matrix[7,i]<-result$matrix[7,i]
          #8. Percent of irrigated agriculture
          result$matrix[8,i]<-result$matrix[8,i]/100
          #9. Lateral Connectivity of Floodplain
          result$matrix[9,i]<-result$matrix[9,i]/100
          #10. Composition of Riparian Zone Lands
          result$matrix[10,i]<-result$matrix[10,i]
          
          #11. Biodiversity Index
          result$matrix[11,i]<-result$matrix[11,i]/10
          #12. T&E Species Area
          result$matrix[12,i]<-ifelse(result$matrix[12,i]<=0.001,0,
                                      ifelse(result$matrix[12,i]<=20,.75,
                                             ifelse(result$matrix[12,i]<=60,.9,1)))
          #13. T&E Number of Species
          result$matrix[13,i]<-ifelse(result$matrix[13,i]==0,0,
                                      ifelse(result$matrix[13,i]==1,.9,
                                             ifelse(result$matrix[1,i]==2,.95,1)))
          #14. Light Pollution Index
          #if higher the better
          #result$matrix[9,i]<-result$matrix[9,i]
          #if lower the better
          result$matrix[14,i]<-1-result$matrix[14,i]
          
          #15. National Register of Historic Places
          result$matrix[15,i]<-ifelse(result$matrix[15,i]==0,0,
                                      ifelse(result$matrix[15,i]==1,.75,
                                             ifelse(result$matrix[15,i]==2,.9,1)))
          #16. National Heritage Area
          result$matrix[16,i]<-result$matrix[16,i]/100
          
          #17
          result$matrix[17,i]<-result$matrix[17,i]
          #18
          result$matrix[18,i]<-result$matrix[18,i]
          #19. Working Lands - High Priority
          result$matrix[19,i]<-result$matrix[19,i]/100
          #20. Commercial Fisheries - Engagement
          result$matrix[20,i]<-ifelse(result$matrix[20,i]==0,0,
                                      ifelse(result$matrix[20,i]==1,.25,
                                             ifelse(result$matrix[20,i]==2,.5,
                                                    ifelse(result$matrix[20,i]==3,.75,1))))
          #21. Recreational Fisheries - Engagement
          result$matrix[21,i]<-ifelse(result$matrix[21,i]==0,0,
                                      ifelse(result$matrix[21,i]==1,.25,
                                             ifelse(result$matrix[21,i]==2,.5,
                                                    ifelse(result$matrix[21,i]==3,.75,1))))
          #22. Access &Recreation
          result$matrix[22,i]<-ifelse(result$matrix[22,i]==0,0,
                                      ifelse(result$matrix[22,i]<=5,.25,
                                             ifelse(result$matrix[22,i]<=10,.75,
                                                    ifelse(result$matrix[22,i]<=15,.9,1)))) 
        }
        
        if(max(result$matrix[7,])==0){
          max7<-1
        }else{
          max7<-max(result$matrix[7,])}
        
        if(max(result$matrix[3,])==0){
          max3<-1
        }else{
          max3<-max(result$matrix[3,])}
        result$matrix[3,]<-result$matrix[3,]/max3
        
        if(max(result$matrix[5,])==0){
          max5<-1
        }else{
          max5<-max(result$matrix[5,])}
        result$matrix[5,]<-result$matrix[5,]/max5
        
        if(max(result$matrix[16,])==0){
          max16<-1
        }else{
          max16<-max(result$matrix[16,])}
        result$matrix[16,]<-result$matrix[16,]/max16
        
        if(max(result$matrix[19,])==0){
          max19<-1
        }else{
          max19<-max(result$matrix[19,])}
        result$matrix[19,]<-result$matrix[19,]/max19

        
        result$matrix[7,]<-result$matrix[7,]/max7
        
        
        rownames(result$matrix) <- coln[1:nrow(result$matrix)]
        result$showing_matrix<-round(result$matrix,2)
        rownames(result$matrix_raw)<-coln[1:nrow(result$matrix_raw)]
        result$showing_matrix_raw<-round(result$matrix_raw,2)
        colnames(result$showing_matrix)<-proplist[1:ncol(result$showing_matrix)]
        colnames(result$showing_matrix_raw)<-proplist[1:ncol(result$showing_matrix_raw)]
        result$rankaccept_altlist<-proplist[1:ncol(result$showing_matrix)]
        
        ##print(result$showing_matrix)
        ##print(class(result$rankaccept_altlist))
        incProgress(0.2, detail = paste("Finished."))
        ##print(result$matrix)
      })
      value1$test12<-0
      value1$lastinput<-1
    }
  })
  
  observe({
    if(value1$test_portfolio3==10){
      withProgress(message = 'Processing...', value = 0, {
        for(i in 1:length(ps_list_portfolio$result)){
          data1<- ps_list_portfolio$result[[i]]
          join_result<-st_join(data1, Hex_tile, join = st_intersects)
          ps_list$hex[[i]]<- join_result$Id-1
          ps_list$hex_final<-c(ps_list$hex_final,ps_list$hex[[i]])
          leafletProxy("map",data = data1)%>%
            addPolygons(color = "#66ff66", weight = 1)
        }
        ps_list$hex_merge<-st_read("./hex_details2/geojson/1.geojson")
        ps_list$hex_merge<-st_transform(ps_list$hex_merge, crs=4326)
        dir <- file.path(".", "hex_details2","geojson")
        #print(length(ps_list$hex_final))
        for(i in 1:length(ps_list$hex_final)){
          temp<-paste(c(as.character(ps_list$hex_final[i]),".geojson"),collapse ="")
          temp1<- st_read(dsn = file.path(dir,temp))
          temp1<- st_transform(temp1, crs=4326 )
          ps_list$hex_merge<- rbind(ps_list$hex_merge,temp1)
          ps_list$hex_merge<-ps_list$hex_merge[match(unique(ps_list$hex_merge$OBJECTID),ps_list$hex_merge$OBJECTID),]
        }
        incProgress(0.2, detail = paste("Sending shape to server."))
        for(i in 1:length(ps_list_portfolio$result)){
          data1<- ps_list_portfolio$result[[i]]
          data2<-st_join(ps_list$hex_merge,data1,join=st_intersects,left=F)
          
          data2$appid<-i
          incProgress(0.4/length(ps_list_portfolio$result), detail = paste("Handling shape: ", i))
          result$datatable_portfolio[[i]]<-c(as.numeric(st_area(data1)/1000000),max(data2$PADUS2),sum(data2$area_conne),1-max(data2$Sleuth_v2),sum(data2$conl_index),sum(data2$area_12_13),mean(data2$wq3),(sum(data2$wq4)/length(data2$wq4))*100,mean(data2[data2$wq5>-1,]$wq5)*100,mean(data2[data2$wq6>-1,]$wq6),max(data2$Index_cpt_),sum(data2$PA1),max(as.numeric(as.character(data2$statuscoun))),min(data2$area_light),max(as.numeric(as.character(data2$Join_Cou_2))),sum(data2$area_nha),max(data2$SOVInew),max(data2$THREATINDE),sum(data2$WORKINGLAN),max(data2$ComEng_ct),max(data2$RecEng_ct),max(data2$AR_boat))
          
          if(is.null(ps_list$hex_merge_final)){
            ps_list$hex_merge_final<- data2
          }else{ps_list$hex_merge_final<-rbind(ps_list$hex_merge_final,data2)}
          result$datatable_portfolio_context[[i]]<-c(length(data2$area_conne))
          result$datatable_portfolio[[i]][3]<-result$datatable_portfolio[[i]][3]/result$datatable_portfolio_context[[i]][1]*result$datatable_portfolio[[i]][4]
          result$datatable_portfolio[[i]][5]<-result$datatable_portfolio[[i]][5]/result$datatable_portfolio_context[[i]][1]*result$datatable_portfolio[[i]][4]
          result$datatable_portfolio[[i]][6]<-result$datatable_portfolio[[i]][6]/result$datatable_portfolio_context[[i]][1]*result$datatable_portfolio[[i]][4]
          result$datatable_portfolio[[i]][12]<-result$datatable_portfolio[[i]][12]/result$datatable_portfolio_context[[i]][1]*result$datatable_portfolio[[i]][4]
          result$datatable_portfolio[[i]][16]<-result$datatable_portfolio[[i]][16]/result$datatable_portfolio_context[[i]][1]*result$datatable_portfolio[[i]][4]
          result$datatable_portfolio[[i]][19]<-result$datatable_portfolio[[i]][19]/result$datatable_portfolio_context[[i]][1]*result$datatable_portfolio[[i]][4]
          if(is.null(result$matrix_portfolio)){result$matrix_portfolio<-result$datatable_portfolio[[i]]}
          else{result$matrix_portfolio<-cbind(result$matrix_portfolio,result$datatable_portfolio[[i]])}
          
        }
        result$matrix_portfolio_raw<-result$matrix_portfolio
        #print('check here')
        #print(result$matrix_portfolio_raw)
        ConservArea<-result$matrix_portfolio[4,]
        incProgress(0.2, detail = paste("Calculating matrix "))
        #Maximum area of 303D, 
        #to be used in calculating utility for Impaired watershed
        
        for (i in 1:ncol(result$matrix_portfolio)){
          #1. Threat of Urbanization
          result$matrix_portfolio[4,i]<-result$matrix_portfolio[4,i]
          #2. Connectivity with PAD-US
          result$matrix_portfolio[2,i]<-result$matrix_portfolio[2,i]
          #3. Connectivity of Natural Lands 
          result$matrix_portfolio[3,i]<-result$matrix_portfolio[3,i]/100
          #4. Proposed Area of Conservation
          result$matrix_portfolio[1,i]<-ifelse(result$matrix_portfolio[1,i]==0,0,
                                     ifelse(result$matrix_portfolio[1,i]<=.4,.3,
                                            ifelse(result$matrix_portfolio[1,i]<=.8,.75,
                                                   ifelse(result$matrix_portfolio[1,i]<=2,.9,1))))
          #5. Composition of Natural Lands
          result$matrix_portfolio[5,i]<-result$matrix_portfolio[5,i]/100
          #6. 303D - Impaired watershed area
          result$matrix_portfolio[6,i]<-1-(result$matrix_portfolio[6,i]/100) 
          #7. Hydrologic Response to Land-Use Change
          result$matrix_portfolio[7,i]<-result$matrix_portfolio[7,i]
          #8. Percent of irrigated agriculture
          result$matrix_portfolio[8,i]<-result$matrix_portfolio[8,i]
          #9. Lateral Connectivity of Floodplain
          result$matrix_portfolio[9,i]<-result$matrix_portfolio[9,i]
          #10. Composition of Riparian Zone Lands
          result$matrix_portfolio[10,i]<-result$matrix_portfolio[10,i]
          
          #11. Biodiversity Index
          result$matrix_portfolio[11,i]<-result$matrix_portfolio[11,i]/10
          #12. T&E Species Area
          result$matrix_portfolio[12,i]<-ifelse(result$matrix_portfolio[12,i]<=0.001,0,
                                      ifelse(result$matrix_portfolio[12,i]<=20,.75,
                                             ifelse(result$matrix_portfolio[12,i]<=60,.9,1)))
          #13. T&E Number of Species
          result$matrix_portfolio[13,i]<-ifelse(result$matrix_portfolio[13,i]==0,0,
                                      ifelse(result$matrix_portfolio[13,i]==1,.9,
                                             ifelse(result$matrix_portfolio[1,i]==2,.95,1)))
          #14. Light Pollution Index
          #if higher the better
          #result$matrix_portfolio[9,i]<-result$matrix_portfolio[9,i]
          #if lower the better
          result$matrix_portfolio[14,i]<-1-result$matrix_portfolio[14,i]
          
          #15. National Register of Historic Places
          result$matrix_portfolio[15,i]<-ifelse(result$matrix_portfolio[15,i]==0,0,
                                      ifelse(result$matrix_portfolio[15,i]==1,.75,
                                             ifelse(result$matrix_portfolio[15,i]==2,.9,1)))
          #16. National Heritage Area
          result$matrix_portfolio[16,i]<-result$matrix_portfolio[16,i]/100
          
          #17
          result$matrix_portfolio[17,i]<-result$matrix_portfolio[17,i]
          #18
          result$matrix_portfolio[18,i]<-result$matrix_portfolio[18,i]
          #19. Working Lands - High Priority
          result$matrix_portfolio[19,i]<-result$matrix_portfolio[19,i]/100
          #20. Commercial Fisheries - Engagement
          result$matrix_portfolio[20,i]<-ifelse(result$matrix_portfolio[20,i]==0,0,
                                      ifelse(result$matrix_portfolio[20,i]==1,.25,
                                             ifelse(result$matrix_portfolio[20,i]==2,.5,
                                                    ifelse(result$matrix_portfolio[20,i]==3,.75,1))))
          #21. Recreational Fisheries - Engagement
          result$matrix_portfolio[21,i]<-ifelse(result$matrix_portfolio[21,i]==0,0,
                                      ifelse(result$matrix_portfolio[21,i]==1,.25,
                                             ifelse(result$matrix_portfolio[21,i]==2,.5,
                                                    ifelse(result$matrix_portfolio[21,i]==3,.75,1))))
          #22. Access &Recreation
          result$matrix_portfolio[22,i]<-ifelse(result$matrix_portfolio[22,i]==0,0,
                                      ifelse(result$matrix_portfolio[22,i]<=5,.25,
                                             ifelse(result$matrix_portfolio[22,i]<=10,.75,
                                                    ifelse(result$matrix_portfolio[22,i]<=15,.9,1)))) 
        }
        
        if(max(result$matrix_portfolio[7,])==0){
          max7<-1
        }else{
          max7<-max(result$matrix_portfolio[7,])}
        
        if(max(result$matrix_portfolio[3,])==0){
          max3<-1
        }else{
          max3<-max(result$matrix_portfolio[3,])}
        result$matrix_portfolio[3,]<-result$matrix_portfolio[3,]/max3
        
        if(max(result$matrix_portfolio[5,])==0){
          max5<-1
        }else{
          max5<-max(result$matrix_portfolio[5,])}
        result$matrix_portfolio[5,]<-result$matrix_portfolio[5,]/max5
        
        if(max(result$matrix_portfolio[16,])==0){
          max16<-1
        }else{
          max16<-max(result$matrix_portfolio[16,])}
        result$matrix_portfolio[16,]<-result$matrix_portfolio[16,]/max16
        
        if(max(result$matrix_portfolio[19,])==0){
          max19<-1
        }else{
          max19<-max(result$matrix_portfolio[19,])}
        result$matrix_portfolio[19,]<-result$matrix_portfolio[19,]/max19
        
        
        result$matrix_portfolio[7,]<-result$matrix_portfolio[7,]/max7
        
        
        
        
        
        rownames(result$matrix_portfolio) <- coln[1:nrow(result$matrix_portfolio)]
        result$showing_matrix_portfolio<-round(result$matrix_portfolio,2)
        rownames(result$matrix_portfolio_raw)<-coln[1:nrow(result$matrix_portfolio_raw)]
        result$showing_matrix_portfolio_raw<-round(result$matrix_portfolio_raw,2)
        colnames(result$showing_matrix_portfolio)<-proplist[1:ncol(result$showing_matrix_portfolio)]
        colnames(result$showing_matrix_portfolio_raw)<-proplist[1:ncol(result$showing_matrix_portfolio_raw)]
        result$rankaccept_altlist<-proplist[1:ncol(result$showing_matrix_portfolio)]
        
        ##print(result$showing_matrix)
        ##print(class(result$rankaccept_altlist))
        incProgress(0.2, detail = paste("Finished."))
        ##print(result$matrix_portfolio)
      })
      value1$test_portfolio3<-0
    }
  })
  
  
  #reactive values needed for systems
  value2<-reactiveValues(clickedMarker=NULL,newaddedweight=0)
  weight<-reactiveValues()
  result<-reactiveValues()
  result$showing_rankaccept<-matrix()
  result$showing_central<-matrix()
  result$final_rank_1<-matrix()
  result_os<-reactiveValues()
  
  
  
  value1$test13<-0
  
  observeEvent(input$file1, {
    value1$test13<-10
  })
  
  observe({
    if(value1$test13==10){
      inFile <- input$file1
      if (!is.null(inFile))
      { 
        traildirectory<-"./input"
        ##print(traildirectory)
        file.copy(inFile$datapath,overwrite=TRUE, file.path(traildirectory, inFile$name))
        unzip(inFile$datapath,exdir = traildirectory)
        
        listfile<-list.files(traildirectory)
        listfile<-grep('shp', listfile, value=TRUE)
        if(length(listfile)==0){
          listfile<-list.files(traildirectory)
          listfile<-grep('kml', listfile, value=TRUE)
        }
        if(length(listfile)==0){
          traildirectory<-"./input/download"
          listfile<-list.files(traildirectory)
          listfile<-grep('shp', listfile, value=TRUE)
        }
        
        
        if(length(listfile)!=0){
          #print(paste0(traildirectory,"/",listfile))
          Newspdata <- st_read(dsn = paste0(traildirectory,"/",listfile)
                             #, layer = substr(listfile,1,nchar(listfile)-4)
                             )
          #print(Newspdata)
          Newspdata.wgs84<- st_transform(Newspdata, crs = 4326)
        if((length(Newspdata.wgs84$geometry)+value1$imported)>10){
          showModal(div(id="ModalDiv", modalDialog(
            title = "The total number of plans captured is over 10",
            "Please revisit the shapefile.",
            footer = tagList(modalButton("OK"))
          )))
          unlink(traildirectory, recursive = T)
        }
        else{
        
        
        boundary<-st_bbox(Newspdata.wgs84)
        if(length(Newspdata.wgs84$Name)>0 &length(Newspdata.wgs84$Name)<=10){
          proplist[(value1$imported+1):(length(Newspdata.wgs84$Name)+value1$imported)] <<-as.character(Newspdata.wgs84$Name)
          Newspdata.wgs84<-Newspdata.wgs84[,c("Name","geometry")]
        }
        else{
          
          Newspdata.wgs84$Name<-proplist[(value1$imported+1):(length(Newspdata.wgs84$geometry)+value1$imported)]
          Newspdata.wgs84<-Newspdata.wgs84[,c("Name","geometry")]
        }
        value1$imported<-value1$imported+length(Newspdata.wgs84$geometry)
        #print(ps_list_import$result)
        ps_list_import$result<-rbind(ps_list_import$result,Newspdata.wgs84)
        leafletProxy("map")%>%
          fitBounds(as.numeric(boundary$xmin), as.numeric(boundary$ymin), as.numeric(boundary$xmax), as.numeric(boundary$ymax))%>%
          addPolygons(data = Newspdata.wgs84,color = "#e87f17", weight = 1)
        ##print(length(ps_list_import$result$geometry))
        #print(ps_list_import$result)
        unlink(traildirectory, recursive = T)
        }
      }}
      Newspdata.wgs84<-NULL
    }
    value1$test13<-0
  })
  
  observeEvent(input$file1_portfolio, {
    value1$test_portfolio2<-10
  })
  
  observe({
    if(value1$test_portfolio2==10){
      inFile <- input$file1_portfolio
      if (!is.null(inFile))
      { 
        traildirectory<-"./input"
        ##print(traildirectory)
        file.copy(inFile$datapath,overwrite=TRUE, file.path(traildirectory, inFile$name))
        unzip(inFile$datapath,exdir = traildirectory)
        ##print(list.files(traildirectory, pattern = "*.shp$"))
        listfile<-list.files(traildirectory)
        #print(listfile)
        listfile<-grep('shp', listfile, value=TRUE)
        if(length(listfile)==0){
          listfile<-list.files(traildirectory)
          listfile<-grep('kml', listfile, value=TRUE)
        }
        if(length(listfile)==0){
          traildirectory<-"./input/download"
          listfile<-list.files(traildirectory)
          listfile<-grep('shp', listfile, value=TRUE)
        }
        
        #print(listfile)
        if(length(listfile)!=0){
        Newspdata <- st_read(dsn = paste0(traildirectory,"/",listfile)
                             #, layer = substr(listfile,1,nchar(listfile)-4)
                             )
        #print(Newspdata)
        Newspdata.wgs84<- st_transform(Newspdata, crs = 4326)
        if((length(Newspdata.wgs84$geometry)+value1$imported)>10){
          showModal(div(id="ModalDiv", modalDialog(
            title = "The total number of plans captured is over 10",
            "Please revisit the shapefile.",
            footer = tagList(modalButton("OK"))
          )))
          unlink(traildirectory, recursive = T)
        }
        else{
          
          
          boundary<-st_bbox(Newspdata.wgs84)
          if(length(Newspdata.wgs84$Name)>0 &length(Newspdata.wgs84$Name)<=10){
            proplist[(value1$imported_portfolio+1):(length(Newspdata.wgs84$Name)+value1$imported_portfolio)] <<-as.character(Newspdata.wgs84$Name)
            Newspdata.wgs84<-Newspdata.wgs84[,c("Name","geometry")]
          }
          else{
            
            Newspdata.wgs84$Name<-proplist[(value1$imported_portfolio+1):(length(Newspdata.wgs84$geometry)+value1$imported_portfolio)]
            Newspdata.wgs84<-Newspdata.wgs84[,c("Name","geometry")]
          }
          value1$imported_portfolio<-value1$imported_portfolio+length(Newspdata.wgs84$geometry)
          
          ps_list_portfolio_import$result<-rbind(ps_list_portfolio_import$result,Newspdata.wgs84)
          leafletProxy("map")%>%
            fitBounds(as.numeric(boundary$xmin), as.numeric(boundary$ymin), as.numeric(boundary$xmax), as.numeric(boundary$ymax))%>%
            addPolygons(data = Newspdata.wgs84,color = "#e87f17", weight = 1)
          ##print(length(ps_list_import$result$geometry))
          unlink(traildirectory, recursive = T)
        }}
        
    }
    value1$test_portfolio2<-0
    }
    
    })
  
  
  observeEvent(input$trail,{
    
    if(value1$lastinput==1){
      showModal(div(id="ModalDiv", modalDialog(
        title = "There are areas of interest captured.",
        "Please use the Startover Feature to input others.",
        footer = tagList(modalButton("OK"))
      )))
    }
    else{
    if(length(ps_list_import$result$geometry)==1){
      showModal(div(id="ModalDiv", modalDialog(
        title = "Only one spatial footprint detected.",
        paste0("Please upload more spatial footprints for area of interests."),
        footer = tagList(modalButton("Ok"))
      )))
    }else{
    if(length(ps_list_import$result$geometry)>0){
    value1$test14<-10
    for (i in 1:length(ps_list_import$result$geometry)) {
      ps_list_import$result_1[[i]]<-ps_list_import$result[i,]
    }
    updateTabsetPanel(session = session, inputId = "tabs", "dataoverview")
    }
    
      else{
        showModal(div(id="ModalDiv", modalDialog(
          title = "No spatial footprint detected.",
          paste0("Please upload spatial footprint for area of interest."),
          footer = tagList(modalButton("Ok"))
        )))
      }
    }
    }
  })
  
  observeEvent(input$trail_portfolio,{
    if(length(ps_list_portfolio_import$result$geometry)>0){    
    value1$test_portfolio1<-10
    for (i in 1:length(ps_list_portfolio_import$result$geometry)) {
      ps_list_portfolio_import$result_1[[i]]<-ps_list_portfolio_import$result[i,]
    }
    updateTabsetPanel(session = session, inputId = "tabs", "portfoliooverview")
    }
    else{
      showModal(div(id="ModalDiv", modalDialog(
        title = "No spatial footprint detected.",
        paste0("Please upload spatial footprint for area of interest."),
        footer = tagList(modalButton("Ok"))
      )))
    }
  })
  
  
  observe({
    if(value1$test14==10){
      withProgress(message = 'Processing...', value = 0, {
        #ps_list_import$result_1<-st_zm(ps_list_import$result_1,drop= TRUE, what = "ZM")
        ps_list$result<-ps_list_import$result_1
        incProgress(0.2, detail = paste("Saving spatial data"))  
        ##print(ps_list_import$result_1)
        ##print(class(ps_list_import$result_1))
        for(i in 1:length(ps_list_import$result_1)){
          data1<- ps_list_import$result_1[[i]]
          ##print(data1)
          ##print(class(data1))
          join_result<-st_join(data1, Hex_tile, join = st_intersects)
          ps_list$hex[[i]]<- join_result$Id-1
          ##print(join_result$Id)
          ps_list$hex_final<-c(ps_list$hex_final,ps_list$hex[[i]])
          leafletProxy("map",data = data1)%>%
            addPolygons(color = "#66ff66", weight = 1)
        }
        ##print(length(ps_list$hex_final))
        ps_list$hex_merge<-st_read("./hex_details2/geojson/1.geojson")
        ps_list$hex_merge<-st_transform(ps_list$hex_merge, crs=4326)
        ##print("Hexmerge!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
        ##print(ps_list$hex_merge[[1]])
        incProgress(0.2, detail = paste("Connecting database"))
        dir <- file.path(".", "hex_details2","geojson")
        ##print(ps_list$hex_final)
        for(i in 1:length(ps_list$hex_final)){
          temp<-paste(c(as.character(ps_list$hex_final[i]),".geojson"),collapse ="")
          temp1<- st_read(dsn = file.path(dir,temp))
          temp1<- st_transform(temp1, crs=4326 )
          ##print(length(temp1$OBJECTID))
          ps_list$hex_merge<- rbind(ps_list$hex_merge,temp1)
          ##print(length(ps_list$hex_merge$OBJECTID))
          ps_list$hex_merge<-ps_list$hex_merge[match(unique(ps_list$hex_merge$OBJECTID),ps_list$hex_merge$OBJECTID),]
          
        }
        ##print(ps_list$hex_merge)
        #plot(st_geometry(ps_list$hex_merge))
        ##print("hey,hex_merge_finish")
        incProgress(0.2, detail = paste("Performing spatial analysis"))
        for(i in 1:length(ps_list_import$result_1)){
          data1<- ps_list_import$result_1[[i]]
          ##print(data1)
          ##print("ps_list$hex_merge")
          ##print(ps_list$hex_merge)
          data2<-st_join(ps_list$hex_merge,data1,join=st_intersects,left=F)
          names(data2)[1]<-paste("OBJECTID")
          ##print(length(data2))
          
          data2$appid<-i
          result$datatable[[i]]<-c(as.numeric(st_area(data1)/1000000),max(data2$PADUS2),sum(data2$area_conne),1-max(data2$Sleuth_v2),sum(data2$conl_index),sum(data2$area_12_13),mean(data2$wq3),(sum(data2$wq4)/length(data2$wq4))*100,mean(data2[data2$wq5>-1,]$wq5)*100,mean(data2[data2$wq6>-1,]$wq6),max(data2$Index_cpt_),sum(data2$PA1),max(as.numeric(as.character(data2$statuscoun))),min(data2$area_light),max(as.numeric(as.character(data2$Join_Cou_2))),sum(data2$area_nha),max(data2$SOVInew),max(data2$THREATINDE),sum(data2$WORKINGLAN),max(data2$ComEng_ct),max(data2$RecEng_ct),max(data2$AR_boat))
          ##print(result$datatable[[i]])
          ##print(class(result$datatable[[i]]))
          result$datatable_context[[i]]<-c(length(data2$area_conne))
          result$datatable[[i]][3]<-result$datatable[[i]][3]/result$datatable_context[[i]][1]*100
          result$datatable[[i]][5]<-result$datatable[[i]][5]/result$datatable_context[[i]][1]*100 
          result$datatable[[i]][6]<-result$datatable[[i]][6]/result$datatable_context[[i]][1]*100 
          result$datatable[[i]][12]<-result$datatable[[i]][12]/result$datatable_context[[i]][1]*100 
          result$datatable[[i]][16]<-result$datatable[[i]][16]/result$datatable_context[[i]][1]*100 
          result$datatable[[i]][19]<-result$datatable[[i]][19]/result$datatable_context[[i]][1]*100 
          ##print(max(result$datatable[[i]]))
          ##print(result$datatable[[i]]/max(result$datatable[[i]]))
          if(is.null(ps_list$hex_merge_final)){
            ps_list$hex_merge_final<- data2
          }else{ps_list$hex_merge_final<-rbind(ps_list$hex_merge_final,data2)}
          ##print("ps_list$hex_merge_final")
          ##print(ps_list$hex_merge_final)
          if(is.null(result$matrix)){result$matrix<-result$datatable[[i]]}
          else{result$matrix<-cbind(result$matrix,result$datatable[[i]])}
          
        }
        result$matrix_raw<-result$matrix
        ConservArea<-result$matrix[1,]
        incProgress(0.2, detail = paste("Calculating matrix "))
        #Maximum area of 303D, 
        #to be used in calculating utility for Impaired watershed
        
        
        #COMPREHENSIVE FOR LOOP
        for (i in 1:ncol(result$matrix)){
          #1. Threat of Urbanization
          result$matrix[4,i]<-result$matrix[4,i]
          #2. Connectivity with PAD-US
          result$matrix[2,i]<-result$matrix[2,i]
          #3. Connectivity of Natural Lands 
          result$matrix[3,i]<-result$matrix[3,i]/100
          #4. Proposed Area of Conservation
          result$matrix[1,i]<-ifelse(result$matrix[1,i]==0,0,
                                     ifelse(result$matrix[1,i]<=.4,.3,
                                            ifelse(result$matrix[1,i]<=.8,.75,
                                                   ifelse(result$matrix[1,i]<=2,.9,1))))
          #5. Composition of Natural Lands
          result$matrix[5,i]<-result$matrix[5,i]/100
          #6. 303D - Impaired watershed area
          result$matrix[6,i]<-(result$matrix[6,i]/100) 
          #7. Hydrologic Response to Land-Use Change
          result$matrix[7,i]<-result$matrix[7,i]
          #8. Percent of irrigated agriculture
          result$matrix[8,i]<-result$matrix[8,i]/100
          #9. Lateral Connectivity of Floodplain
          result$matrix[9,i]<-result$matrix[9,i]/100
          #10. Composition of Riparian Zone Lands
          result$matrix[10,i]<-result$matrix[10,i]
          
          #11. Biodiversity Index
          result$matrix[11,i]<-result$matrix[11,i]/10
          #12. T&E Species Area
          result$matrix[12,i]<-ifelse(result$matrix[12,i]<=0.001,0,
                                      ifelse(result$matrix[12,i]<=20,.75,
                                             ifelse(result$matrix[12,i]<=60,.9,1)))
          #13. T&E Number of Species
          result$matrix[13,i]<-ifelse(result$matrix[13,i]==0,0,
                                      ifelse(result$matrix[13,i]==1,.9,
                                             ifelse(result$matrix[1,i]==2,.95,1)))
          #14. Light Pollution Index
          #if higher the better
          #result$matrix[9,i]<-result$matrix[9,i]
          #if lower the better
          result$matrix[14,i]<-1-result$matrix[14,i]
          
          #15. National Register of Historic Places
          result$matrix[15,i]<-ifelse(result$matrix[15,i]==0,0,
                                      ifelse(result$matrix[15,i]==1,.75,
                                             ifelse(result$matrix[15,i]==2,.9,1)))
          #16. National Heritage Area
          result$matrix[16,i]<-result$matrix[16,i]/100
          
          #17
          result$matrix[17,i]<-result$matrix[17,i]
          #18
          result$matrix[18,i]<-result$matrix[18,i]
          #19. Working Lands - High Priority
          result$matrix[19,i]<-result$matrix[19,i]/100
          #20. Commercial Fisheries - Engagement
          result$matrix[20,i]<-ifelse(result$matrix[20,i]==0,0,
                                      ifelse(result$matrix[20,i]==1,.25,
                                             ifelse(result$matrix[20,i]==2,.5,
                                                    ifelse(result$matrix[20,i]==3,.75,1))))
          #21. Recreational Fisheries - Engagement
          result$matrix[21,i]<-ifelse(result$matrix[21,i]==0,0,
                                      ifelse(result$matrix[21,i]==1,.25,
                                             ifelse(result$matrix[21,i]==2,.5,
                                                    ifelse(result$matrix[21,i]==3,.75,1))))
          #22. Access &Recreation
          result$matrix[22,i]<-ifelse(result$matrix[22,i]==0,0,
                                      ifelse(result$matrix[22,i]<=5,.25,
                                             ifelse(result$matrix[22,i]<=10,.75,
                                                    ifelse(result$matrix[22,i]<=15,.9,1))))
        }
        
        if(max(result$matrix[7,])==0){
          max7<-1
        }else{
          max7<-max(result$matrix[7,])}
        
        result$matrix[7,]<-result$matrix[7,]/max7
        
        if(max(result$matrix[3,])==0){
          max3<-1
        }else{
          max3<-max(result$matrix[3,])}
        result$matrix[3,]<-result$matrix[3,]/max3
        
        if(max(result$matrix[5,])==0){
          max5<-1
        }else{
          max5<-max(result$matrix[5,])}
        result$matrix[5,]<-result$matrix[5,]/max5
        
        if(max(result$matrix[16,])==0){
          max16<-1
        }else{
          max16<-max(result$matrix[16,])}
        result$matrix[16,]<-result$matrix[16,]/max16
        
        if(max(result$matrix[19,])==0){
          max19<-1
        }else{
          max19<-max(result$matrix[19,])}
        result$matrix[19,]<-result$matrix[19,]/max19
        
        rownames(result$matrix) <- coln[1:nrow(result$matrix)]
        result$showing_matrix<-round(result$matrix,2)
        #print(result$showing_matrix)
        #print("result$showing_matrix")
        rownames(result$matrix_raw)<-coln[1:nrow(result$matrix_raw)]
        result$showing_matrix_raw<-round(result$matrix_raw,2)
        colnames(result$showing_matrix)<-proplist[1:ncol(result$showing_matrix)]
        colnames(result$showing_matrix_raw)<-proplist[1:ncol(result$showing_matrix_raw)]
        result$rankaccept_altlist<-proplist[1:ncol(result$showing_matrix)]
        incProgress(0.2, detail = paste("Finished"))
      })
      show(selector = "#nav li a[data-value=MCDA]")
      updateTabsetPanel(session = session, inputId = "nav", "MCDA")
      value1$test14<-0
      value1$lastinput<-1
    }
  })
  
  observe({
    if(value1$test_portfolio1==10){
      withProgress(message = 'Processing...', value = 0, {
        ps_list_portfolio$result<-ps_list_portfolio_import$result_1
        incProgress(0.2, detail = paste("Saving spatial data"))  
        ##print(ps_list_portfolio_import$result_1)
        ##print(class(ps_list_portfolio_import$result_1))
        for(i in 1:length(ps_list_portfolio_import$result_1)){
          data1<- ps_list_portfolio_import$result_1[[i]]
          ##print(data1)
          ##print(class(data1))
          join_result<-st_join(data1, Hex_tile, join = st_intersects)
          ps_list$hex[[i]]<- join_result$Id-1
          ##print(join_result$Id)
          ps_list$hex_final<-c(ps_list$hex_final,ps_list$hex[[i]])
          leafletProxy("map",data = data1)%>%
            addPolygons(color = "#66ff66", weight = 1)
        }
        ##print(length(ps_list$hex_final))
        ps_list$hex_merge<-st_read("./hex_details2/geojson/1.geojson")
        ps_list$hex_merge<-st_transform(ps_list$hex_merge, crs=4326)
        ##print("Hexmerge!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
        ##print(ps_list$hex_merge[[1]])
        incProgress(0.2, detail = paste("Connecting database"))
        dir <- file.path(".", "hex_details2","geojson")
        ##print(ps_list$hex_final)
        for(i in 1:length(ps_list$hex_final)){
          temp<-paste(c(as.character(ps_list$hex_final[i]),".geojson"),collapse ="")
          temp1<- st_read(dsn = file.path(dir,temp))
          temp1<- st_transform(temp1, crs=4326 )
          ##print(length(temp1$OBJECTID))
          ps_list$hex_merge<- rbind(ps_list$hex_merge,temp1)
          ##print(length(ps_list$hex_merge$OBJECTID))
          ps_list$hex_merge<-ps_list$hex_merge[match(unique(ps_list$hex_merge$OBJECTID),ps_list$hex_merge$OBJECTID),]
          
        }
        #print(ps_list$hex_merge)
        #print("ps_list_portfolio_import$result_1[[1]]")
        ##print(ps_list$hex_merge)
        #plot(st_geometry(ps_list$hex_merge))
        ##print("hey,hex_merge_finish")
        incProgress(0.2, detail = paste("Performing spatial analysis"))
        for(i in 1:length(ps_list_portfolio_import$result_1)){
          data1<- ps_list_portfolio_import$result_1[[i]]
          ##print(data1)
          ##print("ps_list$hex_merge")
          ##print(ps_list$hex_merge)
          data2<-st_join(ps_list$hex_merge,data1,join=st_intersects,left=F)
          names(data2)[1]<-paste("OBJECTID")
          ##print(length(data2))
          
          data2$appid<-i
          result$datatable_portfolio[[i]]<-c(as.numeric(st_area(data1)/1000000),max(data2$PADUS2),sum(data2$area_conne),1-max(data2$Sleuth_v2),sum(data2$conl_index),sum(data2$area_12_13),mean(data2$wq3),(sum(data2$wq4)/length(data2$wq4))*100,mean(data2[data2$wq5>-1,]$wq5)*100,mean(data2[data2$wq6>-1,]$wq6),max(data2$Index_cpt_),sum(data2$PA1),max(as.numeric(as.character(data2$statuscoun))),min(data2$area_light),max(as.numeric(as.character(data2$Join_Cou_2))),sum(data2$area_nha),max(data2$SOVInew),max(data2$THREATINDE),sum(data2$WORKINGLAN),max(data2$ComEng_ct),max(data2$RecEng_ct),max(data2$AR_boat))
          ##print(result$datatable[[i]])
          ##print(class(result$datatable[[i]]))
          result$datatable_portfolio_context[[i]]<-c(length(data2$area_conne))
          result$datatable_portfolio[[i]][3]<-result$datatable_portfolio[[i]][3]/result$datatable_portfolio_context[[i]][1]*result$datatable_portfolio[[i]][4]
          result$datatable_portfolio[[i]][5]<-result$datatable_portfolio[[i]][5]/result$datatable_portfolio_context[[i]][1]*result$datatable_portfolio[[i]][4]
          result$datatable_portfolio[[i]][6]<-result$datatable_portfolio[[i]][6]/result$datatable_portfolio_context[[i]][1]*result$datatable_portfolio[[i]][4]
          result$datatable_portfolio[[i]][12]<-result$datatable_portfolio[[i]][12]/result$datatable_portfolio_context[[i]][1]*result$datatable_portfolio[[i]][4]
          result$datatable_portfolio[[i]][16]<-result$datatable_portfolio[[i]][16]/result$datatable_portfolio_context[[i]][1]*result$datatable_portfolio[[i]][4]
          result$datatable_portfolio[[i]][19]<-result$datatable_portfolio[[i]][19]/result$datatable_portfolio_context[[i]][1]*result$datatable_portfolio[[i]][4]
          
          #print(result$datatable_portfolio)
          ##print(result$datatable[[i]]/max(result$datatable[[i]]))
          if(is.null(ps_list$hex_merge_final)){
            ps_list$hex_merge_final<- data2
          }else{ps_list$hex_merge_final<-rbind(ps_list$hex_merge_final,data2)}
          ##print("ps_list$hex_merge_final")
          ##print(ps_list$hex_merge_final)
          if(is.null(result$matrix_portfolio)){result$matrix_portfolio<-result$datatable_portfolio[[i]]}
          else{result$matrix_portfolio<-cbind(result$matrix_portfolio,result$datatable_portfolio[[i]])}
          
        }
        result$matrix_portfolio_raw<-result$matrix_portfolio
        #print(result$matrix_portfolio_raw)
        ConservArea<-result$matrix_portfolio[1,]
        incProgress(0.2, detail = paste("Calculating matrix "))
        #Maximum area of 303D, 
        #to be used in calculating utility for Impaired watershed
        
        
        #COMPREHENSIVE FOR LOOP
        for (i in 1:ncol(result$matrix_portfolio)){
          #1. Threat of Urbanization
          result$matrix_portfolio[4,i]<-result$matrix_portfolio[4,i]
          #2. Connectivity with PAD-US
          result$matrix_portfolio[2,i]<-result$matrix_portfolio[2,i]
          #3. Connectivity of Natural Lands 
          result$matrix_portfolio[3,i]<-result$matrix_portfolio[3,i]/100
          #4. Proposed Area of Conservation
          result$matrix_portfolio[1,i]<-ifelse(result$matrix_portfolio[1,i]==0,0,
                                               ifelse(result$matrix_portfolio[1,i]<=.4,.3,
                                                      ifelse(result$matrix_portfolio[1,i]<=.8,.75,
                                                             ifelse(result$matrix_portfolio[1,i]<=2,.9,1))))
          #5. Composition of Natural Lands
          result$matrix_portfolio[5,i]<-result$matrix_portfolio[5,i]/100
          #6. 303D - Impaired watershed area
          result$matrix_portfolio[6,i]<-result$matrix_portfolio[6,i]/100 
          #7. Hydrologic Response to Land-Use Change
          result$matrix_portfolio[7,i]<-result$matrix_portfolio[7,i]
          #8. Percent of irrigated agriculture
          result$matrix_portfolio[8,i]<-result$matrix_portfolio[8,i]
          #9. Lateral Connectivity of Floodplain
          result$matrix_portfolio[9,i]<-result$matrix_portfolio[9,i]
          #10. Composition of Riparian Zone Lands
          result$matrix_portfolio[10,i]<-result$matrix_portfolio[10,i]
          
          #11. Biodiversity Index
          result$matrix_portfolio[11,i]<-result$matrix_portfolio[11,i]/10
          #12. T&E Species Area
          result$matrix_portfolio[12,i]<-ifelse(result$matrix_portfolio[12,i]<=0.001,0,
                                      ifelse(result$matrix_portfolio[12,i]<=20,.75,
                                             ifelse(result$matrix_portfolio[12,i]<=60,.9,1)))
          #13. T&E Number of Species
          result$matrix_portfolio[13,i]<-ifelse(result$matrix_portfolio[13,i]==0,0,
                                      ifelse(result$matrix_portfolio[13,i]==1,.9,
                                             ifelse(result$matrix_portfolio[1,i]==2,.95,1)))
          #14. Light Pollution Index
          #if higher the better
          #result$matrix_portfolio[9,i]<-result$matrix_portfolio[9,i]
          #if lower the better
          result$matrix_portfolio[14,i]<-1-result$matrix_portfolio[14,i]
          
          #15. National Register of Historic Places
          result$matrix_portfolio[15,i]<-ifelse(result$matrix_portfolio[15,i]==0,0,
                                      ifelse(result$matrix_portfolio[15,i]==1,.75,
                                             ifelse(result$matrix_portfolio[15,i]==2,.9,1)))
          #16. National Heritage Area
          result$matrix_portfolio[16,i]<-result$matrix_portfolio[16,i]/100
          
          #17
          result$matrix_portfolio[17,i]<-result$matrix_portfolio[17,i]
          #18
          result$matrix_portfolio[18,i]<-result$matrix_portfolio[18,i]
          #19. Working Lands - High Priority
          result$matrix_portfolio[19,i]<-result$matrix_portfolio[19,i]/100
          #20. Commercial Fisheries - Engagement
          result$matrix_portfolio[20,i]<-ifelse(result$matrix_portfolio[20,i]==0,0,
                                      ifelse(result$matrix_portfolio[20,i]==1,.25,
                                             ifelse(result$matrix_portfolio[20,i]==2,.5,
                                                    ifelse(result$matrix_portfolio[20,i]==3,.75,1))))
          #21. Recreational Fisheries - Engagement
          result$matrix_portfolio[21,i]<-ifelse(result$matrix_portfolio[21,i]==0,0,
                                      ifelse(result$matrix_portfolio[21,i]==1,.25,
                                             ifelse(result$matrix_portfolio[21,i]==2,.5,
                                                    ifelse(result$matrix_portfolio[21,i]==3,.75,1))))
          #22. Access &Recreation
          result$matrix_portfolio[22,i]<-ifelse(result$matrix_portfolio[22,i]==0,0,
                                      ifelse(result$matrix_portfolio[22,i]<=5,.25,
                                             ifelse(result$matrix_portfolio[22,i]<=10,.75,
                                                    ifelse(result$matrix_portfolio[22,i]<=15,.9,1))))  
        }
        
        if(max(result$matrix_portfolio[7,])==0){
          max7<-1
        }else{
          max7<-max(result$matrix_portfolio[7,])}
        
        if(max(result$matrix_portfolio[3,])==0){
          max3<-1
        }else{
          max3<-max(result$matrix_portfolio[3,])}
        result$matrix_portfolio[3,]<-result$matrix_portfolio[3,]/max3
        
        if(max(result$matrix_portfolio[5,])==0){
          max5<-1
        }else{
          max5<-max(result$matrix_portfolio[5,])}
        result$matrix_portfolio[5,]<-result$matrix_portfolio[5,]/max5
        
        if(max(result$matrix_portfolio[16,])==0){
          max16<-1
        }else{
          max16<-max(result$matrix_portfolio[16,])}
        result$matrix_portfolio[16,]<-result$matrix_portfolio[16,]/max16
        
        if(max(result$matrix_portfolio[19,])==0){
          max19<-1
        }else{
          max19<-max(result$matrix_portfolio[19,])}
        result$matrix_portfolio[19,]<-result$matrix_portfolio[19,]/max19
        
        
        result$matrix_portfolio[7,]<-result$matrix_portfolio[7,]/max7
        
        
        #print("result$matrix_portfolio")
        #print(result$matrix_portfolio)
        
        result$showing_matrix_portfolio<-round(result$matrix_portfolio,2)
        rownames(result$matrix_portfolio_raw)<-coln[1:nrow(result$matrix_portfolio_raw)]
        result$showing_matrix_portfolio_raw<-round(result$matrix_portfolio_raw,2)
        colnames(result$showing_matrix_portfolio_raw)<-proplist[1:ncol(result$showing_matrix_portfolio_raw)]
        incProgress(0.2, detail = paste("Finished"))
      })
      show(selector = "#nav li a[data-value=MCDA]")
      updateTabsetPanel(session = session, inputId = "nav", "MCDA")
      value1$test_portfolio1<-0
    }
  })
  
  output$watershed_uimcda<-renderUI({
    selectizeInput("watershed_mcda","Select a watershed",
                   choice, options = list(maxItems = input$threshold_watershed))
  })
  
  output$watershed_uimcda_id<-renderUI({
    selectizeInput("watershed_mcda_id","Select a watershed",
                   choiceid, options = list(maxItems = input$threshold_watershed))
  })
  
  observeEvent(input$finish_utility,{
    updateTabsetPanel(session = session, inputId = "viewdata", "rawdata")
  })
  
  observeEvent(input$selectingbutton,{
    
    if(value1$lastinput==1){
      showModal(div(id="ModalDiv", modalDialog(
        title = "There are areas of interest captured.",
        "Please use the Startover Feature to input others.",
        footer = tagList(modalButton("OK"))
      )))
    }
    else{
    if(length(input$watershed_mcda)==input$threshold_watershed | length(input$watershed_mcda_id)==input$threshold_watershed |
       nrow(HU12[which(value1$HU12color_mcda =="red"),c(1,2,3,8)])==input$threshold_watershed){
      value1$test15<-10
    show(selector = "#nav li a[data-value=MCDA]")
    updateTabsetPanel(session = session, inputId = "nav", "MCDA")
    updateTabsetPanel(session = session, inputId = "tabs", "dataoverview")
    if(input$way4=="name"){
     ps_list_select$result<-HU12[which(HU12$NAME %in% input$watershed_mcda),c(1,2,3,8)] 
    }
    else if(input$way4=="id"){
      ps_list_select$result<-HU12[which(HU12$HUC12 %in% input$watershed_mcda_id),c(1,2,3,8)] 
    }
    else if(input$way4=="boundary"){
      ps_list_select$result<-HU12[which(value1$HU12color_mcda =="red"),c(1,2,3,8)]
    }
    for(i in 1:input$threshold_watershed){
      ps_list_select$result_1[[i]]<-ps_list_select$result[i,]
      proplist[i]<<-as.character(ps_list_select$result[i,]$NAME)
    }
    }
    else{
      #print(nrow(HU12[which(value1$HU12color_mcda =="red"),c(1,2,3,8)]))
      showModal(modalDialog(
        title = "Incorrect number of area of interests found",footer = modalButton("Ok"),
        "Please adjust the number of watersheds selected."
      ))
    }
    }
  })
  
  observe({
    if(value1$test15==10){
      withProgress(message = 'Processing...', value = 0, {
        #ps_list_import$result_1<-st_zm(ps_list_import$result_1, drop= TRUE, what = "ZM")
        ps_list$result<-ps_list_select$result_1
        incProgress(0.2, detail = paste("Saving spatial data"))  
        ##print(ps_list_import$result_1)
        ##print(class(ps_list_import$result_1))
        for(i in 1:length(ps_list_select$result_1)){
          data1<- ps_list_select$result_1[[i]]
          ##print(data1)
          ##print(class(data1))
          join_result<-st_join(data1, Hex_tile, join = st_intersects)
          ps_list$hex[[i]]<- join_result$Id-1
          ##print(join_result$Id)
          ps_list$hex_final<-c(ps_list$hex_final,ps_list$hex[[i]])
          leafletProxy("map",data = data1)%>%
            addPolygons(color = "#66ff66", weight = 1)
        }
        ##print(length(ps_list$hex_final))
        ps_list$hex_merge<-st_read("./hex_details2/geojson/1.geojson")
        ps_list$hex_merge<-st_transform(ps_list$hex_merge, crs=4326)
        ##print("Hexmerge!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
        ##print(ps_list$hex_merge[[1]])
        incProgress(0.2, detail = paste("Connecting database"))
        dir <- file.path(".", "hex_details2","geojson")
        ##print(ps_list$hex_final)
        for(i in 1:length(ps_list$hex_final)){
          temp<-paste(c(as.character(ps_list$hex_final[i]),".geojson"),collapse ="")
          temp1<- st_read(dsn = file.path(dir,temp))
          temp1<- st_transform(temp1, crs=4326 )
          ##print(length(temp1$OBJECTID))
          ps_list$hex_merge<- rbind(ps_list$hex_merge,temp1)
          ##print(length(ps_list$hex_merge$OBJECTID))
          ps_list$hex_merge<-ps_list$hex_merge[match(unique(ps_list$hex_merge$OBJECTID),ps_list$hex_merge$OBJECTID),]
          
        }
        ##print(ps_list$hex_merge)
        #plot(st_geometry(ps_list$hex_merge))
        ##print("hey,hex_merge_finish")
        incProgress(0.2, detail = paste("Performing spatial analysis"))
        for(i in 1:length(ps_list_select$result_1)){
          data1<- ps_list_select$result_1[[i]]
          ##print(data1)
          ##print("ps_list$hex_merge")
          ##print(ps_list$hex_merge)
          data2<-st_join(ps_list$hex_merge,data1,join=st_intersects,left=F)
          names(data2)[1]<-paste("OBJECTID")
          ##print(length(data2))
          
          data2$appid<-i
          result$datatable[[i]]<-c(as.numeric(st_area(data1)/1000000),max(data2$PADUS2),sum(data2$area_conne),1-max(data2$Sleuth_v2),sum(data2$conl_index),sum(data2$area_12_13),mean(data2$wq3),(sum(data2$wq4)/length(data2$wq4))*100,mean(data2[data2$wq5>-1,]$wq5)*100,mean(data2[data2$wq6>-1,]$wq6),max(data2$Index_cpt_),sum(data2$PA1),max(as.numeric(as.character(data2$statuscoun))),min(data2$area_light),max(as.numeric(as.character(data2$Join_Cou_2))),sum(data2$area_nha),max(data2$SOVInew),max(data2$THREATINDE),sum(data2$WORKINGLAN),max(data2$ComEng_ct),max(data2$RecEng_ct),max(data2$AR_boat))
          ##print(result$datatable[[i]])
          ##print(class(result$datatable[[i]]))
          ##print(max(result$datatable[[i]]))
          ##print(result$datatable[[i]]/max(result$datatable[[i]]))
          result$datatable_context[[i]]<-c(length(data2$area_conne))
          result$datatable[[i]][3]<-result$datatable[[i]][3]/result$datatable_context[[i]][1]*100
          result$datatable[[i]][5]<-result$datatable[[i]][5]/result$datatable_context[[i]][1]*100
          result$datatable[[i]][6]<-result$datatable[[i]][6]/result$datatable_context[[i]][1]*100
          result$datatable[[i]][12]<-result$datatable[[i]][12]/result$datatable_context[[i]][1]*100
          result$datatable[[i]][16]<-result$datatable[[i]][16]/result$datatable_context[[i]][1]*100
          result$datatable[[i]][19]<-result$datatable[[i]][19]/result$datatable_context[[i]][1]*100
          if(is.null(ps_list$hex_merge_final)){
            ps_list$hex_merge_final<- data2
          }else{ps_list$hex_merge_final<-rbind(ps_list$hex_merge_final,data2)}
          ##print("ps_list$hex_merge_final")
          ##print(ps_list$hex_merge_final)
          if(is.null(result$matrix)){result$matrix<-result$datatable[[i]]}
          else{result$matrix<-cbind(result$matrix,result$datatable[[i]])}
          
        }
        
        
        
        result$matrix_raw<-result$matrix
        ConservArea<-result$matrix[1,]
        incProgress(0.2, detail = paste("Calculating matrix "))
        #Maximum area of 303D, 
        #to be used in calculating utility for Impaired watershed
        
        
        #COMPREHENSIVE FOR LOOP
        for (i in 1:ncol(result$matrix)){
          #1. Threat of Urbanization
          result$matrix[4,i]<-result$matrix[4,i]
          #2. Connectivity with PAD-US
          result$matrix[2,i]<-result$matrix[2,i]
          #3. Connectivity of Natural Lands 
          result$matrix[3,i]<-result$matrix[3,i]/100
          #4. Proposed Area of Conservation
          result$matrix[1,i]<-ifelse(result$matrix[1,i]==0,0,
                                     ifelse(result$matrix[1,i]<=.4,.3,
                                            ifelse(result$matrix[1,i]<=.8,.75,
                                                   ifelse(result$matrix[1,i]<=2,.9,1))))
          #5. Composition of Natural Lands
          result$matrix[5,i]<-result$matrix[5,i]/100
          #6. 303D - Impaired watershed area
          result$matrix[6,i]<-(result$matrix[6,i]/100) 
          #7. Hydrologic Response to Land-Use Change
          result$matrix[7,i]<-result$matrix[7,i]
          #8. Percent of irrigated agriculture
          result$matrix[8,i]<-result$matrix[8,i]/100
          #9. Lateral Connectivity of Floodplain
          result$matrix[9,i]<-result$matrix[9,i]/100
          #10. Composition of Riparian Zone Lands
          result$matrix[10,i]<-result$matrix[10,i]
          
          #11. Biodiversity Index
          result$matrix[11,i]<-result$matrix[11,i]/10
          #12. T&E Species Area
          result$matrix[12,i]<-ifelse(result$matrix[12,i]<=0.001,0,
                                      ifelse(result$matrix[12,i]<=20,.75,
                                             ifelse(result$matrix[12,i]<=60,.9,1)))
          #13. T&E Number of Species
          result$matrix[13,i]<-ifelse(result$matrix[13,i]==0,0,
                                      ifelse(result$matrix[13,i]==1,.9,
                                             ifelse(result$matrix[1,i]==2,.95,1)))
          #14. Light Pollution Index
          #if higher the better
          #result$matrix[9,i]<-result$matrix[9,i]
          #if lower the better
          result$matrix[14,i]<-1-result$matrix[14,i]
          
          #15. National Register of Historic Places
          result$matrix[15,i]<-ifelse(result$matrix[15,i]==0,0,
                                      ifelse(result$matrix[15,i]==1,.75,
                                             ifelse(result$matrix[15,i]==2,.9,1)))
          #16. National Heritage Area
          result$matrix[16,i]<-result$matrix[16,i]/100
          
          #17
          result$matrix[17,i]<-result$matrix[17,i]
          #18
          result$matrix[18,i]<-result$matrix[18,i]
          #19. Working Lands - High Priority
          result$matrix[19,i]<-result$matrix[19,i]/100
          #20. Commercial Fisheries - Engagement
          result$matrix[20,i]<-ifelse(result$matrix[20,i]==0,0,
                                      ifelse(result$matrix[20,i]==1,.25,
                                             ifelse(result$matrix[20,i]==2,.5,
                                                    ifelse(result$matrix[20,i]==3,.75,1))))
          #21. Recreational Fisheries - Engagement
          result$matrix[21,i]<-ifelse(result$matrix[21,i]==0,0,
                                      ifelse(result$matrix[21,i]==1,.25,
                                             ifelse(result$matrix[21,i]==2,.5,
                                                    ifelse(result$matrix[21,i]==3,.75,1))))
          #22. Access &Recreation
          result$matrix[22,i]<-ifelse(result$matrix[22,i]==0,0,
                                      ifelse(result$matrix[22,i]<=5,.25,
                                             ifelse(result$matrix[22,i]<=10,.75,
                                                    ifelse(result$matrix[22,i]<=15,.9,1)))) 
        }
        
        if(max(result$matrix[7,])==0){
          max7<-1
        }else{
          max7<-max(result$matrix[7,])}
        
        
        
        
        result$matrix[7,]<-result$matrix[7,]/max7
        
        if(max(result$matrix[3,])==0){
          max3<-1
        }else{
          max3<-max(result$matrix[3,])}
        result$matrix[3,]<-result$matrix[3,]/max3
        
        if(max(result$matrix[5,])==0){
          max5<-1
        }else{
          max5<-max(result$matrix[5,])}
        result$matrix[5,]<-result$matrix[5,]/max5
        
        if(max(result$matrix[16,])==0){
          max16<-1
        }else{
          max16<-max(result$matrix[16,])}
        result$matrix[16,]<-result$matrix[16,]/max16
        
        if(max(result$matrix[19,])==0){
          max19<-1
        }else{
          max19<-max(result$matrix[19,])}
        result$matrix[19,]<-result$matrix[19,]/max19
        
        rownames(result$matrix) <- coln[1:nrow(result$matrix)]
        result$showing_matrix<-round(result$matrix,2)
        rownames(result$matrix_raw)<-coln[1:nrow(result$matrix_raw)]
        result$showing_matrix_raw<-round(result$matrix_raw,2)
        colnames(result$showing_matrix)<-proplist[1:ncol(result$showing_matrix)]
        colnames(result$showing_matrix_raw)<-proplist[1:ncol(result$showing_matrix_raw)]
        result$rankaccept_altlist<-proplist[1:ncol(result$showing_matrix)]
        incProgress(0.2, detail = paste("Finished"))
      })
      value1$test15<-0
      value1$lastinput<-1
    }
  })
  
  observeEvent(input$file2, {
    value1$test20<-10
  })
  
  observe({
    if(value1$test20==10){
      inFile <- input$file2
      if (!is.null(inFile))
      { 
        traildirectory<-"./input"
        #print(traildirectory)
        file.copy(inFile$datapath,overwrite=TRUE, file.path(traildirectory, inFile$name))
        unzip(inFile$datapath,exdir = traildirectory)
        #print(list.files(traildirectory, pattern = "*.shp$"))
        listfile<-list.files(traildirectory)
        listfile<-grep('shp', listfile, value=TRUE)
        if(length(listfile)==0){
          listfile<-list.files(traildirectory)
          listfile<-grep('kml', listfile, value=TRUE)
        }
        if(length(listfile)==0){
          traildirectory<-"./input/download"
          listfile<-list.files(traildirectory)
          listfile<-grep('shp', listfile, value=TRUE)
        }
        
        
        if(length(listfile)!=0){
        Newspdata <- st_read(dsn = paste0(traildirectory,"/",listfile)
                             #, layer = substr(listfile,1,nchar(listfile)-4)
                             )
        #print(Newspdata)
        Newspdata.wgs84<- st_transform(Newspdata, crs = 4326)
        if(length(Newspdata.wgs84$geometry)!=1){
          showModal(div(id="ModalDiv", modalDialog(
            title = "Incorrect number of polygons in the shapefile",
            "Please revisit the shapefile to adjust the format.",
            footer = tagList(modalButton("OK"))
          )))
        }
        else{
        ##print(class(Newspdata.wgs84))
        boundary<-st_bbox(Newspdata.wgs84)
        if(length(Newspdata.wgs84$Name)>0 &length(Newspdata.wgs84$Name)<=10){
          proplist_os <<-as.character(Newspdata.wgs84$Name[1])
        }
        if(length(Newspdata.wgs84$Name)==0){
          proplist_os<<-"area of interest 1"
        }
        ps_list_os$import_result<-Newspdata.wgs84[1,]
        ##print(ps_list_os$import_result)
        leafletProxy("map")%>%
          fitBounds(as.numeric(boundary$xmin), as.numeric(boundary$ymin), as.numeric(boundary$xmax), as.numeric(boundary$ymax))%>%
          addPolygons(data = Newspdata.wgs84[1,],color = "#e87f17", weight = 1)
        ##print(length(ps_list_import$result$geometry))
        ##print(ps_list_import$result[1,])
        unlink(traildirectory, recursive = T)
        }
      }}
    }
    value1$test20<-0
  })
  
  observeEvent(input$ostrail,{
    if(length(ps_list_os$import_result$geometry)>0){
    value1$test16<-10
    ps_list_os$import_result<-st_zm(ps_list_os$import_result, drop = TRUE, what = "ZM")
    ps_list_os$result<-ps_list_os$import_result
    show(selector = "#nav li a[data-value=MCDA]")
    updateTabsetPanel(session = session, inputId = "nav", "MCDA")
    updateTabsetPanel(session = session, inputId = "tabs", "osdataoverview")
    }
    else{
      showModal(div(id="ModalDiv", modalDialog(
        title = "No spatial footprint detected.",
        paste0("Please upload spatial footprint for area of interest."),
        footer = tagList(modalButton("Ok"))
      )))
    }
  })
  
  
  
  observeEvent(input$oswatershedname,{
    ps_list_os$result<-HU12[which(HU12$NAME %in% input$watershed_os_name),c(1,2,3,8)]
    value1$test16<-10
    proplist_os<<-input$watershed_os_name
    show(selector = "#nav li a[data-value=MCDA]")
    updateTabsetPanel(session = session, inputId = "nav", "MCDA")
    updateTabsetPanel(session = session, inputId = "tabs", "osdataoverview")
  })
  observeEvent(input$oswatershedid,{
    ps_list_os$result<-HU12[which(HU12$HUC12 %in% input$watershed_os_id),c(1,2,3,8)]
    value1$test16<-10
    proplist_os<<-as.character(ps_list_os$result$NAME[1])
    show(selector = "#nav li a[data-value=MCDA]")
    updateTabsetPanel(session = session, inputId = "nav", "MCDA")
    updateTabsetPanel(session = session, inputId = "tabs", "osdataoverview")
  })
  observeEvent(input$oswatershedboundry,{
    ps_list_os$result<-HU12[which(value1$HU12color =="red"),c(1,2,3,8)]
    ps_list_os$result<-ps_list_os$result[1,]
    proplist_os<<-as.character(ps_list_os$result$NAME[1])
    value1$test16<-10
    show(selector = "#nav li a[data-value=MCDA]")
    updateTabsetPanel(session = session, inputId = "nav", "MCDA")
    updateTabsetPanel(session = session, inputId = "tabs", "osdataoverview")
  })
  observe({
    if(value1$test16==10){
      withProgress(message = 'Processing...', value = 0, {
        incProgress(0.2, detail = paste("Saving spatial data"))  
        
          data1<- ps_list_os$result
          join_result<-st_join(data1, Hex_tile, join = st_intersects)
          ps_list_os$hex<- join_result$Id-1
          ps_list_os$hex_final<-ps_list_os$hex
          leafletProxy("map",data = data1)%>%
            addPolygons(color = "#66ff66", weight = 1)

        ps_list_os$hex_merge<-st_read("./hex_details2/geojson/1.geojson")
        ps_list_os$hex_merge<-st_transform(ps_list_os$hex_merge, crs=4326)
        
       incProgress(0.2, detail = paste("Connecting database"))
        dir <- file.path(".", "hex_details2","geojson")
        for(i in 1:length(ps_list_os$hex_final)){
          temp<-paste(c(as.character(ps_list_os$hex_final[[i]]),".geojson"),collapse ="")
          temp1<- st_read(dsn = file.path(dir,temp))
          temp1<- st_transform(temp1, crs=4326 )
          #print(length(temp1$OBJECTID))
          ps_list_os$hex_merge<- rbind(ps_list_os$hex_merge,temp1)
          ps_list_os$hex_merge<-ps_list_os$hex_merge[match(unique(ps_list_os$hex_merge$OBJECTID),ps_list_os$hex_merge$OBJECTID),]
          
        }
        
        incProgress(0.2, detail = paste("Performing spatial analysis"))

          data2<-st_join(ps_list_os$hex_merge,data1,join=st_intersects,left=F)
          names(data2)[1]<-paste("OBJECTID")

          result_os$datatable<-c(as.numeric(as.numeric(st_area(data1)/1000000)),max(data2$PADUS2),sum(data2$area_conne),1-max(data2$Sleuth_v2),sum(data2$conl_index),sum(data2$area_12_13),mean(data2$wq3),(sum(data2$wq4)/length(data2$wq4))*100,mean(data2[data2$wq5>-1,]$wq5)*100,mean(data2[data2$wq6>-1,]$wq6),max(data2$Index_cpt_),sum(data2$PA1),max(as.numeric(as.character(data2$statuscoun))),min(data2$area_light),max(as.numeric(as.character(data2$Join_Cou_2))),sum(data2$area_nha),max(data2$SOVInew),max(data2$THREATINDE),sum(data2$WORKINGLAN),max(data2$ComEng_ct),max(data2$RecEng_ct),max(data2$AR_boat))
          result_os$datatable_context<-c(sum(data2$metal_cuzo),min(data2$X303d_desig),length(data2$conl_index))
          tmpworking<-workingland3[which(workingland3$OBJECTID %in% data2$OBJECTID),]
          tmpte<-TE_index[which(TE_index$OBJECTID %in% data2$OBJECTID),]
          tmplanduse<-landuse[which(landuse$objectid %in% data2$OBJECTID),]
          
          if(length(tmpte$OBJECTID)==0){
            tmpte <- 0
          }else{
            tmpte<-paste0(tmpte$SPPCodeNew,collapse="")
            tmpte<-substring(tmpte,seq(1,nchar(tmpte)-3,4),seq(4,nchar(tmpte),4))
            tmpte<-unique(tmpte)
            tmpte<-TE_Code_Name[which(TE_Code_Name$SPCODE %in% tmpte),]
            tmpte<-paste0(tmpte$COMNAME,collapse = ",") 
            #tmpte<-c(tmpte$COMNAME)
            #print(tmpte)
          }
          if(length(tmpworking$OBJECTID)==0){
            tmpworkingsum<-0
            tmpworkingclass<-0
          }else{
            tmpworkingsum<-mean(tmpworking$sum)
            tmpworkingclass<-3
            tmpworkingcode<-as.character(tmpworking$code_safe)
            
            tmpworkingcode<-substr(tmpworkingcode,2,13)
            
            
            Evergreen<- paste0(substr(tmpworkingcode,1,3),".",substr(tmpworkingcode,4,4))
            Cropland<- paste0(substr(tmpworkingcode,5,7), ".", substr(tmpworkingcode,8,8))
            Pasture<- paste0(substr(tmpworkingcode,9,11), ".", substr(tmpworkingcode,12,12))
            
            #convert the 3 columns to numeric
            Evergreen<- as.numeric(Evergreen)
            Cropland<- as.numeric(Cropland)
            Pasture<-as.numeric(Pasture)
            
            #take the average for each of the 3 columns
            avg_evergreen<- round(sum(Evergreen)/length(Evergreen), digits = 1)
            avg_cropland<- round(sum(Cropland)/length(Cropland), digits = 1)
            avg_pasture<- round(sum(Pasture)/length(Pasture), digits = 1)
            
            #put the averages in a vector
            vector_avg<-paste0(avg_evergreen,",",avg_cropland,",",avg_pasture)
            sum_avg<-avg_evergreen + avg_cropland + avg_pasture
            #print(vector_avg)
            #print(sum_avg)
            if(sum(tmpworking$p1)==0){
              tmpworkingclass<-tmpworkingclass-1
            }
            if(sum(tmpworking$p2)==0){
              tmpworkingclass<-tmpworkingclass-1
            }
            if(sum(tmpworking$p3)==0){
              tmpworkingclass<-tmpworkingclass-1
            }
            
          }
          
          
          if(length(tmplanduse$objectid)==0){
            tmplanduse<-0
          }else{
            tmplanduse<-paste0(tmplanduse$classindex,collapse="")
            tmplanduse<-substring(tmplanduse,seq(1,nchar(tmplanduse)-3,4),seq(4,nchar(tmplanduse),4))
            tmplanduse<-unique(tmplanduse)
            tmplanduse<-length(tmplanduse)
          }
          
          result_os$datatable_additional<-c(tmpworkingsum,tmpte,tmpworkingclass,tmplanduse,vector_avg,sum_avg)
          ##print(result_os$datatable_context[3])
          ##print("result_os$datatable_context")
          result_os$matrix_context<-as.matrix(result_os$datatable_context)
            ps_list_os$hex_merge_final<- data2

            result_os$matrix<-as.matrix(result_os$datatable)
            result_os$showing_matrix<-round(result_os$matrix,2)
          result_os$showing_matrix[3]<-round(result_os$showing_matrix[3]/result_os$datatable_context[3]*100,0)
          result_os$showing_matrix[5]<-round(result_os$showing_matrix[5]/result_os$datatable_context[3]*100,0)
          result_os$showing_matrix[6]<-round(result_os$showing_matrix[6]/result_os$datatable_context[3]*100,0)
          result_os$showing_matrix[12]<-round(result_os$showing_matrix[12]/result_os$datatable_context[3]*100,0)
          result_os$showing_matrix[16]<-round(result_os$showing_matrix[16]/result_os$datatable_context[3]*100,0)
          result_os$showing_matrix[19]<-round(result_os$showing_matrix[19]/result_os$datatable_context[3]*100,0)
          
        incProgress(0.2, detail = paste("Calculating matrix "))

        
        rownames(result_os$matrix) <- coln

         colnames(result_os$showing_matrix)<-proplist_os

        incProgress(0.2, detail = paste("Finished"))
      })
      value1$test16<-0
    }
  })
  
  output$showing_matrix_os<-DT::renderDataTable({
    
    
    
    coln_withicon<-paste0(coln)

    data<-result_os$showing_matrix
    data[1]<-format(round(data[1]*247.105,0),scientific = F,trim = T,big.mark = ",")
    data[1]<-paste0(data[1]," acres")
    data[3]<-paste0(data[3],"%")
    data[5]<-paste0(data[5],"%")
    data[6]<-paste0(data[6],"%")
    data[8]<-paste0(data[8],"%")
    data[9]<-paste0(data[9],"%")
    data[12]<-paste0(data[12],"%")
    data[16]<-paste0(data[16],"%")
    data[19]<-paste0(data[19],"%")
    
    if(data[4]==0){
      data[4]<-"Currently Urbanized"
    }
    else if(data[4]>0.67){
      data[4]<-"Low"
    }else if (data[4]>0.33){
      data[4]<-"Medium"
    }else if (data[4]>0){
      data[4]<-"High"
    }else{
      data[4]<-"No Threat"
    }
    
    if(data[2]>0){
      data[2]<-"Yes"
    }else{
      data[2]<-"No"
    }
    
    if(data[11]>6){
      data[11]<-"High"
    }else if (data[11]>3){
      data[11]<-"Medium"
    }else if (data[11]>0){
      data[11]<-"Low"
    }else{
      data[11]<-"NA"
    }
    
    if(data[14]>0.66){
      data[14]<-"High"
    }else if (data[14]>0.33){
      data[14]<-"Medium"
    }else if (data[14]>0){
      data[14]<-"Low"
    }else{
      data[14]<-"No Light Pollution"
    }
    
    if(data[20]>3){
      data[20]<-"High"
    }else if (data[20]>2){
      data[20]<-"Medium-High"
    }else if (data[20]>1){
      data[20]<-"Medium"
    }else if (data[20]>0){
      data[20]<-"Low"
    }else{
      data[20]<-"Insufficient data"
    }
    
    if(data[21]>3){
      data[21]<-"High"
    }else if (data[21]>2){
      data[21]<-"Medium-High"
    }else if (data[21]>1){
      data[21]<-"Medium"
    }else if (data[21]>0){
      data[21]<-"Low"
    }else{
      data[21]<-"Insufficient data"
    }
    
    if(data[17]>=1){
      data[17]<-"Yes"
    }else if (data[17]>=0.75){
      data[17]<-"Medium-High"
    }else if (data[17]>=0.5){
      data[17]<-"Medium"
    }else if (data[17]>=0.25){
      data[17]<-"Medium-Low"
    }else if (data[17]>0){
      data[17]<-"Low"
    }
    else{
      data[17]<-"No"
    }
    
    if(data[18]>=1){
      data[18]<-"High"
    }else if (data[18]>=0.75){
      data[18]<-"Medium-High"
    }else if (data[18]>=0.5){
      data[18]<-"Medium"
    }else if (data[18]>=0.25){
      data[18]<-"Medium-Low"
    }else if (data[18]>0){
      data[18]<-"Low"
    }
    else{
      data[18]<-"Insufficient data"
    }
    
    
    data <-as.data.frame(data)
    rownames(data)<-coln_withicon
    data<-cbind('url'=url,data)
    data<-cbind('Goal'=goal,data)
    data<-cbind('Description'=descript,data)
    unitattr<-c("Acres","Index", "Percentage","Index","Percentage","Percentage","Index","Percentage","Percentage","Index","Index","Percentage","Count","0-1 Index","Count","Percentage","Index","Index","Percentage","Index","Index","Count")                      
    p<-datatable(
      cbind(' ' = paste0(rownames(data),'&#9432;',"  (",unitattr,")"), data), escape = -2,extensions = 'RowGroup',
      options = list(searching = FALSE,paging = FALSE,rowGroup = list(dataSrc = 3),
                     columnDefs = list(
                       list(visible = FALSE, targets = c(0, 2, 3, 4)),
                       list(orderable = FALSE, className = 'details-control', targets = 1)
                     )
      ),
      callback = JS("
                table.column(1).nodes().to$().css({cursor: 'pointer'});
                var format = function(d) {
                return '<div style=\"background-color:#eee; padding: .5em;\"> RESTORE Goal addressed: ' + d[3] + '</div>'+
                '<div style=\"background-color:#eee; padding: .5em;\"> Description: ' + d[2] + '</div>'+
                '<div style=\"background-color:#eee; padding: .5em;\"> <a href='+d[4]+' style=\"font-weight: bolder;\" target=\"_blank\"> Learn More </a>' + '</div>';
                };
                table.on('click', 'td.details-control', function() {
                var td = $(this), row = table.row(td.closest('tr'));
                if (row.child.isShown()) {
                row.child.hide();
                } else {
                row.child(format(row.data())).show();
                }
                });"
      ))%>%
      formatStyle(c(5),
                  color = styleEqual(
                    c("Low","Medium-Low","Medium","Medium-High","High","Insufficient data","Yes","No","Currently Urbanized","No Threat","No Light Pollution"),c('red','pink','blue','lightgreen','green','orange',"green",'red','red','green','green')
                  )
                  )
    p
    
  }, server = FALSE)
  
  
  output$showing_matrix_portfolio<- DT::renderDataTable({
    #coln_withicon<-paste0(c("Threat of Urbanization ", "Connectivity with PAD-US ","Connectivity of Natural Lands Index ","Proposed Area of Conservation ","Composition of Natural Lands ","Impaired Watershed Area ","Biodiversity Index ", "T&E Species Area ","T&E Number of Species ","Light Pollution Index ","National Register of Historic Places ","National Heritage Area ", "High Priority Working Lands ","Commercial Fishing Reliance ", "Recreational Fishing Engagement ")
    #                      ,as.character(icon("info-sign", lib = "glyphicon")))
    #data <-as.data.frame(result$showing_matrix_raw)
    #row.names(data)<-coln_withicon
    ##print(data)
    #datatable(data,escape = FALSE,options = list(searching = FALSE,paging = FALSE),
    #          callback = JS("       var tips = ['Threat of Conversion indicates the likelihood of the proposed conservation area to be urbanized by the year 2060',
    #                        'Connectivity to PAD-US indicates of the proposed conservation area is close to an area classified as protected by PAD-US data. A binary attribute which represents the spatial relationship between Hexagon and PAD-US. Any Hexagon directly intersects or within 1 Hex (1 km<sup>2</sup>) distance would be count as 1, otherwise, 0.',
    #                        'A percent attribute which stands for the proportion of area, been classified as Hub or Corridor by the raw data source, within each Hexagon. Since the Hexagon unit area is 1 Km2, it also stands for the actual area of Hub within each Hexagon.',
    #                        'The area of the proposed plan in square kilometers',
    #                        'The total percentage area of identified top priority land cover (Tier 1) classes within a hexagon created from NCLD, CCAP, and GAP landcover classification maps. ',
    #                        'A percent attribute which stands for the proportion of impaired watershed within each Hexagon. The watershed data is analyzed based on HUC12 level. Any HUC12 watershed contains 303D Impaired streams would be considered as impaired.', 
    #                        'A score of zero indicates the lowest biodiversity and score of 0+ to 10 indicates biodiversity in increasing order. A score of 10 indicates highest biodiversity within Gulf of Mexico region. Biodiversity index were classified into 10 groups based on the same method proposed in Jenkins paper.', 
    #                        'The attribute is based on the U.S. Fish & Wildlife Service designated T&E critical habitat.  The value in each hexagon is the cumulative % area of critical habitats for all T&E species.',
    #                        'A numeric attribute which represents the number of T&E Species within each Hexagon. The attribute is based on the U.S. Fish & Wildlife Service designated T&E critical habitat.',
    #                        'A score of zero indicates the sky above the hexagon is already polluted/bright and score of 0+ to one indicates light pollution (LP) in decreasing order.',
    #                        'A numeric attribute which represents the counts of historic Places within each Hexagon. The data is based on U.S. NPS National Register of historic Places',
    #                        'A percent attribute which stands for the proportion of Heritage area within each Hexagon. The Heritage data is based on the NPS National Heritage Area layer.',
    #                        'The percentage area of Pine, Cropland and Pasture/Hay classes from NLCD classification map excluding the areas that are already protected (PAD-US). ',
    #                        'Commercial fishing engagement measures the presence of commercial fishing through fishing activity as shown through permits and vessel landings. A high rank indicates more engagement.',
    #                        'Recreational fishing engagement measures the presence of recreational fishing through fishing activity estimates. A high rank indicates more engagement.'],
    #                        firstColumn = $('#showing_matrix tr td:first-child');
    #                        for (var i = 0; i < tips.length; i++) {
    #                        $(firstColumn[i]).attr('title', tips[i]);
    #                        }")
    #          
    #          )
    
    
    coln_withicon<-paste0(coln)
    goal<-c("Habitat","Habitat","Habitat","Habitat","Habitat","Water Quality & Quantity","Water Quality & Quantity","Water Quality & Quantity","Water Quality & Quantity","Water Quality & Quantity","Living Coastal & Marine Resources","Living Coastal & Marine Resources","Living Coastal & Marine Resources","Living Coastal & Marine Resources","Community Resilience","Community Resilience","Community Resilience","Community Resilience","Gulf Economy","Gulf Economy","Gulf Economy","Gulf Economy")
    data<-result$showing_matrix_portfolio_raw
    
    data[1,]<-round(data[1,]*247.105,0)
    
    
    data <-as.data.frame(data)
    rownames(data)<-coln_withicon
    data<-cbind('url'=url,data)
    data<-cbind('Goal'=goal,data)
    data<-cbind('Desscprition'=descript,data)
    
    unitattr<-c("Acres","Index", "Percentage","Index","Percentage","Percentage","Index","Percentage","Percentage","Index","Index","Percentage","Count","0-1 Index","Count","Percentage","Index","Index","Percentage","Index","Index","Count")                      
    #print(data)
    datatable(
      cbind(' ' = paste0(rownames(data),'&#9432;',"  (",unitattr,")"), data), escape = -2,
      options = list(searching = FALSE,paging = FALSE,
                     columnDefs = list(
                       list(visible = FALSE, targets = c(0, 2, 3,4)),
                       list(orderable = FALSE, className = 'details-control', targets = 1)
                     )
      ),
      callback = JS("
                table.column(1).nodes().to$().css({cursor: 'pointer'});
                var format = function(d) {
                return '<div style=\"background-color:#eee; padding: .5em;\"> RESTORE Goal addressed: ' + d[3] + '</div>'+
                '<div style=\"background-color:#eee; padding: .5em;\"> Description: ' + d[2] + '</div>'+
                '<div style=\"background-color:#eee; padding: .5em;\"> <a href='+d[4]+' style=\"font-weight: bolder;\" target=\"_blank\"> Learn More </a>' + '</div>';
                };
                table.on('click', 'td.details-control', function() {
                var td = $(this), row = table.row(td.closest('tr'));
                if (row.child.isShown()) {
                row.child.hide();
                } else {
                row.child(format(row.data())).show();
                }
                });"
      ))%>%
      formatCurrency(1:nrow(data), currency = "", interval = 3, mark = ",",digits = 1)
    
  }, server = FALSE)
  
  output$showing_matrix_portfolio_goal<- DT::renderDataTable({
    #coln_withicon<-paste0(c("Threat of Urbanization ", "Connectivity with PAD-US ","Connectivity of Natural Lands Index ","Proposed Area of Conservation ","Composition of Natural Lands ","Impaired Watershed Area ","Biodiversity Index ", "T&E Species Area ","T&E Number of Species ","Light Pollution Index ","National Register of Historic Places ","National Heritage Area ", "High Priority Working Lands ","Commercial Fishing Reliance ", "Recreational Fishing Engagement ")
    #                      ,as.character(icon("info-sign", lib = "glyphicon")))
    #data <-as.data.frame(result$showing_matrix_raw)
    #row.names(data)<-coln_withicon
    ##print(data)
    #datatable(data,escape = FALSE,options = list(searching = FALSE,paging = FALSE),
    #          callback = JS("       var tips = ['Threat of Conversion indicates the likelihood of the proposed conservation area to be urbanized by the year 2060',
    #                        'Connectivity to PAD-US indicates of the proposed conservation area is close to an area classified as protected by PAD-US data. A binary attribute which represents the spatial relationship between Hexagon and PAD-US. Any Hexagon directly intersects or within 1 Hex (1 km<sup>2</sup>) distance would be count as 1, otherwise, 0.',
    #                        'A percent attribute which stands for the proportion of area, been classified as Hub or Corridor by the raw data source, within each Hexagon. Since the Hexagon unit area is 1 Km2, it also stands for the actual area of Hub within each Hexagon.',
    #                        'The area of the proposed plan in square kilometers',
    #                        'The total percentage area of identified top priority land cover (Tier 1) classes within a hexagon created from NCLD, CCAP, and GAP landcover classification maps. ',
    #                        'A percent attribute which stands for the proportion of impaired watershed within each Hexagon. The watershed data is analyzed based on HUC12 level. Any HUC12 watershed contains 303D Impaired streams would be considered as impaired.', 
    #                        'A score of zero indicates the lowest biodiversity and score of 0+ to 10 indicates biodiversity in increasing order. A score of 10 indicates highest biodiversity within Gulf of Mexico region. Biodiversity index were classified into 10 groups based on the same method proposed in Jenkins paper.', 
    #                        'The attribute is based on the U.S. Fish & Wildlife Service designated T&E critical habitat.  The value in each hexagon is the cumulative % area of critical habitats for all T&E species.',
    #                        'A numeric attribute which represents the number of T&E Species within each Hexagon. The attribute is based on the U.S. Fish & Wildlife Service designated T&E critical habitat.',
    #                        'A score of zero indicates the sky above the hexagon is already polluted/bright and score of 0+ to one indicates light pollution (LP) in decreasing order.',
    #                        'A numeric attribute which represents the counts of historic Places within each Hexagon. The data is based on U.S. NPS National Register of historic Places',
    #                        'A percent attribute which stands for the proportion of Heritage area within each Hexagon. The Heritage data is based on the NPS National Heritage Area layer.',
    #                        'The percentage area of Pine, Cropland and Pasture/Hay classes from NLCD classification map excluding the areas that are already protected (PAD-US). ',
    #                        'Commercial fishing engagement measures the presence of commercial fishing through fishing activity as shown through permits and vessel landings. A high rank indicates more engagement.',
    #                        'Recreational fishing engagement measures the presence of recreational fishing through fishing activity estimates. A high rank indicates more engagement.'],
    #                        firstColumn = $('#showing_matrix tr td:first-child');
    #                        for (var i = 0; i < tips.length; i++) {
    #                        $(firstColumn[i]).attr('title', tips[i]);
    #                        }")
    #          
    #          )
   
    
    
    coln_withicon<-paste0(c("Habitat", "Water Quality & Quantity","Living Coastal Marine Resources","Community Resilience","Gulf Economy"))
    data<-result$showing_matrix_portfolio
    #print("showing_matrix_portfolio")
    #print(result$showing_matrix_portfolio)
    habitat<-colSums(data[1:5,])/5  ## 
    WaterQuality<-colSums(data[6:10,])/5
    LCMR<-colSums(data[11:14,])/4
    CL<-colSums(data[15:18,])/4
    Economy<-colSums(data[19:22,])/4
    data<-rbind(habitat,WaterQuality,LCMR,CL,Economy)
    data_rank<-NULL
    data_index<-NULL
    #print(data)
    for (i in 1:ncol(data)) {
      x<-data[,i]
      tmp<-c(0,0,0,0,0)
      tmp[which(x==max( x[x!=max(x)] ))]<-1
      tmp[which(x==max(x))]<-2
      data_rank<-c(data_rank,tmp)
    }
    data_rank<-matrix(data_rank,nrow = 5)
    
    data<-rbind(data,data_rank)
    data<-t(data)
    
    
    data <-as.data.frame(data)
    
    colnames(data)<-c(coln_withicon,"r1","r2","r3","r4","r5")
    rownames(data)<-c(proplist[1:(nrow(data))])
   
    datatable(
      cbind(' ' = paste0(rownames(data)), data), escape = -2,
      options = list(searching = FALSE,paging = FALSE,
                     autoWidth = TRUE,
                     columnDefs = list(
                       list(width = '200px', targets = "_all"),
                       list(visible = FALSE, targets = c(0,10,7,8,9,11)),
                       list(orderable = FALSE, className = 'details-control', targets = 1)
                     )
      ))%>%
      formatStyle(
        "Habitat", 'r1',
        backgroundColor = styleEqual(c(1,2), c('yellow', 'green'))
      )%>%
      formatStyle(
         "Water Quality & Quantity", 'r2',
        backgroundColor = styleEqual(c(1,2), c('yellow', 'green'))
      )%>%
      formatStyle(
        "Living Coastal Marine Resources", 'r3',
        backgroundColor = styleEqual(c(1,2), c('yellow', 'green'))
      )%>%
      formatStyle(
        "Community Resilience", 'r4',
        backgroundColor = styleEqual(c(1,2), c('yellow', 'green'))
      )%>%
      formatStyle(
        "Gulf Economy", 'r5',
        backgroundColor = styleEqual(c(1,2), c('yellow', 'green'))
      )
    
  }, server = FALSE)
  
  output$showing_matrix<- DT::renderDataTable({
    #coln_withicon<-paste0(c("Threat of Urbanization ", "Connectivity with PAD-US ","Connectivity of Natural Lands Index ","Proposed Area of Conservation ","Composition of Natural Lands ","Impaired Watershed Area ","Biodiversity Index ", "T&E Species Area ","T&E Number of Species ","Light Pollution Index ","National Register of Historic Places ","National Heritage Area ", "High Priority Working Lands ","Commercial Fishing Reliance ", "Recreational Fishing Engagement ")
    #                      ,as.character(icon("info-sign", lib = "glyphicon")))
    #data <-as.data.frame(result$showing_matrix_raw)
    #row.names(data)<-coln_withicon
    ##print(data)
    #datatable(data,escape = FALSE,options = list(searching = FALSE,paging = FALSE),
    #          callback = JS("       var tips = ['Threat of Conversion indicates the likelihood of the proposed conservation area to be urbanized by the year 2060',
    #                        'Connectivity to PAD-US indicates of the proposed conservation area is close to an area classified as protected by PAD-US data. A binary attribute which represents the spatial relationship between Hexagon and PAD-US. Any Hexagon directly intersects or within 1 Hex (1 km<sup>2</sup>) distance would be count as 1, otherwise, 0.',
    #                        'A percent attribute which stands for the proportion of area, been classified as Hub or Corridor by the raw data source, within each Hexagon. Since the Hexagon unit area is 1 Km2, it also stands for the actual area of Hub within each Hexagon.',
    #                        'The area of the proposed plan in square kilometers',
    #                        'The total percentage area of identified top priority land cover (Tier 1) classes within a hexagon created from NCLD, CCAP, and GAP landcover classification maps. ',
    #                        'A percent attribute which stands for the proportion of impaired watershed within each Hexagon. The watershed data is analyzed based on HUC12 level. Any HUC12 watershed contains 303D Impaired streams would be considered as impaired.', 
    #                        'A score of zero indicates the lowest biodiversity and score of 0+ to 10 indicates biodiversity in increasing order. A score of 10 indicates highest biodiversity within Gulf of Mexico region. Biodiversity index were classified into 10 groups based on the same method proposed in Jenkins paper.', 
    #                        'The attribute is based on the U.S. Fish & Wildlife Service designated T&E critical habitat.  The value in each hexagon is the cumulative % area of critical habitats for all T&E species.',
    #                        'A numeric attribute which represents the number of T&E Species within each Hexagon. The attribute is based on the U.S. Fish & Wildlife Service designated T&E critical habitat.',
    #                        'A score of zero indicates the sky above the hexagon is already polluted/bright and score of 0+ to one indicates light pollution (LP) in decreasing order.',
    #                        'A numeric attribute which represents the counts of historic Places within each Hexagon. The data is based on U.S. NPS National Register of historic Places',
    #                        'A percent attribute which stands for the proportion of Heritage area within each Hexagon. The Heritage data is based on the NPS National Heritage Area layer.',
    #                        'The percentage area of Pine, Cropland and Pasture/Hay classes from NLCD classification map excluding the areas that are already protected (PAD-US). ',
    #                        'Commercial fishing engagement measures the presence of commercial fishing through fishing activity as shown through permits and vessel landings. A high rank indicates more engagement.',
    #                        'Recreational fishing engagement measures the presence of recreational fishing through fishing activity estimates. A high rank indicates more engagement.'],
    #                        firstColumn = $('#showing_matrix tr td:first-child');
    #                        for (var i = 0; i < tips.length; i++) {
    #                        $(firstColumn[i]).attr('title', tips[i]);
    #                        }")
    #          
    #          )
    

    coln_withicon<-paste0(coln)
    goal<-c("Habitat","Habitat","Habitat","Habitat","Habitat","Water Quality & Quantity","Water Quality & Quantity","Water Quality & Quantity","Water Quality & Quantity","Water Quality & Quantity","Living Coastal & Marine Resources","Living Coastal & Marine Resources","Living Coastal & Marine Resources","Living Coastal & Marine Resources","Community Resilience","Community Resilience","Community Resilience","Community Resilience","Gulf Economy","Gulf Economy","Gulf Economy","Gulf Economy") ########1/14/2021
    data<-result$showing_matrix_raw
    data[1,]<-round(data[1,]*247.105,0)
    data <-as.data.frame(data)
    print(coln_withicon) ###############1/14/2021
    rownames(data)<-coln_withicon
    data<-cbind('url'=url,data)
    data<-cbind('Goal'=goal,data)
    data<-cbind('Desscprition'=descript,data)
    
    
    
    datatable(
      cbind(' ' = paste0(rownames(data),'&#9432;'), data), escape = -2,extensions = 'RowGroup',
      
      options = list(searching = FALSE,paging = FALSE,rowGroup = list(dataSrc = 3),
                     columnDefs = list(
                       list(visible = FALSE, targets = c(0, 2, 3,4)),
                       list(orderable = FALSE, className = 'details-control', targets = 1)
                     )
      ),
      callback = JS("
                table.column(1).nodes().to$().css({cursor: 'pointer'});
                var format = function(d) {
                return '<div style=\"background-color:#eee; padding: .5em;\"> RESTORE Goal addressed: ' + d[3] + '</div>'+
                '<div style=\"background-color:#eee; padding: .5em;\"> Description: ' + d[2] + '</div>'+
                '<div style=\"background-color:#eee; padding: .5em;\"> <a href='+d[4]+' style=\"font-weight: bolder;\" target=\"_blank\"> Learn More </a>' + '</div>';
                };
                table.on('click', 'td.details-control', function() {
                var td = $(this), row = table.row(td.closest('tr'));
                if (row.child.isShown()) {
                row.child.hide();
                } else {
                row.child(format(row.data())).show();
                }
                });"
      ))%>%
      formatCurrency(5:(ncol(data)+1), currency = "", interval = 3, mark = ",",digits = 1)
  }, server = FALSE)
  
  
  
  
  observeEvent(input$showdefault,{
    
    #EC<-rep(0,ncol(result$matrix))
    HA_hf<-result$matrix[1:5,]*(2/3)
    #print(class(HA_hf))
    WQ_hf<-result$matrix[6:10,]*(2/3)
    LC_hf<-result$matrix[11:14,]*(2/3) 
    CL_hf<-result$matrix[15:18,]*(2/3)
    EC_hf<-result$matrix[19:22,]*(2/3)
    ##print(HA_hf)
    if(value2$newaddedweight>0){
      result$newaddattr_weight<-result$newaddattr[,-(1:2)]
      for(i in 1:nrow(result$newaddattr_weight)){
        tmp<-as.numeric(result$newaddattr_weight[i,])
        tmp<-tmp*(as.numeric(weight$UA[i])-1)/3
        if(result$newaddattr$Goals[i]=="HA"){
          HA_hf<-rbind(HA_hf,tmp)
        }
        if(result$newaddattr$Goals[i]=="WQ"){
          WQ_hf<-rbind(WQ_hf,tmp)
        }
        if(result$newaddattr$Goals[i]=="LCMR"){
          LC_hf<-rbind(LC_hf,tmp)
        }
        if(result$newaddattr$Goals[i]=="CL"){
          CL_hf<-rbind(CL_hf,tmp)
        }
        if(result$newaddattr$Goals[i]=="EC"){
          EC_hf<-rbind(EC_hf,tmp)
        }
        
      }
      
      
    }
    
    #test request from Matt
    HA_hf_test<-result$matrix[1:5,]
    WQ_hf_test<-result$matrix[6:10,]
    LC_hf_test<-result$matrix[11:14,] 
    CL_hf_test<-result$matrix[15:18,]
    EC_hf_test<-result$matrix[19:22,]
    
    result$default_rank_test<- rbind(colSums(HA_hf_test),colSums(WQ_hf_test),colSums(LC_hf_test),colSums(CL_hf_test),colSums(EC_hf_test))
    result$default_rank_test1<-rbind(result$default_rank_test,colSums(result$default_rank_test))
    result$default_rank_test1<-cbind(result$default_rank_test1,c(5,5,4,4,4,22))
    
    colnames(result$default_rank_test)<- proplist[1:ncol(result$default_rank_test)]
    rownames(result$default_rank_test)<- c("Habitat","Water Quality & Quantity","Living Coastal Marine Resources", "Community Resilience","Gulf Economy") 
    colnames(result$default_rank_test1)<- c(proplist[1:ncol(result$default_rank_test)],"Total possible score")
    rownames(result$default_rank_test1)<- c("Habitat","Water Quality & Quantity","Living Coastal Marine Resources", "Community Resilience","Gulf Economy","Total Sum") 
    
    result$default_rank_test_1<-colSums(result$default_rank_test)
    #End test request
    
    result$default_rank<- rbind(colSums(HA_hf),colSums(WQ_hf),colSums(LC_hf),colSums(CL_hf),colSums(EC_hf))*0.2
    
    result$default_rank1<-rbind(result$default_rank,colSums(result$default_rank))
    result$default_rank1<-cbind(result$default_rank1,c(20,20,20,20,20,100)/100)
    
    colnames(result$default_rank)<- proplist[1:ncol(result$default_rank)]
    rownames(result$default_rank)<- c("Habitat","Water Quality & Quantity","Living Coastal Marine Resources", "Community Resilience","Gulf Economy") 
    colnames(result$default_rank1)<- c(proplist[1:ncol(result$default_rank)],"Weights")
    rownames(result$default_rank1)<- c("Habitat","Water Quality & Quantity","Living Coastal Marine Resources", "Community Resilience","Gulf Economy","Total Sum") 
    ##print(colSums(WQ_hf))
    ##print(rbind(colSums(HA_hf),colSums(WQ_hf),colSums(WQ_hf),colSums(LC_hf),colSums(CL_hf),EC))
    ##print(result$final_rank)
    result$default_rank_1<-colSums(result$default_rank)
    
    show(selector = "#nav li a[data-value=Result]")
    updateTabsetPanel(session = session, inputId = "nav", "Result")
    show(selector = "#tabsResult li a[data-value=defaultweight]")
    updateTabsetPanel(session = session, inputId = "tabsResult", "defaultweight")
  })
  
  
  output$showing_matrix_scaled<- DT::renderDataTable({
    #coln_withicon<-paste0(c("Threat of Urbanization ", "Connectivity with PAD-US ","Connectivity of Natural Lands Index ","Proposed Area of Conservation ","Composition of Natural Lands ","Impaired Watershed Area ","Biodiversity Index ", "T&E Species Area ","T&E Number of Species ","Light Pollution Index ","National Register of Historic Places ","National Heritage Area ", "High Priority Working Lands ","Commercial Fishing Reliance ", "Recreational Fishing Engagement ")
    #                      ,as.character(icon("info-sign", lib = "glyphicon")))
    #data <-as.data.frame(result$showing_matrix)
    #row.names(data)<-coln_withicon
    ##print(data)
    #datatable(data,escape = FALSE,options = list(searching = FALSE,paging = FALSE),
    #          callback = JS("       var tips = ['A score of zero indicates the hexagon is already urban and score of 0+ to one indicates the predicted likelihood of threat in decreasing order. A score of one indicates absolutely no threat of conversion based on SLEUTH 2060 urbanization model.',
    #                        'Connectivity to PAD-US indicates of the proposed conservation area is close to an area classified as protected by PAD-US data. A binary attribute which represents the spatial relationship between Hexagon and PAD-US. Any Hexagon directly intersects or within 1 Hex (1 km<sup>2</sup>) distance would be count as 1, otherwise, 0.',
    #                        'A percent attribute which stands for the proportion of area, been classified as Hub or Corridor by the raw data source, within each Hexagon. Since the Hexagon unit area is 1 Km2, it also stands for the actual area of Hub within each Hexagon.',
    #                        'The area of the proposed plan in square kilometers',
    #                        'The total percentage area of identified top priority land cover (Tier 1) classes within a hexagon created from NCLD, CCAP, and GAP landcover classification maps. ',
    #                        'A percent attribute which stands for the proportion of impaired watershed within each Hexagon. The watershed data is analyzed based on HUC12 level. Any HUC12 watershed contains 303D Impaired streams would be considered as impaired.',
    #                        'A score of zero indicates the lowest biodiversity and score of 0+ to 10 indicates biodiversity in increasing order. A score of 10 indicates highest biodiversity within Gulf of Mexico region. Biodiversity index were classified into 10 groups based on the same method proposed in Jenkins paper.',
    #                        'The attribute is based on the U.S. Fish & Wildlife Service designated T&E critical habitat.  The value in each hexagon is the cumulative % area of critical habitats for all T&E species.',
    #                        'A numeric attribute which represents the number of T&E Species within each Hexagon. The attribute is based on the U.S. Fish & Wildlife Service designated T&E critical habitat.',
    #                        'A score of zero indicates the sky above the hexagon is already polluted/bright and score of 0+ to one indicates light pollution (LP) in decreasing order.',
    #                        'A numeric attribute which represents the counts of historic Places within each Hexagon. The data is based on U.S. NPS National Register of historic Places', 
    #                        'A percent attribute which stands for the proportion of Heritage area within each Hexagon. The Heritage data is based on the NPS National Heritage Area layer.',
    #                        'The percentage area of Pine, Cropland and Pasture/Hay classes from NLCD classification map excluding the areas that are already protected (PAD-US). ',
    #                        'Commercial fishing engagement measures the presence of commercial fishing through fishing activity as shown through permits and vessel landings. A high rank indicates more engagement.',
    #                        'Recreational fishing engagement measures the presence of recreational fishing through fishing activity estimates. A high rank indicates more engagement.'],
    #                        firstColumn = $('#showing_matrix_scaled tr td:first-child');
    #                        for (var i = 0; i < tips.length; i++) {
    #                        $(firstColumn[i]).attr('title', tips[i]);
    #                        }")
    #          
    #          )
    
    
    coln_withicon<-paste0(coln)
    goal<-c("Habitat","Habitat","Habitat","Habitat","Habitat","Water Quality & Quantity","Water Quality & Quantity","Water Quality & Quantity","Water Quality & Quantity","Water Quality & Quantity","Living Coastal & Marine Resources","Living Coastal & Marine Resources","Living Coastal & Marine Resources","Living Coastal & Marine Resources","Community Resilience","Community Resilience","Community Resilience","Community Resilience","Gulf Economy","Gulf Economy","Gulf Economy","Gulf Economy")
    data<-result$showing_matrix
    data <-as.data.frame(data)
    rownames(data)<-coln_withicon
    data<-cbind('url'=url,data)
    data<-cbind('Goal'=goal,data)
    data<-cbind('Desscprition'=descript,data)
    
    
    
    datatable(
      cbind(' ' = paste0(rownames(data),'&#9432;'), data), escape = -2,extensions = 'RowGroup',

      options = list(searching = FALSE,paging = FALSE,rowGroup = list(dataSrc = 3),
                     columnDefs = list(
                       list(visible = FALSE, targets = c(0, 2, 3,4)),
                       list(orderable = FALSE, className = 'details-control', targets = 1)
                     )
      ),
      callback = JS("
                table.column(1).nodes().to$().css({cursor: 'pointer'});
                var format = function(d) {
                return '<div style=\"background-color:#eee; padding: .5em;\"> RESTORE Goal addressed: ' + d[3] + '</div>'+
                '<div style=\"background-color:#eee; padding: .5em;\"> Description: ' + d[2] + '</div>'+
                '<div style=\"background-color:#eee; padding: .5em;\"> <a href='+d[4]+' style=\"font-weight: bolder;\" target=\"_blank\"> Learn More </a>' + '</div>';
                };
                table.on('click', 'td.details-control', function() {
                var td = $(this), row = table.row(td.closest('tr'));
                if (row.child.isShown()) {
                row.child.hide();
                } else {
                row.child(format(row.data())).show();
                }
                });"
      ))
  }, server = FALSE)
  
  output$showing_matrix2<- DT::renderDataTable({
    data <- result$showing_matrix[c(4,6,14),]
    data <-as.data.frame(data)
    data
  }, server = FALSE, selection='none',escape = FALSE,
  options = list(searching = FALSE,paging = FALSE )
  )
  
  observeEvent(input$utility_urban,{
    if(input$utility_urban=="negative"){
      result$showing_matrix[4,]<-1-result$showing_matrix[4,]
      result$matrix[4,]<-1-result$matrix[4,]
    }
    if(input$utility_urban=="positive"){
      result$showing_matrix[4,]<-1-result$showing_matrix[4,]
      result$matrix[4,]<-1-result$matrix[4,]
    }
    
  })
  observeEvent(input$utility_303D,{
    if(input$utility_303D=="negative"){
      result$showing_matrix[6,]<-1-result$showing_matrix[6,]
      result$matrix[6,]<-1-result$matrix[6,]
    }
    if(input$utility_303D=="positive"){
      result$showing_matrix[6,]<-1-result$showing_matrix[6,]
      result$matrix[6,]<-1-result$matrix[6,]
    }
  })
  observeEvent(input$utility_light,{
    if(input$utility_light=="negative"){
      result$showing_matrix[14,]<-1-result$showing_matrix[14,]
      result$matrix[14,]<-1-result$matrix[14,]
    }
    if(input$utility_light=="positive"){
      result$showing_matrix[14,]<-1-result$showing_matrix[14,]
      result$matrix[14,]<-1-result$matrix[14,]
    }
  })
    
  
  #observeEvent(input$updateutility,{
  #  session$sendCustomMessage('unbind-DT', 'showing_matrix2')
  #    #print(result$matrix[1,])
  #    if(result$utility_num[1]==2){
  #      result$matrix[1,]<-1-result$matrix[1,]
  #      result$showing_matrix[1,]<-1-result$showing_matrix[1,]
  #    }
  #    if(result$utility_num[2]==2){
  #      result$matrix[6,]<-1-result$matrix[6,]
  #    }
  #    if(result$utility_num[3]==2){
  #      result$matrix[10,]<-1-result$matrix[10,]
  #    }
  #    #print(result$matrix[1,])
    
  #})
  
  observeEvent(input$backtomain,{
    
    updateTabsetPanel(session = session, inputId = "nav", "MCDA")
    updateTabsetPanel(session = session, inputId = "tabs", "dataoverview")
    #updateActionButton(session,showdefault ,style="color:#0000FF;")
    #updateActionButton(session, runsmaa,style="color:#190707;")
    #setcolor("showdefault","#0000FF") {
    #  document.getElementById(id).style.color = color;
    #}
    ##print(changecolor)
    #js$pageCol(input$showdefault,"#0000FF")
  })
  
  observeEvent(input$backtomain1,{
    
    updateTabsetPanel(session = session, inputId = "nav", "MCDA")
    updateTabsetPanel(session = session, inputId = "tabs", "dataoverview")
    
    #updateActionButton(session,startweight,style="color:#0000FF;")
    #updateActionButton(session,runsmaa ,style="color:#190707;")
  })
  
  
 
  
  ignoreNext_hab <- ""
  ignoreNext_water <- ""
  ignoreNext_resilience <- ""
  ignoreNext_species <- ""
  ignoreNext_economy <- ""
  
  observeEvent(input$habitat,{ 
    if (ignoreNext_hab == "A") { 
      ignoreNext_hab <<- "" 
    } 
    else{ 
      valB <- as.integer(input$habitat) 
      if(valB != input$habitat1){ 
        ignoreNext_hab <<- "B" 
        updateNumericInput(session, "habitat1", value = valB)
        value1$habitat<-valB
      } 
    } 
  }) 
  
  observeEvent(input$habitat1,{ 
    if (ignoreNext_hab == "B") { 
      ignoreNext_hab <<- "" 
    } 
    else{ 
      valA <- as.integer(input$habitat1) 
      if(valA != input$habitat){ 
        ignoreNext_hab <<- "A" 
        updateSliderInput(session, "habitat", value = valA) 
        value1$habitat<-valA
      } 
    } 
  }) 
  
  
  observeEvent(input$water,{ 
    if (ignoreNext_water == "A") { 
      ignoreNext_water <<- "" 
    } 
    else{ 
      valB <- as.integer(input$water) 
      if(valB != input$water1){ 
        ignoreNext_water <<- "B" 
        updateNumericInput(session, "water1", value = valB)
        value1$water<-valB
      } 
    } 
  }) 
  
  observeEvent(input$water1,{ 
    if (ignoreNext_water == "B") { 
      ignoreNext_water <<- "" 
    } 
    else{ 
      valA <- as.integer(input$water1) 
      if(valA != input$water){ 
        ignoreNext_water <<- "A" 
        updateSliderInput(session, "water", value = valA) 
        value1$water<-valA
      } 
    } 
  }) 
  
  
  
  observeEvent(input$resilience,{ 
    if (ignoreNext_resilience == "A") { 
      ignoreNext_resilience <<- "" 
    } 
    else{ 
      valB <- as.integer(input$resilience) 
      if(valB != input$resilience1){ 
        ignoreNext_resilience <<- "B" 
        updateNumericInput(session, "resilience1", value = valB)
        value1$resilience<-valB
      } 
    } 
  }) 
  
  observeEvent(input$resilience1,{ 
    if (ignoreNext_resilience == "B") { 
      ignoreNext_resilience <<- "" 
    } 
    else{ 
      valA <- as.integer(input$resilience1) 
      if(valA != input$resilience){ 
        ignoreNext_resilience <<- "A" 
        updateSliderInput(session, "resilience", value = valA) 
        value1$resilience<-valA
      } 
    } 
  }) 
  
  
  observeEvent(input$species,{ 
    if (ignoreNext_species == "A") { 
      ignoreNext_species <<- "" 
    } 
    else{ 
      valB <- as.integer(input$species) 
      if(valB != input$species1){ 
        ignoreNext_species <<- "B" 
        updateNumericInput(session, "species1", value = valB)
        value1$species<-valB
      } 
    } 
  }) 
  
  observeEvent(input$species1,{ 
    if (ignoreNext_species == "B") { 
      ignoreNext_species <<- "" 
    } 
    else{ 
      valA <- as.integer(input$species1) 
      if(valA != input$species){ 
        ignoreNext_species <<- "A" 
        updateSliderInput(session, "species", value = valA) 
        value1$species<-valA
      } 
    } 
  }) 
  
  observeEvent(input$economy,{ 
    if (ignoreNext_economy == "A") { 
      ignoreNext_economy <<- "" 
    } 
    else{ 
      valB <- as.integer(input$economy) 
      if(valB != input$economy1){ 
        ignoreNext_economy <<- "B" 
        updateNumericInput(session, "economy1", value = valB)
        value1$economy<-valB
      } 
    } 
  }) 
  
  observeEvent(input$economy1,{ 
    if (ignoreNext_economy == "B") { 
      ignoreNext_economy <<- "" 
    } 
    else{ 
      valA <- as.integer(input$economy1) 
      if(valA != input$economy){ 
        ignoreNext_economy <<- "A" 
        updateSliderInput(session, "economy", value = valA) 
        value1$economy<-valA
      } 
    } 
  }) 
  
  
  
  ###table of RESTORE goal weights  
  GWtable<-reactive({
    tmpsum<-sum(value1$habitat,value1$water,value1$species,value1$resilience,value1$economy)
    weight$tmpsum<-tmpsum
    tmp<-data.frame("Goal"=c("Habitat","Water Quality & Quantity",
                             "Living Coastal & Marine Resources", "Community Resilience",
                             "Gulf Economy","Total"),
                    "Weight"=c(value1$habitat,value1$water,value1$species,value1$resilience,value1$economy,tmpsum))
    weight$goal<-c(value1$habitat,value1$water,value1$species,value1$resilience,value1$economy)*100/tmpsum
    colnames(tmp)<-c("RESTORE Goal", "Weight (%)")
    return(tmp)
  })
  
  output$GW_table<-renderTable(expr = {
    GWtable()}, bordered = T, hover = T, align = "c", striped = T)
  ###
  
  #helper function for making selectbox within data table
  shinyInput = function(FUN, len, id, ...) {
    inputs = character(len)
    for (i in seq_len(len)) {
      inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
    }
    inputs
  }
  
  
  #habitat datatable with selectbox
  output$hab_PA_measures_Table = DT::renderDataTable({
    data.frame(hab_PA_Table,Weight=shinyInput(selectInput,5,"select_hab",
                                              choices=c("Zero","Low","Medium","High"),width="100px",selected = "Medium")
               
    )
  }, selection='none',server = FALSE, escape = F, options=list(
    paging=F,
    searching=F,
    preDrawCallback = JS('function() {
                         Shiny.unbindAll(this.api().table().node()); }'),
    drawCallback = JS('function() {
                      Shiny.bindAll(this.api().table().node()); }')
    ),colnames=c("Measure(s)","Weight"
                 
    ))
  
  
  #Water Quality & Quantity datatable with selectbox
  output$wq_PA_measures_Table = DT::renderDataTable({
    data.frame(wq_PA_Table,Weight=shinyInput(selectInput,5,"select_wq",
                                             choices=c("Zero","Low","Medium","High"),width="100px",selected = "Medium"))
  }, selection='none',server = FALSE, escape = F, options=list(
    paging=F,
    searching=F,
    preDrawCallback = JS('function() {
                         Shiny.unbindAll(this.api().table().node()); }'),
    drawCallback = JS('function() {
                      Shiny.bindAll(this.api().table().node()); }')
    ),colnames=c("Measure(s)","Weight"))
  
  #LCMR datatable with selectbox
  output$lcmr_PA_measures_Table = DT::renderDataTable({
    data.frame(lcmr_PA_Table,Weight=shinyInput(selectInput,4,"select_lcmr",
                                               choices=c("Zero","Low","Medium","High"),width="100px",selected = "Medium"))
  }, selection='none',server = FALSE, escape = F, options=list(
    paging=F,
    searching=F,
    preDrawCallback = JS('function() {
                         Shiny.unbindAll(this.api().table().node()); }'),
    drawCallback = JS('function() {
                      Shiny.bindAll(this.api().table().node()); }')
    ),colnames=c("Measure(s)","Weight"))
  
  #commres datatable with selectbox
  output$commres_PA_measures_Table = DT::renderDataTable({
    data.frame(commres_PA_Table,Weight=shinyInput(selectInput,4,"select_commres",
                                                  choices=c("Zero","Low","Medium","High"),width="100px",selected = "Medium"))
  }, selection='none',server = FALSE, escape = F, options=list(
    paging=F,
    searching=F,
    preDrawCallback = JS('function() {
                         Shiny.unbindAll(this.api().table().node()); }'),
    drawCallback = JS('function() {
                      Shiny.bindAll(this.api().table().node()); }')
    ),colnames=c("Measure(s)","Weight"))
  
  #gulfecon datatable with selectbox
  output$gulfecon_PA_measures_Table = DT::renderDataTable({
    data.frame(gulfecon_PA_Table,Weight=shinyInput(selectInput,4,"select_gulfecon",
                                                   choices=c("Zero","Low","Medium","High"),width="100px",selected = "Medium"))
  }, selection='none',server = FALSE, escape = F, options=list(
    paging=F,
    searching=F,
    preDrawCallback = JS('function() {
                         Shiny.unbindAll(this.api().table().node()); }'),
    drawCallback = JS('function() {
                      Shiny.bindAll(this.api().table().node()); }')
    ),colnames=c("Measure(s)","Weight"))
  
  #User Added Attributes datatable with selectbox
  output$UserAdded_PA_measures_Table = DT::renderDataTable({
    useradded_PA_Table<<-result$newaddattr[,c(1:2)]
    tmp<-data.frame(useradded_PA_Table,Weight=shinyInput(selectInput,length(result$newaddattr$Goals),"select_ua",
                                              choices=c("Zero","Low","Medium","High"),width="100px",selected = "Medium")
    )
    #print(tmp)
    tmp
  }, selection='none',server = FALSE, escape = F, options=list(
    paging=F,
    searching=F,
    preDrawCallback = JS('function() {
                         Shiny.unbindAll(this.api().table().node()); }'),
    drawCallback = JS('function() {
                      Shiny.bindAll(this.api().table().node()); }')
  ),colnames=c("Measure Name","Goals","Weight"
               
  ))
  
  # helper function for reading selectbox
  shinyValue = function(id, len) { 
    unlist(lapply(seq_len(len), function(i) { 
      value = input[[paste0(id, i)]] 
      if (is.null(value)) NA else value 
    })) 
  } 
  #####Review weights table
  WRtable<-reactive({
    
    
    coln_withicon<-paste0(coln)
    goal<-c("Habitat","Habitat","Habitat","Habitat","Habitat","Water Quality & Quantity","Water Quality & Quantity","Water Quality & Quantity","Water Quality & Quantity","Water Quality & Quantity","Living Coastal & Marine Resources","Living Coastal & Marine Resources","Living Coastal & Marine Resources","Living Coastal & Marine Resources","Community Resilience","Community Resilience","Community Resilience","Community Resilience","Gulf Economy","Gulf Economy","Gulf Economy","Gulf Economy")
    
    
    weights<-c(as.character(weight$HA),as.character(weight$WQ),as.character(weight$LC),as.character(weight$CL),as.character(weight$EC))
    tmp<-cbind(result$showing_matrix,weights)
    tmp<-cbind('url'=url,tmp)
    tmp<-cbind('Goal'=goal,tmp)
    tmp<-cbind('Desscprition'=descript,tmp)
    rownames(tmp)<-coln_withicon
    
    return(tmp)
  })
  
  output$weights_review<- renderDataTable({datatable(
    
    cbind(' ' = paste0(rownames(WRtable()),'&#9432;'), WRtable()), escape = -2,extensions = 'RowGroup',
    
    options = list(searching = FALSE,paging = FALSE,rowGroup = list(dataSrc = 3),
                   columnDefs = list(
                     list(visible = FALSE, targets = c(1, 2, 3,4)),
                     list(orderable = FALSE, className = 'details-control', targets = 0)
                   )
    ),
    callback = JS("
                table.column(1).nodes().to$().css({cursor: 'pointer'});
                var format = function(d) {
                return '<div style=\"background-color:#eee; padding: .5em;\"> RESTORE Goal addressed: ' + d[3] + '</div>'+
                '<div style=\"background-color:#eee; padding: .5em;\"> Description: ' + d[2] + '</div>'+
                '<div style=\"background-color:#eee; padding: .5em;\"> <a href='+d[4]+' style=\"font-weight: bolder;\" target=\"_blank\"> Learn More </a>' + '</div>';
                };
                table.on('click', 'td.details-control', function() {
                var td = $(this), row = table.row(td.closest('tr'));
                if (row.child.isShown()) {
                row.child.hide();
                } else {
                row.child(format(row.data())).show();
                }
                });"
    ))},server = F)
  
  
  
  WRtableua<-reactive({
    weights<-c(as.character(weight$UA))
    tmp<-cbind(result$newaddattr,weights)
    colnames(tmp)[1]<-"Measure Name"
    return(tmp)
  })
  
  output$weights_review_ua<- renderTable(expr = WRtableua(),rownames = T,striped = T) 
  
  observe({
    if(length(result$newaddattr)>0){
      show("weights_review_ua")
    }
    if(length(result$newaddattr)==0){
      hide("weights_review_ua")
    }
  })
  
  
  ######MCDA
  value2$test1<-0
  observeEvent(input$runsmaa, {
    
    showModal(div(id="ModalDiv", modalDialog(
      title = "Multiple-criteria decision analysis",
      paste0("Please give up to 5 minutes for the model to run. This model will run " ,format(value1$mcdarun,scientific = F,big.mark   = ","), " iterations through MCDA model. Meanwhile, please not make changes to the application."),
      footer = tagList(modalButton("Cancel"),
                       actionButton("close", "OK"))
      )))
  })
  
  observeEvent(input$close,{
    value2$test1<-10
    value1$txt<-paste0("The result is based on ",format(value1$mcdarun,scientific = F,big.mark = ",") ," iterations of the MCDA model:\n")
  })
  
  
  observe({
    if(value2$test1==10){
      withProgress(message = 'Simulating...', value = 0, {
        HA<-(result$matrix[1,]+result$matrix[2,]+result$matrix[3,]+result$matrix[4,]+result$matrix[5,])/5
        WQ<-(result$matrix[6,]+result$matrix[7,]+result$matrix[8,]+result$matrix[9,]+result$matrix[10,])/5
        LC<-(result$matrix[11,]+result$matrix[12,]+result$matrix[13,]+result$matrix[14,])/4
        CL<-(result$matrix[15,]+result$matrix[16,]+result$matrix[17,]+result$matrix[18,])/4
        EC<-(result$matrix[19,]+result$matrix[20,]+result$matrix[21,]+result$matrix[22,])/4
        
        if(value2$newaddedweight>0){
          if("HA" %in% result$newaddattr$Goals){
            tmpha<-result$newaddattr[which(result$newaddattr$Goals=="HA"),]
            tmpha<-as.matrix(tmpha[,-(1:2)])
            for(i in 1:nrow(tmpha)){
              HA<-HA+as.numeric(tmpha[i,])
            }
          }
          if("WQ" %in% result$newaddattr$Goals){
            tmpha<-result$newaddattr[which(result$newaddattr$Goals=="WQ"),]
            tmpha<-as.matrix(tmpha[,-(1:2)])
            for(i in 1:nrow(tmpha)){
              WQ<-WQ+as.numeric(tmpha[i,])
            }
          }
          if("LCMR" %in% result$newaddattr$Goals){
            tmpha<-result$newaddattr[which(result$newaddattr$Goals=="LCMR"),]
            tmpha<-as.matrix(tmpha[,-(1:2)])
            for(i in 1:nrow(tmpha)){
              LCMR<-LCMR+as.numeric(tmpha[i,])
            }
          }
          if("CL" %in% result$newaddattr$Goals){
            tmpha<-result$newaddattr[which(result$newaddattr$Goals=="CL"),]
            tmpha<-as.matrix(tmpha[,-(1:2)])
            for(i in 1:nrow(tmpha)){
              CL<-CL+as.numeric(tmpha[i,])
            }
          }
          if("EC" %in% result$newaddattr$Goals){
            tmpha<-result$newaddattr[which(result$newaddattr$Goals=="EC"),]
            tmpha<-as.matrix(tmpha[,-(1:2)])
            for(i in 1:nrow(tmpha)){
              EC<-EC+as.numeric(tmpha[i,])
            }
          }
            
        }
        matrix<-rbind(HA,WQ,LC,CL,EC)
        
        result$final_matrix<- matrix
        ##print(result$final_matrix)
        showModal(modalDialog(
          title = "Simulation has begun",footer = modalButton("Ok"),
          "Please wait for the results"
        ))
        incProgress(0.1,detail = paste("Getting parameters ready."))
        N<-value1$mcdarun; 
        alt<-length(ps_list$result);
        
        m<-5 #alt = alternatives, m = criteria
        
        #function y = utility(x,w,mu,sig,pos)
        
        utility<-function(x,w,mu,sig,pos){
          
          kmin = mu-sig
          kmax= mu+sig
          worst = apply(kmin,1,min)
          best = apply(kmax,1,max)
          
          y=0 
          
          for(i in 1:length(worst)){
            # for(i in 2:2){
            
            if (pos[i]==1){sco = w[i]*(x[i]-worst[i])/(best[i]-worst[i])}
            else{sco =  w[i]*(1-(x[i]-worst[i])/(best[i]-worst[i]))}
            y = as.numeric(sco)+y}
          return(y)
        }
        
        
        # alt_labels = categorical({'Follets Island', 'Bahia Grande', 'Pascagula River','Mobile Bay'});
        # alt_labels_2 = {'Follets Island', 'Bahia Grande', 'Pascagula River','Mobile Bay'};
        
        
        means<-result$final_matrix
        #standard=data.frame('m1'=c(1,1,1,1),
        #                    'm2'=c(1,1,1,1),
        #                    'm3'=c(1,1,1,1),
        #                    'm4'=c(1,1,1,1),
        #                    'm5'=c(1,1,1,1))
        #standard<-t(standard)
        #standard<-as.matrix(standard)
        standard<-matrix(0.1,5,alt)
        pos<-c(1,1,1,1,1)
        
        rankaccept = matrix(0,alt,alt)
        #print(rankaccept)
        central = matrix(0,alt,m)
        #print(central)
        confidence = matrix(0,1,alt)
        incProgress(0.1,detail = paste("Simulation starts."))
        # Compute rank acceptability indices + central weight vectors
        weightsR<-NULL
       
        for(i in 1:N){
          incProgress(0.6/N,detail = paste("Iteration: ", i,"out of ",format(value1$mcdarun,scientific = F,big.mark   = ",")))
          # for i = 1:iterations
          
          # Generate weights
          
          randNum <- runif(n=m-1,min=0,max=1)
          q <- c(0,sort(randNum),1)
          for (j in  1:m){
            weightsR[j] <- q[j+1] - q[j]
          }
          
          # Generate criteria measurements + compute utility of each alternative
          
          value<-NULL
          for (a in 1:alt){
            measurements<-NULL
            for (c in 1:m){
              measurements[c] <- means[c,a] + standard[c,a]*rnorm(1,0,1)
            }
            measurements[1] <- exp(measurements[1]);
            #        value[a]=utility(measurements,weights)
            value[a]<-utility(measurements,weightsR,means,standard,pos)
          }
          
          # Ranking of the alternatives based on their utility scores
          
          #  y<-sort(value)
          tmp<-data.frame("value"=value,"y"=c(1:alt))
          tmp<-tmp[order(value,decreasing = T),]
          ranks<-tmp$y
          #[y,rank] = sort(value);
          #  rank = flipud(rank');
          #   ranks<-ranks[4:1]
          
          
          #Update counters
          
          for(a in 1:alt){
            
            rankaccept[ranks[a],a] <- rankaccept[ranks[a],a] + 1
            
            if(ranks[1]==a){central[a,] <- central[a,] + weightsR}
          }
        }
          
        
        #print(result$mcda)
        # Compute SMAA descriptive measures
        for(a in 1:alt){
          if(rankaccept[a,1]>0){for(c in 1:m){central[a,c] = central[a,c]/rankaccept[a,1]}}
        }
        rankaccept <- rankaccept/N
        result$rankaccept <- rankaccept
        result$central<-central

        result$showing_rankaccept<-result$rankaccept
        result$showing_rankaccept<-result$showing_rankaccept*100 #11/02/18 Makes R. accept. to range 0-100
        result$showing_central<-result$central
        result$showing_central<- result$showing_central*100 #11/02/18 Makes C. Weight to range 0-100
        
        #print(result$showing_rankaccept)
        #print(result$showing_central)
        value1$txtbackend<-10
        
        #for(i in 1:length(ps_list$result)){
        #  insertUI(
        #    selector = "#report",
        #    where = "beforeBegin",
        #    ui = textOutput(paste0("txt_result", i),
        #                      paste0(proplist[i]))
        #  )}
        
        incProgress(0.2,detail = paste("Preparing results"))
        show(selector = "#nav li a[data-value=Result]")
        updateTabsetPanel(session = session, inputId = "nav", "Result")
        show(selector = "#tabsResult li a[data-value=SMAA_result]")
        updateTabsetPanel(session = session, inputId = "tabsResult", "SMAA_result")
      })
      value2$test1<-0
    }
  })
  
  
  observeEvent(input$startweight, {
    updateTabsetPanel(session = session, inputId = "nav", "MCDA")
    show(selector = "#tabs li a[data-value=goalweight]")
    updateTabsetPanel(session = session, inputId = "tabs", selected = "goalweight")
  })
  
  observeEvent(input$gotodefineweights, {
    updateTabsetPanel(session = session, inputId = "nav", "MCDA")
    show(selector = "#tabs li a[data-value=goalweight]")
    updateTabsetPanel(session = session, inputId = "tabs", selected = "goalweight")
  })
  
  observeEvent(input$goaldone, {
    
    ##print(weight$goal)
    if(weight$tmpsum != 100){
      showModal(modalDialog(
        title = "The weights should sum up to 100.",footer = modalButton("Ok"),
        "Please go back and review the weights."
      ))
      
    }
    else{
      show(selector = "#tabs li a[data-value=PAweight]")
      weight$finalgoal<-weight$goal
      updateTabsetPanel(session = session, inputId = "tabs", selected = "PAweight")
      if(weight$goal[1]==0){
        hide(selector = "#tabsPA li a[data-value=HA]")
      }else{
        show(selector = "#tabsPA li a[data-value=HA]")
      }
      if(weight$goal[2]==0){
        hide(selector = "#tabsPA li a[data-value=WQ]")
      }else{
        show(selector = "#tabsPA li a[data-value=WQ]")
      }
      if(weight$goal[3]==0){
        hide(selector = "#tabsPA li a[data-value=LC]")
      }else{
        show(selector = "#tabsPA li a[data-value=LC]")
      }
      if(weight$goal[4]==0){
        hide(selector = "#tabsPA li a[data-value=CL]")
      }else{
        show(selector = "#tabsPA li a[data-value=CL]")
      }
      if(weight$goal[5]==0){
        hide(selector = "#tabsPA li a[data-value=EC]")
      }else{
        show(selector = "#tabsPA li a[data-value=EC]")
      }
      if(value2$newaddedweight==0){
        hide(selector = "#tabsPA li a[data-value=UA]")
      }
      else{
        show(selector = "#tabsPA li a[data-value=UA]")
      }
    }
  })
  
  observe({
    if (is.na(shinyValue("select_hab",nrow(hab_PA_Table))) || weight$goal[1]==0) {
      weight$HA<-factor(c("Zero","Zero","Zero","Zero","Zero"),levels = c("Zero","Low","Medium","High"))
    } else {
      weight$HA<-factor(shinyValue("select_hab",nrow(hab_PA_Table)),levels = c("Zero","Low","Medium","High"))
    }
    if (is.na(shinyValue("select_wq",nrow(wq_PA_Table)))|| weight$goal[2]==0) {
      weight$WQ<-factor(c("Zero","Zero","Zero","Zero","Zero"),levels = c("Zero","Low","Medium","High"))
    } else {
      weight$WQ<-factor(shinyValue("select_wq",nrow(wq_PA_Table)),levels = c("Zero","Low","Medium","High"))
    }
    if (is.na(shinyValue("select_lcmr",nrow(lcmr_PA_Table)))|| weight$goal[3]==0) {
      weight$LC<-factor(c("Zero","Zero","Zero"),levels = c("Zero","Low","Medium","High"))
    } else {
      weight$LC<-factor(shinyValue("select_lcmr",nrow(lcmr_PA_Table)),levels = c("Zero","Low","Medium","High"))
    }
    if (is.na(shinyValue("select_commres",nrow(commres_PA_Table)))|| weight$goal[4]==0) {
      weight$CL<-factor(c("Zero","Zero","Zero","Zero","Zero"),levels = c("Zero","Low","Medium","High"))
    } else {
      weight$CL<-factor(shinyValue("select_commres",nrow(commres_PA_Table)),levels = c("Zero","Low","Medium","High"))
    }
    if (is.na(shinyValue("select_gulfecon",nrow(gulfecon_PA_Table)))|| weight$goal[5]==0) {
      weight$EC<-factor(c("Zero","Zero","Zero","Zero"),levels = c("Zero","Low","Medium","High"))
    } else {
      weight$EC<-factor(shinyValue("select_gulfecon",nrow(gulfecon_PA_Table)),levels = c("Zero","Low","Medium","High"))
    }
    if (!is.na(shinyValue("select_ua",1))) {
      weight$UA<-factor(shinyValue("select_ua",length(result$newaddattr$Goals)),levels = c("Zero","Low","Medium","High"))
    } 
    
  })
  
  observeEvent(input$HAmove, {
    
    #session$sendCustomMessage('unbind-DT', 'hab_PA_measures_Table')
    ##print(weight$HA)
    ##print(as.numeric(weight$HA))
    updateTabsetPanel(session = session, inputId = "tabsPA", selected = "WQ")
    
  })
  
  observeEvent(input$WQmove, {
    ##print(as.numeric(weight$WQ))
    #session$sendCustomMessage('unbind-DT', 'wq_PA_measures_Table')
    updateTabsetPanel(session = session, inputId = "tabsPA", selected = "LC")
  })
  
  
  observeEvent(input$LCmove, {
    ##print(as.numeric(weight$LC))
    #session$sendCustomMessage('unbind-DT', 'lcmr_PA_measures_Table')
    updateTabsetPanel(session = session, inputId = "tabsPA", selected = "CL")
  })
  
  observeEvent(input$CLmove, {
    ##print(as.numeric(weight$CL))
    #session$sendCustomMessage('unbind-DT', 'commres_PA_measures_Table')
    updateTabsetPanel(session = session, inputId = "tabsPA", selected = "EC")
  })
  
  observeEvent(input$ECmove, {
    ##print(as.numeric(weight$EC))
    #session$sendCustomMessage('unbind-DT', 'gulfecon_PA_measures_Table')
    if(value2$newaddedweight>0){
      updateTabsetPanel(session = session, inputId = "tabsPA", selected = "UA")
    }
    else{
      updateTabsetPanel(session = session, inputId = "tabsPA", selected = "Weights Review")
    }
  })
  
  observeEvent(input$UAmove, {
    ##print(as.numeric(weight$CL))
    #session$sendCustomMessage('unbind-DT', 'UserAdded_PA_measures_Table')
    updateTabsetPanel(session = session, inputId = "tabsPA", selected = "Weights Reivew")
  })
  
  observeEvent(input$Weightsdone, {
    #EC<-rep(0,ncol(result$matrix))
    HA_hf<-result$matrix[1:5,]*((as.numeric(weight$HA)-1)/3)
    #print(class(HA_hf))
    WQ_hf<-result$matrix[6:10,]*((as.numeric(weight$WQ)-1)/3)
    LC_hf<-result$matrix[11:14,]*((as.numeric(weight$LC)-1)/3) 
    CL_hf<-result$matrix[15:18,]*((as.numeric(weight$CL)-1)/3)
    EC_hf<-result$matrix[19:22,]*((as.numeric(weight$EC)-1)/3)
    ##print(HA_hf)
    if(value2$newaddedweight>0){
      result$newaddattr_weight<-result$newaddattr[,-(1:2)]
      for(i in 1:nrow(result$newaddattr_weight)){
        tmp<-as.numeric(result$newaddattr_weight[i,])
        tmp<-tmp*(as.numeric(weight$UA[i])-1)/3
        if(result$newaddattr$Goals[i]=="HA"){
          HA_hf<-rbind(HA_hf,tmp)
        }
        if(result$newaddattr$Goals[i]=="WQ"){
          WQ_hf<-rbind(WQ_hf,tmp)
        }
        if(result$newaddattr$Goals[i]=="LCMR"){
          LC_hf<-rbind(LC_hf,tmp)
        }
        if(result$newaddattr$Goals[i]=="CL"){
          CL_hf<-rbind(CL_hf,tmp)
        }
        if(result$newaddattr$Goals[i]=="EC"){
          EC_hf<-rbind(EC_hf,tmp)
        }
        
      }
      
      
    }
    
    result$final_rank<- rbind(colSums(HA_hf),colSums(WQ_hf),colSums(LC_hf),colSums(CL_hf),colSums(EC_hf))
    
    result$final_rank1<-rbind(result$final_rank*weight$goal/100,colSums(result$final_rank*weight$goal/100))
    result$final_rank1<-cbind(result$final_rank1,c(weight$goal,100)/100)
    
    colnames(result$final_rank)<- proplist[1:ncol(result$final_rank)]
    rownames(result$final_rank)<- c("Habitat","Water Quality & Quantity","Living Coastal Marine Resources", "Community Resilience","Gulf Economy") 
    colnames(result$final_rank1)<- c(proplist[1:ncol(result$final_rank)],"Weights")
    rownames(result$final_rank1)<- c("Habitat","Water Quality & Quantity","Living Coastal Marine Resources", "Community Resilience","Gulf Economy","Total Sum") 
    ##print(colSums(WQ_hf))
    ##print(rbind(colSums(HA_hf),colSums(WQ_hf),colSums(WQ_hf),colSums(LC_hf),colSums(CL_hf),EC))
    ##print(result$final_rank)
    result$final_rank_1<-colSums(result$final_rank*(weight$goal))/100
    ##print(result$final_rank_1)
    ##print(class(result$final_rank_1))
    show(selector = "#nav li a[data-value=Result]")
    updateTabsetPanel(session = session, inputId = "nav", "Result")
    show(selector = "#tabsResult li a[data-value=Weights_result]")
    updateTabsetPanel(session = session, inputId = "tabsResult", "Weights_result")
    
  })
  
  
  
  output$resultpresent1<-renderUI({
    #tableOutput("showing_rankaccept")
    #plotOutput("showing_plot")
    #plotlyOutput("showing_plot")
    selectInput("selectproposal","Select a proposal: ", choices = proplist[1:nrow(result$showing_central)],selected = proplist[1])
  })
  
  output$resultpie<-renderUI({
    plotlyOutput("showingpie")
  })
  
  output$resultpresent3<-renderUI({
    #tableOutput("showing_central")
    plotOutput("showing_plot2")
  })
  
  output$resultpresent2<-renderUI({
    dataTableOutput("showing_weight_result")
  })
  
  output$defaultresult2<-renderUI({
    dataTableOutput("default_weight_result")
  })
  


  output$resultpresent4<-renderUI({
    #tableOutput("showing_central"
    tagList(
      plotlyOutput("showing_plot3"),
    br(),
    em("The figure above shows the conservation score based on user defined weighting."),
    br()
    )
    
  })
  
  output$defaultresult4<-renderUI({
    #tableOutput("showing_central")
    tagList(
      plotlyOutput("showing_plot4"),
      em("The figure above shows the conservation score based on default weighting."),
      br()
    )
    
  })
  #output$showing_rankaccept<-renderTable(expr=result$showing_rankaccept,rownames = T,colnames=T,striped=T)
  #output$showing_central<-renderTable(expr=result$showing_central,rownames = T,colnames = T,striped=T) 
  
  output$showing_weight_result<-renderDataTable(datatable(result$final_rank1,options = list(searching = FALSE,paging = FALSE))%>%
                                                  formatRound(proplist[1:ncol(result$final_rank1)-1], 2)%>%
                                                  formatPercentage('Weights', 0))
  output$default_weight_result<-renderDataTable(datatable(result$default_rank1,options = list(searching = FALSE,paging = FALSE))%>%
                                                  formatRound(proplist[1:ncol(result$default_rank1)-1], 2)%>%
                                                  formatPercentage('Weights', 0))

  
  output$showing_plot3<-renderPlotly({
    
    xform <- list(categoryorder = "array",
                  categoryarray = names(result$final_rank_1))
    plot_ly(
      x = names(result$final_rank_1),
      y = result$final_rank_1,
      type = "bar")%>%
      layout(title = "Comparison Considering Goal Weights",xaxis = xform,yaxis = list(title="Consrevation score"),width = 600, autosize=F)%>%
      config(displayModeBar = F)
  })
  
  
  output$barportfolio<-renderPlotly({
    data<-result$showing_matrix_portfolio
    
    coln_withicon<-paste0(c("Habitat", "Water Quality & Quantity","Living Coastal Marine Resources","Community Resilience","Gulf Economy"))
    data<-result$showing_matrix_portfolio
    #print("showing_matrix_portfolio")
    #print(result$showing_matrix_portfolio)
    habitat<-colSums(data[1:5,])/5
    WaterQuality<-colSums(data[6:10,])/5
    LCMR<-colSums(data[11:14,])/4
    CL<-colSums(data[15:18,])/4
    Economy<-colSums(data[19:22,])/4
    data<-rbind(habitat,WaterQuality,LCMR,CL,Economy)
    #print(data)
    
    
    data<-t(data)
    data<-colSums(data)
    #print(data)
    p<-plot_ly(
      x = c("Habitat", "Water Quality & Quantity","Living Coastal & Marine Resources","Community Resilience", "Gulf Economy"),
      y = data,
      name = "Summary Report",
      type = "bar"
    )
    p
  })
  
  
  
  
  output$showing_plot4<-renderPlotly({
    xform <- list(categoryorder = "array",
                  categoryarray = names(result$default_rank_1))
    plot_ly(
      x = names(result$default_rank_1),
      y = result$default_rank_1,
      type = "bar")%>%
      layout(title = "Comparison Considering Default Goal Weights",xaxis = xform,yaxis = list(title="Consrevation score"),width = 600, autosize=F)%>%
      config(displayModeBar = F)
  })
  
  output$showingpie<-renderPlotly({
    ranking<-c("Rank 1","Rank 2","Rank 3","Rank 4","Rank 5","Rank 6","Rank 7","Rank 8","Rank 9","Rank 10")
    data<-result$showing_rankaccept[match(input$selectproposal,proplist),]
    #print(data)
    p <- plot_ly(labels = ranking[1:nrow(result$showing_central)], values = data, type = 'pie',
                 marker = list(colors = collist_rgb[1:nrow(result$showing_central)],
                               line = list(color = '#FFFFFF', width = 1))
                 ) %>%
      layout(title = '',
             annotations = list(x=0.5,y=-0.1,text = paste0('Ranking summary for ',input$selectproposal),showarrow = F),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,range=c(0, 1)),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,range=c(0, 1))
             )%>%
      config(displayModeBar = F)
    p
  })
  
  
  #output$showing_plot<-renderPlot({
  #  #print(result$showing_rankaccept)
  #  #print(class(result$showing_rankaccept))
  #  ranking<-c("Rank 1","Rank 2","Rank 3","Rank 4","Rank 5","Rank 6","Rank 7","Rank 8","Rank 9","Rank 10")
  #  barplot(result$showing_rankaccept,main="Rank Acceptability", xlim = c(0, ncol(result$showing_rankaccept) + 1),
  #          xlab="",names=result$rankaccept_altlist,ylab="%",
  #          col=colorlist1[1:length(result$rankaccept_altlist)])
  #  legend("topright", 
  #         legend = ranking[1:length(result$rankaccept_altlist)], 
  #         fill = colorlist1[1:length(result$rankaccept_altlist)])
  #})
  
  
  #output$showing_plot<-renderPlotly({
  #  data<-as.data.frame(t(result$showing_rankaccept))
  #  y<-proplist[1:ncol(result$showing_matrix)]
  #  data<-cbind(as.data.frame(y),data)
  #  top_labels <- c('Rank first', 'Rank last')
  #  color<-
  #    c('rgba(120,49,43,0.8)',
  #      'rgba(140,57,49,0.8)',
  #      'rgba(159,64,55,0.8)',
  #      'rgba(179,71,61,0.8)',
  #      'rgba(193,82,72,0.8)',
  #      'rgba(200,100,90,0.8)',
  #      'rgba(207,117,109,0.8)',
  #      'rgba(214,135,128,0.8)',
  #      'rgba(220,153,147,0.8)',
  #      'rgba(227,171,166,0.8)')
  #  p<-plot_ly(data, x = data[,2], y = ~y, type = 'bar', orientation = 'h',
  #             marker = list(color = color[1],
  #             line = list(color = 'rgb(248, 248, 249)', width = 1)),hoverinfo="none")%>%
  #    config(displayModeBar = F) %>% 
  #    layout(xaxis = list(title = "",
  #                        showgrid = FALSE,
  #                        showline = FALSE,
  #                        showticklabels = FALSE,
  #                        zeroline = FALSE,
  #                        domain = c(0.15, 1)),
  #           yaxis = list(title = "",
  #                        showgrid = FALSE,
  #                        showline = FALSE,
  #                        showticklabels = FALSE,
  #                        zeroline = FALSE),
  #           barmode = 'stack',
  #           paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
  #           margin = list(l = 120, r = 10, t = 140, b = 80),
  #           showlegend = FALSE)%>%
  #    add_annotations(xref = 'x', yref = 'paper',
  #                    x = c(10,90),
  #                    y = 1.15,
  #                    text = top_labels,
  #                    font = list(family = 'Arial', size = 12,
  #                                color = 'rgb(67, 67, 67)'),
  #                    showarrow = FALSE)%>%
  #    add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
  #                    xanchor = 'right',
  #                    text = data[,1],
  #                    font = list(family = 'Arial', size = 12,
  #                                color = 'rgb(67, 67, 67)'),
  #                    showarrow = FALSE, align = 'right')
  #  for(i in 3:ncol(data)){
  #    p<-add_trace(p,x = data[,i], marker = list(color = color[i-1]), hoverinfo="none")
  #  }      
  #  p
  #})
  
  
  
  
  output$showing_plot2<-renderPlot({
    #print(result$showing_central)
    ylabel<-c(0,10,20,30,40,50,60,70,80,90,100)
    plot(result$showing_central[1,],type='b',ylim=c(0,100),col="green",lwd=2,
         main="Central Weights",ylab="Goal Weights (%)",
         xlab="",xaxt="n")
    axis(side=1,at=c(1:length(criteria_labels)),
         labels=criteria_labels,srt=45,xpd=T)
    axis(side=2,at=ylabel)
    for(j in 2:nrow(result$showing_central)){
      lines(result$showing_central[j,],type='b',ylim=c(0,1),col=colorlist1[j])
    }
    legend("topleft",
           legend=proplist[1:ncol(result$showing_matrix)],
           col=colorlist1[1:ncol(result$showing_matrix)], lty=1,lwd=2, cex=1)
  })
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      spatial_footprint<-do.call(rbind,ps_list$result)
      spatial_footprint$proposal<-1:length(spatial_footprint$geometry)
      value1$proplist<-proplist
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(#n = input$threshold, 
        #r= result$final_rank_1, 
        footprint=spatial_footprint,
        rankmatrix=result$showing_rankaccept,
        rankalt=result$rankaccept_altlist,
        centralweights =result$showing_central,
        rawdata= as.data.frame(result$showing_matrix),
        centralalt= result$showing_matrix
        #,
        #proplist_final= value1$proplist
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file, 
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$report1 <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report1.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      weights<-c(as.character(weight$HA),as.character(weight$WQ),as.character(weight$LC),as.character(weight$CL),as.character(weight$EC))
      tmp<-cbind(result$showing_matrix,weights)
      spatial_footprint<-do.call(rbind,ps_list$result)
      spatial_footprint$proposal<-1:length(spatial_footprint$geometry)
      tempReport <- file.path(tempdir(), "report1.Rmd")
      file.copy("report1.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(#n = input$threshold, 
        #r= result$final_rank_1, 
        footprint=spatial_footprint,
        scorebygoal=result$final_rank,
        finalscore=result$final_rank_1,
        table=tmp,
        weightstable =  GWtable()
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file, 
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
 # output$osdownload <- downloadHandler(
#    # For PDF output, change this to "report.pdf"
#   filename = "report.html",
#    content = function(file) {
#      # Copy the report file to a temporary directory before processing it, in
#      # case we don't have write permissions to the current working dir (which
#      # can happen when deployed).
#      spatial_footprint<-ps_list_os$result
#      spatial_footprint$proposal<-1
#      tempReport <- file.path(tempdir(), "report2.Rmd")
#      file.copy("report2.Rmd", tempReport, overwrite = TRUE)
#      
#      params <- list(#n = input$threshold, 
#        #r= result$final_rank_1, 
#        footprint=spatial_footprint,
#        
#        rawdata= as.data.frame(result_os$showing_matrix)
#        #,
#        #proplist_final= value1$proplist
#      )
      
#      # Knit the document, passing in the `params` list, and eval it in a
#      # child of the global environment (this isolates the code in the document
#      # from the code in this app).
#      rmarkdown::render(tempReport, output_file = file, 
#                        params = params,
#                        envir = new.env(parent = globalenv())
#      )
#    }
#  )
  
  output$osdownload1 <- downloadHandler(
    
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      spatial_footprint<-ps_list_os$result
      spatial_footprint$proposal<-1
      tempReport <- file.path("./reporttesting/CPTReports.Rmd")
      #file.copy("./reporttesting/CPTReports.Rmd", tempReport, overwrite = TRUE)
      
      #####Table of information to be referenced in CPT reports#############################
      ##All elements start with 'plan_'
      ##elements that report names end with 'name'
      ##elements that report numbers end with 'num'
      ##elements that report percentages end with 'perc'
      ##elements that report any other value will end with the unit that value is reported in, such as 'acres' or 'sq meters'
      
      report_table<-data.frame("plan_Name"=proplist_os,              #Proposed area name / IN APP
                               #"plan_PadUSname"= "Grand Bay NERR",   #Connectivity w/PAD-US / NOT AVAILABLE RIGHT NOW 
                               "plan_acres"= format(round(result_os$showing_matrix[1]*247.105,0),scientific=F,trim = T,big.mark = ","),                   #Proposed Area of Conservation / IN APP
                               "plan_Habitatnum"= 1,                 #Number of Habitats within proposed area  / NOT AVAILABLE RIGHT NOW **Get number of tier 1 classes by zonal stats number of unique?
                               "plan_strucHubperc"= result_os$showing_matrix[3],              #% of proposed Area comprised of hubs & corridors / IN APP
                               "plan_TEnum"= result_os$showing_matrix[11],                      #Number of T&E Species within proposed area / IN APP
                               "plan_TEcrithabnum"= 100,               #Number of T&E Species with critical habitat in proposed area / NOT AVAILABLE RIGHT NOW
                               "plan_TEperc"= 500,                     #% of critical habitat within proposed area for a T&E species / IN APP, but not for ind. species
                               "plan_HPCperc"=round(result_os$showing_matrix[5],0),                 #% of proposed area comprised of High Priority Habitat / IN APP
                               "plan_Worklandsperc"= round(result_os$showing_matrix[19],0),              #% of proposed area comprised of High Priority Working Lands / IN APP
                               "plan_ImpairedWS"=round(result_os$showing_matrix[6],0),                #Acres of proposed area comprised of impaired watershed / IN APP             #% of proposed area comprised of impaired watershed / IN APP
                               "plan_QpChange"= ifelse(result_os$showing_matrix[7]>=1,"no change or a decline",
                                                       ifelse(result_os$showing_matrix[7]>=0.75,"minimal (0-1% increase)",
                                                              ifelse(result_os$showing_matrix[7]>=0.5,"moderate (1-5% increase)",
                                                                     ifelse(result_os$showing_matrix[7]>=0.25,"significant (5-10% increase)",
                                                                            ifelse(result_os$showing_matrix[7]>=0,"very significant (10-50% increase)","Insufficient Data"))))),
                               "plan_Urbanthreat"=ifelse(result_os$showing_matrix[4]>0.66,"high",
                                                         ifelse(result_os$showing_matrix[4]>0.33,"medium",
                                                                ifelse(result_os$showing_matrix[4]>0,"low","no"))),            #potential threat of development within proposed area (low/med/high) / IN APP
                               "plan_commercialfishing"=ifelse(result_os$showing_matrix[20]==1,"Low",
                                                               ifelse(result_os$showing_matrix[20]==2,"Medium",
                                                                      ifelse(result_os$showing_matrix[20]==3,"Medium-High",
                                                                             ifelse(result_os$showing_matrix[20]==4,"High","Insufficient Data")))),
                               "plan_recreationfishing"=ifelse(result_os$showing_matrix[21]==1,"Low",
                                                               ifelse(result_os$showing_matrix[21]==2,"Medium",
                                                                      ifelse(result_os$showing_matrix[21]==3,"Medium-High",
                                                                             ifelse(result_os$showing_matrix[21]==4,"High","Insufficient Data")))),
                               "plan_CV_slr"=ifelse(result_os$showing_matrix[15]==1,"High",
                                                    ifelse(result_os$showing_matrix[15]==0.75,"Medium-High",
                                                           ifelse(result_os$showing_matrix[15]==0.5,"Medium",
                                                                  ifelse(result_os$showing_matrix[15]==.25,"Low","Insufficient Data")))),
                               "plan_CV_flooding"=ifelse(result_os$showing_matrix[16]==1,"High",
                                                         ifelse(result_os$showing_matrix[16]==0.75,"Medium-High",
                                                                ifelse(result_os$showing_matrix[16]==0.5,"Medium",
                                                                       ifelse(result_os$showing_matrix[16]==.25,"Low","Insufficient Data")))),
                               
                               "plan_NHA"=result_os$showing_matrix[14],
                               "plan_historicplaces"=result_os$showing_matrix[13],
                               "plan_biodiversity"=result_os$showing_matrix[9],
                               #"plan_streamabundance"=result_os$showing_matrix[7],
                            
                               
                               "plan_RSTORE_check"=ifelse(length(st_intersects(ps_list_os$result,SCA)[[1]])==0,0,1),                #is the AOI within RESTORE boundary?
                               "plan_Urbanthreat_check"=ifelse(result_os$showing_matrix[4]>0,1,0),           #is there an urban threat within the AOI?
                               "plan_PADUS_check"=ifelse(result_os$showing_matrix[2]>0,1,0),                 #is there protected land within 1 sq km?
                               "plan_crithab_check"=ifelse(result_os$showing_matrix[10]>0,1,0),               #is there critical habitat for a T&E in the AOI?
                               "plan_impairedwb_check"=ifelse(result_os$showing_matrix[6]>0,1,0),            #is there impaired waterbody in AOI?
                               "plan_historic_check"=ifelse(result_os$showing_matrix[13]>0,1,0),              #is there historic place within AOI?
                               "plan_heritage_check"=ifelse(result_os$showing_matrix[14]>0,1,0))              #is there heritage area within AOI?
      ##For elements with multiple items, read in separately with list(c())
      ##print(report_table)
      #T&E NAMES / NEED TO CHECK
      report_table$plan_TEname<-list(c("Gulf Sturgeon","Gopher Tortoise")) 
      
      #HIGH PRIORITY HABITAT NAMES / NOT AVAILABLE RIGHT NOW
      #report_table$plan_HPCname=list(c("estuarine emergent marsh","prairie depressional wetlands","upland prairie"))
      report_table$plan_HPCname=list(c("A","B","C","D"))
      
      #HIGH PRIORITY WORKING LANDS NAMES  / NOT AVAILABLE RIGHT NOW
      #report_table$plan_HPWLname=list(c("A","B","C","D"))
      #report_table$plan_HPWLname=list(c("longleaf pine","rangeland"))
      #report_table$plan_HPWLname=list(c("longleaf pine"))
      report_table$plan_HPWLname=list(c("[Names of High Priority Working Lands coming soon]"))
      
      #IMPAIRED WATERSHED NAMES / NOT AVAILABLE RIGHT NOW
      report_table$plan_Impairedname=list(c("1st impaired watershed","2nd impaired watershed"))
      
      #IMPAIRED WATERBODY NAMES / NOT AVAILABLE RIGHT NOW 
      #report_table$plan_Impairedwbodyname=list(c("McCrae Dead River","Tchoutacabouffa River","Wolf River"))
      #report_table$plan_Impairedwbodyname=list(c("McCrae Dead River","Tchoutacabouffa River"))
      report_table$plan_Impairedwbodyname=list(c("[Names of impaired waterbody coming soon]"))
      
      
      
      ##########PADUS##################
      report_table$PADUS_Check<-ifelse(report_table$plan_PADUS_check==1,paste("that is within 1 km of currently protected land, according to the PAD-US layer"),paste("that is not within 1 sq km of any known protected land"))
      ##########END PADUS
      
      #Statement on habitat diversity
      report_table$hab_abundance<-ifelse(report_table$plan_Habitatnum>=5,"a broad diversity of habitats including",
                                         ifelse(report_table$plan_Habitatnum>=3,"several distinct habitats including",
                                                "Number of tier-1 habitat classes coming soon"))
      
      #Statement for T&E species
      report_table$TE_text<-ifelse(report_table$plan_TEnum>=2,"and this area of interest supports habitat ranges for two federally listed species",
                                   ifelse(report_table$plan_TEnum==1,"and this area of interest supports habitat for the federally listed ",
                                          "and no federally endangered species are known to inhabit the project area"))
      
      #Statement for working lands
      #WL_list<-ifelse(length(report_table$plan_HPWLname[[1]])>2,
      #                paste(implode(report_table$plan_HPWLname[[1]][-length(report_table$plan_HPWLname[[1]])],sep=", "),
      #                      report_table$plan_HPWLname[[1]][length(report_table$plan_HPWLname[[1]])],sep = ", and "),
      #                ifelse(length(report_table$plan_HPWLname[[1]])==2,implode(report_table$plan_HPWLname[[1]],sep = " and "),
      #                report_table$plan_HPWLname[[1]]))
      report_table$WL_list<-ifelse(length(report_table$plan_HPWLname[[1]])>2,
                                   implode(report_table$plan_HPWLname[[1]],sep=", ",", and "),
                                   ifelse(length(report_table$plan_HPWLname[[1]])==2,
                                          implode(report_table$plan_HPWLname[[1]],sep=" and "),
                                          report_table$plan_HPWLname[[1]]))
      report_table$WL_perc<-report_table$plan_Worklandsperc
      report_table$WL_text<-ifelse(length(report_table$plan_HPWLname[[1]])>=2,
                                   paste("Conserving this area of interest would protect ",report_table$WL_list," working lands (about ",
                                         report_table$WL_perc," percent of the project area)",sep = ""),
                                   ifelse(report_table$plan_HPWLname[[1]]!=-99, 
                                          paste("Conserving this area of interest would also provide protection to working lands, ",
                                                "with ",report_table$WL_list," comprising roughly ",report_table$WL_perc," percent of the landscape",sep=""),
                                          "This area of interest provides no protection of working lands"))
      
      #Statement for Water Quality & Quantity
      #WQ_list<-ifelse(length(report_table$plan_Impairedwbodyname[[1]])>2,
      #                paste(implode(report_table$plan_Impairedwbodyname[[1]][-length(report_table$plan_Impairedwbodyname[[1]])],sep=", "),
      #                      report_table$plan_Impairedwbodyname[[1]][length(report_table$plan_Impairedwbodyname[[1]])],sep = ", and "),
      #                ifelse(length(report_table$plan_Impairedwbodyname[[1]])==2,implode(report_table$plan_Impairedwbodyname[[1]],sep = " and "),
      #                       report_table$plan_Impairedwbodyname[[1]]))
      report_table$WQ_list<-ifelse(length(report_table$plan_Impairedwbodyname[[1]])>2,
                                   implode(report_table$plan_Impairedwbodyname[[1]],sep=", ",", and "),
                                   ifelse(length(report_table$plan_Impairedwbodyname[[1]])==2,
                                          implode(report_table$plan_Impairedwbodyname[[1]],sep=" and "),
                                          report_table$plan_Impairedwbodyname[[1]]))
      report_table$WQ_perc<-paste(report_table$plan_Impairedperc, " percent of the project area)")
      report_table$WQ_text<-ifelse(length(report_table$plan_Impairedwbodyname[[1]])>=2,
                                   paste("This area of interest also buffers water flowing into waterbodies with known impairments (",report_table$WQ_list,") ",  
                                         "and preservation would allow this landscape to continue to provide such water quality protections",sep=""),
                                   ifelse(report_table$plan_Impairedwbodyname[[1]]!=-99,
                                          paste("This area of interest also buffers water flowing into the ", report_table$WQ_list,", a waterbody with known impairments, ",  
                                                "and preservation would allow this landscape to continue to provide such water quality protections",sep=""),
                                          "No waterways in or around this area of interest are considered impaired"))
      #Statement for Sea Level Rise Risk
      report_table$SLR_text<-ifelse(report_table$plan_CV_slr=="High",paste("In the future, the ",report_table$plan_Name, " area will be highly vulnerable
                                                                      to inundation due to sea level rise.",sep=""),
                                    ifelse(report_table$plan_CV_slr!="Insufficient Data",paste("In the future, the ",report_table$plan_Name, " area may be vulnerable to
                                                                               to inundation due to sea level rise.",sep=""),
                                           paste("There is insufficient data to determine if ",report_table$plan_Name, " will be vulnerable to 
                                               inundation due to sea level rise.",sep="")))
      #Statement for Threat of development
      report_table$Sleuth_text<-ifelse(report_table$plan_Urbanthreat=="no",paste(" ",report_table$plan_Name," is expected to have ", report_table$plan_Urbanthreat, " threat of development
                       by the year 2060 according to the SLEUTH urbanization model",sep=""),paste(" ",report_table$plan_Name," is expected to have a ", report_table$plan_Urbanthreat, " threat of development
                       by the year 2060 according to the SLEUTH urbanization model",sep=""))
      
      
      #ATTRIBUTES NOT IN APP BUT INCLUDED IN REPORT
      extrareport_table<-data.frame("plan_DesignUse"=result_os$matrix_context[2])
      extrareport_table$plan_DesignUse<-ifelse(extrareport_table$plan_DesignUse==1,"Public Drinking Supply",
                                               ifelse(extrareport_table$plan_DesignUse==.75,"Protection and Propagation Of Fish and Wildlife",
                                                      ifelse(extrareport_table$plan_DesignUse==.5,"Recreation",
                                                             ifelse(extrareport_table$plan_DesignUse==.25,"Agricultural, Industrial, Navigational and other purposes",
                                                                    ifelse(extrareport_table$plan_DesignUse==.1,"N/A","No Data Available")))))
      extrareport_table$plan_CauseImpaire<-result_os$matrix_context[1]
      
      ###################END REPORT TABLE###################################################
      
     
      report_table_1<-data.frame("AOI_Name"=proplist_os,                                                                                #DONE
                                 "AOI_Area"= format(round(result_os$showing_matrix[1]*247.105,0),scientific=F,trim = T,big.mark = ","), #DONE
                                 "Connectivity"= result_os$showing_matrix[3],                                                           #DONE
                                 "PADUS"= ifelse(result_os$showing_matrix[2]>0,1,0),                                                    #DONE
                                 "SLEUTH"= ifelse(result_os$showing_matrix[4]>0.66,"high",
                                                  ifelse(result_os$showing_matrix[4]>0.33,"medium",
                                                         ifelse(result_os$showing_matrix[4]>0,"low","no"))),
                                 "HPL_Num"= result_os$datatable_additional[4],                                                          #DONE
                                 "HPL_Perc"= round(result_os$showing_matrix[5],0),                                                     #DONE
                                 "Impaired_Perc"=round(result_os$showing_matrix[6],0),                                                  #DONE
                                 "Impaired_Name"=0,                                                                                     #PROGRESS/NOT READY
                                 #"Stream_Abund"=result_os$showing_matrix[7],                                                            #DONE                                                                                  
                                 "Qp_Change"= ifelse(result_os$showing_matrix[7]>=1,"no change or a decline",
                                                     ifelse(result_os$showing_matrix[7]>=0.75,"minimal (0-1% increase)",
                                                            ifelse(result_os$showing_matrix[7]>=0.5,"moderate (1-5% increase)",
                                                                   ifelse(result_os$showing_matrix[7]>=0.25,"significant (5-10% increase)",
                                                                          ifelse(result_os$showing_matrix[7]>=0,"very significant (10-50% increase)","Insufficient Data"))))),                                                                                        #PROGRESS/NEED TO FINISH HYDROLOGIC RESPONSE
                                 "Biodiversity"=ifelse(result_os$showing_matrix[11]>6,"high",
                                                       ifelse(result_os$showing_matrix[11]>3,"medium",
                                                              ifelse(result_os$showing_matrix[11]>0,"low",
                                                                     "NA"))),                                                            #DONE
                                 "TE_Perc"=result_os$showing_matrix[12],                                                                                           #PROGRESS/LIKELY 9
                                 "TE_Num"= result_os$showing_matrix[13],                                                                #DONE
                                 "TE_Name"=  result_os$datatable_additional[2],                                                                          #DONE
                                 "Light_Pollut"= ifelse(result_os$showing_matrix[14]>0.66,"high",
                                                        ifelse(result_os$showing_matrix[14]>0.33,"medium",
                                                               ifelse(result_os$showing_matrix[14]>0,"low","no"))),                     #DONE
                                 "Historic"= result_os$showing_matrix[15],                                                              #DONE
                                 "Heritage"= result_os$showing_matrix[16],                                                              #DONE
                                 "SOVI"= result_os$showing_matrix[17],                                                                  #DONE
                                 "Comm_Threat"= ifelse(result_os$showing_matrix[18]>=1,"high",
                                                       ifelse(result_os$showing_matrix[18]>=0.75,"medium-high",
                                                              ifelse(result_os$showing_matrix[18]>=0.5,"medium",
                                                                     ifelse(result_os$showing_matrix[18]>=0.25,"medium-low",
                                                                            ifelse(result_os$showing_matrix[18]>=0,"low","Insufficient Data"))))), #DONE
                                 "HPWL_Num"= result_os$datatable_additional[5],                                                                                         #DONE
                                 "HPWL_Perc"=result_os$datatable_additional[6],                                                                                        #DONE
                                 "C_Fish"= ifelse(result_os$showing_matrix[20]==1,"Low",
                                                  ifelse(result_os$showing_matrix[20]==2,"Medium",
                                                         ifelse(result_os$showing_matrix[20]==3,"Medium-High",
                                                                ifelse(result_os$showing_matrix[20]==4,"High","Insufficient Data")))),  #DONE
                                 "R_Fish"= ifelse(result_os$showing_matrix[21]==1,"Low",
                                                  ifelse(result_os$showing_matrix[21]==2,"Medium",
                                                         ifelse(result_os$showing_matrix[21]==3,"Medium-High",
                                                                ifelse(result_os$showing_matrix[121]==4,"High","Insufficient Data")))),  #DONE
                                 "A_and_R"= result_os$showing_matrix[22])  #DONE
      
      ##String Split TE Name
      te_names_split<-strsplit(result_os$datatable_additional[2],split = ",")
      HPL_Num_numeric<-as.numeric(as.character(report_table_1$HPL_Num))
      WL_split<-strsplit(result_os$datatable_additional[5],split=",")
      Evergreen<-ifelse(WL_split[[1]][1]!=0,paste0("Evergreen (",WL_split[[1]][1],"%)"),paste0(""))
      Cropland<-ifelse(WL_split[[1]][2]!=0,paste0("Cropland (",WL_split[[1]][2],"%)"),paste0(""))
      Pasture<-ifelse(WL_split[[1]][3]!=0,paste0("Pasture (",WL_split[[1]][3],"%)"),paste0(""))
      WL_full<-c(Evergreen,Cropland,Pasture)
      WL_final<-WL_full[WL_full != ""]
      #print(WL_final)
      ##Conditional Statements
      dat_1_1<-paste0("This report evaluates the ","**",report_table_1$AOI_Name,"**"," area of interest") 
      dat_2_1<-paste0(", approximately ","**",report_table_1$AOI_Area,"**"," acres of land") 
      dat_3_1<-paste0(", that is within 1 km of currently protected land, according to the PAD-US layer. ") 
      dat_3_2<-paste0(", that is not within 1 km of currently protected land. ")
      dat_4_1<-paste0(report_table_1$AOI_Name," also supports Connectivity of Natural Lands, as ",
                      report_table_1$Connectivity," percent of the area is classified as a hub or corridor by the EPA National Ecological Framework (NEF). ")
      dat_4_2<-paste0(report_table_1$AOI_Name," does not have any land classified as a hub or corridor by the EPA National Ecological Framework (NEF). ")
      dat_5_1<-paste0(report_table_1$AOI_Name," is expected to have a ",report_table_1$SLEUTH," threat of development by the year 2060, according to the SLEUTH urbanization model. ")
      dat_5_2<-paste0(report_table_1$AOI_Name," is currently urbanized. ")
      dat_5_3<-paste0("There is insufficient data to determine the future threat of development for ",report_table_1$AOI_Name,". ")
      dat_6_1<-paste0(report_table_1$AOI_Name," houses ","**",report_table_1$HPL_Num,"**"," habitats deemed high priority, ")
      dat_6_2<-paste0(report_table_1$AOI_Name," houses ",report_table_1$HPL_Num," habitat that is deemed high priority, ")
      dat_6_3<-paste0(report_table_1$AOI_Name," is not known to house any habitats deemed high priority. ")
      dat_7_1<-paste0("roughly ","**",report_table_1$HPL_Perc,"**"," **percent**"," of the area of interest. ")
      dat_8_1<-paste0("This area of interest also buffers water flowing into the ",report_table_1$Impaired_Name,
                      ", a waterbody with known impairments, and preservation would allow this landscape to continue to provide such water quality protections. ")
      dat_9_1<-paste0("Approximately ",report_table_1$Impaired_Perc," percent of the waterways within ",report_table_1$AOI_Name," are designated as impaired according to the EPA's 303(d) list. ")
      #dat_10_1<-paste0("This area of interest contains roughly ",report_table_1$Stream_Abund," km of stream according to the National Hydrography Dataset. ")
      #dat_10_2<-paste0("There are not any streams or rivers recognized within the area of interest.")
      dat_11_1<-paste0("Land-use change in ",report_table_1$AOI_Name," has resulted in ",report_table_1$Qp_Change," in hydrologic response to a standard rainfall event for this region. ")
      dat_11_2<-paste0("Land-use change in ",report_table_1$AOI_Name," has resulted in a ",report_table_1$Qp_Change," hydrologic response to a standard rainfall event for this region. ")
      dat_11_3<-paste0("There is insufficient data to determine the hydrologic response of ",report_table_1$AOI_Name," to land-use change.")
      dat_12_1<-paste0("The landscape of ",report_table_1$AOI_Name," has a ",report_table_1$Biodiversity," biodiversity index, in accordance with the methods used by Jenkins et. al, 2015. ")
      dat_12_2<-paste0("")
      dat_13_1<-paste0("Lands within ",report_table_1$AOI_Name," support roughly ",report_table_1$TE_Perc," of the critical habitat ranges for federally listed species. ")
      dat_13_2<-paste0("Lands within ",report_table_1$AOI_Name," are not known to support critical habitats for any federally listed species. ")
      dat_14_1<-paste0("Lands within ",report_table_1$AOI_Name," support habitat ranges for ",report_table_1$TE_Num," federally listed species, ")
      dat_14_2<-paste0("Lands within ",report_table_1$AOI_Name," support the habitat range of the ","**",report_table_1$TE_Name,"**",", a federally listed species. ")
      dat_14_3<-paste0("Lands within ",report_table_1$AOI_Name," are not known to support habitat ranges for any federally listed species. ")
      dat_15_1<-paste0("including the ",implode(te_names_split[[1]],sep=", ",", and "))
      dat_15_2<-paste0(" the ",implode(te_names_split[[1]],sep=" and "))
      dat_15_3<-paste0("")
      dat_16_1<-paste0(". ",report_table_1$AOI_Name," has a ",report_table_1$Light_Pollut," level of light pollution.")
      dat_16_2<-paste0(". There is no light pollution in the area of interest.")
      dat_17_1<-paste0(" The National Register of Historic Places indicates that there are ",report_table_1$Historic," historic places within or around the area of interest. ")
      dat_17_2<-paste0(" The National Register of Historic Places indicates that there is ",report_table_1$Historic," historic place within or around the area of interest. ")
      dat_17_3<-paste0(" No places listed under the National Register of Historic Places are known to exist within or around the area of interest. ")
      dat_18_1<-paste0(" About ",report_table_1$Heritage," percent of ",report_table_1$AOI_Name," is within a designated National Heritage Area. ")
      dat_18_2<-paste0(report_table_1$AOI_Name," is not within a designated National Heritage Area. ")
      dat_19_1<-paste0(" According to NOAA's Office for Coastal Management, the area of interest is nearby a community that is socially vulnerable.")
      dat_19_2<-paste0(" There is insufficient data to determine the social vulnerability of communities nearby ",report_table_1$AOI_Name,".")
      dat_20_1<-paste0(" ",report_table_1$AOI_Name," has a ",report_table_1$Comm_Threat," threat from coastal flooding and severe storm hazards.")
      dat_20_2<-paste0(" There is insufficient data to determine ",report_table_1$AOI_Name,"'s community threat index.")
      dat_21_1<-paste0(" Conserving this area of interest would also provide protection to working lands, with ",implode(WL_final,sep=", ",", and "))
      dat_21_2<-paste0(" Conserving this area of interest would also provide protection to working lands, with ",implode(WL_final,sep=" and "))
      dat_21_3<-paste0(" Conserving this area of interest would also provide protection to working lands, with ",WL_final," ")
      dat_21_4<-paste0(" No working lands are known to exist within the area of interest. ")
      dat_22_1<-paste0(" comprising about ",report_table_1$HPWL_Perc," percent of the landscape. ")
      dat_22_2<-paste0("")
      dat_23_1<-paste0(" The community in and around ",report_table_1$AOI_Name," has a ",report_table_1$C_Fish," level of commercial fishing reliance. ")
      dat_23_2<-paste0(" There is insufficient data to determine the commercial fishing reliance of the communities that ",report_table_1$AOI_Name," is associated with. ")
      dat_24_1<-paste0(" The community in and around ",report_table_1$AOI_Name," has a ",report_table_1$R_Fish," level of recreational fishing engagement. ")
      dat_24_2<-paste0(" There is insufficient data to determine the level recreational fishing engagement of the community that ",report_table_1$AOI_Name," is associated with.")
      dat_25_1<-paste0(" There are ",report_table_1$A_and_R," access points to natural areas within 25 km of ",report_table_1$AOI_Name,".")
      
      #Blank Statement if measure is zero-weighted
     
      
      report_table_2<-report_table_1
      report_table_2$AOI_Name<-dat_1_1
      report_table_2$AOI_Area<-dat_2_1
      report_table_2$PADUS<-ifelse(report_table_1$PADUS==1,dat_3_1,
                                  ifelse(report_table_1$PADUS==0,dat_3_2,paste0(".")))
      report_table_2$Connectivity<-ifelse(report_table_1$Connectivity>0,dat_4_1,dat_4_2)
      report_table_2$SLEUTH<-ifelse(report_table_1$SLEUTH!="no",dat_5_1,dat_5_2)
      report_table_2$HPL_Num<-ifelse(HPL_Num_numeric>1,dat_6_1,ifelse(HPL_Num_numeric==1,dat_6_2,dat_6_3))
      report_table_2$HPL_Perc<-ifelse(HPL_Num_numeric>=1,dat_7_1,paste0(""))
      #report_table_2$HPL_Num<-ifelse(report_table_1$HPL_Num>=2,dat_6_1,
      #                               ifelse(report_table_1$HPL_Num==1,dat_6_2,dat_6_3))
      #report_table_2$HPL_Perc<-ifelse(report_table_1$HPL_Num>=1,dat_7_1,paste0(""))
      report_table_2$Impaired_Perc<-dat_9_1
      report_table_2$Impaired_Name<-paste0("")
      #report_table_2$Impaired_Name<-ifelse(report_table_1$Impaired_Name!=0,paste0("This area of interest also buffers water flowing into the ",report_table_1$Impaired_Name,
       #                                                                           ", a waterbody with known impairments, and preservation would allow this landscape to continue to provide such water quality protections."),paste0(""))
      #report_table_2$Stream_Abund<-ifelse(report_table_1$Stream_Abund>0,dat_10_1,dat_10_2)
      report_table_2$Qp_Change<-ifelse(report_table_1$Qp_Change=="no change or a decline",dat_11_1,ifelse(report_table_1$Qp_Change=="Insufficient Data",dat_11_3,dat_11_2))
       #report_table_2$Qp_Change<-ifelse(report_table_1$Qp_Change=="insufficient data",paste0("There is insufficient data to determine the hydrologic response of ",report_table_1$AOI_Name," to land-use change."),
      #                                 paste0("Land-use change in ",report_table_1$AOI_Name," has resulted in a ",report_table_1$Qp_Change," hydrologic response to a standard rainfall event for this region."))
      report_table_2$Biodiveristy<-ifelse(report_table_1$Biodiversity!="NA",dat_12_1,dat_12_2)
      report_table_2$TE_Perc<-ifelse(report_table_1$TE_Perc>0,dat_13_1,dat_13_2)
      report_table_2$TE_Num<-ifelse(report_table_1$TE_Num>=2,dat_14_1,
                                    ifelse(report_table_1$TE_Num==1,dat_14_2,
                                           dat_14_3))
      report_table_2$TE_Name<-ifelse(report_table_1$TE_Num>2,dat_15_1,
                                     ifelse(report_table_1$TE_Num==2,dat_15_2,
                                            dat_15_3))
      report_table_2$Light_Pollut<-ifelse(report_table_1$Light_Pollut==0,dat_16_2,dat_16_1)
      report_table_2$Historic<-ifelse(report_table_1$Historic>1,dat_17_1,
                                      ifelse(report_table_1$Historic==1,dat_17_2,dat_17_3))
      report_table_2$Heritage<-ifelse(report_table_1$Heritage!=0,dat_18_1,dat_18_2)
      report_table_2$SOVI<-ifelse(report_table_1$SOVI>0,dat_19_1,dat_19_2)
      report_table_2$Comm_Threat<-ifelse(report_table_1$Comm_Threat!=0,dat_20_1,dat_20_2)
      report_table_2$HPWL_Num<-ifelse(length(WL_final)==3,dat_21_1,
                                      ifelse(length(WL_final)==2,dat_21_2,
                                             ifelse(length(WL_final)==1,dat_21_3,dat_21_4)))
      report_table_2$HPWL_Perc<-ifelse(result_os$datatable_additional[3]>0,dat_22_1,dat_22_2)
     # report_table_2$HPWL_Num<-ifelse(report_table_1$HPWL_Num>=1,paste0("Conserving this area of interest would also provide protection to working lands, with [Names of WL] "),
    #                                  paste0("No working lands are known to exist within the area of interest."))
     # report_table_2$HPWL_Perc<-ifelse(report_table_1$HPWL_Num>=1,paste0("comprising about ",report_table_1$HPWL_Perc," percent of the landscape."),
      #                                 paste0(""))
      report_table_2$C_Fish<-ifelse(report_table_1$C_Fish!="Insufficient Data",dat_23_1,dat_23_2)
      report_table_2$R_Fish<-ifelse(report_table_1$R_Fish!="Insufficient Data",dat_24_1,dat_24_2)
      report_table_2$A_and_R<-dat_25_1
      
      report_table_2<<-report_table_2
      
      params <- list(#n = input$threshold,
        report_table=report_table,
        report_table_1=report_table_1,
        report_table_2=report_table_2,
        extrareport_table=extrareport_table,
        footprint=spatial_footprint
      )
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file, 
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  ####MultiProject Report  
  output$download2 <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      spatial_footprint<-do.call(rbind,ps_list$result)
      spatial_footprint$proposal<-1:length(spatial_footprint$geometry)
      tempReport <- file.path("./reporttesting/CPTReportsMulti.Rmd")
      #file.copy("./reporttesting/CPTReports.Rmd", tempReport, overwrite = TRUE)
      
      #####Table of information to be referenced in CPT reports#############################
      ##All elements start with 'plan_'
      ##elements that report names end with 'name'
      ##elements that report numbers end with 'num'
      ##elements that report percentages end with 'perc'
      ##elements that report any other value will end with the unit that value is reported in, such as 'acres' or 'sq meters'
      
      report_table<-data.frame("plan_Name"=proplist[1:length(ps_list$result)],              #Proposed area name / IN APP
                               #"plan_PadUSname"= "Grand Bay NERR",   #Connectivity w/PAD-US / NOT AVAILABLE RIGHT NOW 
                               "plan_acres"= format(round(result$showing_matrix[1,]*247.105,0),scientific=F),                   #Proposed Area of Conservation / IN APP
                               "plan_Habitatnum"= 3,                 #Number of Habitats within proposed area  / NOT AVAILABLE RIGHT NOW **Get number of tier 1 classes by zonal stats number of unique?
                               "plan_strucHubperc"= result$showing_matrix[3,]*100,              #% of proposed Area comprised of hubs & corridors / IN APP
                               "plan_TEnum"= result$showing_matrix[13,],                      #Number of T&E Species within proposed area / IN APP
                               "plan_TEcrithabnum"= 100,               #Number of T&E Species with critical habitat in proposed area / NOT AVAILABLE RIGHT NOW
                               "plan_TEperc"= 500,                     #% of critical habitat within proposed area for a T&E species / IN APP, but not for ind. species
                               "plan_HPCperc"=round(result$showing_matrix[5,]*100,0),                 #% of proposed area comprised of High Priority Habitat / IN APP
                               "plan_Worklandsperc"= round(result$showing_matrix[19,]*100,0),              #% of proposed area comprised of High Priority Working Lands / IN APP
                               "plan_ImpairedWS"=round(result$showing_matrix[6,]*100,0),                 #% of proposed area comprised of impaired watershed / IN APP
                               "plan_Urbanthreat"=ifelse(result$showing_matrix[4,]>0.66,"high",
                                                         ifelse(result$showing_matrix[4,]>0.33,"medium",
                                                                ifelse(result$showing_matrix[4,]>0,"low","no"))),            #potential threat of development within proposed area (low/med/high) / IN APP
                               "plan_commercialfishing"=result$showing_matrix[20,],
                               "plan_recreationfishing"=result$showing_matrix[21,],
                               "plan_Urbanthreat_check"=ifelse(result$showing_matrix[4,]>0,1,0),           #is there an urban threat within the AOI?
                               "plan_PADUS_check"=ifelse(result$showing_matrix[2,]>0,1,0),                 #is there protected land within 1 sq km?
                               "plan_crithab_check"=ifelse(result$showing_matrix[12,]>0,1,0),               #is there critical habitat for a T&E in the AOI?
                               "plan_impairedwb_check"=ifelse(result$showing_matrix[6,]>0,1,0),            #is there impaired waterbody in AOI?
                               "plan_historic_check"=ifelse(result$showing_matrix[15,]>0,1,0),              #is there historic place within AOI?
                               "plan_heritage_check"=ifelse(result$showing_matrix[16,]>0,1,0))              #is there heritage area within AOI?
      ##NUMBER OF PROJECTS
      Project_num<-length(report_table$plan_Name)
      
      ##For elements with multiple items, read in separately with list(c())
      #print(report_table)
      for(i in 1:length(report_table$plan_Name)){
        report_table$plan_RSTORE_check[i]=ifelse(length(st_intersects(ps_list$result[[i]],SCA)[[1]])==0,0,1)
      }
      #T&E NAMES / NEED TO CHECK
      #report_table$plan_TEname<-list(c("Gulf Sturgeon","Gopher Tortoise")) 
      
      #HIGH PRIORITY HABITAT NAMES / NOT AVAILABLE RIGHT NOW
      #report_table$plan_HPCname=list(c("estuarine emergent marsh","prairie depressional wetlands","upland prairie"))
      #report_table$plan_HPCname=list(c("A","B","C","D"))
      
      #HIGH PRIORITY WORKING LANDS NAMES  / NOT AVAILABLE RIGHT NOW
      #report_table$plan_HPWLname=list(c("A","B","C","D"))
      #report_table$plan_HPWLname=list(c("longleaf pine","rangeland"))
      #report_table$plan_HPWLname=list(c("longleaf pine"))
      #report_table$plan_HPWLname=list(c(-99))
      
      #IMPAIRED WATERSHED NAMES / NOT AVAILABLE RIGHT NOW
      #report_table$plan_Impairedname=list(c("1st impaired watershed","2nd impaired watershed"))
      
      #IMPAIRED WATERBODY NAMES / NOT AVAILABLE RIGHT NOW 
      #report_table$plan_Impairedwbodyname=list(c("McCrae Dead River","Tchoutacabouffa River","Wolf River"))
      #report_table$plan_Impairedwbodyname=list(c("McCrae Dead River","Tchoutacabouffa River"))
      #report_table$plan_Impairedwbodyname=list(c("McCrae Dead River"))
      
      #########END OF BUILDING TABLE#####################
      
      ##########PADUS##################
      #PADUS_Check<-ifelse(report_table$plan_PADUS_check==1,
      #                    paste("that is within 1 sq km of currently protected land, according to the PAD-US layer"),
      #                    paste("that is not within 1 sq km of any known protected land"))
      ##########END PADUS
      
      #Statement on habitat diversity
      #hab_abundance<-ifelse(report_table$plan_Habitatnum>=5,"a broad diversity of habitats including",
      #                      ifelse(report_table$plan_Habitatnum>=3,"several distinct habitats including",""))
      
      #Statement for T&E species
      #TE_text<-ifelse(report_table$plan_TEnum>=2,"This area of interest supports habitat ranges for two federally listed species",
      #                ifelse(report_table$plan_TEnum==1,"This area of interest supports habitat for the federally listed ",
      #                       "No federally endangered species are known to inhabit the project area."))
      
      #Statement for working lands
      #WL_list<-ifelse(length(report_table$plan_HPWLname[[1]])>2,
      #                paste(implode(report_table$plan_HPWLname[[1]][-length(report_table$plan_HPWLname[[1]])],sep=", "),
      #                      report_table$plan_HPWLname[[1]][length(report_table$plan_HPWLname[[1]])],sep = ", and "),
      #                ifelse(length(report_table$plan_HPWLname[[1]])==2,implode(report_table$plan_HPWLname[[1]],sep = " and "),
      #                report_table$plan_HPWLname[[1]]))
      #WL_list<-ifelse(length(report_table$plan_HPWLname[[1]])>2,
      #                implode(report_table$plan_HPWLname[[1]],sep=", ",", and "),
      #                ifelse(length(report_table$plan_HPWLname[[1]])==2,
      #                       implode(report_table$plan_HPWLname[[1]],sep=" and "),
      #                       report_table$plan_HPWLname[[1]]))
      #WL_perc<-report_table$plan_Worklandsperc
      #WL_text<-ifelse(length(report_table$plan_HPWLname[[1]])>=2,
      #                paste("Conserving this area of interest would protect ",WL_list," working lands (about ",
      #                      WL_perc," percent of the project area)",sep = ""),
      #                ifelse(report_table$plan_HPWLname[[1]]!=-99, 
      #                       paste("Conserving this area of interest would also provide protection to working lands, ",
      #                             "with ",WL_list," comprising roughly ",WL_perc," percent of the landscape",sep=""),
      #                       "This area of interest provides no protection of working lands"))
      
      #Statement for Water Quality & Quantity
      #WQ_list<-ifelse(length(report_table$plan_Impairedwbodyname[[1]])>2,
      #                paste(implode(report_table$plan_Impairedwbodyname[[1]][-length(report_table$plan_Impairedwbodyname[[1]])],sep=", "),
      #                      report_table$plan_Impairedwbodyname[[1]][length(report_table$plan_Impairedwbodyname[[1]])],sep = ", and "),
      #                ifelse(length(report_table$plan_Impairedwbodyname[[1]])==2,implode(report_table$plan_Impairedwbodyname[[1]],sep = " and "),
      #                       report_table$plan_Impairedwbodyname[[1]]))
      #WQ_list<-ifelse(length(report_table$plan_Impairedwbodyname[[1]])>2,
      #                implode(report_table$plan_Impairedwbodyname[[1]],sep=", ",", and "),
      #                ifelse(length(report_table$plan_Impairedwbodyname[[1]])==2,
      #                       implode(report_table$plan_Impairedwbodyname[[1]],sep=" and "),
      #                       report_table$plan_Impairedwbodyname[[1]]))
      #WQ_perc<-paste(report_table$plan_Impairedperc, " percent of the project area)")
      #WQ_text<-ifelse(length(report_table$plan_Impairedwbodyname[[1]])>=2,
      #                paste("This area of interest also buffers water flowing into waterbodies with known impairments (",WQ_list,") ",  
      #                      "and preservation would allow this landscape to continue to provide such water quality protections",sep=""),
      #                ifelse(report_table$plan_Impairedwbodyname[[1]]!=-99,
      #                       paste("This area of interest also buffers water flowing into the ", WQ_list,", a waterbody with known impairments, ",  
      #                             "and preservation would allow this landscape to continue to provide such water quality protections",sep=""),
      #                       "No waterways in or around this area of interest are considered impaired"))
      
      
      #Statement for Threat of development
      #Sleuth_text<-paste("In the future, the ",report_table$plan_Name, " area may be vulnerable to inundation due to sea level rise,
      #                 and is expected to have a ", report_table$plan_Urbanthreat, " threat of development
      #                 by the year 2060 according to the SLEUTH urbanization model",sep="")
      #ATTRIBUTES NOT IN APP BUT INCLUDED IN REPORT
      #    extrareport_table<-data.frame("plan_DesignUse"=result$matrix_context[2,])
      #    extrareport_table$plan_DesignUse<-ifelse(extrareport_table$plan_DesignUse==1,"Public Drinking Supply",
      #                                             ifelse(extrareport_table$plan_DesignUse==.75,"Protection and Propagation Of Fish and Wildlife",
      #                                                    ifelse(extrareport_table$plan_DesignUse==.5,"Recreation",
      #                                                           ifelse(extrareport_table$plan_DesignUse==.25,"Agricultural, Industrial, Navigational and other purposes",
      #                                                                  ifelse(extrareport_table$plan_DesignUse==.1,"N/A","No Data Available")))))
      #    extrareport_table$plan_CauseImpaire<-result$matrix_context[1,]
      ###################END REPORT TABLE###################################################
      
      
      
      
      params <- list(#n = input$threshold,
        Project_num=Project_num,
        report_table=report_table,
        footprint=spatial_footprint,
        proplist=proplist
      )
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file, 
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Rawdatatable",".csv",sep = "")
    },
    content = function(file) {
      write.csv(result$showing_matrix_raw, file, row.names = T)
    }
  )
  
  output$downloadDataos <- downloadHandler(
    filename = function() {
      paste("Rawdatatable",".csv",sep = "")
    },
    content = function(file) {
      data<-result_os$showing_matrix
      rownames(data)<-coln
      write.csv(data, file, row.names = T)
    }
  )
  
  output$mapresult<-renderLeaflet({
    spatial_footprint<-do.call(rbind,ps_list$result)
    spatial_footprint$proposal<-1:length(spatial_footprint$geometry)
    color<-colorNumeric(colorlist,spatial_footprint$proposal)
    
    labs <- lapply(seq(nrow(ps_list$hex_merge_final)), function(i) {
      paste0("Threat of Urbanization: ", ps_list$hex_merge_final$Sleuth_v2[i], "<br>",
            "Connectivity with PAD-US: ", ps_list$hex_merge_final$PADUS2[i], "<br>",
            "Connectivity of Natural Lands Index: ", ps_list$hex_merge_final$area_conne[i], "<br>",
            "Proposed Area of Conservation: ", "1", "<br>",
            "Composition of Natural Lands: ", ps_list$hex_merge_final$conl_index[i], "<br>",
            "Imparied Watershed Area: ", ps_list$hex_merge_final$area_12_13[i], "<br>",
            "Hydrologic Response to Land-Use Change: ",ps_list$hex_merge_final$wq3[i], "<br>",
            "Percent Irrigated Agriculture: ",ps_list$hex_merge_final$wq4[i], "<br>",
            "Lateral Connectivity of Floodplain: ",ps_list$hex_merge_final$wq5[i], "<br>",
            "Composition of Riparian Zone Lands: ",ps_list$hex_merge_final$wq6[i], "<br>",
            "Biodiversity Index: ", ps_list$hex_merge_final$Index_cpt_[i], "<br>",
            "T&E Species Area: ", ps_list$hex_merge_final$PA1[i], "<br>",
            "T&E Number of Species: ", ps_list$hex_merge_final$statuscoun[i], "<br>",
            "Light Pollution Index: ", ps_list$hex_merge_final$area_light[i], "<br>",
            "National Heritage Area: ", ps_list$hex_merge_final$area_nha[i], "<br>",
            "National Registery of Historic Places: ", ps_list$hex_merge_final$Join_Cou_2[i], "<br>",
            "High Priority Working Lands: ", ps_list$hex_merge_final$WORKINGLAN[i], "<br>",
            "Commercial Fishing Reliance: ", ps_list$hex_merge_final$ComEng_ct[i], "<br>",
            "Recreational Fishing Engagement: ", ps_list$hex_merge_final$RecEng_ct[i],"<br>",
            "Access & Recreation: Number of Access Points", ps_list$hex_merge_final$AR_boat[i]
            )
    })
    
    leaflet() %>% addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addPolygons(data= spatial_footprint,fillColor = color(spatial_footprint$proposal),
                  fillOpacity = 0.3,weight=0.5,group = "Proposal boudaries", options = pathOptions(clickable = FALSE) ) %>%
      addPolygons(data= ps_list$hex_merge_final, fillOpacity = 0.6, layerId = as.character(ps_list$hex_merge_final$OBJECTID),
                  fillColor = "blue",weight=0.6,group="View Hexagons",stroke = F,
                  label = lapply(labs, HTML),
                  highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE,fillColor = "white",fillOpacity = 0.1),
                  labelOptions = labelOptions(textsize = "15px")
      ) %>%
      addEsriFeatureLayer(url = 'https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/SCA_Boundary/FeatureServer/0',fill = F,weight = 1)%>%
      hideGroup("View Hexagons")%>%
      addLegend(position = "bottomright",colors = color(spatial_footprint$proposal),
                labels = proplist[1:length(spatial_footprint$proposal)],opacity = 0.5,layerId = "legend")%>%
      addFullscreenControl(position="bottomleft")%>%
      addLayersControl(position="topright",overlayGroups=c("Proposal boudaries", "View Hexagons"))
  })
  
  output$mapresultos<-renderLeaflet({
    spatial_footprint<-ps_list_os$result
    spatial_footprint$proposal<-1
    color<-colorNumeric(colorlist,spatial_footprint$proposal)
    labs <- lapply(seq(nrow(ps_list_os$hex_merge_final)), function(i) {
      paste0("Threat of Urbanization: ", ps_list_os$hex_merge_final$Sleuth_v2[i], "<br>",
             "Connectivity with PAD-US: ", ps_list_os$hex_merge_final$PADUS2[i], "<br>",
             "Connectivity of Natural Lands Index: ", ps_list_os$hex_merge_final$area_conne[i], "<br>",
             "Proposed Area of Conservation: ", "1", "<br>",
             "Composition of Natural Lands: ", ps_list_os$hex_merge_final$conl_index[i], "<br>",
             "Imparied Watershed Area: ", ps_list_os$hex_merge_final$area_12_13[i], "<br>",
             "Hydrologic Response to Land-Use Change: ",ps_list_os$hex_merge_final$wq3[i], "<br>",
             "Percent Irrigated Agriculture: ",ps_list_os$hex_merge_final$wq4[i], "<br>",
             "Lateral Connectivity of Floodplain: ",ps_list_os$hex_merge_final$wq5[i], "<br>",
             "Composition of Riparian Zone Lands: ",ps_list_os$hex_merge_final$wq6[i], "<br>",
             "Biodiversity Index: ", ps_list_os$hex_merge_final$Index_cpt_[i], "<br>",
             "T&E Species Area: ", ps_list_os$hex_merge_final$PA1[i], "<br>",
             "T&E Number of Species: ", ps_list_os$hex_merge_final$statuscoun[i], "<br>",
             "Light Pollution Index: ", ps_list_os$hex_merge_final$area_light[i], "<br>",
             "National Heritage Area: ", ps_list_os$hex_merge_final$area_nha[i], "<br>",
             "National Registery of Historic Places: ", ps_list_os$hex_merge_final$Join_Cou_2[i], "<br>",
             "High Priority Working Lands: ", ps_list_os$hex_merge_final$WORKINGLAN[i], "<br>",
             "Commercial Fishing Reliance: ", ps_list_os$hex_merge_final$ComEng_ct[i], "<br>",
             "Recreational Fishing Engagement: ", ps_list_os$hex_merge_final$RecEng_ct[i], "<br>",
             "Access & Recreation: Number of Access Points",ps_list_os$hex_merge_final$AR_boat[i])
    })
    leaflet() %>% addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addPolygons(data= spatial_footprint,fillColor = color(spatial_footprint$proposal),
                  fillOpacity = 0.3,weight=0.5,group = "Proposal boudaries", options = pathOptions(clickable = FALSE) ) %>%
      addPolygons(data= ps_list_os$hex_merge_final, fillOpacity = 0.6, layerId = as.character(ps_list_os$hex_merge_final$OBJECTID),
                  fillColor = "blue",weight=0.6,group="View Hexagons",stroke = F,
                  label = lapply(labs, HTML),
                  highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE,fillColor = "white",fillOpacity = 0.1),
                  labelOptions = labelOptions(textsize = "15px")
      ) %>%
      addEsriFeatureLayer(url = 'https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/SCA_Boundary/FeatureServer/0',fill = F,weight = 1)%>%
      hideGroup("View Hexagons")%>%
      addLegend(position = "bottomright",colors = color(spatial_footprint$proposal),
                labels = proplist_os,opacity = 0.5,layerId = "legend")%>%
      addFullscreenControl(position="bottomleft")%>%
      addLayersControl(position="topright",overlayGroups=c("Proposal boudaries", "View Hexagons"))
  })
  
  output$mapresult1<-renderLeaflet({
    spatial_footprint<-do.call(rbind,ps_list$result)
    spatial_footprint$proposal<-1:length(spatial_footprint$geometry)
    color<-colorNumeric(colorlist,spatial_footprint$proposal)
    
    leaflet() %>% addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addPolygons(data= spatial_footprint,fill = F,
                  weight=2,group = "Proposal boudaries", options = pathOptions(clickable = FALSE) ) %>%
      addPolygons(data= ps_list$hex_merge_final,fillColor =  color(ps_list$hex_merge_final$appid),stroke = F,
                  fillOpacity = 0.5,weight=0.5,group = "Proposal boudaries", options = pathOptions(clickable = FALSE) ) %>%
      addEsriDynamicMapLayer(
        url = paste0("https://gis.usgs.gov/sciencebase2/rest/services/Catalog/5da9e701e4b09fd3b0c9cb6a/MapServer"),
        options = dynamicMapLayerOptions(transparent = TRUE,opacity = 0.15),group = "SECAS Blueprint")%>%
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/TNC_Alabama_Terrestrial_Priority_Sites_2008/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green",group = "AL Conservation Opportunity Areas")%>%
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/ArcGIS/rest/services/Mississippi_COA/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green",group = "MS Conservation Opportunity Areas")%>%
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/coas_april2019_la/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green",group = "LA Conservation Opportunity Areas")%>%
      addEsriFeatureLayer(
        url = "https://fnai04.fnai.org:6443/arcgis/rest/services/ConLands/FLMA_FFBOT_AP_Combined/MapServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green" ,group = "FNAI BOT Conservation Areas")%>%
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/NERRmerge/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green" ,group = "NERR Conservation Areas")%>%
      
      addEsriFeatureLayer(url = 'https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/SCA_Boundary/FeatureServer/0',fill = F,weight = 1)%>%
      addLegend(position = "bottomright",colors = color(spatial_footprint$proposal),
                labels = proplist[1:length(spatial_footprint$proposal)],opacity = 0.5)%>%
      addFullscreenControl(position="bottomleft")%>%
      #hideGroup("MDEQ Target Areas")%>%
      hideGroup("FNAI BOT Conservation Areas")%>%
      hideGroup("AL Conservation Opportunity Areas")%>%
      hideGroup("MS Conservation Opportunity Areas")%>%
      hideGroup("LA Conservation Opportunity Areas")%>%
      hideGroup("SECAS Blueprint")%>%
      hideGroup("NERR Conservation Areas")%>%
      addLayersControl(position="topright",overlayGroups=c("NERR Conservation Areas","FNAI BOT Conservation Areas","AL Conservation Opportunity Areas","MS Conservation Opportunity Areas","LA Conservation Opportunity Areas","SECAS Blueprint"))
  })
  
  
  
  output$mapresult5<-renderLeaflet({
    spatial_footprint<-do.call(rbind,ps_list$result)
    spatial_footprint$proposal<-1:length(spatial_footprint$geometry)
    color<-colorNumeric(colorlist,spatial_footprint$proposal)
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addPolygons(data= spatial_footprint,fill = F,
                  weight=2,group = "Proposal boudaries", options = pathOptions(clickable = FALSE) ) %>% 
      addEsriFeatureLayer(url = 'https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/SCA_Boundary/FeatureServer/0',fill = F,weight = 1)%>%
      addPolygons(data= ps_list$hex_merge_final,fillColor = color(ps_list$hex_merge_final$appid),stroke = F,
                  fillOpacity = 0.5,weight=0.5,group = "Proposal boudaries", options = pathOptions(clickable = FALSE) ) %>%
      addEsriDynamicMapLayer(
        url = paste0("https://gis.usgs.gov/sciencebase2/rest/services/Catalog/5da9e701e4b09fd3b0c9cb6a/MapServer"),
        options = dynamicMapLayerOptions(transparent = TRUE,opacity = 0.15),group = "SECAS Blueprint")%>%
      #addEsriTiledMapLayer(
      #  url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/Target_Areas_MDEQ/MapServer/",
      #  options = providerTileOptions(opacity = 0.5),group = "MDEQ Target Areas")%>%
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/TNC_Alabama_Terrestrial_Priority_Sites_2008/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green",group = "AL Conservation Opportunity Areas")%>%
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/ArcGIS/rest/services/Mississippi_COA/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green",group = "MS Conservation Opportunity Areas")%>%
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/coas_april2019_la/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green",group = "LA Conservation Opportunity Areas")%>%
      addEsriFeatureLayer(
        url = "https://fnai04.fnai.org:6443/arcgis/rest/services/ConLands/FLMA_FFBOT_AP_Combined/MapServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green" ,group = "FNAI BOT Conservation Areas")%>%
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/NERRmerge/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green" ,group = "NERR Conservation Areas")%>%
      
      addLegend(position = "bottomright",colors = color(spatial_footprint$proposal),
                labels = proplist[1:length(spatial_footprint$proposal)],opacity = 0.5,layerId = "legend")%>%
      addFullscreenControl(position="bottomleft")%>%
      #hideGroup("MDEQ Target Areas")%>%
      hideGroup("FNAI BOT Conservation Areas")%>%
      hideGroup("AL Conservation Opportunity Areas")%>%
      hideGroup("MS Conservation Opportunity Areas")%>%
      hideGroup("LA Conservation Opportunity Areas")%>%
      hideGroup("SCA Boundary")%>%
      hideGroup("SECAS Blueprint")%>%
      hideGroup("NERR Conservation Areas")%>%
      addLayersControl(position="topright",overlayGroups=c("NERR Conservation Areas","FNAI BOT Conservation Areas","AL Conservation Opportunity Areas","MS Conservation Opportunity Areas","LA Conservation Opportunity Areas","SECAS Blueprint"))
  })
  
  output$mapresult6<-renderLeaflet({
    spatial_footprint<-do.call(rbind,ps_list$result)
    spatial_footprint$proposal<-1:length(spatial_footprint$geometry)
    color<-colorNumeric(colorlist,spatial_footprint$proposal)
    leaflet() %>% addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addEsriFeatureLayer(url = 'https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/SCA_Boundary/FeatureServer/0',fill = F,weight = 1)%>% 
      addPolygons(data= spatial_footprint,fill = F,
                  weight=2,group = "Proposal boudaries", options = pathOptions(clickable = FALSE) ) %>%
      addPolygons(data= ps_list$hex_merge_final,fillColor = color(ps_list$hex_merge_final$appid),stroke = F,
                  fillOpacity = 0.5,weight=0.5,group = "Proposal boudaries", options = pathOptions(clickable = FALSE) ) %>%
      addEsriDynamicMapLayer(
        url = paste0("https://gis.usgs.gov/sciencebase2/rest/services/Catalog/5da9e701e4b09fd3b0c9cb6a/MapServer"),
        options = dynamicMapLayerOptions(transparent = TRUE,opacity = 0.15),group = "SECAS Blueprint")%>%
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/TNC_Alabama_Terrestrial_Priority_Sites_2008/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green",group = "AL Conservation Opportunity Areas")%>%
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/ArcGIS/rest/services/Mississippi_COA/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green",group = "MS Conservation Opportunity Areas")%>%
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/coas_april2019_la/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green",group = "LA Conservation Opportunity Areas")%>%
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/NERRmerge/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green" ,group = "NERR Conservation Areas")%>%
      
      addEsriFeatureLayer(
        url = "https://fnai04.fnai.org:6443/arcgis/rest/services/ConLands/FLMA_FFBOT_AP_Combined/MapServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green" ,group = "FNAI BOT Conservation Areas")%>%
      addLegend(position = "bottomright",colors = color(spatial_footprint$proposal),
                labels = proplist[1:length(spatial_footprint$proposal)],opacity = 0.5)%>%
      addFullscreenControl(position="bottomleft")%>%
      #hideGroup("MDEQ Target Areas")%>%
      hideGroup("FNAI BOT Conservation Areas")%>%
      hideGroup("AL Conservation Opportunity Areas")%>%
      hideGroup("MS Conservation Opportunity Areas")%>%
      hideGroup("LA Conservation Opportunity Areas")%>%
      hideGroup("NERR Conservation Areas")%>%
      hideGroup("SECAS Blueprint")%>%
      addLayersControl(position="topright",overlayGroups=c("NERR Conservation Areas","FNAI BOT Conservation Areas","AL Conservation Opportunity Areas","MS Conservation Opportunity Areas","LA Conservation Opportunity Areas","SECAS Blueprint"))
  })
  
  output$mapresult4<-renderLeaflet({
    spatial_footprint<-ps_list_os$result
    spatial_footprint$proposal<-1
    color<-colorNumeric(colorlist,spatial_footprint$proposal)
    leaflet() %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% 
      addEsriFeatureLayer(url = 'https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/SCA_Boundary/FeatureServer/0',fill = F,weight = 1)%>%
      #addEsriTiledMapLayer(
      #  url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/Target_Areas_MDEQ/MapServer/",
      #  options = providerTileOptions(opacity = 0.5),group = "MDEQ Target Areas")%>%
      addEsriDynamicMapLayer(
        url = paste0("https://gis.usgs.gov/sciencebase2/rest/services/Catalog/5da9e701e4b09fd3b0c9cb6a/MapServer"),
        options = dynamicMapLayerOptions(transparent = TRUE,opacity = 0.15),group = "SECAS Blueprint")%>%
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/TNC_Alabama_Terrestrial_Priority_Sites_2008/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green",group = "AL Conservation Opportunity Areas")%>%
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/NERRmerge/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green" ,group = "NERR Conservation Areas")%>%
      
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/ArcGIS/rest/services/Mississippi_COA/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green",group = "MS Conservation Opportunity Areas")%>%
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/coas_april2019_la/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green",group = "LA Conservation Opportunity Areas")%>%
      addEsriFeatureLayer(
        url = "https://fnai04.fnai.org:6443/arcgis/rest/services/ConLands/FLMA_FFBOT_AP_Combined/MapServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green" ,group = "FNAI BOT Conservation Areas")%>%
      
      addPolygons(data= spatial_footprint,fill = F,
                  weight=2,group = "Proposal boudaries", options = pathOptions(clickable = FALSE) ) %>%
      addPolygons(data= ps_list_os$hex_merge_final,fillColor = color(spatial_footprint$proposal),stroke = F,
                  fillOpacity = .5,weight=0.5,group = "Proposal boudaries", options = pathOptions(clickable = FALSE) ) %>%
      addLegend(position = "bottomright",colors = color(spatial_footprint$proposal),labels = proplist_os,opacity = 0.5,layerId = "legend")%>%
      addFullscreenControl(position="bottomleft")%>%
      #hideGroup("MDEQ Target Areas")%>%
      hideGroup("FNAI BOT Conservation Areas")%>%
      hideGroup("AL Conservation Opportunity Areas")%>%
      hideGroup("MS Conservation Opportunity Areas")%>%
      hideGroup("LA Conservation Opportunity Areas")%>%
      hideGroup("NERR Conservation Areas")%>%
      hideGroup("SECAS Blueprint")%>%
      addLayersControl(position="topright",overlayGroups=c("NERR Conservation Areas","FNAI BOT Conservation Areas","AL Conservation Opportunity Areas","MS Conservation Opportunity Areas","LA Conservation Opportunity Areas","SECAS Blueprint"))
  })
  
  output$mapresult_portfolio<-renderLeaflet({
    spatial_footprint<-do.call(rbind,ps_list_portfolio$result)
    spatial_footprint$proposal<-1:length(spatial_footprint$geometry)
    color<-colorNumeric(colorlist,spatial_footprint$proposal)
    leaflet() %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% 
      addEsriFeatureLayer(url = 'https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/SCA_Boundary/FeatureServer/0',fill = F,weight = 1)%>%
      #addEsriTiledMapLayer(
      #  url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/Target_Areas_MDEQ/MapServer/",
      #  options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green",group = "MDEQ Target Areas")%>%
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/TNC_Alabama_Terrestrial_Priority_Sites_2008/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green",group = "AL Conservation Opportunity Areas")%>%
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/NERRmerge/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green" ,group = "NERR Conservation Areas")%>%
      
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/ArcGIS/rest/services/Mississippi_COA/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green",group = "MS Conservation Opportunity Areas")%>%
      addEsriFeatureLayer(
        url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/coas_april2019_la/FeatureServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green",group = "LA Conservation Opportunity Areas")%>%
      addEsriFeatureLayer(
        url = "https://fnai04.fnai.org:6443/arcgis/rest/services/ConLands/FLMA_FFBOT_AP_Combined/MapServer/0",
        options = providerTileOptions(opacity = 0.5),stroke=F,weight = 0.01 ,fillColor="green" ,group = "FNAI BOT Conservation Areas")%>%
      addEsriDynamicMapLayer(
        url = paste0("https://gis.usgs.gov/sciencebase2/rest/services/Catalog/5da9e701e4b09fd3b0c9cb6a/MapServer"),
        options = dynamicMapLayerOptions(transparent = TRUE,opacity = 0.15),group = "SECAS Blueprint")%>%
      addPolygons(data= spatial_footprint,fill = F,
                  weight=2,group = "Proposal boudaries", options = pathOptions(clickable = FALSE) ) %>%
      addPolygons(data= ps_list$hex_merge_final,fillColor = color(ps_list$hex_merge_final$appid),stroke = F,
                  fillOpacity = .5,weight=0.5,group = "Proposal boudaries", options = pathOptions(clickable = FALSE) ) %>%
      addLegend(position = "bottomright",colors = color(spatial_footprint$proposal),
                labels = proplist[1:length(spatial_footprint$proposal)],opacity = 0.5)%>%
      addFullscreenControl(position="bottomleft")%>%
      #hideGroup("MDEQ Target Areas")%>%
      hideGroup("FNAI BOT Conservation Areas")%>%
      hideGroup("AL Conservation Opportunity Areas")%>%
      hideGroup("MS Conservation Opportunity Areas")%>%
      hideGroup("LA Conservation Opportunity Areas")%>%
      hideGroup("NERR Conservation Areas")%>%
      hideGroup("SECAS Blueprint")%>%
      addLayersControl(position="topright",overlayGroups=c("NERR Conservation Areas","FNAI BOT Conservation Areas","AL Conservation Opportunity Areas","MS Conservation Opportunity Areas","LA Conservation Opportunity Areas","SECAS Blueprint"))
  })
  
  output$report2 <- downloadHandler(
    filename = function() { 
      paste("Spatialfootprint",Sys.Date(),'.zip', sep='') 
    },
    content = function(file) {
      temp_shp <- "download"
      spatial_footprint<-do.call(rbind,ps_list$result)
      spatial_footprint$proposal<-1:length(spatial_footprint$geometry)
      st_write(spatial_footprint,paste0(c(temp_shp,"Spatial_footprint.shp"),collapse ="/"),delete_layer = T)
      files <- list.files(temp_shp, "Spatial_footprint",recursive=TRUE) 
      #print(files)
      zip(zipfile = file, files=paste(temp_shp), recurse = TRUE)
    },contentType = "application/zip"
  )
  
  output$report3 <- downloadHandler(
    filename = function() { 
      paste("Spatialfootprint",Sys.Date(),'.zip', sep='') 
    },
    content = function(file) {
      temp_shp <- "download"
      spatial_footprint<-do.call(rbind,ps_list$result)
      spatial_footprint$proposal<-1:length(spatial_footprint$geometry)
      st_write(spatial_footprint,paste0(c(temp_shp,"Spatial_footprint.shp"),collapse ="/"),delete_layer = T)
      files <- list.files(temp_shp, "Spatial_footprint",recursive=TRUE) 
      ##print(files)
      zip(zipfile = file, files=paste(temp_shp), recurse = TRUE)
    },contentType = "application/zip"
  )
  
  output$osspdownload <- downloadHandler(
    filename = function() { 
      paste("Spatialfootprint",Sys.Date(),'.zip', sep='') 
    },
    content = function(file) {
      temp_shp <- "download"
      spatial_footprint<-ps_list_os$result
      spatial_footprint$proposal<-1
      st_write(spatial_footprint,paste0(c(temp_shp,"Spatial_footprint.shp"),collapse ="/"),delete_layer = T)
      files <- list.files(temp_shp, "Spatial_footprint",recursive=TRUE) 
      #print(files)
      zip(zipfile = file, files=paste(temp_shp), recurse = TRUE)
    },contentType = "application/zip"
  )
  
  output$portfoliospdownload <- downloadHandler(
    filename = function() { 
      paste("Spatialfootprint",Sys.Date(),'.zip', sep='') 
    },
    content = function(file) {
      temp_shp <- "download"
      spatial_footprint<-ps_list_portfolio$result
      spatial_footprint$proposal<-1
      st_write(spatial_footprint,paste0(c(temp_shp,"Spatial_footprint.shp"),collapse ="/"),delete_layer = T)
      files <- list.files(temp_shp, "Spatial_footprint",recursive=TRUE) 
      #print(files)
      zip(zipfile = file, files=paste(temp_shp), recurse = TRUE)
    },contentType = "application/zip"
  )
  
  #ABOUT tab
  #output$logo<-renderImage({
  
  # filename <- normalizePath(file.path('./images',
  #                                      paste('msu_fws_logos', '.png', sep='')))
  #  return(list(
  #    src= filename, width=450))
  #}, deleteFile = F) 
  
  
  #output$logo2<-renderImage({
  #  
  #  filename <- normalizePath(file.path('./images',
  #                                      paste('msu_fws_logos', '.png', sep='')))
  #  return(list(
  #    src= filename, width=450))
  #}, deleteFile = F) 
  
  #output$logo3<-renderImage({
  
  #  filename <- normalizePath(file.path('./images',
  #                                      paste('msu_fws_logos', '.png', sep='')))
  #  return(list(
  #    src= filename, width=450))
  #}, deleteFile = F) 
  
  #output$logo4<-renderImage({
  #  
  #  filename <- normalizePath(file.path('./images',
  #                                      paste('msu_fws_logos', '.png', sep='')))
  #  return(list(
  #    src= filename, width=450))
  #}, deleteFile = F) 
  
  output$T1download<-downloadHandler(
    filename = function(){
      paste('MCDA Simulation Tutorial', Sys.Date(),'.pdf',sep='')
    },
    content =  function(file) {
      file.copy("./SCA-CPT -Tutorial1-MCDA_Simulation_Guided_Walkthrough.pdf", file)
    }
  )
  
  output$explaincentral<-downloadHandler(
    filename = function(){
      paste('What can I learn from the Central Weights Graph_', Sys.Date(),'.pdf',sep='')
    },
    content =  function(file) {
      file.copy("./central.pdf", file)
    }
  )
  
  output$T2download<-downloadHandler(
    filename = function(){
      paste('User Weights Simulation Tutorial', Sys.Date(),'.pdf',sep='')
    },
    content =  function(file) {
      file.copy("./SCA-CPT -Tutorial2-User_Weights_Simulation_Guided_Walkthrough.pdf", file)
    }
  )
  ##END of ABOUT tab
  
  value2$test2<-10
  
  observeEvent(input$addweight,{
    if(value2$test2==10){
      for(i in 1:length(ps_list$result)){
      insertUI(
        selector = "#finishaddweight",
        where = "beforeBegin",
        ui = numericInput(paste0("txt", i),
                          paste0(proplist[i]),0,min = 0,max = 10)
        )
      }
      insertUI(
        selector = "#finishaddweight",
        where = "beforeBegin",
        ui = textAreaInput(paste0("txt", "i"),
                          "Description for the attribute", width = "400px",height = "150px")
      )
    }
    value2$test2<-0
    show(selector = "#viewdata li a[data-value=addattr]")
    updateTabsetPanel(session = session, inputId = "viewdata", "addattr")
  })
  
  observeEvent(input$renameproject,{
    if(is.null(input$txtrename1)){
    for(i in 1:length(ps_list$result)){
    insertUI(
      selector = "#confirmname",
      where = "beforeBegin",
      ui = textInput(paste0("txtrename", i),
                     paste0(proplist[i]),
                     paste0(proplist[i]))
    )
  }
    show(selector = "#viewdata li a[data-value=rename]")
    updateTabsetPanel(session = session, inputId = "viewdata", "rename")
    }
  })
  
  output$osnamechange<-renderUI(
    textInput("osrename1",value = proplist_os,label = proplist_os)
  )
  
  observeEvent(input$osrename,{
    show(selector = "#viewdataos li a[data-value=renameos]")
    updateTabsetPanel(session = session, inputId = "viewdataos", "renameos")
  })
  
  observeEvent(input$osgotodeselect,{
    show(selector = "#viewdataos li a[data-value=redefineos]")
    updateTabsetPanel(session = session, inputId = "viewdataos", "redefineos")
    shinyjs::show("osupdatetable")
    leafletProxy('mapresultos')%>%
      addPolygons(data=SCA, fill = F ,group = "SCA Boundary",color = "red",
                  weight = .5, options = pathOptions(clickable = FALSE))
  })
  
  
  
  observeEvent(input$adjustmcdanumbers,{
    shinyjs::show("numMCDA")
  })
  
  observeEvent(input$numMCDA,{
    #print(input$numMCDA)
    value1$mcdarun<-as.numeric(input$numMCDA)
  
    })
  observeEvent(input$osconfirmname,{
    proplist_os<<- input$osrename1
    # The row andcolumn names of showing_matrix_os will automatically update after the changes of showing_matrix
    rownames(result_os$showing_matrix)<-coln
    colnames(result_os$showing_matrix)<-proplist_os
    updateTabsetPanel(session = session, inputId = "viewdataos", "osdatasummary")

    spatial_footprint<-ps_list_os$result
    spatial_footprint$proposal<-1
    color<-colorNumeric(colorlist,spatial_footprint$proposal)
    # Rename the legend labels of the small map at the bottom right
    leafletProxy("mapresult4")%>%
      removeControl("legend") %>%
      addLegend(position = "bottomright",colors = color(spatial_footprint$proposal),labels = proplist_os,opacity = 0.5,layerId = "legend")
    # Rename the legend labels of the large map for refining area
    leafletProxy("mapresultos")%>%
      removeControl("legend") %>%
      addLegend(position = "bottomright",colors = color(spatial_footprint$proposal),labels = proplist_os,opacity = 0.5,layerId = "legend")
  })
  
  
  observeEvent(input$confirmname,{
    for(i in 1:length(ps_list$result)){
      proplist[i]<<-eval(parse(text = paste0("input$txtrename",i)))
    }
    # print(proplist)
    # The column names of showing_matrix will automatically update after the changes of showing_matrix_raw
    # and column names of showing_matrix_scaled will automatically update after the changes of showing_matrix
    colnames(result$showing_matrix_raw)<-proplist[1:ncol(result$showing_matrix_raw)]
    colnames(result$showing_matrix)<-proplist[1:ncol(result$showing_matrix)]
    colnames(result$matrix)<-proplist[1:ncol(result$matrix)]
    updateTabsetPanel(session = session, inputId = "viewdata", "rawdata")

    spatial_footprint<-do.call(rbind,ps_list$result)
    spatial_footprint$proposal<-1:length(spatial_footprint$geometry)
    color<-colorNumeric(colorlist,spatial_footprint$proposal)
    # Rename the legend labels of the small map at the bottom right
    leafletProxy("mapresult5")%>%
      removeControl("legend") %>%
      addLegend(position = "bottomright",colors = color(spatial_footprint$proposal),
                labels = proplist[1:length(spatial_footprint$proposal)],opacity = 0.5,layerId = "legend")
    # Rename the legend labels of the large map for refining area
    leafletProxy("mapresult")%>%
      removeControl("legend") %>%
      addLegend(position = "bottomright",colors = color(spatial_footprint$proposal),
                labels = proplist[1:length(spatial_footprint$proposal)],opacity = 0.5,layerId = "legend")
  })
  
 
  result$newaddattr<-NULL
  
  observeEvent(input$finishaddweight,{
    addattrtmp<-NULL
    addattrtmp<-c(addattrtmp,input$namenewadded,input$goalnewadded)
    for(i in 1:length(ps_list$result)){
      addattrtmp<-c(addattrtmp,eval(parse(text = paste0("input$txt",i))))
    }
    addattrtmp<-t(addattrtmp)
    addattrtmp<-as.data.frame(addattrtmp)
    names(addattrtmp)<-c("Attribute name","Goals",proplist[1:length(ps_list$result)])
    result$newaddattr<-rbind(result$newaddattr,addattrtmp)
    value2$newaddedweight<-value2$newaddedweight+1
    #print(result$newaddattr)
  })
  observeEvent(input$removeaddweight,{
    if(!is.null(input$addattrtable_rows_selected)){
      result$newaddattr<-result$newaddattr[-c(input$addattrtable_rows_selected), ]
      value2$newaddedweight<- value2$newaddedweight -1
    }
    
  })
  
  output$addattrtable<-renderDataTable(result$newaddattr,options = list(searching = FALSE,selection = 'single',paging = FALSE))
  
  observeEvent(input$mapresultos_shape_click, { 
    value2$clickedMarker <- input$mapresultos_shape_click
    hex_id<-value2$clickedMarker$id
    hex_id<-as.character(hex_id)
    #print(hex_id)
    #print(class(hex_id))
    #print(is.null(ps_list_os$hex_select))
    #print(class(ps_list_os$hex_merge_final))
    #print(as.character(hex_id)%in% ps_list_os$hex_select$OBJECTID)
    #print(hex_id %in% ps_list$hex_merge_final$OBJECTID)
    #print(colnames(ps_list_os$hex_merge_final))
    #df_Rshortdatage$data[df_Rshortdatage$data$Geo.Extent==hex_id,]
    if(is.null(ps_list_os$hex_select) && hex_id %in% ps_list_os$hex_merge_final$OBJECTID){
      ps_list_os$hex_select<-ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]
      leafletProxy('mapresultos')%>%
        removeShape(layerId = as.character(hex_id))%>%
        addPolygons(data=ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,], fillOpacity = 0.6, layerId = as.character(hex_id),
                    fillColor = "gray",weight=0.4,group="View Hexagons",
                    label= HTML(paste0("Threat of Urbanization: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$Sleuth_v2, "<br>",
                           "Connectivity with PAD-US: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$padus, "<br>",
                           "Connectivity of Natural Lands Index: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$area_conne, "<br>",
                           "Proposed Area of Conservation: ", "1", "<br>",
                           "Composition of Natural Lands: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$conl_index, "<br>",
                           "Imparied Watershed Area: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$area_12_13, "<br>",
                           "Hydrologic Response to Land-Use Change: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$wq3, "<br>",
                           "Percent Irrigated Agriculture: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$wq4, "<br>",
                           "Lateral Connectivity of Floodplain: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$wq5, "<br>",
                           "Composition of Riparian Zone Lands: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$wq6, "<br>",
                           "Biodiversity Index: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$Index_cpt_, "<br>",
                           "T&E Species Area: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$PA1, "<br>",
                           "T&E Number of Species: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$statuscoun, "<br>",
                           "Light Pollution Index: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$area_light, "<br>",
                           "National Heritage Area: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$area_nha, "<br>",
                           "National Registery of Historic Places: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$Join_Cou_2, "<br>",
                           "High Priority Working Lands: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$WORKINGLAN, "<br>",
                           "Commercial Fishing Reliance: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$ComEng_ct, "<br>",
                           "Recreational Fishing Engagement: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$RecEng_ct)),
                    highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE,fillColor = "gray",fillOpacity = 0.5),
                    labelOptions = labelOptions(textsize = "15px")
                    )
        #setShapeStyle(layerId = as.character(hex_id), fillColor="gray",weight=0.4, fillOpacity=0.6)
      #ps_list$hex_merge_final<-ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID.x!=hex_id,]
    }else if(as.character(hex_id) %in% ps_list_os$hex_select$OBJECTID && hex_id %in% ps_list_os$hex_merge_final$OBJECTID){
      #ps_list$hex_merge_final<-rbind(ps_list$hex_merge_final,ps_list$hex_select[ps_list$hex_select$OBJECTID.x==hex_id,])
     
      leafletProxy('mapresultos')%>%
        removeShape(layerId = as.character(hex_id))%>%
        addPolygons(data=ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,], fillOpacity = 0.6, layerId = as.character(hex_id),
                    fillColor = "blue",weight=0.4,group="View Hexagons",
                    label= HTML(paste0("Threat of Urbanization: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$Sleuth_v2, "<br>",
                                       "Connectivity with PAD-US: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$padus, "<br>",
                                       "Connectivity of Natural Lands Index: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$area_conne, "<br>",
                                       "Proposed Area of Conservation: ", "1", "<br>",
                                       "Composition of Natural Lands: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$conl_index, "<br>",
                                       "Imparied Watershed Area: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$area_12_13, "<br>",
                                       "Hydrologic Response to Land-Use Change: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$wq3, "<br>",
                                       "Percent Irrigated Agriculture: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$wq4, "<br>",
                                       "Lateral Connectivity of Floodplain: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$wq5, "<br>",
                                       "Composition of Riparian Zone Lands: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$wq6, "<br>",
                                       "Biodiversity Index: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$Index_cpt_, "<br>",
                                       "T&E Species Area: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$PA1, "<br>",
                                       "T&E Number of Species: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$statuscoun, "<br>",
                                       "Light Pollution Index: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$area_light, "<br>",
                                       "National Heritage Area: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$area_nha, "<br>",
                                       "National Registery of Historic Places: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$Join_Cou_2, "<br>",
                                       "High Priority Working Lands: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$WORKINGLAN, "<br>",
                                       "Commercial Fishing Reliance: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$ComEng_ct, "<br>",
                                       "Recreational Fishing Engagement: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$RecEng_ct)),
                    highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE,fillColor = "gray",fillOpacity = 0.5),
                    labelOptions = labelOptions(textsize = "15px")
        )
      
      ps_list_os$hex_select<-ps_list_os$hex_select[ps_list_os$hex_select$OBJECTID!=as.character(hex_id),]
    }
    else if(!(as.character(hex_id)%in% ps_list_os$hex_select$OBJECTID) && as.character(hex_id) %in% ps_list_os$hex_merge_final$OBJECTID)
    {
      ps_list_os$hex_select<-rbind(ps_list_os$hex_select,ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==as.character(hex_id),])
      leafletProxy('mapresultos')%>%
        removeShape(layerId = as.character(hex_id))%>%
        addPolygons(data=ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,], fillOpacity = 0.6, layerId = as.character(hex_id),
                    fillColor = "gray",weight=0.4,group="View Hexagons",
                    label= HTML(paste0("Threat of Urbanization: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$Sleuth_v2, "<br>",
                                       "Connectivity with PAD-US: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$padus, "<br>",
                                       "Connectivity of Natural Lands Index: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$area_conne, "<br>",
                                       "Proposed Area of Conservation: ", "1", "<br>",
                                       "Composition of Natural Lands: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$conl_index, "<br>",
                                       "Imparied Watershed Area: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$area_12_13, "<br>",
                                       "Hydrologic Response to Land-Use Change: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$wq3, "<br>",
                                       "Percent Irrigated Agriculture: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$wq4, "<br>",
                                       "Lateral Connectivity of Floodplain: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$wq5, "<br>",
                                       "Composition of Riparian Zone Lands: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$wq6, "<br>",
                                       "Biodiversity Index: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$Index_cpt_, "<br>",
                                       "T&E Species Area: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$PA1, "<br>",
                                       "T&E Number of Species: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$statuscoun, "<br>",
                                       "Light Pollution Index: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$area_light, "<br>",
                                       "National Heritage Area: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$area_nha, "<br>",
                                       "National Registery of Historic Places: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$Join_Cou_2, "<br>",
                                       "High Priority Working Lands: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$WORKINGLAN, "<br>",
                                       "Commercial Fishing Reliance: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$ComEng_ct, "<br>",
                                       "Recreational Fishing Engagement: ", ps_list_os$hex_merge_final[ps_list_os$hex_merge_final$OBJECTID==hex_id,]$RecEng_ct)),
                    highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE,fillColor = "gray",fillOpacity = 0.5),
                    labelOptions = labelOptions(textsize = "15px")
        )
    }
  })
  
  observeEvent(input$mapresult_shape_click, { 
    value2$clickedMarker <- input$mapresult_shape_click
    hex_id<-value2$clickedMarker$id
    ##print(hex_id)
    ##print(class(hex_id))
    #df_Rshortdatage$data[df_Rshortdatage$data$Geo.Extent==hex_id,]
    if(is.null(ps_list$hex_select) && hex_id %in% ps_list$hex_merge_final$OBJECTID){
      ps_list$hex_select<-ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]
      leafletProxy('mapresult')%>%
        removeShape(layerId = as.character(hex_id))%>%
        addPolygons(data=ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,], fillOpacity = 0.6, layerId = as.character(hex_id),
                    fillColor = "gray",weight=0.4,group="View Hexagons",
                    label= HTML(paste0("Threat of Urbanization: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$Sleuth_v2, "<br>",
                                       "Connectivity with PAD-US: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$padus, "<br>",
                                       "Connectivity of Natural Lands Index: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$area_conne, "<br>",
                                       "Proposed Area of Conservation: ", "1", "<br>",
                                       "Composition of Natural Lands: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$conl_index, "<br>",
                                       "Imparied Watershed Area: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$area_12_13, "<br>",
                                       "Hydrologic Response to Land-Use Change: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$wq3, "<br>",
                                       "Percent Irrigated Agriculture: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$wq4, "<br>",
                                       "Lateral Connectivity of Floodplain: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$wq5, "<br>",
                                       "Composition of Riparian Zone Lands: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$wq6, "<br>",
                                       "Biodiversity Index: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$Index_cpt_, "<br>",
                                       "T&E Species Area: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$PA1, "<br>",
                                       "T&E Number of Species: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$statuscoun, "<br>",
                                       "Light Pollution Index: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$area_light, "<br>",
                                       "National Heritage Area: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$area_nha, "<br>",
                                       "National Registery of Historic Places: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$Join_Cou_2, "<br>",
                                       "High Priority Working Lands: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$WORKINGLAN, "<br>",
                                       "Commercial Fishing Reliance: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$ComEng_ct, "<br>",
                                       "Recreational Fishing Engagement: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$RecEng_ct)),
                    highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE,fillColor = "gray",fillOpacity = 0.5),
                    labelOptions = labelOptions(textsize = "15px")
        )
      #setShapeStyle(layerId = as.character(hex_id), fillColor="gray",weight=0.4, fillOpacity=0.6)
      #ps_list$hex_merge_final<-ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID.x!=hex_id,]
    }else if(as.character(hex_id) %in% ps_list$hex_select$OBJECTID && as.character(hex_id) %in% ps_list$hex_merge_final$OBJECTID){
      #ps_list$hex_merge_final<-rbind(ps_list$hex_merge_final,ps_list$hex_select[ps_list$hex_select$OBJECTID.x==hex_id,])
      leafletProxy('mapresult')%>%
        removeShape(layerId = as.character(hex_id))%>%
        addPolygons(data=ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==as.character(hex_id),], fillOpacity = 0.6, layerId = hex_id,
                    fillColor = "blue",weight=0.4,group="View Hexagons",
                    label= HTML(paste0("Threat of Urbanization: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$Sleuth_v2, "<br>",
                                       "Connectivity with PAD-US: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$padus, "<br>",
                                       "Connectivity of Natural Lands Index: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$area_conne, "<br>",
                                       "Proposed Area of Conservation: ", "1", "<br>",
                                       "Composition of Natural Lands: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$conl_index, "<br>",
                                       "Imparied Watershed Area: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$area_12_13, "<br>",
                                       "Hydrologic Response to Land-Use Change: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$wq3, "<br>",
                                       "Percent Irrigated Agriculture: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$wq4, "<br>",
                                       "Lateral Connectivity of Floodplain: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$wq5, "<br>",
                                       "Composition of Riparian Zone Lands: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$wq6, "<br>",
                                       "Biodiversity Index: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$Index_cpt_, "<br>",
                                       "T&E Species Area: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$PA1, "<br>",
                                       "T&E Number of Species: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$statuscoun, "<br>",
                                       "Light Pollution Index: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$area_light, "<br>",
                                       "National Heritage Area: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$area_nha, "<br>",
                                       "National Registery of Historic Places: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$Join_Cou_2, "<br>",
                                       "High Priority Working Lands: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$WORKINGLAN, "<br>",
                                       "Commercial Fishing Reliance: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$ComEng_ct, "<br>",
                                       "Recreational Fishing Engagement: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$RecEng_ct)),
                    highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE,fillColor = "white",fillOpacity = 0.1),
                    labelOptions = labelOptions(textsize = "15px")
        )
      ps_list$hex_select<-ps_list$hex_select[ps_list$hex_select$OBJECTID!=as.character(hex_id),]
    }
    else if(!(as.character(hex_id)%in% ps_list$hex_select$OBJECTID) && as.character(hex_id) %in% ps_list$hex_merge_final$OBJECTID)
    {
      ps_list$hex_select<-rbind(ps_list$hex_select,ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==as.character(hex_id),])
      leafletProxy('mapresult')%>%
        removeShape(layerId = as.character(hex_id))%>%
        addPolygons(data=ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==as.character(hex_id),], fillOpacity = 0.6, layerId = as.character(hex_id),
                    fillColor = "gray",weight=0.4,group="View Hexagons",
                    label= HTML(paste0("Threat of Urbanization: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$Sleuth_v2, "<br>",
                                       "Connectivity with PAD-US: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$padus, "<br>",
                                       "Connectivity of Natural Lands Index: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$area_conne, "<br>",
                                       "Proposed Area of Conservation: ", "1", "<br>",
                                       "Composition of Natural Lands: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$conl_index, "<br>",
                                       "Imparied Watershed Area: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$area_12_13, "<br>",
                                       "Hydrologic Response to Land-Use Change: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$wq3, "<br>",
                                       "Percent Irrigated Agriculture: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$wq4, "<br>",
                                       "Lateral Connectivity of Floodplain: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$wq5, "<br>",
                                       "Composition of Riparian Zone Lands: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$wq6, "<br>",
                                       "Biodiversity Index: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$Index_cpt_, "<br>",
                                       "T&E Species Area: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$PA1, "<br>",
                                       "T&E Number of Species: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$statuscoun, "<br>",
                                       "Light Pollution Index: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$area_light, "<br>",
                                       "National Heritage Area: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$area_nha, "<br>",
                                       "National Registery of Historic Places: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$Join_Cou_2, "<br>",
                                       "High Priority Working Lands: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$WORKINGLAN, "<br>",
                                       "Commercial Fishing Reliance: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$ComEng_ct, "<br>",
                                       "Recreational Fishing Engagement: ", ps_list$hex_merge_final[ps_list$hex_merge_final$OBJECTID==hex_id,]$RecEng_ct)),
                    highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE,fillColor = "gray",fillOpacity = 0.5),
                    labelOptions = labelOptions(textsize = "15px")
        )
    }
  })
  
  observeEvent(input$updatetable,{
    ##print(length(ps_list$hex_merge_final$OBJECTID))
    ps_list$hex_merge_final<-subset(ps_list$hex_merge_final, !(ps_list$hex_merge_final$OBJECTID %in% ps_list$hex_select$OBJECTID))
    ##print(length(ps_list$hex_merge_final$OBJECTID))
    ps_list$hex_merge<-ps_list$hex_merge_final[match(unique(ps_list$hex_merge_final$OBJECTID),ps_list$hex_merge_final$OBJECTID),]
    
    ##print(result$matrix_raw)
    result$matrix<-NULL
    ##print("result$matrix")
    ##print(result$matrix)
    for(i in 1:length(ps_list$result)){
      data1<- ps_list$result[[i]]
      ##print(data1)
      ##print(ps_list$hex_merge)
      data2<-st_join(ps_list$hex_merge,data1,join=st_intersects,left=F)
      
      data2$appid<-i
      ##print(length(data2))
      result$datatable[[i]]<-c(as.numeric(as.numeric(st_area(data1)/1000000)),max(data2$PADUS2),sum(data2$area_conne),1-max(data2$Sleuth_v2),sum(data2$conl_index),sum(data2$area_12_13),mean(data2$wq3),(sum(data2$wq4)/length(data2$wq4))*100,mean(data2[data2$wq5>-1,]$wq5)*100,mean(data2[data2$wq6>-1,]$wq6),max(data2$Index_cpt_),sum(data2$PA1),max(as.numeric(as.character(data2$statuscoun))),min(data2$area_light),max(as.numeric(as.character(data2$Join_Cou_2))),sum(data2$area_nha),max(data2$SOVInew),max(data2$THREATINDE),sum(data2$WORKINGLAN),max(data2$ComEng_ct),max(data2$RecEng_ct),max(data2$AR_boat))
      ##print(result$datatable[[i]])
      result$datatable[[i]][3]<-result$datatable[[i]][3]/result$datatable_context[[i]][1]*100
      result$datatable[[i]][5]<-result$datatable[[i]][5]/result$datatable_context[[i]][1]*100 
      result$datatable[[i]][6]<-result$datatable[[i]][6]/result$datatable_context[[i]][1]*100 
      result$datatable[[i]][12]<-result$datatable[[i]][12]/result$datatable_context[[i]][1]*100 
      result$datatable[[i]][16]<-result$datatable[[i]][16]/result$datatable_context[[i]][1]*100 
      result$datatable[[i]][19]<-result$datatable[[i]][19]/result$datatable_context[[i]][1]*100 
      if(is.null(result$matrix)){result$matrix<-result$datatable[[i]]}
      else{result$matrix<-cbind(result$matrix,result$datatable[[i]])}
      
    }
    result$matrix_raw<-result$matrix
    ConservArea<-result$matrix[1,]
    #Maximum area of 303D, 
    #to be used in calculating utility for Impaired watershed
    #COMPREHENSIVE FOR LOOP
    for (i in 1:ncol(result$matrix)){
      #1. Threat of Urbanization
      result$matrix[4,i]<-result$matrix[4,i]
      #2. Connectivity with PAD-US
      result$matrix[2,i]<-result$matrix[2,i]
      #3. Connectivity of Natural Lands 
      result$matrix[3,i]<-result$matrix[3,i]/100
      #4. Proposed Area of Conservation
      result$matrix[1,i]<-ifelse(result$matrix[1,i]==0,0,
                                 ifelse(result$matrix[1,i]<=.4,.3,
                                        ifelse(result$matrix[1,i]<=.8,.75,
                                               ifelse(result$matrix[1,i]<=2,.9,1))))
      #5. Composition of Natural Lands
      result$matrix[5,i]<-result$matrix[5,i]/100
      #6. 303D - Impaired watershed area
      result$matrix[6,i]<-(result$matrix[6,i]/100) 
      #7. Hydrologic Response to Land-Use Change
      result$matrix[7,i]<-result$matrix[7,i]
      #8. Percent of irrigated agriculture
      result$matrix[8,i]<-result$matrix[8,i]/100
      #9. Lateral Connectivity of Floodplain
      result$matrix[9,i]<-result$matrix[9,i]/100
      #10. Composition of Riparian Zone Lands
      result$matrix[10,i]<-result$matrix[10,i]
      
      #11. Biodiversity Index
      result$matrix[11,i]<-result$matrix[11,i]/10
      #12. T&E Species Area
      result$matrix[12,i]<-ifelse(result$matrix[12,i]<=0.001,0,
                                  ifelse(result$matrix[12,i]<=20,.75,
                                         ifelse(result$matrix[12,i]<=60,.9,1)))
      #13. T&E Number of Species
      result$matrix[13,i]<-ifelse(result$matrix[13,i]==0,0,
                                  ifelse(result$matrix[13,i]==1,.9,
                                         ifelse(result$matrix[1,i]==2,.95,1)))
      #14. Light Pollution Index
      #if higher the better
      #result$matrix[9,i]<-result$matrix[9,i]
      #if lower the better
      result$matrix[14,i]<-1-result$matrix[14,i]
      
      #15. National Register of Historic Places
      result$matrix[15,i]<-ifelse(result$matrix[15,i]==0,0,
                                  ifelse(result$matrix[15,i]==1,.75,
                                         ifelse(result$matrix[15,i]==2,.9,1)))
      #16. National Heritage Area
      result$matrix[16,i]<-result$matrix[16,i]/100
      
      #17
      result$matrix[17,i]<-result$matrix[17,i]
      #18
      result$matrix[18,i]<-result$matrix[18,i]
      #19. Working Lands - High Priority
      result$matrix[19,i]<-result$matrix[19,i]/100
      #20. Commercial Fisheries - Engagement
      result$matrix[20,i]<-ifelse(result$matrix[20,i]==0,0,
                                  ifelse(result$matrix[20,i]==1,.25,
                                         ifelse(result$matrix[20,i]==2,.5,
                                                ifelse(result$matrix[20,i]==3,.75,1))))
      #21. Recreational Fisheries - Engagement
      result$matrix[21,i]<-ifelse(result$matrix[21,i]==0,0,
                                  ifelse(result$matrix[21,i]==1,.25,
                                         ifelse(result$matrix[21,i]==2,.5,
                                                ifelse(result$matrix[21,i]==3,.75,1))))
      #22. Access &Recreation
      result$matrix[22,i]<-ifelse(result$matrix[22,i]==0,0,
                                  ifelse(result$matrix[22,i]<=5,.25,
                                         ifelse(result$matrix[22,i]<=10,.75,
                                                ifelse(result$matrix[22,i]<=15,.9,1)))) 
    }
    
    if(max(result$matrix[7,])==0){
      max7<-1
    }else{
      max7<-max(result$matrix[7,])}
    result$matrix[7,]<-result$matrix[7,]/max7
    
    if(max(result$matrix[3,])==0){
      max3<-1
    }else{
      max3<-max(result$matrix[3,])}
    result$matrix[3,]<-result$matrix[3,]/max3
    
    if(max(result$matrix[5,])==0){
      max5<-1
    }else{
      max5<-max(result$matrix[5,])}
    result$matrix[5,]<-result$matrix[5,]/max5
    
    if(max(result$matrix[16,])==0){
      max16<-1
    }else{
      max16<-max(result$matrix[16,])}
    result$matrix[16,]<-result$matrix[16,]/max16
    
    if(max(result$matrix[19,])==0){
      max19<-1
    }else{
      max19<-max(result$matrix[19,])}
    result$matrix[19,]<-result$matrix[19,]/max19
    
    
    
    
    
    rownames(result$matrix) <- coln[1:nrow(result$matrix)]
    result$showing_matrix<-round(result$matrix,2)
    rownames(result$matrix_raw)<-coln[1:nrow(result$matrix_raw)]
    result$showing_matrix_raw<-round(result$matrix_raw,2)
    colnames(result$showing_matrix)<-proplist[1:ncol(result$showing_matrix)]
    colnames(result$showing_matrix_raw)<-proplist[1:ncol(result$showing_matrix_raw)]
    result$rankaccept_altlist<-proplist[1:ncol(result$showing_matrix)]
    ##print("result$matrix_raw")
    ##print(result$matrix_raw)
  })
  
  
  
  
  observeEvent(input$osupdatetable,{
    ##print(length(ps_list$hex_merge_final$OBJECTID))
    ps_list_os$hex_merge_final<-subset(ps_list_os$hex_merge_final, !(ps_list_os$hex_merge_final$OBJECTID %in% ps_list_os$hex_select$OBJECTID))
    ##print(length(ps_list_os$hex_merge_final$OBJECTID))
    ps_list_os$hex_merge<-ps_list_os$hex_merge_final[match(unique(ps_list_os$hex_merge_final$OBJECTID),ps_list_os$hex_merge_final$OBJECTID),]
    
    ##print(result$matrix_raw)
    result$matrix<-NULL
    ##print("result$matrix")
    ##print(result$matrix)
   
      data1<- ps_list_os$result
      ##print(data1)
      ##print(ps_list$hex_merge)
      data2<-st_join(ps_list_os$hex_merge,data1,join=st_intersects,left=F)
      ##print(length(data2))
      result_os$datatable<-c(as.numeric(as.numeric(st_area(data1)/1000000)),max(data2$PADUS2),sum(data2$area_conne),1-max(data2$Sleuth_v2),sum(data2$conl_index),sum(data2$area_12_13),mean(data2$wq3),(sum(data2$wq4)/length(data2$wq4))*100,mean(data2[data2$wq5>-1,]$wq5)*100,mean(data2[data2$wq6>-1,]$wq6),max(data2$Index_cpt_),sum(data2$PA1),max(as.numeric(as.character(data2$statuscoun))),min(data2$area_light),max(as.numeric(as.character(data2$Join_Cou_2))),sum(data2$area_nha),max(data2$SOVInew),max(data2$THREATINDE),sum(data2$WORKINGLAN),max(data2$ComEng_ct),max(data2$RecEng_ct),max(data2$AR_boat))
      ##print(result$datatable[[i]])
      #print(result_os$datatable)
      result_os$datatable_context<-c(sum(data2$metal_cuzo),min(data2$X303d_desig),length(data2$area_conne))
      ##print("testing")
      ##print(result_os$datatable_context)
      
      result_os$matrix_context<-as.matrix(result_os$datatable_context)
      result_os$matrix<-as.matrix(result_os$datatable)
      incProgress(0.2, detail = paste("Calculating matrix "))
      
      
      
      rownames(result_os$matrix) <- coln
      result_os$showing_matrix<-round(result_os$matrix,2)
      result_os$showing_matrix[3]<-round(result_os$showing_matrix[3]/result_os$datatable_context[3]*100,0)
      result_os$showing_matrix[5]<-round(result_os$showing_matrix[5]/result_os$datatable_context[3]*100,0)
      result_os$showing_matrix[6]<-round(result_os$showing_matrix[6]/result_os$datatable_context[3]*100,0)
      result_os$showing_matrix[12]<-round(result_os$showing_matrix[12]/result_os$datatable_context[3]*100,0)
      result_os$showing_matrix[16]<-round(result_os$showing_matrix[16]/result_os$datatable_context[3]*100,0)
      result_os$showing_matrix[19]<-round(result_os$showing_matrix[19]/result_os$datatable_context[3]*100,0)
      colnames(result_os$showing_matrix)<-proplist_os
      #print(result_os$showing_matrix)
  })
  
  observeEvent(input$advance,{
    shinyjs::show("gotorawtable")
    shinyjs::show("gotodeselect")
    shinyjs::show("gotoutility")
    shinyjs::show("addweight")
    shinyjs::show("renameproject")
    shinyjs::show("downloadData")
    shinyjs::show("download2")
    shinyjs::show("adjustmcdanumbers")
    shinyjs::show("advancedoptions")
    
  })
  observeEvent(input$osadvance,{
    shinyjs::show("advancedoptionsos")
  })
  
  observeEvent(input$gotorawtable,{
    show(selector = "#viewdata li a[data-value=scaledata]")
    updateTabsetPanel(session = session, inputId = "viewdata", "scaledata")
  })
  
  observeEvent(input$gotodeselect,{
    show(selector = "#viewdata li a[data-value=deselect]")
    updateTabsetPanel(session = session, inputId = "viewdata", "deselect")
    shinyjs::show("updatetable")
    leafletProxy("mapresult")%>%
      addPolygons(data=SCA, fill = F ,group = "SCA Boundary",color = "red",
                  weight = .5, options = pathOptions(clickable = FALSE))
  })
  
  observeEvent(input$gotoutility,{
    show(selector = "#viewdata li a[data-value=utility]")
    updateTabsetPanel(session = session, inputId = "viewdata", "utility")
  })
  
  output$textresult <- renderUI({
    
    verbatimTextOutput("textresult1")
  })
  
  observe({
    if(value1$txtbackend == 10){
      for(i in 1: nrow(result$showing_central)){
        #if(var(result$showing_central[i,])>10){
        #  temp<-"This proposal has a bias among different RESTORE Goals."
        #}
        #else{
        #  temp<-"This proposal has no significant bias among different RESTORE Goals."
        #}
        #ranktxt<-c("first","second","third","fourth","fifth","sixth","seventh","eighth","nineth","tenth")
        value1$txt<-paste0(value1$txt, 
                        proplist[i],
                        " was ranked first ", 
                        round(as.numeric(result$showing_rankaccept[i]),1),
                        "% of the time.","\n \n"
                        #, and mostly ranked (",
                        #round(as.numeric(result$showing_rankaccept[i,order(result$showing_rankaccept[i,],decreasing=T)[1]]),1),
                        #"%) in the ",
                        #ranktxt[order(result$showing_rankaccept[i,],decreasing=T)[1]],
                        #" place.\n",
                        #temp,"\n \n"
        )
      }
      value1$txtbackend<-0
    }
  })
  
  output$textresult1<-renderText({value1$txt})
  
  observeEvent(input$refresh, {
    js$refresh();
  })
  
  output$mode<-renderText({value1$mode})
  outputOptions(output, "mode", suspendWhenHidden = FALSE)
  
  
  addPopover(session, "habitat1", "Info", content = paste0("The default weighting for each of the five goals is 20%. 
                                                           However, a user may adjust the weighting of the goals to reflect their preferred values. 
                                                           The only weighting limitation is that the total weight of all goals must sum to 100%."), trigger = 'focus')
  addPopover(session, "habitat", "Info", content = paste0("The default weighting for each of the five goals is 20%. 
                                                           However, a user may adjust the weighting of the goals to reflect their preferred values. 
                                                           The only weighting limitation is that the total weight of all goals must sum to 100%."), trigger = 'focus')
  addPopover(session, "water1", "Info", content = paste0("The default weighting for each of the five goals is 20%. 
                                                           However, a user may adjust the weighting of the goals to reflect their preferred values. 
                                                           The only weighting limitation is that the total weight of all goals must sum to 100%."), trigger = 'focus')
  addPopover(session, "water", "Info", content = paste0("The default weighting for each of the five goals is 20%. 
                                                           However, a user may adjust the weighting of the goals to reflect their preferred values. 
                                                           The only weighting limitation is that the total weight of all goals must sum to 100%."), trigger = 'focus')
  addPopover(session, "species1", "Info", content = paste0("The default weighting for each of the five goals is 20%. 
                                                           However, a user may adjust the weighting of the goals to reflect their preferred values. 
                                                           The only weighting limitation is that the total weight of all goals must sum to 100%."), trigger = 'focus')
  addPopover(session, "species", "Info", content = paste0("The default weighting for each of the five goals is 20%. 
                                                           However, a user may adjust the weighting of the goals to reflect their preferred values. 
                                                           The only weighting limitation is that the total weight of all goals must sum to 100%."), trigger = 'focus')
  addPopover(session, "resilience1", "Info", content = paste0("The default weighting for each of the five goals is 20%. 
                                                           However, a user may adjust the weighting of the goals to reflect their preferred values. 
                                                           The only weighting limitation is that the total weight of all goals must sum to 100%."), trigger = 'focus')
  addPopover(session, "resilience", "Info", content = paste0("The default weighting for each of the five goals is 20%. 
                                                           However, a user may adjust the weighting of the goals to reflect their preferred values. 
                                                           The only weighting limitation is that the total weight of all goals must sum to 100%."), trigger = 'focus')
  addPopover(session, "economy1", "Info", content = paste0("The default weighting for each of the five goals is 20%. 
                                                           However, a user may adjust the weighting of the goals to reflect their preferred values. 
                                                           The only weighting limitation is that the total weight of all goals must sum to 100%."), trigger = 'focus')
  addPopover(session, "economy", "Info", content = paste0("The default weighting for each of the five goals is 20%. 
                                                           However, a user may adjust the weighting of the goals to reflect their preferred values. 
                                                           The only weighting limitation is that the total weight of all goals must sum to 100%."), trigger = 'focus')
  addPopover(session, "hab_PA_measures_Table", "Info", content = paste0('If a user decides to give any goal a weight of zero, the measures aligned to that goal will also be zeroed out (ignored completely). 
  A user can also adjust the weight of each measure. The default weight for each of the measures is set at medium (66%). 
  A measures weight of low indicate 33%, medium 66% and high 100%.  
  Any measure can be assigned a value of zero, and as such that measure will be ignored by the model. ' ), trigger = 'focus')
  
  addPopover(session, "wq_PA_measures_Table", "Info", content = paste0('If a user decides to give any goal a weight of zero, the measures aligned to that goal will also be zeroed out (ignored completely). 
  A user can also adjust the weight of each measure. The default weight for each of the measures is set at medium (66%). 
  A measures weight of low indicate 33%, medium 66% and high 100%.  
  Any measure can be assigned a value of zero, and as such that measure will be ignored by the model. ' ), trigger = 'focus')
  
  addPopover(session, "lcmr_PA_measures_Table", "Info", content = paste0('If a user decides to give any goal a weight of zero, the measures aligned to that goal will also be zeroed out (ignored completely). 
  A user can also adjust the weight of each measure. The default weight for each of the measures is set at medium (66%). 
  A measures weight of low indicate 33%, medium 66% and high 100%.
  Any measure can be assigned a value of zero, and as such that measure will be ignored by the model. ' ), trigger = 'focus')
  
  addPopover(session, "commres_PA_measures_Table", "Info", content = paste0('If a user decides to give any goal a weight of zero, the measures aligned to that goal will also be zeroed out (ignored completely). 
  A user can also adjust the weight of each measure. The default weight for each of the measures is set at medium (66%). 
  A measures weight of low indicate 33%, medium 66% and high 100%.  
  Any measure can be assigned a value of zero, and as such that measure will be ignored by the model. '), trigger = 'focus')
  
  addPopover(session, "gulfecon_PA_measures_Table", "Info", content = paste0('If a user decides to give any goal a weight of zero, the measures aligned to that goal will also be zeroed out (ignored completely). 
  A user can also adjust the weight of each measure. The default weight for each of the measures is set at medium (66%). 
  A measures weight of low indicate 33%, medium 66% and high 100%.  
  Any measure can be assigned a value of zero, and as such that measure will be ignored by the model. '), trigger = 'focus')
 
  observeEvent(input$showsupportlayeros,{
    leafletProxy("mapresult4")%>%
      showGroup("FNAI BOT Conservation Areas")%>%
      showGroup("AL Conservation Opportunity Areas")%>%
      showGroup("MS Conservation Opportunity Areas")%>%
      showGroup("LA Conservation Opportunity Areas")%>%
      
      addPolygons(data=SCA, fill = F ,group = "SCA Boundary", color = "red",
                  weight = .5, options = pathOptions(clickable = FALSE))
  })
  
  observeEvent(input$showsupportlayer,{
    leafletProxy("mapresult5")%>%
      showGroup("FNAI BOT Conservation Areas")%>%
      showGroup("AL Conservation Opportunity Areas")%>%
      showGroup("MS Conservation Opportunity Areas")%>%
      showGroup("LA Conservation Opportunity Areas")%>%
      addPolygons(data=SCA, fill = F ,group = "SCA Boundary",color = "red",
                  weight = .5, options = pathOptions(clickable = FALSE))
  })
  
  observeEvent(input$learnmore,{
    showModal(div(id="ModalDiv123", modalDialog(
      title = "How to draw your own area of interest",
      HTML(c("<div align='center'><img src='./cptos.gif' style='width:600px;height:400px;'></div>
             <div align='left'>
             &nbsp; &nbsp;Using this tool, you can identify your own area of interest by following the steps below.<br/>
             &nbsp; &nbsp; Steps:</br>
             <ul>
             <li>Enable drawing feature by clicking the draw tool bar</li>
             <li>Finishing one or more polygons as area of interest</li>
             <li>Naming the area of interest</li>
             <li>Finalizing the input</li>
             </ul>
             </div>")),
      footer = tagList(modalButton("OK")),
      size = "l"
    )))
  })
  observeEvent(input$learnmoreos,{
    showModal(div(id="ModalDiv123", modalDialog(
      title = "How to draw your own area of interest",
      HTML(c("<div align='left'>
             &nbsp; &nbsp;Using this tool, you can identify your own area of interest by following the steps below.<br/>
             &nbsp; &nbsp; Steps:</br>
             <ul>
             <li>Enable drawing feature by clicking the draw tool bar</li>
             <li>Finishing one or more polygons as area of interest</li>
             <li>Naming the area of interest</li>
             <li>Finalizing the input</li>
             </ul>
             </div>","<div align='left'><a><img src='./cptos.gif' style='width:600px;height:400px;'></a></div>"
             )),
      footer = tagList(modalButton("OK")),
      size = "l"
    )))
  })
   
}

###END OF SERVER

