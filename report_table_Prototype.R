#Table for report prototype#
#Last Updated: OCT_10_19 By: ACS


library(tractor.base) #needed to use implode()

report_table_1<-data.frame("AOI_Name"="Grand Bay",               
                         "AOI_Area"= 10000,
                         "Connectivity"= 34,
                         "PADUS"= "NA",
                         "SLEUTH"= 3,
                         "HPL_Num"= 3,     
                         "HPL_Perc"= 45,
                         "Impaired_Perc"=96,
                         "Impaired_Name"="Escatawpa River",
                         "Stream_Abund"=15,
                         "Qp_Change"= 2,
                         "Biodiversity"=1,
                         "TE_Perc"=2,
                         "TE_Num"= 1,              
                         "TE_Name"= "Gopher Tortoise", 
                         "Light_Pollut"= 1, 
                         "Historic"= 3,
                         "Heritage"=1,
                         "SOVI"= 2,                
                         "Comm_Threat"= 1,            
                         "HPWL_Num"= 2,                     
                         "HPWL_Perc"=29, 
                         "C_Fish"= 2,                 
                         "R_Fish"= 1,
                         "A_and_R"= 12)            


#Rewrite the index values to English
report_table_1$Qp_Change<-ifelse(report_table_1$Qp_Change==0,"high",
                                 ifelse(report_table_1$Qp_Change==1,"medium-high",
                                        ifelse(report_table_1$Qp_Change==2,"medium",
                                               ifelse(report_table_1$Qp_Change==3,"medium-high",
                                                      ifelse(report_table_1$Qp_Change==4,"high")))))


#3 is high, 2 med, 1 low, 0 NA
weight_table<-data.frame("AOI_Area"= 1,
                         "Connectivity"= 3,
                         "PADUS"= 1,
                         "SLEUTH"= 2,
                         "HPL_Num"= 3,     
                         "HPL_Perc"= 3,
                         "Impaired_Perc"=3,
                         "Stream_Abund"=2,
                         "Qp_Change"= 2,
                         "Biodiversity"=1,
                         "TE_Perc"= 3,              
                         "TE_Num"=3, 
                         "Light_Pollut"= 1, 
                         "Historic"= 3,
                         "Heritage"=1,
                         "SOVI"= 2,                
                         "Comm_Threat"= 1,            
                         "HPWL_Num"= 2,                     
                         "HPWL_Perc"=3, 
                         "C_Fish"= 3,                 
                         "R_Fish"= 1,
                         "A_and_R"= 0)

##Conditional Statements
dat_1_1<-paste0("This report evaluates the ","**",report_table_1$AOI_Name,"**"," area of interest")
dat_2_1<-paste0(", approximately ","**",report_table_1$AOI_Area,"**"," acres of land")
dat_3_1<-paste0(", that is within 1 km of currently protected land, according to the PAD-US layer. ")
dat_3_2<-paste0(", that is not within 1 km of currently protected land. ")
dat_4_1<-paste0(report_table_1$AOI_Name," also supports structural connectivity, as ",
                                                                         report_table_1$Connectivity," percent of the area is classified as a hub or corridor by the EPA National Ecological Framework (NEF). ")
dat_4_2<-paste0(report_table_1$AOI_Name," does not have any land classified as a hub or corridor by the EPA National Ecological Framework (NEF). ")
dat_5_1<-paste0(report_table_1$AOI_Name," is expected to have a ",report_table_1$SLEUTH," threat of development by the year 2060, according to the SLEUTH urbanization model.")
dat_5_2<-paste0(report_table_1$AOI_Name," is currently urbanized. ")
dat_5_3<-paste0("There is insufficient data to determine the future threat of development for ",report_table_1$AOI_Name,". ")
dat_6_1<-paste0(report_table_1$AOI_Name," houses ","**",report_table_1$HPL_Num,"**"," habitats deemed high priority, ")
dat_6_2<-paste0(report_table_1$AOI_Name," houses ",report_table_1$HPL_Num," habitat that is deemed high priority, ")
dat_6_3<-paste0(report_table_1$AOI_Name," is not known to house any habitats deemed high priority. ")
dat_7_1<-paste0("roughly ","**",report_table_1$HPL_Perc,"**"," **percent**"," of the area of interest. ")
dat_8_1<-paste0("This area of interest also buffers water flowing into the ",report_table_1$Impaired_Name,
                ", a waterbody with known impairments, and preservation would allow this landscape to continue to provide such water quality protections.")
dat_9_1<-paste0("Approximately ",report_table_1$Impaired_Perc," percent of the waterways within ",report_table_1$AOI_Name," are designated as impaired according to the EPA's 303(d) list.")
dat_10_1<-paste0("This area of interest contains roughly ",report_table_1$Stream_Abund," km of stream according to the National Hydrography Dataset.")
dat_10_2<-paste0("There are not any streams or rivers recognized within the area of interest.")
dat_11_1<-paste0("Land-use change in ",report_table_1$AOI_Name," has resulted in a ",report_table_1$Qp_Change," hydrologic response to a standard rainfall event for this region.")
dat_11_2<-paste0("There is insufficient data to determine the hydrologic response of ",report_table_1$AOI_Name," to land-use change.")
dat_12_1<-paste0("The landscape of ",report_table_1$AOI_Name," has a ",report_table_1$Biodiversity," biodiversity index, in accordance with the methods used by Jenkins et. al, 2015.")
dat_13_1<-paste0("Lands within ",report_table_1$AOI_Name," support roughly ",report_table_1$TE_Perc," of the critical habitat ranges for two or more federally listed species, including the [T&E Spp Names].")
dat_13_2<-paste0("Lands within ",report_table_1$AOI_Name," support roughly ",report_table_1$TE_Perc," of the critical habitat range for [T&E Species], a federally listed species.")
dat_13_3<-paste0("Lands within ",report_table_1$AOI_Name," are not known to support critical habitats for any federally listed species.")
dat_14_1<-paste0("Lands within ",report_table_1$AOI_Name," support habitat ranges for ",report_table_1$TE_Num," federally listed species, ")
dat_14_2<-paste0("Lands within ",report_table_1$AOI_Name," support the habitat range of the ","**",report_table_1$TE_Name,"**",", a federally listed species. ")
dat_14_3<-paste0("Lands within ",report_table_1$AOI_Name," are not known to support habitat ranges for any federally listed species. ")
dat_15_1<-paste0("including the ",implode(report_table_1$TE_Name[[1]],sep=", ",", and "))
dat_15_2<-paste0(" the ",implode(report_table_1$TE_Name[[1]],sep=" and "))
dat_15_3<-paste0("")
dat_16_1<-paste0(report_table_1$AOI_Name," has a ",report_table_1$Light_Pollut," level of light pollution.")
dat_16_2<-paste0("There is no light pollution in the area of interest.")
dat_17_1<-paste0("The National Registry of Historic Places indicates that there are ",report_table_1$Historic," historic places within or around the area of interest. ")
dat_17_2<-paste0("No places listed under the National Registry of Historic Places are known to exist within or around the area of interest. ")
dat_18_1<-paste0("About ",report_table_1$Heritage," percent of ",report_table_1$AOI_Name," is within a designated National Heritage Area. ")
dat_18_2<-paste0(report_table_1$AOI_Name," is not within a designated National Heritage Area. ")
dat_19_1<-paste0("According to NOAA's Office for Coastal Management, the area of interest is nearby a community that is socially vulnerable.")
dat_19_2<-paste0("There is insufficient data to determine the social vulnerability of communities nearby [AOI Name].")
dat_20_1<-paste0("[AOI] is has a [CTI] threat from coastal flooding and severe storm hazards.")
dat_20_2<-paste0("There is insufficient data to determine [AOI Name]'s community threat index.")
dat_21_1<-paste0("Conserving this area of interest would also provide protection to working lands, with [Names of WL] ")
dat_21_2<-paste0("No working lands are known to exist within the area of interest. ")
dat_22_1<-paste0("comprising about ",report_table_1$HPWL_Perc," percent of the landscape. ")
dat_22_2<-paste0("")
dat_23_1<-paste0("The community in and around ",report_table_1$AOI_Name," has a ",report_table_1$C_Fish," level of commercial fishing reliance. ")
dat_23_2<-paste0("There is insufficient data to determine the commercial fishing reliance of the communities that ",report_table_1$AOI_Name," is associated with. ")
dat_24_1<-paste0("The community in and around ",report_table_1$AOI_Name," has a ",report_table_1$R_Fish," level of recreational fishing engagement. ")
dat_24_2<-paste0("There is insufficient data to determine the level recreational fishing engagement of the community that ",report_table_1$AOI_Name," is associated with.")
dat_25_1<-paste0("There are ",report_table_1$A_and_R," access points to natural areas within 25 km of ",report_table_1$AOI_Name,".")

#Blank Statement if measure is zero-weighted
dat_all_0<-paste0("")

report_table_2<-report_table_1

report_table_2$AOI_Name<-dat_1_1
report_table_2$AOI_Area<-ifelse(weight_table$AOI_Area==0,dat_all_0,dat_2_1)
report_table_2$PADUS<-ifelse(weight_table$PADUS==0,dat_all_0,
                             ifelse(report_table_1$PADUS==1,dat_3_1,
                             ifelse(report_table_1$PADUS==0,dat_3_2,paste0("."))))
report_table_2$Connectivity<-ifelse(weight_table$Connectivity==0,dat_all_0,
                                    ifelse(report_table_1$Connectivity>0,dat_4_1,dat_4_2))
report_table_2$SLEUTH<-ifelse(weight_table$SLEUTH==0,dat_all_0,
                              ifelse(report_table_1$SLEUTH!=0,dat_5_1,dat_5_2))
report_table_2$HPL_Num<-ifelse(report_table_1$HPL_Num>=2,paste0(" ",report_table_1$AOI_Name," houses ","**",report_table_1$HPL_Num,"**"," habitats deemed high priority, "),
                               ifelse(report_table_1$HPL_Num==1,paste0(report_table_1$AOI_Name," houses ",report_table_1$HPL_Num," habitat that is deemed high priority, "),
                                      paste0(report_table_1$AOI_Name," is not known to house any habitats deemed high priority. ")))
report_table_2$HPL_Perc<-ifelse(report_table_1$HPL_Num>=1,paste0("roughly ","**",report_table_1$HPL_Perc,"**"," **percent**"," of the area of interest. "),paste0(""))
report_table_2$Impaired_Perc<-ifelse(weight_table$Impaired_Perc==0,paste0(""),paste0("Approximately ",report_table_1$Impaired_Perc," percent of the waterways within ",report_table_1$AOI_Name," are designated as impaired according to the EPA's 303(d) list."))
report_table_2$Impaired_Name<-ifelse(report_table_1$Impaired_Name!=0,paste0("This area of interest also buffers water flowing into the ",report_table_1$Impaired_Name,
                                                                       ", a waterbody with known impairments, and preservation would allow this landscape to continue to provide such water quality protections."),paste0(""))
report_table_2$Stream_Abund<-ifelse(weight_table$Stream_Abund==0,"",paste0("This area of interest contains roughly ",report_table_1$Stream_Abund," km of stream according to the National Hydrography Dataset."))
report_table_2$Qp_Change<-ifelse(report_table_1$Qp_Change=="insufficient data",paste0("There is insufficient data to determine the hydrologic response of ",report_table_1$AOI_Name," to land-use change."),
                                 paste0("Land-use change in ",report_table_1$AOI_Name," has resulted in a ",report_table_1$Qp_Change," hydrologic response to a standard rainfall event for this region."))
report_table_2$Biodiveristy<-paste0("The landscape of ",report_table_1$AOI_Name," has a ",report_table_1$Biodiversity," biodiversity index, in accordance with the methods used by Jenkins et. al, 2015.")
report_table_2$TE_Num<-ifelse(report_table_1$TE_Num>=2,paste0("Lands within ",report_table_1$AOI_Name," support habitat ranges for ",report_table_1$TE_Num," federally listed species, "),
                              ifelse(report_table_1$TE_Num==1,paste0("Lands within ",report_table_1$AOI_Name," support the habitat range of the ","**",report_table_1$TE_Name,"**",", a federally listed species. "),
                                     paste0("Lands within ",report_table_1$AOI_Name," are not known to support habitat ranges for any federally listed species. ")))
report_table_2$TE_Name<-ifelse(report_table_1$TE_Num>2,paste0("including the ",implode(report_table_1$TE_Name[[1]],sep=", ",", and ")),
                               ifelse(report_table_1$TE_Num==2,paste0(" the ",implode(report_table_1$TE_Name[[1]],sep=" and ")),
                                      paste0("")))
report_table_2$Light_Pollut<-ifelse(report_table_1$Light_Pollut==0,paste0(report_table_1$AOI_Name," has a high level of light pollution."),
                                    ifelse(report_table_1$Light_Pollut<=.99,paste0(report_table_1$AOI_Name," has a medium level of light pollution."),
                                           paste0("There is no light pollution in the area of interest.")))
report_table_2$Historic<-ifelse(report_table_1$Historic!=0,paste0("The National Registry of Historic Places indicates that there are ",report_table_1$Historic," historic places within or around the area of interest."),
                                paste0("No places listed under the National Registry of Historic Places are known to exist within or around the area of interest."))
report_table_2$Heritage<-ifelse(report_table_1$Heritage!=0,paste0("About ",report_table_1$Heritage," percent of ",report_table_1$AOI_Name," is within a designated National Heritage Area."),
                                paste0(report_table_1$AOI_Name," is not within a designated National Heritage Area."))
report_table_2$SOVI<-ifelse(report_table_1$SOVI!=0,paste0("According to NOAA's Sea Level Rise mid-range predictions for 2060 and Social Vulnerability Index (SoVi), ",report_table_1$AOI_Name," is expected to have a ",report_table_1$SOVI_SLR," risk of vulnerability to inundation from sea level rise."),
                            paste0("There is insufficient data to determine the vulnerability of ",report_table_1$AOI_Name," to sea level rise."))                             
report_table_2$Comm_Threat<-ifelse(report_table_1$Comm_Threat!=0,paste0("According to NOAA's Coastal Flood Frequency data and Social Vulnerability Index (SoVi), ",report_table_1$AOI_Name," is expected to have a ",report_table_1$SOVI_Flood," risk of vulnerability to flooding."),
                                   paste0("There is insufficient data to determine the vulnerability of ",report_table_1$AOI_Name," to flooding."))
report_table_2$HPWL_Num<-ifelse(report_table_1$HPWL_Num>=1,paste0("Conserving this area of interest would also provide protection to working lands, with [Names of WL] "),
                                paste0("No working lands are known to exist within the area of interest."))
report_table_2$HPWL_Perc<-ifelse(report_table_1$HPWL_Num>=1,paste0("comprising about ",report_table_1$HPWL_Perc," percent of the landscape."),
                                 paste0(""))
report_table_2$C_Fish<-ifelse(report_table_1$C_Fish!=0,paste0("The community in and around ",report_table_1$AOI_Name," has a ",report_table_1$C_Fish," level of commercial fishing reliance."),
                              paste0("There is insufficient data to determine the commercial fishing reliance of the community that ",report_table_1$AOI_Name," is associated with."))
report_table_2$R_Fish<-ifelse(report_table_1$R_Fish!=0,paste0("The community in and around ",report_table_1$AOI_Name," has a ",report_table_1$R_Fish," level of commercial fishing reliance."),
                              paste0("There is insufficient data to determine the commercial fishing reliance of the community that ",report_table_1$AOI_Name," is associated with."))
report_table_2$A_and_R<-paste0("There are ",report_table_1$A_and_R," access points to natural areas within 25 km of ",report_table_1$AOI_Name,".")


#put variables in **bold** 
#include a references cited header
#order the definitions to be consistent with the data table
#explore interactibility of report
