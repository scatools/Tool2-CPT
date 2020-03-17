
#####Table of information to be referenced in CPT reports#############################
##All elements start with 'plan_'
##elements that report names end with 'name'
##elements that report numbers end with 'num'
##elements that report percentages end with 'perc'
##elements that report any other value will end with the unit that value is reported in, such as 'acres' or 'sq meters'
library(tractor.base) #needed to use implode()

report_table<-data.frame("plan_Name"="Grand Bay",              #Proposed area name / IN APP
                         #"plan_PadUSname"= "Grand Bay NERR",   #Connectivity w/PAD-US / NOT AVAILABLE RIGHT NOW 
                         "plan_acres"= 2000,                   #Proposed Area of Conservation / IN APP
                         #"plan_Habitatnum"= 3,                 #Number of Habitats within proposed area  / NOT AVAILABLE RIGHT NOW 
                         "plan_strucHubperc"= 32,              #% of proposed Area comprised of hubs & corridors / IN APP
                         "plan_TEnum"= 2,                      #Number of T&E Species within proposed area / IN APP
                         "plan_TEcrithabnum"= 1,               #Number of T&E Species with critical habitat in proposed area / NOT AVAILABLE RIGHT NOW
                         "plan_TEperc"= 5,                     #% of critical habitat within proposed area for a T&E species / IN APP, but not for ind. species
                         "plan_HPCperc"=29,                    #% of proposed area comprised of High Priority Habitat / IN APP
                         "plan_Worklandsperc"=31,              #% of proposed area comprised of Working Lands / IN APP
                         "plan_Impairedperc"=5,                #% of proposed area comprised of impaired watershed / IN APP
                         "plan_Urbanthreat"="high",            #potential threat of development within proposed area (low/med/high) / IN APP
                         "plan_RSTORE_check"=1,                #is the AOI within RESTORE boundary?
                         "plan_Urbanthreat_check"=1,           #is there an urban threat within the AOI?
                         "plan_PADUS_check"=1,                 #is there protected land within 1 sq km?
                         "plan_crithab_check"=1,               #is there critical habitat for a T&E in the AOI?
                         "plan_impairedwb_check"=1,            #is there impaired waterbody in AOI?
                         "plan_historic_check"=1,              #is there historic place within AOI?
                         "plan_heritage_check"=1)              #is there heritage area within AOI?
##For elements with multiple items, read in separately with list(c())

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

##########Check boxes##################


#PROPOSED AREA NAME  (to create a shorter object name, since it is likely to be used a lot)
AOI<-report_table[1,1]

#Statement on habitat diversity
hab_abundance<-ifelse(report_table$plan_Habitatnum>=5,"a broad diversity of habitats including",
                      ifelse(report_table$plan_Habitatnum>=3,"several distinct habitats including",""))

#Statement for T&E species
TE_text<-ifelse(report_table$plan_TEnum>=2,"This area of interest supports habitat ranges for two federally listed species",
                ifelse(report_table$plan_TEnum==1,"This area of interest supports habitat for the federally listed ",
                       "No federally endangered species are known to inhabit the project area."))

#Statement for working lands
#WL_list<-ifelse(length(report_table$plan_HPWLname[[1]])>2,
#                paste(implode(report_table$plan_HPWLname[[1]][-length(report_table$plan_HPWLname[[1]])],sep=", "),
#                      report_table$plan_HPWLname[[1]][length(report_table$plan_HPWLname[[1]])],sep = ", and "),
#                ifelse(length(report_table$plan_HPWLname[[1]])==2,implode(report_table$plan_HPWLname[[1]],sep = " and "),
#                report_table$plan_HPWLname[[1]]))
WL_list<-ifelse(length(report_table$plan_HPWLname[[1]])>2,
                implode(report_table$plan_HPWLname[[1]],sep=", ",", and "),
                ifelse(length(report_table$plan_HPWLname[[1]])==2,
                implode(report_table$plan_HPWLname[[1]],sep=" and "),
                       report_table$plan_HPWLname[[1]]))
WL_perc<-report_table$plan_Worklandsperc
WL_text<-ifelse(length(report_table$plan_HPWLname[[1]])>=2,
                paste("Conserving this area of interest would protect ",WL_list," working lands (about ",
                      WL_perc," percent of the project area)",sep = ""),
                ifelse(report_table$plan_HPWLname[[1]]!=-99, 
                       paste("Conserving this area of interest would also provide protection to working lands, ",
                             "with ",WL_list," comprising roughly ",WL_perc," percent of the landscape",sep=""),
                "This area of interest provides no protection of working lands"))

#Statement for water quality
#WQ_list<-ifelse(length(report_table$plan_Impairedwbodyname[[1]])>2,
#                paste(implode(report_table$plan_Impairedwbodyname[[1]][-length(report_table$plan_Impairedwbodyname[[1]])],sep=", "),
#                      report_table$plan_Impairedwbodyname[[1]][length(report_table$plan_Impairedwbodyname[[1]])],sep = ", and "),
#                ifelse(length(report_table$plan_Impairedwbodyname[[1]])==2,implode(report_table$plan_Impairedwbodyname[[1]],sep = " and "),
#                       report_table$plan_Impairedwbodyname[[1]]))
WQ_list<-ifelse(length(report_table$plan_Impairedwbodyname[[1]])>2,
                implode(report_table$plan_Impairedwbodyname[[1]],sep=", ",", and "),
                ifelse(length(report_table$plan_Impairedwbodyname[[1]])==2,
                       implode(report_table$plan_Impairedwbodyname[[1]],sep=" and "),
                       report_table$plan_Impairedwbodyname[[1]]))
WQ_perc<-paste(report_table$plan_Impairedperc, " percent of the project area)")
WQ_text<-ifelse(length(report_table$plan_Impairedwbodyname[[1]])>=2,
paste("This area of interest also buffers water flowing into waterbodies with known impairments (",WQ_list,") ",  
      "and preservation would allow this landscape to continue to provide such water quality protections",sep=""),
ifelse(report_table$plan_Impairedwbodyname[[1]]!=-99,
       paste("This area of interest also buffers water flowing into the ", WQ_list,", a waterbody with known impairments, ",  
           "and preservation would allow this landscape to continue to provide such water quality protections",sep=""),
       "No waterways in or around this area of interest are considered impaired"))


#Statement for Threat of development
Sleuth_text<-paste("In the future, the ",AOI, " area may be vulnerable to inundation due to sea level rise,
and is expected to have a ", report_table$plan_Urbanthreat, " threat of development
by the year 2060 according to the SLEUTH urbanization model",sep="")

