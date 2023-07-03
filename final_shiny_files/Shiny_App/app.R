#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(shinythemes)
library(shinyjs)
library(leaflet)
library(plotly)
library(sf)
library(dplyr)
library(readxl)
library("writexl")
library(colorspace) 
library(RColorBrewer)
library(ggplot2)
library(viridis)
library(leaflet.extras)
library(htmlwidgets)
library(webshot)
library(purrr)
library(rsconnect)

# plotlyOutput("gsoil", height = "500px") %>% withSpinner(type = 4, color = "#CF4420", size = 1.25),

#   output$gsoil <- renderPlotly({
#gsoil
#})

#gsoil <- ggplot(soil_quality, aes(x = `G_Value`, y = `G_Area_acre`, fill = `G_Value`, 
                                  #text = paste0(`G_Value`, "\n", "Total Acres: ", round(`G_Area_acre`, 0)))) +
# geom_bar(stat = "identity")+
# coord_flip() +
# theme(legend.position = "none") +
# scale_x_discrete(limits = rev) +
# labs( title = "Total Acreage by Soil Quality Classification", y = "Acreage", x = "Soil Quality Classification")+
# scale_fill_manual(values=soilColors)
# gsoil <-ggplotly(gsoil, tooltip = "text")

##################
#leafletOutput("goochland_con") %>% withSpinner(type = 4, color = "#861F41", size = 1.25),

# output$goochland_con<- renderLeaflet({
#   goochland_con
# })

# gcon <- st_read("data/Conservation/Gooch_Preservation.shp") %>% st_transform("+proj=longlat +datum=WGS84")

# 
# goochland_con <- leaflet()%>%
#   addTiles() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   setView(lng=-77.9, lat=37.73, zoom=10.48) %>% 
#   addPolygons(data=gcon, weight=0, fillOpacity=0.5, fillColor="purple")%>%
#   addPolygons(data=gl_cnty, weight=2, color="black", fillOpacity=0, opacity = 1)

############################

merged_copy <- readRDS(file = "data/mergedNew.Rds")

# SOIL MAPPING

soilshape <- st_read("data/Soil_Mapping/soil_map/soil_map/VA085/spatial/soilmu_a_va085.shp")
soillabels <- read_excel("data/soil/soillabelsranking.xlsx")

joined_soil <- left_join(soilshape, soillabels, by = c("MUSYM" = "Map Unit Symbol")) 

joined_data <- joined_soil %>% 
  select(MUSYM, "Map Unit Name", ...3, geometry, Rating) %>%
  mutate(labels = Rating)

#st_write(joined_data, "C:/Users/deepd/Downloads/DSPG/DSPG2023_VCE_Hanover/DSPG2023_VCE_Hanover/joined_soil.shp")
color_map <- colorFactor(palette = alpha(turbo(length(unique(joined_data$Rating))), 1), 
                         domain = joined_data$Rating)
#loading in the county outiline shapefile
hanover_boundary <- st_read("data/Hanover_County_Boundary/Hanover_County_Boundary.shp")
boundaryleaflet <- hanover_boundary %>%
  st_as_sf() 


hansoil <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = joined_data, color = ~color_map(joined_data$Rating), stroke = FALSE, weight = 1, fillOpacity = 1) %>%
  addPolygons(data = boundaryleaflet, fillColor = "transparent",color = "black",fillOpacity = 0,weight = 1) %>%
  addLegend(values = joined_data$Rating, pal = color_map, position = "bottomright", title = "Soil Quality in Hanover County") %>%
  addProviderTiles(providers$CartoDB.Positron)


shapefile <- st_read("data/Hanover_Parcels/Hanover_Parcels.shp")
assess <- read_excel("data/Assessor_Data_Changed.xlsx")

baseHan <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = shapefile, weight = 0.1)  # Add the shapefile to the map

# Leaflet of the land types
colored_land <- merged_copy %>%
  select(geometry, land_use)

color_map <- colorFactor(palette = turbo(length(unique(merged_copy$land_use))), 
                         domain = merged_copy$land_use)

zoneHan <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = colored_land, color = ~color_map(colored_land$land_use), stroke = 1, weight = 1, fillOpacity = 1) %>%
  addLegend(values = colored_land$land_use, pal = color_map, position = "bottomright", title = "Land Parcel Types in Hanover County")

###########################

#setting a variable for the crop excel file
crop_excel <- "C:/Users/deepd/Downloads/DSPG/DSPG2023_VCE_Hanover/DSPG2023_VCE_Hanover/HanoverShiny/data/crop/croplabelswithcategory.xlsx"

#reading in the crop excel file and assigning it to crop data
crop_data <- read_excel(crop_excel)

#selecting just Count and Category columns from the data and assigning it to category data
category_data <- crop_data[, c("Count", "Category")]

combcrop <- aggregate(Count ~ Category, data = category_data, FUN = paste, collapse = ",")

combcrop$Category <- factor(combcrop$Category)

# Sum up the integers by category
summed_cat <- category_data %>%
  group_by(Category) %>%
  summarise(SumCount = sum(Count)) %>%
  ungroup()

#creating bar graph of count by category
landAll <- ggplot(summed_cat, aes(x = reorder(Category, SumCount),
                       y = SumCount, 
                       fill = Category,
                       text = paste0("Crop Cover Type: ", `Category`, "\n","Amount: ", `SumCount`))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_viridis_d(option = "viridis") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold")) +
  labs(x = "Use Category", 
       y = "Acres", 
       title = "Land Use in Hanover County by Category", 
       caption = "Data Source: USDA Cropland-CROS, 2022") 

landAll <- ggplotly(landAll, tooltip = "text")

#subsetting the crop data to only contain categories that are crops and assigning it to just crop
justcrop <- subset(crop_data, 
                   !(Category == "forested" | 
                       Category == "developed" | 
                       Category == "wetlands" | 
                       Category == "other" |
                       Category == "water"))

#setting Category to a factor so we can run viridis
justcrop$Category <- factor(justcrop$Category) 

# Sum up the integers by category
summed_catSub <- justcrop %>%
  group_by(Category) %>%
  summarise(SumCount = sum(Count)) %>%
  ungroup()


#plotting count by category with just crop data in a sideways bar graph
landCropONLY <- ggplot(summed_catSub, aes(x = reorder(Category, SumCount), y = SumCount, fill = Category,
                                          text = paste0("Crop Cover Type: ", `Category`, "\n","Amount: ", `SumCount`))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_viridis_d(option = "viridis") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold")) +
  labs(x = "Crop Type", 
       y = "Acres", 
       title = " Land Crops in Hanover County by Category", 
       caption = "Data Source: USDA Cropland-CROS, 2022") 


landCropONLY <- ggplotly(landCropONLY, tooltip = "text")
###############################################################

#assigning the data file path to soil excel
soil_excel <- "data/soil/soillabelsranking.xlsx"

#reading in soil excel and assigning it to soil data
soil_data <- read_excel(soil_excel)

#pulling just Rating, and Acres in AOI columns from soil data and assigning it to rateacre data
rateacre_data <- soil_data[, c("Rating", "Acres in AOI")]

#re-naming Rating and Acres in Aoi columns to soil rating and acres
colnames(rateacre_data) <- c("soil_rating", "acres")
rateacre_data_clean <- rateacre_data[-c(120), ]

#turning soil rating column to a factor to use viridis 
rateacre_data_clean$soil_rating <- factor(rateacre_data_clean$soil_rating) 

total_acres <- aggregate(acres ~ soil_rating, data = rateacre_data_clean, FUN = sum)

#creating a sideways bar plot showing the acres of each rating in the county with viridis
sR <- ggplot(total_acres, aes(x = reorder(soil_rating, acres), y = acres, fill = soil_rating,
                              text = paste0("Soil Rating: ", `soil_rating`, "\n","Acres: ", `acres`))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_viridis_d(option = "viridis") +
  theme(legend.position = "none") +
  labs(x = "Soil Rating", y = "Acreage", title = "USDA Soil Rating by Acerage in Hanover County", caption = "Data Source: USDA, NRCC Web Soil Survey, 2019") 

sR <- ggplotly(sR, tooltip = "text") 

#trying to read in soil map shapefile
soilmap <- st_read("data/soil/hanoversoil.shp")

# soilmapped <- ggplot() + 
#   geom_sf(data = soilmap,
#           aes(fill = "MUSYM"),
#           color = "black") +
#   scale_fill_viridis_d()

#soils based on suitability for solar installations 

#assigning the data file path to soil excel
solarsoil_excel <- "data/soil/solarsoils.xlsx"

#reading in soil excel and assigning it to soil data
solarsoil_data <- read_excel(solarsoil_excel)

#pulling just Rating, and Acres in AOI columns from soil data and assigning it to rateacre data
rateacre_solar <- solarsoil_data[, c("Rating", "Acres_in_AOI")]

#re-naming Rating and Acres in Aoi columns to soil rating and acres
colnames(rateacre_solar) <- c("soil_rating", "acres")

#turning soil rating column to a factor to use viridis 
rateacre_solar$soil_rating <- factor(rateacre_solar$soil_rating) 

#creating a sideways bar plot showing the acres of each rating in the county with viridis
rateacre <- ggplot(rateacre_solar, aes(x = reorder(soil_rating, acres), y = acres, fill = soil_rating,
                                       text = paste0("Soil Rating: ", `soil_rating`, "\n","Acreage: ", `acres`))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_viridis_d(option = "viridis") +
  theme(legend.position = "none") +
  labs(x = "Soil Rating", 
       y = "Acreage", 
       title = "Suitability for Soil-Anchored Solar Array by Acerage in Hanover County", 
       caption = "Data Source: USDA, NRCC Web Soil Survey, 2019") 

rateacre <- ggplotly(rateacre, tooltip = "text") 

###############################################################


acres_landuse <- merged_copy %>%
  select(LOT_ACRES, land_use)

# acres_landuse$land_use <- as.factor(acres_landuse$land_use)
# ggplot(acres_landuse, aes(land_use, LOT_ACRES, color = land_use))+
#   geom_col()+
#   theme(legend.position = "none", axis.text.x = element_text(size = 7))+
#   labs(x = "Land Use", y="Lot Acres")+
#   scale_y_continuous(labels = scales::comma)+
#   ggtitle("Lot Acres By Land Use")



acre <- as.data.frame(acres_landuse) %>%
  na.omit()%>%
  group_by(land_use)%>%
  summarize(mean_acres = mean(LOT_ACRES),
            median_acres = median(LOT_ACRES),
            min_acres = min(LOT_ACRES),
            max_acres = max(LOT_ACRES),
            sd_acres = sd(LOT_ACRES))



# acre$`Land Use` <- acre$land_use
# acre$`Mean Lot Acres` <- acre$mean_acres



mean_a <-ggplot(acre, aes(reorder(land_use, mean_acres), mean_acres, fill = land_use))+
  geom_col()+
  aes(text= paste0("Land Use:", `land_use`, "<br>",
                   "Mean Lot Acres:", `mean_acres`))+
  scale_fill_viridis_d()+
  theme(legend.position = "none", axis.text.x = element_text(size = 7))+
  labs(x = "Land Use Type", y="Lot Acres", caption = "Data Source: Hanover County Assessor Data")+
  ggtitle("Mean Lot Acres By Land Use Type")

interactive_plot <- ggplotly(mean_a, tooltip = "text") 


# Employment Graph
employ <- read.csv("data/Employment.csv")
employ <- head(employ, -1)
employ <- na.omit(employ)

############################################################################
employ_plot <- ggplot(employ, aes(x = reorder(EmploymentTypes, Percent), y = Percent, fill = EmploymentTypes,
                                  text = paste0("Employment Type:", `EmploymentTypes`, "\n","Percent: ", `Percent`))) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position = "none") +
  labs(title = "Total Employment For Each Industry", 
       x = "Industry",
       y = "Percent",
       caption = "Data Source: US Census ACS 5-Year 2019 Data") +
  coord_flip() 

employ_plot <- ggplotly(employ_plot, tooltip = "text")


###########################################################################


ui <- navbarPage(title = "DSPG 2023",
                 selected = "home",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')), 
                 useShinyjs(),
                 tabPanel("Home", value = "home",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   h1(strong("Land Use and Solar Farming Assessment in Hanover County"),
                                      br("")
                                    )
                           
                          ),
                          fluidRow(style = "margin: 8px;",
                                   align = "center",
                                   column(4,
                                          h2(strong("Project Background")),
                                          h4(strong("Setting")),
                                          p("Hanover county, Virginia is a predominantly rural area 
                                            located twelve miles north of the state capital, Richmond. 
                                            The county ranges over 474 square miles and is known for 
                                            its farmlands, rolling hills and forests bordered by the 
                                            Chickahominy and Pamunkey Rivers. Hanover’s rich agricultural 
                                            history has thrived from 1720 to present day through its
                                            tobacco cultivation, crop diversification, dairy farming
                                            and small family farms. Hence, the agricultural heritage has
                                            majorly influenced the landscape, community and rural charm of 
                                            the county."),
                                          p(),
                                          h4(strong("Problem")),
                                          p("Hanover county takes pride in their rural lifestyle and heritage 
                                            therefore, as they look to attain economic growth challenges arise. 
                                            The main problems facing this county as it looks to achieve
                                            economic growth are urban sprawl, land conversion and solar
                                            farm land usage. Urban sprawl is the extension of urban areas
                                            which in turn cuts into the rural land that makes up Hanover County.
                                            Furthermore, land conversion shifts land use from one purpose to another.
                                            For instance, agricultural land to commercial, residential and industrial
                                            land. Solar farm land development also cuts into the land that
                                            can be used for agricultural purposes."),
                                          p(),
                                          h4(strong("Project")),
                                          p(" Virginia Tech Department of Agricultural and Applied Economics
                                            Data Science for the Public Good (DSPG) program assesses land 
                                            conversion and solar farm land usage in Hanover County through 
                                            the use of data analytics, agricultural economics and geospatial tools.")
                                         ),
                                   column(4,
                                          h2(strong("Aims")),
                                          tags$li("Use GIS analysis to asses current land use patterns"),
                                          tags$li("Evaluate protected land and prime farmland"),
                                          tags$li("Analyze competing demands for prime farmland from solar energy"),
                                          tags$li("Identify parcels with the highest likelihood of transitioning to solar farms"),
                                          p(),
                                          leafletOutput("baseHan") %>% withSpinner(type = 6, color = "#861F41", size = 1.25)
                                   ),
                                   column(4,
                                          h2(strong("County Overview")),
                                          p("The county consists of towns and cities including Ashland, Beaverdam,
                                            Doswell, Hanover, Mechanicsville, Montpellier, and Rockville.
                                            According to the US Census as of July 2022, the population
                                            of the county is estimated to be above 110,000 people with
                                            around 50% of the population being female and slightly lower
                                            for males with around 18% under 18, about 5% under 5, and 
                                            about 18% over 65 years and over where it is white predominant
                                            at 85.6%. Regarding the population, it is said by the US Census
                                            that the population per square mile in 2020 is 235.2. With the 
                                            rich history that the county has, Hanover boasts many historical
                                            sites and landmarks. Adding to the fact it has a strong agricultural
                                            heritage, with fertile farmlands that add to its long history of farming.
                                            Hanover hosts a mix of suburban and rural environments with residential
                                            areas, agricultural farmland, commercial districts, industrial zones,
                                            and natural landscapes. With the county promoting outdoor recreation 
                                            activities, maintaining parks, trails, and recreational facilities 
                                            and even the amusement park Kings Dominion, there is much to do and see in 
                                            Hanover County to keep you occupied."),
                                          p()
                                   )
  
                                   
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: 6/14/2023')))
                          ) 
                          
                 ),
                 tabPanel("Sociodemographics",
                          fluidRow(style = "margin: 8px;",
                                   align = "center",
                                   h1(strong("Sociodemographics"),
                                    br(""))
                          ),
                          fluidRow(style = "margin: 8px;",
                                   align = "center",
                                   column(6,
                                          h2(strong("Hanover County Overview")),
                                          p("Hanover is known to have a rich history and background.
                                            Formed by the Virginia General Assembly on November 26th,
                                            1720 named in honor of King George the First of England, it is
                                            iconic for the historic landmarks in the county and other historical places.
                                            Hanover currently has 39 sites registered in the National Register of 
                                            Historical Places/ Virginia Landmarks Register and 56 sites in Virginia Historical Markers.
                                            In general, it has over 1700 historical sites within the locality! The National
                                            Historic Landmarks (NHL) are historic properties that illustrate the heritage of
                                            the United States and are officially recognized by the US government. The historic
                                            properties found in Hanover County include Hanover Courthouse, Scotchtown, and
                                            Malbourne/Edmund Ruffin Plantation. In particular, Hanover Courthouse is the symbol
                                            and pride of Hanover County which is dated back to around 1740 where it is one of
                                            the oldest courthouses in Virginia. Some exceptional historic resources found here
                                            include Hanover Tavern, Hanover Meeting House, Garthwright-Kelley House, Gaines Mill
                                            Battlefield, Cold Harbor Battlefield, Rural Plains and so much more. For more points
                                            of interest, there is a whole slew of Century Farms recognized by the Virginia 
                                            Department of Agriculture and Consumer Affairs where these farms, as the name
                                            suggests to some extent, have each been owned by farmer families for 100 years or more. 
                                            When visiting you can’t forget the fascinating battlefield sites that revolved around the 
                                            Civil War and the Revolutionary War. During the Civil War, in particular, Hanover County 
                                            was a frequented battlefield by Union and Confederate troops where Union troops, commanded
                                            by generals, fought their way through to Richmond against the Confederate Army led by Robert
                                            E. Lee. Hanover County has a vast array of historical and cultural land sites, markers, and resources
                                            that provide it the opportunity for benefits on all accounts from economic to cultural for the owners
                                            of the county as a whole."),
                                          p(),
                                          h2(strong("Statistics Summary")),
                                          p("Will be done once graphs are set and ready to be analyzed")
                                          ),
                                   column(6,
                                          h2(strong("Visualizations")),
                                          p("Visualizations go here"),
                                          plotlyOutput("employ_plot", height = "500px") %>% withSpinner(type = 6, color = "#CF4420", size = 1.5)
                                          ),
                                   column(12,
                                          h4(strong("References")),
                                          p("References go here")
                                          )
                          )
                                   
                 ),
                 navbarMenu("Policy",
                          tabPanel("Policy",
                                   
                                  fluidRow(style = "margin: 8px;",
                                           align = "center",
                                           h1(strong("Policy"),
                                              br(""))
                                  ),
                                  fluidRow(style = "margin: 8px;",
                                           align = "justify",
                                           column(4,
                                                  h2(strong("Federal")),
                                                  h4(strong("Forest Legacy Program (FLP)")),
                                                  p("The FLP is a federal conservation program orchestrated
                                                    by the U.S Forest Service collaborating with State agencies
                                                    to preserve and protect private forests through land
                                                    purchases or conservation easements. This program promotes
                                                    feasible forest management by granting economic benefits
                                                    to landowners persuading them to sustain their forest land.
                                                    The FLP has been successful in conserving more than 2.8
                                                    million acres of forest land spanning across the United States.
                                                    To qualify for this program, the land must be located within an 
                                                    identified Forest Legacy Area and a non-federal match of 25% to obtain the grant."),
                                                  p(),
                                                  h4(strong("Conservation Reserve Program (CRP)")),
                                                  p("The CRP is a land conservation program managed by the Farm
                                                    Service Agency aimed to trade yearly rental payments to farmers
                                                    enrolled for the agreement of removing land sensitive to agriculture 
                                                    production and plant species to implement conservation practices.
                                                    This is meant to ensure the enhancement of the environmental health 
                                                    and quality of land. The land registered in this program is contracted
                                                    for 10 to 15 years. Land desired to participate in this program
                                                    is agricultural land easily susceptible to erosion, located near
                                                    bodies of water or providing habitats to wildlife. Hence, the
                                                    motivating factor behind this program is to reduce soil erosion,
                                                    enhance wildlife habitats, improve water quality, and stimulate
                                                    conservation and restoration of land.  Conservation Reserve
                                                    Enhancement Program (CREP) is a sub-program that troubleshoots
                                                    priority conservation issues identified by localities."),
                                                  p(),
                                                  h4(strong("Emergency Conservation Program (ECP)")),
                                                  p("The ECP aims to support agricultural producers in repairing
                                                    and restoring any damaged farmland and agricultural infrastructure
                                                    due to natural disasters. Assistance from this program includes
                                                    financial support in order to remove debris, restore fencing,
                                                    repair water sources, and seed damaged areas. Funding is granted 
                                                    based on the severity of the damage to handle a portion of the expenses.")
                                           ),
                                           column(4,
                                                  h2(strong("State")),
                                                  h4(strong("The Code of Virginia")),
                                                  p("The code of Virginia enacts various policies that allow localities
                                                    to enter into voluntary agreements with landowners across the state within
                                                    districts designated to protect farm lands and forest lands. 
                                                    There are several land conservation policies outlined in the Code of Virginia:"),
                                                  tags$li("Virginia Conservation Easement Act (Title 10.1, Chapter 10) – 
                                                          This act develops a voluntary legal agreement to permanently 
                                                          limit the uses of the landowner’s land in order to protect its 
                                                          natural values. The act ensures that landowners maintain their 
                                                          rights to own and use their land or sell or pass it onto heirs."),
                                                  tags$li("Virginia Open-Space Land Act (Title 15.2, Chapter 18): 
                                                          The Commonwealth of Virginia is authorized to form partnerships
                                                          with landowners to decrease urban sprawl and protect open space
                                                          through this act. It influences the localities to designate parcels
                                                          of land in Hanover County for use as open-space land. Open-space 
                                                          land is defined as any “any land which is provided or preserved for
                                                          (i) park or recreational purposes, (ii) conservation of land or other 
                                                          natural resources, (iii) historic or scenic purposes, (iv) assisting
                                                          in the shaping of the character, direction, and timing of community development,
                                                          (v) wetlands as defined in § 28.2-1300, or (vi) agricultural and forestal production.” 
                                                          (Code of Virginia Code - Chapter 17. Open-Space Land Act)"),
                                                  tags$li("Agricultural and Forest Districts Act (Title 15.2, Chapter 43): 
                                                          This act develops legal agreements aimed to stimulate the preservation 
                                                          of land occupied by forest and land used for agriculture. For a duration 
                                                          of 4-10 years, the land is sustained with its pre-existing use once landowners 
                                                          agree to partake in this act."),
                                                  tags$li(" Land Preservation Tax Credit (Title 58.1, Chapter 32): Through this policy,
                                                          landowners who register property under the conservation land easements 
                                                          are granted income tax credit for 40% of the value of the land donated."),
                                                  p(),
                                                  h4(strong("Purchase of Development Rights Program (PDR)")),
                                                  p("The PDR program is conducted by the Virginia Department of 
                                                    Agriculture and Customer Services in order to assist in funding 
                                                    PDR programs with local agencies to compensate landowners who 
                                                    voluntarily partake in agricultural conservation easements.")
                                                  
                                           ),
                                           column(4,
                                                  h2(strong("County")),
                                                  h4(strong("Zoning Regulations")),
                                                  p("Zoning districts promote the preservation of open space, 
                                                    particularly in areas containing environmentally sensitive land.
                                                    This strategy encourages rural conservation districts to protect 
                                                    open spaces while providing the flexibility to develop residential areas."),
                                                  p(),
                                                  h4(strong("Rural Conservation Subdivisions")),
                                                  p("Since January 2013, Hanover county has established
                                                    34 Rural Conservation subdivisions. Rural Conservation zoning 
                                                    districts require a minimum of 70% of the acreage of a district 
                                                    to be placed in a conservation area. Thus, this zoning tool has ensured 
                                                    that a total of 5466 acres are included in conservation areas.")
                                           ),
                                           column(12,
                                                  align = "center",
                                                  h4(strong("References")),
                                                  p("References go here")
                                           )
                                  )
                          ),
                          tabPanel("Conservation Analysis",
                                   fluidRow(style = "margin: 8px;",
                                            align = "center",
                                            h1(strong("Conservation Analysis"),
                                               br(""))
                                   )
                          )
                 ),
                 navbarMenu("Land Use Analysis", #will need to revisit due to design conflict in google site
                            tabPanel("Land Use/Zoning",
                                     align = "center",
                                     fluidRow(style = "margin: 8px;",
                                              align = "center",
                                              h1(strong("Land Use/Zoning"),
                                                 br(""))
                                     ),
                                     fluidRow(style = "margin: 8px;",
                                              align = "center",
                                              column(6,
                                                     h2(strong("Land Use by Parcel")),
                                                     p("This map shows the parcel-level zoning 
                                                       classification for Hanover County. 
                                                       We used Virginia's land use codes, along 
                                                       with assessor data from Hanover County to display 
                                                       areas in the county according to their zoning ordinances.  
                                                       Here we see that the majority of the county is zoned for 
                                                       agricultural and residential use."),
                                                     p(),
                                                     p("In the map above we saw that the majority of 
                                                       area in Hanover county is zoned for agriculture. 
                                                       However, from using the same data we derived this bar
                                                       graph that shows the number of residential parcels is more 
                                                       than the number of agriculture parcels.")
                                              ),
                                              column(6,
                                                     h2(strong("Visualizations")),
                                                     p("Visualizations go here"),
                                                     plotlyOutput("interactive_plot", height = "500px") %>% withSpinner(type = 6, color = "#CF4420", size = 1.5),
                                                     leafletOutput("zoneHan") %>% withSpinner(type = 6, color = "#861F41", size = 1.25),
                                              )
                                     )
                                     
                            ),
                            tabPanel("Crop Cover",
                                     fluidRow(style = "margin: 8px;",
                                              align = "center",
                                              h1(strong("Crop Cover"),
                                                 br(""))
                                     ),
                                     fluidRow(style = "margin: 8px;",
                                              align = "center",
                                              column(6,
                                                     h2(strong("What we have so far")),
                                                     p("Used Arc GIS Pro with USDA Cropland-CROS data 
                                                       to map types of cropland cover over Hanover County."),
                                                     p(),
                                                     p("Joined the data from the cropland survey 
                                                       and parcel data in Stata to create a clean data 
                                                       file we will use for maps and graphs in R.")
                                              ),
                                              column(6,
                                                     h2(strong("Agricultural History")),
                                                     p("Agriculture is a dominant economic, cultural, 
                                                       and social force in Hanover County. Dating back to 
                                                       the colonial era, tobacco was the dominant cash crop and
                                                       was cultivated starting in the early 17th century and 
                                                       continuing well into the 19th century. However, after the 
                                                       soil become depleted due to overuse and market conditions
                                                       changed. During the turn of the 19th century, the focus shifted 
                                                       to other crops including corn, oats, and wheat, and to livestock 
                                                       farming, including cattle, pigs, sheep, poultry, 
                                                       etc. It is also seen that the landscape diversified 
                                                       agriculturally and many farmers started using different
                                                       crop rotation and soil conservation techniques. Once the 
                                                       railroad was introduced, transportation made it easier for 
                                                       farmers to bring their products to the market and at the same
                                                       time helped facilitate the growth of fruits, vegetables, 
                                                       and dairy farming. As time went on leading to the early 20th
                                                       century there were many advancements in technology regarding 
                                                       agriculture. Manual labor was replaced by machinery and tractors 
                                                       which led to increased productivity and efficiency. Then farmers 
                                                       adopted more modern techniques such as crop cultivation, pest control,
                                                       and irrigation. After multiple wars throughout centuries and
                                                       especially following World War II, much of the agricultural
                                                       landscape was converted to residential and commercial areas
                                                       which prompted suburban development. It is still the case 
                                                       though that currently nearly half of the county is covered by 
                                                       forests and/or a mixture of agriculture-forest land. Despite
                                                       many changes many farms have adapted into niche markets such as 
                                                       organic farming, agri-tourism, and farmers markets including the
                                                       support for initiatives to promote sustainable agriculture and to
                                                       preserve farmland. Currently today, Hanover County’s agricultural
                                                       presence plays a vital role in its economy, heritage, and local culture.  "),
                                                     p()
                                              )
                                     ),
                                     
                                     fluidRow(style = "margin: 8px;",
                                              column(6, 
                                                     h4(strong("Crop Covers")),
                                                     selectInput(inputId = "crop_type", label = "Select Variable:", width = "100%", choices = c(
                                                       "Row crops" = "RC",
                                                       "Horticulture crops" = "HC",
                                                       "Small grains" = "SG",
                                                       "Double cropped" = "DC",
                                                       "Forages" = "F",
                                                       "Tree crops" = "TC",
                                                       "Other" = "O",
                                                       "Forested" = "FR",
                                                       "Wetlands" = "WL",
                                                       "Water" = "W",
                                                       "Developed" = "DEV")
                                                     ),
                                                     imageOutput("crop_typePNG", width = "400px", height = "400px"),
                                                     p(),
                                                     plotlyOutput("landAll", height = "500px") %>% withSpinner(type = 6, color = "#CF4420", size = 1.5),
                                                     p(),
                                                     plotlyOutput("landCropONLY", height = "500px") %>% withSpinner(type = 6, color = "#CF4420", size = 1.5)
                                                     
                                                     
                                              )
                                              
                                              
                                     )
                                              
                                          
                                     
                                     
                                     
                            ),
                            tabPanel("Soil Type",
                                     fluidRow(style = "margin: 8px;",
                                              align = "center",
                                              h1(strong("Soil Type"),
                                                 br(""))
                                     ),
                                     fluidRow(style = "margin: 8px;",
                                              align = "center",
                                              column(6,
                                                     h2(strong("Soil Quality Analysis")),
                                                     p("We plan to use data from the National 
                                                       Cooperative Survey to show soil types of Hanover County. 
                                                       Soil type and quality is an influential factor in the resulting 
                                                       agricultural productivity of land. Understanding where the quality 
                                                       soil is will help us to identify the best farmland within the county."),
                                                     p(),
                                                     p("Using a graph that shows the acreage of different soil 
                                                       classes within the county will help to give insight to which
                                                       types of soil are more common than others."),
                                                     p(),
                                                     p("We can use this space to analyze the results of our
                                                       graph and give background information on what the different
                                                       soil classes mean."),
                                                     p(),
                                                     h4(strong("What We Have So Far")),
                                                     p(),
                                                     p("Used USDA-NRCS Soil Survey Geographic database to map
                                                       soil types over Hanover County in Arc GIS Pro."),
                                                     p(),
                                                     p("Joined the data from soil survey and parcel data in State
                                                       to create a clean data file we will use for maps and graphs in R.")
                                              )
                                              
                                     ),
                                     fluidRow(style = "margin: 8px;",
                                              align = "center",
                                              column(6,
                                                     h2(strong("Soil Types")),
                                                     p(),
                                                     plotlyOutput("sR", height = "500px") %>% withSpinner(type = 6, color = "#CF4420", size = 1.5),
                                                     p(),
                                                     plotlyOutput("rateacre", height = "500px") %>% withSpinner(type = 6, color = "#CF4420", size = 1.5)
                                                     
                                                    
                                              
                                     ),
                                     column(6,
                                            h2(strong("Soil Quality Leaflet Placeholder Title")),
                                            p(),
                                            leafletOutput("hansoil") %>% withSpinner(type = 6, color = "#861F41", size = 1.5),
                                           
                                     )
                            )
                      )
                           
                 ),
                 tabPanel("Solar Farming Assessment",
                          fluidRow(style = "margin: 8px;",
                                   align = "center",
                                   h1(strong("Solar Farming Assessment"),
                                      br(""))
                          ),
                          fluidRow(style = "margin: 8px;",
                                   h2(strong("Hanover Solar Assessment")),
                                   p("Hanover County is currently home to Mechanicsville Solar PV Park. 
                                     This 28-megawatt solar farm has been in operation since 2018. 
                                     Developed by SunEnergy1, the park spans 222 acres and consists of 93,000 modules. 
                                     The electricity generated by the solar farm is being sold to Dominion Energy 
                                     and has the capacity to power approximately 5,000 households."),
                                   p(),
                                   p("In addition to the Mechanicsville Solar PV Park, Hanover County has
                                     approved a new solar farm. Developed by Ameriesco Solar, this 22-acre 
                                     facility is capable of generating 5 megawatts of power, which is estimated
                                     to meet the energy needs of 1,500 homes. The farm is located on Peppertown
                                     road and is expected to have a lifespan of 40 years. As part of their environmental 
                                     commitment, the developer plans to plant pollinator-friendly vegetation between 
                                     the solar panels."),
                                   p(),
                                   p("Virginia's renewable energy goals have made the construction of 
                                     solar farms more common. The state has implemented a policy that requires 
                                     Dominion Energy to achieve 100% renewable energy by 2045, and Virginia Power, 
                                     a subsidiary of Dominion, to do the same by 2050. This policy encourages energy 
                                     companies to develop more sources of renewable energy. With the development of more
                                     energy sources, a degree of environmental impact is inevitable. Certain developments,
                                     like the new solar farm in Hanover County, off of Peppertown Road, have plans to
                                     retire the solar field at the end of their useful lives. The developers plan to 
                                     return the area to its natural state as best as they can. However, it is important
                                     to note that Hanover County currently does not have specific requirements for native or 
                                     pollinator-friendly vegetation at solar facilities, as outlined in their zoning ordinance."),
                                   p(),
                                   h2(strong("Optimal Solar Farm Locations")),
                                   p("Solar farms require large areas of space, clear from any obstructions,
                                     such as trees or buildings that could potentially cast shadows onto the panels.
                                     This helps to ensure that they have maximum exposure to sunlight
                                     throughout the day. Having flat land is also preferred for solar farms
                                     as it simplifies installation, and allows for better panel
                                     positioning and alignment. The energy produced from the solar 
                                     farm has to be sent to a substation where it is stored before being
                                     released onto the grid. Therefore, a solar farm in close proximity to a
                                     substation is beneficial as it reduces the need to run lines long distances.
                                     Solar farms will also need to be inspected and maintained throughout their 
                                     operation, so service road access is very important when determining suitable properties."),
                                   p()
                          )
                          
                 ),
                 tabPanel("Findings Report",
                          fluidRow(style = "margin: 8px;",
                                   align = "center",
                                   h1(strong("Findings Report"),
                                      br(""))
                          ),
                          fluidRow(style = "margin: 8px;",
                                   h2(strong("Agriculture")),
                                   p(""),
                                   p(),
                                   h2(strong("Land Use")),
                                   p(""),
                                   p(),
                                   h2(strong("Solar Farming")),
                                   p(""),
                                   p()
                          )
                                   
                 ),
                 tabPanel("Data Sources",
                          fluidRow(style = "margin: 8px;",
                                   h1(strong("Data Sources"),
                                      br(""))
                          )
                 )
           
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$interactive_plot <- renderPlotly({
    interactive_plot
  })
  
  output$crop_typePNG <- renderImage(deleteFile = FALSE,{
    if(input$crop_type == "RC"){
      return(list(src = "www/RowCrops.png", width = "100%", height = "100%"))
    }
    else if(input$crop_type == "HC"){
      return(list(src = "www/HorCrops.png", width = "100%", height = "100%"))
    }
    else if(input$crop_type == "SG"){
      return(list(src = "www/SmallGR.png", width = "100%", height = "100%"))
    }
    else if(input$crop_type == "DC"){
      return(list(src = "www/DobCrop.png", width = "100%", height = "100%"))
    }
    else if(input$crop_type == "F"){
      return(list(src = "www/Forages.png", width = "100%", height = "100%"))
    }
    else if(input$crop_type == "TC"){
      return(list(src = "www/TreeCrops.png", width = "100%", height = "100%"))
    }
    else if(input$crop_type == "O"){
      return(list(src = "www/Other.png", width = "100%", height = "100%"))
    }
    else if(input$crop_type == "FR"){
      return(list(src = "www/Forests.png", width = "100%", height = "100%"))
    }
    else if(input$crop_type == "WL"){
      return(list(src = "www/Wetlands.png", width = "100%", height = "100%"))
    }
    else if(input$crop_type == "W"){
      return(list(src = "www/Water.png", width = "100%", height = "100%"))
    }
    else if(input$crop_type == "DEV"){
      return(list(src = "www/Developed.png", width = "100%", height = "100%"))
    }
  })
  
  output$employ_plot <- renderPlotly({
    employ_plot
  })
  
  output$landAll <- renderPlotly({
    
    landAll
  
  })
  
  output$landCropONLY <- renderPlotly({
  
    landCropONLY
    
  })
  
  output$sR <- renderPlotly({
    
    sR
    
  })
  
  output$rateacre <- renderPlotly({
    
    rateacre
    
  })
  
  # LEAFLET GRAPHS 
  
  output$hansoil<- renderLeaflet({
    hansoil
  })
  
  output$baseHan<- renderLeaflet({
    baseHan
  })
  
  output$zoneHan<- renderLeaflet({
    zoneHan
  })
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
