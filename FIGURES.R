## Script for Figure 1


# functions--------------------------------------------------
library(dplyr)
# load packages for maps
library(maptools)
library(ggmap)
library(rnaturalearth)
library(countrycode)
library(viridis)
library(ggplot2)
library(knitr)
library(patchwork)
library(reshape2)


# data--------------------------------------------------
metadatreview <- read.csv("Data/References_pollution_review.csv", h = TRUE)

stud <- metadatreview %>% 
  group_by(Country) %>% 
  summarize(no.stu = n())

multicountrystud <- stud[grepl(";|,", stud$Country),]
multicountrystud

# Manually add multiple countries studies:
addingcountries <- data.frame(rbind(c("Brazil", 1), c("Portugal", 1), 
                                    c("Canada", 1), c("france", 1), c("The Netherlands", 1), c("Switzerland", 1),
                                    c("Germany", 1), c("The Netherlands", 1), c("UK", 1), c("Portugal", 1),
                                    c("Indonesia", 1),c("Malaysia", 1),
                                    c("UK", 1),
                                    c("The Netherlands", 1), c("Belgium", 1), c("france", 1), c("germany", 1)))
colnames(addingcountries) <- colnames(stud)
addingcountries$no.stu <- as.numeric(addingcountries$no.stu)

stud <- rbind(as.data.frame(stud), addingcountries)

# harmonize country names
stud <- stud %>%
  mutate(iso =  countrycode(stud$Country, origin = "country.name", destination = "iso3c")) %>%
  dplyr::group_by(iso) %>%
  summarize(no.stud = sum(no.stu, na.rm = TRUE))


wm <- ne_countries(scale = 110, returnclass = "sf") %>%
  left_join(stud, by = c("adm0_a3" = "iso"))

wm <- sf::st_transform(wm,"+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

mybreaks <- c(5, 10, 15, 20, 25)

map <- ggplot(wm)+
  geom_sf(aes(fill = (no.stud)))+
  scale_fill_viridis(option = "viridis", 
                     begin = 0,
                     end = 1,
                     na.value = "gray", breaks = mybreaks,
                     name = "Number of\nstudies")+
  theme_bw()+
  theme(legend.position = c(0,0),
        # legend.position = "left",
        legend.justification = c(-0.2, -0.1),
        legend.title = element_text(size=15),
        legend.text = element_text(size=14))

map

# save fig high resolution--------------------------------------------------

# save a png with high res
ppi <- 300# final: 600 # resolution
w <- 20 # width in cm

png("figs/Fig2A_map.png",
    width=w,
    height=w,
    units = "cm",
    res=ppi)

map + labs(tag = "A")

dev.off()



## PANEL B - Time trends and pollutant types------------------

## FIG.2B - Pollutant types -----------------------------------------

# create new pol type where = mixture when more than one category of pollutant is involved (for plot purposes)
metadatreview$PollutantType2 <- 
  as.factor(ifelse(grepl(",", metadatreview$PollutantType), "Mixtures", 
                   ifelse(grepl("PAH", metadatreview$PollutantType), "PAHs (hydrocarbons)",
                          as.character(metadatreview$PollutantType))))
summary(metadatreview$PollutantType2)

## Sorting and resetting the levels of pollutant type
xLev = names(table(metadatreview$PollutantType2))[order(table(metadatreview$PollutantType2), decreasing = TRUE)]
metadatreview$PollutantType2 = factor(metadatreview$PollutantType2, levels=xLev)

# colors and esthetics
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# set sizes of text in plots
sizetext <- 18
sizelegend <- 15


Fig2B <- ggplot(
  
  metadatreview %>%  # calculate cumulated sums per pollutant type and across years
    group_by(PollutantType2, Published.Year) %>%
    summarise(no.studies1 = n()) %>% 
    mutate(no.studies = cumsum(no.studies1)),
  
  aes(x = Published.Year, y = no.studies, color = PollutantType2))+ # plot no. studies cum per year per poll type
  
  # plot lines with colors per type pollutant
  geom_line(size = 1.5)+ #
  scale_color_manual(values = cbPalette)+
  scale_x_continuous(name = "Publication year")+
  scale_y_continuous(name = "Number of studies")+
  
  # theme stuff
  theme_bw() +
  theme(#axis.text.y=element_text(face = "bold", size = sizetext), 
    axis.text.x=element_text(size = sizetext),
    axis.text.y=element_text(size = sizetext),
    axis.title.x = element_text(size=sizetext, face = "bold"),
    axis.title.y = element_text(size=sizetext, face = "bold"),
    
    #legend
    legend.title = element_blank() ,
    legend.text = element_text(size=sizelegend), 
    legend.position = "right",
    legend.spacing.x = unit(0.25, 'cm'),
    
  )
Fig2B



# save fig high resolution--------------------------------------------------

# save a png with high res
ppi <- 300# final: 600 # resolution
w <- 20 # width in cm

png("figs/Fig2B_Pollutants.png",
    width=w,
    height=w/2,
    units = "cm",
    res=ppi)

Fig2B + labs(tag = "B")

dev.off()


## Create data for alluvial plot (Fig 2C)---------

# split multiple taxa and pollutants into separate rows with strsplit
new_df_combi_taxalist <- strsplit(as.character(metadatreview$TaxaGroupAtlas), ',')
new_df_combi_taxa <- data.frame(
  TaxaGroupAtlas = unlist(new_df_combi_taxalist), 
  PollutantType = rep(metadatreview$PollutantType2, sapply(new_df_combi_taxalist, FUN=length)))
head(new_df_combi_taxa)


new_df_combi_taxa_pollist <- strsplit(as.character(new_df_combi_taxa$PollutantType), ',')
new_df_combi_taxa_pol <- data.frame(
  TaxaGroupAtlas = rep(new_df_combi_taxa$TaxaGroupAtlas, sapply(new_df_combi_taxa_pollist, FUN=length)), 
  PollutantType = unlist(new_df_combi_taxa_pollist))
head(new_df_combi_taxa_pol)

# Final dataset for the figure
df_alluvial <- new_df_combi_taxa_pol %>% 
  group_by(TaxaGroupAtlas, PollutantType) %>% 
  summarise(freq=n())
write.csv(df_alluvial, "Data/df_alluvial_forHelen.csv")



## FIGURE 3 - Ecosystem perspective ----------------------------------------

# data--------------------------------------------------
metadatreview <- metadatreview %>% 
  mutate(OtherBiodiversity = factor(ifelse(OtherBiodiversity=="lichens", "plants", # lichens in the plant category for simplification
                                           as.character(OtherBiodiversity))))

## Panel 1 - Multiple Biodiversity=====================================
metadatreview <- metadatreview %>% 
  mutate(OtherBiodiversity = factor(ifelse(OtherBiodiversity=="lichens", "plants", # lichens in the plant category for simplification
                                           as.character(OtherBiodiversity))))

# Calculate the percentage of each group
metadatreviewsummary <- metadatreview %>%
  group_by(OtherBiodiversity) %>%
  summarise(no.studies = n(), Percent = n()/nrow(.) * 100)

## Reorder levels of categorical factor by increasing frequency
# increasing freq in the overall dat
xLev = names(table(metadatreview$OtherBiodiversity))[
  order(table(metadatreview$OtherBiodiversity), decreasing = FALSE)]

# assign factor levels to this order
metadatreviewsummary$OtherBiodiversity = factor(metadatreviewsummary$OtherBiodiversity, levels=xLev)
# The Plot
metadatreviewsummary$MultiDiversity = metadatreviewsummary$OtherBiodiversity

# Rename levels for the plot
levels(metadatreviewsummary$MultiDiversity)
levels(metadatreviewsummary$MultiDiversity) <- c("plants, aboveground invertebrates",
                                                 "aboveground invertebrates",
                                                 "plants, microbes",
                                                 "plants",
                                                 "microbes", 
                                                 "no")

# set sizes of text in plots
sizetext <- 20
sizelegend <- 18

## Color palette matching the venn diagram
colMB1 <- c("#424b54", "#cc444b", "#aa4465", "#f5cac3", "#da5552",
            "#F68D92") # color from venn diagram

colMB <- c( "#424b54", "#cce8cc","#aa4465", "#b38d97","#ecc8ae",
            "#F68D92") # color from venn diagram

PlotMultiB <- ggplot(data = metadatreviewsummary, aes(x = 1, y = Percent, fill = MultiDiversity))+
  geom_bar(stat = "identity")+
  # scale_fill_viridis(discrete = TRUE, name="Multiple Biodiversity") +
  scale_fill_manual(values = colMB, name="Multiple Biodiversity") +
  scale_y_continuous(name="Proportion of studies (%)")+
  # theme stuff
  theme_bw() +
  theme(axis.text.y=element_text(size = sizelegend),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.y = element_text(size=sizetext, face = "bold"),
        #legend
        legend.title = element_text( size=sizetext, face = "bold"),
        legend.text = element_text(size=sizelegend), 
        legend.position = "right",
        legend.spacing.x = unit(0.5, 'cm'))

PlotMultiB

## Panel 2 - Multiple Functions =====================================
# Calculate the percentage of each group
metadatreviewsummary2 <- metadatreview %>%
  filter(!EcosystemFunction == "") %>% 
  group_by(EcosystemFunction) %>%
  summarise(no.studies = n(), Percent = n()/nrow(.) * 100)
metadatreviewsummary2

## Reorder levels of categorical factor by increasing frequency
# increasing freq in the overall dat
xLev = names(table(metadatreview$EcosystemFunction))[order(table(metadatreview$EcosystemFunction))]
# assign factor levels to this order
metadatreviewsummary2$EcosystemFunction = factor(metadatreviewsummary2$EcosystemFunction, levels=xLev)

## Color palette matching the venn diagram
colEF1 <- c( "#1B4965","#BEE9E8","#87BADD")
colEF2 <- c( "#1B4965","#d7d9b1","#87BADD")

# The Plot
PlotMultiEF <- ggplot(data = metadatreviewsummary2, aes(x = 1, y = Percent, fill = EcosystemFunction))+
  geom_bar(stat = "identity")+
  # scale_fill_viridis(discrete = TRUE, name="Multiple Ecosystem Functions") +
  scale_fill_manual(values=colEF1, name="Multiple Ecosystem Functions") +
  # scale_y_continuous(name="Proportion of studies (%)")+
  # theme stuff
  theme_bw() +
  theme(axis.text.y=element_text(size = sizelegend),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        # axis.title.y = element_text(size=sizetext, face = "bold"),
        axis.title.y = element_blank(),
        #legend
        legend.title = element_text( size=sizetext, face = "bold"),
        legend.text = element_text(size=sizelegend), 
        legend.position = "right",
        legend.spacing.x = unit(0.5, 'cm'))
# PlotMultiEF

## Panel 3 - Multiple Drivers =====================================
# create the multiple stressors var
metadatreview <- metadatreview %>% 
  mutate(Multistressors = factor(ifelse(grepl(",", PollutantType), "Multiple stressors", 
                                        "Single stressor")),
         Multidrivers = factor(ifelse(LandUse=="TRUE"|Intensification=="TRUE"|Fragmentation.Loss=="TRUE"|Nutrient=="TRUE"|ClimateChange=="TRUE"|Invasives=="TRUE", "yes", "no")))


# Calculate the percentage of each group
metadatreviewsummary3 <- metadatreview %>%
  group_by(Multidrivers) %>%
  summarise(no.studies = n(), Percent = n()/nrow(.) * 100)



## Reorder levels of categorical factor by increasing frequency
# increasing freq in the overall dat
xLev = names(table(metadatreview$Multidrivers))[order(table(metadatreview$Multidrivers))]
# assign factor levels to this order
metadatreviewsummary3$Multidrivers = factor(metadatreviewsummary3$Multidrivers, levels=xLev)


colMD <- c("#9e924a", 
           "#FEEE8A")# COlor from venn diag

# The Plot
PlotMultiGCD <- ggplot(data = metadatreviewsummary3, aes(x = 1, y = Percent, fill = Multidrivers))+
  geom_bar(stat = "identity")+
  # scale_fill_viridis(discrete = TRUE, name="Multiple Drivers") +
  scale_fill_manual(values = colMD, name="Multiple Drivers") +
  # scale_y_continuous(name="Proportion of studies (%)")+
  # theme stuff
  theme_bw() +
  theme(axis.text.y=element_text(size = sizelegend),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        # axis.title.y = element_text(size=sizetext, face = "bold"),
        axis.title.y = element_blank(),
        #legend
        legend.title = element_text( size=sizetext, face = "bold"),
        legend.text = element_text(size=sizelegend), 
        legend.position = "right",
        legend.spacing.x = unit(0.5, 'cm'))


# PlotMultiGCD

PlotMultiB + PlotMultiEF + PlotMultiGCD


# save fig high resolution--------------------------------------------------

# save a png with high res
ppi <- 600# final: 600 # resolution
w <- 40 # width in cm

png("figs/Fig3_Scope.png",
    width=w,
    height=w/4,
    units = "cm",
    res=ppi)

PlotMultiB + PlotMultiEF + PlotMultiGCD +
  plot_annotation(tag_levels = "A")

dev.off()


## Dataset for Venn diagram: interactions between the three topics-------

WhichTopic <- c("Multi-diversity", "Multidiversity + EF", "Multi-diversity + Multiple drivers",
                "Ecosystem Functions", "Ecosystem Functions + Multiple drivers", 
                "Multiple drivers",
                "3 topics")

No.studies <- c("Multi-diversity (=plant-soil)" = nrow(metadatreview[metadatreview$OtherBiodiversity == "plants,microbes",]),
                "Multi-diversity + EF"= nrow(metadatreview[metadatreview$OtherBiodiversity == "plants,microbes" &
                                                             metadatreview$EcosystemFunction != "none",]),
                "Multi-diversity + Multi Drivers" = nrow(metadatreview[metadatreview$OtherBiodiversity == "plants,microbes" &
                                                                         metadatreview$Multidrivers != "no",]),
                "Ecosystem Functions" = nrow(metadatreview[metadatreview$EcosystemFunction != "none",]),
                "Ecosystem Functions + Multiple drivers" = nrow(metadatreview[metadatreview$EcosystemFunction != "none" &
                                                                                metadatreview$Multidrivers != "no",]),
                "Multiple drivers" = nrow(metadatreview[metadatreview$Multidrivers != "no",]),
                "Three topics" = nrow(metadatreview[metadatreview$Multidrivers != "no" &
                                                      metadatreview$OtherBiodiversity == "plants,microbes" &
                                                      metadatreview$EcosystemFunction != "none",]),
                "Three tropics if true multidiv" = nrow(metadatreview[metadatreview$Multidrivers != "no" &
                                                                        metadatreview$OtherBiodiversity != "no" &
                                                                        metadatreview$EcosystemFunction != "none",])
                
)

CitationsMultiStudies <- metadatreview[metadatreview$Multidrivers != "no" &
                                              metadatreview$OtherBiodiversity == "plants,microbes" &
                                              metadatreview$EcosystemFunction != "none",]

# MultiD studies
muliDivstudies <- metadatreview[metadatreview$OtherBiodiversity != "no",]
muliEFstudies <- metadatreview[metadatreview$EcosystemFunction != "none",]
muliGCDstudies <- metadatreview[metadatreview$Multidrivers != "no",]
