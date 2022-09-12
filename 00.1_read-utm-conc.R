# Read Concentration Factor data
# ------------------------------------------------------
data_dir <- "data"

data_file_conc <- file.path(data_dir, "UTM_concentration_2022.csv")

# get UTM concentrations
# dat_conc <- read.csv(data_file_conc) %>% 
#   #rename(wshed = watershed) %>% 
#   add_row(site = c("Fay Bay", "Upper Exeter", "Pigeon Stream R1", "Pigeon Stream R7"), 
#           concent = 0, wshed = "pigeon", easting = 517135, northing =   7182238) %>%
#   add_row(site = c("S5", "S6"), concent = 23, wshed = "koala") %>%
#   mutate(wshed = ifelse(str_detect(site, "LLCF|KPSF"), "source", as.character(wshed))) %>%
#   filter(!is.na(concent)) %>%
#   arrange(concent) 
# 
# 
# write.csv(dat_conc, file.path(data_dir, "UTM_concentration_imputed_use.csv"), row.names = FALSE)


dat_conc <- read.csv(data_file_conc, stringsAsFactors = FALSE) %>%
  #rename(wshed = watershed) %>%
  mutate(wshed = ifelse(str_detect(site, "LLCF|KPSF"), "source", as.character(wshed))) %>%
  filter(!is.na(concent)) %>%
  arrange(concent)

dat_conc %>%
  arrange(concent) %>% 
  pull(site) -> site_order



# Remove Variables
# - not included in project regulatory filings
# -----------------------------------------------------

param_remove <- c("Bismuth", "Cesium", "Lithium",
                  "Rubidium", "Sulfur", "Tellurium",
                  "Thallium", "Thorium", "Tin", "Titanium",
                  "Tungsten", "Zirconium")




# Set colors
# ------------------------------------------------------


# REFERENCE LAKES/STREAMS
Colours<-colours()
# Reference Lakes
NanuqColour<-Colours[261]
CountsColour<-Colours[303]
VultureColour<-Colours[336]                                                     
# Reference Streams
NanuqOutflowColour<-Colours[261]
CountsOutflowColour<-Colours[303]
VultureOutflowColour<-Colours[336]  
# KOALA LAKES/STREAMS
KoalaHEAT<-fBasics::timPalette(n = 9)
# LLCF
LLCFColour<- '#423434'#KoalaHEAT[9]
# Koala Lakes
GrizzlyColour<-Colours[116]
KodiakColour<-Colours[464]
LeslieColour<-KoalaHEAT[8]
MooseColour<-KoalaHEAT[7]
NemaColour<-KoalaHEAT[6]
SlipperColour<-KoalaHEAT[5]
S2Colour<-KoalaHEAT[4]
S3Colour<-KoalaHEAT[3]
S5Colour<- KoalaHEAT[2]
S6Colour<- KoalaHEAT[1]
# Koala Streams
LowerPDCColour<-Colours[116]
KodiakLittleColour<-Colours[464]
LeslieMooseColour<-KoalaHEAT[6]
MooseNeroColour<-KoalaHEAT[5]
NemaMartineColour<-KoalaHEAT[4]
SlipperLacdeGrasColour<-KoalaHEAT[3]
# KING-CUJO LAKES/STREAMS
KingCujoHEAT<-fBasics::timPalette(n = 5)
# KPSF
KPSFColour<- KingCujoHEAT[5]
# King-Cujo Lakes
CujoColour<-"darkgreen" #KingCujoHEAT[4]
LdS1Colour<-KingCujoHEAT[2]
LdS2Colour<-'cyan2'
# King-Cujo Streams
CujoOutflowColour<-KingCujoHEAT[4]
ChristineLacduSauvageColour<-KingCujoHEAT[3]


#####Added by Thilini- Horseshoe colors

NortheastColor <- "grey55"


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


species_col = gg_color_hue(3)




ref_sites <- c('Vulture', 'Vulture-Polar',  'Nanuq', 
               'Nanuq Outflow', 'Counts',
               'Counts Outflow',
               "Pigeon Stream R7")

ref_body <- c(rep(c('lake', 'stream'), 3), 'stream')
koala_body <- c('lake', rep(c('lake', 'stream'), 4), 
                rep('lake', 4), 'stream', 'lake', 'stream')
kingcujo_body <- c('lake', 'lake', 'stream', 'stream', 'lake', 'lake')
pigeon_body <- c('lake', 'lake','stream')


koala_sites <- c('1616-30 (LLCF)','Leslie','Leslie-Moose', 'Moose',
                 'Moose-Nero','Nema','Nema-Martine', 'Slipper', 
                 'Slipper-Lac de Gras',
                 'S2','S3', 'S5', 'S6','Grizzly',
                 'Lower PDC','Kodiak','Kodiak-Little')

kingcujo_sites <- c('1616-43 (KPSF)','Cujo','Cujo Outflow', 
                    'Christine-Lac du Sauvage', 'LdS2', 'LdS1' )

pigeon_sites <- c("Fay Bay", "Upper Exeter", "Pigeon Stream R1")



site_colours_dat <- dplyr::data_frame(site = c(ref_sites, 
                    koala_sites,
                    kingcujo_sites,
                    pigeon_sites),
           use_colour = c(
             VultureColour,
             VultureOutflowColour,
             NanuqColour,
             NanuqOutflowColour,
             CountsColour,
             CountsOutflowColour,
             "grey85",
             LLCFColour, 
             LeslieColour, LeslieMooseColour, MooseColour, 
             MooseNeroColour, NemaColour, NemaMartineColour, SlipperColour, 
             SlipperLacdeGrasColour, 
             S2Colour, S3Colour, S5Colour, S6Colour,
             GrizzlyColour, LowerPDCColour,  KodiakColour,
             KodiakLittleColour,
             KPSFColour,
             CujoColour,
             CujoOutflowColour,
             ChristineLacduSauvageColour,
             LdS2Colour,
             LdS1Colour,
             "#FF7C00", "#008BFF", "#800000"))

site_colours_vec <- site_colours_dat$use_colour

names(site_colours_vec) <- site_colours_dat$site


wshed_col <- gg_color_hue(4) %>% tail(-1)

wshed_colours_vec <- c(wshed_col, CountsColour, CountsColour)

names(wshed_colours_vec) <- c("kingcujo", "koala", "pigeon", "reference")







