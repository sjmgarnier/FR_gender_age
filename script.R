#+ license, echo=FALSE
# 
# Copyright (C) 2014 Simon Garnier
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#+ libraries, echo=FALSE
require("data.table")
require("bit64")
require("dplyr")
require("rgdal") 
require("maptools")
require("ggplot2")
if (!require("graphZoo")) {
  require("devtools")
  install_github("morpionZ/graphZoo")
  require("graphZoo")
}
require("extrafont")
loadfonts()


#+ download.raw.data, echo=FALSE
if (!file.exists("data/BTT_TD_POP1B_2010.txt")) {
  download.file("http://www.insee.fr/fr/ppp/bases-de-donnees/donnees-detaillees/rp2010/tab-detailles/td-population-10/BTT_TD_POP1B_2010.zip", destfile = "data/BTT_TD_POP1B_2010.zip")
  unzip("data/BTT_TD_POP1B_2010.zip", exdir = "data")
  file.remove("data/BTT_TD_POP1B_2010.zip")
}

if (!file.exists("data/com_coords.csv")) {
  download.file("http://osm13.openstreetmap.fr/~cquest/openfla/export/communes-20140306-100m-shp.zip",
                destfile = "data/communes-20140306-100m-shp.zip")
  unzip("data/communes-20140306-100m-shp.zip", exdir = "data")
  file.remove("data/communes-20140306-100m-shp.zip")
  
  france <- readOGR(dsn = "data/", layer = "communes-20140306-100m")
  france@data$id <- rownames(france@data)
  france.points <- fortify(france, region="id")
  france.df <- as.data.table(left_join(france.points, france@data, by="id")) %>%
    select(-hole, -nom, -wikipedia)
  setnames(france.df, names(france.df), c("LONG", "LAT", "ORDER", "PIECE",
                                          "GROUP", "ID", "POSTAL.CODE", "SURF.M2"))
  write.csv(france.df, file = "data/com_coords.csv", row.names = FALSE)
}


#+ load.data, echo=FALSE
pop.fr <- fread("data/BTT_TD_POP1B_2010.txt", 
                colClasses = c("factor", "factor", "factor", "factor", 
                               "numeric", "factor", "character")) %>%
  filter(NIVEAU == "COM") %>%
  select(-NIVEAU)
setnames(pop.fr, names(pop.fr), c("POSTAL.CODE", "REG.CODE", "DEP.CODE",
                                  "AGE", "GENDER", "COUNT"))
pop.fr <- mutate(pop.fr, COUNT = sub(",", ".", COUNT)) %>%
  mutate(COUNT = as.numeric(COUNT))


#+ plot.gender.bal, eco=false
gender.bal.fr <- group_by(pop.fr, POSTAL.CODE, GENDER) %>%
  summarize(COUNT = sum(COUNT)) %>%
  group_by(POSTAL.CODE) %>%
  summarize(WOM.PROP = COUNT[GENDER == 2] / sum(COUNT))

towns <- fread("data/com_coords.csv") %>%
  filter(LAT > 40) %>%
  merge(gender.bal.fr, by = "POSTAL.CODE")

g <- ggplot(towns, 
            aes(LONG, LAT, group = POSTAL.CODE, fill = WOM.PROP)) + 
  geom_polygon(color = "black", size = .05) +
  scale_fill_gradient2(low = "blue4", mid = "white", high = "red", midpoint = 0.5, limits = c(0.4, 0.6),
                       guide = guide_colorbar(title = element_text(" <- plus d'hommes | plus de femmes ->", 
                                                                   hjust = 1),
                                              label.position = "bottom",
                                              title.position = "top",
                                              barwidth = 25)) +
  #coord_fixed() +
  theme_graphzoo(base_size = 22, family = "Open Sans") +
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(1, 0, 0, -1), "lines"),
        legend.position = "bottom",
        legend.title.align = 0.5) +
  ggtitle(bquote(atop("Ratio femmes/hommes par commune", 
                      atop(italic(.("France métropolitaine (2010)")), ""))))

g <- addBanner(g, font.size = 6,
               l.txt = "GRAPHZOO.TUMBLR.COM", 
               r.txt = "SOURCE: DATA.GOUV.FR, INSEE, OSM")

png("gender_fr.png", width = 800, height = 900, bg = "#F0F0F0")
print(g)
dev.off() 


#+ plot.age.bal, eco=false
age.bal.fr <- group_by(pop.fr, POSTAL.CODE, AGE) %>%
  summarize(COUNT = sum(COUNT)) %>%
  group_by(POSTAL.CODE) %>%
  summarize(MED.AGE = AGE[which(cumsum(COUNT) > (sum(COUNT) / 2))[1]],
            AVG.AGE = sum(AGE * COUNT) / sum(COUNT))

towns <- fread("data/com_coords.csv") %>%
  filter(LAT > 40) %>%
  merge(age.bal.fr, by = "POSTAL.CODE")

g <- ggplot(towns, 
            aes(LONG, LAT, group = POSTAL.CODE, fill = MED.AGE)) + 
  geom_polygon(color = "black", size = .05) +
  scale_fill_gradient2(low = "blue4", mid = "white", high = "red", midpoint = (max(towns$MED.AGE) + min(towns$MED.AGE)) / 2,
                       guide = guide_colorbar(title = element_text("<- plus jeune | plus vieux ->", 
                                                                   hjust = 1),
                                              label.position = "bottom",
                                              title.position = "top",
                                              barwidth = 25)) +
  #coord_fixed() +
  theme_graphzoo(base_size = 22, family = "Open Sans") +
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(1, 0, 0, -1), "lines"),
        legend.position = "bottom",
        legend.title.align = 0.5) +
  ggtitle(bquote(atop("Age median par commune", 
                      atop(italic(.("France métropolitaine (2010)")), ""))))
  
g <- addBanner(g, font.size = 6,
               l.txt = "GRAPHZOO.TUMBLR.COM", 
               r.txt = "SOURCE: DATA.GOUV.FR, INSEE, OSM")

png("median_age_fr.png", width = 800, height = 900, bg = "#F0F0F0")
print(g)
dev.off()   


