library(RColorBrewer)
library(data.table)
library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)

fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[1]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=15, vjust=1.25)) +
    theme(axis.text.x=element_text(size=10,color=color.axis.text, angle=0, hjust=0.4)) +
    theme(axis.text.y=element_text(size=10,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=15,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=15,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

palette <- brewer.pal("Greys", n=9)
redon <- function(x) as.character(round(x, 3))

# zonas de chile
norte_grande <- c(1, 2, 15)
norte_chico <- c(3, 4)
centro <- c(5, 6, 7, 13)
sur <- c(8, 9, 10, 14)
austral <- c(11, 12)

map_zona <- function(x) {
  if(x %in% norte_grande) {
    return("Norte Grande")
  } else if(x %in% norte_chico) {
    return("Norte Chico")
  } else if(x %in% centro) {
    return("Centro")  
  } else if(x %in% sur) {
    return("Zona Sur")
  } else if(x %in% austral) {
    return("Zona Austral") 
  } else {
    return("Extranjero")
  }
}

map_zonas <- function(x) factor(sapply(x, map_zona))

setwd('~/EL/datos/')

# todos los datos
raw <- fread('ELAs 20160616-00_30 - Raw.csv')

# por comuna
comunas <- fread('ELAs 20160616-00_30 - Comunas.csv')

# por region y poblacion
poblacion <- fread('ELAs 20160616-00_30 - Estaticos-Regiones.csv')
por_region <- read.csv('ELAs 20160616-00_30 - ELAs-por-Region.csv', sep = ",")
por_region <- data.table(por_region)

por_region <- por_region[, .(numero, ELAsPorRegion)]
poblacion <- poblacion[, .(numero, Nombre, Poblacion)]
  
regionales <- merge(por_region, poblacion, by = "numero")
regionales <- na.omit(regionales)

# por fecha y ubicacion
por_region_fecha <- fread('ELAs 20160616-00_30 - ELAs-Regiones-Dias.csv')
por_region_fecha[, Region:=NULL]
por_region_fecha[, CHECK:=NULL]

por_region_fecha <- por_region_fecha %>%
  gather(Fecha, ELAs, -1) %>%
  mutate(Fecha = as.Date(Fecha, "%Y/%m/%d")) %>%
  left_join(poblacion, "numero") %>%
  mutate(zona = map_zonas(numero),
         numero = factor(numero))

por_region_fecha <- data.table(por_region_fecha)


##### Graficos

# ELAs por cada 1000 habitantes
ggplot(regionales, aes(x=reorder(Nombre, ELAsPorRegion/Poblacion), 
                       y=ELAsPorRegion/Poblacion * 1000, 
                       fill="red",
                       label=redon(ELAsPorRegion/Poblacion * 1000))) + 
  geom_bar(stat="identity") +
  geom_text(size=2.5, hjust=-.1, color=palette[6]) +
  coord_flip() + 
  ylab("ELAs") +
  xlab("Región") +
  ggtitle("ELAs por cada 1000 habitantes") +
  scale_y_continuous(labels=comma) +
  fte_theme()

# Poblacion por Region
ggplot(regionales, aes(x=reorder(Nombre, Poblacion), 
                       y=Poblacion, 
                       fill="blue",
                       label=comma(Poblacion))) + 
  geom_bar(stat="identity") +
  geom_text(size=2.5, hjust=-.1, color=palette[6]) +
  coord_flip() + 
  ylab("Población") +
  xlab("Región") +
  ggtitle("Población por Región") +
  scale_y_continuous(labels=comma) +
  fte_theme()


# Densidad de ELAs por fecha
raw$Date <- as.Date(raw$Fecha, "%d-%m-%y")
raw_dates <- raw[Date > "2016-01-01" & Date < "2016-06-16"]
ggplot(raw_dates, aes(x=Date, fill="red", alpha=0.5)) + 
  geom_density() + 
  fte_theme() +
  scale_y_continuous(labels=scales::percent) +
  xlab("Fecha") +
  ylab("Porcentaje de ELAs") +
  ggtitle("Porcentaje de ELAs por fecha")

# Acumulado por fecha
ggplot(raw_dates, aes(x=Date)) + 
  stat_ecdf(geom="line") + 
  fte_theme() +
  scale_y_continuous(labels=scales::percent) +
  xlab("Fecha") +
  ylab("Porcentaje de ELAs") +
  ggtitle("Porcentaje acumulado de ELAs por fecha")


colors <- c("red", "blue", "orange", "green", "purple", "brown")

i <- 1
for(z in unique(por_region_fecha[, zona])) {
  p <- ggplot(por_region_fecha[zona == z], aes(x=Fecha, y=ELAs, fill=colors[i])) +
    geom_bar(stat="identity") +
    scale_y_continuous(limits=c(0, 1300)) +
    fte_theme() +
    ggtitle(z)  
  print(p)
  i <- i + 1
}
