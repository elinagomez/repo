


library(ggplot2) #visualización
library(dplyr) #manipulación de datos
library(grid) #estructura de cuadrantes de infografía
library(gridExtra) #estructura de cuadrantes de infografía
library(RColorBrewer) #paleta de colores
library(extrafont) #tipos de letras
library(sf) #cargar shape de mapas
library(png) #abrir iconos 
library(classInt) #calcular intervalos
library(useful)


nombre ="Inforgrafia"

serie_linea=read.csv("C:/Users/Elina/Documents/repo/content/blog/2020-04-25-inforgrafias/Inforgrafia/serie_linea.csv",sep = ";")

serie_linea_graf = ggplot(serie_linea,aes(x = as.factor(fecha_dato) , y = conteo,group = 1)) + 
  geom_line(size=2, stat="identity",colour = "#E13D3D") +
  theme(axis.text.x = element_text(size=25,family = "Impact"), axis.text.y = element_text(size=25,family = "Impact"),plot.title = element_text(size=30,family = "Impact"),axis.title.x=element_blank(),axis.title.y=element_blank())+
  geom_text(aes(label = round(conteo, 1)),
            vjust = "inward", hjust = "inward",
            show.legend = FALSE,size=10,family="Impact")+
  ylab("Casos") + xlab("Año") + ggtitle("Gráfico de línea")

serie_barra=read.csv("C:/Users/Elina/Documents/repo/content/blog/2020-04-25-inforgrafias/Inforgrafia/serie_barra.csv",sep=";")

serie_barra_graf = ggplot(serie_barra,aes(x = factor(fecha_dato) , y = conteo,group = 1)) + 
  geom_bar(size=2, stat="identity", colour = "#E13D3D",fill = "#E13D3D",width = 0.6) +
  scale_y_continuous(limits = c(0, 100))+ 
  theme(axis.text.x = element_text(size=25,family = "Impact"), axis.text.y = element_blank(),plot.title = element_text(size=30,family = "Impact"),axis.title.x=element_blank(),axis.title.y=element_blank())+
  geom_text(aes(label = paste0(round(conteo, 1),"%")),
            position = position_dodge(0.9) ,
            vjust = 0,show.legend = FALSE,size=10,family="Impact")+
  ylab("%") + xlab("Año") + ggtitle("Gráfico de barras")
serie_barra_graf

depto=read.csv("C:/Users/Elina/Documents/repo/content/blog/2020-04-25-inforgrafias/Inforgrafia/depto.csv",sep=";")

uru = st_read("C:/Users/Elina/Documents/repo/content/blog/2020-04-25-inforgrafias/Inforgrafia/ine_depto.shp", crs = 32721, stringsAsFactors = F,quiet = TRUE)
coord = st_read("C:/Users/Elina/Documents/repo/content/blog/2020-04-25-inforgrafias/Inforgrafia/dptos_centroides.shp", crs = 32721,quiet = TRUE)
uru = merge(uru, depto,  by.x = "NOMBRE",by.y = "coddepartamento_in")
coord = merge(coord, depto,  by.x = "NOMBRE",by.y = "coddepartamento_in")
brks=classIntervals(uru$conteo, n=5, style="quantile")
brks<- brks$brks

mapa=ggplot() + 
  geom_sf(aes(fill = conteo), data = uru) +
  scale_y_continuous(breaks = brks)+
  scale_fill_continuous(high = "#e13d3d", low = "#fcebeb")+
  geom_text(data = subset(coord, coord$conteo !=0), aes(XCOORD, YCOORD -3000 , label = conteo),size = 10,family="Impact") +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 25,family="Impact"),
    legend.title = element_text(size = 25,family="Impact"),
    plot.title = element_text(size=30,family="Impact"),
    panel.border = element_rect(colour = "#ffffff", fill = NA, size = 0.5))+
  guides(fill=guide_legend(title="Casos"))+
  ggtitle("Mapa de densidad")


piramide=read.csv2("C:/Users/Elina/Documents/repo/content/blog/2020-04-25-inforgrafias/Inforgrafia/piramide_edad.csv")

piramide$Var1 <- as.character(piramide$Var1)
piramide$Var1 <- factor(piramide$Var1, levels=c("0 a 4","5 a 9","10 a 14","15 a 19","20 a 24","25 a 29","30 a 34","35 a 39","40 a 44","45 a 49","50 a 54","55 a 59","60 a 64","65 a 69","70 a 74","75 a 79","80 a 84","85 a 89","90 y +"))

piramide$Freq <- ifelse(piramide$Var2 == "Varón", -1*piramide$Freq, piramide$Freq)


piramide_graf=ggplot(piramide, aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(data = subset(piramide, Var2 == "Mujer"), stat = "identity") +
  geom_bar(data = subset(piramide, Var2 == "Varón"), stat = "identity") + 
  scale_fill_manual(values = c("#f09e9e","#e76363"))+
  coord_flip()+
  theme(axis.text.x = element_text(size=25,family = "Impact"), axis.text.y = element_text(size=25,family = "Impact"),axis.title.x = element_blank(),axis.title.y = element_blank(),legend.text = element_text(
    size = 25,family = "Impact"),legend.title = element_text(
      size = 25,family = "Impact"),plot.title = element_text(size=30,family = "Impact"))+
  guides(fill=guide_legend(title="Sexo",reverse = T))+
  ggtitle("Pirámide poblacional")


datos=read.csv2(paste0("C:/Users/Elina/Documents/repo/content/blog/2020-04-25-inforgrafias/Inforgrafia/datos.csv"))

#cargo íconos  
icon1=readPNG("C:/Users/Elina/Documents/repo/content/blog/2020-04-25-inforgrafias/Inforgrafia/icon1.png")
icon2=readPNG("C:/Users/Elina/Documents/repo/content/blog/2020-04-25-inforgrafias/Inforgrafia/icon2.png")
icon3=readPNG("C:/Users/Elina/Documents/repo/content/blog/2020-04-25-inforgrafias/Inforgrafia/icon3.png")
icon4=readPNG("C:/Users/Elina/Documents/repo/content/blog/2020-04-25-inforgrafias/Inforgrafia/icon4.png")

#cargo logo 
logo=readPNG("C:/Users/Elina/Documents/repo/content/blog/2020-04-25-inforgrafias/Inforgrafia/rlogo.png")


pdf(paste0("C:/Users/Elina/Desktop/",nombre,".pdf"), width = 40, height = 22)

grid.newpage() 
pushViewport(viewport(layout = grid.layout(12,12)))

grid.rect(gp = gpar(fill = "#E13D3D", col = "#E13D3D"), x = unit(0.5, "npc"), y = unit(0.82, "npc"), width = unit(1, "npc"), height = unit(1.2, "npc"),vp = vplayout(1,1:11))

grid.text("INFOGRAFÍA EN R",gp = gpar(fontsize = 45, fontface ="bold",fontfamily = "Impact", col = "#000000"),just = c("left"),vp = vplayout(1,1)) 

grid.raster(logo,width = unit(0.6, "npc"),height=unit(0.6, "npc"),vp = vplayout(1,12))

print(serie_linea_graf, vp = vplayout(2:6, 1:6))
print(mapa, vp = vplayout(8:12, 1:3))
print(piramide_graf, vp = vplayout(8:12,4:6))
print(serie_barra_graf, vp = vplayout(8:12,7:12))

grid.text("Iconos con datos",
          gp = gpar(fontsize = 30, fontfamily = "Impact"),just = c("left"),vp = vplayout(2,8))

grid.raster(icon1,width = unit(0.3, "npc"),height=unit(0.5, "npc"),vp = vplayout(3,8))

grid.text("Dato 1",
          gp = gpar(fontsize = 30, fontfamily = "Impact"),just = c("left"),vp = vplayout(3,9))

grid.text(datos[1,2],gp = gpar(fontsize = 32, fontface = "bold",fontfamily = "Impact"),just = c("right"),vp = vplayout(3,11))   

grid.raster(icon2,width = unit(0.3, "npc"),height=unit(0.5, "npc"),vp = vplayout(4,8))
grid.text("Dato 2",
          gp = gpar(fontsize = 30, fontfamily = "Impact"),just = c("left"),vp = vplayout(4,9))

grid.text(datos[2,2],gp = gpar(fontsize = 32, fontface = "bold",fontfamily = "Impact"),just = c("right"),vp = vplayout(4,11))   

grid.raster(icon3,width = unit(0.3, "npc"),height=unit(0.5, "npc"),vp = vplayout(5,8))

grid.text("Dato 3",
          gp = gpar(fontsize = 30, fontfamily = "Impact"),just = c("left"),vp = vplayout(5,9))
grid.text(datos[3,2],gp = gpar(fontsize = 32, fontface = "bold",fontfamily = "Impact"),just = c("right"),vp = vplayout(5,11))   

grid.raster(icon4,width = unit(0.3, "npc"),height=unit(0.5, "npc"),vp = vplayout(6,8))

grid.text("Dato 4",gp = gpar(fontsize = 30, fontfamily = "Impact"),just = c("left"),vp = vplayout(6,9)) 
grid.text(datos[4,2],gp = gpar(fontsize = 32, fontface = "bold",fontfamily = "Impact"),just = c("right"),vp = vplayout(6,11))   

dev.off()
