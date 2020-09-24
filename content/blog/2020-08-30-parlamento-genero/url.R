
library(dplyr)
library(xml2)
library(rvest)
library(purrr)
library(speech)
library(pdftools)


##Diputados-------

##hay 8 paginas (0 al 7)
ruta= "https://parlamento.gub.uy/documentosyleyes/documentos/diarios-de-sesion?Cpo_Codigo_2=D&Lgl_Nro=48&DS_Fecha%5Bmin%5D%5Bdate%5D=15-02-2015&DS_Fecha%5Bmax%5D%5Bdate%5D=14-02-2020&Ssn_Nro=&TS_Diario=&tipoBusqueda=T&Texto="
paginas=as.character(c(0:7)) ##cantidad de paginas
url = vector("list", 0)


##para cámara de representantes (diputados), 
##según número de páginas, obtengo los links en lista y luego en vector con n cantidad de páginas

url <- map(paginas,~ paste0(ruta, "&page=", .))%>% 
       unlist() %>% 
       map(~ .x  %>%
       read_html() %>%  
       html_nodes(".views-field-DS-File-IMG a") %>%   
       html_attr("href") %>%
       map(~ paste0("https://parlamento.gub.uy", .)))%>%
       unlist()

url=url[1]

discursos_diputados=map(url,possibly(speech_build,otherwise = NULL))


discursos_diputados_df = discursos_diputados %>% 
  map_df(as_tibble)


#save(discursos_diputados_df,file="C:/Users/Elina/Desktop/Parlamento/Parlamento_genero/discursos_diputados.RData")


 ##308 diarios de sesiones
##Senadores----- ##260 diarios de sesiones

##hay 8 paginas (0 al 7)
ruta= "https://parlamento.gub.uy/documentosyleyes/documentos/diarios-de-sesion?Cpo_Codigo_2=S&Lgl_Nro=48&DS_Fecha%5Bmin%5D%5Bdate%5D=15-02-2015&DS_Fecha%5Bmax%5D%5Bdate%5D=14-02-2020&Ssn_Nro=&TS_Diario=&tipoBusqueda=T&Texto="
paginas=as.character(c(0:7)) ##cantidad de paginas
url = vector("list", 0)


##para cámara de senadores, 
##según número de páginas, obtengo los links en lista y luego en vector con n cantidad de páginas

url <- map(paginas,~ paste0(ruta, "&page=", .))%>% 
  unlist() %>% 
  map(~ .x  %>%
        read_html() %>%  
        html_nodes(".views-field-DS-File-IMG a") %>%   
        html_attr("href") %>%
        map(~ paste0("https://parlamento.gub.uy", .)))%>%
  unlist()


discursos_senadores=map(url,possibly(speech_build,otherwise = NULL))


discursos_senadores_df = discursos_senadores %>% 
  map_df(as_tibble)


#save(discursos_senadores_df,file="C:/Users/Elina/Desktop/Parlamento/Parlamento_genero/discursos_senadores.RData")
#write.csv(discursos_senadores_df,"C:/Users/Elina/Desktop/Parlamento/Parlamento_genero/discursos_senadores.csv")


##CEG----

##Extraigo todo los discursos de la Comisión de Equidad y género (28 documentos) 
##(leg. 2015-2020) para entrenar al modelo sobre discursos que tratan la temática de género


##comisión



ceg <- list.files(path = "C:/Users/Elina/Desktop/Parlamento/Parlamento_genero/Bases/CEG/cegdoc1.pdf",pattern = "*.pdf") %>% speech_build()

ceg1 <- speech_build("C:/Users/Elina/Desktop/Parlamento/Parlamento_genero/Bases/CEG/cegdoc1.pdf")


ceg=rbind(ceg1,ceg2,ceg3,ceg4,ceg5,ceg6,ceg7,ceg8,ceg9,ceg10,ceg11,ceg12,ceg13,ceg14,ceg15,
          ceg16,ceg17,ceg18,ceg19,ceg20,ceg21,ceg22,ceg23,ceg24,ceg25,ceg26,ceg27,ceg28)


# write.csv(ceg,"C:/Users/Elina/Desktop/Parlamento/Parlamento_genero/CEG.csv")
# save(ceg,file="C:/Users/Elina/Desktop/Parlamento/Parlamento_genero/ceg.RData")




