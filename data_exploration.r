install.packages("extrafont")
library(tidyverse)
library(ggthemes)
library(extrafont)
library(scales)

data <- read.csv("./DATA/etup_mensual_tr_cifra_1986_2023.csv")
identificador_estado <- read.csv("./DATA/tc_entidad.csv")

unique(data$VARIABLE)

#Exploración de dataset
data%>%
    filter(ID_ENTIDAD==9)%>%
    group_by(TRANSPORTE) %>%
    distinct(VARIABLE) %>%
    View()
#
transporte_cdmx <- data %>%
    filter(ID_ENTIDAD==9,VARIABLE=="Pasajeros transportados") %>%
    unite(ANIOMES,c(ANIO,ID_MES)) %>%
    mutate(FECHA=ym(ANIOMES),ANIO=year(FECHA)) %>%
    filter(ANIO>2023-15)
View(transporte_cdmx)

ggplot(data=transporte_cdmx, mapping=aes(x=FECHA,y=VALOR,color=TRANSPORTE))+
    geom_line(size=1.2)+
    labs(title="Uso de medios de transporte en la CDMX",
    caption="Fuente: INEGI, Transporte Urbano de Pasajeros 2023",
    y="Pasajeros transportados",color="") +
    theme_fivethirtyeight() +
    theme(axis.title.y=element_text(size=20),plot.title = element_text(size=30),legend.text = element_text(size=15)) +
    scale_color_brewer(palette="Dark2") +
    scale_y_continuous(breaks=seq(0,200000000,by=20000000),labels=comma) +
    scale_x_date(breaks = "2 years",labels =date_format("20%y"))
#Used extrafont to import fonts to R to be able to change it, font_import()
#fonts()-font_import()-loadfonts(device = "win")-names(wf[wf=="Forte"])
#unable to change

dataCDMX <- data %>%
    filter(ID_ENTIDAD==9) %>%
    unite(ANIOMES,c(ANIO,ID_MES)) %>%
    mutate(FECHA=ym(ANIOMES),ANIO=year(FECHA))  %>%
    select(TRANSPORTE,VARIABLE,VALOR,FECHA,ANIO)

dataCDMX %>%
    filter(VARIABLE=="Pasajeros transportados",ANIO>2023-10)%>%
    ggplot(mapping=aes(x=FECHA,y=VALOR,color=TRANSPORTE))+
    geom_line(size=1.2)+
    labs(title="Uso de medios de transporte en la CDMX",
    caption="Fuente: INEGI, Transporte Urbano de Pasajeros 2023",
    y="Pasajeros transportados",color="") +
    theme_fivethirtyeight() +
    theme(axis.title.y=element_text(size=20),plot.title = element_text(size=30),legend.text = element_text(size=15)) +
    scale_color_brewer(palette="Dark2") +
    scale_y_continuous(breaks=seq(0,200000000,by=20000000),labels=comma) +
    scale_x_date(breaks = "2 years",labels =date_format("20%y"))
##Number of trains available in Metro and Metrobus throughout 10 years 
dataCDMX %>%
    filter(VARIABLE=="Trenes en servicio"|VARIABLE=="Unidades en existencia",ANIO>2023-10)%>%
    ggplot(mapping=aes(x=ANIO,y=VALOR,color=TRANSPORTE)) +
    geom_line(size=1.5) +
    labs(title="Disponibilidad de transportes",
    caption="Fuente: INEGI, Transporte Urbano de Pasajeros 2023",
    y="Unidades en circulación")

##Incremento en rutas Metrobus?
dataCDMX %>%
    filter(VARIABLE=="Rutas",TRANSPORTE=="Metrobús",ANIO>2023-10)%>%
    ggplot(mapping=aes(x=FECHA,y=VALOR))+
    geom_line(size=1.5) +
    geom_smooth() +
    labs(title="",
    caption="Fuente: INEGI, Transporte Urbano de Pasajeros 2023",
    y="Rutas")
##KM travelled for Metro and Metrobus
dataCDMX %>% 
    filter(VARIABLE=="Pasajeros transportados",ANIO>2023-10) %>%
    group_by(TRANSPORTE,ANIO) %>%
    summarize(TOTAL_PASAJEROS=sum(VALOR)) %>%
    ggplot(mapping =aes(x=ANIO,y=TOTAL_PASAJEROS, fill=reorder(TRANSPORTE,TOTAL_PASAJEROS,sum))) +
    geom_col(position="stack")
