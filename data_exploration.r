library(tidyverse)

data <- read.csv("./DATA/etup_mensual_tr_cifra_1986_2023.csv")

transporte_cdmx <- data %>%
    filter(ID_ENTIDAD==9,VARIABLE=="Pasajeros transportados") %>%
    unite(ANIOMES,c(ANIO,ID_MES)) %>%
    mutate(FECHA=ym(ANIOMES),ANIO=year(FECHA)) %>%
    filter(ANIO>2023-25)


View(transporte_cdmx)

ggplot(data=transporte_cdmx, mapping=aes(x=FECHA,y=VALOR,color=TRANSPORTE))+geom_line()
