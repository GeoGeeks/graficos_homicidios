library(plotly)
library(ggplot2)
library(readxl)
library(tidyverse)
library(shiny)
library(DT)
library(flexdashboard)
library(reshape2)
library(htmlwidgets)

datos <- read_excel("data/IHME-GBD_DATA.xlsx")

names(datos)
muertesviolentas <- datos  %>% group_by_(.dots = list("measure_name",  "location_name", "year" )) %>%
  summarise(Rate_lower = sum(lower), Rate_val = sum(val), Rate_upper = sum(upper)) %>% select( "measure_name",  "location_name"   , "year", "Rate_val","Rate_lower"  , "Rate_upper") %>% arrange(desc(location_name))   
muertesviolentas


# write.xlsx(data.frame(muertesviolentas), file= "muertesviolentas.xlsx", sheetName = "Hoja1")

paises <- NULL
for (i in unique(muertesviolentas$location_name)) {
  pais <- c(i, t(muertesviolentas[muertesviolentas$location_name == i, 4]))
  paises <- as.tibble(rbind(paises, pais))
}

colnames(paises) <- c("País", t(muertesviolentas[muertesviolentas$location_name == i, 3]) )



media.nacional <- NULL
for (i in 1998:2017) {
  mediaaño <- mean(sapply(select (paises, contains (paste0(i))), as.numeric, 2), na.rm = T)
  media.nacional <- rbind(media.nacional, mediaaño) 
}



desv.nacional <- NULL
for(i in 1998:2017){
  sdaño <- sd(sapply(select (paises, contains (paste0(i))), as.numeric, 2), na.rm = T)
  desv.nacional <- rbind(desv.nacional, sdaño) 
  
}


años <- 1998:2017

datosnal <- data_frame(años, media.nacional, desv.nacional)
colnames(datosnal) <- c("year", "media.nacional", "sd.nacional")
if (1994 %in% datosnal$year){
  datosnal$sd.nacional[datosnal$year == 1994,] <- (datosnal$sd.nacional[datosnal$year == 1995,] + datosnal$sd.nacional[datosnal$year == 1996,])/2
  datosnal$media.nacional[datosnal$year == 1994,] <- (datosnal$media.nacional[datosnal$year == 1995,] + datosnal$media.nacional[datosnal$year == 1996,])/2
}
if (1999 %in% datosnal$year){
  datosnal$sd.nacional[datosnal$year == 1999,] <- (datosnal$sd.nacional[datosnal$year == 1998,] + datosnal$sd.nacional[datosnal$year == 2000,])/2
}
if (2011 %in% datosnal$year){
  datosnal$sd.nacional[datosnal$year == 2011,] <- (datosnal$sd.nacional[datosnal$year == 2010,] + datosnal$sd.nacional[datosnal$year == 2012,])/2
}



longitud <- length(seq(1998,2017, by = 1))



if(6 <= longitud ){
  mediaquinquenio1  <- apply(sapply(paises[,-1], as.numeric, 1)[,(1+(1998-1990)):((6-1)+(1998-1990))], 1, mean, na.rm=T)
}

if(6 <= longitud){
  media.municipal <- NULL
  for(i in ((6+1)+(1998-1990)):(longitud+(1998-1990))){
    mediaquinquenio <- apply(sapply(paises[,-1], as.numeric, 1)[,(i-(6-1)):i-1], 1, mean, na.rm=T)
    media.municipal <- cbind(media.municipal, mediaquinquenio)
  }
}else{
  media.municipal <- NULL
}


if(6 <= longitud){
  constante <- matrix(rep(mediaquinquenio1), ncol=6, nrow=195)
}else{
  constante <-NULL
}


if(6 <= longitud){
  media.movil <- cbind(constante,media.municipal)
  colnames(media.movil) <- años
  rownames(media.movil) <- paises$País
}


if(6 <= longitud){
  sdquinquenio1 <- apply(sapply(paises[,-1], as.numeric, 1)[,1:6-1], 1, sd, na.rm=T)
}else{
  sdquinquenio1 <- NULL
}



if(6 <= longitud){
  sd.municipal <- NULL
  for(i in ((6+1)+(1998-1990)):(longitud+(1998-1990))){
    sdquinquenio <- apply(sapply(paises[,-1], as.numeric, 1)[,(i-(6-1)):i-1], 1, sd, na.rm=T)
    sd.municipal <- cbind(sd.municipal, sdquinquenio)
  }
}else{
  sd.municipal <- NULL
}


if(6 <= longitud){
  constante2 <- matrix(rep(sdquinquenio1), ncol=6, nrow=195)
}else{
  constante2 <- NULL
}


if(6 <= longitud){
  sd.movil <- cbind(constante2,sd.municipal)
  rownames(sd.movil) <- paises$País
  colnames(sd.movil)  <- años
}





salvador <- data.frame(rep(paises[paises$País== "El Salvador",]$País,longitud),  x = datosnal$year, y=datosnal$media.nacional,
                       mun= as.numeric(t(paises[paises$País == "El Salvador", (2+ (1998-1990) ):(2+(1998-1990) + longitud-1)])),
                       upper3 = datosnal$media.nacional+3*datosnal$sd.nacional,
                       upper2 = datosnal$media.nacional+2*datosnal$sd.nacional,
                       upper = datosnal$media.nacional+datosnal$sd.nacional,
                       lower= datosnal$media.nacional-datosnal$sd.nacional,
                       movil= media.movil[rownames(media.movil) == "El Salvador",],
                       sdmovil =sd.movil[rownames(sd.movil) == "El Salvador",]
)

colnames( salvador) <- c("País", "Año", "Tasa", "Tasa_El_Salvador",  "más_3_desv.","más_2_desv.",   "más_1_desv.","menos_1_desv.", "Movil", "SDMovil")





salvador %>%
  mutate(tsa.cmbio= (Tasa_El_Salvador-lag(Tasa_El_Salvador, na.pad = TRUE))/(lag(Tasa_El_Salvador, na.pad = TRUE)), dfrncia=
           Tasa_El_Salvador-lag(Tasa_El_Salvador, na.pad = TRUE) , tsa.cmbio2 = ((-(-Tasa_El_Salvador + lead(Tasa_El_Salvador, na.pad = TRUE)))/Tasa_El_Salvador))


colombia <- data.frame(rep(paises[paises$País== "Colombia",]$País,longitud),  x = datosnal$year, y=datosnal$media.nacional,
                       mun= as.numeric(t(paises[paises$País == "Colombia", (2+ (1998-1990) ):(2+(1998-1990) + longitud-1)])),
                       upper3 = datosnal$media.nacional+3*datosnal$sd.nacional,
                       upper2 = datosnal$media.nacional+2*datosnal$sd.nacional,
                       upper = datosnal$media.nacional+datosnal$sd.nacional,
                       lower= datosnal$media.nacional-datosnal$sd.nacional,
                       movil= media.movil[rownames(media.movil) == "Colombia",],
                       sdmovil =sd.movil[rownames(sd.movil) == "Colombia",]
)

colnames( colombia) <- c("País", "Año", "Tasa", "Tasa_Colombia",  "más_3_desv.","más_2_desv.",   "más_1_desv.","menos_1_desv.", "Movil", "SDMovil")





colombia %>%
  mutate(tsa.cmbio= (Tasa_Colombia-lag(Tasa_Colombia, na.pad = TRUE))/(lag(Tasa_Colombia, na.pad = TRUE)), dfrncia=
           Tasa_Colombia-lag(Tasa_Colombia, na.pad = TRUE) , tsa.cmbio2 = ((-(-Tasa_Colombia + lead(Tasa_Colombia, na.pad = TRUE)))/Tasa_Colombia))


iraq <- data.frame(rep(paises[paises$País== "Iraq",]$País,longitud),  x = datosnal$year, y=datosnal$media.nacional,
                   mun= as.numeric(t(paises[paises$País == "Iraq", (2+ (1998-1990) ):(2+(1998-1990) + longitud-1)])),
                   upper3 = datosnal$media.nacional+3*datosnal$sd.nacional,
                   upper2 = datosnal$media.nacional+2*datosnal$sd.nacional,
                   upper = datosnal$media.nacional+datosnal$sd.nacional,
                   lower= datosnal$media.nacional-datosnal$sd.nacional,
                   movil= media.movil[rownames(media.movil) == "Iraq",],
                   sdmovil =sd.movil[rownames(sd.movil) == "Iraq",]
)

colnames( iraq) <- c("País", "Año", "Tasa", "Tasa_Iraq",  "más_3_desv.","más_2_desv.",   "más_1_desv.","menos_1_desv.", "Movil", "SDMovil")





iraq %>%
  mutate(tsa.cmbio= (Tasa_Iraq-lag(Tasa_Iraq, na.pad = TRUE))/(lag(Tasa_Iraq, na.pad = TRUE)), dfrncia=
           Tasa_Iraq-lag(Tasa_Iraq, na.pad = TRUE) , tsa.cmbio2 = ((-(-Tasa_Iraq + lead(Tasa_Iraq, na.pad = TRUE)))/Tasa_Iraq))


syria <- data.frame(rep(paises[paises$País== "Syria",]$País,longitud),  x = datosnal$year, y=datosnal$media.nacional,
                    mun= as.numeric(t(paises[paises$País == "Syria", (2+ (1998-1990) ):(2+(1998-1990) + longitud-1)])),
                    upper3 = datosnal$media.nacional+3*datosnal$sd.nacional,
                    upper2 = datosnal$media.nacional+2*datosnal$sd.nacional,
                    upper = datosnal$media.nacional+datosnal$sd.nacional,
                    lower= datosnal$media.nacional-datosnal$sd.nacional,
                    movil= media.movil[rownames(media.movil) == "Syria",],
                    sdmovil =sd.movil[rownames(sd.movil) == "Syria",]
)

colnames( syria) <- c("País", "Año", "Tasa", "Tasa_Syria",  "más_3_desv.","más_2_desv.",   "más_1_desv.","menos_1_desv.", "Movil", "SDMovil")





syria %>%
  mutate(tsa.cmbio= (Tasa_Syria-lag(Tasa_Syria, na.pad = TRUE))/(lag(Tasa_Syria, na.pad = TRUE)), dfrncia=
           Tasa_Syria-lag(Tasa_Syria, na.pad = TRUE) , tsa.cmbio2 = ((-(-Tasa_Syria + lead(Tasa_Syria, na.pad = TRUE)))/Tasa_Syria))


eritrea <- data.frame(rep(paises[paises$País== "Eritrea",]$País,longitud),  x = datosnal$year, y=datosnal$media.nacional,
                         mun= as.numeric(t(paises[paises$País == "Eritrea", (2+ (1998-1990) ):(2+(1998-1990) + longitud-1)])),
                         upper3 = datosnal$media.nacional+3*datosnal$sd.nacional,
                         upper2 = datosnal$media.nacional+2*datosnal$sd.nacional,
                         upper = datosnal$media.nacional+datosnal$sd.nacional,
                         lower= datosnal$media.nacional-datosnal$sd.nacional,
                         movil= media.movil[rownames(media.movil) == "Eritrea",],
                         sdmovil =sd.movil[rownames(sd.movil) == "Eritrea",]
)

colnames( eritrea) <- c("País", "Año", "Tasa", "Tasa_Eritrea",  "más_3_desv.","más_2_desv.",   "más_1_desv.","menos_1_desv.", "Movil", "SDMovil")





eritrea %>%
  mutate(tsa.cmbio= (Tasa_Honduras - lag(Tasa_Honduras, na.pad = TRUE))/(lag(Tasa_Honduras, na.pad = TRUE)), dfrncia=
           Tasa_Honduras - lag(Tasa_Honduras, na.pad = TRUE) , tsa.cmbio2 = ((-(-Tasa_Honduras + lead(Tasa_Honduras, na.pad = TRUE)))/Tasa_Honduras))




venezuela <- data.frame(rep(paises[paises$País== "Venezuela",]$País,longitud),  x = datosnal$year, y=datosnal$media.nacional,
                        mun= as.numeric(t(paises[paises$País == "Venezuela", (2+ (1998-1990) ):(2+(1998-1990) + longitud-1)])),
                        upper3 = datosnal$media.nacional+3*datosnal$sd.nacional,
                        upper2 = datosnal$media.nacional+2*datosnal$sd.nacional,
                        upper = datosnal$media.nacional+datosnal$sd.nacional,
                        lower= datosnal$media.nacional-datosnal$sd.nacional,
                        movil= media.movil[rownames(media.movil) == "Venezuela",],
                        sdmovil =sd.movil[rownames(sd.movil) == "Venezuela",]
)

colnames( venezuela) <- c("País", "Año", "Tasa", "Tasa_Venezuela",  "más_3_desv.","más_2_desv.",   "más_1_desv.","menos_1_desv.", "Movil", "SDMovil")





venezuela %>%
  mutate(tsa.cmbio= (Tasa_Venezuela-lag(Tasa_Venezuela, na.pad = TRUE))/(lag(Tasa_Venezuela, na.pad = TRUE)), dfrncia=
           Tasa_Venezuela-lag(Tasa_Venezuela, na.pad = TRUE) , tsa.cmbio2 = ((-(-Tasa_Venezuela + lead(Tasa_Venezuela, na.pad = TRUE)))/Tasa_Venezuela))



# colnames(venezuela) <- c("País", "Año", "Tasa", "Tasa",  "más_3_desv.","más_2_desv.",   "más_1_desv.","menos_1_desv.", "Movil", "SDMovil")
# colnames(colombia) <- c("País", "Año", "Tasa", "Tasa",  "más_3_desv.","más_2_desv.",   "más_1_desv.","menos_1_desv.", "Movil", "SDMovil")
# colnames(salvador) <- c("País", "Año", "Tasa", "Tasa",  "más_3_desv.","más_2_desv.",   "más_1_desv.","menos_1_desv.", "Movil", "SDMovil")
# colnames(iraq) <- c("País", "Año", "Tasa", "Tasa",  "más_3_desv.","más_2_desv.",   "más_1_desv.","menos_1_desv.", "Movil", "SDMovil")
# colnames(eritrea) <- c("País", "Año", "Tasa", "Tasa",  "más_3_desv.","más_2_desv.",   "más_1_desv.","menos_1_desv.", "Movil", "SDMovil")

allcountries <- data_frame(venezuela$Año, venezuela$Tasa, venezuela$Tasa_Venezuela, eritrea$Tasa_Eritrea, colombia$Tasa_Colombia, salvador$Tasa_El_Salvador, iraq$Tasa_Iraq, syria$Tasa_Syria)
colnames( allcountries) <- c("Ano","Tasa_Promedio", "Venezuela", "Eritrea",  "Colombia",  "El_Salvador", "Iraq", "Siria" )

dd = melt(allcountries, id=c("Ano"))
colnames(dd) <- c("Año","País", "Tasa")


p <- ggplot(data = dd) + geom_line(aes(x= Año, y=Tasa, colour = País))  +
  geom_ribbon(data= venezuela, aes(ymin=más_2_desv., ymax=más_3_desv., x=Año), alpha = 0.3 , fill = "red")+
  geom_ribbon(data= venezuela, aes(ymin=más_1_desv., ymax=más_2_desv., x=Año), alpha = 0.3, fill = "orange")+
  geom_ribbon(data= venezuela, aes(ymin=Tasa, ymax=más_1_desv., x=Año) ,alpha = 0.3, , fill = "yellow")+
  geom_ribbon(data= venezuela, aes(ymin=menos_1_desv., ymax=Tasa, x=Año), fill = "green" ,alpha = 0.3)+
  ggtitle(paste0("Países con violencia crónica en los últimos 20 años")) +
  theme(axis.text.x = element_text(face="bold", color="#993333", 
                                   size=10, angle=40),
        axis.text.y = element_text(face="bold", color="#993333", 
                                   size=10))+
  scale_x_discrete(name ="Año", 
                   limits= años) +
  scale_y_continuous(name ="Tasa de muertes violentas", 
                     breaks= seq(0, max(dd$Tasa), 50)) 

p <- ggplotly(p)
p


saveWidget(p, "graphs/Violencia_20_anos.html")
