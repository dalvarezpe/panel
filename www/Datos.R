library(readr)
library(dplyr)
DATAB <- read.csv("D:/DATA SCIENCE PROYECTS/DATA NAVIGATOR INCITATECH/DATABTABLE.txt", sep="")

DATAB$DESDE <- ave(DATAB$Año, DATAB$Indicador, FUN = min)
DATAB$HASTA <- ave(DATAB$Año, DATAB$Indicador, FUN = max)

CODEBOOK <- DATAB%>%group_by(Indicador)%>%slice(1)

CODEBOOK <-select(CODEBOOK, Indicador,NOTA,DESDE,HASTA)
CODEBOOK <- CODEBOOK%>%rename(INDICADOR=Indicador)
#write.table(CODEBOOK, file = "WBCODEBOOK.txt", row.names = FALSE)
