#----[AUTOR]----------------------------------------------------------------------------------------
'''
Autor: David Felipe Calvo
Fecha: 28/03/2025

'''
#===================================================================================================
#----[LIBRERÍAS]------------------------------------------------------------------------------------

#===================================================================================================
#----[LIMPIEZA DE DATOS]----------------------------------------------------------------------------
#--------[DATOS INDIVIDUALES]-----------------------------------------------------------------------
dfInd <- read.csv('C:/data/Childcare_ML/MTUS_haf.csv')

dfInd <- dfInd[dfInd['country'] == 'United States', ]

dfInd <- dfInd[dfInd['survey'] >= 2015, ]
dfInd <- dfInd[dfInd['survey'] <= 2019, ]
save(dfInd, file = 'C:/data/Childcare_ML/IndUSA1519.RData')

rm(dfInd)

#--------[DATOS DIARIOS]----------------------------------------------------------------------------
dfDiaries <- read.csv(file = 'C:/data/Childcare_ML/MTUS_hef2.csv')

dfDiaries <- dfDiaries[dfDiaries['country'] == 'United States', ]

dfDiaries <- dfDiaries[dfDiaries['survey'] >= 2015, ]
dfDiaries <- dfDiaries[dfDiaries['survey'] <= 2019, ]
save(dfDiaries, file = 'C:/data/Childcare_ML/DiariesUSA1519.RData')

#===================================================================================================

#===================================================================================================
#----[LIMPIEZA DE DATOS AMPLIADA]-------------------------------------------------------------------
#--------[DATOS INDIVIDUALES]-----------------------------------------------------------------------
dfInd <- read.csv('C:/data/Childcare_ML/MTUS_haf.csv')

dfInd <- dfInd[dfInd['country'] == 'United States', ]

dfInd <- dfInd[dfInd['survey'] >= 2010, ]
dfInd <- dfInd[dfInd['survey'] <= 2019, ]
save(dfInd, file = 'C:/data/Childcare_ML/IndUSA1019.RData')

rm(dfInd)

#--------[DATOS DIARIOS]----------------------------------------------------------------------------
dfDiaries <- read.csv(file = 'C:/data/Childcare_ML/MTUS_hef2.csv')

dfDiaries <- dfDiaries[dfDiaries['country'] == 'United States', ]

dfDiaries <- dfDiaries[dfDiaries['survey'] >= 2010, ]
dfDiaries <- dfDiaries[dfDiaries['survey'] <= 2019, ]
save(dfDiaries, file = 'C:/data/Childcare_ML/DiariesUSA1019.RData')

#========================================================================================================

