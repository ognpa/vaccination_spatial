setwd("~/Downloads/vaccination_spatial")
library(readxl)
#Question to ask is there any correlation spatially for seifa and census data
allImmunization<-read_excel("./data/immunisationAIW.xlsx",sheet="TAB 3",skip=15)
allImmunization=(allImmunization[,c(1,2,3,4,5,6,7,8,9)])
allImmunization1=allImmunization[allImmunization$`Reporting Year`=='2016-17',]
allImmunization1$`Percent fully immunised (%)`<-as.numeric(allImmunization1$`Percent fully immunised (%)`)

unique(allImmunization$`Reporting Year`)

#fc=read.csv("./data/familycompo.csv")
head(allImmunization)
#required=(merge(allImmunization,fc,by.x='SA3 code',by.y='sa3_code16'))
library(sp)
library(rgdal)
library(rmapshaper)



sa3<- readOGR("./data","SA3_2016_AUST")

head(sa3)
nsw_2016<-sa3[sa3$STE_CODE16==1,]
detect<-nsw_2016@data$SA3_CODE16 ==10803
nsw_2016<-nsw_2016[!detect,]
plot(nsw)


plot(sa3)
states_simp <- ms_simplify(nsw)

total=merge(nsw_2016,allImmunization1,by.x='SA3_CODE16',by.y='SA3 code',duplicateGeoms = TRUE)
total=total[complete.cases(total@data),]

proj4string(total) <- CRS("+init=EPSG:27700")
library(tmap)
#Remoteness +education+single mums+socio economic+
tm_shape(total) + tm_fill("opf_chu15_a_no_dss_a_ndch_p") 

remoteness_2016=read.csv("./data/2016/data2016.csv")
head(remoteness_2016)


all_tot=merge(total,remoteness_2016,by.x='SA3_CODE16',by.y='sa3_code')
all_tot=all_tot[complete.cases(all_tot@data),]
tm_shape(all_tot)+tm_fill("remoteness", palette = "Reds", style = "quantile", title = "% with a Qualification") + tm_borders(alpha=.4) 


library(spdep)
neighbours <- poly2nb(all_tot, queen = FALSE)
plot(all_tot, border = 'lightgrey')

 plot(neighbours, coordinates(all_tot), add=TRUE, col='red')

 listw<-nb2listw(neighbours)
 listw


# FUNCTION TO REMOVE NA's IN sp DataFrame OBJECT
#   x           sp spatial DataFrame object
#   margin      Remove rows (1) or columns (2) 
sp.na.omit <- function(x, margin=1) {
  if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT") 
  na.index <- unique(as.data.frame(which(is.na(x@data),arr.ind=TRUE))[,margin])
  if(margin == 1) {  
    cat("DELETING ROWS: ", na.index, "\n") 
    return( x[-na.index,]  ) 
  }
  if(margin == 2) {  
    cat("DELETING COLUMNS: ", na.index, "\n") 
    return( x[,-na.index]  ) 
  }
}

# DELETE NA's IN meuse AND SHOW CHANGE IN dim
alltot2 <- sp.na.omit(all_tot)     
dim(alltot2) 
listw <- nb2listw(neighbours)
listw
moran.test(all_tot$`Percent fully immunised (%)`,listw )
moran.plot(all_tot$`Percent fully immunised (%)`,listw=nb2listw(neighbours,style='W'))
local<-localmoran(x=all_tot$`Percent fully immunised (%)`,listw=nb2listw(neighbours,style='W'))
head(local)
moran.map<-cbind(all_tot,local)
tm_shape(moran.map)+tm_fill(col='Ii',style='quantile',title='bla')
