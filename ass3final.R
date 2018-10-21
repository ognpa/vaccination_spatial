setwd("c:/users/subu/vaccination_spatial")

library(readxl)

nsw_immunization=read_excel("./data/nsw-sep2017.xlsx",sheet="Sheet1",skip=5)
nsw_immunization=nsw_immunization[,c(2,4,13)]
nsw_immunization$`% Fully`=as.numeric(nsw_immunization$`% Fully`)
nsw_immunization=na.omit(nsw_immunization)
dim(nsw_immunization)
rownames(nsw_immunization)=as.data.frame(seq(1:276))

mbs=read_excel("./data/datasheet-report-hc48.xlsx",sheet="GP SA3",skip=11)
mbs_nsw=mbs[((mbs$State=='NSW') & (mbs$`Reporting Year`=='2016-17')),]

head(mbs_nsw)
colnames(mbs_nsw)
mbs_nsw=mbs_nsw[,c(2,4,7)]

colnames(mbs_nsw)=c('SA3 code','RemotenessSES','attendance')
mbs_nsw$attendance=as.numeric(mbs_nsw$attendance)
which(is.na(mbs_nsw))
mbs_nsw=na.omit(mbs_nsw)
head(mbs_nsw)
head(nsw_immunization)
one=merge(nsw_immunization,mbs_nsw,by.x='SA3 Code',by.y='SA3 code',all.x=TRUE)
dim(one)
dim(nsw_immunization)
dim(mbs)
one=na.omit(one)


diis=read.csv("./data/diis.csv")
diis['bachelors']=diis$bachelor_degree_level_tot_2016/diis$est_res_pop
diis['advancedDiploma']=diis$advanced_diploma_diploma_level_tot_2016/diis$est_res_pop
diis['noschool']=diis$no_scl_tot_2016/diis$est_res_pop
diis['gradcert']=diis$graduate_diploma_graduate_certificate_level_tot_2016/diis$est_res_pop
diis['yr12']=diis$yr_12_equivalent_tot_2016/diis$est_res_pop
diis['some_edu']=(diis$bachelor_degree_level_tot_2016+
                    diis$advanced_diploma_diploma_level_tot_2016+
                    diis$graduate_diploma_graduate_certificate_level_tot_2016+
                    diis$yr_12_equivalent_tot_2016)/diis$est_res_pop
diis['unemployed']=diis$unemployed_tot/diis$est_res_pop

diis=diis[,c(6,10,12,13,14,15,16,17,18,19,20,21,22)]
head(diis)

two=merge(one,diis,by.x='SA3 Code',by.y='sa3_code',all.x=TRUE)
median_info=read.csv("./data/medianinfo.csv")
median_info=median_info[,c(2,3,4,7,8)]
three=merge(two,median_info,by.x='SA3 Code',by.y='sa3_code16',all.x=TRUE)

head(three)



library(sp)
library(rgdal)
library(rmapshaper)



sa3<- readOGR("./data","SA3_2016_AUST")

nsw_2016<-sa3[sa3$STE_CODE16==1,]
detect<-nsw_2016@data$SA3_CODE16 ==10803
nsw_2016<-nsw_2016[!detect,]


total=merge(nsw_2016,three,by.x='SA3_CODE16',by.y='SA3 Code',how='left',duplicateGeoms = TRUE)

proj4string(total) <- CRS("+init=EPSG:27700")
library(tmap)


#Remoteness +education+income+family size

#tm_shape(total) + tm_fill("% Fully") 
tm_shape(total) + tm_fill("median_tot_fam_inc_weekly") 
tm_shape(total) + tm_fill("average_household_size") 
tm_shape(total)+tm_fill("some_edu")
tm_shape(total)+tm_fill("RemotenessSES")

all_tot=total[complete.cases(total@data),]
library(spdep)
neighbours <- poly2nb(all_tot, queen = FALSE)
plot(all_tot, border = 'lightgrey')


plot(neighbours, coordinates(all_tot), add=TRUE, col='red')

listw<-nb2listw(neighbours)
listw
moran.test((all_tot$`% Fully`),listw )
moran.plot((all_tot$`% Fully`),listw=nb2listw(neighbours,style='W'))
local<-localmoran(x=all_tot$`% Fully`,listw=nb2listw(neighbours,style='W'))
head(local)
# draw a LISA map

moran.map<-cbind(all_tot,local)
tm_shape(moran.map)+tm_fill(col='Ii',style='quantile',title='local morran statistic')
names(moran.map@data)

tm_shape(moran.map) + tm_fill(col = "Pr.z...0.", style = "fixed", breaks=c(0.001,0.01,0.05,0.1,0.2,1), title = "p-values") 
model1=lm(all_tot$`% Fully`~
            all_tot$median_tot_fam_inc_weekly+
            all_tot$some_edu+
            all_tot$`Age Group`+
            all_tot$RemotenessSES+
            all_tot$median_age_persons+
            all_tot$average_household_size
)
summary(model1)
plot(model1)

resids<-residuals(model1)
map.resids <- cbind(all_tot, resids) 
# we need to rename the column header from the resids file - in this case its the 6th column of map.resids
names(map.resids)[6] <- "resids"
# maps the residuals using the quickmap function from tmap
qtm(map.resids, fill = "resids")
tm_shape(map.resids) + tm_fill("resids") 
library("spgwr")
GWRbandwidth <- gwr.sel(all_tot$`% Fully`~
                          all_tot$`Age Group`+
                          all_tot$median_tot_fam_inc_weekly+
                          all_tot$median_age_persons+
                         # all_tot$RemotenessSES+
                          all_tot$some_edu,
                        data=all_tot,adapt=T)
#fit the gwr model (note it has the same formula as before)
row.names(all_tot)=make.names(all_tot$SA3_CODE16,unique=TRUE)
#fit the gwr model (note it has the same formula as before)
gwr.model = gwr(`% Fully`~
                  `Age Group`+
                  median_tot_fam_inc_weekly+
                  median_age_persons+
                  some_edu,
                data = all_tot, adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 
#OK. What did it do?
gwr.model
names(model1)
names(gwr.model)
model1$coefficients
results <-as.data.frame(gwr.model$SDF)
head(results)
gwr.model$SDF@data


gwr.map <- cbind(all_tot, as.matrix(results))
tm_shape(gwr.map) + tm_fill("localR2")
