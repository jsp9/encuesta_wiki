rm(list = ls())
setwd('~/Documents/GIT/muestra_wiki/data/')

nsec<-60
edonum<-c(9)


library(data.table)
library(readxl)
library(pps)
library(rgdal)

system(' head ~/Documents/GIT/muestra_wiki/data/pdln_edmslm_re_sexo_20180331.csv')
ln<-fread('pdln_edmslm_re_sexo_20180331.csv')
dto_26<-read_xlsx('basexSeccion_mge2017-2018_3.xlsx', skip = 8)

names(dto_26)<-c('cve_del', 'del', 'dto_fed', 'dto_loc', 'circ', 'SECCION', 'pd', 'ln', 'pob')
sec_26<-dto_26[dto_26$del %in% c('COYOACAN', 'BENITO JUAREZ') & dto_26$dto_loc ==26,]$SECCION
sec_26<-sec_26[!is.na(sec_26)]



sec<-aggregate(data=ln, LISTA~ENTIDAD+SECCION+DISTRITO+MUNICIPIO, sum)

sec<-sec[sec$ENTIDAD %in% edonum & sec$SECCION %in% sec_26,]

computos<-fread('/Users/pepe_opi/Documents/GIT/forotv/carac_votantes/data/diputados.csv', sep = '|', skip = 5)
table(computos$CAND_IND_1)

computos$C_PRI_PVEM<-gsub('-', '',  computos$C_PRI_PVEM )
computos$C_PRD_PT<-gsub('-', '',  computos$C_PRD_PT )
computos$CAND_IND_1<-gsub('-', '',  computos$CAND_IND_1 )
computos$CAND_IND_2<-gsub('-', '',  computos$CAND_IND_2 )
names(computos)

computos<-with(computos, data.frame(ENTIDAD=ESTADO,
                          SECCION=SECCION,
                          PRI=rowSums(cbind(PRI, PVEM, as.numeric(C_PRI_PVEM)), na.rm = T),
                          FRENTE=rowSums(cbind(PAN, PRD, as.numeric(C_PRD_PT), MOVIMIENTO_CIUDADANO),  na.rm = T),
                          MORENA=MORENA+PT,
                          OTROS=rowSums(cbind(NUEVA_ALIANZA, PH,PS)),
                          INDEP=rowSums(cbind(as.numeric(CAND_IND_1), as.numeric(CAND_IND_2)), na.rm = T)
                          ))

computos<-aggregate(data=computos, .~ENTIDAD+SECCION, sum)


part<-c("PRI",      "FRENTE",   "MORENA",   "OTROS",    "INDEP"  )
sec<-merge(computos, sec, by=c('ENTIDAD', 'SECCION'))


#####GENERAR ESTRATOS COMPETENCIA 1ª-2ª
sec$dif<-apply(sec[, part], 1,function(x) max(x))-apply(sec[, part], 1,function(x) x[order(x, decreasing = T)][2])

sec$dif<-sec$dif/rowSums(sec[, c('PRI', 'FRENTE',  'MORENA', 'OTROS', 'INDEP')])

sec$estrato<-cut(sec$dif, breaks = c(min(sec$dif), quantile(sec$dif, probs = c(0.33, 0.66)), max(sec$dif)))

tamano<-aggregate(data=sec, LISTA~estrato, sum)

tamano$prop<-tamano$LISTA/sum(tamano$LISTA)

tamano$nsec<-round(tamano$prop*nsec)
#tamano<-tamano[-9,]
#tamano$nsec[2]<-15
sum(tamano$nsec)


sec<-sec[order(sec$estrato),]


#####SELECCIONAR MEJORES MUESTRAS#######
muestra_secciones<-lapply(1:10, function(x) sec[ppssstrat(sec$LISTA,sec$estrato,tamano$nsec),])
real<-apply(sec[,part], 2, sum)/sum(sec[,part])
errores<-unlist(lapply(muestra_secciones, function(x) sqrt(mean((cbind.data.frame(real,
                                                       muestra=apply(x[, part], 2, sum)/sum(x[, part]),
                                                       dif=real-apply(x[, part], 2, sum)/sum(x[, part])
                                                       )$dif)^2))))

#select<-errores<quantile(errores, 0.001) 


#muestra_secciones<-muestra_secciones[select]

muestra_secciones<-lapply(muestra_secciones,
       function(x) cbind.data.frame(x, strat_sec=ifelse(duplicated(x$SECCION),
                          paste0(x$SECCION, '_a'),
                          x$SECCION)))

muestra_secciones<-lapply(muestra_secciones,
                          function(x) cbind.data.frame(x[,-ncol(x)], strat_sec=ifelse(duplicated(x$strat_sec),
                                                                           paste0(x$SECCION, '_b'),
                                                                           x$strat_sec)))

muestra_manzanas<-lapply(muestra_secciones, function(x) merge(ln, x, by=c('ENTIDAD', 'SECCION'), all.y = T))

lapply(muestra_manzanas,function(x) length(unique(x$strat_sec)))



muestra_manzanas<-lapply(muestra_manzanas, function(x) x[order(x$ ENTIDAD, x$SECCION)])

ppssstrat(sizes = muestra_manzanas[[1]]$LISTA.x,
          stratum = unique(muestra_manzanas[[1]]$strat_sec),
          n= rep(2, length(unique(muestra_manzanas[[1]]$strat_sec))))

muestra_manzanas<-lapply(muestra_manzanas, function(x) do.call(rbind.data.frame, lapply(split(x =  x,
      x$strat_sec), function(y) y[ppss(sizes = y$LISTA.x, n = 2),])))


muestra_manzanas<-lapply(muestra_manzanas, function(x) x[order(x$estrato,x$strat_sec ,x$LISTA.x)])




shps_mza<-readOGR(dsn = '.', layer = 'MANZANA')



muestra_puntos<-lapply(muestra_manzanas,
                       function(x) aggregate(data=x, rep(1, nrow(x))~MANZANA+SECCION+ENTIDAD, sum))




shps_mza<-spTransform(shps_mza,
                      CRS("+proj=longlat +ellps=WGS84 +no_defs+ellps=WGS84 +towgs84=0,0,0 "))
shps_puntos<-list()
for(i in 1:length(muestra_puntos)){
  
 
shps_puntos[[i]]<-shps_mza[shps_mza@data$SECCION %in% unique(muestra_puntos[[i]]$SECCION) &
           shps_mza@data$MANZANA %in% unique(muestra_puntos[[i]]$MANZANA) , ]



shps_puntos[[i]]@data<-merge(shps_puntos[[i]]@data, muestra_puntos[[i]], by=c('SECCION', 'MANZANA'))
}
lapply(split(shps_puntos[[10]], paste0(shps_puntos[[10]]@data$SECCION,
                                     shps_puntos[[10]]@data$MANZANA)),
             function(x) x@data)


muestras_puntos<- lapply(shps_puntos,
       function(x) do.call(rbind,
                           lapply(split(x, paste0(x@data$SECCION,
                                                  x@data$MANZANA)),
       function(y) cbind.data.frame(y@data,
                                    data.frame(lng=(runif(y@data[,'rep(1, nrow(x))']*5,
                                                          min = y@bbox['x', 'min'],
                                                          max =y@bbox['x', 'max'] )),
                                               lat=(runif(y@data[,'rep(1, nrow(x))']*5,
                                                          min = y@bbox['y', 'min'],
                                                          max =y@bbox['y', 'max'] )))))))


lapply(muestras_puntos, nrow)

muestras_puntos[[1]]
library(googleway)

for( a in 1:length(muestras_puntos)){
  print(a)
  for( i in 1:nrow(muestras_puntos[[a]])){
    print(i)
    muestras_puntos[[a]]$direccion[i]<-google_reverse_geocode(unlist(muestras_puntos[[a]][i, c('lat', 'lng')]),
                         key = "",
                         simplify = TRUE)$results[1,]$formatted_address
  }}
  View(muestras_puntos[[1]])

lapply(1:length(muestras_puntos), 
       function(x) write.csv(muestras_puntos[[x]], paste0('muestras_puntos_', x,'.csv' )))



desc_muestra<-muestras_puntos[[1]]

#plot(shps_mza)
#points(desc_muestra$lng, desc_muestra$lat, col='red')




shps_26<-shps_mza[shps_mza@data$SECCION %in% unique(sec$SECCION), ]


shps_26@data<-merge(shps_26@data, sec, by=c('SECCION'))
plot(shps_26, col=shps_26$dif)
points(desc_muestra$lng,desc_muestra$lat , col='red')
library(ggmap)
library(ggplot2)
require("plyr")

gg_shps_26<-fortify(shps_26)
google_dto<-get_map(location = apply(gg_shps_26[, c('long', 'lat')], 2, mean), zoom = 13)

shps_26@data$id<-shps_26@data$ID

gg_shps_26<-join(gg_shps_26, shps_26@data, by='id')



table(merge(muestras_puntos[[1]], muestra_secciones[[1]])$estrato)

ggmap(google_dto)+
geom_polygon(data=gg_shps_26, aes(long, lat, group=group,  alpha=LISTA, fill = estrato))+
  geom_point(data=muestras_puntos[[1]], aes(lng,lat), color='red',alpha=0.2)+
  theme_void()
ggsave('muestra_1.png', device = 'png')
  
names(muestras_puntos[[1]])[3]<-'ENTIDAD'
cbind.data.frame(muestra=apply(computos[computos$ENTIDAD=='9' & computos$SECCION %in% muestras_puntos[[1]]$SECCION,part],2,  sum)/sum(computos[computos$ENTIDAD=='9' & computos$SECCION %in% muestras_puntos[[1]]$SECCION,part]),
                 real=apply(computos[computos$ENTIDAD=='9' & computos$SECCION %in% dto_26$SECCION,part],2,  sum)/sum(computos[computos$ENTIDAD=='9' & computos$SECCION %in% dto_26$SECCION,part]))
dto_26

merge(muestras_puntos[[1]], computos, by=c('ENTIDAD', 'SECCION'), all.y=T)