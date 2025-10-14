
library(spdep)
library(sf)
library(spatialreg)
chi.poly <- st_read('dados/foreclosures.shp')
class(chi.poly)

# tem os polígonos e data.frame

# pra extyrair o df

df_chicago <- st_drop_geometry(chi.poly)
head(df_chicago)

# est_fcs: estimated count of foreclosure starts from Jan. 2007 through June 2008
# est_mtgs: estimated number of active mortgages from Jan. 2007 through June 2008
# est_fcs_rt: number of foreclosure starts divided by number of mortgages times 100
# bls_unemp: June 2008 place or county unemployment rate
# totpop: total population from 2000 Census
# violent: number of violent crimes reported between Jan. 2007 through December 2008
# property: number of property crimes reported between Jan. 2007 through December 2008

str(slot(chi.poly,"data"))

plot(chi.poly)

### OLS

# podemos rodar um modelo de regressão:

chicago_ols <- lm(violent~est_fcs_rt+bls_unemp, data=df_chicago)
summary(chicago_ols)  

### Modelando Dependência Espacial

#### criando vizinhanças
list.queen <- poly2nb(chi.poly, queen=TRUE)
W <- nb2listw(list.queen, style="W", zero.policy=TRUE)
W

#### coordenadas
coords <- st_coordinates(st_centroid(chi.poly))
plot(W,coordinates(chi.poly))

plot(st_geometry(chi.poly), border = 'grey')
plot(W, coords, add = TRUE, col = "red")


W_dist <- dnearneigh(coords,0,1,longlat = FALSE)

#### Spatial Autoregressive (SAR) Models


####  Spatial Error Models (SEM)


#### Moran's teste

moran.lm <- lm.morantest(chicago_ols, W, alternative="two.sided")
print(moran.lm)

### Teste de Multiplicadores de Lagrange

LM <-  lm.RStests(chicago_ols, W, test="all")
print(LM)

sar.chi <- spatialreg::lagsarlm(violent~est_fcs_rt+bls_unemp, data=df_chicago, W)
summary(sar.chi)

df_chicago$chi.ols.res <- resid(chicago_ols) # residuals ols

df_chicago$chi.sar.res <- resid(sar.chi) # residual sar

sp::spplot(chi.poly,"chi.sar.res",
       at=seq(min(df_chicago$chi.sar.res,na.rm=TRUE),
                                     max(df_chicago@data$chi.sar,na.rm=TRUE), length=12),
       col.regions=rev(brewer.pal(11,"RdBu")))

chi.poly$chi.sar.res <- df_chicago$chi.sar.res

library(tmap)
tm_shape(chi.poly) +
  tm_fill("chi.sar.res", 
          palette = "-RdBu", 
          style = "cont",
          n = 11) +
  tm_borders()


library(ggplot2)
ggplot(chi.poly) +
  geom_sf(aes(fill = chi.sar.res)) +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  theme_minimal()
