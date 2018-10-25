# crgeocoder

En R podemos crear mapas utilizando paquetes como [leaflet](https://rstudio.github.io/leaflet/)
que utilizan coordenadas para poder generar marcadores u otras utilidades. En
el caso de trabajar con datos de Costa Rica, no encontramos fácilmente un
paquete que pueda representar las coordenadas de todos los cantones y 
distritos en el sistema que leaflet requiere.


En este paquete hicimos la traducción de las coordenadas oficiales, tomadas del
documento: **"División Territorial Administrativa de Costa Rica según decreto N° 35213-MG"**, 
el cual fue publicado en el periódico oficial La Gaceta el N° 85 DEL 5 DE MAYO DE 2009, 
en formato CRTM05 al sistema usado en Open Street Map llamado WSG84-GPS

## Instalación

Para instalar el paquete desde este repositorio usa el siguiente código:

    devtools::install_github("ixpantia/crgeocoder")

## Distritos

Incluimos 471 distritos de Costa Rica

![](vignettes/img/distritos.png?raw=true "Distritos CR")

