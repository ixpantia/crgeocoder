# crgeocoder

Al trabar con paquetes como leaflet, y datos de Costa Rica no encontramos un
paquet que pueda presentar las coordenadas de los cantones y los distritos
sobre el sistema de coordenadas que leaflet requiere.

En este paquet hicimos la traducción de las coordenadas oficiales, publicadas
por el gobierno de Costa Rica en el sistem CRTM05 al sistema usada en Open
Street Map que se llama WSG84-GPS.

## instalación

Para instalar el paquete desde este repositorio usa el siguiente código:

    devtools::install_github("ixpantia/crgeocoder")

## Distritos

Incluimos 471 distritos de Costa Rica

![](vignettes/img/distritos.png?raw=true "Distritos CR")

