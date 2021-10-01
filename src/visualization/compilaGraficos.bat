ECHO OFF

cd ../..

RScript src/visualization/001_visualizacionFechaCreacionFRE.R
RScript src/visualization/002_analisisProfesiones.R
RScript src/visualization/004_analisisPrecioVenta.R
RScript src/visualization/006_rutaTecnologica_1.R
RScript src/visualization/007_manejoInventarios_1.R
RScript src/visualization/008_rutaTecnologica_2.R

PAUSE