ECHO OFF

RScript -e "bookdown::render_book(c('index.Rmd', 'L001_Introduccion.Rmd', 'L002_Objetivos.Rmd', 'L003_Justificacion.Rmd', 'L101_EstructuraFRE.Rmd', 'L102_RecOfi_existencias.Rmd', 'L103_RecOfi_costo.Rmd', 'L104_RecOfi_adquisicion.Rmd', 'L105_RecOfi_procesos.Rmd', 'L106_RecOfi_caracteristicas.Rmd', 'L151_Medicamentos_Med.Rmd', 'L152_Medicamentos_Adq.Rmd', 'L153_Medicamentos_Rcp.Rmd', 'L154_Medicamentos_Alm.Rmd', 'L155_Medicamentos_Prec.Rmd', 'L200_RutaTecnologica.Rmd', 'L301_GestionInformes.Rmd', 'L451_Regional.Rmd', 'L500_Glosario.Rmd', 'L501_Creditos.Rmd'))"

PAUSE

