#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#.
#                                                                                                           #
#                              APLICACION PARA GENERAR REPORTE BANRURAL                                     #
#                                       AUTHOR: Juan Isaula                                                 #
#                                    UNIDAD DE ACTUARIA - IHSS                                              #
#                                                                                                           #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Llamamos los procesos de iu y server                               ----
processes_paths <- paste0(getwd(), "/Procesos/")
source(paste0(processes_paths,"IU.R"))
source(paste0(processes_paths,"Servidor.R"))

# 7. Compilacion de la aplicación

shinyApp(IU, Servidor)



