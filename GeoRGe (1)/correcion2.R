
library(tidyverse)

resumen_jubilados_vejez_viudez <- read_csv("C:/Users/juan.isaula/Documents/Arch_obt_ded_agos.CSV")


salario_promedio <- resumen_jubilados_vejez_viudez  %>% 
  select(no_personal,cc_nomina,salario_promedio) %>%
  pivot_longer(!c(no_personal, cc_nomina), names_to = "tipo", values_to  = "monto") %>% 
  select(-tipo) %>% 
  group_by(no_personal,cc_nomina) %>%
  summarise(monto = sum(as.numeric(monto), na.rm = TRUE)) %>%
  ungroup() %>% 
  pivot_wider(names_from = cc_nomina, values_from=monto)

salario_mensual <- salario_promedio %>% 
  mutate_if(is.numeric,~replace(.,is.na(.),0))


prestaciones <- resumen_jubilados_vejez_viudez[!duplicated(resumen_jubilados_vejez_viudez %>% select(no_personal)),] %>% 
  select(no_personal, nombre,id,fecha_ingreso,denominacion_ci_colectivo)

prestaciones <- prestaciones %>% left_join(salario_mensual,by = "no_personal")

prestaciones <- as.data.frame(prestaciones)
  
  
prestaciones <- prestaciones %>% select(no_personal,nombre,id,fecha_ingreso,denominacion_ci_colectivo,`9019`) %>% 
  rename("tipo_contrato" = denominacion_ci_colectivo)
  
  
 