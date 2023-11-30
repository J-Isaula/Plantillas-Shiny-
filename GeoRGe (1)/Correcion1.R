
library(tidyverse)

resumen_jubilados_vejez_viudez <- read_csv("C:/Users/juan.isaula/Documents/Arch_obt_ded_agos.CSV")


data_original <- resumen_jubilados_vejez_viudez %>% 
  mutate(deducciones = ifelse(as.numeric(deducciones)<0,as.numeric(deducciones),
                              as.numeric(deducciones)*-1),
         devengos = as.numeric(devengos),
         salario_promedio = as.numeric(salario_promedio)) %>% 
  select(no_personal, cc_nomina,deducciones,devengos) %>% 
  pivot_longer(!c(no_personal, cc_nomina), names_to = "tipo", values_to  = "monto") %>% 
  select(-tipo) %>% 
  group_by(no_personal,cc_nomina) %>% 
  summarise(monto = sum(as.numeric(monto), na.rm = TRUE)) %>%
  ungroup() %>% 
  pivot_wider(names_from = cc_nomina, values_from = monto) %>% 
  mutate_if(is.numeric,~replace(.,is.na(.),0)) %>% 
  mutate(Devengo_por_Salario = `1M00`+`1M82`+`1M94`+`1M01`+`1MA3`+`1M53`+`1M59`+
           `1M84`+`1M78`+`1M79`+`1M58`+`1M77`+`1M81`+`1MA1`+`1M81`+`1MA1`+`1MD7`,
         
         #,+`1M96`,`1MC1`+`1M72`+
         
         Devengos_Adicionales = `1M04`+`1M18`+`1M09`+`1M15`+`1M46`+`1M48`+`1MA9`+`1M43`+`1M13`+
           `1M14`+`1M20`+`1M11`+`1M47`+`1M10`+`1M52`+`1M34`+`1M51`+`1M16`+`1M29`+
           `1M36`+`1M45`+`1ME6`+`1ME7`+`1ME8`+`1ME4`+`1MD6`+`1M32`+`1ME1`+`1M99`+`1MD3`,
         # `1M40`+`1M98`
         
         Judiciales = `2T59`+`2T60`+`2TM0`,
         
         Deducciones_de_Ley = `/300`+`/310`+`2T02`+`2T03`+`2T55`+`/400`+`2T33`+`/315`+`2T07`+
           `2T32`+`2T41`+`2T31`+`2T29`+`2T65`+`2T40`+`2T04`+`2T30`+`2T61`+`2T10`+
           `/410`+`2T27`,
         
         Deducciones_Adicionales = `2T11`+`2T67`+`2T34`+`2TI0`+`2T24`+`2T48`+`2T19`+`2T35`+`2T46`+
           `2T45`+`2T21`+`2T08`+`2T25`+`2T39`+`2T68`+`2T44`+`2T51`+`2T09`+
           `2T16`+`2T47`+`2T42`+`2T17`+`2T26`+`2T14`+`2T62`+`2TE0`+`2T69`+`2TQ0`,
         
         Neto = round(Devengo_por_Salario+Devengos_Adicionales+Judiciales+Deducciones_de_Ley+Deducciones_Adicionales,2)
         
  ) 

salario_promedio <- resumen_jubilados_vejez_viudez %>% 
  select(no_personal,cc_nomina,salario_promedio) %>%
  pivot_longer(!c(no_personal, cc_nomina), names_to = "tipo", values_to  = "monto") %>% 
  select(-tipo) %>% 
  group_by(no_personal,cc_nomina) %>%
  summarise(monto = sum(as.numeric(monto), na.rm = TRUE)) %>%
  ungroup() %>% 
  pivot_wider(names_from = cc_nomina, values_from=monto)

# -----------------------------------------------------------------------------
salario_promedio <- as.data.frame(salario_promedio)

prueba2 <- filter(resumen_jubilados_vejez_viudez,!duplicated(no_personal))

prueba2 <- as.data.frame(prueba2)
# -----------------------------------------------------------------------------


reporte_prelm <- salario_promedio %>% 
  select(no_personal) %>% 
  rename('No_Personal'= no_personal) %>% 
  mutate(
    Salario_Mensual = salario_promedio$`9019`,
    Devengo_por_Salario = data_original$Devengo_por_Salario,
    Devengos_Adicionales = data_original$Devengos_Adicionales,
    Judiciales = data_original$Judiciales,
    Deducciones_de_Ley = data_original$Deducciones_de_Ley,
    Deducciones_Adiconales = data_original$Deducciones_Adicionales,
    Neto = data_original$Neto,
    Tipo_Contrato = prueba2$denominacion_ci_colectivo,
    Regimen = prueba2$regimen)


reporte_prelm <- as.data.frame(reporte_prelm)


# -----------------------------------------------------------------------------


salario <-  resumen_jubilados_vejez_viudez %>% 
  select(no_personal,cc_nomina,nombre) %>% 
  pivot_longer( !c(no_personal,cc_nomina),names_to = "tipo", values_to = "monto") %>%
  select(-tipo) %>% 
  group_by(no_personal,cc_nomina)

salario <- as.data.frame(salario) %>% select(-cc_nomina)

salario <- salario[!duplicated(salario %>% select(no_personal)),] %>% rename("No_Personal" = no_personal)

reporte <- reporte_prelm %>%  left_join(salario,by = "No_Personal")

# -----------------------------------------------------------------------------
identidad <- resumen_jubilados_vejez_viudez %>% 
  select(no_personal,cc_nomina,id) %>% 
  pivot_longer(!c(no_personal,cc_nomina),names_to = "tipo", values_to = "id") %>% 
  select(-tipo) %>% 
  group_by(no_personal,cc_nomina)

identidad <- as.data.frame(identidad) %>% select(-cc_nomina)

identidad <- identidad[!duplicated(identidad %>% select(no_personal)),] %>% rename("No_Personal" = no_personal)

reporte <- reporte %>% left_join(identidad,by = "No_Personal")
# -----------------------------------------------------------------------------

regimen <- resumen_jubilados_vejez_viudez %>%  
  select(no_personal,cc_nomina,regimen) %>% 
  pivot_longer(!c(no_personal, cc_nomina), names_to = "tipo",values_to = "tip_regimen") %>% 
  select(-tipo) %>% 
  group_by(no_personal,cc_nomina)

regimen <- as.data.frame(regimen) %>% select(-cc_nomina)

regimen <- regimen[!duplicated(regimen %>%  select(no_personal)),] %>% rename("No_Personal" = no_personal)

reporte <- reporte %>% left_join(regimen, by = "No_Personal")
# -----------------------------------------------------------------------------

tipo_contrato <-  resumen_jubilados_vejez_viudez %>% 
  select(no_personal, cc_nomina, denominacion_ci_colectivo) %>% 
  pivot_longer(!c(no_personal,cc_nomina),names_to = "tipo", values_to = "tip_contrato") %>% 
  select(-tipo) %>% 
  group_by(no_personal,cc_nomina)

tipo_contrato <- as.data.frame(tipo_contrato) %>% select(-cc_nomina)

tipo_contrato <- tipo_contrato[!duplicated(tipo_contrato %>%  select(no_personal)),] %>% rename("No_Personal" = no_personal)

reporte <- reporte %>%  left_join(tipo_contrato,by = "No_Personal")


# -----------------------------------------------------------------------------

reporte_final <- reporte %>% 
  select(No_Personal) %>% 
  rename("No. Personal" = No_Personal) %>% 
  mutate(Nombre    = reporte$monto,
         Identidad = reporte$id,
         Salario_Mensual = reporte$Salario_Mensual,
         Devengo_por_Salario = data_original$Devengo_por_Salario,
         Devengos_Adicionales = data_original$Devengos_Adicionales,
         Judiciales = data_original$Judiciales,
         Deducciones_de_Ley = data_original$Deducciones_de_Ley,
         Deducciones_Adiconales = data_original$Deducciones_Adicionales,
         Neto = data_original$Neto,
         Tipo_Contrato = reporte$tip_contrato,
         Regimen = reporte$tip_regimen)

# -----------------------------------------------------------------------------

  


