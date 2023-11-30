#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
#                                                                                                                                                                                                              #
#                                                                                                       SERVIDOR                                                                                               #
#                                                                                                 AUTHOR: Juan Isaula                                                                                          #
#                                                                                              UNIDAD DE ACTUARIA - IHSS                                                                                       #
#                                                                                                                                                                                                              #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
 
library(lubridate)
Servidor <- function(input, output) {
  
  # 4.1 Cargar datos -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  df_products_upload <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df <- read_delim(inFile$datapath,  delim = input$separator)
    return(df)
  })
  # 4.2 Leer Datos --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
  
  output$sample_table <- DT::renderDataTable({
    df <- df_products_upload()
    DT::datatable(df)
  })
  
  
  
  # 4.3 Código para generar reporte ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
  
  output$reporte <- DT::renderDataTable({
 
    if(is.null(df_products_upload())){
      DT::datatable(df_products_upload())
    }
    else{
    
    
    data_original <- df_products_upload() %>% 
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
      mutate(Devengo_por_Salario = `1M00`+`1M82`+`1M01`+`1MA3`+`1M53`+`1M58` +`1M72`+
                `1M84`+`1M78`+`1M77`+`1M81`+`1M94`+`1MA1`+`1M79`+`1M96`,
             # +`1M59`  +`M600`+`1MD7`+`1MC1`
             
             Devengos_Adicionales = `1M18`+`1M09`+`1M15`+`1M46`+`1M48`+
               `1M29`+`1M45`+`1M32`+`1M04`+`1M20` +`1M99` +`1M13`+`1M43`+
               `1M34`+`1M16`+`1ME6`+`1ME7`+`1ME8`+`1ME4`+`1MD3` +`1M10`+
                `1M14` +`1M51`+`1MA9`+`1M47`+`1M52`+`1M36`,
              # +`1ME1`+`1M40` +`1M11`
           
             
             Judiciales = `2T59`+`2T60`+`2TM0`,
             # 
             
             Deducciones_de_Ley = `/300`+`2T02`+`2T03`+`2T55`+`/400`+`2T33`+
               `2T32`+`2T29`+`2T04`+`2T30`+`2T10`+`/310`+`2T40`+`2T27`+`/315`+
               `2T41`+`2T07`+`2T65`+`/410`+`2T61`+`2T31`,
            # 
             
             Deducciones_Adicionales = `2T11`+`2T67`+`2T34`+`2T19`+`2T24`+`2T35`+`2T44`+`2TE0`+`2T26`+
               `2T45`+`2T08`+`2T25`+`2T39`+`2T51`+`2T09`+`2T42`+`2T14`+`2TQ0`+`2T21`+`2T46`+
              `2T47`+`2T16`+`2TI0`+`2T17`+`2T68`+`2T48`+`2T62` +`2T69`,
            
             # 
             
             Neto = round(Devengo_por_Salario+Devengos_Adicionales+Judiciales+Deducciones_de_Ley+Deducciones_Adicionales,2)
             
      ) 
    
    salario_promedio <- df_products_upload() %>% 
      select(no_personal,cc_nomina,salario_promedio) %>%
      pivot_longer(!c(no_personal, cc_nomina), names_to = "tipo", values_to  = "monto") %>% 
      select(-tipo) %>% 
      group_by(no_personal,cc_nomina) %>%
      summarise(monto = sum(as.numeric(monto), na.rm = TRUE)) %>%
      ungroup() %>% 
      pivot_wider(names_from = cc_nomina, values_from=monto)
    
    salario_promedio <- as.data.frame(salario_promedio)
    
    # -----------------------------------------------------------------------------
    salario_promedio <- as.data.frame(salario_promedio)
    
    prueba2 <- filter(df_products_upload(),!duplicated(no_personal))
    
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
    salario <- df_products_upload() %>% 
      select(no_personal,cc_nomina,nombre) %>% 
      pivot_longer( !c(no_personal,cc_nomina),names_to = "tipo", values_to = "monto") %>%
      select(-tipo) %>% 
      group_by(no_personal,cc_nomina)
    
    salario <- as.data.frame(salario) %>% select(-cc_nomina)
    
    salario <- salario[!duplicated(salario %>% select(no_personal)),] %>% rename("No_Personal" = no_personal)
    
    reporte <- reporte_prelm %>%  left_join(salario,by = "No_Personal")
    # -----------------------------------------------------------------------------
    identidad <- df_products_upload() %>% 
      select(no_personal,cc_nomina,id) %>% 
      pivot_longer(!c(no_personal,cc_nomina),names_to = "tipo", values_to = "id") %>% 
      select(-tipo) %>% 
      group_by(no_personal,cc_nomina)
    
    identidad <- as.data.frame(identidad) %>% select(-cc_nomina)
    
    identidad <- identidad[!duplicated(identidad %>% select(no_personal)),] %>% rename("No_Personal" = no_personal)
    
    reporte <- reporte %>% left_join(identidad,by = "No_Personal")
    # -----------------------------------------------------------------------------
    
    regimen <- df_products_upload() %>%  
      select(no_personal,cc_nomina,regimen) %>% 
      pivot_longer(!c(no_personal, cc_nomina), names_to = "tipo",values_to = "tip_regimen") %>% 
      select(-tipo) %>% 
      group_by(no_personal,cc_nomina)
    
    regimen <- as.data.frame(regimen) %>% select(-cc_nomina)
    
    regimen <- regimen[!duplicated(regimen %>%  select(no_personal)),] %>% rename("No_Personal" = no_personal)
    
    reporte <- reporte %>% left_join(regimen, by = "No_Personal")
    # -----------------------------------------------------------------------------
    tipo_contrato <-  df_products_upload() %>% 
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
    
    
    
    #tipo_contrato <- df_products_upload()[!duplicated(df_products_upload() %>% select(no_personal)),]
    
    #reporte <- tipo_contrato %>% 
     # select(no_personal, nombre, id) %>% 
     # rename('No_Personal'=no_personal,
      #       'Nombre' = nombre,
       #      'Identidad' = id) %>% 
      #mutate(
       # Salario_Mensual = salario_promedio$`9019`,
        #Devengo_por_Salario = data_original$Devengo_por_Salario,
        #Devengos_Adicionales = data_original$Devengos_Adicionales,
        #Judiciales = data_original$Judiciales,
        #Deducciones_de_Ley = data_original$Deducciones_de_Ley,
        #Deducciones_Adiconales = data_original$Deducciones_Adicionales,
        #Neto = data_original$Neto,
        #Tipo_Contrato = tipo_contrato$denominacion_ci_colectivo,
        #Regimen = tipo_contrato$regimen)
    
    
    
    DT::datatable(reporte_final,extensions = c('Buttons','ColReorder','KeyTable','Responsive'),options = list(dom = 'Bfrtip',
                                                                                                        lengthMenu = list(c(10,15,25,-1), c("10","15","25","All")),
                                                                                                        pageLength = -1,
                                                                                                        colReorder = TRUE,keys = TRUE,pageLength=20,
                                         language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                         initComplete = JS(
                                           "function(settings, json) {",
                                           "$(this.api().table().header()).css({'background-color': 'gray', 'color': '#fff'});",
                                           "}"),
                                         search = list(regex = TRUE, caseInsensitive = FALSE, search = ' '),
                                         buttons = c("copy", "csv","excel","pdf","print"))) %>% 
      formatRound(4:7, 3)
    }
    
  })
  # 4.4 Valor de caja con cantidad de empleados item 2 ----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  output$EmpleadosSinDuplicar <- renderValueBox({
    if(is.null(df_products_upload())){
      valueBox(
        paste0('-'),'Total Empleados',
        icon = icon('user'),
        color = 'orange'
      )
    }
    else{
      
    
      salario_promedio <- df_products_upload() %>% 
        select(no_personal,cc_nomina,salario_promedio) %>%
        pivot_longer(!c(no_personal, cc_nomina), names_to = "tipo", values_to  = "monto") %>% 
        select(-tipo) %>% 
        group_by(no_personal,cc_nomina) %>%
        summarise(monto = sum(as.numeric(monto), na.rm = TRUE)) %>%
        ungroup() %>% 
        pivot_wider(names_from = cc_nomina, values_from=monto)
      
      salario_mensual <-  salario_promedio%>% 
        mutate_if(is.numeric,~replace(.,is.na(.),0))
      
      valueBox(
        paste0(prettyNum(nrow(salario_mensual),big.mark = ',',scientific = FALSE)),'Empleados no Duplicados',
        icon = icon('user'),
        color = 'orange'
      )
    }
  })
  
  
  
  
  output$tabla3 <- DT::renderDataTable({
    if(is.null(df_products_upload())){
      DT::datatable(df_products_upload())
    }
    else{
      
    
    salario_promedio <- df_products_upload() %>% 
      select(no_personal,cc_nomina,salario_promedio) %>%
      pivot_longer(!c(no_personal, cc_nomina), names_to = "tipo", values_to  = "monto") %>% 
      select(-tipo) %>% 
      group_by(no_personal,cc_nomina) %>%
      summarise(monto = sum(as.numeric(monto), na.rm = TRUE)) %>%
      ungroup() %>% 
      pivot_wider(names_from = cc_nomina, values_from=monto)
    
    salario_mensual <- salario_promedio %>% 
      mutate_if(is.numeric,~replace(.,is.na(.),0))

    
    # 4.6 Estimacion de deducciones y devengo -------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    estimacion_deducciones <- df_products_upload()[!duplicated(df_products_upload() %>% select(no_personal)),] %>% 
      select(no_personal, nombre,id,fecha_ingreso,denominacion_ci_colectivo,regimen) %>%
      rename("tipo_contrato" = denominacion_ci_colectivo) %>% 
      mutate(sueldo =  salario_mensual$`9019`,
             fecha_ingreso = ymd(dmy(fecha_ingreso)),
             fecha_referencia = input$date1,
             antiguedad = (360*(year(fecha_referencia)-year(fecha_ingreso))+
                             30*(month(fecha_referencia)-month(fecha_ingreso))+
                             (day(fecha_referencia) -day(fecha_ingreso)))/360, 
             anios_completos = floor(antiguedad),
             fraccion_anios = antiguedad - anios_completos,
             meses = floor(fraccion_anios*12),
             dias = round(fraccion_anios*360 - meses*30,1),
             fecha_ultimo_decimo_cuarto = input$date2,
             fecha_ultimo_aguinaldo = input$date3, 
             
             porcentaje_clapsula_32 = ifelse(tipo_contrato=="Permanente",ifelse(antiguedad<5.5 & antiguedad>=1,65,
                                                                                ifelse(antiguedad>=5.5 & antiguedad<10.5,70,
                                                                                       ifelse(antiguedad>=10.5 & antiguedad<15.5,75,
                                                                                              ifelse(antiguedad>15.5 & antiguedad<20.5,80,
                                                                                                     ifelse(antiguedad>=20.5 & antiguedad<23.5,90,
                                                                                                            ifelse(antiguedad>=23.5 & antiguedad<30.5,100,
                                                                                                                   ifelse(antiguedad>=30.5,120,0))))))),0)/100,
             porcentaje_bono_junio = ifelse(tipo_contrato == "Permanente",ifelse(antiguedad<1,0,
                                                                                 ifelse(antiguedad<10,0.125,
                                                                                        ifelse(antiguedad<20,0.175,0.225))),0),
             porcentaje_bono_vacaciones = ifelse(tipo_contrato == "Permanente",
                                                 ifelse(antiguedad>5,1.2,1),0),
             dias_bono_vacaciones = ifelse(ceiling(antiguedad)>4,30,ifelse(ceiling(antiguedad) == 1,12,
                                                                           ifelse(ceiling(antiguedad) == 2,15,
                                                                                  ifelse(ceiling(antiguedad) == 3,20,
                                                                                         ifelse(ceiling(antiguedad) == 4,25,0))))),
             SBM = sueldo,
             SOM = ifelse(tipo_contrato == "Permanente",(14+dias_bono_vacaciones/30*porcentaje_bono_vacaciones+porcentaje_bono_junio)/12*SBM,14/12*SBM),
             
             aguinaldo_proporcional = round((360*(year(fecha_referencia)-year(fecha_ultimo_aguinaldo)) - 30*(month(fecha_ultimo_aguinaldo)-month(fecha_referencia)))/360*SBM,2),
             decimo_cuarto_proporcional = round((360*(year(fecha_referencia)-year(fecha_ultimo_decimo_cuarto)) - 30*(month(fecha_ultimo_decimo_cuarto)-month(fecha_referencia)))/360*SBM,2),
             bono_vacaciones_proporcional = fraccion_anios*sueldo*dias_bono_vacaciones/30*porcentaje_bono_vacaciones,
             bono_junio_proporcional = sueldo*porcentaje_bono_junio,
             vaciones_venciadas = 0,
             total_derechos_laborales = aguinaldo_proporcional+decimo_cuarto_proporcional+bono_vacaciones_proporcional+bono_junio_proporcional,
             preaviso = ifelse(anios_completos<1, ifelse(meses<3,1,ifelse(meses>=3 & meses<6,7,14)),ifelse(anios_completos == 1 & meses==0 & dias==0,14,ifelse(anios_completos<2,30,60)))/30*SOM,
             auxilio_por_cesantia = round(pmin(25,anios_completos)*SOM,2),
             auxilio_por_cesantia_prop = ifelse(antiguedad>25,0,ifelse(antiguedad>1,fraccion_anios*30,ifelse(meses+dias/30<=3,0,ifelse(meses+dias/30<=6,10,ifelse(antiguedad<=1,20,30))))*SOM/30),
             total_prestaciones = preaviso + auxilio_por_cesantia + auxilio_por_cesantia_prop,
             clausula_32 = porcentaje_clapsula_32*antiguedad*sueldo,
             derechos_laborales = ifelse(aguinaldo_proporcional>0,aguinaldo_proporcional,0)+bono_vacaciones_proporcional+ifelse(decimo_cuarto_proporcional>0,decimo_cuarto_proporcional,0)+bono_junio_proporcional+vaciones_venciadas,
             prestaciones = pmax(total_prestaciones,clausula_32),
             total_a_indemnizar = derechos_laborales + prestaciones,
             garantia_para_banrural = prestaciones + sueldo*1.25) %>% 
      select(no_personal,nombre,id,aguinaldo_proporcional,total_derechos_laborales,total_prestaciones,clausula_32,
             derechos_laborales,prestaciones,
             total_a_indemnizar,garantia_para_banrural,tipo_contrato,regimen) %>% 
      rename("Identidad" = id)
    
    
    
    DT::datatable(estimacion_deducciones,extensions = c('Buttons','ColReorder','KeyTable','Responsive'),options = list(dom = 'Bfrtip',
                                                                                                                       lengthMenu = list(c(10,15,25,-1), c("10","15","25","All")),
                                                                                                                       pageLength = -1,
                                                                                                                       colReorder = TRUE,keys = TRUE,pageLength=20,
                                                        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                                        initComplete = JS(
                                                          "function(settings, json) {",
                                                          "$(this.api().table().header()).css({'background-color': 'gray', 'color': '#fff'});",
                                                          "}"),
                                                        search = list(regex = TRUE, caseInsensitive = FALSE, search = ' '),
                                                        buttons = c("copy", "csv","excel","pdf","print"))) %>% 
      formatRound(4:11, 3)
    }
  })
  
  
 
  # 4.8 Estimacion prestaciones y derechos laborales ------------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$fidecomiso <- DT::renderDataTable({
    if(is.null(df_products_upload())){
      DT::datatable(df_products_upload())
    }
    else{
      
    salario_promedio <- df_products_upload() %>% 
      select(no_personal,cc_nomina,salario_promedio) %>%
      pivot_longer(!c(no_personal, cc_nomina), names_to = "tipo", values_to  = "monto") %>% 
      select(-tipo) %>% 
      group_by(no_personal,cc_nomina) %>%
      summarise(monto = sum(as.numeric(monto), na.rm = TRUE)) %>%
      ungroup() %>% 
      pivot_wider(names_from = cc_nomina, values_from=monto)
    
    salario_mensual <- salario_promedio %>% 
      mutate_if(is.numeric,~replace(.,is.na(.),0))
    
    prestaciones <- df_products_upload()[!duplicated(df_products_upload() %>% select(no_personal)),] %>% 
      select(no_personal, nombre,id,fecha_ingreso,denominacion_ci_colectivo)
    
    prestaciones <- prestaciones %>% left_join(salario_mensual,by = "no_personal")
    
    prestaciones <- as.data.frame(prestaciones)
    
    
    
    prestaciones <- prestaciones %>% select(no_personal,nombre,id,fecha_ingreso,denominacion_ci_colectivo,`9019`) %>% 
      rename("tipo_contrato" = denominacion_ci_colectivo,"salario_mensual" = `9019`) %>% 
      mutate(sueldo =  salario_mensual,
             fecha_ingreso = ymd(dmy(fecha_ingreso)),
             fecha_referencia = input$date1, #ymd(paste0(input$anio,"-",input$mes,"-",input$dias)),
             antiguedad = (360*(year(fecha_referencia)-year(fecha_ingreso))+
                             30*(month(fecha_referencia)-month(fecha_ingreso))+
                             (day(fecha_referencia) -day(fecha_ingreso)))/360, 
             anios_completos = floor(antiguedad),
             fraccion_anios = antiguedad - anios_completos,
             meses = floor(fraccion_anios*12),
             dias = round(fraccion_anios*360 - meses*30,1),
             fecha_ultimo_decimo_cuarto = input$date2,#ymd(paste0(input$anios2,"-",input$mes2,"-",input$dias2)),
             fecha_ultimo_aguinaldo = input$date3, ymd(paste0(input$anio1,"-",input$mes1,"-",input$dias1)),
             
             porcentaje_clapsula_32 = ifelse(tipo_contrato=="Permanente",ifelse(antiguedad<5.5 & antiguedad>=1,65,
                                                                                ifelse(antiguedad>=5.5 & antiguedad<10.5,70,
                                                                                       ifelse(antiguedad>=10.5 & antiguedad<15.5,75,
                                                                                              ifelse(antiguedad>15.5 & antiguedad<20.5,80,
                                                                                                     ifelse(antiguedad>=20.5 & antiguedad<23.5,90,
                                                                                                            ifelse(antiguedad>=23.5 & antiguedad<30.5,100,
                                                                                                                   ifelse(antiguedad>=30.5,120,0))))))),0)/100,
             porcentaje_bono_junio = ifelse(tipo_contrato == "Permanente",ifelse(antiguedad<1,0,
                                                                                 ifelse(antiguedad<10,0.125,
                                                                                        ifelse(antiguedad<20,0.175,0.225))),0),
             porcentaje_bono_vacaciones = ifelse(tipo_contrato == "Permanente",
                                                 ifelse(antiguedad>5,1.2,1),0),
             dias_bono_vacaciones = ifelse(ceiling(antiguedad)>4,30,ifelse(ceiling(antiguedad) == 1,12,
                                                                           ifelse(ceiling(antiguedad) == 2,15,
                                                                                  ifelse(ceiling(antiguedad) == 3,20,
                                                                                         ifelse(ceiling(antiguedad) == 4,25,0))))),
             SBM = sueldo,
             SOM = ifelse(tipo_contrato == "Permanente",(14+dias_bono_vacaciones/30*porcentaje_bono_vacaciones+porcentaje_bono_junio)/12*SBM,14/12*SBM),
             
             aguinaldo_proporcional = round((360*(year(fecha_referencia)-year(fecha_ultimo_aguinaldo)) - 30*(month(fecha_ultimo_aguinaldo)-month(fecha_referencia)))/360*SBM,2),
             decimo_cuarto_proporcional = round((360*(year(fecha_referencia)-year(fecha_ultimo_decimo_cuarto)) - 30*(month(fecha_ultimo_decimo_cuarto)-month(fecha_referencia)))/360*SBM,2),
             bono_vacaciones_proporcional = fraccion_anios*sueldo*dias_bono_vacaciones/30*porcentaje_bono_vacaciones,
             bono_junio_proporcional = sueldo*porcentaje_bono_junio,
             vaciones_venciadas = 0,
             total_derechos_laborales = aguinaldo_proporcional+decimo_cuarto_proporcional+bono_vacaciones_proporcional+bono_junio_proporcional,
             preaviso = ifelse(anios_completos<1, ifelse(meses<3,1,ifelse(meses>=3 & meses<6,7,14)),ifelse(anios_completos == 1 & meses==0 & dias==0,14,ifelse(anios_completos<2,30,60)))/30*SOM,
             auxilio_por_cesantia = round(pmin(25,anios_completos)*SOM,2),
             auxilio_por_cesantia_prop = ifelse(antiguedad>25,0,ifelse(antiguedad>1,fraccion_anios*30,ifelse(meses+dias/30<=3,0,ifelse(meses+dias/30<=6,10,ifelse(antiguedad<=1,20,30))))*SOM/30),
             total_prestaciones = preaviso + auxilio_por_cesantia + auxilio_por_cesantia_prop,
             clausula_32 = porcentaje_clapsula_32*antiguedad*sueldo,
             derechos_laborales = ifelse(aguinaldo_proporcional>0,aguinaldo_proporcional,0)+bono_vacaciones_proporcional+ifelse(decimo_cuarto_proporcional>0,decimo_cuarto_proporcional,0)+bono_junio_proporcional+vaciones_venciadas,
             prestaciones = pmax(total_prestaciones,clausula_32),
             total_a_indemnizar = derechos_laborales + prestaciones,
             garantia_para_banrural = prestaciones + sueldo*1.25) %>% 
      select(id,nombre,no_personal,tipo_contrato,fecha_ingreso,sueldo,garantia_para_banrural) %>% 
      rename('Identidad' = id,'Nombre' = nombre,'Codigo Empleado' = no_personal,
             'Tipo Contrato' = tipo_contrato,'Fecha Ingreso' = fecha_ingreso,'Salario'= sueldo,
             'Garantia Prestamo' = garantia_para_banrural)
    
    DT::datatable(prestaciones,extensions = c('Buttons','ColReorder','KeyTable','Responsive','Scroller'),options = list(dom = 'Bfrtip',
                                                                                                                        lengthMenu = list(c(10,15,25,-1), c("10","15","25","All")),
                                                                                                                        pageLength = -1,
                                                                                                                        colReorder = TRUE,keys = TRUE,pageLength=100,
                                              language = list(url = '//cdn.datatables.net/pl,ug-ins/1.10.11/i18n/Spanish.json'),
                                              initComplete = JS(
                                                "function(settings, json) {",
                                                "$(this.api().table().header()).css({'background-color': 'gray', 'color': '#fff'});",
                                                "}"),
                                              search = list(regex = TRUE, caseInsensitive = FALSE, search = ' '),
                                              buttons = c("copy", "csv","excel","pdf","print"))) %>% 
      formatRound(6:7, 3)
    }
  })
  
  
}




