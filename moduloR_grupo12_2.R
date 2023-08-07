#Leemos las librerias: ----
library(openxlsx) #importardatos excel
library(magrittr) #pipe
library(dplyr) #filter, select, arange, mutate, etc.
library(tidyr) #para obtener tibble
library(tibble) #para crear un tibble
library(ggplot2)

#Cargamos los datos:----
data1 <- read.xlsx("Data/balances_2014.xlsx")

dim(data1)
  #47033 filas 347 columnas

data1 %>% head(10) %>% View()

# Tibble empresas: ----
empresas <- tibble::as_tibble(data1)

glimpse(empresas) 

#Seleccionamos las variables: ----
empresas <- empresas %>% select(nombre_cia, situacion, tipo, pais, provincia, canton, ciudad, ciiu4_nivel1,
                                tamanio,
                                trab_direc,
                                trab_admin,
                                ciiu4_nivel6, v345, v539, v599, v499, v698,v498)
                    #v379, v389, v439, v424, v445)
                    
#data_z <- empresas %>% select(v379, v389, v439, v424, v445)
#rowsum(data_z)


    #Fórmula A:                
    #v345 := TOTAL ACTIVO CORRIENTE (Activo Corriente) No corresponde al formulario.
    #v539 := TOTAL PASIVO CORRIENTE (Pasivo Corriente)
    
    #Fórmula B:
    #v599 := TOTAL DEL PASIVO (Pasivo)
    #v499 := TOTAL DEL ACTIVO (Activo)
     
    #Fómula C:
    #v698 := TOTAL PATRIMONIO NETO (Patrimonio)
                    
    #Fórmula D:
    #v498	:= TOTAL ACTIVOS NO CORRIENTES (Activo No Corriente) 
      #No hay en el formulario:
          #379 := TOTAL PROPIEDADES, PLANTA Y EQUIPO, PROPIEDADES DE INVERSIÓN Y ACTIVOS BIOLÓGICOS
          #389 := TOTAL ACTIVOS INTANGIBLES
          #429 := v439	TOTAL ACTIVOS FINANCIEROS NO CORRIENTES
          #431 := no existe en el glosario de variables, pero podemos usar
                  #v424	OTROS ACTIVOS FINANCIEROS NO CORRIENTES
                  #no muchas empresas tienen estos impuestos

          #432 := #v445	OTROS ACTIVOS NO CORRIENTES

    #Fórmula E:
      #(Activo/Patrimonio)
    
    
empresas <- empresas %>% mutate(Liquidez_corriente= v345/v539,
                    Endeudamiento_activo= v599/v499,
                    Endeudamiento_patrimonial = v599/v698,
                    Endeudamiento_activo_fijo = v698/v498,
                    Apalancamiento=v499/v698)

    
empresas <- empresas %>% select(-c(v345,v539,v599,v499,v698,v498))


#names(empresas)

    
#Renombrar variables ----
empresas <- empresas %>% rename("Empresas"="nombre_cia",
                    "Status"="situacion",
                    "Tipo_empresa"= "tipo",
                    "Actividad_economica"= "ciiu4_nivel1",
                    "Subactividad" = "ciiu4_nivel6") %>% 
             select_all(toupper)
#--------------------------------------------------------------------------------------------

  
#Tabla Empresas por Actividad Economica por Canton ----
empresas %>% group_by(ACTIVIDAD_ECONOMICA,CANTON) %>% 
             summarise(TOTAL_EMPRESAS=n()) %>% View()
#empresas %>% View()


#Limpieza de "NaN" e "inf" : ----
empresas <- empresas %>% mutate_if(is.numeric, ~replace(., is.nan(.), 0)) %>%
             mutate_if(is.numeric, ~replace(., is.infinite(.), NA))
  #inf por NA 
  #NaN por 0 
  
#empresas %>% View()


#STATUS : ----
#Vemos los STATUS más representativos de las empresas:
  #ACTIVO
  #DISOLUCION
  #OTROS: LIQUIDACION, INACTIVA, CANCELACION
empresas %>% select(STATUS,TAMANIO) %>% group_by(STATUS,TAMANIO) %>%
             summarize(total_empresas=n()) %>% 
             pivot_wider(names_from=TAMANIO,values_from=total_empresas) %>% View()


empresas <- empresas %>% mutate(STATUS_2 = 
                                  if_else(STATUS %in% c("DISOLUC. LIQUIDAC. OFICIO INSC. EN RM",
                                                        "DISOLUC. LIQUIDAC. OFICIO NO INSC. EN RM",
                                                        "DISOLUC. Y LIQUIDAC. ANTIC. INSCR. RM",
                                                        "DISOLUC. Y LIQUIDAC. ANTIC. NO INSC. EN RM"), "DISOLUCION",
                                  if_else(STATUS == "ACTIVA", "ACTIVA","OTROS")))

#Pivoteamos la base:  ----
empresas <-  empresas %>%
  gather(key = "Indicador",
         value = "Valor", 
         LIQUIDEZ_CORRIENTE, ENDEUDAMIENTO_ACTIVO, ENDEUDAMIENTO_PATRIMONIAL, ENDEUDAMIENTO_ACTIVO_FIJO, APALANCAMIENTO)

#View(empresas)


#Tamaño por empresas: ----
  #pequeña
  #micro
  #mediana
  #grande

empresas_pequenia <- empresas %>% filter(TAMANIO=="PEQUEÑA")
empresas_micro <- empresas %>% filter(TAMANIO=="MICRO")
empresas_mediana <- empresas %>% filter(TAMANIO=="MEDIANA")
empresas_grande <- empresas %>% filter(TAMANIO=="GRANDE")


#Imputación de datos:----
  #Como hay muchos NAs, imputaremos los datos de acuerdo al tamaño de la empresa, reemplazando con el promedio.

empresas_pequenia_imp <- empresas_pequenia %>% mutate_if(is.numeric, ~replace(.,is.na(.),mean(.,na.rm = TRUE)))
empresas_micro_imp <- empresas_micro %>% mutate_if(is.numeric, ~replace(.,is.na(.),mean(.,na.rm = TRUE)))
empresas_mediana_imp <- empresas_mediana %>% mutate_if(is.numeric, ~replace(.,is.na(.),mean(.,na.rm = TRUE)))
empresas_grande_imp <- empresas_grande %>% mutate_if(is.numeric, ~replace(.,is.na(.),mean(.,na.rm = TRUE)))

empresas <- empresas_pequenia_imp %>% 
  bind_rows(empresas_micro_imp) %>% 
  bind_rows(empresas_mediana_imp) %>%
  bind_rows(empresas_grande_imp) 

#empresas_grande_imp %>% View()

#Grafica: liquidez, solvencia por Status y Provincia: ----


# Unificando múltiples columnas en una sola variable


##Creando el gráfico de barras para las empresas pequeñas
ggplot(empresas_pequenia_imp, aes(x = Valor, y = PROVINCIA, fill = Indicador)) +
  geom_bar(stat = "identity") +
  labs(title = "Indicadores financieros Empresas Pequeñas",
       x = "Provincia",
       y = "Valor",
       fill = "Indicador") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  facet_wrap("STATUS_2")


##Creando el gráfico de barras para las empresas micro
ggplot(empresas_micro_imp, aes(x = Valor, y = PROVINCIA, fill = Indicador)) +
  geom_bar(stat = "identity") +
  labs(title = "Indicadores financieros Empresas Micro",
       x = "Provincia",
       y = "Valor",
       fill = "Indicador") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  facet_wrap("STATUS_2")


##Creando el gráfico de barras para las empresas mediana
ggplot(empresas_mediana_imp, aes(x = Valor, y = PROVINCIA, fill = Indicador)) +
  geom_bar(stat = "identity") +
  labs(title = "Indicadores financieros Empresas Mediana",
       x = "Provincia",
       y = "Valor",
       fill = "Indicador") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  facet_wrap("STATUS_2")


##Creando el gráfico de barras para las empresas grande
ggplot(empresas_grande_imp, aes(x = Valor, y = PROVINCIA, fill = Indicador)) +
  geom_bar(stat = "identity") +
  labs(title = "Indicadores financieros Empresas Grandes",
       x = "Provincia",
       y = "Valor",
       fill = "Indicador") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  facet_wrap("STATUS_2")


#4. Gráficamente muestra el comparativo de los indicadores financieros de liquidez 
#y solvencia por tipo de empresa.

  
##Creando el gráfico de barras por tipo de empresa
ggplot(empresas, aes(x = Valor, y = TIPO_EMPRESA, fill = Indicador)) +
  geom_bar(stat = "identity") +
  labs(title = "Indicadores Financieros por Tipo de Empresa",
       x = "Valor",
       y = "Tipo de Empresa",
       fill = "Indicador") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


 
#Pregunta 1: ----
#¿El endeudamiento del activo fue mayor en empresas micro + pequeñas vs. grandes?

empresas_endeu_act <- empresas %>% mutate(TAMANIO_2=
              if_else(TAMANIO %in% c("PEQUEÑA","MICRO"),"MICRO+PEQUE",
                      if_else(TAMANIO == "GRANDE", "GRANDE","OTROS"))) %>% 
  filter(Indicador=="ENDEUDAMIENTO_ACTIVO", TAMANIO_2 %in% c("GRANDE","MICRO+PEQUE"))


ggplot(empresas_endeu_act , aes(x = TAMANIO_2, y = Valor)) +
  geom_point() +
  labs(title = "Endeudamiento del Activo: Micro + Peque vs Grande ",
       x = "Tamaño",
       y = "Endeudamiento del activo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")


#Pregunta 2: ----
#¿La liquidez por tipo de compañía es diferente entre aquellas empresas que tienen más de
#60 trabajadores directos(directivos) y que cuenta 
#con 100 a 800 trabajadores administrativos?



empresas_trab_0<- empresas %>% 
                  mutate(TRAB_DIREC_2= if_else(TRAB_DIREC>60,1,0),
                         TRAB_ADMIN_2=if_else(TRAB_ADMIN>=100&TRAB_ADMIN<=800,1,0),
                         TRABAJADORES=if_else(TRAB_DIREC_2==1&TRAB_ADMIN_2==1,1,0)) %>% 
                  filter(Indicador=="LIQUIDEZ_CORRIENTE") 

#View(empresas_trab_0)


ggplot(empresas_trab_0 , aes(x = Valor , y = TIPO_EMPRESA)) +
  geom_point() +
  labs(title = "Liquidez por Tipo de Compañía",
       x = "Liquidez corriente",
       y = "Tipo de compañía") +
  facet_wrap("TRABAJADORES")+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())



#Pregunta 3: ----
#Describe el top 10 de empresas con mayor apalancamiento
empresas %>% filter(Indicador=="APALANCAMIENTO") %>% 
    arrange(desc(Valor)) %>% 
  head(10) %>% View()

