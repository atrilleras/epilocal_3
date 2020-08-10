library (RCurl)
library(squire)
library(patchwork)
library(ggplot2)
library(dplyr)
library(epitrix)
library(readr)
library(incidence)
library(parsedate)
library(readxl)
library(EpiEstim)
library(openxlsx)
library(gridExtra)
rm(list=ls())

#Funciones
fCleanData <- function(datos){
    
    names(datos) <- epitrix::clean_labels(names(datos))
    
    datos$fecha_diagnostico <- as.Date(as.numeric(datos$fecha_diagnostico),
                                       origin = "1900-01-01")
    datos_inc <- datos %>%
        mutate(date = as.Date (fis),
               fecha_diagnostico=as.Date(fecha_diagnostico),
               type=case_when(tipo=="Importado"~"imported",
                              tipo=="Relacionado"~"local",
                              tipo=="En estudio"~"local"))
    
    datos_inc$fis_clean <- clean_labels(datos_inc$fis)
    datos_inc$asymptomatic <- 0
    datos_inc$asymptomatic[datos_inc$fis_clean ==  "asintomatico"] <- 1
    
    return(datos_inc)
    
}

fEpiEstim <- function(datos_inc, rezago = 14) {
    
    datos <- datos_inc %>% filter(asymptomatic == 0)
    
    
    fechaConfiable <- as.Date(max(datos$fecha_de_notificacion),origin = "1900-01-01") - rezago
    
    inc_multiple  <- incidence(datos$date, groups = datos$type)[1:(max(datos$date)-min(datos$date)- rezago)] 
    #inc_simple  <- incidence(datos$date)[1:(max(datos$date)-min(datos$date)- rezago)] 
    
    days <- seq_along(inc_multiple$dates)[2:(length(inc_multiple$dates)-7)]
    RTo  <- estimate_R(inc_multiple, method = "parametric_si",
                       config = make_config(list(
                           mean_si = 6.48, std_si = 3.83,
                           t_start=days, t_end=days+7)))
    
    data_rt <- RTo$R
    data_rt$Fecha_Inicio_Ventana <- min(inc_multiple$dates) + RTo$R$t_start 
    data_rt$Fecha_Fin_Ventana    <- min(inc_multiple$dates) + RTo$R$t_end 
    
    rt <- list (data_rt = data_rt,
                RTo = RTo,
                inc_multiple = inc_multiple,
                #inc_simple   = inc_simple,
                fechaConfiable = fechaConfiable,
                datos = datos,
                datos_inc = datos_inc)
    
    return(rt)
}
calibracion<-function(int,region="Choco",categorias= "transit_stations_percent_change_from_baseline"){
    
    movilidad<- int %>% filter(sub_region_1 == region)
    movilidad<- movilidad %>% select(date,categorias)
    movilidad <-  movilidad%>%rename(modalidad=categorias)
    movilidad <-movilidad%>%mutate(CATEGORIA=rank(modalidad,ties.method = "average"))
    movilidad<-movilidad%>%mutate(C=(CATEGORIA/nrow(movilidad)))
    movilidad <- movilidad %>%mutate(x.1 = seq_along(movilidad$date))
    movilidad <- movilidad %>%mutate(x= seq_along(movilidad$date))
    intervencion <- movilidad%>%select(x.1,x,date,C)
    intervencion$date <- as.Date(as.character(intervencion$date))
    
    return(intervencion)
}

fPlotRt <- function(rt){
    
    numeroEfectivo <- rt$data_rt
    datos <- rt$datos
    datos_inc <- rt$datos_inc
    
    
    fechaReporte <- Sys.Date()
    fechaCuarentena <- as.Date("2020-03-25")
    fechaApertura   <- as.Date("2020-04-27")
    fechaConfiable  <- rt$fechaConfiable
    fechaend <- Sys.Date() + 5
    date_start  <- as.Date("2020-02-28", origin = "1899-12-30")
    date_end   <-  as.Date(fechaend, origin = "1899-12-30")
    
    primer_caso_reportado <- min(datos$fecha_diagnostico, na.rm = T)
    
    size <- 15
    
    plot_rt <- ggplot(numeroEfectivo, aes(x = Fecha_Fin_Ventana, 
                                          y = `Mean(R)`)) + 
        geom_ribbon(aes(ymin = `Quantile.0.025(R)`, 
                        ymax = `Quantile.0.975(R)`), 
                    fill = 'darkgreen', alpha = 0.4) +
        geom_line(colour = 'steelblue', size = 0.75) +
        theme_bw(size) +
        cowplot::theme_minimal_grid() +
        scale_x_date(date_breaks = '1 week')+ #,limits=as.Date(c("2020-02-05",date_end))) +
        
        geom_hline(yintercept = 1, linetype="dashed", lwd = 0.8,
                   color = "red") +
        geom_vline(xintercept = fechaCuarentena, linetype="dashed", lwd = 0.8,
                   color = "orange") +
        geom_vline(xintercept = fechaApertura, linetype="dashed", lwd = 0.8,
                   color = "darkgreen") +
        geom_vline(xintercept = fechaConfiable, linetype="dashed", lwd = 0.8,
                   color = "darkblue") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        geom_point(aes(x = primer_caso_reportado, y = 2), 
                   size = 6, 
                   colour = '#DC143C') +
        xlab("") + ylab("R(t)") +
        coord_cartesian(ylim=c(0,5), xlim=c(date_start,date_end)) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) +
        ggtitle("Colombia") +
        labs (subtitle = paste0("( n=", NROW(datos_inc), ")")) 
    
    
    return(plot_rt)
    
}
##########Base de datos#########################################################
#getwd()
setwd( "C:/Users/trill/Desktop/Documentos_INS/epilocal")
link_data <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
datos <- read_csv("data/Casos_positivos_de_COVID-19_02_07_20.csv")
datos1 <- read_csv("data/Casos_positivos_de_COVID-19_02_07_20.csv")
poblaciones<-loadWorkbook("data/matriz_poblacion.xlsx")
pop_bogota <-read.xlsx(poblaciones,"pob_bogota") %>% as.data.frame()
pop_cali <-read.xlsx(poblaciones,"pob_cali") %>% as.data.frame()
pop_barranquilla <-read.xlsx(poblaciones,"pop_baranquilla") %>% as.data.frame()
pop_medellin <-read.xlsx(poblaciones,"pob_medellin") %>% as.data.frame()
names(datos) <- epitrix::clean_labels(names(datos))
base1 <- fCleanData(datos1)

download <- getURL(link_data)
int0 <- read.csv (text = download)

#Exploración de la movilidad

int <- int0 %>% filter(country_region == "Colombia")
int$sub_region_1 <- as.character(int$sub_region_1)
int$sub_region_1[int$sub_region_1 == "" ] <- "Colombia"


######## CALIBRACIÓN PARA COLOMBIA#################

# Get the population
colombia <- get_population("Colombia")
poblacion <- colombia$n
contact_matrix <- get_mixing_matrix("Colombia")

#Serie de fallecimientos
future::plan(future::multiprocess())

datos$fecha_de_muerte <- as.Date(datos$fecha_de_muerte)
datos$fis <- as.Date(datos$fis)
datos1 <- datos %>% 
    group_by(fis) %>% summarise(n = n())

datos1 <- datos1[c(1:(nrow(datos1)-14)),]
names(datos1) <- c("date","cases")

datos2 <- base1 %>% 
   group_by(fecha_de_muerte) %>% summarise(n = n())
datos2 <- datos2[c(1:(nrow(datos2)-14)),]
names(datos2)<- c("date","deaths")

incidencia <- full_join(datos2,datos1, by = 'date')
incidencia$deaths[is.na(incidencia$deaths)] <- 0
incidencia <- as.data.frame(incidencia)
#incidencia <- incidencia[!is.na(incidencia$date),]
incidencia <- incidencia %>%arrange(date)
incidencia <- incidencia %>%mutate(x = seq_along(incidencia$date))
incidencia <- incidencia %>% select(x,date,deaths,cases)
incidencia$date <- as.Date(incidencia$date)

##Movilidad
intervencion_rentail <-calibracion(int,region="Colombia",categorias= "retail_and_recreation_percent_change_from_baseline")
intervencion_grocery <-calibracion(int,region="Colombia",categorias= "grocery_and_pharmacy_percent_change_from_baseline")
intervencion_parks <-calibracion(int,region="Colombia",categorias= "parks_percent_change_from_baseline")
intervencion_transit <-calibracion(int,region="Colombia",categorias= "transit_stations_percent_change_from_baseline")
intervencion_workplaces <-calibracion(int,region="Colombia",categorias= "workplaces_percent_change_from_baseline")

##Primera categoría
int_unique1 <- squire:::interventions_unique(intervencion_rentail)

out1 <- calibrate(
    data = incidencia,
    R0_min = 0.6,
    R0_max = 5.2,
    R0_step = 0.1,
    first_start_date = "2020-01-28",
    last_start_date = "2020-02-25",
    day_step = 1,
    replicates = 5,
    n_particles = 20,
    R0_change = int_unique1$change,
    date_R0_change = int_unique1$dates_change,
    country = "Colombia"
)

categoria_1 <-plot(out1, particle_fit = TRUE)+coord_cartesian(ylim=c(0,120))+ggtitle("Cat_rentail")

##segunda categoría
int_unique2 <- squire:::interventions_unique(intervencion_grocery)

out2 <- calibrate(
    data = incidencia,
    R0_min = 0.8,
    R0_max =5.2 ,
    R0_step = 0.1,
    first_start_date = "2020-02-01",
    last_start_date = "2020-02-25",
    day_step = 1,
    replicates = 5,
    n_particles = 20,
    R0_change = int_unique2$change,
    date_R0_change = int_unique2$dates_change,
    country = "Colombia"
)

categoria_2 <-plot(out2, particle_fit = TRUE)+coord_cartesian(ylim=c(0,120))+
                                              ggtitle("Cat_grocery")

##tercera categoría
int_unique3 <- squire:::interventions_unique(intervencion_parks)

out3 <- calibrate(
    data = incidencia,
    R0_min = 0.6,
    R0_max = 5.2,
    R0_step = 0.1,
    first_start_date = "2020-02-01",
    last_start_date = "2020-02-25",
    day_step = 1,
    replicates = 5,
    n_particles = 20,
    R0_change = int_unique3$change,
    date_R0_change = int_unique3$dates_change,
    country = "Colombia"
)

categoria_3 <-plot(out3, particle_fit = TRUE)+coord_cartesian(ylim=c(0,120))+
                                               ggtitle("Cat_parks")

##Cuarta categoría
int_unique4 <- squire:::interventions_unique(intervencion_transit)

out4 <- calibrate(
    data = incidencia,
    R0_min = 0.6,
    R0_max = 4.5,
    R0_step = 0.1,
    first_start_date = "2020-02-01",
    last_start_date = "2020-02-25",
    day_step = 1,
    replicates = 5,
    n_particles = 20,
    R0_change = int_unique4$change,
    date_R0_change = int_unique4$dates_change,
    country = "Colombia"
)

categoria_4 <-plot(out4, particle_fit = TRUE)+coord_cartesian(ylim=c(0,120))+
                                                ggtitle("Cat_transit")

##quinta categoría
int_unique5 <- squire:::interventions_unique(intervencion_workplaces)

out5 <- calibrate(
    data = incidencia,
    R0_min = 0.5,
    R0_max = 4.5,
    R0_step = 0.1,
    first_start_date = "2020-02-01",
    last_start_date = "2020-02-25",
    day_step = 1,
    replicates = 5,
    n_particles = 20,
    R0_change = int_unique5$change,
    date_R0_change = int_unique5$dates_change,
    country = "Colombia"
)

categoria_5 <-plot(out5, particle_fit = TRUE)+coord_cartesian(ylim=c(0,120))+
                                               ggtitle("Cat_workplaces")

grid.arrange(categoria_1,categoria_2,categoria_3,categoria_4,categoria_5, ncol=2)

####### CALIBRACIÓN PARA BOGOTÁ #####################################

# Get the population
poblacion_bog <- pop_bogota$n
contact_matrix <- get_mixing_matrix("Colombia")
datos_bog <- datos %>% filter(departamento_o_distrito=='Bogotá D.C.')
datos_bog1 <-base1 %>% filter(departamento_o_distrito=='Bogotá D.C.')

#Serie de fallecimientos
future::plan(future::multiprocess())

datos_bog$fecha_de_muerte <- as.Date(datos_bog$fecha_de_muerte)
datos_bog$fis <- as.Date(datos_bog$fis)
datos1_bog <- datos_bog %>% 
    group_by(fis) %>% summarise(n = n())

datos1_bog <- datos1_bog[c(1:(nrow(datos1_bog)-14)),]
names(datos1_bog) <- c("date","cases")
datos2_bog <- datos_bog1 %>% 
    group_by(fecha_de_muerte) %>% summarise(n = n())
datos2_bog <- datos2_bog[c(1:(nrow(datos2_bog)-14)),]
names(datos2_bog)<- c("date","deaths")

incidencia_bog <- full_join(datos2_bog,datos1_bog, by = 'date')
incidencia_bog$deaths[is.na(incidencia_bog$deaths)] <- 0
incidencia_bog <- as.data.frame(incidencia_bog)
#incidencia <- incidencia[!is.na(incidencia$date),]
incidencia_bog <- incidencia_bog %>%arrange(date)
incidencia_bog <- incidencia_bog %>%mutate(x = seq_along(incidencia_bog$date))
incidencia_bog <- incidencia_bog %>% select(x,date,deaths,cases)
incidencia_bog$date <- as.Date(incidencia_bog$date)

##Movilidad
intervencion_rentail_B <-calibracion(int,region="Bogota",categorias= "retail_and_recreation_percent_change_from_baseline")
intervencion_grocery_B <-calibracion(int,region="Bogota",categorias= "grocery_and_pharmacy_percent_change_from_baseline")
intervencion_parks_B <-calibracion(int,region="Bogota",categorias= "parks_percent_change_from_baseline")
intervencion_transit_B <-calibracion(int,region="Bogota",categorias= "transit_stations_percent_change_from_baseline")
intervencion_workplaces_B <-calibracion(int,region="Bogota",categorias= "workplaces_percent_change_from_baseline")

##Primera categoría
int_unique1_B <- squire:::interventions_unique(intervencion_rentail_B)

out1_B <- calibrate(
    data = incidencia_bog,
    R0_min = 0.6,
    R0_max = 3.5,
    R0_step = 0.1,
    first_start_date = "2020-02-01",
    last_start_date = "2020-02-25",
    day_step = 1,
    replicates = 5,
    n_particles = 20,
    R0_change = int_unique1_B$change,
    date_R0_change = int_unique1_B$dates_change,
    population = poblacion_bog,
    baseline_contact_matrix=contact_matrix
)

categoria_1_B <-plot(out1_B, particle_fit = TRUE)+coord_cartesian(ylim=c(0,40))+ggtitle("Cat_rentail_B")

##segunda categoría
int_unique2_B <- squire:::interventions_unique(intervencion_grocery_B)

out2_B <- calibrate(
    data = incidencia_bog,
    R0_min = 0.6,
    R0_max = 3.5,
    R0_step = 0.1,
    first_start_date = "2020-02-01",
    last_start_date = "2020-02-25",
    day_step = 1,
    replicates = 5,
    n_particles = 20,
    R0_change = int_unique2_B$change,
    date_R0_change = int_unique2_B$dates_change,
    population = poblacion_bog,
    baseline_contact_matrix=contact_matrix
)

categoria_2_B <-plot(out2_B, particle_fit = TRUE)+coord_cartesian(ylim=c(0,40))+
    ggtitle("Cat_grocery_B")

##tercera categoría
int_unique3_B <- squire:::interventions_unique(intervencion_parks_B)

out3_B <- calibrate(
    data = incidencia_bog,
    R0_min = 0.6,
    R0_max = 3.5,
    R0_step = 0.1,
    first_start_date = "2020-02-01",
    last_start_date = "2020-02-25",
    day_step = 1,
    replicates = 5,
    n_particles = 20,
    R0_change = int_unique3_B$change,
    date_R0_change = int_unique3_B$dates_change,
    population = poblacion_bog,
    baseline_contact_matrix=contact_matrix
)

categoria_3_B <-plot(out3_B, particle_fit = TRUE)+coord_cartesian(ylim=c(0,40))+
    ggtitle("Cat_parks_B")

##Cuarta categoría
int_unique4_B <- squire:::interventions_unique(intervencion_transit_B)

out4_B <- calibrate(
    data = incidencia_bog,
    R0_min = 0.6,
    R0_max = 3.5,
    R0_step = 0.1,
    first_start_date = "2020-02-01",
    last_start_date = "2020-02-25",
    day_step = 1,
    replicates = 5,
    n_particles = 20,
    R0_change = int_unique4_B$change,
    date_R0_change = int_unique4_B$dates_change,
    population = poblacion_bog,
    baseline_contact_matrix=contact_matrix
)

categoria_4_B <-plot(out4_B, particle_fit = TRUE)+coord_cartesian(ylim=c(0,40))+
    ggtitle("Cat_transit_B")

##quinta categoría
int_unique5_B <- squire:::interventions_unique(intervencion_workplaces_B)

out5_B <- calibrate(
    data = incidencia_bog,
    R0_min = 0.6,
    R0_max = 3.5,
    R0_step = 0.1,
    first_start_date = "2020-02-01",
    last_start_date = "2020-02-25",
    day_step = 1,
    replicates = 5,
    n_particles = 20,
    R0_change = int_unique5_B$change,
    date_R0_change = int_unique5_B$dates_change,
    population = poblacion_bog,
    baseline_contact_matrix=contact_matrix
)

categoria_5_B <-plot(out5_B, particle_fit = TRUE)+coord_cartesian(ylim=c(0,40))+
    ggtitle("Cat_workplaces_B")

grid.arrange(categoria_1_B,categoria_2_B,categoria_3_B,categoria_4_B,categoria_5_B, ncol=2)


####### CALIBRACIÓN PARA Barranquilla #####################################

# Get the population
    poblacion_barr <- pop_barranquilla$n
    contact_matrix <- get_mixing_matrix("Colombia")
    datos_barr <- datos %>% filter(departamento_o_distrito=='Barranquilla D.E.')
    datos_barr1 <-base1 %>% filter(departamento_o_distrito=='Barranquilla D.E.')
    
    #Serie de fallecimientos
    future::plan(future::multiprocess())
    
    datos_barr$fecha_de_muerte <- as.Date(datos_barr$fecha_de_muerte)
    datos_barr$fis <- as.Date(datos_barr$fis)
    datos1_barr <- datos_barr %>% 
        group_by(fis) %>% summarise(n = n())
    
    datos1_barr <- datos1_barr[c(1:(nrow(datos1_barr)-14)),]
    names(datos1_barr) <- c("date","cases")
    datos2_barr <- datos_barr1 %>% 
        group_by(fecha_de_muerte) %>% summarise(n = n())
    datos2_barr <- datos2_barr[c(1:(nrow(datos2_barr)-14)),]
    names(datos2_barr)<- c("date","deaths")
    
    incidencia_barr <- full_join(datos2_barr,datos1_barr, by = 'date')
    incidencia_barr$deaths[is.na(incidencia_barr$deaths)] <- 0
    incidencia_barr <- as.data.frame(incidencia_barr)
    #incidencia <- incidencia[!is.na(incidencia$date),]
    incidencia_barr <- incidencia_barr %>%arrange(date)
    incidencia_barr <- incidencia_barr %>%mutate(x = seq_along(incidencia_barr$date))
    incidencia_barr <- incidencia_barr %>% select(x,date,deaths,cases)
    incidencia_barr$date <- as.Date(incidencia_barr$date)
    
    ##Movilidad
    intervencion_rentail_BARR <-calibracion(int,region="Atlantico",categorias= "retail_and_recreation_percent_change_from_baseline")
    intervencion_grocery_BARR <-calibracion(int,region="Atlantico",categorias= "grocery_and_pharmacy_percent_change_from_baseline")
    intervencion_parks_BARR <-calibracion(int,region="Atlantico",categorias= "parks_percent_change_from_baseline")
    intervencion_transit_BARR <-calibracion(int,region="Atlantico",categorias= "transit_stations_percent_change_from_baseline")
    intervencion_workplaces_BARR <-calibracion(int,region="Atlantico",categorias= "workplaces_percent_change_from_baseline")
    
    ##Primera categoría
    int_unique1_BARR <- squire:::interventions_unique(intervencion_rentail_BARR)
    
    out1_BARR <- calibrate(
        data = incidencia_barr,
        R0_min = 0.6,
        R0_max = 3.8,
        R0_step = 0.1,
        first_start_date = "2020-02-01",
        last_start_date = "2020-02-25",
        day_step = 1,
        replicates = 5,
        n_particles = 20,
        R0_change = int_unique1_BARR$change,
        date_R0_change = int_unique1_BARR$dates_change,
        population = poblacion_barr,
        baseline_contact_matrix=contact_matrix
    )
    
    categoria_1_BARR <-plot(out1_BARR, particle_fit = TRUE)+coord_cartesian(ylim=c(0,40))+ggtitle("Cat_rentail_BARR")
    
    ##segunda categoría
    int_unique2_BARR <- squire:::interventions_unique(intervencion_grocery_BARR)
    
    out2_BARR <- calibrate(
        data = incidencia_barr,
        R0_min = 0.6,
        R0_max = 3.8,
        R0_step = 0.1,
        first_start_date = "2020-02-01",
        last_start_date = "2020-02-25",
        day_step = 1,
        replicates = 5,
        n_particles = 20,
        R0_change = int_unique2_BARR$change,
        date_R0_change = int_unique2_BARR$dates_change,
        population = poblacion_barr,
        baseline_contact_matrix=contact_matrix
    )
    
    categoria_2_BARR <-plot(out2_BARR, particle_fit = TRUE)+coord_cartesian(ylim=c(0,40))+
        ggtitle("Cat_grocery_BARR")
    
    ##tercera categoría
    int_unique3_BARR <- squire:::interventions_unique(intervencion_parks_BARR)
    
    out3_BARR <- calibrate(
        data = incidencia_barr,
        R0_min = 0.6,
        R0_max = 3.8,
        R0_step = 0.1,
        first_start_date = "2020-02-01",
        last_start_date = "2020-02-25",
        day_step = 1,
        replicates = 5,
        n_particles = 20,
        R0_change = int_unique3_BARR$change,
        date_R0_change = int_unique3_BARR$dates_change,
        population = poblacion_barr,
        baseline_contact_matrix=contact_matrix
    )
    
    categoria_3_BARR <-plot(out3_BARR, particle_fit = TRUE)+coord_cartesian(ylim=c(0,60))+
        ggtitle("Cat_parks_BARR")
    
    ##Cuarta categoría
    int_unique4_BARR <- squire:::interventions_unique(intervencion_transit_BARR)
    
    out4_BARR <- calibrate(
        data = incidencia_barr,
        R0_min = 0.6,
        R0_max = 3.8,
        R0_step = 0.1,
        first_start_date = "2020-02-01",
        last_start_date = "2020-02-25",
        day_step = 1,
        replicates = 5,
        n_particles = 20,
        R0_change = int_unique4_BARR$change,
        date_R0_change = int_unique4_BARR$dates_change,
        population = poblacion_barr,
        baseline_contact_matrix=contact_matrix
    )
    
    categoria_4_BARR <-plot(out4_BARR, particle_fit = TRUE)+coord_cartesian(ylim=c(0,40))+
        ggtitle("Cat_transit_BARR")
    
    ##quinta categoría
    int_unique5_BARR <- squire:::interventions_unique(intervencion_workplaces_BARR)
    
    out5_BARR <- calibrate(
        data = incidencia_barr,
        R0_min = 0.6,
        R0_max = 3.8,
        R0_step = 0.1,
        first_start_date = "2020-02-01",
        last_start_date = "2020-02-25",
        day_step = 1,
        replicates = 5,
        n_particles = 20,
        R0_change = int_unique5_BARR$change,
        date_R0_change = int_unique5_BARR$dates_change,
        population = poblacion_barr,
        baseline_contact_matrix=contact_matrix
    )
    
    categoria_5_BARR <-plot(out5_BARR, particle_fit = TRUE)+coord_cartesian(ylim=c(0,40))+
        ggtitle("Cat_workplaces_BARR")
    
    grid.arrange(categoria_1_BARR,categoria_2_BARR,categoria_3_BARR,categoria_4_BARR,categoria_5_BARR, ncol=2)


####### CALIBRACIÓN PARA CALI #####################################

# Get the population
poblacion_cali <- pop_cali$n
contact_matrix <- get_mixing_matrix("Colombia")
datos_cali <- datos %>% filter(ciudad_de_ubicacion=='Cali')
datos_cali1 <-base1 %>% filter(ciudad_de_ubicacion=='Cali')

#Serie de fallecimientos
future::plan(future::multiprocess())

datos_cali$fecha_de_muerte <- as.Date(datos_cali$fecha_de_muerte)
datos_cali$fis <- as.Date(datos_cali$fis)
datos1_cali <- datos_cali %>% 
    group_by(fis) %>% summarise(n = n())

datos1_cali <- datos1_cali[c(1:(nrow(datos1_cali)-14)),]
names(datos1_cali) <- c("date","cases")
datos2_cali <- datos_cali1 %>% 
    group_by(fecha_de_muerte) %>% summarise(n = n())
datos2_cali <- datos2_cali[c(1:(nrow(datos2_cali)-14)),]
names(datos2_cali)<- c("date","deaths")

incidencia_cali <- full_join(datos2_cali,datos1_cali, by = 'date')
incidencia_cali$deaths[is.na(incidencia_cali$deaths)] <- 0
incidencia_cali <- as.data.frame(incidencia_cali)
#incidencia <- incidencia[!is.na(incidencia$date),]
incidencia_cali <- incidencia_cali %>%arrange(date)
incidencia_cali <- incidencia_cali %>%mutate(x = seq_along(incidencia_cali$date))
incidencia_cali <- incidencia_cali %>% select(x,date,deaths,cases)
incidencia_cali$date <- as.Date(incidencia_cali$date)

##Movilidad
intervencion_rentail_CALI <-calibracion(int,region="Valle del Cauca",categorias= "retail_and_recreation_percent_change_from_baseline")
intervencion_grocery_CALI <-calibracion(int,region="Valle del Cauca",categorias= "grocery_and_pharmacy_percent_change_from_baseline")
intervencion_parks_CALI <-calibracion(int,region="Valle del Cauca",categorias= "parks_percent_change_from_baseline")
intervencion_transit_CALI <-calibracion(int,region="Valle del Cauca",categorias= "transit_stations_percent_change_from_baseline")
intervencion_workplaces_CALI <-calibracion(int,region="Valle del Cauca",categorias= "workplaces_percent_change_from_baseline")

##Primera categoría
int_unique1_CALI <- squire:::interventions_unique(intervencion_rentail_CALI)

out1_CALI <- calibrate(
    data = incidencia_cali,
    R0_min = 0.8,
    R0_max = 5.3,
    R0_step = 0.1,
    first_start_date = "2020-02-01",
    last_start_date = "2020-02-25",
    day_step = 1,
    replicates = 5,
    n_particles = 20,
    R0_change = int_unique1_CALI$change,
    date_R0_change = int_unique1_CALI$dates_change,
    population = poblacion_cali,
    baseline_contact_matrix=contact_matrix
)

categoria_1_CALI <-plot(out1_CALI, particle_fit = TRUE)+coord_cartesian(ylim=c(0,40))+ggtitle("Cat_rentail_CALI")

##segunda categoría
int_unique2_CALI <- squire:::interventions_unique(intervencion_grocery_CALI)

out2_CALI <- calibrate(
    data = incidencia_cali,
    R0_min = 0.8,
    R0_max = 5-3,
    R0_step = 0.1,
    first_start_date = "2020-02-01",
    last_start_date = "2020-02-25",
    day_step = 1,
    replicates = 5,
    n_particles = 20,
    R0_change = int_unique2_CALI$change,
    date_R0_change = int_unique2_CALI$dates_change,
    population = poblacion_cali,
    baseline_contact_matrix=contact_matrix
)

categoria_2_CALI <-plot(out2_CALI, particle_fit = TRUE)+coord_cartesian(ylim=c(0,40))+
    ggtitle("Cat_grocery_CALI")

##tercera categoría
int_unique3_CALI <- squire:::interventions_unique(intervencion_parks_CALI)

out3_CALI <- calibrate(
    data = incidencia_cali,
    R0_min = 0.8,
    R0_max = 5.3,
    R0_step = 0.1,
    first_start_date = "2020-02-01",
    last_start_date = "2020-02-25",
    day_step = 1,
    replicates = 5,
    n_particles = 20,
    R0_change = int_unique3_CALI$change,
    date_R0_change = int_unique3_CALI$dates_change,
    population = poblacion_cali,
    baseline_contact_matrix=contact_matrix
)

categoria_3_CALI <-plot(out3_CALI, particle_fit = TRUE)+coord_cartesian(ylim=c(0,40))+
    ggtitle("Cat_parks_CALI")

##Cuarta categoría
int_unique4_CALI <- squire:::interventions_unique(intervencion_transit_CALI)

out4_CALI <- calibrate(
    data = incidencia_cali,
    R0_min = 0.8,
    R0_max = 5.3,
    R0_step = 0.1,
    first_start_date = "2020-02-01",
    last_start_date = "2020-02-25",
    day_step = 1,
    replicates = 5,
    n_particles = 20,
    R0_change = int_unique4_CALI$change,
    date_R0_change = int_unique4_CALI$dates_change,
    population = poblacion_cali,
    baseline_contact_matrix=contact_matrix
)

categoria_4_CALI <-plot(out4_CALI, particle_fit = TRUE)+coord_cartesian(ylim=c(0,40))+
    ggtitle("Cat_transit_CALI")

##quinta categoría
int_unique5_CALI <- squire:::interventions_unique(intervencion_workplaces_CALI)

out5_CALI <- calibrate(
    data = incidencia_cali,
    R0_min = 0.8,
    R0_max = 5.3,
    R0_step = 0.1,
    first_start_date = "2020-02-01",
    last_start_date = "2020-02-25",
    day_step = 1,
    replicates = 5,
    n_particles = 20,
    R0_change = int_unique5_CALI$change,
    date_R0_change = int_unique5_CALI$dates_change,
    population = poblacion_cali,
    baseline_contact_matrix=contact_matrix
)

categoria_5_CALI <-plot(out5_CALI, particle_fit = TRUE)+coord_cartesian(ylim=c(0,40))+
    ggtitle("Cat_workplaces_CALI")

grid.arrange(categoria_1_CALI,categoria_2_CALI,categoria_3_CALI,categoria_4_CALI,categoria_5_CALI, ncol=2)

