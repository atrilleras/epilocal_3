rm(list=ls())
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
library(squire)
library(gridExtra)
# Get the population
pop <- get_population("Colombia")
population <- pop$n

# Get the mixing matrix
contact_matrix <- get_mixing_matrix("Colombia")

# run the model
estimacion_1 <- run_explicit_SEEIR_model(population = population, 
                              contact_matrix_set = contact_matrix,
                              R0 = 2.28, 
                              time_period = 300,
                              dt = 1,
                              ICU_bed_capacity = 100000,
                              replicates = 5)

grafica1 <- plot(estimacion_1, var_select = "ICU_occupancy")


estimacion_2 <- run_explicit_SEEIR_model(population = population, 
                                         contact_matrix_set = contact_matrix,
                                         R0 = 1.5, 
                                         time_period = 200,
                                         dt = 0.1,
                                         ICU_bed_capacity = 2000,
                                         replicates = 5)

grafica2 <- plot(estimacion_2, var_select = "ICU_occupancy")

estimacion_3 <- run_explicit_SEEIR_model(population = population, 
                                         contact_matrix_set = contact_matrix,
                                         R0 = 1.1, 
                                         time_period = 200,
                                         dt = 0.1,
                                         ICU_bed_capacity = 2000,
                                         replicates = 5)

grafica3 <- plot(estimacion_3, var_select = "ICU_occupancy")

grid.arrange(grafica1,grafica2,grafica3)
