# --- Objetos necesarios para las funciones "Nombres_metricas_X", antes de 
#     su primer uso.




Nombres_metricas <- function(year, metClass, statGroup="Tend") {
    
    df_NomMetPhe     <- read.csv(paste0(dir_P, dir_pB, "Met_PhenoC_groups.csv"))
    ve_Set1          <- c("Set_1", "Set_2", "Set_3")
    ve_Set2          <- c("Sets_1_2", "Sets_1_3", "Sets_2_3")
    df_NomMetPhe$VAR <- str_replace(df_NomMetPhe$VAR, "YYYY",as.character(year))
    ve_Calidad       <- subset(df_NomMetPhe, Class_Metric=="Set_4")$VAR
    
    # sii una sóla Clase de Métrica ------------------
    if (metClass %in% ve_Set1) {
        if (metClass != "Set_3") {
            if (statGroup=="All") {
                ve_Nombres <- subset(df_NomMetPhe, Class_Metric==metClass)$VAR
            } else {
                ve_Nombres <- subset(df_NomMetPhe, Class_Metric==metClass & 
                                         Statistical_group==statGroup)$VAR
            }
        } else {
            ve_Nombres <- subset(df_NomMetPhe, Class_Metric==metClass)$VAR
        }
        ve_Nombres <- c(ve_Nombres, ve_Calidad)
    } else {
        
        # sii dos Clases de Métricas -----------------
        if (metClass %in% ve_Set2) {
            
            ch_6 <- substr(metClass,  6, 6)
            ch_8 <- substr(metClass,  8, 8)
            
            if (statGroup[1]=="All") {
                ve_Nombres1 <- subset(df_NomMetPhe, 
                                      Class_Metric==paste0("Set_", ch_6))$VAR
            } else {
                ve_Nombres1 <- subset(df_NomMetPhe, 
                                      Class_Metric==paste0("Set_", ch_6) & 
                                      Statistical_group==statGroup[1])$VAR
            }
            if (ch_8=="2") {
                if (statGroup[2]=="All") {
                    ve_Nombres2 <- subset(df_NomMetPhe, 
                                          Class_Metric=="Set_2")$VAR
                } else {
                    ve_Nombres2 <- subset(df_NomMetPhe, 
                                          Class_Metric=="Set_2" & 
                                          Statistical_group==statGroup[2])$VAR
                }
            } else {
                ve_Nombres2 <- subset(df_NomMetPhe, 
                                      Class_Metric=="Set_3")$VAR
            }
            ve_Nombres <- c(ve_Nombres1, ve_Nombres2, ve_Calidad)
        } else {
            
            # sii las tres Clases de Métricas --------
            if (metClass =="Sets_All") {
                if (statGroup[1]=="All") {
                    ve_Nombres1 <- subset(df_NomMetPhe, 
                                          Class_Metric=="Set_1")$VAR
                } else {
                    ve_Nombres1 <- subset(df_NomMetPhe, 
                                          Class_Metric=="Set_1" & 
                                          Statistical_group==statGroup[1])$VAR
                }
                
                if (statGroup[2]=="All") {
                    ve_Nombres2 <- subset(df_NomMetPhe, 
                                          Class_Metric=="Set_2")$VAR
                } else {
                    ve_Nombres2 <- subset(df_NomMetPhe, 
                                          Class_Metric=="Set_2" & 
                                          Statistical_group==statGroup[2])$VAR
                }
                
                ve_Nombres3 <- subset(df_NomMetPhe, 
                                      Class_Metric=="Set_3")$VAR
                
                ve_Nombres <- c(ve_Nombres1,ve_Nombres2,ve_Nombres3,ve_Calidad)
                
            } else {
                print("ERROR en la especifición de la Clase de Métrica fenológica que se utilizará.")
                ve_Nombres <- NA
            }
        }
    }
    #---
    return(ve_Nombres)
}
# Nombres_metricas("2018", "Sets_1_2", c("Tend", "Disp"))
# Nombres_metricas("2019", "Sets_1_3", "Disp")
# Nombres_metricas("2020", "Sets_All", c("Disp", "Disp"))
# Nombres_metricas("2021", "Set_2", "Tend")
# Nombres_metricas("2022", "Set_3")



Nombres_metricas_LO <- function(year, nombre, Cant=22) {

    df_NomMetPhe     <- read.csv(paste0(dir_P, dir_pB, "Met_PhenoC_groups.csv"))
    df_NomMetPhe$VAR <- str_replace(df_NomMetPhe$VAR, "YYYY", as.character(year))
    ve_Calidad       <- subset(df_NomMetPhe, Class_Metric=="Set_4")$VAR
    ve_CorrVar       <- c("LST", "RN", "S2N")
    
    if (nombre %in% ve_CorrVar) {
        if (Cant=="All") {
            ve_NombrLO <- subset(df_NomMetPhe, CorrespVAR==nombre)$VAR
        } else {
            ve_NombrLO <- subset(df_NomMetPhe, CorrespVAR==nombre &
                                               Statistical_group==Cant)$VAR
        }
    } else {
        nom_Col    <- ifelse(Cant==10, "GLO_10_3", "GLO_22_3")
        ve_NombrLO <- subset(df_NomMetPhe, df_NomMetPhe[[nom_Col]]==nombre)$VAR
    }
    ve_Nombres <- c(ve_NombrLO, ve_Calidad)
    #---
    return(ve_Nombres)
}

# Nombres_metricas_LO("2018", "av75max", 10)
# Nombres_metricas_LO("2019", "av75max", 22)
# Nombres_metricas_LO("2020", "RNph", 10)
# Nombres_metricas_LO("2021", "RNph", 22)
# -----------
# Nombres_metricas_LO("2021", "LST", "Tend")
# Nombres_metricas_LO("2021", "RN",  "Disp")
# Nombres_metricas_LO("2021", "S2N", "All")
