poblacion$juri = as.numeric(poblacion$juri)
poblacion$sexo_codigo = as.numeric(poblacion$sexo_codigo)

poblacion_standard = poblacion$poblacion[poblacion$ano=="2020" &
                                         poblacion$sexo_nombre=="Ambos sexos" &
                                         poblacion$juri=="1"]

tasas = data.frame()

for (a in unique(defunciones$ano)) {
  for (j in unique(defunciones$juri)) {
    for (s in unique(defunciones$sexo_codigo)) {
      browser()
      def_data = defunciones[defunciones$ano == a &
                             defunciones$juri == j &
                             defunciones$sexo_codigo == 2 &
                             defunciones$edad!="06.Sin especificar",] %>% left_join(poblacion)
      
      def_data
      ano = first(def_data$ano)
      juri_codigo = first(def_data$juri)
      juri_nombre = first(def_data$juri_nombre)
      
      sexo = first(def_data$sexo_nombre)
      tasas = epitools::ageadjust.direct(
        count = def_data$cantidad,
        pop = def_data$poblacion,
        stdpop = poblacion_standard,
        conf.level = .95
        
      ) *100000
      
    }
  }  
}



