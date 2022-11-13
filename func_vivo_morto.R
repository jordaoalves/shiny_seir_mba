func_vivo_morto <- function(pop_inicial, n_dias_infeccao){
  
  npop_inicial <- nrow(pop_inicial)
  
  # Filtra quem está infectado e aumenta mais 1 dia nesse estado
  cod_agente_I <- pop_inicial$cod_agente[pop_inicial$estado_agente == "I"]
  pop_inicial$tempo_I_agente[cod_agente_I] <- pop_inicial$tempo_I_agente[cod_agente_I] + 1
  
  # Filtra quem está infectado há mais de 15 dias e se sobreviveu por mais de n_dias_infeccao pode-se considerar recuperado
  cod_agente_I_R <- pop_inicial$cod_agente[pop_inicial$estado_agente == "I" & pop_inicial$tempo_I_agente > n_dias_infeccao] 
  pop_inicial$estado_agente[cod_agente_I_R] <- "R" 
  
  # Filtra quem está infectado há n_dias_infeccao dias ou menos e atribui aleatoriamente se vai morrer ou não
  
  pop_inicial$prob_viver <- rep(0, npop_inicial)
  
  pop_inicial$prob_viver[pop_inicial$estado_agente == "I" & pop_inicial$tempo_I_agente <= n_dias_infeccao] <- 
    runif(length(pop_inicial$prob_viver[pop_inicial$estado_agente == "I" & pop_inicial$tempo_I_agente <= n_dias_infeccao]),0,1)
  
  pop_inicial$estado_agente[pop_inicial$estado_agente == "I" & 
                              pop_inicial$tempo_I_agente <= n_dias_infeccao & 
                              pop_inicial$prob_viver <
                              pop_inicial$tx_mort] <- "D"
  
 return(pop_inicial)
  
}