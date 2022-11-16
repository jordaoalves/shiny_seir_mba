modelo <- function( pop_inicial, 
                    diasSimulados = 1,
                    n_dias_infeccao = 15,
                    n_dias_incubacao = 3,
                    rodada = 1
){
  
  
  # Tamanho da população
  nPop <- nrow(pop_inicial)
  
  
  # Cria uma matrix com estados vazia para ser preenchida ao decorrer do tempo da simulação  
  dfSEIR <- data.frame( S = rep( 0, diasSimulados ),
                        E = rep( 0, diasSimulados),
                        I = rep( 0, diasSimulados),
                        R = rep( 0, diasSimulados),
                        D = rep( 0, diasSimulados),   
                        fI = rep( 0, diasSimulados)) 
  
  
  
  for(k in 1:diasSimulados){  
    
    
    cod_agente_S <- pop_inicial$cod_agente[pop_inicial$estado_agente == "S"] # gravando os Suscetíveis
    cod_agente_S_E <- pop_inicial$cod_agente[pop_inicial$estado_agente %in% c("S","E")] # gravando os Suscetíveis e Expostos
    prob_soc_agente_S_E <- pop_inicial$prob_socializacao_agente[pop_inicial$estado_agente %in% c("S","E")] # gravando os Suscetíveis e Expostos
    
    # encontro dos agentes entre si
    # para o encontro dos agentes eu considero que o infectado assim que descobre a doença deixa de interagir
    

    for(i in cod_agente_S){

      # Pegue os agentes que eles encontrarão
      cod_agente_encontro <- sample(cod_agente_S_E, 
                                    pop_inicial$n_agente_encontro[i], 
                                    replace = TRUE
                                    )
      
      n_agente_exposto <- length(pop_inicial$cod_agente[pop_inicial$cod_agente %in% 
                                                        cod_agente_encontro & 
                                                        pop_inicial$estado_agente == "E"])
      
      
      probs_n_expor <- ifelse(n_agente_exposto > 0,
                              runif(n_agente_exposto, 0,1),
                              1)

      pop_inicial$estado_agente[pop_inicial$cod_agente == i & 
                                min(probs_n_expor) <
                                pop_inicial$tx_exp] <- "E"
      
    }         
   
    
    # Atualizaçao de há quanto tempo estão expostos e suas mudanças de estado para infectados e recuperados.  
    
    # Pegue aqueles que foram expostos e aumente o tempo
    cod_agente_E <- pop_inicial$cod_agente[pop_inicial$estado_agente == "E"]
    pop_inicial$tempo_E_agente[cod_agente_E] <- pop_inicial$tempo_E_agente[cod_agente_E] + 1
    
    # Se alguem ficou exposto por mais de n_dias_infeccao, eu considero que ele foi um caso assintomático e mudo seu estado pra Recuperado
    cod_agente_E_R <- pop_inicial$cod_agente[pop_inicial$estado_agente == "E" & pop_inicial$tempo_E_agente > n_dias_infeccao]
    pop_inicial$estado_agente[cod_agente_E_R] <- "R"
    
    # Filtra quem está exposto há n_dias_incubacao ou mais e atribui aleatoriamente se vai se infectar ou não

    pop_inicial$prob_n_adoecer[pop_inicial$estado_agente == "E" & pop_inicial$tempo_E_agente > n_dias_incubacao] <- 
      runif(length(pop_inicial$prob_n_adoecer[pop_inicial$estado_agente == "E" & pop_inicial$tempo_E_agente > n_dias_incubacao]),0,1)
    
    pop_inicial$estado_agente[pop_inicial$estado_agente == "E" & 
                                pop_inicial$tempo_E_agente > n_dias_incubacao & 
                                pop_inicial$prob_n_adoecer <
                                pop_inicial$tx_inf] <- "I"
    
    
    # grava a informação se o indivíduo já foi infectado
    pop_inicial$foi_infectado[pop_inicial$estado_agente == "I"] <- "Y"
    
    
    # Atualizaçao de há quanto tempo estão infectados e suas mudanças de estado para recuperado ou morto
    
    pop_inicial = func_sobrevivencia(pop_inicial, n_dias_infeccao)
    
    
    # Pegue aqueles que foram recuperados e aumenta o tempo
    cod_agente_R <- pop_inicial$cod_agente[pop_inicial$estado_agente == "R"]
    pop_inicial$tempo_R_agente[cod_agente_R] <- pop_inicial$tempo_R_agente[cod_agente_R] + 1
    
    
    # Gerando matriz de estados durante o tempo  
    
    dfSEIR$S[k]   <- length(pop_inicial$estado_agente[pop_inicial$estado_agente == "S"])
    dfSEIR$E[k]   <- length(pop_inicial$estado_agente[pop_inicial$estado_agente == "E"]) 
    dfSEIR$I[k]   <- length(pop_inicial$estado_agente[pop_inicial$estado_agente == "I"])
    dfSEIR$R[k]   <- length(pop_inicial$estado_agente[pop_inicial$estado_agente == "R"])
    dfSEIR$D[k]   <- length(pop_inicial$estado_agente[pop_inicial$estado_agente == "D"])
    dfSEIR$fI[k]  <- length(pop_inicial$foi_infectado[pop_inicial$foi_infectado == "Y"])
    
  } 
  
  names(dfSEIR) <- c(paste0("S.", rodada),
                     paste0("E.", rodada),
                     paste0("I.", rodada),
                     paste0("R.", rodada),
                     paste0("D.", rodada),
                     paste0("fI.", rodada)
  )
  
  return(dfSEIR)
  
}