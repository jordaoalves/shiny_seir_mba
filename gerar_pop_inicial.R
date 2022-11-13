gerar_pop_inicial <- 
  function(
    nPop = 1000, 
    percJovem = 0.25,
    percAdulto = 0.60,
    percIdoso = 0.15,
    nExp = 5, 
    nInfec = 2,
    nMaxEncAgt = 5,
    
    n_dias_infeccao = 15,
    n_dias_incubacao = 3,
    
    tx_exp_jovem = 0.25, 
    tx_inf_jovem = 0.05,  
    tx_mort_jovem = 0.01,
    
    tx_exp_adulta = 0.30, 
    tx_inf_adulta = 0.07,  
    tx_mort_adulta = 0.03,
    
    tx_exp_idosa = 0.45, 
    tx_inf_idosa = 0.1,  
    tx_mort_idosa = 0.05
    
  ){
    
    
    # Atribuindo o perfil etário dos agentes a característica da pop --------
    
    
    perfil_agente <- sample(c("Jovem", "Adulto", "Idoso"), 
                            size = nPop, 
                            replace = TRUE, 
                            prob = c(percJovem, percAdulto, percIdoso)
    )
    
    # Atribuindo a taxa de exposição de acordo com o perfil etário dos agentes --------
    
    tx_exp <- perfil_agente
    tx_exp[tx_exp == "Jovem"] = tx_exp_jovem
    tx_exp[tx_exp == "Adulto"] = tx_exp_adulta
    tx_exp[tx_exp == "Idoso"] = tx_exp_idosa
    
    
    # Atribuindo a taxa de infecção de acordo com o perfil etário dos agentes --------
    
    tx_inf <- perfil_agente
    tx_inf[tx_inf == "Jovem"] = tx_inf_jovem
    tx_inf[tx_inf == "Adulto"] = tx_inf_adulta
    tx_inf[tx_inf == "Idoso"] = tx_inf_idosa
    
    # Atribuindo a taxa de mortalidade de acordo com o perfil etário dos agentes --------
    
    tx_mort <- perfil_agente
    tx_mort[tx_mort == "Jovem"] = tx_mort_jovem
    tx_mort[tx_mort == "Adulto"] = tx_mort_adulta
    tx_mort[tx_mort == "Idoso"] = tx_mort_idosa
    
    
    # Gerando população inicial toda suscetível -------------------------
    
    pop_inicial <- data.frame(cod_agente = 1:nPop,
                              perfil_agente = perfil_agente,
                              tx_exp = tx_exp,
                              tx_inf = tx_inf,
                              prob_n_adoecer = rep(0, nPop),
                              tx_mort = tx_mort,
                              prob_viver = rep(0, nPop),
                              estado_agente = "S",
                              prob_socializacao_agente = runif(nPop,0,1),
                              tempo_E_agente = rep(0, nPop),
                              tempo_I_agente  = rep(0, nPop),
                              tempo_R_agente  = rep(0, nPop),
                              foi_infectado = "N",
                              stringsAsFactors = FALSE)
    
    
    pop_inicial$n_agente_encontro = round(pop_inicial$prob_socializacao_agente * nMaxEncAgt, 0) + 1
    
    
    # selecionando aleatoriamente pessoas expostas ----------------------------
    
    cod_agente_exposto <- sample(pop_inicial[pop_inicial$estado_agente == "S",]$cod_agente, 
                                 size = nExp, 
                                 replace = F)
    
    pop_inicial[pop_inicial$cod_agente %in% cod_agente_exposto,]$estado_agente <- "E"
    
    pop_inicial[pop_inicial$cod_agente %in% cod_agente_exposto,]$tempo_E_agente <- rbinom(nExp, n_dias_incubacao, 0.5) + 1 
    
    # selecionando aleatoriamente pessoas infectadas ----------------------------    
    
    cod_agente_infectado <- sample(pop_inicial[pop_inicial$estado_agente == "S",]$cod_agente, 
                                   size = nInfec, 
                                   replace = F)
    
    pop_inicial[pop_inicial$cod_agente %in% cod_agente_infectado,]$estado_agente <- "I"
    
    pop_inicial[pop_inicial$cod_agente %in% cod_agente_infectado,]$tempo_I_agente <- rbinom(nInfec, n_dias_infeccao, 0.5) + 1
    
    # return ------------------------------------------------------------------
    
    return(pop_inicial)
    
  }


