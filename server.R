function(input, output, session){
  
  
  observeEvent(input$percJovem,{
    updateSliderInput(session, "percAdulto", max = 100 - input$percJovem - 1)
    disable("percIdoso") # putting this here keeps the slider disabled all the time (but still shows updating)
  })
  
  
  observe({
    updateSliderInput(session, "percIdoso", value = 100 - input$percJovem - input$percAdulto)
  })
  
 

observeEvent(input$botao, {
  
  
  pop_inicial <<- gerar_pop_inicial(nPop = input$nPop, 
                                    percJovem = input$percJovem/100,
                                    percAdulto = input$percAdulto/100,
                                    percIdoso = input$percIdoso/100,
                                    nExp = input$nExpos, 
                                    nInfec = input$nInfec,
                                    nMaxEncAgt = input$nEncAgt,
                                    
                                    n_dias_infeccao = input$nDiasInfec,
                                    n_dias_incubacao = input$nDiasIncub,
                                    
                                    tx_exp_jovem = input$txExposJovem, 
                                    tx_inf_jovem = input$txInfecJovem,  
                                    tx_mort_jovem = input$txMortJovem,
                                    
                                    tx_exp_adulta = input$txExposAdulto, 
                                    tx_inf_adulta = input$txInfecAdulto,  
                                    tx_mort_adulta = input$txMortAdulto,
                                   
                                    tx_exp_idosa = input$txExposIdoso, 
                                    tx_inf_idosa = input$txInfecIdoso,  
                                    tx_mort_idosa = input$txMortIdoso
                                   )
  
  

  output$table <- renderReactable({
    
    
    reactable::reactable(pop_inicial,
                         defaultColDef = colDef(
                           header = function(value) gsub(".", " ", value, fixed = TRUE),
                           cell = function(value) format(value, nsmall = 3),
                           align = "center",
                           minWidth = 100,
                           headerStyle = list(background = "#A52A2A")
                         ),
                         bordered = TRUE,
                         highlight = TRUE,
                         defaultPageSize = 13
                         )
    
  })
  
  
  rodadas <<- input$rodadas
  diasSimulados <<- input$diasSimulados
  nMaxEncAgt <<- input$nEncAgt
  n_dias_infeccao <<- input$nDiasInfec
  n_dias_incubacao <<- input$nDiasIncub 
  
  plan(multisession, workers = future::availableCores())
  
  SEIRDfI_rodadas <<- furrr::future_map_dfc(1:input$rodadas, function(x){
    modelo(pop_inicial = pop_inicial, 
           diasSimulados = diasSimulados,
           n_dias_infeccao = n_dias_infeccao,
           n_dias_incubacao = n_dias_incubacao,
           rodada = x)
  },
  .options =   furrr::furrr_options(seed = TRUE)
  )
  
  SEIR_rodadas <<- 
    SEIRDfI_rodadas[,!stringr::str_detect(colnames(SEIRDfI_rodadas), 'fI')]  
  
  infectados_acumulados <<- 
    SEIRDfI_rodadas[,stringr::str_detect(colnames(SEIRDfI_rodadas), 'fI')] %>% 
    filter(row_number()==n()) %>% 
    gather() %>% 
    mutate(fI_leve = round(value * ((100-input$percInfecGrave)/100), digits = 0),
           fI_grave = round(value * (input$percInfecGrave/100), digits = 0),
           value = fI_leve + fI_grave,
           fI_leve_vl = fI_leve * input$vlAmb,
           fI_grave_vl = fI_grave * input$vlInt,
           fI_total_vl = fI_leve_vl + fI_grave_vl
    )
  
 SEIRD.S <<- 
  SEIR_rodadas %>%
    tidyr::gather() %>%
    filter(stringr::str_detect(key, 'S') ) %>%
    mutate(dia = rep(1:input$diasSimulados, input$rodadas)) %>%
    select(value, dia) %>% 
    group_by(dia) %>% 
    summarise(
      minimo = min(value),
      media = mean(value),
      maximo = max(value)
    )
  
  SEIRD.E <<- 
    SEIR_rodadas %>%
    tidyr::gather() %>%
    filter(stringr::str_detect(key, 'E') ) %>%
    mutate(dia = rep(1:input$diasSimulados, input$rodadas)) %>%
    select(value, dia) %>% 
    group_by(dia) %>% 
    summarise(
      minimo = min(value),
      media = mean(value),
      maximo = max(value)
    )
  
  SEIRD.I <<- 
    SEIR_rodadas %>%
    tidyr::gather() %>%
    filter(stringr::str_detect(key, 'I') ) %>%
    mutate(dia = rep(1:input$diasSimulados, input$rodadas)) %>%
    select(value, dia) %>% 
    group_by(dia) %>% 
    summarise(
      minimo = min(value),
      media = mean(value),
      maximo = max(value)
    )
  
  SEIRD.R <<- 
    SEIR_rodadas %>%
    tidyr::gather() %>%
    filter(stringr::str_detect(key, 'R') ) %>%
    mutate(dia = rep(1:input$diasSimulados, input$rodadas)) %>%
    select(value, dia) %>% 
    group_by(dia) %>% 
    summarise(
      minimo = min(value),
      media = mean(value),
      maximo = max(value)
    )
  
  SEIRD.D <<- 
    SEIR_rodadas %>%
    tidyr::gather() %>%
    filter(stringr::str_detect(key, 'D') ) %>%
    mutate(dia = rep(1:input$diasSimulados, input$rodadas)) %>%
    select(value, dia) %>% 
    group_by(dia) %>% 
    summarise(
      minimo = min(value),
      media = mean(value),
      maximo = max(value)
    )
  
  
  
  
  output$plot <- renderPlotly({
    
    rbind(cbind(Estado = "S", SEIRD.S[1], SEIRD.S[3]),
          cbind(Estado = "E", SEIRD.E[1], SEIRD.E[3]),
          cbind(Estado = "I", SEIRD.I[1], SEIRD.I[3]),
          cbind(Estado = "R", SEIRD.R[1], SEIRD.R[3]),
          cbind(Estado = "D", SEIRD.D[1], SEIRD.D[3])
    ) %>% 
    ggplot(aes(x = dia, y = media, colour = Estado)) +
      geom_line(size = 0.75) +
      scale_color_manual(
        values = c(S = "#68177B",
                   E = "#E98246",
                   I = "#A1BCEB",
                   R = "#79AB14",
                   D = "#000000")
      ) +
      labs(title = "Gráfico SEIRD com valores médios simulados dos estados por dia") +
      xlab("Dia simulado") +
      ylab("Frequência") +
      theme_classic()
    
  })
  
  
  output$plotS <- renderPlotly({
    
    SEIRD.S %>%
      ggplot(aes(x = dia, y = media)) + 
      geom_line(size = 0.75,col='#68177B') + 
      geom_ribbon(aes(ymin = minimo, ymax = maximo), alpha = 0.1) + 
      theme_classic() +
      labs(title = "Valor médio e seus limites dos números de suscetíveis por dia") +
      xlab("Dia simulado") +
      ylab("Frequência")
  })
  
  output$plotE <- renderPlotly({
    
    SEIRD.E %>%
      ggplot(aes(x = dia, y = media)) + 
      geom_line(size = 0.75,col='#E98246') + 
      geom_ribbon(aes(ymin = minimo, ymax = maximo), alpha = 0.1) + 
      theme_classic() +
      labs(title = "Valor médio e seus limites dos números de expostos por dia") +
      xlab("Dia simulado") +
      ylab("Frequência")
  })
  
  output$plotI <- renderPlotly({
    
    SEIRD.I %>%
      ggplot(aes(x = dia, y = media)) + 
      geom_line(size = 0.75,col='#A1BCEB') + 
      geom_ribbon(aes(ymin = minimo, ymax = maximo), alpha = 0.1) + 
      theme_classic() +
      labs(title = "Valor médio e seus limites dos números de infectados por dia") +
      xlab("Dia simulado") +
      ylab("Frequência")
  })
  
  output$plotR <- renderPlotly({
    
    SEIRD.R %>%
      ggplot(aes(x = dia, y = media)) + 
      geom_line(size = 0.75,col='#79AB14') + 
      geom_ribbon(aes(ymin = minimo, ymax = maximo), alpha = 0.1) + 
      theme_classic() +
      labs(title = "Valor médio e seus limites dos números de recuperados por dia") +
      xlab("Dia simulado") +
      ylab("Frequência")
  })
  
  
  output$plotD <- renderPlotly({
    
    SEIRD.D %>%
      ggplot(aes(x = dia, y = media)) + 
      geom_line(size = 0.75,col='#000000') + 
      geom_ribbon(aes(ymin = minimo, ymax = maximo), alpha = 0.1) + 
      theme_classic() +
      labs(title = "Valor médio e seus limites dos números de mortos por dia") +
      xlab("Dia simulado") +
      ylab("Frequência")

  })
  
  
   output$plotInfectados <- renderPlotly({
  
  ggplot(infectados_acumulados) +
    aes(x = value) +
    geom_density(adjust = 1, fill = "#A1BCEB") +
    labs(title = "Distribuição do número total de infectados simulados") +
    xlab("Dia simulado") +
    ylab("Frequência") +
    geom_vline(aes(xintercept = mean(value)), linetype = "dashed", size = 0.6) +
    theme_classic()
  })
  
  
  
  output$plotDespesas <- renderPlotly({
    
    ggplot(infectados_acumulados) +
      aes(x = fI_total_vl) +
      geom_density(adjust = 1, fill = "#32CD32") +
      labs(title = "Distribuição das despesas totais simuladas") +
      xlab("Valor total das despesas") +
      ylab("Frequência") +
      geom_vline(aes(xintercept = mean(fI_total_vl)), linetype = "dashed", size = 0.6) +
      theme_classic()
  })
  
  output$summary <- renderReactable({
    
    infectados_acumulados %>% 
      select(Infectados = value,
             Despesas = fI_total_vl) %>% 
      df_summary() %>%
      mutate(across(where(is.numeric), num_format)) %>% 
    reactable::reactable(
                         defaultColDef = colDef(
                           header = function(value) gsub(".", " ", value, fixed = TRUE),
                           cell = function(value) format(value, nsmall = 3),
                           align = "center",
                           minWidth = 100,
                           headerStyle = list(background = "#A52A2A")
                         ),
                         bordered = TRUE,
                         highlight = TRUE,
                         defaultPageSize = 13
    )
    
  })
  
  
})

}


