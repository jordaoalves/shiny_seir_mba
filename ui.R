dashboardPage(skin = "black",
              dashboardHeader(title = "Simulador Epidemiológico SEIR + MBA",  titleWidth = 700
                              ),
              dashboardSidebar(
                #      menuItem("Liberal Quotient (LQ) Scores", tabName = "frontpage"),
                width = 750,
                sidebarMenu(
                useShinyjs(),
                  fluidRow(
                    column(3,
                           numericInput("nPop", "Tamanho da população:", value = 1000, min = 100, max = 200000000),
                           #selectInput("tipoPop", "Tipo da população:", choices = c("Jovem", "Adulta","Idosa")),
                           numericInput("nInfec", "Número de infectados iniciais:", value = 3, min = 1, max = 100000),
                           numericInput("nExpos", "Número de expostos iniciais:", value = 19, min = 1, max = 100000),
                           numericInput("nEncAgt", "Número máximo de encontros por agente:", value = 10, min = 1, max = 100),
                           numericInput("nDiasIncub", "Período de incubação em dias:", value = 5, min = 1, max = 100000),
                           numericInput("nDiasInfec", "Período de infecção em dias:", value = 15, min = 1, max = 100000)
                    ),
                     column(3,
                           sliderInput("percJovem", "Percentual de Jovens:", min = 1, max = 98, value = 33, step = 1),
                           numericInput("txExposJovem", "Taxa de exposição dos jovens:", value = 0.14, min = 0.01, max = 1),
                           numericInput("txInfecJovem", "Taxa de infecção dos jovens:", value = 0.1, min = 0.01, max = 1),
                           numericInput("txMortJovem", "Taxa de mortalidade dos jovens:", value = 0.01, min = 0.01, max = 1),
                           sliderInput("percInfecGrave" , "Percentual de casos graves:" ,
                                       min  =  1 , max  =100 , value  = 30
                           ),
                           sliderInput("diasSimulados" , "Dias de simulação:" ,
                                       min  =  1 , max  =100 , value  = 10
                           )
                    ),
                    column(3,
                           sliderInput("percAdulto", "Percentual de adultos:", min = 1, max = 100, value = 60, step = 1),
                           numericInput("txExposAdulto", "Taxa de exposição dos adutos:", value = 0.15, min = 0.01, max = 1),
                           numericInput("txInfecAdulto", "Taxa de infecção dos adutos:", value = 0.11, min = 0.01, max = 1),
                           numericInput("txMortAdulto", "Taxa de mortalidade dos adutos:", value = 0.01, min = 0.01, max = 1),
                           numericInput("vlAmb" , "Valor médio para casos leves em R$:" ,
                                        min  =  1 , max  =20000 , value  = 200
                           ),
                           br(),
                           sliderInput("rodadas" , "Número de Rodadas:" ,
                                       min  =  10 , max  = 1000 , value  = 10
                           )
                    ),
                    column(3,
                           sliderInput("percIdoso", "Percentual de idosos:", min = 1, max = 100, value = 34, step = 1),
                           numericInput("txExposIdoso", "Taxa de exposição dos idosos:", value = 0.16, min = 0.01, max = 1),
                           numericInput("txInfecIdoso", "Taxa de infecção dos idosos:", value = 0.12, min = 0.01, max = 1),
                           numericInput("txMortIdoso", "Taxa de mortalidade dos idosos:", value = 0.03, min = 0.01, max = 1),
                           numericInput("vlInt" , "Valor médio para casos graves em R$:" ,
                                        min  =  1 , max  =20000 , value  = 2000
                           ),
                           br(),
                           br(),
                           br(),
                           actionButton("botao", "Rodar Simulação", icon("play"),
                                        style='padding:6px; 
                                                  font-size:130%;
                                                  color: #fff; 
                                                  background-color: #51A800; 
                                                  border-color: #FFFFFF')
                    )
                  )
                  
                )#,
                #tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"))
              ),
              dashboardBody(includeCSS("www/custom.css"),
                            useWaiter(), # dependencies
                            waiter_on_busy(
                              html = spin_1(),
                              color = "#8B0000",
                              logo = "",
                              image = "",
                              fadeout = TRUE
                            ),
                            fluidPage(
                              tabBox(width = 12,
                                     id = "frontpage",
                                     tabPanel("População inicial simulada", reactableOutput("table", height = "570px")),
                                     tabPanel("MC(S)", plotlyOutput("plotS", height = "570px")),
                                     tabPanel("MC(E)", plotlyOutput("plotE", height = "570px")),
                                     tabPanel("MC(I)", plotlyOutput("plotI", height = "570px")),
                                     tabPanel("MC(R)", plotlyOutput("plotR", height = "570px")),
                                     tabPanel("MC(D)", plotlyOutput("plotD", height = "570px")),
                                     tabPanel("E(MC(SEIR))", plotlyOutput("plot", height = "570px")),
                                     tabPanel("Número de infectados por rodada", plotlyOutput("plotInfectados", height = "570px")),
                                     tabPanel("Despesas médico-hospitalares por rodada", plotlyOutput("plotDespesas", height = "570px")),
                                     tabPanel("Sumário", reactableOutput("summary")),
                                     
                                     tabPanel("Sobre",
                                              fluidPage(
                                                column(7,
                                                       h4("O aplicativo:"),
                                                       p("Este aplicativo propõe a utilização de modelos baseados em agentes para simular diferentes 
                                                         cenários epidemiologicos com base nos cenários do modelo SEIR a fim de analisar as 
                                                         diferentes possibilidades de resultados a serem obtidos de acordo com a troca de parâmetros."),
                                                       p(""),
                                                       p(""),
                                                       br(),
                                                       h4("Autor:"),
                                                       p("Jordão de Lima Alves - Cientista de Dados"),
                                                       p(a("www.jordaoalvesds.wordpress.com", href = 'https://jordaoalvesds.wordpress.com/' )),
                                                       br(),
                                                       h4("Agradecimentos:"),
                                                       p("Antonio Marcos Batista do Nascimento - DEST/UFRN"),
                                                       p("José Vilton Costa - DDCA/UFRN"),
                                                       p("Marcus Alexandre Nunes - DEST/UFRN"),
                                                       br(),
                                                       h4("Github:"),
                                                       p(a("https://github.com/jordaoalves/shiny_seir_mba", href = 'https://github.com/jordaoalves/shiny_seir_mba' ))
                                                       
                                                       
                                                       
                                                       
                                                )))
                              ),
                              useShinyjs(), tags$head({tags$style(HTML("
.material-switch > input[type='checkbox']:checked + label::before {
opacity: 1;
border: 1px solid #000000;
}
.material-switch > label::before {
width: 36px;
}
.material-switch > label::after {
height: 16px;
width: 16px;
top: 0px;
}
.selectize-input.focus {
border-color: #000000;
}
code {
color: #000000;
background-color: transparent;
padding: 0px 0px;
font-size: 80%;
}
a:hover {
color: #000000;
}
.irs-slider {
top: 19px;
width: 6px;
height: 20px;
background: white;
border: 1px solid #000000;
}
.irs-grid-pol {
opacity: 0.7;
background: #000000;
}
.js-irs-1 .irs-single,
.js-irs-1 .irs-bar-edge,
.js-irs-1 .irs-bar, .irs-from, .irs-to
{
color: #000000;
background: none;
border-top: none;
border-bottom: none;}
.irs-single, .irs-bar, .irs-bar-edge, .irs-min, .irs-max
{
background: none;
border-top: none;
border-bottom: none;
}
.skin-black .sidebar-menu>li>a {
border-left: none;
}
.skin-black .sidebar-menu>li.active>a, .skin-black .sidebar-menu>li:hover>a {
background: none;
border-left: none;
}
.skin-black .main-header .navbar>.sidebar-toggle {
border-right: none;
}
.skin-black .main-header>.logo {
border-right: none;
}
.nav-tabs-custom> .nav-tabs>li.active {
border-top-color: transparent;
}
label {
font-weight: 750;
}
.main-header .logo {
font-family: 'Roboto Condensed', sans-serif;
font-weight: normal;
font-size: 24px;
}
.main-sidebar, .left-side {
width: 750px;
.wrapper {
    background-color: #800000;
}
}
.skin-black .left-side, .skin-black .main-sidebar, .skin-black .wrapper {
    background-color: #800000;
}
@media (min-width: 768px) {
.content-wrapper,
.right-side,
.main-footer {
background-color: #800000;
margin-left: 750px;
}
}
@media (max-width: 767px) {
.sidebar-open .content-wrapper,
.sidebar-open .right-side,
.sidebar-open .main-footer {
-webkit-transform: translate(800px, 0);
-ms-transform: translate(800px, 0);
-o-transform: translate(800px, 0);
transform: translate(800px, 0);
}
}
@media (max-width: 767px) {
.main-sidebar,
.left-side {
-webkit-transform: translate(-800px, 0);
-ms-transform: translate(-800px, 0);
-o-transform: translate(-800px, 0);
transform: translate(-800px, 0);
}
}
@media (min-width: 768px) {
.sidebar-collapse .main-sidebar,
.sidebar-collapse .left-side {
-webkit-transform: translate(-800px, 0);
-ms-transform: translate(-800px, 0);
-o-transform: translate(-800px, 0);
transform: translate(-800px, 0);
}
}"
                              ))})))
)