
# Lógica do servidor

# Inicia o arquivo de servidor com a função principal
shinyServer(function(input, output) {
  
  ################## Lendo o conjunto de dados para uso posterior
  Data <- reactive({
    
    ## Arquivo a ser lido
    inFile <- input$file1
    
    ## Se nenhum arquivo estiver carregado não faz nada
    if (is.null(inFile))
      return(NULL)
    
    ## Se o arquivo tiver sido carregado, aqui ele é lido
    df.raw <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    
    ## Cria objeto Surv
    ## Aqui o padrão é que a primeira coluna tem os tempos
    ## e a segunda coluna tem o indicador de censura
    df.surv <- Surv(df.raw[,1], df.raw[,2])
    
    ## Cria o survfit
    df.survfit <- survfit(formula = df.surv ~ 1, type="kaplan-meier")
      
    ## Cria uma lista para retornar os dados e o Surv  
    info <- list(df.raw=df.raw, df.surv=df.surv, df.survfit=df.survfit)
    return(info)
  })
  
  # Opção para tornar paginável
  myOptions <- reactive({  
    list(
      page=ifelse(input$pageable==TRUE,'enable','disable'),
      pageSize=input$pagesize
    ) 
  } )
  
  ################## Output para a tabela
  output$raw <- renderGvis({
    
    ## Por segurança caso o arquivo ainda n tenha sido lido
    if (is.null(input$file1)) { return() }
    
    gvisTable(Data()$df.raw, options=myOptions())         
  })
  
  
  ################## Output para a tabela
  output$summary <- renderPrint({
    
    ## Segurança para caso nenhum arquivo tenha sido lido
    if (is.null(input$file1)) { return() }
    
    ## Salvando os dados em um data.frame para ser usado
    ## localmente nesta função
    ## Aqui, como a função Data lá em cima para ler, basta
    ## me referir ao Data() que eu pego o data.frame lido
    ys <- Data()$df.survfit
    
    ## Apresenta o sumário do modelo de sobrevivência
    summary(ys)
    
  })
  
  ################## Output do plot 1
  output$plot1 <- renderPlot({
    
    ## Segurança para caso nenhum arquivo tenha sido lido
    if (is.null(input$file1)) { return() }
    
    ## Recuperando o survfit
    ys <- Data()$df.survfit
    
    ## Plot da função de sobrevivência
    plot(ys, xlab="Horas", ylab="Probabilidade de sobrevivência")
    
  })
  
  ################## Output do plot 2
  output$plot2 <- renderPlot({
    
    ## Segurança para caso nenhum arquivo tenha sido lido
    if (is.null(input$file1)) { return() }
    
    ## Recuperando o survfit
    dados <- Data()$df.raw
    
    ## Pontos de probabilidade igualmente espaçados
    p = ppoints(dados$falhas, a=0.3)
    
    ## Criando um data.frame com os tempos de falha e o risco acumulado
    survresult = data.frame(tempos = dados$falhas, risco = -log(1-p))
    
    ## Plot da função de sobrevivência
    plot(data = survresult, risco ~ tempos, log="xy", pch=19, col="red", xlab="Horas", ylab="Risco acumulado")
    
  })
  
  ################## Output do plot 3
  output$plot3 <- renderPlot({
    
    ## Segurança para caso nenhum arquivo tenha sido lido
    if (is.null(input$file1)) { return() }
    
    ## Recebe os dados de sobrevivência
    y <- Data()$df.surv
    
    ## Estimativa dos parametros da weibull.
    ##Supondo que os tempos de falha vieram de uma weilbull,  entÃ£o survreg 
    ##encontrar os parametros da weibull que melhor se ajusta aos dados.
    ##o R calcula estes parametros pelo metodo da maxima verossimilhanÃ§a 
    yw = survreg(y ~ 1, dist="weibull")
    
    
    ##Apenas ajustando numero que digitos que se deseja trabalhar.
    signif(summary(yw)$loglik, 5)
    signif(extractAIC(yw), 5)
    
    ## Maximum likelihood estimates:
    ## For the Weibull model, survreg fits log(T) = log(eta) +
    ## (1/beta)*log(E), where E has an exponential distribution with mean 1
    ## eta = Characteristic life (Scale) 
    ## beta = Shape
    
    ## Cria lista com estimativas
    estimativas <- list()
    
    ## Adiciona estimativas a lista
    estimativas$escala <- exp(coefficients(yw)[1]) #escala
    estimativas$forma <- 1/yw$scale #forma
    
    ## Lifetime: expected value and standard deviation.
    #\muHAT Ã© o calculo da esperanÃ§a da minha distribuiÃ§Ã£o
    estimativas$média = estimativas$escala * gamma(1 + 1/estimativas$forma)
    estimativas$desvio_padrão = estimativas$escala * sqrt(gamma(1+2/estimativas$forma) - (gamma(1+1/estimativas$forma))^2)
    
    ## Densidade de probabilidade do modelo ajustado .
    curve(dweibull(x, shape=estimativas$forma, scale=estimativas$escala),
          from=0, to=estimativas$média+6*estimativas$desvio_padrão, col="blue",
          xlab="Horas", ylab="Densidade de probabilidade")
  })
  
  ################## Output do plot 4
  output$plot4 <- renderPlot({
    
    ## Criando um gráfico de Cullen and Frey
    descdist(Data()$df.raw[,1], boot = 1000)
    
  })
  
  ################## Output do plot 5
  output$plot5 <- renderPlot({
    
    ## Ajustes de distribuição
    fw <- fitdist(Data()$df.raw[,1], "weibull")
    fg <- fitdist(Data()$df.raw[,1], "exp")
    fln <- fitdist(Data()$df.raw[,1], "lnorm")
    
    ## Divide a janela em quatro partes
    par(mfrow = c(2, 2))
    
    ## Cria a legenda
    plot.legend <- c("Weibull", "lognormal", "exp")
    
    ## Cria os gráficos
    denscomp(list(fw, fln, fg), legendtext = plot.legend, xlim = c(0,400), ylim = c(0,0.025), main = 'Histogramas e Densidades Teóricas', xlab = 'dados', ylab = 'Densidade')
    qqcomp(list(fw, fln, fg), legendtext = plot.legend, xlab = 'Quantis Teóricos', ylab = 'Quantis Empíricos')
    cdfcomp(list(fw, fln, fg), legendtext = plot.legend, main = 'CDFs Empiricas e Teoricas')
    ppcomp(list(fw, fln, fg), legendtext = plot.legend, xlab = 'Probabilidade teóricas', ylab = 'Probabilidades empíricas')
    
  })
  
  ################## Output dos testes qui-quadrado
  output$aderencia <- renderPrint({
    
    ## Ajustes de distribuição
    fw <- fitdist(Data()$df.raw[,1], "weibull")
    fg <- fitdist(Data()$df.raw[,1], "exp")
    fln <- fitdist(Data()$df.raw[,1], "lnorm")
    
    ## Testes de bondade de ajuste
    gofstat(list(fw, fg, fln), fitnames = c('Weibull', 'Exponencial', 'Log-Normal'), discrete = T)
    
  })
  
  ################## Output do sumário de Cullen and Frey
  output$cullen <- renderPrint({
    
    ## Texto de saída do gráfico
    descdist(Data()$df.raw[,1], boot = 1000)
  })
  
  ################## Output das regressão
  output$regressao <- renderPrint({
    
    ## Recebe os dados de sobrevivência
    y <- Data()$df.surv
    
    ## Estimativa dos parametros da weibull.
    ##Supondo que os tempos de falha vieram de uma weilbull,  entÃ£o survreg 
    ##encontrar os parametros da weibull que melhor se ajusta aos dados.
    ##o R calcula estes parametros pelo metodo da maxima verossimilhanÃ§a 
    yw = survreg(y ~ 1, dist="weibull")
    
    ## Sumário do ajuste
    summary(yw)
    
  })
  
  
  ################## Output das regressão
  output$estimativas <- renderPrint({
    
    ## Recebe os dados de sobrevivência
    y <- Data()$df.surv
    
    ## Estimativa dos parametros da weibull.
    ##Supondo que os tempos de falha vieram de uma weilbull,  entÃ£o survreg 
    ##encontrar os parametros da weibull que melhor se ajusta aos dados.
    ##o R calcula estes parametros pelo metodo da maxima verossimilhanÃ§a 
    yw = survreg(y ~ 1, dist="weibull")
    
  
    ##Apenas ajustando numero que digitos que se deseja trabalhar.
    signif(summary(yw)$loglik, 5)
    signif(extractAIC(yw), 5)
    
    ## Maximum likelihood estimates:
    ## For the Weibull model, survreg fits log(T) = log(eta) +
    ## (1/beta)*log(E), where E has an exponential distribution with mean 1
    ## eta = Characteristic life (Scale) 
    ## beta = Shape
    
    ## Cria lista com estimativas
    estimativas <- list()
    
    ## Adiciona estimativas a lista
    estimativas$escala <- exp(coefficients(yw)[1]) #escala
    estimativas$forma <- 1/yw$scale #forma
    
    ## Lifetime: expected value and standard deviation.
    #\muHAT Ã© o calculo da esperanÃ§a da minha distribuiÃ§Ã£o
    estimativas$média = estimativas$escala * gamma(1 + 1/estimativas$forma)
    estimativas$desvio_padrão = estimativas$escala * sqrt(gamma(1+2/estimativas$forma) - (gamma(1+1/estimativas$forma))^2)
    
    ## Imprime na tela
    print(estimativas)
    
  })
  
  ################## Output da confiabilidade
  output$confiabilidade <- renderPrint({
    
    ## Recebe os dados de sobrevivência
    y <- Data()$df.surv
    
    ## Estimativa dos parametros da weibull.
    ##Supondo que os tempos de falha vieram de uma weilbull,  entÃ£o survreg 
    ##encontrar os parametros da weibull que melhor se ajusta aos dados.
    ##o R calcula estes parametros pelo metodo da maxima verossimilhanÃ§a 
    yw = survreg(y ~ 1, dist="weibull")
    
    
    ##Apenas ajustando numero que digitos que se deseja trabalhar.
    signif(summary(yw)$loglik, 5)
    signif(extractAIC(yw), 5)
    
    ## Maximum likelihood estimates:
    ## For the Weibull model, survreg fits log(T) = log(eta) +
    ## (1/beta)*log(E), where E has an exponential distribution with mean 1
    ## eta = Characteristic life (Scale) 
    ## beta = Shape
    
    ## Cria lista com estimativas
    estimativas <- list()
    
    ## Adiciona estimativas a lista
    escala <- exp(coefficients(yw)[1]) #escala
    forma <- 1/yw$scale #forma
    
    ## Lifetime: expected value and standard deviation.
    #\muHAT Ã© o calculo da esperanÃ§a da minha distribuiÃ§Ã£o
    média = escala * gamma(1 + 1/forma)
    desvio_padrão = escala * sqrt(gamma(1+2/forma) - (gamma(1+1/forma))^2)
    
    ## Segurança para caso tempo não tenha sido utilizado
    if (is.null(input$tempo)) { return() }
    
    ## Cálculo da confiabilidade
    estimativas$confiabilidade <- round(confiabilidade(forma = forma, escala = escala, tempo = input$tempo),2)
    estimativas$confiabilidade <- paste0(estimativas$confiabilidade*100, "%")
    
    ## Segurança para caso probabilidade não tenha sido utilizado
    if (is.null(input$probabilidade)) { return() }
    
    ## Cálculo do quantil
    estimativas$quantil <- quantil(escala = escala, forma = forma, probabilidade = 1 - input$probabilidade)
    estimativas$quantil <- round(estimativas$quantil, 2)
    
    ## Imprime o resultado na tela
    print(estimativas)
  })
  
  
  ################### Textos nos captions ##################
  output$caption1 <- renderText( {
    if (is.null(input$file1)) { return() }
    
    "Exploração dos dados"
  })
  
  output$caption2 <- renderText( {
    if (is.null(input$file1)) { return() }
    
    "Gráfico da função de sobrevivência."
  })
  
  
  output$caption3 <- renderText( {
    if (is.null(input$file1)) { return() }
    
    "Sumário do modelo de sobrevivência ajustado"
  })
  
  output$caption4 <- renderText( {
    if (is.null(input$file1)) { return() }
    
    "Sumário do ajuste para determinação do fator de forma e do fator de escala da Weibull."
  })
  
  output$caption5 <- renderText( {
    if (is.null(input$file1)) { return() }
    
    "Estimativas de momentos e parâmetros da Weibull."
  })
  
  output$caption6 <- renderText( {
    if (is.null(input$file1)) { return() }
    
    "Gráfico da função Risco Acumulado."
  })
  
  output$caption7 <- renderText( {
    if (is.null(input$file1)) { return() }
    
    "Densidade da distribuição Weibull, feito a partir dos parâmetros estimados."
  })
  
  output$caption8 <- renderText( {
    if (is.null(input$file1)) { return() }
    
    "Calculos da confiabilidade e dos quantis."
  })
  
  output$caption9 <- renderText( {
    if (is.null(input$file1)) { return() }
    
    "O gráfico de Cullen and Frey fornece a representação gráfica dos momentos de diversas
     distribuições de probabilidade e a localização dos dados fornecidos após um processo de reamostragem.
    
    É uma orientação geral para a escolha do melhor modelo probabilístico."
  })
  
  output$caption10 <- renderText( {
    if (is.null(input$file1)) { return() }
    
    "Os gráficos a seguir apresentam informações com relação a bondade
    de ajuste em relação as três distribuições mais utilizadas para modelagem
    dos tempos de falha."
  })
  
  output$caption11 <- renderText( {
    if (is.null(input$file1)) { return() }
    
    "Teste Qui-quadrado para avaliar a bondade de ajuste dos modelos anteriores.
    
    IMPORTANTE: AO TRABALHAR COM DADOS CENSURADOS O PODER DO TESTE DE ADERÊNCIA VAI DIMINUIR."
  })
})
