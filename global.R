# load required libraries
library(shiny)
library(plyr)
library(ggplot2)
library(googleVis)
library(reshape2)
library(survival)
library(fitdistrplus)

## Implementa a função confiabilidade R(t)
confiabilidade <- function(forma, escala, tempo) {
  
  ## Calcula o R(t)
  rt <- exp(-(tempo/escala)^forma)
  
  ## Retorna o valor calculado
  return(rt)
}

## Implementa o cálculo dos quantis
quantil <- function(escala, forma, probabilidade) {
  
  ## Cálculo do quantil
  tp <- escala*(-log(1 - probabilidade))^(1/forma)
  
  ## Retorna o resultado
  return(tp)
}