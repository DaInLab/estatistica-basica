#
# Clean up the area
rm(list = ls())
#

# Localizando o arquivo .xls no Winddows
#setwd("D:\Transferência\estatistica_basica")

# No R, para utilizar determinadas funções, é preciso "carregar" um "pacote".
# Neste caso, carregamos o "pacote" tidyverse diretamente do CRAN-R.
# O tidyverse é uma coleção opinativa de pacotes R projetados para ciência de dados. 
# Todos os pacotes compartilham uma filosofia de design subjacente, gramática e estruturas de dados.
# Dentro da biblioteca tidyverse, existe o pacote readxl, o qual facilita a importação de dados do Excel para o R.
# Caso o mesmo ainda não esteja transferido o comando é:
if(!("tidyverse") %in% installed.packages()) install.packages("tidyverse", type = "source") 
library(tidyverse)

library(readxl)
eb = read_excel("./dados/estatistica_basica.xlsx", sheet = "Plan1")
head(eb)
#escolaridade inscritos
#1   Fundamental       451
#2         Médio       627
#3      Superior       292
#4 Pós-graduação        95
#
# Gráfico de Barras
barplot(eb$inscritos, names.arg = eb$escolaridade, col = c("blue", "green", "red", "lavender"), 
        legend.text = eb$inscritos, sub = "Gráfico de Barras",  main = "Número de Inscritos por Modalidade")

# Histograma
eh <- read_excel("./dados/estatistica_basica.xlsx", sheet = "Plan2")
head(eh)
#  eixos
#1   4.8
#2   4.9
#3   5.1
#4   4.8
#5   5.1
#6   4.9

hist(eh$eixos,  main = "Histograma", labels = TRUE, 
     col = c("blue", "green", "red", "lavender", "mistyrose", "cornsilk", "purple", "yellow"),
     ylab = "Frequência", xlab = "Dados")

# Gráfico Pizza
ep <- read_excel("./dados/estatistica_basica.xlsx", sheet = "Plan3")
head(ep)
#                  ccusto defeitos
#1           Pré-usinagem        9
#2     Tratamento térmico       12
#3               Fundição       10
#4               Usinagem       45
#5 Tratamento superficial       13
#
# Calculando a porcentagem 
#Rótulos para a figura
lbls <- ep$ccusto
pct <- round(ep$defeitos/sum(ep$defeitos)*100, digits=1)

lbls <- paste(lbls, pct) # acrescentar os percentuais aos rótulos 
lbls <- paste(lbls,"%",sep="") # acrescentar o símbolo "%" aos rótulos labels 
lbls

pie(ep$defeitos, labels = lbls, edges = 200, radius = 0.8, 
    clockwise = TRUE, angle = 45, main = "Gráfico de Pizza",
    col = c("blue", "lavender", "mistyrose", "cornsilk", "purple"),
    lty = NULL)

# +++++
# Estatísticas Descritivas
# Média
ma <- read_excel("./dados/estatistica_basica.xlsx", sheet = "Plan4")
head(ma)
#  dados
#1   4.5
#2   4.6
#3   4.5
#4   4.4
#5   4.5
xma = mean(ma$dado)
xma
#[1] 4.5

xma = (4.5+4.6+4.5+4.4+4.5) / 5
xma
#[1] 4.5
mean(4.5,4.6,4.5,4.4,4.5)
#[1] 4.5

# Mediana
md <- read_excel("./dados/estatistica_basica.xlsx", sheet = "Plan5")
head(md)
#  dados
#1    65
#2    72
#3    70
#4    72
#5    60
#6    67
mediana = median(md$dados)
mediana
#[1] 68.5

listamd=sort(md$dados, decreasing=F)
listamd
#[1] 60 65 67 68 69 70 72 72
xbarra = (68 + 69) / 2 
xbarra
#[1] 68.5

# Moda
moda <- read_excel("./dados/estatistica_basica.xlsx", sheet = "Plan5")
head(moda)
# Create the function
#https://www.tutorialspoint.com/r/r_mean_median_mode.htm

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Calculate the mode using the user function.
result <- getmode(md$dados)
print(result)
#[1] 72
# Função melhor
# https://www.r-bloggers.com/computing-the-mode-in-r/

Mode = function(x){ 
ta = table(x)
tam = max(ta)
if (all(ta == tam))
  mod = NA
else
  if(is.numeric(x))
    mod = as.numeric(names(ta)[ta == tam])
else
  mod = names(ta)[ta == tam]
return(mod)
}

#Let’s see how it works for nominal data:
#One mode
fruit = c(rep("apple", 10), rep("pear", 5), rep("banana", 2))
getmode(fruit)
#[1] "apple"
Mode(fruit)
#[1] "apple"

#Two modes
fruit2 = c(rep("apple", 5), rep("pear", 5), rep("banana", 2))
getmode(fruit2)
#[1] "apple" # Hummm... achou só a primeira moda...
Mode(fruit2)
# [1] "apple" "pear" # Ok!!

#No mode
fruit3 = c(rep("apple", 5), rep("pear", 5), rep("banana", 5))
Mode(fruit3)
# [1] NA
getmode(fruit3)
#[1] "apple" # Crap! Error!!

#Let’s check for numerical data:
#One mode
count1 = c(rep(1, 10), rep(2, 5), rep(3, 2))
Mode(count1)
# [1] 1
getmode(count1)
# [1] 1

#Two modes
count2 = c(rep(1, 5), rep(2, 5), rep(3, 2))
Mode(count2)
# [1] 1 2
getmode(count2)
#[1] 1 # Nope!

#No mode
count3 = c(rep(1, 5), rep(2, 5), rep(3, 5))
Mode(count3)
# [1] NA
getmode(count3)
#[1] 1 # For Scotch! Error...

# +++++
# Estatísticas Descritivas
# Amplitude
#Vamos simular uma amostra de 10 valores, tomados de uma 
#distribuição normal com média = 10 e desvio-padrão = 2,5:
normal.1 <- rnorm(10, mean = 10, sd = 2.5)
#Calcule mínimo e máximo e a amplitude destes valores:
range(normal.1)
diff( range(normal.1) )
# [1] 9.224055
# Amplitude
ampl <- read_excel("./dados/estatistica_basica.xlsx",sheet = "Plan5")
head(ampl)
## A tibble: 6 × 1
#dados
#   <dbl>
# 1    65
# 2    72
# 3    70
# 4    72
# 5    60
# 6    67
range(ampl)
#1] 60 72
diff( range(ampl) )
#[1] 12

# Quartis
quartil <- read_excel("./dados/estatistica_basica.xlsx", sheet = "Plan6")
head(quartil)
## A tibble: 6 × 1
#  dados
#    <dbl>
#  1    72
#  2    70
#  3    77
#  4    60
#  5    67
#  6    69
listaq <- sort(quartil$dados)
listaq
# [1] 60 65 66 67 68 69 69 70 71 72 77
min(listaq)
#[1] 60
max(listaq)
#[1] 77
q1 = (length(listaq) + 1) / 4
q1
#[1] 3
listaq[q1]
#[1] 66
## Logo, 25% das observações etão abaixo de 66 e 75% das observações estão acima de 66.
q3 <- 3 * (length(listaq) + 1) / 4
q3
#[1] 9
listaq[q3]
#[1] 71
##Portanto, 75% das observações estão abaixo de 71 e 25% das observações estão acima de 71.

quantile(listaq)
# 0%  25%  50%  75% 100% 
#60.0 66.5 69.0 70.5 77.0 

boxplot(quantile(listaq) , pch=15, main="Quartiz" , col = "lightblue", pars = list(boxwex = 1))
