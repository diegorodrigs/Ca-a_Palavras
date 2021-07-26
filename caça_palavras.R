


###########################
# Parâmetros
###########################

palavras <- c("DONA GRAÇA","DIEGO","EMILIA","LIZ","PRISCILLA","SEVERINO","PATRICIA","JOÃO","FRANCISCO","EUNICE")
TAMANHO = c(20,20)

m <- matrix(rep("",prod(TAMANHO)),nrow = TAMANHO[1])


#################################################
# Criando a matriz com os nomes
#################################################
# pal <- palavras[1]
# dire <- "Horizontal"
# dire <- "Vertical"
# dire <- "Diagonal"
# sent <- "Positivo"
# sent <- "Negativo"

for(pal in palavras){
  
  print(pal)

  # Definir aleatoriamente a direção da palavra
  dire <- sample(c("Horizontal","Vertical","Diagonal","Diagonal"),1)
  
  # Definir aleatoriamente a sentido da palavra
  sent <- sample(c("Positivo","Negativo"),1)
  print(c(dire,sent))
  
  # Pegando as posições para colocar a palavra
  vec_pos <- posiciona_palavra(pal,m,sent,dire)
  posicoes_x <- seq(vec_pos[1],vec_pos[3])
  posicoes_y <- seq(vec_pos[2],vec_pos[4])
  
  # Inserindo as letras
  pal_dividida <- strsplit(pal,"")[[1]]
  for(i in seq(nchar(pal))){
    # m[posicoes_x[i],posicoes_y[i]] <- pal_dividida[i]
    m[posicoes_x[pmin(length(posicoes_x),i)],posicoes_y[pmin(length(posicoes_y),i)]] <- pal_dividida[i]
  }
  
}

m_solved <- m


#################################################
# Preenchendo o restante da matriz
#################################################

# Inserindo letras aleatórias na matriz
qtde_vazios <- length(m[m == ""])
subst_vazios <- sample(LETTERS,qtde_vazios,replace = TRUE)
m[m == ""] <- subst_vazios

m


#################################################
# Colocando a matriz em uma imagem
#################################################
# library('plot.matrix')
# library('psych')
# 
# plot(m)
# 
# fa <- fa(m, 5, rotate="varimax")
# par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
# plot(loadings(fa), cex=0.5)


# plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10))
# plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="y label", xlab="x lablel")
# par(mar = c(2, 2, 2, 2))
par(mar = rep(1,4))

j_x <- 2
j_y <- 1
m[j_y,j_x]

plot(0,type='n',axes=FALSE,ann=FALSE, xlim=c(0,TAMANHO[1]), ylim=c(0,TAMANHO[1]), frame.plot = FALSE)
for(j_x in seq(TAMANHO[1])){
  for(j_y in seq(TAMANHO[2])){
  text(j_x,TAMANHO[2] + 1 -j_y, m[j_y,j_x],cex = 0.8)
  }
}

