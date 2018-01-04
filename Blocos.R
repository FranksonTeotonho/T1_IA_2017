source("Estado.R")

#Classe e metodos para o jogo de blocos
Blocos <- function(desc = NULL, pai = NULL){
  
  e <-environment()

  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("Blocos","Estado")
  
  return (e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.Blocos = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc[obj1$desc != 'E'] == obj2$desc[obj2$desc != 'E']))
  }
}

## Sobrecarga da função genérica "print" do R
print.Blocos <- function(obj) {
  cat("Peças: (", obj$desc, ")\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}
## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.Blocos <- function(atual){
  if(is.null(atual$desc))
    return(Inf)
  
  Vteste <- c('W','W','W','B','B','B')
  
  desc <- atual$desc[atual$desc!='E']
  h <- 6
  for(i in 1:6){
    if(Vteste[i] == desc[i]){
      h <- h - 1
    }
  }
  return(h)
}

##Função que auxilia nas trocas e na crianção de um filho
criaFilho <- function(i,j,obj){
  V <- obj$desc
  aux <- V[i]
  V[i] <- V[j]
  V[j] <- aux
  
  filho <- Blocos(desc = V ,pai = obj)
  filho$h <- heuristica(filho)
  
  if(abs(i-j) == 1){
    filho$g <- obj$g + 1  
  }else if(abs(i-j) == 2){
    filho$g <- obj$g + 2
  }else if(abs(i-j) == 3){
    filho$g <- obj$g + 3
  }
  return(filho)
}

# Gerando os filhos
geraFilhos.Blocos <- function(obj) {
  
  desc <- obj$desc
  filhos <- list()
  
  if(desc[7] == 'E'){
    filhos <- list(criaFilho(7,6,obj), ##lado esquerdo
                   criaFilho(7,5,obj),
                   criaFilho(7,4,obj))  
  }else if(desc[6] == 'E'){
    filhos <- list(criaFilho(6,7,obj), ##lado direito
                   criaFilho(6,5,obj), ##lado esquerdo
                   criaFilho(6,4,obj),
                   criaFilho(6,3,obj))
  }else if(desc[5] == 'E'){
    filhos <- list(criaFilho(5,6,obj), #lado direito
                   criaFilho(5,7,obj),
                   criaFilho(5,4,obj),#lado esquerdo
                   criaFilho(5,3,obj),
                   criaFilho(5,2,obj))
  }else if(desc[4] == 'E'){
    filhos <- list(criaFilho(4,5,obj), #lado direito
                   criaFilho(4,6,obj),
                   criaFilho(4,5,obj),
                   criaFilho(4,3,obj), #lado esquerdo
                   criaFilho(4,2,obj),
                   criaFilho(4,1,obj))
  }else if(desc[3] == 'E'){
    filhos <- list(criaFilho(3,4,obj), #lado direito
                   criaFilho(3,5,obj),
                   criaFilho(3,6,obj),
                   criaFilho(3,2,obj),#lado esquerdo
                   criaFilho(3,1,obj))
  }else if(desc[2] == 'E'){
    filhos <- list(criaFilho(2,3,obj), #lado direito
                   criaFilho(2,4,obj),
                   criaFilho(2,5,obj),
                   criaFilho(2,1,obj)) #lado esquerdo
  }else if(desc[1] == 'E'){
    filhos <- list(criaFilho(1,2,obj), #lado direito
                   criaFilho(1,3,obj),
                   criaFilho(1,4,obj))
  }
  
  return (filhos)
}