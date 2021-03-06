debugSource("Blocos.R")
debugSource("buscaDesinformada.R")
debugSource("buscaInformada.R")
#(B,B,B,W,W,W,E)
inicial <- Blocos(desc = c('B','B','B','W','W','W','E'))

#(E,W,W,W,B,B,B)
objetivo <- Blocos()
objetivo$desc <- c('E','W','W','W','B','B','B')

cat("====\tBusca em Largura\t====\n")
print(unlist(buscaEmLargura(inicial, objetivo)))

cat("====\tBusca em Profundidade\t=====\n")
print(buscaEmProfundidade(inicial, objetivo))

cat("====\tBusca de Custo Uniforme\t=====\n")
print(buscaCustoUniforme(inicial, objetivo))

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))

cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))

