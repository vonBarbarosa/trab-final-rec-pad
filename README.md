# Trabalho Final: Reconhecimento de Padrões

A proposta do Trabalho Final propõe um algoritmo para seleção de amostras mais significativas para a aplicação do método KDE (kernel density estimation). O método se estrutura da seguinte forma:


1. Aplicação do método KDE para gerar separação entre duas classes (será aplicado para problema binário).
  1. Escolher h (largura de banda) segundo alguma heurística. Possivelmente fazer um processo de seleção de h específico para cada problema/base de dados. Provavelmente utilizando-se seleção de h  que provê menor erro para amostras de validação/teste.
1. Fazer filtragem de pontos de treinamento, retirando aqueles que seriam classificados erroneamente, devido a ruído nas classes.
1. Vasculhar superfície/espaço buscando pontos de fronteira entre as duas classes.
  1. Para cada ponto de fronteira, selecionar k pontos de maior proximidade em cada classe.
    1. É necessário definir um número k que gere melhor resultado. Serão testados vários valores para k.
1. Iterar sobre todos as amostras, retirando aquelas que não foram escolhidas nem ao menos uma vez no passo 3, mantendo as restantes como amostras mais relevantes para a aplicação do KDE.
1. Finalmente, reaplicar o KDE, dessa vez com as amostras consideradas mais relevantes. Verificar desempenho.
