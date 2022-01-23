{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
import Control.Exception
import System.IO
import System.IO.Error
import System.Process
import Data.List
import Data.Function
import Data.Char
import System.Exit (exitSuccess)


--Tipos referentes a tabela de jogos presente dentro do arquivo DadosTexto/DataBase.txt
type Tabelas = [Jogo]
type Rodada = Int
type Time = String 
type Time2 = String 
type Gols = Int 
type Gols2 = Int 
data Jogo = Jogo Rodada Time Gols Time2 Gols2 
                    deriving(Show,Read)

--Tipos referentes a tabela de pontuação presente dentro do arquivo DadosTexto/pontos.txt
type Pontuacoes = [Pontuacao]
type TimePonto = String
type PontosTime = Int
data Pontuacao = Pontuacao TimePonto PontosTime
                    deriving(Show,Read)

{- Função que inicia o programa e le os dados dos times salvo em arquivo .txt,
Aqui ela faz uma verificação se dentro do arquivo DadosTexto/pontos.txt estiver escrito os pontos desconsiderando 
o abre e fecha colchetes então ele apenas le, senão ele realiza a criação da tabela de pontos dos time  -}  

main :: IO ()
main = do
        contents <- readFile "./DadosTexto/pontos.txt"
        let newContents = map toUpper contents
        if (length newContents > 2) then 
         apenasLe
        else do 
         writeFile "./DadosTexto/pontos.txt" "[]"
         criartabelatimes

-- Menu usado para a interação do usuario com o programa
menu :: Tabelas -> Pontuacoes-> IO Tabelas
menu dados dadosPontos= do
    putStrLn "|########################################################################|"
    putStrLn "|                        CAMPEONATO DE FUTEBOL GP3                       |"  
    putStrLn "|------------------------------------------------------------------------|"
    putStrLn "| [1]  Número de vitórias, empates e derrotas do time X no campeonato    |"
    putStrLn "| [2]  Classificação do time X no campeonato                             |"
    putStrLn "| [3]  Aproveitamento do time X no campeonato                            |"
    putStrLn "| [4]  Saldo de gols do time X no campeonato                             |"
    putStrLn "| [5]  Resultado da partida da rodada N do time X no campeonato          |"
    putStrLn "| [6]  Número de pontos do time X no campeonato                          |"
    putStrLn "| [7]  Times que estão nas primeiras 3 colocações                        |"
    putStrLn "| [8]  Times rebaixados                                                  |"
    putStrLn "| [9]  Classificação geral do campeonato                                 |"
    putStrLn "| [0]  Sair                                                              |"
    putStrLn "|------------------------------------------------------------------------|"
    op <- getChar 
    getChar 
    resultado dados dadosPontos op


resultado :: Tabelas ->Pontuacoes-> Char -> IO Tabelas

resultado dados dadosPontos '1' = do
                            putStrLn "|------------------------------------------------------------------------|"
                            putStrLn "|Qual time você quer consultar?                                          |"
                            putStrLn "|------------------------------------------------------------------------|"
                            nomeTime <- getLine 
                            vitoriasTime <-  vitoriasDoTime dados nomeTime 0
                            derrotasTime <-  derrotasDoTime dados nomeTime 0
                            emapatesTime <-  empatesDoTime  dados nomeTime 0 
                            aux dadosPontos nomeTime vitoriasTime derrotasTime emapatesTime
                            putStrLn "|Pressione <Enter> para voltar ao menu...                                |"
                            getChar
                            menu dados dadosPontos

resultado dados dadosPontos '2' = do  
                           putStrLn "|------------------------------------------------------------------------|"
                           putStrLn "|Qual time voce deseja consultar a classificação?                        |"
                           putStrLn "|------------------------------------------------------------------------|"
                           ntime<- getLine
                           classTimeX (reverse(ordenar dadosPontos)) ntime 1
                           putStrLn "|Pressione <Enter> para voltar ao menu...                                |"
                           getChar
                           menu dados dadosPontos

resultado dados dadosPontos '3' = do
                           putStrLn "|------------------------------------------------------------------------|"
                           putStrLn "|Qual time você deseja consultar o aproveitamento?                       |"
                           putStrLn "|------------------------------------------------------------------------|"
                           nomeTime <- getLine 
                           pontosTime <- pontosDoTimeX dados nomeTime 0
                           aproveitamentoTime dados dadosPontos nomeTime pontosTime
                           putStrLn "|Pressione <Enter> para voltar ao menu...                                |"
                           getChar
                           menu dados dadosPontos

resultado dados dadosPontos '4' = do
                           putStrLn "|------------------------------------------------------------------------|"
                           putStrLn "|Qual time você deseja consultar o saldo de gols?                        |"
                           putStrLn "|------------------------------------------------------------------------|"
                           nomeTime <- getLine
                           golsFeitos <- saldoGolsFeitos dados nomeTime 0
                           golsTomados <- saldoGolsTomados dados nomeTime 0
                           imprimeSaldo dadosPontos nomeTime golsFeitos golsTomados
                           putStrLn "|Pressione <Enter> para voltar ao menu...                                |"
                           getChar
                           menu dados dadosPontos

resultado dados dadosPontos '5' = do
                           putStrLn "|------------------------------------------------------------------------|"
                           putStrLn "|Qual rodada você deseja consultar?                                      |"
                           putStrLn "|------------------------------------------------------------------------|"
                           rodada <- getLine 
                           putStr "Qual time da rodada você deseja consultar?\n"
                           time <- getLine 
                           resultadoPartida dados dadosPontos rodada time
                           putStrLn "|Pressione <Enter> para voltar ao menu...                                |"
                           getChar
                           menu dados dadosPontos

resultado dados dadosPontos '6' = do
                           putStrLn "|------------------------------------------------------------------------|"
                           putStrLn "|Qual time você deseja consultar os pontos?                              |"
                           putStrLn "|------------------------------------------------------------------------|"
                           time <- getLine 
                           pontosTime <- pontosDoTimeX dados time 0
                           imprimePontos dadosPontos time pontosTime
                           putStrLn "|Pressione <Enter> para voltar ao menu...                                |"
                           getChar
                           menu dados dadosPontos

resultado dados dadosPontos '7' = do
                           putStrLn "|------------------------------------------------------------------------|"
                           putStrLn "|            Times que estão nas 3 primeiras colocações                  |"
                           putStrLn "|------------------------------------------------------------------------|"
                           time3primeiros (reverse(ordenar dadosPontos)) 0
                           putStrLn "|Pressione <Enter> para voltar ao menu...                                |"
                           getChar
                           menu dados dadosPontos

resultado dados dadosPontos '8' = do
                           putStrLn "|------------------------------------------------------------------------|"
                           putStrLn "|                         Times Rebaixados                               |" 
                           putStrLn "|------------------------------------------------------------------------|"
                           timesRebaixados (ordenar dadosPontos) 0
                           putStrLn "|Pressione <Enter> para voltar ao menu...                                |"
                           getChar
                           menu dados dadosPontos

resultado dados dadosPontos '9' = do
                           putStrLn "|------------------------------------------------------------------------|"
                           putStrLn "|                  Classificação geral do campeonato                     |" 
                           putStrLn "|------------------------------------------------------------------------|"
                           classificGeralCampeonato (reverse(ordenar dadosPontos)) 1
                           putStrLn "|Pressione <Enter> para voltar ao menu...                                |"
                           getChar
                           menu dados dadosPontos

resultado dados dadosPontos '0' = do

                                exitSuccess
                         
--Semelhante ao default do switch case
resultado dados dadosPontos _ = do
                           putStrLn "|------------------------------------------------------------------------|"
                           putStrLn "|Digite uma opção valida!                                                |" 
                           putStrLn "|Pressione <Enter> para voltar ao menu...                                |" 
                           getChar
                           menu dados dadosPontos

{-Área de funções que pegam nome, pontos ou gols dos times para os calculos-}

-- Função que retorna quantos pontos que o time que está dentro da tabela de pontuações fez
obterPontos :: Pontuacao -> PontosTime
obterPontos (Pontuacao _ pontos) = pontos

-- Função que retorna o nome do time que está dentro da tabela de pontuações
obterNomeTime :: Pontuacao -> TimePonto
obterNomeTime (Pontuacao nome _ ) = nome

-- Função que retorna o nome do time mandante
obterNome :: Jogo -> Time
obterNome (Jogo _ time _ _ _) = time

-- função que retorna o nome do time visitante
obterNome2 :: Jogo -> Time2
obterNome2 (Jogo _ _ _ time2 _) = time2

-- Função que retorna os gols do time mandante
obterGols :: Jogo -> Gols
obterGols (Jogo _ _ gols _ _) = gols 

--Função que retorna os gols do time visitante
obterGols2 :: Jogo -> Gols2
obterGols2 (Jogo _ _ _ _ gols2) = gols2  

--Função que retorna a rodada dos jogos
obterRodada :: Jogo -> Rodada
obterRodada (Jogo rodada _ _ _ _) = rodada  

{-Área das funções que realizam a interação de infomações com o usuario-}

-- Função que retorna o as informações de vitoria, derrota e empate
aux :: Pontuacoes -> String -> Int -> Int -> Int -> IO ()
aux dadosPontos nomeTime vit der emp = do
    if existeTime dadosPontos nomeTime then do
        putStrLn "|------------------------------------------------------------------------|"
        putStrLn "|                       Progresso do time no campeonato                  |"
        putStrLn "|------------------------------------------------------------------------|"
        putStrLn("|Time: " ++ nomeTime++ "\t                                                  |")
        putStrLn("|Vitorias: " ++ show(vit)++"\t                                                  |")
        putStrLn("|Derrotas: " ++ show(der)++"\t                                                  |")
        putStrLn("|Empates: "  ++ show(emp)++"\t                                                  |")
    else putStrLn"|Nome do time não existe                                           |"

-- Função que mostra a classificação do campeonato
classTimeX :: Pontuacoes -> String -> Int-> IO ()
classTimeX [] _ _ = return ()
classTimeX (x:xs) nomeDoTime count = do
    if existeTime (x:xs) nomeDoTime then do
        if  nomeDoTime == obterNomeTime x then do
            putStrLn ("|"++nomeDoTime ++ " " ++ "Posição " ++ (show(count)))
         else classTimeX xs nomeDoTime (count+1)
    else putStrLn "|Nome do time não existe                                                        |"

--Função que retorna o aproveitamento do time
aproveitamentoTime :: Tabelas -> Pontuacoes -> String -> Int -> IO ()
aproveitamentoTime [] _ _ _= return ()
aproveitamentoTime (x:xs) dadosPontos nome pontos = do
    if existeTime dadosPontos nome then do
        let pontosPossiveis = 114
        let aproveitamentoTime =  fromIntegral pontos/pontosPossiveis *100
        putStrLn $ "|Aproveitamento: " ++ show aproveitamentoTime
    else putStrLn "|Nome do time não existe                                                       |"

-- Função que que imprime o saldo de gols do time
imprimeSaldo :: Pontuacoes -> String -> Int-> Int -> IO()
imprimeSaldo dadosPontos nome golsFeitos golsTomados = do
     if existeTime dadosPontos nome then do
                let totalGols =  golsFeitos - golsTomados 
                putStrLn("|O time "++nome++" "++"Fez um saldo de gols no total de: "++ show(totalGols)++ " gols")
     else putStrLn "|Nome do time não existe                                                        |"

-- Retorna a partida da rodada informada do time
-- A função recebe como paramentro os dados da tabela e duas Strings sendo a rodada e o nome dos time 
-- Exemplo de retorno  Vasco 1 X Flamengo 2 
resultadoPartida :: Tabelas -> Pontuacoes -> String-> String -> IO()
resultadoPartida [] _ _ _ = return ()
resultadoPartida (x:xs) dadosPontos pesquisaRodada pesquisaTime = do
    if existeTime dadosPontos pesquisaTime then do
                if (pesquisaRodada == (show (obterRodada x)) && pesquisaTime == obterNome x || pesquisaRodada == (show (obterRodada x)) && pesquisaTime == obterNome2 x) then do
                putStr("|Jogando em casa "++ "\t       Jogando fora \n" )
                putStrLn ("|"++(obterNome x)++" "++(show( obterGols x )) ++" Gols"++"     \tX \t"++ (obterNome2 x)++" "++(show  (obterGols2 x))++" Gols")
                else if (pesquisaRodada  == [] || pesquisaTime == []) then putStrLn "Entrada invalida"
                else do  resultadoPartida xs dadosPontos pesquisaRodada pesquisaTime
    else putStrLn "|Time não existe                                                        |"

--Função que imprime ao usuario os pontos do time informado 
imprimePontos :: Pontuacoes -> String -> Int -> IO()
imprimePontos dadosPontos nomeTime pontos = do
     if existeTime dadosPontos nomeTime then do
                putStrLn ("|O time: "++ nomeTime ++ " possui "++ show(pontos)++ " pontos")
     else  putStrLn "|Time não existe                                                      |"

-- Função que mostra os 3 primeiros colocados no campeonato
-- Pegando do arquivo em ordem decrescente ele, utilizando de um count enquanto ele 
-- Não for igual a três ele vai pegando os nomes dos times 
time3primeiros :: Pontuacoes->Int -> IO()
time3primeiros [] _ = return ()
time3primeiros (x:xs) count = do 
                if(count /= 3) then do 
                    putStrLn("| "++show(count+1)++"."++obterNomeTime x ++ " - " ++ (show (obterPontos x))++ " pontos")
                    time3primeiros xs (count+1)
                else do return ()

-- Função que mostra os 3 times rebaixados do campeonato
-- Essa função acontece a mesma coisa que na anterior de pegar o nome dos 3 primeiros
timesRebaixados :: Pontuacoes->Int -> IO()
timesRebaixados [] _ = return ()
timesRebaixados (x:xs) count = do 
                if(count /= 3) then do 
                   putStrLn("| "++show(10-count)++"."++obterNomeTime x ++ " - " ++ (show (obterPontos x))++ " pontos")
                   timesRebaixados xs (count+1)
                else do return ()

-- Função que exibe a classificação geral dos times
classificGeralCampeonato :: Pontuacoes -> Int ->IO()
classificGeralCampeonato [] _= return ()
classificGeralCampeonato (x:xs) count = do
    putStrLn("| "++show(count)++"."++obterNomeTime x ++ " - " ++ (show (obterPontos x))++ " pontos")
    classificGeralCampeonato xs (count+1) 
  

{-Area das funções auxiliares de calculos de pontos, gols, vitorias, empates e derrotas-}

--Função que contabiliza as vitorias de cada equiepe no campeonato                      
vitoriasDoTime :: Tabelas -> String -> Int -> IO Int
vitoriasDoTime [] _ vitorias  = return vitorias
vitoriasDoTime (x:xs) nomeTime vitorias = do 
                -- comparação do nome digitado pelo usuario com os nomes cadastrados 
                -- comparação feita para os times que jogam em casa
                if (nomeTime == obterNome x) then do
                    -- estrutura de if´s else´s onde é prodessado o progresso dos jogos em casa do time
                    -- adicionando vitoria
                         if(obterGols x > obterGols2 x) then vitoriasDoTime xs nomeTime (vitorias+1)
                         else 
                             vitoriasDoTime xs nomeTime vitorias
                 -- comparação do nome digitado pelo usuario com os nomes cadastrados 
                -- comparação feita para os times que jogam fora de casa
                else if (nomeTime == obterNome2 x) then do
                     -- estrutura de if´s else´s onde é prodessado o progresso dos jogos fora casa do time
                    -- adicionando vitoria
                         if(obterGols2 x > obterGols x) then vitoriasDoTime xs nomeTime (vitorias+1)  
                         else  
                            vitoriasDoTime xs nomeTime vitorias           
                else do vitoriasDoTime xs nomeTime vitorias

--Função que contabiliza as derrotas de cada equiepe no campeonato 
derrotasDoTime :: Tabelas -> String -> Int -> IO Int
derrotasDoTime [] _ derrotas  = return derrotas
derrotasDoTime (x:xs) nomeTime derrotas = do 
                -- comparação do nome digitado pelo usuario com os nomes cadastrados 
                -- comparação feita para os times que jogam em casa
                if (nomeTime == obterNome x) then do
                    -- estrutura de if´s else´s onde é prodessado o progresso dos jogos em casa do time
                    -- adicionando derrota
                         if(obterGols x < obterGols2 x) then derrotasDoTime xs nomeTime (derrotas+1)
                         else 
                             derrotasDoTime xs nomeTime derrotas
                 -- comparação do nome digitado pelo usuario com os nomes cadastrados 
                -- comparação feita para os times que jogam fora de casa
                else if (nomeTime == obterNome2 x) then do
                     -- estrutura de if´s else´s onde é prodessado o progresso dos jogos fora casa do time
                    -- adicionando derrota
                         if(obterGols2 x < obterGols x) then derrotasDoTime xs nomeTime (derrotas+1)  
                         else  
                            derrotasDoTime xs nomeTime derrotas           
                else do vitoriasDoTime xs nomeTime derrotas

--Função que contabiliza os empates de cada equiepe no campeonato     
empatesDoTime :: Tabelas -> String -> Int -> IO Int
empatesDoTime [] _ empates  = return empates
empatesDoTime (x:xs) nomeTime empates = do 
                -- comparação do nome digitado pelo usuario com os nomes cadastrados 
                -- comparação feita para os times que jogam em casa
                if (nomeTime == obterNome x) then do
                    -- estrutura de if´s else´s onde é prodessado o progresso dos jogos em casa do time
                    -- adicionando empate
                         if(obterGols x == obterGols2 x) then empatesDoTime xs nomeTime (empates+1)
                         else 
                             empatesDoTime xs nomeTime empates
                 -- comparação do nome digitado pelo usuario com os nomes cadastrados 
                -- comparação feita para os times que jogam fora de casa
                else if (nomeTime == obterNome2 x) then do
                     -- estrutura de if´s else´s onde é prodessado o progresso dos jogos fora casa do time
                    -- adicionando empate
                         if(obterGols2 x == obterGols x) then empatesDoTime xs nomeTime (empates+1)
                         else  
                          empatesDoTime xs nomeTime empates           
                else do empatesDoTime xs nomeTime empates


-- Função que retorna o saldo de gols realizados pelo time X 
saldoGolsFeitos :: Tabelas ->  String ->  Int -> IO Int
saldoGolsFeitos [] _ re= return re
saldoGolsFeitos (y:ys) time gols = do
                    if (time == obterNome y) then do 
                        let golsdoTime =  gols + obterGols y
                        saldoGolsFeitos ys time golsdoTime
                    else if (time == obterNome2 y) then do 
                        let golsdoTime =  gols + obterGols2 y
                        saldoGolsFeitos ys time golsdoTime
                    else saldoGolsFeitos ys time gols

-- Função que retorna o saldo de gols tomados pelo time adversario do time X
saldoGolsTomados :: Tabelas ->  String ->  Int -> IO Int
saldoGolsTomados [] _ re= return re
saldoGolsTomados (y:ys) time gols = do
                    if (time == obterNome y) then do 
                        let golsAdversario =  gols + obterGols2 y
                        saldoGolsTomados ys time gols
                    else if (time == obterNome2 y) then do 
                        let golsAdversario =  gols + obterGols y
                        saldoGolsTomados ys time golsAdversario
                    else saldoGolsTomados ys time gols

-- Função que retorna os pontos de um determinado time informado 
-- Um exemplo caso queira saber os pontos do inter ela retorna Inter fez 40 pontos
pontosDoTimeX :: Tabelas -> String  -> Int-> IO Int
pontosDoTimeX [] _  pontos= return (pontos)
pontosDoTimeX (x:xs) nomeDoTime  pontos= do 
                if (nomeDoTime == obterNome x) then do

                         if(obterGols x > obterGols2 x) then do 
                         pontosDoTimeX xs nomeDoTime (pontos+3)
                          else if (obterGols x == obterGols2 x) then do 
                         pontosDoTimeX xs nomeDoTime(pontos+1)
                         else pontosDoTimeX xs nomeDoTime pontos

                else if (nomeDoTime == obterNome2 x) then do

                         if(obterGols2 x > obterGols x) then do 
                         pontosDoTimeX xs nomeDoTime (pontos+3)
                         else if (obterGols2 x == obterGols x) then do 
                         pontosDoTimeX xs nomeDoTime (pontos+1)
                         else pontosDoTimeX xs nomeDoTime pontos
                         
                else pontosDoTimeX xs nomeDoTime pontos


{-Área das funções auxiliares de criação de arquivos,leitura de arquevos, validação do nome dos times e ordenação-}

-- Escreve no arquivo os pontos dos time mandante
escreveArquivoPontosTime1 ::  Pontuacoes -> Tabelas -> IO ()
escreveArquivoPontosTime1 _ [] = return ()
escreveArquivoPontosTime1 dados (x:xs) 
                |((obterRodada x) == 1) =  do
                                        arq2 <- openFile "./DadosTexto/pontos.txt" WriteMode -- abre o arquivo para escrita
                                        n <- (pontosDoTimeX (x:xs) (obterNome2 x) 0)
                                        hPutStrLn arq2 (show ((Pontuacao (obterNome2 x) n):dados))
                                        hClose arq2
                                        --print (obterNome2 x)
                                        arq2 <- openFile "./DadosTexto/pontos.txt" ReadMode;
                                        dados2 <- hGetLine arq2;
                                        hClose arq2;
                                        escreveArquivoPontosTime1 (read dados2) xs
                |otherwise = escreveArquivoPontosTime1 dados xs

-- Escreve no arquivo os pontos dos time visitante
escreveArquivoPontosTime2 ::  Pontuacoes -> Tabelas -> IO ()
escreveArquivoPontosTime2 _ [] = return ()
escreveArquivoPontosTime2 dados (x:xs) 
                |((obterRodada x) == 1) =  do
                                        arq3 <- openFile "./DadosTexto/pontos.txt" WriteMode -- abre o arquivo para escrita
                                        z <- (pontosDoTimeX (x:xs) (obterNome x) 0)
                                        hPutStrLn arq3 (show ((Pontuacao (obterNome x) z):dados))
                                        hClose arq3
                                        arq3 <- openFile "./DadosTexto/pontos.txt" ReadMode;
                                        dados3 <- hGetLine arq3;
                                        hClose arq3;
                                        escreveArquivoPontosTime2 (read dados3) xs
                |otherwise = escreveArquivoPontosTime2 dados xs

-- Função que irá criar o conteudo de pontos com a pontuação de todos os times
criartabelatimes :: IO ()
criartabelatimes = do
       arq <- openFile "./DadosTexto/database.txt" ReadMode;
              dadosJogos <- hGetLine arq;
              hClose arq;
        escreveArquivoPontosTime1 [] (read dadosJogos);
        arq2 <- openFile "./DadosTexto/pontos.txt" ReadMode;
              dados <- hGetLine arq2;
              hClose arq2;
        escreveArquivoPontosTime2  (read dados)  (read dadosJogos);
             arqJogos <- openFile "./DadosTexto/pontos.txt" ReadMode;
             dadosPontos <- hGetLine arqJogos;
             hClose arqJogos;
             menu (read dadosJogos) (read dadosPontos);
        return()

-- Função que irá ler o arquivo de pontos caso ele ja tenha conteudo 
apenasLe :: IO ()
apenasLe = do
        arq <- openFile "./DadosTexto/database.txt" ReadMode;
              dadosJogos <- hGetLine arq;
              hClose arq; 
        arq2 <- openFile "./DadosTexto/pontos.txt" ReadMode;
              dadosPontos <- hGetLine arq2;
              menu  (read dadosJogos) (read dadosPontos);
        return()

--Função que serve para confirmar se existe o time informado pelo usuario, 
--servindo para garantir que ele não digite por exemplo swkdwkd e o programa volte uma informação
existeTime :: Pontuacoes -> String -> Bool
existeTime [] _ = False
existeTime ((Pontuacao timePonto pontosTime):xs) nomeTime
 | nomeTime == timePonto = True
 | otherwise = existeTime xs nomeTime

-- Função otimizada para ordenar os times
ordenar :: Pontuacoes -> Pontuacoes
ordenar dados = sortBy (compare `on` obterPontos) dados

