module Funcoes (
    adicionarTarefa,
    removerTarefa,
    marcarConcluida,
    listarPorCategoria,
    listarPorPrioridade,
    ordenarPorPrioridade,
    filtrarPorStatus,
    buscarPorPalavraChave,
    verificarAtrasos,
    calcularDiasRestantes,
    filtrarPorTag,
    nuvemDeTags,
    criarRelatorio,
    proximoId,
    lerCategoria,
    lerPrioridade,
    lerStatus,
    listarTarefas,
    lerEntradaValidada,
    splitOnChar,
    lerPrazo
) where
import Tipos
import qualified Data.Map as Map
import Data.Time.Calendar(Day, diffDays)
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.Char(toLower)
import System.IO (hFlush,stdout)

--essa função é de uma biblioteca que estava dando erro na hora de ser importada, pegamos a sua definição e implementamos no código
splitOnChar :: Char -> String -> [String] --divide a string utilizando um delimitador
splitOnChar _ [] = [""]
splitOnChar delim (x:xs)
    | x == delim = "" : rest
    | otherwise  = (x : head rest) : tail rest
    where
        rest = splitOnChar delim xs

--funcoes basicas
adicionarTarefa :: Tarefa -> [Tarefa] -> [Tarefa]
adicionarTarefa tarefa listaTarefas = tarefa : listaTarefas --adiciona um item do tipo Tarefa na lista dada como segundo argumento

removerTarefa :: Int -> [Tarefa] -> Either Bool [Tarefa] --remove uma tarefa da lista dada pelo Id dela, se não existir uma tarefa com esse Id, e função retorna False
removerTarefa _ [] = Left False
removerTarefa n (x:xs)
    |idTarefa x == n = Right xs
    |otherwise =
        case removerTarefa n xs of
            Left False -> Left False
            Right r -> Right (x : r)

marcarConcluida :: Int -> [Tarefa] -> Either Bool [Tarefa] --muda o status de uma tarefa dado seu Id para Concluída, se a tarefa não for encontrada, a função retorna False
marcarConcluida _ [] = Left False
marcarConcluida n (x:xs)
    |idTarefa x == n = Right (x {status = Concluida} : xs)
    |otherwise =
        case marcarConcluida n xs of
            Left False -> Left False
            Right l -> Right (x : l)

lerStatus :: String -> Maybe Status
lerStatus s = case map toLower s of
     "pendente"  -> Just Pendente
     "concluida" -> Just Concluida
     _           -> Nothing

mostrarPrazo :: Show a => Maybe a -> String --show prazo retorna "Just aaaa-mm-dd", esssa função pega um prazo (data) e retorna uma string "aaaa-mm-dd" ou "indefinido" se não conseguir achar um prazo no argumento dado
mostrarPrazo (Just p) = drop 5 (show (Just p))
mostrarPrazo Nothing = "indefinido"

listarTarefas :: [Tarefa] -> IO ()
listarTarefas tarefas =
    mapM_ (\t -> putStrLn $ "- [" ++ show (idTarefa t) ++ "] " ++ descricao t ++ " (" ++ show (status t) ++ ") Prazo: " ++  mostrarPrazo (prazo t)) tarefas

--funcoes avancadas

recursaoGenerica :: (Tarefa -> Bool) -> [Tarefa] -> [Tarefa] --as 5 funções seguintes estavam muito parecidas, então implementei essa função de alta ordem para melhorar o código, ela pega uma função com um argumento que retorna booleano e faz a recursao necessária
recursaoGenerica _ [] = []
recursaoGenerica condicao (x:xs)
    |condicao x = x : recursaoGenerica condicao xs
    |otherwise = recursaoGenerica condicao xs

listarPorCategoria :: Categoria -> [Tarefa] -> [Tarefa] --lista todas as tarefas de uma determinada categoria
listarPorCategoria c l = recursaoGenerica (\t -> categoria t == c) l

listarPorPrioridade :: Prioridade -> [Tarefa] -> [Tarefa] --lista todas as tarefas com uma determinada prioridade
listarPorPrioridade p l = recursaoGenerica (\t -> prioridade t == p) l

ordenarPorPrioridade :: [Tarefa] -> [Tarefa] --ordena uma lista de tarefas da prioridade alta até a baixa
ordenarPorPrioridade l = concat [listarPorPrioridade Alta l, listarPorPrioridade Media l, listarPorPrioridade Baixa l]

filtrarPorStatus :: Status -> [Tarefa] -> [Tarefa] --retorna uma lista com as tarefas do status desejado
filtrarPorStatus s l = recursaoGenerica (\t -> status t == s) l

buscarPorPalavraChave :: String -> [Tarefa] -> [Tarefa] --retorna uma lista com todas as tarefas com a tag desejada
buscarPorPalavraChave p l = recursaoGenerica (\t -> elem p (tags t)) l

-- gestao de prazos

verificarAtrasos :: [Tarefa] -> Day -> [Tarefa] --retorna as tarefas com prazo < data atual e status pendente
verificarAtrasos tarefas hoje =
    filter(\t -> case prazo t of
                  Just d -> d < hoje && status t == Pendente
                  Nothing -> False) tarefas


calcularDiasRestantes :: Tarefa -> Day -> Maybe Int  --calcula os dias restante para o prazo
calcularDiasRestantes t hoje =
    case prazo t of
        Just d -> Just (fromIntegral (diffDays d hoje))
        Nothing -> Nothing

--tags

filtrarPorTag :: String -> [Tarefa] -> [Tarefa]  -- pesquisa as tarefas com a tag especificada
filtrarPorTag p tarefas = recursaoGenerica (\t -> elem p (tags t)) tarefas

nuvemDeTags :: [Tarefa] -> [(String, Int)]  --retorna uma lista de tags com suas frequencias de uso
nuvemDeTags tarefas =
    let todasTags = concatMap tags tarefas
        mapa = foldr (\tag acc -> Map.insertWith (+) tag 1 acc) Map.empty todasTags
    in Map.toList mapa

-- Formatação do percentual com uma casa decimal
formatacao :: Float -> String
formatacao valor = 
    let arredonda = (fromIntegral (round (valor * 10)) / 10) :: Float
        str = show arredonda
    in if last str == '0' 
       then init str
       else str

criarRelatorio :: [Tarefa] -> IO ()
criarRelatorio tarefas = do
    -- Cálculo dos totais
    let totalTarefas = length tarefas
    let tarefasPendentes = length $ filter (\t -> status t == Pendente) tarefas
    let tarefasConcluidas = length $ filter (\t -> status t == Concluida) tarefas
    
    -- Cálculo da distribuição por categorias
    let categoriasUnicas = [Trabalho, Estudos, Pessoal, Outro]
    let contarCategoria cat = length $ filter (\t -> categoria t == cat) tarefas
    let distribuicaoCategoria = [(cat, contarCategoria cat) | cat <- categoriasUnicas]
    
    -- Exibição do relatório
    putStrLn "\nRelatório Resumido:"
    putStrLn $ "- Total de tarefas: " ++ show totalTarefas
    putStrLn $ "- Pendentes: " ++ show tarefasPendentes ++ " | Concluídas: " ++ show tarefasConcluidas
    putStrLn "- Distribuição por categoria:"
    
    -- Exibição das categorias com contagem e percentual
    mapM_ (\(cat, quantidade) -> do
        let percentual = if totalTarefas > 0 
                         then (fromIntegral quantidade / fromIntegral totalTarefas) * 100
                         else 0.0
        let pluralSuffix = if quantidade == 1 then "tarefa" else "tarefas"
        putStrLn $ "  * " ++ show cat ++ ": " ++ show quantidade ++ " " ++ pluralSuffix ++ 
                   " (" ++ formatacao percentual ++ "%)"
          ) distribuicaoCategoria

proximoId :: [Tarefa] -> Int
proximoId tarefas =
    if null tarefas
        then 1
        else maximum (map idTarefa tarefas) + 1

-- Funções para converter String em tipos
lerCategoria :: String -> Maybe Categoria
lerCategoria s = case map toLower s of
    "trabalho" -> Just Trabalho
    "estudos"  -> Just Estudos
    "pessoal"  -> Just Pessoal
    "outro"    -> Just Outro
    _          -> Nothing

lerPrioridade :: String -> Maybe Prioridade
lerPrioridade s = case map toLower s of
    "alta"   -> Just Alta
    "media"  -> Just Media
    "baixa"  -> Just Baixa
    _        -> Nothing


lerEntradaValidada :: String -> (String -> Maybe a) -> IO a
lerEntradaValidada prompt parser = do
    putStr prompt
    hFlush stdout
    entrada <- getLine
    maybe (putStrLn "Entrada inválida! Tente novamente." >> lerEntradaValidada prompt parser) 
          return 
          (parser entrada)
          
lerPrazo :: String -> Maybe Day  --le o prazo utilizando o Data.Time com o formato sendo(ano-mes-dia)
lerPrazo d = parseTimeM True defaultTimeLocale "%Y-%m-%d" d
