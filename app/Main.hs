{-
Integrantes do grupo:
- Davi Paiva Sendin - 12421BCC004
- Gabriel Fachini - 12411BCC092
- Gabriel Henrique Carneiro Amorim - 12411BCC055
-}

module Main (main) where
import Tipos
import Funcoes
import Persistencia
import Testes
import System.IO (hFlush, stdout)
import Data.Time (getCurrentTime, utctDay)
import System.IO.Error (tryIOError)
import Data.Char (toLower)

criarTarefa :: [Tarefa] -> IO Tarefa --Criar uma nova tarefa a partir da entrada do usuário
criarTarefa tarefas = do
    putStr "Descrição: "
    hFlush stdout
    descricao <- getLine

    -- Leitura das entradas validades para categoria, prioridade e prazo
    categoria <- lerEntradaValidada "Digite a categoria (Trabalho/Estudos/Pessoal/Outro): " lerCategoria
    prioridade <- lerEntradaValidada "Digite a prioridade (Alta/Media/Baixa): " lerPrioridade
    prazo <- lerEntradaValidada "Prazo(ex: 2025-04-23):" $ \s -> if null s then Just Nothing else fmap Just (lerPrazo s)
    putStr "Tags (separadas por vírgula): "
    hFlush stdout
    
    tags <- fmap (splitOnChar ',') getLine -- leitura das tags

    let novoId = proximoId tarefas -- Calcula o proximo ID disponivel

    return Tarefa { -- Retorna a estrutura Tarefa preenchida
        idTarefa = novoId,
        descricao = descricao,
        status = Pendente,
        prioridade = prioridade,
        categoria = categoria,
        tags = tags,
        prazo = prazo
    }

main :: IO () -- função principal do programa
main = do
    -- Tenta carregar tarefas do arquivo com tratamento de erro
    tentativa <- tryIOError (carregarDeArquivo "tarefas.txt")
    tarefasIniciais <- case tentativa of
        Left _ -> do
            putStrLn "Arquivo 'tarefas.txt' não encontrado. Iniciando com lista vazia."
            return []
        Right t -> return t
    
    putStrLn "Bem-vindo ao Gerenciador de Tarefas!"
    case tarefasIniciais of
        [] -> putStrLn "Nenhuma tarefa encontrada."
        ts -> putStrLn $ "Tarefas carregadas: " ++ show (length ts) ++ " encontradas."

    -- Inicia o loop principal
    mainLoop tarefasIniciais

-- Exibe o menu de opções
menu :: IO ()
menu = do
    putStrLn "\nMenu de Tarefas"
    putStrLn "1. Carregar arquivo"
    putStrLn "2. Salvar tarefas"
    putStrLn "3. Adicionar tarefa"
    putStrLn "4. Remover tarefa"
    putStrLn "5. Marcar tarefa como concluída"
    putStrLn "6. Listar todas as tarefas"
    putStrLn "7. Listar por categoria"
    putStrLn "8. Listar por prioridade"
    putStrLn "9. Ordenar por prioridade"
    putStrLn "10. Filtrar por status"
    putStrLn "11. Buscar por palavra-chave"
    putStrLn "12. Verificar tarefas atrasadas"
    putStrLn "13. Calcular dias restantes"
    putStrLn "14. Filtrar por tag"
    putStrLn "15. Gerar nuvem de tags"
    putStrLn "16. Criar relatório"
    putStrLn "17. Executar testes manuais"
    putStrLn "18. Executar testes QuickCheck"
    putStrLn "19. Sair"
    putStr "Escolha uma opção: "
    hFlush stdout -- força a exibição imediata, evitando o buffer

-- Loop principal 
mainLoop :: [Tarefa] -> IO ()
mainLoop tarefas = do
    menu
    opcao <- getLine

    case opcao of
        "1" -> do -- Carrega tarefas de um arquivo especificado
            putStr "Digite o nome do arquivo para carregar: "
            hFlush stdout
            arquivo <- getLine
            novasTarefas <- tryIOError (carregarDeArquivo arquivo) 
            case novasTarefas of 
                Left _ -> do
                    putStrLn "Erro, o arquivo não foi encontrado!"
                    mainLoop tarefas
                Right novasTarefas -> do
                    putStrLn $ "Tarefas carregadas de " ++ arquivo
                    mainLoop novasTarefas

        "2" -> do -- Salvar tarefas no arquivo
            putStr "Digite o nome do arquivo para salvar: "
            hFlush stdout
            arquivo <- getLine
            salvarEmArquivo arquivo tarefas
            putStrLn $ "Tarefas salvas em " ++ arquivo
            mainLoop tarefas

        "3" -> do -- Adiciona nova tarefa
            novaTarefa <- criarTarefa tarefas
            let novas = adicionarTarefa novaTarefa tarefas
            putStrLn "Tarefa adicionada com sucesso!"
            mainLoop novas

        "4" -> do -- Remove tarefa por ID
            putStr "Digite o ID da tarefa a ser removida: "
            hFlush stdout
            idStr <- getLine
            let id = read idStr :: Int
            case removerTarefa id tarefas of
                Left False -> do
                    putStrLn "Tarefa não encontrada!"
                    mainLoop tarefas
                Right novasTarefas -> do
                    putStrLn "Tarefa removida com sucesso!"
                    mainLoop novasTarefas

        "5" -> do -- Marca tarefa como concluída
            putStr "Digite o ID da tarefa a marcar como concluída: "
            hFlush stdout
            idStr <- getLine
            let id = read idStr :: Int
            case marcarConcluida id tarefas of
                Left False -> do
                    putStrLn "Tarefa não encontrada!"
                    mainLoop tarefas
                Right novasTarefas -> do
                    putStrLn "Tarefa marcada como concluída!"
                    mainLoop novasTarefas

        "6" -> do -- Lista todas as tarefas
            listarTarefas tarefas
            mainLoop tarefas

        "7" -> do - -- Lista por categoria
            categoria <- lerEntradaValidada "Digite a categoria (Trabalho/Estudos/Pessoal/Outro): " lerCategoria
            let tarefasFiltradas = listarPorCategoria categoria tarefas
            listarTarefas tarefasFiltradas
            mainLoop tarefas

        "8" -> do -- Lista por prioridade
            prioridade <- lerEntradaValidada "Digite a prioridade (Alta/Media/Baixa): " lerPrioridade
            let tarefasFiltradas = listarPorPrioridade prioridade tarefas
            listarTarefas tarefasFiltradas
            mainLoop tarefas

        "9" -> do -- Ordena por prioridade
            let ordenadas = ordenarPorPrioridade tarefas
            listarTarefas ordenadas
            mainLoop tarefas

        "10" -> do -- Filtra por status
            status <- lerEntradaValidada "Digite o status (Pendente/Concluida): " lerStatus
            let tarefasFiltradas = filtrarPorStatus status tarefas
            listarTarefas tarefasFiltradas
            mainLoop tarefas

        "11" -> do -- Busca palavra-chave
            putStr "Digite a palavra-chave: "
            hFlush stdout
            palavra <- getLine
            let resultados = buscarPorPalavraChave palavra tarefas
            listarTarefas resultados
            mainLoop tarefas

        "12" -> do -- Retorna tarefas atrasadas
            dataAtual <- utctDay <$> getCurrentTime
            let atrasadas = verificarAtrasos tarefas dataAtual
            putStrLn "Tarefas atrasadas:"
            listarTarefas atrasadas
            mainLoop tarefas

        "13" -> do -- Calcula dias restantes da tarefa
            dataAtual <- utctDay <$> getCurrentTime
            let resultados = map (\t -> (t, calcularDiasRestantes t dataAtual)) tarefas
            mapM_ (\(t, dias) -> case dias of
                Just d -> putStrLn $ "Tarefa " ++ show (idTarefa t) ++ ": " ++ show d ++ " dias restantes."
                Nothing -> putStrLn $ "Tarefa " ++ show (idTarefa t) ++ ": Sem prazo definido."
                  ) resultados
            mainLoop tarefas

        "14" -> do -- Filtra por tag
            putStr "Digite a tag: "
            hFlush stdout
            tag <- getLine
            let tarefasFiltradas = filtrarPorTag tag tarefas
            listarTarefas tarefasFiltradas
            mainLoop tarefas

        "15" -> do -- Gera nuvem de tags
            let nuvem = nuvemDeTags tarefas
            putStrLn "Nuvem de Tags:"
            mapM_ (\(tag, count) -> putStrLn $ "  - " ++ tag ++ ": " ++ show count ++ " ocorrências") nuvem
            mainLoop tarefas

        "16" -> do -- Cria um relatorio
            criarRelatorio tarefas
            mainLoop tarefas
            
        "17" -> do -- Executa todos os testes manuais
            putStrLn "Executando testes..."
            Testes.executarTestes
            mainLoop tarefas

        "18" -> do -- Executa os teste QuickCheck
            putStrLn "Executando testes QuickCheck..."
            Testes.executarQuickCheck
            mainLoop tarefas

        "19" -> putStrLn "Até logo! :)" -- Encerra o programa

        _   -> do -- Verifica se a entreda é uma opção valida
            putStrLn "Opção inválida! Tente novamente."
            mainLoop tarefas
