module Persistencia(salvarEmArquivo, carregarDeArquivo) where
import Tipos
salvarEmArquivo :: FilePath -> [Tarefa] -> IO ()
salvarEmArquivo caminho tarefas = writeFile caminho (show tarefas)

carregarDeArquivo :: FilePath -> IO [Tarefa]
carregarDeArquivo caminho = do
   conteudo <- readFile caminho
   return (read conteudo :: [Tarefa])
