module Persistencia(salvarEmArquivo, carregarDeArquivo) where
import Tipos
--salva as tarefas adicionadas em um arquivo txt com o nome especificado pelo usuario
salvarEmArquivo :: FilePath -> [Tarefa] -> IO () 
salvarEmArquivo caminho tarefas = writeFile caminho (show tarefas)

--le o arquivo txt especificado e adiciona ele a tarefa
carregarDeArquivo :: FilePath -> IO [Tarefa]
carregarDeArquivo caminho = do
   conteudo <- readFile caminho
   return (read conteudo :: [Tarefa])
