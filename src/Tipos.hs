module Tipos (
        Tarefa(..),
        Status(..),
        Prioridade(..),
        Categoria(..),
        Day
)where

import Data.Time.Calendar (Day, fromGregorian)
data Status = Pendente | Concluida deriving (Show, Eq, Read)
data Prioridade = Baixa | Media | Alta deriving (Show, Eq, Ord, Read)
data Categoria = Trabalho | Estudos | Pessoal | Outro deriving (Show, Eq, Read)
data Tarefa = Tarefa
     { idTarefa :: Int
     , descricao :: String
     , status :: Status
     , prioridade :: Prioridade
     , categoria :: Categoria
     , prazo :: Maybe Day 
     , tags :: [String]
     } deriving (Show, Eq, Read)
