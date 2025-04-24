# Gerenciador de Tarefas em Haskell

Esse é um programa de gerenciamento de tarefas escrito em Haskell. O programa permite criar, organizar, consultar e modificar tarefas, que são separadas por categorias, prioridades e persistência de dados em arquivos.

## Autores

- Davi Paiva Sendin - 12421BCC004
- Gabriel Fachini - 12411BCC092
- Gabriel Henrique Carneiro Amorim - 12411BCC055

## Estrutura do Projeto

O projeto está organizado em módulos:

- **Tipos.hs**: Define os tipos algébricos (Status, Prioridade, Categoria e Tarefa).
- **Funcoes.hs**: Implementa as funções básicas e avançadas.
- **Persistencia.hs**: Implementa as funções para salvar e carregar tarefas de arquivos.
- **Testes.hs**: Contém testes manuais e teste com QuickCheck.
- **Main.hs**: Contém uma interface para o usuário via terminal.

## Requisitos

- GHC (Glasgow Haskell Compiler)
- Cabal (sistema de build e gerenciador de dependências)
- Dependências gerenciadas automaticamente pelo `cabal`

## Como Compilar o Programa no Terminal Linux

1. Certifique-se de ter o GHC e o Cabal instalados:
   ```bash
   sudo apt update
   sudo apt install ghc cabal-install
   ```

2. Navegue até a pasta do projeto (onde está o arquivo `.cabal`) e atualize os pacotes:
   ```bash
   cd /caminho/para/agendaHaskell
   cabal update
   ```

3. Compile o projeto:
   ```bash
   cabal build
   ```

4. Execute o programa:
   ```bash
   cabal run
   ```

## Como Utilizar o Programa

Ao iniciar o programa, será apresentado um menu com as seguintes opções:

1. Carregar arquivo
2. Salvar tarefas
3. Adicionar tarefa
4. Remover tarefa
5. Marcar tarefa como concluída
6. Listar todas as tarefas
7. Listar por categoria
8. Listar por prioridade
9. Ordenar por prioridade
10. Filtrar por status
11. Buscar por palavra-chave
12. Verificar tarefas atrasadas
13. Calcular dias restantes
14. Filtrar por tag
15. Gerar nuvem de tags
16. Criar relatório
17. Executar testes manuais
18. Executar testes QuickCheck
19. Sair

Para selecionar uma opção, basta digitar o número correspondente e pressionar Enter.

## Funcionalidades Implementadas

### Operações Básicas
- Adicionar tarefas
- Remover tarefas
- Marcar tarefas como concluídas

### Operações Avançadas
- Listar tarefas por categoria
- Listar tarefas por prioridade
- Ordenar tarefas por prioridade
- Filtrar tarefas por status
- Buscar tarefas por palavra-chave
- Verificar tarefas atrasadas
- Calcular dias restantes para o prazo

### Sistema de Tags
- Filtrar tarefas por tag
- Gerar nuvem de tags (lista com frequência de uso)

### Persistência de Dados
- Salvar tarefas em arquivo
- Carregar tarefas de arquivo

### Relatórios
- Gerar relatório resumido com estatísticas de tarefas

## Exemplo de Uso

1. Adicione algumas tarefas com a opção 3
2. Liste todas as tarefas na opção 6
3. Marque uma tarefa como concluída com a opção 5
4. Na opção 16 você pode gerar um relatório
5. Salve as tarefas em um arquivo na opção 2

## Validação

O programa possui um sistema impede a adição de tarefas com identificadores duplicados e valida as entradas do usuário para garantir que correspondam aos tipos definidos (categoria, prioridade, status).

## Observações

- As tarefas são salvas em formato de texto, utilizando a função `show` do Haskell.
- Para carregar tarefas de um arquivo, este deve estar no formato correto gerado pela função de salvamento.
- Por padrão, o programa tenta carregar tarefas de um arquivo chamado "tarefas.txt" na inicialização.
