# Produção personalizada ELC117-2024b

Nome: João Henrique Scherer Wolski \
Curso: Sistemas de informação

# Tema do trabalho

Neste trabalho de faculdade, desenvolvi um serviço web utilizando Haskell e a biblioteca Scotty para criar um quiz interativo de geografia. O objetivo do projeto foi proporcionar uma ferramenta educativa que permite aos usuários testar seus conhecimentos sobre diferentes países, capitais e características geográficas. 

# Processo de desenvolvimento

**Primeira versão** 

```haskell

{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as TIO

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  get "/home" $ do
    file "static/home.html"

  get "/quiz" $ do
    file "static/quiz.html"

  get "/static/:file" $ do
    fileName <- pathParam "file"  
    let filePath = "static/" ++ fileName
    file filePath

  post "/submit" $ do
    answer <- formParam "answer"  
    if answer == ("Brasília" :: Text)
      then html "<h2>Resposta Correta!</h2><a href=\"/quiz\">Voltar ao quiz</a>"
      else html "<h2>Resposta Incorreta.</h2><a href=\"/quiz\">Tentar de novo</a>"

``` 
Primeira versão/teste com a biblioteca Scotty, decidi começar implementando tanto a parte Web quanto a lógica do quiz. Sofri bastante até conseguir chegar em um protótipo minimamente funcional. \

```html

<!DOCTYPE html>
<html lang="pt-BR">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Bem-vindo ao Quiz</title>
  <style>
    body {
      font-family: Arial, sans-serif;
      background-color: #f4f4f4;
      display: flex;
      justify-content: center;
      align-items: center;
      height: 100vh;
      margin: 0;
    }
    .container {
      background-color: white;
      padding: 20px;
      border-radius: 10px;
      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
      text-align: center;
      max-width: 400px;
      width: 100%;
    }
    h1 {
      margin-bottom: 20px;
      color: #333;
    }
    p {
      margin-bottom: 20px;
      color: #666;
    }
    .btn {
      background-color: #4CAF50;
      color: white;
      padding: 10px 20px;
      text-decoration: none;
      border: none;
      border-radius: 5px;
      cursor: pointer;
      display: inline-block;
      transition: background-color 0.3s ease;
    }
    .btn:hover {
      background-color: #45a049;
    }
  </style>
</head>
<body>
  <div class="container">
    <h1>Bem-vindo ao Quiz de Geografia!</h1>
    <p>Teste seus conhecimentos sobre a capital do Brasil.</p>
    <a class="btn" href="/quiz">Iniciar Quiz</a>
  </div>
</body>
</html>

```

Por algum motivo, o CSS não estava sendo aplicado quando tentei carregá-lo a partir de um arquivo separado. Fiz várias tentativas para resolver o problema, mas nenhuma delas funcionou. Como resultado, optei por usar estilos inline diretamente no arquivo HTML para garantir que a estética do programa não fosse prejudicada.

```haskell

{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Data.List (sortBy)
import Data.Ord (Down(..))
import Data.Function (on)

type Leaderboard = [(Text, Int)]

main :: IO ()
main = do
  leaderboard <- newIORef [] -- Lista de jogadores e suas pontuações
  scotty 3000 $ do
    middleware logStdoutDev

    -- Página de registro
    get "/" $ do
      file "static/register.html"

    -- Após registrar o nome, redireciona para o quiz
    post "/start" $ do
      name <- formParam "name" :: ActionM Text -- O nome é enviado via form
      liftIO $ updateLeaderboard leaderboard (name, 0) -- Define a pontuação inicial como 0
      welcomeHtml <- liftIO $ TIO.readFile "static/welcome.html"
      let responseHtml = T.replace "{name}" name welcomeHtml
      html responseHtml

    -- Página do quiz
    get "/quiz" $ do
      name <- queryParam "name" :: ActionM Text -- Usando queryParam
      file "static/quiz.html"

    -- Submissão da resposta
    post "/submit" $ do
      answer <- formParam "answer" :: ActionM Text -- O answer é enviado via form
      name <- formParam "name" :: ActionM Text -- O name também é enviado via form
      let correctAnswer = "Brasília"
      if answer == correctAnswer
        then do
          liftIO $ updateLeaderboard leaderboard (name, 1) -- Incrementa a pontuação em 1
          correctHtml <- liftIO $ TIO.readFile "static/correct.html"
          let responseHtml = T.replace "{name}" name correctHtml
          html responseHtml
        else do
          liftIO $ updateLeaderboard leaderboard (name, 0) -- Define a pontuação como 0
          incorrectHtml <- liftIO $ TIO.readFile "static/incorrect.html"
          let responseHtml = T.replace "{name}" name incorrectHtml
          html responseHtml

    -- Página do leaderboard
    get "/leaderboard" $ do
      lb <- liftIO $ readIORef leaderboard
      let sortedLb = sortBy (compare `on` Down . snd) lb -- Ordena por pontuação decrescente
      let leaderboardHtml = T.concat $ map (\(n, s) -> T.concat ["<div>", n, ": ", T.pack (show s), " ponto(s)</div>"]) sortedLb
      leaderboardTemplate <- liftIO $ TIO.readFile "static/leaderboard.html"
      let responseHtml = T.replace "{scores}" leaderboardHtml leaderboardTemplate
      html responseHtml

-- Função para atualizar o leaderboard
updateLeaderboard :: IORef Leaderboard -> (Text, Int) -> IO ()
updateLeaderboard lbRef (name, score) = do
  lb <- readIORef lbRef
  let updatedLb = case lookup name lb of
                    Just currentScore -> (name, currentScore + score) : filter ((/= name) . fst) lb -- Atualiza a pontuação existente
                    Nothing           -> (name, score) : lb -- Adiciona novo jogador
  writeIORef lbRef updatedLb

```

Segunda versão já com o esqueleto pronto, utilizei o chat GPT para algumas funções relacionadas à renderização dos arquivos HTML.

```haskell

{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Data.List (sortBy)
import Data.Ord (Down(..))
import Data.Function (on)

type Leaderboard = [(Text, Int)]
type Question = (Text, [Text], Int) -- (Pergunta, Alternativas, Índice da resposta correta)

-- Definindo as perguntas do quiz
questions :: [Question]
questions = [("Qual é a capital da Austrália?", ["Camberra", "Sydney", "Melbourne", "Brisbane"], 0),
             ("Quantos países fazem parte da União Europeia?", ["27", "22", "18", "30"], 0),
             ("Qual é a língua oficial do Suriname?", ["Português", "Holandês", "Espanhol", "Inglês"], 1),
             ("Qual o nome da província do Canadá que é famosa pela comunidade francófona?", ["Ontário", "Alberta", "Québec", "Manitoba"], 2),
             ("Qual é o maior país do mundo em extensão territorial?", ["Canadá", "Estados Unidos", "China", "Rússia"], 2),
             ("Qual é o nome do maior deserto do mundo?", ["Saara", "Atacama", "Gobi", "Antártida"], 0),
             ("Qual é o nome do rio mais extenso do mundo?", ["Nilo", "Amazonas", "Yangtzé", "Mississipi"], 1),
             ("Qual é o nome do maior oceano do mundo?", ["Atlântico", "Índico", "Pacífico", "Ártico"], 2),
             ("Qual é o nome do maior lago do mundo?", ["Baikal", "Michigan", "Superior", "Vitória"], 0),
             ("Qual é o nome do maior arquipélago do mundo?", ["Havaí", "Japão", "Filipinas", "Indonésia"], 3),
             ("Onde fica o Monte Kilimanjaro?", ["Quênia", "Nepal", "Chile", "Tanzânia"], 3),
             ("Qual outro país da América do Sul possui o português como disciplina obrigatória do currículo escolar?", ["Argentina", "Uruguai", "Paraguai", "Venezuela"], 1),
             ("Qual país possui o maior número de habitantes no mundo?", ["Índia", "Estados Unidos", "China", "Rússia"], 0)]

main :: IO ()
main = do
  leaderboard <- newIORef [] -- Lista de jogadores e suas pontuações
  currentQuestionIndex <- newIORef 0 -- Índice da pergunta atual
  scotty 3000 $ do
    middleware logStdoutDev

    -- Página de registro
    get "/" $ do
      file "static/register.html"

    -- Após registrar o nome, redireciona para o quiz
    post "/start" $ do
      name <- formParam "name" :: ActionM Text -- O nome é enviado via formulário
      liftIO $ updateLeaderboard leaderboard (name, 0) -- Define a pontuação inicial como 0
      liftIO $ writeIORef currentQuestionIndex 0 -- Reinicia o índice da pergunta
      redirect ("/quiz?name=" <> name)

    -- Página do quiz
    get "/quiz" $ do
      names <- queryParam "name" -- `queryParam` retorna uma lista de parâmetros
      case names of
        [] -> redirect "/" -- Se não houver nome na query string, redireciona para o início
        (playerName:_) -> do
          index <- liftIO $ readIORef currentQuestionIndex
          let (question, alternatives, _) = questions !! index -- Obtém a pergunta atual
          let alternativesHtml = T.concat $ zipWith (\i a -> T.concat ["<div class='option'><input type='radio' id='option-", T.pack (show i), "' name='answer' value='", T.pack (show i), "' required><label for='option-", T.pack (show i), "'>", a, "</label></div>"]) [0..] alternatives

          -- Lê o template do quiz
          questionTemplate <- liftIO $ TIO.readFile "static/quiz.html"

          -- Substitui os placeholders pelo conteúdo real
          let questionHtml = T.replace "{question}" question $
                             T.replace "{alternatives}" alternativesHtml $
                             T.replace "{name}" playerName $
                             questionTemplate

          html questionHtml

    -- Submissão da resposta
    post "/submit" $ do
      answerIndex <- formParam "answer" :: ActionM Text -- O índice da alternativa escolhida
      name <- formParam "name" :: ActionM Text -- O nome também é enviado via formulário
      index <- liftIO $ readIORef currentQuestionIndex -- Lê o índice da pergunta atual diretamente do IORef
      
      let (question, alternatives, correctIndex) = questions !! index -- Obtém a pergunta atual corretamente
      
      if read (T.unpack answerIndex) == correctIndex
        then do
          liftIO $ updateLeaderboard leaderboard (name, 1) -- Incrementa a pontuação em 1
          correctHtml <- liftIO $ TIO.readFile "static/correct.html"
          let responseHtml = T.replace "{name}" name correctHtml
          html responseHtml
        else do
          liftIO $ updateLeaderboard leaderboard (name, 0) -- Não incrementa a pontuação
          incorrectHtml <- liftIO $ TIO.readFile "static/incorrect.html"
          let responseHtml = T.replace "{name}" name incorrectHtml
          html responseHtml

      -- Atualiza o índice da pergunta
      liftIO $ writeIORef currentQuestionIndex (index + 1)

      -- Verifica se há mais perguntas
      if (index + 1) < length questions
        then redirect ("/quiz?name=" <> name) -- Redireciona para a próxima pergunta
        else redirect "/leaderboard" -- Redireciona para o leaderboard após a última pergunta

    -- Página do leaderboard
    get "/leaderboard" $ do
      lb <- liftIO $ readIORef leaderboard
      let sortedLb = sortBy (compare `on` Down . snd) lb -- Ordena por pontuação decrescente
      let leaderboardHtml = T.concat $ map (\(n, s) -> T.concat ["<div>", n, ": ", T.pack (show s), " ponto(s)</div>"]) sortedLb
      leaderboardTemplate <- liftIO $ TIO.readFile "static/leaderboard.html"
      let responseHtml = T.replace "{scores}" leaderboardHtml leaderboardTemplate
      html responseHtml

-- Função para atualizar o leaderboard
updateLeaderboard :: IORef Leaderboard -> (Text, Int) -> IO ()
updateLeaderboard lbRef (name, score) = do
  lb <- readIORef lbRef
  let updatedLb = case lookup name lb of
                    Just currentScore -> (name, currentScore + score) : filter ((/= name) . fst) lb -- Atualiza a pontuação existente
                    Nothing           -> (name, score) : lb -- Adiciona novo jogador
  writeIORef lbRef updatedLb

```

Terceira versão com a lógica para implementação de mais de uma questão implementada.