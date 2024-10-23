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
Primeira versão/teste com a biblioteca Scotty, decidi começar implementando tanto a parte Web quanto a lógica do quiz. Sofri bastante até conseguir chegar em um protótipo minimamente funcional. Adicionei também bibliotecas para manipulação de strings.

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
  leaderboard <- newIORef [] 
  scotty 3000 $ do
    middleware logStdoutDev

    get "/" $ do
      file "static/register.html"

    post "/start" $ do
      name <- formParam "name" :: ActionM Text 
      liftIO $ updateLeaderboard leaderboard (name, 0) 
      welcomeHtml <- liftIO $ TIO.readFile "static/welcome.html"
      let responseHtml = T.replace "{name}" name welcomeHtml
      html responseHtml

    get "/quiz" $ do
      name <- queryParam "name" :: ActionM Text 
      file "static/quiz.html"

    post "/submit" $ do
      answer <- formParam "answer" :: ActionM Text 
      name <- formParam "name" :: ActionM Text 
      let correctAnswer = "Brasília"
      if answer == correctAnswer
        then do
          liftIO $ updateLeaderboard leaderboard (name, 1) 
          correctHtml <- liftIO $ TIO.readFile "static/correct.html"
          let responseHtml = T.replace "{name}" name correctHtml
          html responseHtml
        else do
          liftIO $ updateLeaderboard leaderboard (name, 0) 
          incorrectHtml <- liftIO $ TIO.readFile "static/incorrect.html"
          let responseHtml = T.replace "{name}" name incorrectHtml
          html responseHtml

    get "/leaderboard" $ do
      lb <- liftIO $ readIORef leaderboard
      let sortedLb = sortBy (compare `on` Down . snd) lbdecrescente
      let leaderboardHtml = T.concat $ map (\(n, s) -> T.concat ["<div>", n, ": ", T.pack (show s), " ponto(s)</div>"]) sortedLb
      leaderboardTemplate <- liftIO $ TIO.readFile "static/leaderboard.html"
      let responseHtml = T.replace "{scores}" leaderboardHtml leaderboardTemplate
      html responseHtml

updateLeaderboard :: IORef Leaderboard -> (Text, Int) -> IO ()
updateLeaderboard lbRef (name, score) = do
  lb <- readIORef lbRef
  let updatedLb = case lookup name lb of
                    Just currentScore -> (name, currentScore + score) : filter ((/= name) . fst) lb 
                    Nothing           -> (name, score) : lb 
  writeIORef lbRef updatedLb

```

Segunda versão já com o esqueleto pronto, utilizei o chat GPT para algumas funções relacionadas à renderização dos arquivos HTML. Também foram adicionadas bibliotecas para manipulação de listas.

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
type Question = (Text, [Text], Int) 

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
  leaderboard <- newIORef [] 
  currentQuestionIndex <- newIORef 0 
  scotty 3000 $ do
    middleware logStdoutDev

    get "/" $ do
      file "static/register.html"

    post "/start" $ do
      name <- formParam "name" :: ActionM Text 
      liftIO $ updateLeaderboard leaderboard (name, 0) 
      liftIO $ writeIORef currentQuestionIndex 0 
      redirect ("/quiz?name=" <> name)

    get "/quiz" $ do
      names <- queryParam "name" 
      case names of
        [] -> redirect "/" 
        (playerName:_) -> do
          index <- liftIO $ readIORef currentQuestionIndex
          let (question, alternatives, _) = questions !! index 
          let alternativesHtml = T.concat $ zipWith (\i a -> T.concat ["<div class='option'><input type='radio' id='option-", T.pack (show i), "' name='answer' value='", T.pack (show i), "' required><label for='option-", T.pack (show i), "'>", a, "</label></div>"]) [0..] alternatives

          questionTemplate <- liftIO $ TIO.readFile "static/quiz.html"

          let questionHtml = T.replace "{question}" question $
                             T.replace "{alternatives}" alternativesHtml $
                             T.replace "{name}" playerName $
                             questionTemplate

          html questionHtml

    post "/submit" $ do
      answerIndex <- formParam "answer" :: ActionM Text 
      name <- formParam "name" :: ActionM Text
      index <- liftIO $ readIORef currentQuestionIndex 
      
      let (question, alternatives, correctIndex) = questions !! index
      
      if read (T.unpack answerIndex) == correctIndex
        then do
          liftIO $ updateLeaderboard leaderboard (name, 1) 
          correctHtml <- liftIO $ TIO.readFile "static/correct.html"
          let responseHtml = T.replace "{name}" name correctHtml
          html responseHtml
        else do
          liftIO $ updateLeaderboard leaderboard (name, 0) 
          incorrectHtml <- liftIO $ TIO.readFile "static/incorrect.html"
          let responseHtml = T.replace "{name}" name incorrectHtml
          html responseHtml

      liftIO $ writeIORef currentQuestionIndex (index + 1)

      if (index + 1) < length questions
        then redirect ("/quiz?name=" <> name) 
        else redirect "/leaderboard" 
      
    get "/leaderboard" $ do
      lb <- liftIO $ readIORef leaderboard
      let sortedLb = sortBy (compare `on` Down . snd) lb -- Ordena por pontuação decrescente
      let leaderboardHtml = T.concat $ map (\(n, s) -> T.concat ["<div>", n, ": ", T.pack (show s), " ponto(s)</div>"]) sortedLb
      leaderboardTemplate <- liftIO $ TIO.readFile "static/leaderboard.html"
      let responseHtml = T.replace "{scores}" leaderboardHtml leaderboardTemplate
      html responseHtml

updateLeaderboard :: IORef Leaderboard -> (Text, Int) -> IO ()
updateLeaderboard lbRef (name, score) = do
  lb <- readIORef lbRef
  let updatedLb = case lookup name lb of
                    Just currentScore -> (name, currentScore + score) : filter ((/= name) . fst) lb 
                    Nothing           -> (name, score) : lb 
  writeIORef lbRef updatedLb

```

Terceira versão com a lógica para implementação de mais de uma questão implementada.

# Versão final em funcionamento

[![Watch the video](https://raw.githubusercontent.com/joaohsw/perso-2024b-joaohsw/main/thumbnail.png)](https://raw.githubusercontent.com/joaohsw/perso-2024b-joaohsw/main/exemplo.mp4)

# Fontes

https://hackage.haskell.org/package/scotty-0.22/docs/Web-Scotty.html \
https://www.reddit.com/r/haskell/ \
https://stackoverflow.com/questions/5944055/newline-in-haskell-string \
https://chatgpt.com/
