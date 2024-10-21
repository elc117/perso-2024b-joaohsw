# Produção personalizada ELC117-2024b

Nome: João Henrique Scherer Wolski \
Curso: Sistemas de informação

# Tema do trabalho

Neste trabalho de faculdade, desenvolvi um serviço web utilizando Haskell e a biblioteca Scotty para criar um quiz interativo de geografia. O objetivo do projeto foi proporcionar uma ferramenta educativa que permite aos usuários testar seus conhecimentos sobre diferentes países, capitais e características geográficas. 

# Processo de desenvolvimento

**Primeira versão** \

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
Primeira versão/teste com a biblioteca Scotty, sofri um pouco para entender como funciona. \

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

Só consegui fazer o css funcionar integrando no arquivo html 