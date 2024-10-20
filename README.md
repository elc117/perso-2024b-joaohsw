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

``` \
Primeira versão/teste com a biblioteca Scotty, sofri um pouco para entender como funciona.