{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Data.Text.Lazy as T  -- Importa o módulo para manipular 'Text'

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  -- Define your routes and handlers aqui
  get "/hello" $ do
    text (T.pack "Quiz de geografia")  -- Converte a string para 'Text'
    text (T.pack "Qual a capital do Brasil?")
    text (T.pack "a) São Paulo")
    text (T.pack "b) Rio de Janeiro")
    text (T.pack "c) Brasília")
    text (T.pack "d) Salvador")
