{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)

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
      html $ T.concat ["<h2>Bem-vindo, ", name, "!</h2><a href=\"/quiz?name=", name, "\">Começar Quiz</a>"]

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
          liftIO $ updateLeaderboard leaderboard (name, 1) -- Atualiza pontuação
          html "<h2>Resposta Correta!</h2><a href=\"/quiz\">Voltar ao quiz</a><br><a href=\"/leaderboard\">Ver Leaderboard</a>"
        else html "<h2>Resposta Incorreta.</h2><a href=\"/quiz\">Tentar de novo</a>"

    -- Página do leaderboard
    get "/leaderboard" $ do
      lb <- liftIO $ readIORef leaderboard
      let leaderboardHtml = T.concat $ map (\(n, s) -> T.concat ["<div>", n, ": ", T.pack (show s), " ponto(s)</div>"]) lb
      html $ T.concat ["<h1>Leaderboard</h1>", leaderboardHtml, "<a href=\"/quiz\">Voltar ao Quiz</a>"]

-- Função para atualizar o leaderboard
updateLeaderboard :: IORef Leaderboard -> (Text, Int) -> IO ()
updateLeaderboard lbRef (name, score) = do
  lb <- readIORef lbRef
  let updatedLb = (name, score) : lb
  writeIORef lbRef updatedLb
