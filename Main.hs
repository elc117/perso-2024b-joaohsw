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
import Data.Function (on) -- Importa on para uso na comparação

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
