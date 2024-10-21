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
             ("Qual é o maior país do mundo em extensão territorial?", ["Canadá", "Estados Unidos", "China", "Rússia"], 3)]

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
