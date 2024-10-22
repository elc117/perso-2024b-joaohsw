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
        then redirect ("/quiz?name=" <> name) -- Redireciona para a próxima pergunta
        else redirect "/leaderboard" -- Redireciona para o leaderboard após a última pergunta

    get "/leaderboard" $ do
      lb <- liftIO $ readIORef leaderboard
      let sortedLb = sortBy (compare `on` Down . snd) lb 
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
