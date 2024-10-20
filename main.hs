import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  -- Define your routes and handlers here
  get "/hello" $ do
    text "Quiz de geografia"
    text "Qual a capital do Brasil?"
    text "a) São Paulo"
    text "b) Rio de Janeiro"
    text "c) Brasília"
    text "d) Salvador"
