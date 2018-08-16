{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Applicative
import           Data.Text           (Text)
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/input InputR GET
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

data Person = Person
    { personName :: Text
    , personAge   :: Int
    }
    deriving Show

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
    <!DOCTYPE html>
    <html>
      <head>
        <title>Eta is running on Yesod!
      <body style="background-color: #2cd4d9">
        <div class="box">
          <h1 style="color: #ffffff; text-align: center;">Yesod form example in Eta!
          <h3 style="color: #ffffff; text-align: center;">
          <form action=@{InputR}>
              <p style="color:white">
                  Name
                  <input type=text name=name>
                  Age
                  <input type=text name=age>
                  <input type=submit value="Submit">
      <style>
        .box{
             width: 400px;
             background: #43414e;
             border-radius: 10px;
             padding: 60px;
             margin: auto;
             position: relative;
             top: 150px;
             -webkit-box-shadow: 0 10px 6px -6px #777;
             -moz-box-shadow: 0 10px 6px -6px #777;
             box-shadow: 0 10px 6px -6px #777;
            }
        input[type=text], select {
            width: 100%;
            padding: 12px 20px;
            margin: 8px 0;
            display: inline-block;
            border: 1px solid #ccc;
            border-radius: 4px;
            box-sizing: border-box;
           }
       input[type=submit] {
           width: 100%;
           background-color: #2cd4d9;
           color: white;
           padding: 14px 20px;
           margin: 8px 0;
           border: none;
           border-radius: 4px;
           cursor: pointer;
          }
       input[type=submit]:hover {
           background-color: #3a9699;
       }

    |]

getInputR :: Handler Html
getInputR = do
    person <- runInputGet $ Person
                <$> ireq textField "name"
                <*> ireq intField "age"
    defaultLayout [whamlet|<p>#{show person}|]

main :: IO ()
main = warp 3000 App
