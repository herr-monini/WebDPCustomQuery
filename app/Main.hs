{-# LANGUAGE OverloadedStrings #-}

module Main where


import Web.Scotty
import Lib
import Compiler
import Data.Text.Lazy.Encoding
import Data.Text
import qualified Data.Text.Lazy as TL
import WebDPConv.Par (myLexer, pProg)
import WebDPConv.Abs
import Network.HTTP.Types.Status
import Data.Aeson (Value (Array), (.=), object, toJSON)
import Debug.Trace
import Hjayson

type Err = Either String


main :: IO ()
main = scotty 3000 $ do
    
    editor

    help
    
    compileQuery


editor :: ScottyM() 
editor = get "/editor" $ do
        file "/project/app/editor.html"

help :: ScottyM()
help = get "/help" $ do 
    file "/project/app/help.html"

compileQuery :: ScottyM ()
compileQuery = post "/compile" $ do
        rawBody <- body
        let decodedText = (TL.toStrict . decodeUtf8) rawBody :: Text

        let program = unpack decodedText
     
        case parseProg program of 
            Left err -> do 
                status badRequest400
                text $ "Error: " <> TL.pack err
            Right tree -> do 
                case compile tree of 
                    Left err -> do 
                        status badRequest400
                        text $ "Error: " <> TL.pack err 
                    Right js -> do 
                        let res = Prelude.map (\(J _ p) -> TL.pack p) js 
                        status ok200
                        setHeader "Content-Type" "application/json"
                        text $ TL.pack (commas $ Prelude.map (\(J n q)-> curly ("\"name\"" <> ":" <> "\"" <> n <> "\"" <> "," <> "\"query\"" <> ":" <> q)) js)

                    

parseProg :: String -> Err Prog 
parseProg = pProg . myLexer 
