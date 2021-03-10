{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module MessageParser where

import Data.Text as T hiding(reverse)

import Text.Megaparsec hiding (parse, parseMaybe)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


hspaceConsumer :: MonadParsec Char T.Text m => m ()
hspaceConsumer = L.space (hspace1 >> pure()) (L.skipLineComment "'") (L.skipBlockComment "/'" "'/")

--lexeme :: MonadParsec Char T.Text m => m a -> m a
--lexeme  = L.lexeme spaceConsumer
lexeme :: MonadParsec Char T.Text m => m a -> m a
lexeme  = L.lexeme hspaceConsumer

many1:: MonadParsec Char T.Text m => m a -> m [a]
many1 p = do
  c <- p
  cs <- many p
  return (c : cs)

----
ident :: MonadParsec Char T.Text m => m T.Text
ident = T.pack <$> many1 letterChar


--
data API where
  API :: T.Text -> [T.Text] -> API
  deriving(Show, Eq)



api :: MonadParsec Char T.Text m => m API
api = API <$> (hspaceConsumer *> lexeme ident) <*> between (char' '(') (char' ')') (cons <|> nil)
  where
    cons = do
      x <- lexeme ident
      xs <- many $ list'
      return $ x : xs
      
    list' = lexeme (char ',') *> lexeme ident
    -- may consume nothing
    nil = hspace >> pure []
    char' = lexeme . char
      
      
      
      
      
                                                  





