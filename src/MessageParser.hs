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


-- | Message has 0 or more arguments between '(' and ')'.
-- | Signal has no argument.
data Message where
  Message :: T.Text -> [T.Text] -> Message
  Signal  :: T.Text -> Message
  deriving(Show, Eq)


api :: MonadParsec Char T.Text m => m Message
api = try (Message <$> (hspaceConsumer *> lexeme ident) <*> between (char' '(') (char' ')') (cons <|> nil))
  <|> Signal . T.pack <$> (hspaceConsumer *> many letterChar)
  where
    cons = do
      x <- lexeme ident
      xs <- many $ list'
      return $ x : xs
      
    list' = lexeme (char ',') *> lexeme ident
    -- nil can consume nothing. This should be called after cons.
    nil = hspace >> pure []
    char' = lexeme . char
      
      
      
      
      
                                                  





