module UriExample where

import Control.Monad (void)
import Control.Monad.Fail.Utils
import Data.Char (isDigit)
import Data.Function ((&))
import Data.Word (Word16)
import Network.URI
import Text.ParserCombinators.ReadP

parsePostgresURI :: (MonadFail m) => String -> m (String, String, String, Word16, String)
parsePostgresURI uristr = do
  uri <- parseURI uristr & failNothing ("parseURI: can not parse: " <> uristr)
  failWhen ("scheme must be postgresql: " <> uristr) (uriScheme uri /= "postgresql:")
  auth <- failNothing ("no authority (=hostname, username, password, port): " <> uristr) (uriAuthority uri)
  (user, pass) <- failReadS ("can not parse user/password: " <> uristr) (readP_to_S parseUserInfo (uriUserInfo auth))
  port <-
    if null (uriPort auth)
      then pure 5432
      else failReadS ("can not parse port: " <> uristr) (readP_to_S parsePort (uriPort auth))
  database <- failReadS ("can not parse database: " <> uristr) (readP_to_S parseDatabase (uriPath uri))
  pure (user, pass, uriRegName auth, port, database)

parseUserInfo :: ReadP (String, String)
parseUserInfo = parseUserPass <++ parseUser
  where
    parseUserPass = do
      user <- munch1 (/= ':')
      void (char ':')
      pass <- munch1 (/= '@')
      void (char '@')
      pure (user, pass)

    parseUser = do
      user <- munch1 (/= '@')
      void (char '@')
      pure (user, "")

parseDatabase :: ReadP String
parseDatabase = do
  void (char '/')
  db <- munch1 (/= '/')
  optional (char '/')
  pure db

parsePort :: ReadP Word16
parsePort = do
  void (char ':')
  read <$> munch1 isDigit
