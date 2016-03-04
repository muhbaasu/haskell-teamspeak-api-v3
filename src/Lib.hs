{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Lib where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import           Data.List                 (intersperse)
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as Enc
import qualified Data.Text.IO              as T
import qualified Network.Socket            as NS
import qualified Pipes                     as P
import qualified Pipes.Network.TCP         as PN
import qualified Pipes.Prelude             as P

-- | Smart constructor for help commands
help :: Maybe CommandType -> Command
help mct = Help HelpCommand $ HelpParams mct

-- | Smart constructor for quit commands
quit :: Command
quit = Quit QuitCommand

-- | Smart constructor for login commmands
-- TODO: newtypes
login :: T.Text -> T.Text -> Command
login user pass = Login LoginCommand $ LoginParams user pass

-- | Commands for the server
data Command =
  Help HelpCommand HelpParams
  | Quit QuitCommand
  | Login LoginCommand LoginParams

instance Show Command where
  show (Help _ HelpParams {_helpForCommand}) =
    "help" ++ fromMaybe ""
    (((" " ++) . T.unpack . unCommandType) <$> _helpForCommand)
  show (Quit _) = "quit"
  show (Login _ LoginParams {_loginUser, _loginPass}) =
    "login " ++ T.unpack _loginUser ++ " " ++ T.unpack _loginPass

data HelpCommand = HelpCommand deriving Show
data QuitCommand = QuitCommand deriving Show
data LoginCommand = LoginCommand deriving Show

instance ToCommandType HelpCommand where
  toCommandType _ = CommandType "help"

instance ToCommandType QuitCommand where
  toCommandType _ = CommandType "quit"

instance ToCommandType LoginCommand where
  toCommandType _ = CommandType "login"

-- | Wrapper for commmand types
newtype CommandType = CommandType { unCommandType :: T.Text }
                    deriving Show

-- | Wrapper for response types
newtype ResponseType = ResponseType { unResponseType :: T.Text }
                     deriving Show

-- | Type class for converting to CommandTypes so the types can still
-- | be separate types with this common interface
class ToCommandType c where
  toCommandType :: c -> CommandType

-- | Responses from the server
data Response =
  Response
  deriving Show

-- | Push notifications from the server
data Notification =
  Notification
  deriving Show

-- | Show help for the interface or a particular command
data HelpParams = HelpParams
  { _helpForCommand :: Maybe CommandType
  } deriving Show

-- | Login parameters
data LoginParams = LoginParams
  { _loginUser :: T.Text
  , _loginPass :: T.Text
  } deriving Show

-- | Wrapper for formatted and escaped commands
newtype TsCommand = TsCommand { unTsCommand :: T.Text }
                  deriving Show

-- | Create a new TsCommand from a CommandType and the arguments
-- TODO: possibly newtype arguments
newTsCommand :: ToCommandType ct => ct -> [T.Text] -> TsCommand
newTsCommand ct params = TsCommand commandText
  where commandType = unCommandType $ toCommandType ct
        args = [commandType] ++ (escape <$> params) ++ [T.singleton '\n']
        commandText = T.concat $ intersperse (T.singleton ' ') args

-- | Wrapper for unescaped responses
newtype TsResponse = TsResponse { unTsResponse :: T.Text }
                     deriving Show

-- | Serialization interface for commands
class ToTsCommand c where
  toTsCommand :: c -> TsCommand

instance ToTsCommand Command where
  toTsCommand (Help ct HelpParams {_helpForCommand}) =
    case _helpForCommand of
      Just cmd -> newTsCommand ct [unCommandType cmd]
      Nothing -> newTsCommand ct []
  toTsCommand (Quit ct) =
    newTsCommand ct []
  toTsCommand (Login ct LoginParams {_loginUser, _loginPass}) =
    newTsCommand ct [_loginUser, _loginPass]

type Error = T.Text

-- | Deserialization interface for responses
class FromTsResponse r where
  fromTsResponse :: T.Text -> Either Error r

-- | Escape special chars according to the official server query doc
escape :: T.Text -> T.Text
escape = T.concatMap escape'
  where escape' '\\' = "\\\\"     -- Backslash
        escape' '/' = "\\/"       -- Slash
        escape' ' ' = "\\s"       -- Whitespace
        escape' '|' = "\\p"       -- Pipe
        escape' '\a' = "\\a"      -- Bell
        escape' '\b' = "\\b"      -- Backspace
        escape' '\f' = "\\f"      -- Formfeed
        escape' '\n' = "\\n"      -- Newline
        escape' '\r' = "\\r"      -- Carriage Return
        escape' '\t' = "\\t"      -- Horizontal tab
        escape' '\v' = "\\v"      -- Vertical tab
        escape' c = T.singleton c -- Regular char

-- | Return a new connected socket for the given IP and port
connect :: String -> NS.PortNumber -> IO NS.Socket
connect host port = do
  sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
  h <- NS.inet_addr host
  let addr = NS.SockAddrInet port h
  NS.connect sock addr
  return sock

-- | Create a producer which yields all messages from the server
fromServer :: MonadIO m => NS.Socket -> P.Producer' T.Text m ()
fromServer sock = PN.fromSocket sock 4096 P.>-> P.map Enc.decodeUtf8

-- | Create a consumer which sends all messages to the server
toServer :: MonadIO m => NS.Socket -> P.Consumer' T.Text m ()
toServer sock = PN.toSocket sock P.<-< P.map Enc.encodeUtf8
