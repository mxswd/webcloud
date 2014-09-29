module Web.Cloud where

import Data.IORef
import Data.ByteString.Lazy.Char8 (pack, unpack)
import System.Environment
import Options.Applicative
import Network.CGI
import Network.CGI.Monad
import Network.CGI.Protocol
import System.Exit

execParserWebCloud :: ParserInfo a -> IO a
execParserWebCloud pinfo = do
  ref <- newIORef Nothing
  title <- (\x -> "<title>" ++ x ++ "</title>") `fmap` getProgName
  runCGI . handleErrors $ do
    setHeader "Content-Type" "text/html; charset=utf-8" 
    clouds <- cgiGet (execParserPure (prefs idm) pinfo . getCloud . cgiInputs)
    val <- mkWebCloud clouds
    case val of
      Left e -> do
        output $ title ++ "<code><pre>" ++ e
      Right v -> do
        liftIO $ writeIORef ref (Just v)
        output $ title ++ "<code><pre>"
  r <- readIORef ref
  case r of
    Just v -> return v
    Nothing -> exitWith ExitSuccess -- it's ok to error! :)

-- getCloud :: [(String, Input)]
getCloud =
  flip (>>=) $ \(k, v) ->
    if unpack (inputValue v) == ""
      then ["--" ++ k]
      else ["--" ++ k, show (inputValue v)]

mkWebCloud :: Monad m => ParserResult a -> m (Either String a)
mkWebCloud (Success a) = return (Right a)
mkWebCloud (Failure failure) = return (Left (fst (renderFailure failure "cloud")))
mkWebCloud (CompletionInvoked _) = return (Left "not web")
