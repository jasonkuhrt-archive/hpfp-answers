module AxingTediusCode where



type Query = String
data Err = Err String deriving (Show)
data Object = Object String deriving (Show)
data Context a = Context a deriving (Show)



fetch :: Query -> IO [String]
fetch _ = fmap words getLine

decode :: String -> Either Err Object
decode "error" = Left $ Err "error"
decode x = Right $ Object x

contextualize :: [Object] -> IO [(Object, Context String)]
contextualize os = fmap (\ctx -> fmap (\o -> (o, Context ctx)) os) getLine
-- AKA
-- contextualize os = do
--   ctx <- getLine
--   return $ fmap (\o -> (o, Context ctx)) os



before :: Query -> IO (Either Err [(Object, Context String)])
before query = do
  ss <- fetch query
  case sequence (fmap decode ss) of
    Left err -> return $ Left err
    Right os -> do
      withContext <- contextualize os
      return $ Right withContext

after :: Query -> IO (Either Err [(Object, Context String)])
-- after query = do
--   ss <- fetch query
--   traverse contextualize (traverse decode ss) -- `traverse` could be mapM
-- OR
-- Note that =<< is being used as a section, this seems somewhat tricky!
after = (traverse contextualize . traverse decode =<<) . fetch
