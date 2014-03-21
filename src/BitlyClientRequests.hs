{-
Created       : 2014 Mar 03 (Mon) 20:39:50 by Harold Carr.
Last Modified : 2014 Mar 21 (Fri) 16:14:10 by Harold Carr.
-}

{-# LANGUAGE TemplateHaskell #-}

module BitlyClientRequests
       ( Request
       , makeRequestUrl
       , mkExpandRequest
       , mkInfoRequest
       , mkLinkLookupRequest
       , mkShortenRequest
       , mkLinkEditRequest
       )
       where

import           BitlyClientTH as BCTH

data Request
  = ExpandRequest { shortUrl :: [String] -- [URI]
                  , hash     :: [String]
                  }
  | InfoRequest { hash        :: [String]
                , shortUrl    :: [String] -- URI
                , expand_user :: Maybe Bool
                }
  | Link_SL_LookupRequest { url :: [String]
                          }
  | ShortenRequest { longUrl :: String
                   , domain  :: Maybe String
                   }
  | User_SL_Link_EditRequest { link     :: String
                             , title    :: Maybe String
                             , note     :: Maybe String
                             , private  :: Maybe Bool
                             , user_ts  :: Maybe Int
                             , archived :: Maybe Bool
                             , edit     :: [String]
                             }
  deriving (Eq, Show)

BCTH.mk ''Request

mkExpandRequest :: [String] -> [String] -> Request
mkExpandRequest shortUrl hash = ExpandRequest shortUrl hash

mkInfoRequest :: [String] -> [String] -> Maybe Bool -> Request
mkInfoRequest hash shortUrl expand_user = InfoRequest hash shortUrl expand_user

mkLinkLookupRequest :: [String] -> Request
mkLinkLookupRequest url = Link_SL_LookupRequest url

mkShortenRequest :: String -> Maybe String -> Request
mkShortenRequest longUrl  domain = ShortenRequest longUrl domain

mkLinkEditRequest :: String -> Maybe String -> Maybe String -> Maybe Bool -> Maybe Int -> Maybe Bool -> [String] -> Request
mkLinkEditRequest link title note private user_ts archived edit = User_SL_Link_EditRequest link title note private user_ts archived edit

-- End of file.
