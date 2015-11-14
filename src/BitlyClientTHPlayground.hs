{-
Created       : 2014 Mar 06 (Thu) 17:12:50 by Harold Carr.
Last Modified : 2014 May 17 (Sat) 14:45:46 by Harold Carr.
-}

{-# LANGUAGE TemplateHaskell #-}

module BitlyClientTHPlayground where

import           BitlyClientCommon
import           BitlyClientRequests
import           BitlyClientTH

{-
cd ~/.sync/.esync/java/org/openhc/bitly-client/src/
ghci -XTemplateHaskell
:m + Language.Haskell.TH

runQ [d| data Foo a b = Bar a | Baz b deriving (Eq, Show) |]
[DataD [] Foo_0 [PlainTV a_3,PlainTV b_4] [NormalC Bar_2 [(NotStrict,VarT a_3)],NormalC Baz_1 [(NotStrict,VarT b_4)]] [GHC.Classes.Eq,GHC.Show.Show]]

runQ [| \x -> concat [[1],[x]] |]
LamE [VarP x_0] (AppE (VarE GHC.List.concat) (ListE [ListE [LitE (IntegerL 1)],ListE [VarE x_0]]))

-- calling runQ worked at one time, not right now
runQ [| \(ShortenRequest l d) -> mru "shorten" (concat [(zr "longUrl"  [l]), (zr "domain" [d])]) |]
LamE [ConP Shorten [VarP l_0,VarP d_1]]
     (AppE (AppE (VarE BitlyClientTH.mru) (LitE (StringL "shorten")))
      (AppE (VarE GHC.List.concat)
       (ListE
        [AppE (AppE (VarE BitlyClientTH.zr) (LitE (StringL "longUrl"))) (ListE [VarE l_0])
        ,AppE (AppE (VarE BitlyClientTH.zr) (LitE (StringL "domain")))  (ListE [VarE d_1])])))

runQ [| \(Expand s h) -> mru "shorten" (concat [(zr "shortUrl"  s), (zr "domain" h)]) |]
LamE [ConP Expand [VarP s_4,VarP h_5]]
     (AppE (AppE (VarE BitlyClientTH.mru) (LitE (StringL "shorten")))
      (AppE (VarE GHC.List.concat)
       (ListE
        [AppE (AppE (VarE BitlyClientTH.zr) (LitE (StringL "shortUrl"))) (VarE s_4)
        ,AppE (AppE (VarE BitlyClientTH.zr) (LitE (StringL "domain")))   (VarE h_5)])))

runQ [| \(LinkEdit l t n p u a e) -> mru "linkedit" (concat [(zr "link" [l]), (zr "title" (showNothing t)), (zr "note" (showNothing n)), (zr "private" (showNothing p)), (zr "user_ts" (showNothing u)), (zr "archived" (showNothing a)), (zr "edit" e)]) |]
LamE [ConP LinkEdit [VarP l_7,VarP t_8,VarP n_9,VarP p_10,VarP u_11,VarP a_12,VarP e_13]]
     (AppE (AppE (VarE BitlyClientTH.mru) (LitE (StringL "linkedit")))
      (AppE (VarE GHC.List.concat)
       (ListE
        [AppE (AppE (VarE BitlyClientTH.zr) (LitE (StringL "link"))) (ListE [VarE l_7])
        ,AppE (AppE (VarE BitlyClientTH.zr) (LitE (StringL "title")))
        ,AppE (AppE (VarE BitlyClientTH.zr) (LitE (StringL "note"))) (AppE (VarE BitlyClientTH.showNothing) (VarE n_9))
        ,AppE (AppE (VarE BitlyClientTH.zr) (LitE (StringL "private"))) (AppE (VarE BitlyClientTH.showNothing) (VarE p_10))
        ,AppE (AppE (VarE BitlyClientTH.zr) (LitE (StringL "user_ts"))) (AppE (VarE BitlyClientTH.showNothing) (VarE u_11))
        ,AppE (AppE (VarE BitlyClientTH.zr) (LitE (StringL "archived"))) (AppE (VarE BitlyClientTH.showNothing) (VarE a_12))
        ,AppE (AppE (VarE BitlyClientTH.zr) (LitE (StringL "edit"))) (VarE e_13)
        ])))
-}

------------------------------------------------------------------------------

-- http://www.haskell.org/haskellwiki/Template_Haskell#Why_does_runQ_crash_if_I_try_to_reify_something.3F
{-
$(stringE . show =<< reify ''Request)

TyConI (DataD [] TH.Request [] [RecC TH.Expand   [(TH.shortUrl,NotStrict,AppT ListT (ConT GHC.Base.String))
                                                 ,(TH.hash,NotStrict,AppT ListT (ConT GHC.Base.String))]
                               ,RecC TH.Shorten  [(TH.longUrl,NotStrict,ConT GHC.Base.String)
                                                 ,(TH.domain,NotStrict,ConT GHC.Base.String)]
                               ,RecC TH.LinkEdit [(TH.link,NotStrict,ConT GHC.Base.String)
                                                 ,(TH.title,NotStrict,AppT (ConT Data.Maybe.Maybe) (ConT GHC.Base.String))
                                                 ,(TH.note,NotStrict,AppT (ConT Data.Maybe.Maybe) (ConT GHC.Base.String))
                                                 ,(TH.private,NotStrict,AppT (ConT Data.Maybe.Maybe) (ConT GHC.Types.Bool))
                                                 ,(TH.user_ts,NotStrict,AppT (ConT Data.Maybe.Maybe) (ConT GHC.Types.Int))
                                                 ,(TH.archived,NotStrict,AppT (ConT Data.Maybe.Maybe) (ConT GHC.Types.Bool))
                                                 ,(TH.edit,NotStrict,AppT ListT (ConT GHC.Base.String))]]
        [])
-}


-- End of file.
