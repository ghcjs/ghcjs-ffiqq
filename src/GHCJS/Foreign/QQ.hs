{-# LANGUAGE QuasiQuotes, TemplateHaskell, DeriveDataTypeable #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}

{- | An experimental quasiquoter for more convenient FFI

     This is mostly a demonstration of the approach, not production ready!
 -}

module GHCJS.Foreign.QQ (js, js', js_, jsu, jsu', jsu_, jsi, jsi', jsi_) where

import           Control.Applicative

import           Data.Char
import           Data.List.Split
import qualified Data.List as L
import qualified Data.Map  as M
import           Data.Maybe
import           Data.Typeable

import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH

import           GHCJS.Types
import           GHCJS.Marshal.Pure

-- result: PFromJSVal a => IO a
js :: QuasiQuoter
js = mkFFIQQ False True Safe

-- result: PFromJSVal a => a
js' :: QuasiQuoter
js' = mkFFIQQ False False Safe

-- result: IO ()
js_ :: QuasiQuoter
js_ = mkFFIQQ True True Safe

-- result: PFromJSVal a => IO a
jsu :: QuasiQuoter
jsu = mkFFIQQ False True Unsafe

-- result: PFromJSVal a => a
jsu' :: QuasiQuoter
jsu' = mkFFIQQ False False Unsafe

-- result: IO ()
jsu_ :: QuasiQuoter
jsu_ = mkFFIQQ True True Unsafe

-- result: PFromJSVal a => IO a
jsi :: QuasiQuoter
jsi = mkFFIQQ False True Interruptible

-- result: PFromJSVal a => a
jsi' :: QuasiQuoter
jsi' = mkFFIQQ False False Interruptible

-- result: IO ()
jsi_ :: QuasiQuoter
jsi_ = mkFFIQQ True True Interruptible

mkFFIQQ :: Bool -> Bool -> Safety -> QuasiQuoter
mkFFIQQ isUnit isIO s = QuasiQuoter { quoteExp = jsExpQQ isUnit isIO s }

newtype QQCounter = QQCounter { getCount :: Int } deriving (Typeable, Show)

jsExpQQ :: Bool -> Bool -> Safety -> String -> Q Exp
jsExpQQ isUnit isIO s pat = do
  c <- maybe 0 getCount <$> qGetQ
  n <- newName ("__ghcjs_foreign_qq_spliced_" ++ show c)
  let (p:ps)       = linesBy (=='`') pat
      isNameCh c   = isAlphaNum c || c `elem` "_"
      names        = L.nub (map (takeWhile isNameCh) ps)
      nameMap      = M.fromList $ zip names [1..]
      ffiDecl    = ForeignD (ImportF JavaScript s pat' n (importTy returnTy (length names) []))
      importTy :: Type -> Int -> [Name] -> Type
      importTy t n xs = fst (importTy' t n xs)
      importTy' :: Type -> Int -> [Name] -> (Type, [Name])
      importTy' t 0 xs = (t,xs)
      importTy' t n xs =
        let v         = mkName ('b':show n)
            (t', xs') = importTy' t (n-1) xs
        in (AppT (AppT ArrowT (ConT ''JSVal)) t', v:xs')
      convertRes r | isUnit    = r
                   | isIO      = AppE (AppE (VarE 'fmap) (LamE [VarP $ mkName "l"] (uref (VarE (mkName "l"))))) r
                   | otherwise = uref r
        where uref r = AppE (VarE 'pFromJSVal) r
      ffiCall = convertRes (ffiCall' (VarE n) names)
      ffiCall' x []     = x
      ffiCall' f (x:xs) = ffiCall' (AppE f (toJSValE x)) xs
      toJSValE n   = AppE (VarE 'pToJSVal) (VarE $ mkName n)
      jsRefT v     = ConT ''JSVal
      returnTy     = let r = if isUnit then ConT ''() else ConT ''JSVal
                     in if isIO then AppT (ConT ''IO) r else r
      pat'         = p ++ concatMap (\nr -> let (n,r) = break (not . isNameCh) nr in namePl n ++ r) ps
      namePl n     = '$':show (fromJust (M.lookup n nameMap))
  qAddTopDecls [ffiDecl]
  qPutQ (QQCounter (c+1))
  return ffiCall


