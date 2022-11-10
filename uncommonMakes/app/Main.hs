{-# LANGUAGE NoMonomorphismRestriction#-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module Main (main) where

import Control.Arrow (loop, (***), second, (>>>))
import Data.Function (fix, (&))
import Graphics.UI.GLUT hiding (Matrix)
import Graphics.Rendering.OpenGL hiding (Matrix)
import qualified Graphics.Rendering.OpenGL as GL (Matrix)
-- settable state var is a cont set by the main loop
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  flush

type Origin = (forall a.Fragment a=>a,forall b.HomoMorphism b=>b)
type Coordinates = (ProjectionReflection (forall a.a -> a) => Either (forall a.a -> a) ((forall a.a -> a),(forall a.a -> a)))
type Matrix = (Origin,Coordinates,(forall f g.Functor f => Either (g f) (g f)))

class HomoMorphism a where
  type Mapping a

instance (a ~ Matrix) => HomoMorphism ( a -> a) where
  type Mapping (Matrix -> Matrix) = () -- type family extract the adjuction
-- (forall a f. (Vertex a, Functor f) => f a)
class Fragment a

class Animation a

class ProjectionReflection a
-- [cont | group by x using f]
