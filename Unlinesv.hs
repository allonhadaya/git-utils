{-# LANGUAGE FlexibleInstances #-}

module Unlinesv(
  unlinesv
) where

class UnlinesvType r where
  unlinesv :: [Char] -> r

instance UnlinesvType [Char] where
  unlinesv = id

instance UnlinesvType r => UnlinesvType ([Char] -> r) where
  unlinesv x = unlinesv . ((x ++ "\n") ++)
