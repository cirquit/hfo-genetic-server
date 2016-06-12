module HFO.ToFlags where


-- | Simple Interface to work with `proc` and supply it with corresponding
--   command line arguments

class ToFlags a where

    toFlags :: a -> [String]

-- | Filter empty lists, because python does not like that
--
    toFlags_ :: a -> [String] 
    toFlags_ = filter (not . null) . toFlags
