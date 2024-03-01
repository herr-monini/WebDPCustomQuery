{-# LANGUAGE OverloadedStrings #-}

module AesonWebDP where 




import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import WebDPConv.Abs
import Data.Text


kv a b = a A..= b

instance A.ToJSON MParam where 
    toJSON m = case m of 
        MParam name noise budget -> undefined 
        MParamC name -> undefined 
        MParamN noise -> undefined 
        MParamB budget -> undefined 
        MParamCN name noise -> undefined 
        MParamCB name budget  -> undefined  
        MParamNB noise budget -> undefined 
        MParamNull -> undefined 


instance A.ToJSON Budget where 
    toJSON b = case b of 
        PureDP e -> A.object [kv "epsilon" e]
        ApproxDP e d -> A.object [kv "epsilon" e, kv "delta" d]

instance A.ToJSON Value where 
    toJSON v = case v of 
        TVal -> A.toJSON True 
        FVal -> A.toJSON False 
        IVal i -> A.toJSON i 
        DVal d -> A.toJSON d 
        SVal s -> A.toJSON s 

