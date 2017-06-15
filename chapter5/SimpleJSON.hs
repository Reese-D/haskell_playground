module SimpleJSON
  (
    JValue(..),
    getString,
    getInt,
    getDouble,
    getBool,
    getObject,
    getArray,
    isNull)
  where

--Or you could type 'module SimpleJSON where' to simply export everything
--To do the exact opposite you cuold type 'module SimpleJSON () where' to export nothing
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
             deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getInt (JNumber n) = Just (truncate n)
getInt _ = Nothing

getDouble (JNumber n) = Just n
getDouble _ = Nothing

getBool (JBool n) = Just n
getBool _ = Nothing

getObject (JObject n) = Just n
getObject _ = Nothing

getArray (JArray n) = Just n
getArray _ = Nothing

isNull v = v == JNull
