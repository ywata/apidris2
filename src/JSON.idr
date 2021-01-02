module JSON

import Language.JSON

j1 : JSON
j1 = JNull


j2 : JSON
j2 = JBoolean True

j3 : JSON
j3 = JNumber 1.0

j4 : JSON
j4 = JString "string"

j5 : JSON
j5 = JArray [JNumber 1.0, JString "string"]

j6 : JSON
j6 = JObject [("string", j4), ("number", j3)]
