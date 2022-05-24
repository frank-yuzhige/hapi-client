module LibCurl.Curl.Types where
import Foreign
import Control.Concurrent (MVar)

-- Types

data Curl_
type CurlH    = Ptr Curl_

type URLString = String
type Port = Long
type Long = Word32
type LLong = Word64
data Slist_

