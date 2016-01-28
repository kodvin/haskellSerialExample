import Control.Monad 
import Control.Applicative ((<$>))
import Data.ByteString.Char8 (unpack)
import System.Hardware.Serialport 
import System.IO
import Data.Maybe
import Control.Concurrent
import Data.Char (isSpace)
import Data.Time.Clock.POSIX
import System.Directory


directoryName = "experiments"

-- linereading code borrowed from http://stackoverflow.com/questions/31678873/haskell-serial-received-data-not-printed-correctly/31684578#31684578
-- Read a character from the serial port.
receive :: SerialPort -> IO String
receive sp = fmap unpack (recv sp 1)

-- Read a line, ending with '\n'.
readLine :: SerialPort -> IO String
readLine sp = readLineRec sp [] 

-- Recursive function to read a line from the serial port.
readLineRec :: SerialPort -> String -> IO String
readLineRec sp [] = receive sp >>= readLineRec sp 
readLineRec sp chars
    | last chars == '\n' = return chars
    | otherwise = (chars ++) <$> receive sp >>= readLineRec sp

--input reading
-- borowed from http://stackoverflow.com/questions/3894792/what-is-a-simple-way-to-wait-for-and-then-detect-keypresses-in-haskell
ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing

saveData :: String -> String -> String -> IO ()
saveData name time value = appendFile name (time++ ", " ++ value ++ "\n")

rstrip :: String -> String
rstrip  = reverse . dropWhile isSpace . reverse 

--time is in second resolution 
getTime:: IO Integer
getTime = fmap (round . (*1000)) getPOSIXTime 

getSerialInterface:: IO SerialPort
getSerialInterface = 
  let port = "/dev/tty.usbmodem1421" -- Linux
  in openSerial port SerialPortSettings { commSpeed = CS9600,
                                    bitsPerWord = 8,
                                    stopb = One,
                                    parity = NoParity,
                                    flowControl = NoFlowControl,
                                    timeout = 10 } 

handleReceivedValue :: String -> String -> IO ()                        
handleReceivedValue line filePath = do
        let value = (rstrip line)
        time <- getTime 
        saveData filePath (show time) value
        print value

-- main loop for reading serial port and checking input from keyboard
-- loop stops after "q" is entered
-- is something is entered we do not save in this iteration
looping:: SerialPort -> String -> IO ()
looping s filePath = do 
  line <- readLine s
  a <- ifReadyDo stdin getLine
  case a of
    Just "q" -> do return ()
    Just a -> do
        print a
        looping s filePath
    Nothing -> do 
        handleReceivedValue line filePath
        looping s filePath

main = do
s <- getSerialInterface
time <- getTime
createDirectoryIfMissing True directoryName
let filePath = "./"++directoryName++"/"++ (show time) ++ ".txt"
flush s --flush serial port so we could start fresh
looping s filePath
closeSerial s



