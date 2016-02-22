
import System.Directory
import System.IO 
import System.Environment

-- this command line programa will "average" experiment values, so it would bee smoother
-- it will take previous nth elements and wil average them to get current value
-- first argument files name
-- second argument nth elements to take for averaging (default to 20)

--removes last element from a list
removeLast xs = reverse $ drop 1 $ reverse xs

-- provides new filname
getNewFileName name =  (reverse $ drop 4 (reverse name)) ++ "-av_it.txt"

--get value part form "time, value" format of the data
getValues content = map (\line -> let [time, value] = words line in (read value::Int)) $  lines content 

--get time part form "time, value" format of the data
getTimes content = map 
    (\line -> 
        let [time, value] = words line 
            time' = removeLast time
        in (read time'::Int))
    $ lines content 

getSegment interval offeset list = drop offeset ( take (interval + offeset) $ list)

-- averages vaulues for provided array with nth nearest elements, first n - 1 element is replicated
newElements' xs interval =  [ (\xs -> (fromIntegral (foldr (+) 0 xs)) / (fromIntegral (length xs)) ) $ getSegment interval x xs | x <- [0..((length xs) - interval) ]]
newElements xs interval = (replicate (interval - 1) (fromIntegral (head xs))) ++ (newElements' xs interval)


formatContent content n = zipWith (\x y -> (show x)++ ", " ++ (show y)) (getTimes content) (newElements (getValues content) n)

transformFile filePath number = do 
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    appendFile (getNewFileName filePath) (unlines $ formatContent contents number)
    hClose handle
    return ()

proccessArgs [] = print "need more arguments"
--default to 20 elements
proccessArgs (filePath:[]) = transformFile filePath 20
-- have enaough arguments
proccessArgs (filePath:number:xs) =  transformFile filePath (read number::Int)

main = do
    args <- getArgs
    proccessArgs args 
