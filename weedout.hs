
import System.Directory
import System.IO 
import System.Environment

-- this command line programa will "weed out" experiments, it will change time interval from milli seconds to seconds
-- and skip every nth element 
-- first argument files name
-- second argument nth element to take (20 would skip 19 elements and only take the 20th element)

-- gets every nth element
extractEvery m = map snd . filter (\(x,y) -> (mod x m) == 0) . zip [1..]

-- provides new filname
getNewFileName name =  (reverse $ drop 4 (reverse name)) ++ "-weedout.txt"

--removes last element from a list
removeLast xs = reverse $ drop 1 $ reverse xs

--provided first element( from "12345, 456" format data) removes "," and divides by 1000 to get seconds from miliseconds
getSeconds xs = let millis = removeLast xs 
                    mInt = read millis::Integer
                in mInt `quot` 1000

-- gets old values ("12345, 456" format data) and transforms first element to seconds
toSeconds xs = map (\line -> let [time, value] = words line in (show (getSeconds time) ++ ", "++ value )) xs

transformFile filePath number = do 
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    appendFile (getNewFileName filePath) (unlines $ toSeconds $ extractEvery number (lines contents))
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