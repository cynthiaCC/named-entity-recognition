import Data.List.Split
import Data.List
import System.IO
import Data.Char


nameEntityRecog :: [String] -> [String] -> [String] -> [String] -> [String] -> [String]
nameEntityRecog [] _ _ _ x = x
nameEntityRecog (x:xs) countries cities name result =
    nameEntityRecog xs countries cities name (result ++ [(jointList (getOneWordEntities (getTwoWordsEntities (getOrgEntities (splitOn " " x) []) countries cities name [])countries cities name []))])

getOrgEntities :: [String] -> [String] -> [String]
getOrgEntities [] x = x
getOrgEntities (x:xs) result
    | (length orgList) > 2 && (head orgList) `elem` ["of", "The", "the"] && (last (last orgList)) `elem` ['.', ','] = getOrgEntities (drop (length orgList) (x:xs)) (result ++ [(head orgList)] ++ [("<ENAMEX TYPE=\"ORGANIZATION\">" ++ (jointList (init (tail orgList))) ++ " " ++ (init (last orgList)) ++ "</ENAMEX>" ++ [(last (last orgList))])])
    | (length orgList) > 2 && (head orgList) `elem` ["of", "The", "the"] = getOrgEntities (drop (length orgList) (x:xs)) (result ++ [(head orgList)] ++ [("<ENAMEX TYPE=\"ORGANIZATION\">" ++ (jointList (tail orgList)) ++ "</ENAMEX>")])
    | otherwise = getOrgEntities xs (result ++ [x])
    where orgList = (getOrgList (x:xs) [])

getOrgList words org = if (null words) == False
                                    then if (length (head words)) > 1 && ((head (head words)) `elem` ['A' .. 'Z'] || (head words) `elem` ["of", "the"])
                                        then getOrgList (tail words) (org ++ [(head words)])
                                        else org
                                        else org

dateWords = ["year","month","week","quarter","Today","today","tomorrow","Tomorrow","yesterday","Yesterday","January","Jan","February","Feb","March","Mar","April","Apr","May","June","Jun","July","Jul","August","Aug", "September","Sep", "October","Oct", "November","Nov", "December","Dec", "Monday","Mon", "Tuesday","Tue", "Wednsday","Wed","Thursday","Thu", "Friday","Fri", "Saturday","Sat", "Sunday","Sun"]
timeWords = ["p.m.","a.m."]
nonName = ["The","In","He","She","I","Mr.","Mrs.","Ms.","Gov."]
timeNumber = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"]
punctuation = [',','.','\'',';','?','!']

getTwoWordsEntities :: [String] -> [String] -> [String] -> [String] -> [String] -> [String]
getTwoWordsEntities [] _ _ _ x = x
getTwoWordsEntities (x:xs) countries cities name result
    | (null xs) == False && twoWordChunk `elem` countries = getTwoWordsEntities (tail xs) countries cities name (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ twoWordChunk ++"</ENAMEX>")])
    | (null xs) == False && (null x) == False && twoWordChunk `elem` cities = getTwoWordsEntities (tail xs) countries cities name (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ twoWordChunk ++"</ENAMEX>")])
    | (null xs) == False && twoWordChunk' `elem` countries = getTwoWordsEntities (tail xs) countries cities name (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ twoWordChunk' ++"</ENAMEX>" ++ [(last x)])])
    | (null xs) == False && (null x) == False &&twoWordChunk' `elem` cities = getTwoWordsEntities (tail xs )countries cities name (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ twoWordChunk' ++"</ENAMEX>" ++ [(last x)])])
    | x `elem` ["last", "next", "Last", "Next", "first", "second", "third", "forth"] && (head xs) `elem` dateWords = getTwoWordsEntities (tail xs) countries cities name (result ++ [("<TIMEX TYPE=\"DATE\">" ++ x ++ " " ++ (head xs) ++"</TIMEX>")])
    | (isDateNumber x) == True && (head xs) `elem` dateWords = getTwoWordsEntities (tail xs) countries cities name (result ++ [("<TIMEX TYPE=\"DATE\">" ++ x ++ (head xs) ++"</TIMEX>")])
    | (isDateNumber x) == True && (init (head xs)) `elem` dateWords = getTwoWordsEntities (tail xs) countries cities name (result ++ [("<TIMEX TYPE=\"DATE\">" ++ x ++ " " ++ (init (head xs)) ++"</TIMEX>" ++ [(last (head xs))] )])
    | x `elem` ["last", "next", "Last", "Next", "first", "second", "third", "forth"] && (init (head xs)) `elem` dateWords = getTwoWordsEntities (tail xs) countries cities name (result ++ [("<TIMEX TYPE=\"DATE\">" ++ x ++ " " ++ (init (head xs)) ++"</TIMEX>" ++ [(last (head xs))] )])
    | x `elem` ["at","At"] && (length (head xs)) > 3 && (head xs)!!2 == ':' && (last (head xs)) `elem` punctuation = getTwoWordsEntities (tail xs) countries cities name (result ++ [("<TIMEX TYPE=\"TIME\">" ++ (init (head xs)) ++"</TIMEX>" ++ [(last (head xs))] )])
    | x `elem` timeNumber && (head xs) `elem` timeWords = getTwoWordsEntities (tail xs) countries cities name (result ++ [("<TIMEX TYPE=\"TIME\">" ++ x ++ " " ++ (init (head xs)) ++"</TIMEX>" ++ [(last (head xs))] )])
    | (length x) > 1 && (null xs) == False && (head x) == '$' && (init (head xs)) `elem` ["million", "thousand", "hundred"] = getTwoWordsEntities (tail xs) countries cities name (result ++ [("<NUMEX TYPE=\"MONEY\">" ++ x ++ " " ++ (init (head xs)) ++"</NUMEX>" ++ [(last (head xs))])])
    | (length x) > 1 && (null xs) == False && (head x) == '$' && (head xs) `elem` ["million", "thousand", "hundred"] = getTwoWordsEntities (init xs) countries cities name (result ++ [("<NUMEX TYPE=\"MONEY\">" ++ x ++ " " ++ (head xs) ++"</NUMEX>")])
    | (length x) > 1 && (length x) > 2 && isUpper(head x) && isUpper(head (head xs)) && x `isElem` name  && (head xs) `isElem` name = getTwoWordsEntities (tail xs) countries cities name (result ++ [("<ENAMEX TYPE=\"PERSON\">" ++ twoWordChunk ++"</ENAMEX>")])
    | (length x) > 1 && (length (head xs)) > 1  && (length x) > 2 && x `notElem` nonName && (last (init (head xs))) `elem` punctuation && isUpper(head (head xs)) && isUpper(head x) && x `isElem` name  && (init (head xs)) `isElem` name = getTwoWordsEntities (tail xs) countries cities name (result ++ [("<ENAMEX TYPE=\"PERSON\">" ++ twoWordChunk' ++"</ENAMEX>" ++ [(last x)])])
    | otherwise = getTwoWordsEntities (xs) countries cities name (result ++ [x])
    where twoWordChunk = (x ++ " " ++ (head xs))
          twoWordChunk' = (x ++ " " ++ (init (head xs)))

isDateNumber string = if string `elem` ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"] then True
                        else False

getOneWordEntities :: [String] -> [String] -> [String] -> [String] -> [String] -> [String]
getOneWordEntities [] _ _ _ x = x
getOneWordEntities (x:xs) countries cities name result
    | (null x) == False && x `elem` countries = getOneWordEntities xs countries cities name (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ x ++"</ENAMEX>")])
    | (null x) == False && x `elem` cities = getOneWordEntities xs countries cities name (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ x ++"</ENAMEX>")])
    | (length x) > 1 && (init x) `elem` countries = getOneWordEntities xs countries cities name (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ (init x) ++"</ENAMEX>" ++ [(last x)])])
    | (length x) > 1 && (init x) `elem` cities = getOneWordEntities xs countries cities name (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ (init x) ++"</ENAMEX>" ++ [(last x)])])
    | (length x) > 2 && (init (init x)) `elem` countries = getOneWordEntities xs countries cities name (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ (init (init x)) ++"</ENAMEX>" ++ [(last (init x))] ++ [(last x)])])
    | (length x) > 2 && (init (init x)) `elem` cities = getOneWordEntities xs countries cities name (result ++ [("<ENAMEX TYPE=\"LOCATION\">" ++ (init (init x)) ++"</ENAMEX>" ++ [(last (init x))] ++ [(last x)])])
    | x `elem` dateWords = getOneWordEntities xs countries cities name (result ++ [("<TIMEX TYPE=\"DATE\">" ++ x ++"</TIMEX>")])
    | (length x) > 1 && (init x) `elem` dateWords = getOneWordEntities xs countries cities name (result ++ [("<TIMEX TYPE=\"DATE\">" ++ (init x) ++"</TIMEX>" ++ [(last x)])])
    | (length x) > 1 && (head x) == '$' = getOneWordEntities xs countries cities name (result ++ [("<NUMEX TYPE=\"MONEY\">" ++ x ++"</NUMEX>")])
    | (length x) > 2 && x `notElem` nonName && isUpper(head x) && x `isElem` name = getOneWordEntities xs countries cities name (result ++ [("<ENAMEX TYPE=\"PERSON\">" ++ x ++"</ENAMEX>")])
    | (length x) > 2 && x `notElem` nonName && isUpper(head x) && (last x) `elem` punctuation && (init x) `isElem` name = getOneWordEntities xs countries cities name (result ++ [("<ENAMEX TYPE=\"PERSON\">" ++ (init x) ++"</ENAMEX>" ++ [(last x)])])
    | (length x) > 2 && x `notElem` nonName && isUpper(head x) && (last (init x)) `elem` punctuation &&(init (init x)) `isElem` name = getOneWordEntities xs countries cities name (result ++ [("<ENAMEX TYPE=\"PERSON\">" ++ (init (init x)) ++"</ENAMEX>" ++ [(last (init x))] ++ [(last x)])])
    | otherwise = getOneWordEntities xs countries cities name (result ++ [x])

jointList ss = (intercalate " " ss)

isElem :: String->[String]->Bool
isElem x [] = False
isElem x (y:ys) = if dist x y >1 then isElem x ys else True

dist :: Eq a => [a] -> [a] -> Int
dist a b 
    = last (if lab == 0 then mainDiag
        else if lab > 0 then lowers !! (lab - 1)
         else{- < 0 -}   uppers !! (-1 - lab))
    where mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
          uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
          lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
          eachDiag a [] diags = []
          eachDiag a (bch:bs) (lastDiag:diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
            where nextDiag = head (tail diags)
          oneDiag a b diagAbove diagBelow = thisdiag
            where doDiag [] b nw n w = []
                  doDiag a [] nw n w = []
                  doDiag (ach:as) (bch:bs) nw n w = me : (doDiag as bs me (tail n) (tail w))
                    where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
                  firstelt = 1 + head diagBelow
                  thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
          lab = length a - length b
          min3 x y z = if x < y then x else min y z

getCorrectLabel :: [String]->[String]->Float->Float
getCorrectLabel [] _ count = count
getCorrectLabel _ [] count = count
getCorrectLabel taggedResultList (x:xs) count = if x `elem` taggedResultList then getCorrectLabel (delete x taggedResultList) xs count+1.0 else getCorrectLabel taggedResultList xs count

getlbdEty :: [String] -> [String] -> [String]
getlbdEty [] x = x
getlbdEty (x:xs) tags = (getlbdEty xs (tags ++ (getlbdEty' (splitOn " " x) [])))

getlbdEty' :: [String] -> [String] -> [String]
getlbdEty' [] x = x
getlbdEty' (x:xs) tags
    | (x == "<ENAMEX" || x == "<TIMEX" || x == "<NUMEX")  = (getlbdEty' (drop (length (splitOn " " textElem)) (x:xs)) (tags ++ [textElem]))
    | otherwise = getlbdEty' (xs) tags
    where textElem = (getTxtElem (x:xs) "")

getTxtElem :: [String] -> String -> String
getTxtElem [] x = x
getTxtElem (x:xs) tagContent
    | "</ENAMEX>" `isInfixOf` x = (tagContent ++ x)
    | "</TIMEX>" `isInfixOf` x = (tagContent ++ x)
    | "</NUMEX>" `isInfixOf` x = (tagContent ++ x)
    | otherwise = getTxtElem xs (tagContent ++ x ++ " ")

main = do
    untagged <- readFile ("untagged.txt")
    fcountries <- readFile ("countries.txt")
    fcities <- readFile ("cities.txt")
    fname <- readFile ("name.txt")
    tagged <- readFile ("tagged.txt")
    let unTaggedList = (lines untagged)
    let countries = (splitOn "\r\n" fcountries)
    let cities = (lines fcities)
    let name = (lines fname)
    let taggedResult = (lines tagged)

    let result =  nameEntityRecog unTaggedList countries cities name []
    writeFile "result.txt" $ unlines result

    myTagged <- readFile ("result.txt")
    let mytaggedResult = (lines myTagged)

    let taggedResultList = getlbdEty taggedResult []
    let myTaggedResultList = getlbdEty mytaggedResult []
    putStrLn ("-------------------------------------")
    putStrLn ("The untagged.txt")
    putStrLn ("-------------------------------------")
    print untagged
 

    putStrLn ("-------------------------------------")
    putStrLn ("The entities extracted from tagged.txt:")
    putStrLn ("-------------------------------------")
    print taggedResultList
    
    putStrLn ("-------------------------------------")
    putStrLn ("my tagged result:")
    putStrLn ("-------------------------------------")
    
    print myTaggedResultList
    let correctLabel = getCorrectLabel taggedResultList myTaggedResultList 0.0
    let recall = correctLabel/(fromIntegral (length taggedResultList) :: Float)
    let precision = correctLabel/(fromIntegral (length myTaggedResultList) :: Float)
    let f1measure =2.0 * recall * precision/(recall + precision)
    putStrLn ("-------------------------------------")
    putStrLn ("The correct labelled entities: " ++ show correctLabel)
    putStrLn ("The recall: " ++ show recall)
    putStrLn ("The precision: " ++ show precision)
    putStrLn ("The F1 measure: " ++ show f1measure)
    putStrLn ("-------------------------------------")


