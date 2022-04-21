-- The words function counts the number of words in a string. Modify the wc.hs example to count the number of words in a file.

main = interact wordCount
    where wordCount input = show (length (words input)) ++ "\n"