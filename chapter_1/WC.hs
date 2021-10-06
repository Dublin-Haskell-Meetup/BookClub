main = interact wordCount
    where wordCount input = show (length input) ++ "\n"

-- wrote this with vim, using tab causes compile error, I think this can be fixed in the vim setting, but need to investigate
--runghc WC < text.txt
