import Data.Char
isPalindrome sentence = reverse normalized == normalized
                        where normalized = map toLower (filter (/= ' ') sentence)