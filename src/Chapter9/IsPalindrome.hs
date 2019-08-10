module Chapter9.IsPalindrome where

main = interact isPalindrome

isPalindrome :: String -> String
isPalindrome = unlines . detectPalindromes . lines where
  detectPalindromes = map (\s -> if reverse s == s then "Palindrome" else "Not a palindrome")
