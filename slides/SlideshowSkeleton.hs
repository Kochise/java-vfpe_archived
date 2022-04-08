
header title n m =
  "<title>" ++ title ++ "</title>" ++ "\n" ++
  "<!-- colour styles for Joel's web pages -->" ++ "\n" ++
  "<body bgcolor=\"#f0f0e0\" link=\"#808000\" vlink=\"#808000\">" ++ "\n" ++
  "<font face=\"arial\">" ++ "\n" ++
  "\n" ++
  (if n > 1 then "" else
    "<font size=4>" ++ title ++ "</font>" ++ "\n") ++
  "<p>" ++ "\n"

navigation basename n m menu =
  "<a href=\"" ++ basename ++ "1.html\">start</a> | \n" ++ 
  (if n >= m then "" else
    "<a href=\"" ++ basename ++ (show (n+1)) ++ ".html\">next</a> | \n") ++
  (if n <= 1 then "" else
    "<a href=\"" ++ basename ++ (show (n-1)) ++ ".html\">prev</a> | \n") ++
  "<a href=\"" ++ menu ++ "\">menu</a> \n" ++
  "<p>\n"
  
slide basename n =
  "<center>\n" ++
  "<img src=\"" ++ basename ++ (show n) ++ ".gif\">\n" ++
  "</center>"
  
page title basename n m menu =
  header title n m ++ navigation basename n m menu ++ slide basename n
  
pages title basename m menu = 
  [(basename ++ (show x) ++ ".html", page title basename x m menu) | x <- [1..m]]

writePages title basename m menu =
  sequence_ [writeFile fn p | (fn,p) <- pages title basename m menu]
  
-- invoke from Hugs like eg
-- writePages "Parallel Quicksort" "shot" 17 "../../index.html#slideshows" 
