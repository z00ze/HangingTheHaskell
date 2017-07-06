import Data.List
import Data.Char
import System.IO

----- Määritellään sana
sana = "teekkarintupsu"
----- Määritellään arvattusana
arvattusana = (teesana sana)

----- Metodi joka muuttaa sanan "*"-merkeiksi.
teesana :: String -> String
teesana [] = ""
teesana (x:xs) = "*" ++ (teesana xs)

----- Arvaus, jos arvaus on yli 1 niin arvataan koko sanaa, muussa tapauksessa kutsutaan arvaakirjainta.
arvaus :: String -> String -> String
arvaus a b
    | length a > 1 = arvaasana a
    | otherwise = arvaakirjain (head a) sana b

----- Arvataan sanaa, jos sana ei ole oikein, palautetaan "Väärin!"
arvaasana :: String -> String
arvaasana a
    | a == sana = sana
    | otherwise = "Arvasit väärin!"

----- Arvataan kirjainta. Palautetaan sana josta on muutettu oikein menneet kirjaimet.
arvaakirjain :: Char -> String -> String -> String
arvaakirjain a [] [] = ""
arvaakirjain a (x:xs) (y:ys)
    | a == x = (charToString (toUpper x)) ++ arvaakirjain a xs ys
    | y `elem` ['A'..'Z'] = charToString y ++ arvaakirjain a xs ys
    | otherwise = "*" ++ arvaakirjain a xs ys

----- Muutetaan parametrina annettu merkki merkkijonoksi.
charToString :: Char -> String
charToString c = [c]

----- Piirretään arvauksien mukaan graafinen hirsipuu.
piirto :: String -> String
piirto i
    | (read i) == 7 = "  ___\n" ++ "  |  |\n" ++ "  |  O\n" ++ "  | -|-" ++ "\n" ++ "  | //" ++ "\n" ++ "  |" ++ "\n" ++ "__|___"
    | (read i) == 6 = "  ___\n" ++ "  |  |\n" ++ "  |  O\n" ++ "  | -|-" ++ "\n" ++ "  | " ++ "\n" ++ "  |" ++ "\n" ++ "__|___"
    | (read i) == 5 = "  ___\n" ++ "  |  |\n" ++ "  |  O\n" ++ "  |  |" ++ "\n" ++ "  | " ++ "\n" ++ "  |" ++ "\n" ++ "__|___"
    | (read i) == 5 = "  ___\n" ++ "  |  |\n" ++ "  |  O\n" ++ "  |  " ++ "\n" ++ "  | " ++ "\n" ++ "  |" ++ "\n" ++ "__|___"
    | (read i) == 4 = "  ___\n" ++ "  |  |\n" ++ "  |  \n" ++ "  |  " ++ "\n" ++ "  | " ++ "\n" ++ "  |" ++ "\n" ++ "__|___"
    | (read i) == 3 = "  ___\n" ++ "  |  \n" ++ "  |  \n" ++ "  |  " ++ "\n" ++ "  | " ++ "\n" ++ "  |" ++ "\n" ++ "__|___"
    | (read i) == 2 = "  \n" ++ "  |  \n" ++ "  |  \n" ++ "  |  " ++ "\n" ++ "  | " ++ "\n" ++ "  |" ++ "\n" ++ "__|___"
    | (read i) == 1 = "  \n" ++ "    \n" ++ "    \n" ++ "    " ++ "\n" ++ "   " ++ "\n" ++ "  " ++ "\n" ++ "______"
    | otherwise = "GAME OVER"

----- Hirsipuu-peli! Ajetaan komennolla: hirsipuu arvatusana "1"
hirsipuu x i = do
    -- Luetaan pelaajalta sana/merkki
    a <- getLine
    -- Kutsutaan arvaus metodia 
    let b = (arvaus a x) --- Palauttaa sanan jossa on muutettu oikein menneet kirjaimet
    -- Tarkistetaan onko palautettusana sama kuin sana
    if a == b
        then putStrLn "OIKEIN! Voitit pelin!"
        -- Jos palautettu sana ei ollut oikein, jatketaan peliä.
        else do
            -- Tulostetaan nykyinen sanan tila. (Arvatut kirjaimet näkyvät, muut ovat "*"-merkkejä)
            putStrLn b
            -- Kutsutaan piirto-metodia joka piirtää pelin tilanteen.
            let m = (piirto i)
            -- Jos arvauksia on liikaa, metodi palauttaa "GAME OVER" ja tarkistetaan se ja tulostetaan pelaajalle, että hän hävisi pelin.
            if m == "GAME OVER"
                then putStrLn "Hävisit pelin!"
                -- Jos peli jatkuu, tulostetaan pelin tilanne, sekä kutsutaan hirsipuu metodia uudestaan.
                else do
                    putStrLn m
                    hirsipuu b (show (1+ (read i)))