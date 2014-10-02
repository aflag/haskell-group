module TatiPhone where
-- Dado o seguinte teclado numérico:
--
-- 0 - e
-- 1 - j n q
-- 2 - r w x
-- 3 - d s y
-- 4 - f t
-- 5 - a m
-- 6 - c i v
-- 7 - b k u
-- 8 - l o p
-- 9 - g h z
--
-- uma lista de palavras, e uma lista de números.
-- Precisamos encontrar palavras que se encaixem nos
-- números dados.
--
-- Por exemplo, dada a lista de palavras:
--
-- good
-- test
-- go
-- od
--
-- Podemos transformar o número
--
-- 9883-4034
--
-- em
--
-- good test
-- go od test
--
-- Isso pode ser utilizado para tentar advinhar que
-- palavra o usuario está digitando num teclado
-- numérico. Também pode ser utilizado para criar
-- palavras que vão ajudar as pessoas a decorarem
-- números telefonicos.

import qualified Data.Map.Lazy as M
import Data.Maybe

vocabulary = [
    "xubirusistico",
    "xubiru",
    "pep",
    "geralmente",
    "bocoio",
    "higienicos",
    "silencio",
    "inusitado",
    "bembolado",
    "dojo",
    "pinheirinho",
    "alegria",
    "bullying",
    "vamos",
    "partiu",
    "combinador"]

--translate :: String -> String
translate x = catMaybes $ map (flip M.lookup mapping) x
  where
    mapping = M.fromList $ zip intab outab
    intab = "abcdefghijklmnopqrstuvwxyz"
    outab = "57630499617851881234762239"
