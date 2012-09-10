import Crypto.Basic

oldCipher = hexsToWords "6c73d5240a948c86981bc294814d"
oldText = charsToWords "attack at dawn"
newText = charsToWords "attack at dusk"

key = xorTexts oldCipher oldText
newCipher = wordsToHexs $ xorTexts key newText

