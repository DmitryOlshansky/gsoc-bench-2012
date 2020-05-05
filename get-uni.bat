set PREFIX="http://www.unicode.org/Public/UNIDATA/"

curl %PREFIX%/CaseFolding.txt --output CaseFolding.txt
curl %PREFIX%/Blocks.txt --output Blocks.txt
curl %PREFIX%/PropList.txt --output PropList.txt
curl %PREFIX%/DerivedCoreProperties.txt --output DerivedCoreProperties.txt
curl %PREFIX%/DerivedNormalizationProps.txt --output DerivedNormalizationProps.txt
curl %PREFIX%/Scripts.txt --output Scripts.txt
curl %PREFIX%/HangulSyllableType.txt --output HangulSyllableType.txt
curl %PREFIX%/UnicodeData.txt --output UnicodeData.txt
curl %PREFIX%/CompositionExclusions.txt --output CompositionExclusions.txt
curl %PREFIX%/SpecialCasing.txt --output SpecialCasing.txt

curl %PREFIX%/DerivedGeneralCategory.txt --output DerivedGeneralCategory.txt
curl %PREFIX%/DerivedCombiningClass.txt --output DerivedCombiningClass.txt
