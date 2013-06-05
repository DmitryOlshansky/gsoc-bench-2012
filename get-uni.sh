#!/bin/sh
PREFIX="http://www.unicode.org/Public/UNIDATA/"
FILES="CaseFolding.txt Blocks.txt PropList.txt DerivedCoreProperties.txt DerivedNormalizationProps.txt Scripts.txt HangulSyllableType.txt UnicodeData.txt CompositionExclusions.txt SpecialCasing.txt"
EXT="DerivedGeneralCategory.txt DerivedCombiningClass.txt"
for NAME in $FILES
do
    wget -nc $PREFIX$NAME
done

for NAME in $EXT
do
    wget -nc ${PREFIX}extracted/$NAME
done