import std.stdio, std.conv, std.format, uni;

void main()
{
	gen2();
	gen3();
	gen4();
}
 
void gen2(){
		 auto tWhite_Space = CodepointTrie!(10,11)(unicodeSet("White_Space"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestWhite_Space2 = CodepointTrie!(10,11).fromRawArray(");
		 tWhite_Space.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tAlphabetic = CodepointTrie!(10,11)(unicodeSet("Alphabetic"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestAlphabetic2 = CodepointTrie!(10,11).fromRawArray(");
		 tAlphabetic.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tMark = CodepointTrie!(10,11)(unicodeSet("Mark"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestMark2 = CodepointTrie!(10,11).fromRawArray(");
		 tMark.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tNumber = CodepointTrie!(10,11)(unicodeSet("Number"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestNumber2 = CodepointTrie!(10,11).fromRawArray(");
		 tNumber.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tPunctuation = CodepointTrie!(10,11)(unicodeSet("Punctuation"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestPunctuation2 = CodepointTrie!(10,11).fromRawArray(");
		 tPunctuation.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tSymbol = CodepointTrie!(10,11)(unicodeSet("Symbol"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestSymbol2 = CodepointTrie!(10,11).fromRawArray(");
		 tSymbol.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tSpace_Separator = CodepointTrie!(10,11)(unicodeSet("Space_Separator"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestSpace_Separator2 = CodepointTrie!(10,11).fromRawArray(");
		 tSpace_Separator.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tGraphical = CodepointTrie!(10,11)(
		 	unicodeSet("Alphabetic") | unicodeSet("N") | unicodeSet("M")
		 	 | unicodeSet("Zs") | unicodeSet("S") | unicodeSet("P") 
	 		);
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestGraphical2 = CodepointTrie!(10,11).fromRawArray(");
		 tGraphical.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tControl = CodepointTrie!(10,11)(unicodeSet("Control"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestControl2 = CodepointTrie!(10,11).fromRawArray(");
		 tControl.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tFormat = CodepointTrie!(10,11)(unicodeSet("Format"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestFormat2 = CodepointTrie!(10,11).fromRawArray(");
		 tFormat.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tCn = CodepointTrie!(10,11)(unicodeSet("Cn"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestCn2 = CodepointTrie!(10,11).fromRawArray(");
		 tCn.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 
}


 void gen3(){
		 auto tWhite_Space = CodepointTrie!(7,6,8)(unicodeSet("White_Space"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestWhite_Space3 = CodepointTrie!(7,6,8).fromRawArray(");
		 tWhite_Space.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tAlphabetic = CodepointTrie!(9,4,8)(unicodeSet("Alphabetic"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestAlphabetic3 = CodepointTrie!(9,4,8).fromRawArray(");
		 tAlphabetic.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tMark = CodepointTrie!(8,5,8)(unicodeSet("Mark"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestMark3 = CodepointTrie!(8,5,8).fromRawArray(");
		 tMark.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tNumber = CodepointTrie!(9,5,7)(unicodeSet("Number"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestNumber3 = CodepointTrie!(9,5,7).fromRawArray(");
		 tNumber.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tPunctuation = CodepointTrie!(8,5,8)(unicodeSet("Punctuation"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestPunctuation3 = CodepointTrie!(8,5,8).fromRawArray(");
		 tPunctuation.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tSymbol = CodepointTrie!(9,5,7)(unicodeSet("Symbol"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestSymbol3 = CodepointTrie!(9,5,7).fromRawArray(");
		 tSymbol.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tSpace_Separator = CodepointTrie!(7,6,8)(unicodeSet("Space_Separator"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestSpace_Separator3 = CodepointTrie!(7,6,8).fromRawArray(");
		 tSpace_Separator.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tGraphical = CodepointTrie!(9,4,8)(
		 	unicodeSet("Alphabetic") | unicodeSet("N") | unicodeSet("M")
		 	 | unicodeSet("Zs") | unicodeSet("S") | unicodeSet("P") 
	 		);
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestGraphical3 = CodepointTrie!(9,4,8).fromRawArray(");
		 tGraphical.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tControl = CodepointTrie!(7,5,9)(unicodeSet("Control"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestControl3 = CodepointTrie!(7,5,9).fromRawArray(");
		 tControl.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tFormat = CodepointTrie!(7,5,9)(unicodeSet("Format"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestFormat3 = CodepointTrie!(7,5,9).fromRawArray(");
		 tFormat.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tCn = CodepointTrie!(9,4,8)(unicodeSet("Cn"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestCn3 = CodepointTrie!(9,4,8).fromRawArray(");
		 tCn.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 
}

void gen4()
{
		 auto tWhite_Space = CodepointTrie!(6,4,4,7)(unicodeSet("White_Space"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestWhite_Space4 = CodepointTrie!(6,4,4,7).fromRawArray(");
		 tWhite_Space.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tAlphabetic = CodepointTrie!(6,4,4,7)(unicodeSet("Alphabetic"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestAlphabetic4 = CodepointTrie!(6,4,4,7).fromRawArray(");
		 tAlphabetic.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tMark = CodepointTrie!(6,4,4,7)(unicodeSet("Mark"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestMark4 = CodepointTrie!(6,4,4,7).fromRawArray(");
		 tMark.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tNumber = CodepointTrie!(7,4,4,6)(unicodeSet("Number"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestNumber4 = CodepointTrie!(7,4,4,6).fromRawArray(");
		 tNumber.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tPunctuation = CodepointTrie!(7,4,4,6)(unicodeSet("Punctuation"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestPunctuation4 = CodepointTrie!(7,4,4,6).fromRawArray(");
		 tPunctuation.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tSymbol = CodepointTrie!(6,5,4,6)(unicodeSet("Symbol"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestSymbol4 = CodepointTrie!(6,5,4,6).fromRawArray(");
		 tSymbol.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tSpace_Separator = CodepointTrie!(6,4,4,7)(unicodeSet("Space_Separator"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestSpace_Separator4 = CodepointTrie!(6,4,4,7).fromRawArray(");
		 tSpace_Separator.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tGraphical = CodepointTrie!(6,5,4,6)(
		 	unicodeSet("Alphabetic") | unicodeSet("N") | unicodeSet("M")
		 	 | unicodeSet("Zs") | unicodeSet("S") | unicodeSet("P") 
	 		);
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestGraphical4 = CodepointTrie!(6,5,4,6).fromRawArray(");
		 tGraphical.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tControl = CodepointTrie!(5,4,4,8)(unicodeSet("Control"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestControl4 = CodepointTrie!(5,4,4,8).fromRawArray(");
		 tControl.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tFormat = CodepointTrie!(6,4,4,7)(unicodeSet("Format"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestFormat4 = CodepointTrie!(6,4,4,7).fromRawArray(");
		 tFormat.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 auto tCn = CodepointTrie!(7,4,4,6)(unicodeSet("Cn"));
		 formattedWrite(stdout.lockingTextWriter, 
			"immutable bestCn4 = CodepointTrie!(7,4,4,6).fromRawArray(");
		 tCn.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
}