// Unicode character classifier. Think "wc on steroids"
module uniclass;

import  std.datetime, std.file, std.getopt, std.range, std.stdio, std.uni;

size_t codepointCount(const(char)[] text)
{
	size_t count = 0;
	foreach(dchar ch; text) count++;
	return count;
}

size_t countUp(alias matcher, Range)(Range r)
{
	size_t count=0;
	Range s = r;
	while(s.length)
	{
		auto m = matcher(s);
		if(m)
			count++;
		s = s[m.stride..$];
	}
	return count;
}

void classifyRaw(alias matcher)(string inp, bool lazyRange)
{
	if(!lazyRange)
	{
		auto text = cast(char[])std.file.read(inp);
	    StopWatch sw;
	    sw.start();
        auto matched = countUp!matcher(text);
        sw.stop();
        auto points = codepointCount(text);
        writefln("%d / %d; %.3f M/s", matched, points, cast(double)points/sw.peek().usecs);
    }
    else
    	assert(0, "Not implemented yet.");
}

void classifyDec(alias table)(string inp, bool lazyRange)
{
	
}

enum MatcherType {
	tbst,
	utrie,
	dtrie2,
	dtrie3
}

int main(string[] args)
{
	string setExpr = "Alphabetic"; // defaults
	MatcherType matcher = MatcherType.tbst;
	bool lazyRange = false;
	auto ret = getopt(args,
		"s|set", "Unicode set expression - use quotes to bypass shell escaping", &setExpr,
		"m|matcher", "Type of matcher to use - tbst (tiny Bineary search table),"
			~ " utrie (UTF trie) or dtrie2, dtrie3 (dchar Trie lvl-2/3)", &matcher,
		"l|lazy", "Use lazy character range instead of reading whole file at once", &lazyRange
	);
	if(ret.helpWanted)
	{
		defaultGetoptPrinter("uniclass - unicode character classifier", ret.options);
		return 1;
	}
	auto set = unicode(setExpr); // TODO: implement full expressions in std.uni
	void delegate(string, bool) processor;
	switch(matcher) with(MatcherType){
		case tbst:
			{
				auto bst = tinyUtfBst!char(set);
				processor = &classifyRaw!bst;
			}
			break;
		case utrie:
			{
				auto ut = utfMatcher!char(set);
				processor = &classifyRaw!ut;
			}
			break;
		case dtrie2:
		case dtrie3:
			break;
		default:
			assert(0);
	}
	if(args.length <= 1)
	{
		stderr.writeln("Expected at least one input file");
		return 1;
	}
	foreach(inp; args[1..$])
		processor(inp, lazyRange);
	return 0;
}
