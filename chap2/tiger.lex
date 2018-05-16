type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val count = ref 0;
val str = ref "";
val balancedstr = ref true;

fun eof() = let val pos = hd(!linePos) in 
	if !count <> 0 then 
		(ErrorMsg.error pos ("Unbalanced Comment " ); Tokens.EOF(pos,pos))
	else if !balancedstr <> true then
			(ErrorMsg.error pos ("Unbalanced String "); Tokens.EOF(pos,pos))
		else
			Tokens.EOF(pos,pos)
		end

%%
%s STRING2 STRING COMMENT;
%%

<INITIAL>\" => (YYBEGIN STRING;str := "";balancedstr := false; continue());
<STRING>\\ => (YYBEGIN STRING2;continue());

<STRING>\" => 
	(YYBEGIN INITIAL;
	balancedstr := true;
	Tokens.STRING(!str,yypos, yypos+(size (!str))));

<STRING>. => (YYBEGIN STRING; str := !str ^ yytext; continue());
<STRING>\n => (raise Fail("newline must be typed as followed: \\n"));

<STRING2>(t|n|\\|\"|\r|\f) => (YYBEGIN STRING;
	str := !str ^ "\\";
	str := !str^ yytext;
	continue());

<STRING2>([0-9]{3}) => 
	(YYBEGIN STRING;
	str := !str^(String.str(Char.chr(valOf(Int.fromString yytext))));
	continue());

<STRING2>(\^[a-zA-Z\\\[\]\^_@]) => 
	(YYBEGIN STRING;
	str := !str ^ "\\"^ yytext;
	continue());

<STRING2>(\n)  => 
	(YYBEGIN STRING;
	lineNum := !lineNum+1;
	linePos := yypos :: !linePos;
	continue());

<STRING2>. => (raise Fail("String contians illegal use of backslash"));

<INITIAL>"/*" => (YYBEGIN COMMENT;count := 1;continue());
<COMMENT>"/*" => (YYBEGIN COMMENT;count := !count + 1;continue());
<COMMENT>"*/" => (count := !count - 1;
	if !count = 0 then
 		YYBEGIN INITIAL 
 	else 
 		YYBEGIN COMMENT; 
 		continue());
<COMMENT>. => (continue());
<COMMENT>(\n|\f)  => (YYBEGIN COMMENT; continue());


<INITIAL>(\n) => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>(\t|[ ]|\r|\f) => (continue());

<INITIAL>"," => (Tokens.COMMA(yypos,yypos+1));
<INITIAL>var  	=> (Tokens.VAR(yypos,yypos+3));
<INITIAL>[0-9]+	=> (Tokens.INT(valOf (Int.fromString (yytext)),yypos,yypos+(size (yytext))));

<INITIAL>"type" => (Tokens.TYPE(yypos, yypos+4));
<INITIAL>"break" => (Tokens.BREAK(yypos, yypos+5));
<INITIAL>"of" => (Tokens.OF(yypos, yypos+2));
<INITIAL>"end" => (Tokens.END(yypos, yypos+3));
<INITIAL>"in" => (Tokens.IN(yypos, yypos+2));
<INITIAL>"nil" => (Tokens.NIL(yypos, yypos+3));
<INITIAL>"let" => (Tokens.LET(yypos, yypos+3));
<INITIAL>"do" => (Tokens.DO(yypos, yypos+2));
<INITIAL>"to" => (Tokens.TO(yypos, yypos+2));
<INITIAL>"while" => (Tokens.WHILE(yypos, yypos+5));
<INITIAL>"else" => (Tokens.ELSE(yypos, yypos+4));
<INITIAL>"then" => (Tokens.THEN(yypos, yypos+4));
<INITIAL>"if" => (Tokens.IF(yypos, yypos+2));
<INITIAL>"array" => (Tokens.ARRAY(yypos, yypos+5));
<INITIAL>":=" => (Tokens.ASSIGN(yypos, yypos+6));
<INITIAL>"|" => (Tokens.OR(yypos, yypos+2));
<INITIAL>"&" => (Tokens.AND(yypos, yypos+3));
<INITIAL>"." => (Tokens.DOT(yypos, yypos+1));
<INITIAL>"}" => (Tokens.RBRACE(yypos, yypos+1));
<INITIAL>"{" => (Tokens.LBRACE(yypos, yypos+1));
<INITIAL>"[" => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL>"]" => (Tokens.RBRACK(yypos, yypos+1));
<INITIAL>"(" => (Tokens.LPAREN(yypos, yypos+1));
<INITIAL>")" => (Tokens.RPAREN(yypos, yypos+1));
<INITIAL>";" => (Tokens.SEMICOLON(yypos, yypos+1));
<INITIAL>":" => (Tokens.COLON(yypos, yypos+1));
<INITIAL>")" => (Tokens.RPAREN(yypos, yypos+1));

<INITIAL>"+" => (Tokens.PLUS(yypos, yypos+1));
<INITIAL>"-" => (Tokens.MINUS(yypos, yypos+1));
<INITIAL>"|" => (Tokens.OR(yypos, yypos+1));
<INITIAL>"&" => (Tokens.AND(yypos, yypos+1));
<INITIAL>"*" => (Tokens.TIMES(yypos, yypos+1));
<INITIAL>"/" => (Tokens.DIVIDE(yypos, yypos+1));
<INITIAL>"=" => (Tokens.EQ(yypos, yypos+1));
<INITIAL>"<>" => (Tokens.NEQ(yypos, yypos+2));
<INITIAL>"<" => (Tokens.LT(yypos, yypos+1));
<INITIAL>">" => (Tokens.GT(yypos, yypos+1));
<INITIAL>"<=" => (Tokens.LE(yypos, yypos+2));
<INITIAL>">=" => (Tokens.GE(yypos, yypos+2));

<INITIAL>[a-zA-Z][a-zA-Z0-9_]*  => (Tokens.ID(yytext,yypos, yypos+(size (yytext))));

<INITIAL>.  => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());