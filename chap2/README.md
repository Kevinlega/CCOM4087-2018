# Kevin Legarreta - Lexer

## Nested Comments:
```
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


```
 Here we can see that for every openning of a comment we increase a counter, this way you will have to close every comment before continuing. If not the whole program is a comment and is and error for unbalanced comments.


## String with escape sequences:

```
<INITIAL>\" => (YYBEGIN STRING;str := "";balancedstr := false; continue());
<STRING>\\ => (YYBEGIN STRING2;continue());

<STRING>\" => 
	(YYBEGIN INITIAL;
	balancedstr := true;
	Tokens.STRING(!str,yypos, yypos+(size (!str))));

<STRING>. => (YYBEGIN STRING; str := !str ^ yytext; continue());

<STRING2>(t|n|\\|\"|\r) => (YYBEGIN STRING;
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
```

Este empieza colectando el " luego va al estado string donde toma cualquier caracter excepto el \ y newline. si encuentra un \ va a string2 estado donde acepta algunas opciones provistas en el apendice si no estan ahi hace un raise y para hacer multiples lineas es \ y un enter automatico sin espacios.

Possible string:
"hola"
""
"Soy un string de \
multiples lineas yay"

"\^c ^c no se ejecuta pero se muestran."

Must have in mind that to have multiple lines you must not leave a space after the \, it should be straight to the newline.

Invalid string may be:
"\a"
"\b"

## End of file:
In the end of file if the string or comments are unbalanced we show an error. 