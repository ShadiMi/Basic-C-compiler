
%%
"bool"			{ yylval = mknode(yytext,NULL,NULL); return BOOL; }
"char"			{ yylval = mknode(yytext,NULL,NULL); return CHAR; }
"int"			{ yylval = mknode(yytext,NULL,NULL); return INT; }
"real"			{ yylval = mknode(yytext,NULL,NULL); return REAL; }
"string"		{ yylval = mknode(yytext,NULL,NULL); return STRING; }
"int\*"			{ yylval = mknode(yytext,NULL,NULL); return INTP; }
"char\*"		{ yylval = mknode(yytext,NULL,NULL); return CHARP; }
"real\*"		{ yylval = mknode(yytext,NULL,NULL); return REALP; }
"if"			{ return IF; }
"elif"			{ return ELIF; }
"else"			{ return ELSE; }
"while"			{ return WHILE; }
"for"			{ return FOR; }
"var"			{ return VAR; }
"par"[0-9]+		{ yylval = mknode(yytext,NULL,NULL); return PAR; }
"return"		{ return RETURN; }
"null"			{ return NULLT; }
"do"			{ return DO; }
"returns"		{ return RETURNS; }
"begin"			{ return BEGINT; }
"end"			{ return END; }
"def"			{ return DEF; }
"call"			{ return CALL; }
"and"			{ return AND; }
"not"			{ return NOT; }
"or"			{ return OR; }
"type"			{ return TYPED; }
"__main__"		{ return MAIN; }

"/"				{ return DIV; }
"="				{ return ASSIGN; }
"=="			{ return EQ; }
">"				{ return GT; }
">="			{ return GE; }
"<"				{ return LT; }
"<="			{ return LE; }
"-"				{ return MINUS; }
"!="			{ return NEQ; }
"+"				{ return PLUS; }
"*"				{ return MULT; }
"&"				{ return ADDR; }

":"				{ return COLON; }
";"				{ return SEMI; }
","				{ return COMMA; }
"|"				{ return BAR; }
"("				{ return LPAREN; }
")"				{ return RPAREN; }
"["				{ return LBRACKET; }
"]"				{ return RBRACKET; }

[0-9]+ { yylval = mknode(yytext,NULL,NULL); return NUM; }
0[xX][0-9a-fA-F]+ { yylval = mknode(yytext,NULL,NULL); return HEXNUM; }
[a-zA-Z][a-zA-Z0-9]* {yylval = mknode(yytext,NULL,NULL);return ID;}
["TRUE"|"FALSE"] { yylval = mknode(yytext,NULL,NULL); return BOOL_LITERAL; }
\'[a-zA-Z0-9]\' {yylval = mknode(yytext,NULL,NULL); return CHAR_LITERAL;}
\"[^\"]*\" { yylval = mknode(yytext,NULL,NULL); return STRING_LITERAL; }
[+-]?([0-9]+\.[0-9]*([eE][+-]?[0-9]+)?|\.[0-9]+([eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+) { yylval = mknode(yytext,NULL,NULL); return REAL_LITERAL; }
[ \t\n\r]+ ;
. {yylval = mknode(yytext,NULL,NULL);return yytext[0];}
\#\-\>[a-zA-Z0-9\t\n\r]*\<\-\# ;
%%