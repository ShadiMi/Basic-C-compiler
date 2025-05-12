%token BOOL CHAR INT REAL STRING INTP CHARP REALP IF ELIF ELSE WHILE FOR VAR PAR NULLT TYPED
%token RETURN DO RETURNS BEGINT END DEF CALL AND MAIN
%token ASSIGN EQ GT GE LT LE NOT NEQ OR PLUS MINUS MULT DIV ADDR
%token COLON SEMI COMMA BAR LPAREN RPAREN LBRACKET RBRACKET
%token NUM ID BOOL_LITERAL CHAR_LITERAL STRING_LITERAL REAL_LITERAL HEXNUM
%left OR
%left AND
%left EQ NEQ
%left GT GE LT LE
%left PLUS MINUS
%left MULT DIV
%right NOT UMINUS

%{

	#include<stdio.h>
	#include<string.h>
	#include<stdlib.h>

	typedef struct node
	{
		char *token;
		struct node *left;
		struct node *right;
	} node;

	node *mknode(char *token, node *left, node *right);
	void printtree(node *tree,int tabs);

	#define YYSTYPE struct node*
	#define YYDEBUG 1

%}

%%
code: funcs {printf("CODE\n"); printtree($1, -1); printf("\n");}
;

funcs: func funcs {$$=mknode("",$1,$2); }
	|func {$$=$1;}
;

func: DEF ID LPAREN parameter_list RPAREN COLON RETURNS type var_list body { $$=mknode("FUNC",mknode("",$2,$4),mknode("",mknode("RET",$8,$9),mknode("BODY",$10,NULL)));}
	|DEF ID LPAREN parameter_list RPAREN COLON var_list body { $$=mknode("FUNC",mknode("",$2,$4),mknode("",mknode("RET NONE",NULL,$7),mknode("BODY",$8,NULL)));}
	|DEF MAIN LPAREN RPAREN COLON var_list body { $$=mknode("MAIN",$6,$7) ;}
;

parameter_list: param SEMI parameter_list { $$=mknode("",$1,$3) ;}
	|param { $$=$1; }
;

param:PAR type COLON ID { $$=mknode("",mknode("",$1,$2),mknode("",$4,NULL)) ;}
;

type: INT {$$=$1;} 
	|CHAR {$$=$1;}
	|REAL {$$=$1;}
	|INTP {$$=$1;}
	|CHARP {$$=$1;}
	|REALP {$$=$1;}
	|STRING {$$=$1;}
;

var_list: /* empty */{$$=NULL;}
	|VAR declarations { $$=mknode("",$1,$2);}
;

declarations: /* empty */{$$=NULL;}
	|TYPED type COLON decl SEMI declarations {$$=mknode("VARS",$1,mknode("",$3,$5));}
;

decl: array { $$=$1; }
    |ID COLON literal COMMA decl { $$ = mknode("var", mknode("", $1, $3), mknode("", $5, NULL)); }
    |ID COLON literal { $$ = mknode("var", $1, $3); }
    |ID COMMA decl { $$ = mknode("var", $1, mknode("", $3, NULL)); }  
    |ID { $$ = $1; }  
;

array: ID LBRACKET NUM RBRACKET COLON STRING_LITERAL COMMA decl { $$ = mknode("array", mknode("", $1, $3), mknode("", $6, $8)); }
    |ID LBRACKET NUM RBRACKET COLON STRING_LITERAL { $$ = mknode("array", $1, $3); }
    |ID LBRACKET NUM RBRACKET COMMA decl { $$ = mknode("array", mknode("", $1, $3), mknode("", $6, NULL)); }
    |ID LBRACKET NUM RBRACKET { $$ = mknode("array", $1, $3); }
    ;

literal: NUM {$$=$1;}
	|ID {$$=$1;}
	|BOOL_LITERAL {$$=$1;}
	|CHAR_LITERAL {$$=$1;}
	|STRING_LITERAL {$$=$1;}
	|REAL_LITERAL {$$=$1;}
	|HEXNUM {$$=$1;}
	|ADDR ID{$$=$1;}
;

body: BEGINT opt_funcs statements END {$$=mknode("",$2,$3);}
;

opt_funcs: funcs { $$ = $1; }
         | /* empty */ { $$ = NULL; }
;

statements: statement statements {$$=mknode("BLOCK",$1,$2);}
	|statement {$$=$1;}
	| /* empty */ { $$ = NULL; }
;

statement:assignment SEMI {$$=$1;}
	|func_call_stmt SEMI  {$$=$1;}
	|if_stmt  {$$=$1;}
	|while_stmt  {$$=$1;}
	|do_while_stmt  {$$=$1;}
	|for_stmt  {$$=$1;}
	|return_stmt SEMI  {$$=$1;}
;

assignment:lhs ASSIGN expr {$$=mknode("=",$1,$3);}
;

lhs: ID  {$$=$1;}
	|MULT ID  {$$=$2;}
	|ID LBRACKET expr RBRACKET {$$=mknode("ARRAY",$1,$3);}
;

return_stmt: RETURN { $$=$1; }
	|RETURN expr {$$=mknode("RETURN",$2,NULL);}
;

if_stmt:IF expr COLON statement {$$=mknode("IF",$2,$4);}
	|IF expr COLON body {$$=mknode("IF",$2,$4);}
	|IF expr COLON statement ELSE COLON statement {$$=mknode("IF",$2,mknode("BLOCK",$4,$7));}
	|IF expr COLON body ELSE COLON body{$$=mknode("IF",$2,mknode("BLOCK",$4,$7));}
;

func_call_stmt: CALL ID LPAREN expr_list RPAREN { $$ = mknode("CALL",mknode("",$2,$4),NULL); }
    | lhs ASSIGN CALL ID LPAREN expr_list RPAREN { $$ = mknode("CALL",mknode("",$1,mknode("",$4,$6)),NULL); }
;
while_stmt: WHILE expr COLON statement { $$ = mknode("WHILE",$2,$4); }
    | WHILE expr COLON var_list body { $$ = mknode("WHILE",mknode("",$2,$4),$5); }
;

do_while_stmt: DO COLON var_list body WHILE COLON expr SEMI { $$ = mknode("DO_WHILE",$4,$7); }
    ;

for_stmt: FOR LPAREN assignment SEMI expr SEMI assignment RPAREN COLON statement { $$ = mknode("FOR",mknode("",mknode("",$3,$5),$7),$10); }
	|FOR LPAREN assignment SEMI expr SEMI assignment RPAREN COLON body { $$ = mknode("FOR",mknode("",mknode("",$3,$5),$7),$10); }
;

expr_list: expr COMMA expr_list { $$ = mknode(",", $1, $3); }
         | expr { $$ = $1; }
         | /* empty */ { $$ = NULL; }
;

expr: expr OR expr            { $$ = mknode("||",$1,$3); }
    | expr AND expr           { $$ = mknode("&&",$1,$3); }
    | expr EQ expr            { $$ = mknode("==",$1,$3); }
    | expr NEQ expr           { $$ = mknode("!=",$1,$3); }
    | expr GT expr            { $$ = mknode(">",$1,$3); }
    | expr GE expr            { $$ = mknode(">=",$1,$3); }
    | expr LT expr            { $$ = mknode("<",$1,$3); }
    | expr LE expr            { $$ = mknode("<=",$1,$3); }
    | expr PLUS expr          { $$ = mknode("+",$1,$3); }
    | expr MINUS expr         { $$ = mknode("-",$1,$3); }
    | expr MULT expr          { $$ = mknode("*",$1,$3); }
    | expr DIV expr           { $$ = mknode("/",$1,$3); }
    | NOT expr                { $$ = mknode("NOT",$2,NULL); }
    | MINUS expr %prec UMINUS { $$ = mknode("UMINUS",$2,NULL); }
    | LPAREN expr RPAREN      { $$ = $2; }
    | MULT ID 				{ $$=$2; }
    | literal                 { $$ = $1; }
;
%%

#include "lex.yy.c"

int main()
{
	yydebug = 1;
    return yyparse();
}

node *mknode(char *token,node *left,node *right) { 
    node *newnode = (node*)malloc(sizeof(node)); 
    char *newstr = (char*)malloc(sizeof(token) + 1); 
    strcpy(newstr,token); 
    newnode->left = left; 
    newnode->right = right; 
    newnode->token = newstr; 
    return newnode; 
}

void printtree(node *tree, int tabs){
	if( tabs==0 )tabs++;
	for(int i=0; i<tabs;i++,printf("\t"));
    if(strcmp(tree->token,"")){
        printf("%s", tree->token);
	}
    if(tree->left){
    	if((strcmp(tree->token,"")!=0)&&(strcmp(tree->token,"IF")!=0)&&(strcmp(tree->token,"BLOCK")!=0)){
    		printf("(");
        	printtree(tree->left, tabs+1);
    	}
        else{
        	printf("\n");
        	printtree(tree->left, tabs);
        }
    }
    if(tree->right){
    	if((strcmp(tree->token,"")!=0)&&(strcmp(tree->token,"IF")!=0)&&(strcmp(tree->token,"BLOCK")!=0)){
        	printtree(tree->right, tabs+1);
        	printf(")");
    	}
        else{
        	printf("\n");
        	printtree(tree->right, tabs);
        }
    }

}


int yyerror()
{
    printf("Couldn't finish parsing.\n");
    return 0;
}
