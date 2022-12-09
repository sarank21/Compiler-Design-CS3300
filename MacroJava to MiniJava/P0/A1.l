%{
#include <stdio.h>
#include <stdlib.h>
%}

INTEGER [0-9]+
ID      [a-zA-Z_][A-Za-z0-9_]*
COMMENT "//".*\n|"/*".*"*/" 
SPACES  [ \t\n\r]+

%%
{COMMENT}   {}
"#defineExpr"    {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return DE;}
"#defineExpr0"    {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return DE0;}
"#defineExpr1"    {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return DE1;}
"#defineExpr2"   {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return DE2;}
"#defineStmt"    {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return DS;}
"#defineStmt0"    {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return DS0;}
"#defineStmt1"    {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return DS1;}
"#defineStmt2"    {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return DS2;}
"public"        {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return PUBLIC;}
"static"        {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return STATIC;}
"void"          {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return VOID;}
"main"          {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return MAIN;}
"class"         {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return CLASS;}
"extends"       {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return EXTENDS;}
"int"           {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return INT;}
"String"        {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return STRING;}
"new"           {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return NEW;}
"this"          {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return THIS;}
"boolean"       {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return BOOLEAN;}
"true"          {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return TR;}
"false"         {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return FA;}
"System.out.println"    {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return PRINT;}
"if"            {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return IF;}
"else"          {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return ELSE;}
"while"         {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return WHILE;}
"return"        {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return RETURN;}
"length"        {yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return LENGTH;}
"("				{yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return LP;}
"{"				{yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return LBR;}
"["				{yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return LB;}
")"				{yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return RP;}
"}"				{yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return RBR;}
"]"				{yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return RB;}
"+"				{yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return ADD;}
"-"				{yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return SUB;}
"*"				{yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return MUL;}
"/"				{yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return DIV;}
"="				{yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return EQUAL;}
";"				{yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return SEMICOLON;}
"."				{yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return DOT;}
","				{yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return COMMA;}
"&&"			{yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return RELAND;}
"||"			{yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return RELOR;}
"!="			{yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return NOTEQUAL;}
"<="			{yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return LTEQUAL;}
"!"				{yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);return NOT;}
{ID}     {
    yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);
    return ID;
}
{INTEGER}   {
    yylval.id = (char*)malloc(sizeof(yytext));
    strcpy(yylval.id, yytext);
    return INTEGER;
}
{SPACES}    {}
.       {
    printf("ERROR: Unknown character %s\n", yytext);
    exit(0);
}
%%