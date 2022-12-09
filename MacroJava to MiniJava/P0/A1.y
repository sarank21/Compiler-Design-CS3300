%{
    #include<stdio.h>
    #include<stdlib.h>
    #include<string.h>
    #include<stdarg.h>

int yylex(void);
void yyerror(const char *c);

struct Node {
    char* str;
    struct Node* next;
};

struct LL {
    struct Node* head;
    struct Node* tail;
};

struct macrostmt {
    char* name;
    struct LL* arguments;
    struct LL* content;
    struct macrostmt* next;
};

struct macroexpr {
    char* name;
    struct LL* arguments;
    struct LL* content;
    struct macroexpr* next;
};

struct macrostmt* heads = NULL;
struct macrostmt* tails = NULL;
struct macroexpr* heade = NULL;
struct macroexpr* taile = NULL;

struct macroLL* macrolist;

struct Node* new_node(char* str, struct Node* next);
struct LL* append_node(char* str);
struct LL* new_list(struct Node* head, struct Node* tail);
struct LL* join_list(struct LL* LL1, struct LL* LL2);
struct LL* join_list_3(struct LL* LL1, struct LL* LL2, struct LL* LL3);
void print(struct LL* start);
void new_macroexpr(char* name, struct LL* args, struct LL* cont);
void new_macrostmt(char* name, struct LL* args, struct LL* cont);
void print_macros();
struct LL* check_macrostmt(struct LL* ident, struct LL* leftp, struct LL* args, struct LL* rightp, struct LL* semicol);
struct LL* check_macroexpr(struct LL* ident, struct LL* leftp, struct LL* exprs, struct LL* rightp);
int args_counter(struct LL* list);
struct LL* change_search(char* name, struct LL* args, struct LL* exp);
struct LL* content_shift(struct LL* body, struct LL* args, struct LL* exp);
    

%}

%union {
    char* id;
    struct LL* ll;
}

%token<id> ID INTEGER PUBLIC STATIC VOID MAIN INT CLASS EXTENDS STRING NEW THIS BOOLEAN TR FA PRINT IF ELSE WHILE RETURN LENGTH LP LBR LB RP RBR RB ADD SUB MUL DIV EQUAL SEMICOLON DOT COMMA RELAND RELOR NOTEQUAL LTEQUAL NOT
%token<id> DE DE0 DE1 DE2 DS DS0 DS1 DS2

%type<ll> Goal MacroDefLL MainClass TypeDeclarationLL class id lbr public static void main lp string lb rb rp print expr semicolon rbr
%type<ll> TypeDeclaration IdentifiersemiLL MethodDeclarationLL extends MethodDeclaration Identifiersemi
%type<ll> type int boolean IdentifiercommaLL StatementLL return comma Identifierdec Statement equal if else while exprLL exprarg Pex MacroDef MacroDefExpr MacroDefStmt multiid 
%type<ll> length and or neq leq add sub mul div dot true false this new not integer ds ds0 ds1 ds2 de de0 de1 de2 commaID

%left NO_ELSE

%%

Goal:  MacroDefLL MainClass TypeDeclarationLL    { $$ = join_list($2, $3);  print($$); }
;

MainClass: class id lbr public static void main lp string lb rb id rp lbr print lp expr rp semicolon rbr rbr { $$ = join_list_3(join_list_3(join_list_3($1, $2, $3), join_list_3($4, $5, $6), join_list_3($7, $8, $9)), join_list_3(join_list_3($10, $11, $12), join_list_3($13, $14, $15), join_list_3($16, $17, $18)), join_list_3($19, $20, $21)); }
;
TypeDeclarationLL:                                  { $$ = new_list(NULL, NULL); }
                | TypeDeclaration TypeDeclarationLL { $$ = join_list($1, $2);}
;
TypeDeclaration: class id lbr IdentifiersemiLL MethodDeclarationLL rbr { $$ = join_list(join_list_3($1, $2, $3), join_list_3($4, $5, $6)); }
                | class id extends id lbr IdentifiersemiLL MethodDeclarationLL rbr { $$ = join_list_3(join_list_3($1, $2, $3), join_list_3($4, $5, $6), join_list($7, $8)); }
;
IdentifiersemiLL:                                   { $$ = new_list(NULL, NULL); }
                |  IdentifiersemiLL Identifiersemi  { $$ = join_list($1, $2);}
 ;           
Identifiersemi: type id semicolon   { $$ = join_list_3($1, $2, $3); }
;
type: int lb rb { $$ = join_list_3($1, $2, $3); }
    | boolean   { $$ = $1; }
    | int       { $$ = $1; }
    | id        { $$ = $1; }
;
MethodDeclarationLL:                                        { $$ = new_list(NULL, NULL);}
                    | MethodDeclaration MethodDeclarationLL { $$ = join_list($1, $2);}
;
MethodDeclaration: public type id lp type id IdentifiercommaLL rp lbr IdentifiersemiLL StatementLL return expr semicolon rbr { $$ = join_list(join_list_3(join_list_3($1, $2, $3), join_list_3($4, $5, $6), join_list_3($7, $8, $9)), join_list(join_list_3($10, $11, $12), join_list_3($13, $14, $15))); }
                | public type id lp rp lbr IdentifiersemiLL StatementLL return expr semicolon rbr { $$ = join_list(join_list(join_list_3($1, $2, $3), join_list_3($4, $5, $6)), join_list(join_list_3($7, $8, $9), join_list_3($10, $11, $12))); }
;
IdentifiercommaLL:                                      { $$ = new_list(NULL, NULL); }
                | IdentifiercommaLL comma Identifierdec { $$ = join_list_3($1, $2, $3); }
;
Identifierdec: type id              { $$ = join_list($1, $2);}
;
StatementLL:                        { $$ = new_list(NULL, NULL); }
            | Statement StatementLL { $$ = join_list($1, $2);}
; 
Statement: lbr StatementLL rbr  { $$ = join_list_3($1, $2, $3); }   
        | print lp expr rp semicolon    { $$ = join_list(join_list($1, $2), join_list_3($3, $4, $5)); }
        | id equal expr semicolon   { $$ = join_list(join_list($1, $2), join_list($3, $4)); }
        | id lb expr rb equal expr semicolon { $$ = join_list_3(join_list_3($1, $2, $3), join_list_3($4, $5, $6), $7); }
        | if lp expr rp Statement else Statement    { $$ = join_list_3(join_list_3($1, $2, $3), join_list_3($4, $5, $6), $7); }
        | if lp expr rp Statement %prec NO_ELSE     { $$ = join_list(join_list_3($1, $2, $3), join_list($4, $5)); }
        | while lp expr rp Statement        { $$ = join_list(join_list_3($1, $2, $3), join_list($4, $5)); }
        | id lp exprLL rp semicolon     { $$ = check_macrostmt($1, $2, $3, $4, $5); }
;

exprLL: expr            { $$ = $1; }
        | expr comma exprLL { $$ = join_list_3($1, $2, $3); }
;
expr: Pex and Pex       { $$ = join_list_3($1, $2, $3); }
    | Pex or Pex        { $$ = join_list_3($1, $2, $3); }
    | Pex neq Pex       { $$ = join_list_3($1, $2, $3); }
    | Pex leq Pex       { $$ = join_list_3($1, $2, $3); }
    | Pex add Pex       { $$ = join_list_3($1, $2, $3); }
    | Pex sub Pex       { $$ = join_list_3($1, $2, $3); }
    | Pex mul Pex       { $$ = join_list_3($1, $2, $3); }
    | Pex div Pex       { $$ = join_list_3($1, $2, $3); }
    | Pex lb Pex rb     { $$ = join_list(join_list($1, $2), join_list($3, $4)); }
    | Pex dot length    { $$ = join_list_3($1, $2, $3); }
    | Pex               { $$ = $1; }
    | Pex dot id lp exprarg rp  { $$ = join_list(join_list_3($1, $2, $3), join_list_3($4, $5, $6)); }
    | id lp exprarg rp  { $$ = check_macroexpr($1, $2, $3, $4); }
;
exprarg:                { $$ = new_list(NULL, NULL);}
        | exprLL        { $$ = $1; }
;
Pex: integer            { $$ = $1; }
    | true              { $$ = $1; }
    | false             { $$ = $1; }
    | id                { $$ = $1; }
    | this              { $$ = $1; }
    | new int lb expr rb    { $$ = join_list(join_list_3($1, $2, $3), join_list($4, $5)); }
    | new id lp rp      { $$ = join_list(join_list($1, $2), join_list($3, $4)); }
    | not expr          { $$ = join_list($1, $2);}
    | lp expr rp        { $$ = join_list_3($1, $2, $3); }
;
MacroDefLL:             { $$ = new_list(NULL, NULL);}
        | MacroDef MacroDefLL   { $$ = join_list($1, $2);}
;
MacroDef: MacroDefExpr  { $$ = $1; }
        | MacroDefStmt  { $$ = $1; }
;
MacroDefExpr: de id lp id comma id comma id multiid rp lp expr rp
            {
                new_macroexpr($2->head->str, join_list(join_list_3($4, $5, $6), join_list_3($7, $8, $9)), join_list_3($11, $12, $13));
            }
            | de0 id lp rp lp expr rp
            {
                new_macroexpr($2->head->str, new_list(NULL, NULL), join_list_3($5, $6, $7));
            }
            | de1 id lp id rp lp expr rp
            {
                new_macroexpr($2->head->str, $4, join_list_3($6, $7, $8));
            }
            | de2 id lp id comma id rp lp expr rp
            {
                new_macroexpr($2->head->str, join_list_3($4, $5, $6), join_list_3($8, $9, $10));
            }
;
MacroDefStmt: ds id lp id comma id comma id multiid rp lbr StatementLL rbr
            {
                new_macrostmt($2->head->str, join_list(join_list_3($4, $5, $6), join_list_3($7, $8, $9)), join_list_3($11, $12, $13));
            }
            | ds0 id lp rp lbr StatementLL rbr
            {
                new_macrostmt($2->head->str, new_list(NULL, NULL), join_list_3($5, $6, $7));
            }
            | ds1 id lp id rp lbr StatementLL rbr
            {
                new_macrostmt($2->head->str, $4, join_list_3($6, $7, $8));
            }
            | ds2 id lp id comma id rp lbr StatementLL rbr
            {
                new_macrostmt($2->head->str, join_list_3($4, $5, $6), join_list_3($8, $9, $10));
            }
           
;
multiid:                    { $$ = new_list(NULL, NULL);}
        | commaID multiid  { $$ = join_list($1, $2); }

commaID: comma id { $$ = join_list($1, $2); }
;

class: CLASS { $$ = append_node($1); };
extends: EXTENDS { $$ = append_node($1); };
id: ID { $$ = append_node($1); };
lbr: LBR { $$ = append_node($1); };
public: PUBLIC { $$ = append_node($1); };
static: STATIC { $$ = append_node($1); };
void: VOID { $$ = append_node($1); };
main: MAIN { $$ = append_node($1); };
lp: LP { $$ = append_node($1); };
string: STRING { $$ = append_node($1); }; 
lb: LB { $$ = append_node($1); };
rb: RB { $$ = append_node($1); };
rp: RP { $$ = append_node($1); };
print: PRINT { $$ = append_node($1); };  
semicolon: SEMICOLON { $$ = append_node($1); };
rbr: RBR { $$ = append_node($1); };
return: RETURN { $$ = append_node($1); };
int: INT  { $$ = append_node($1); }  ;
boolean: BOOLEAN { $$ = append_node($1); };
equal: EQUAL { $$ = append_node($1); };
if: IF { $$ = append_node($1); };
else: ELSE { $$ = append_node($1); };
while: WHILE { $$ = append_node($1); };
comma: COMMA { $$ = append_node($1); };
length: LENGTH { $$ = append_node($1); };
and: RELAND { $$ = append_node($1); };
or: RELOR { $$ = append_node($1); };
neq: NOTEQUAL { $$ = append_node($1); };
leq: LTEQUAL { $$ = append_node($1); };
add: ADD { $$ = append_node($1); };
sub: SUB { $$ = append_node($1); };
mul: MUL { $$ = append_node($1); };
div: DIV { $$ = append_node($1); };
dot: DOT { $$ = append_node($1); };
true: TR { $$ = append_node($1); };
false: FA { $$ = append_node($1); };
this: THIS { $$ = append_node($1); };
new: NEW { $$ = append_node($1); };
not: NOT { $$ = append_node($1); };
integer: INTEGER { $$ = append_node($1); };
ds: DS { $$ = append_node($1); };
ds0: DS0 { $$ = append_node($1); };
ds1: DS1 { $$ = append_node($1); };
ds2: DS2 { $$ = append_node($1); };
de: DE { $$ = append_node($1); };
de0: DE0 { $$ = append_node($1); };
de1: DE1 { $$ = append_node($1); };
de2: DE2 { $$ = append_node($1); };

%%

void yyerror (const char *s)    {
    printf("//Failed to parse code\n");
}

int main()
{
    yyparse();
    return 0;
}

struct Node* new_node(char* str, struct Node* next) {
    struct Node* temp = (struct Node*)malloc(sizeof(struct Node));
    temp->str = str;
    temp->next = next;
    return temp;
}

struct LL* append_node(char* str)   {
    struct Node* temp = new_node(str, NULL);
    struct LL* temp1 = new_list(temp, temp);
    return temp1;
}

struct LL* new_list(struct Node* head, struct Node* tail) {
    struct LL* temp = (struct LL*)malloc(sizeof(struct LL));
    temp->head = head;
    temp->tail = tail;
    return temp;
}

struct LL* join_list(struct LL* LL1, struct LL* LL2) {
    struct LL* temp;
    if(LL2->tail==NULL)
        return LL1;
    if(LL1->head==NULL)
        return LL2;
    LL1->tail->next = LL2->head;
    temp = new_list(LL1->head, LL2->tail);
    return temp;
}

struct LL* join_list_3(struct LL* LL1, struct LL* LL2, struct LL* LL3) {
    struct LL* temp = join_list(LL1, LL2);
    struct LL* temp1 = join_list(temp, LL3);
    return temp1;
}


void print_tabs(int tabs) {
    for (int i = 0; i < tabs; i++)
        printf("    ");
}

void print(struct LL* start) {
    int tabs = 0, at_next_line=0;
    struct Node* trav = start->head;
    while (trav != NULL) {
        if(strlen(trav->str)>0) {
            if(at_next_line) {
                if(strcmp(trav->str, "}") == 0) 
                    print_tabs(tabs - 1);
                else 
                    print_tabs(tabs);
                at_next_line = 0;
            }
            printf("%s ", trav->str);
            if(strcmp(trav->str, ";") == 0) {
                printf("\n");
                at_next_line = 1;
            }
            else if(strcmp(trav->str, "{") == 0) {
                tabs++;
                printf("\n");
                at_next_line = 1;
            }
            else if(strcmp(trav->str, "}") == 0) {
                tabs--;
                printf("\n");
                at_next_line = 1;
            }
        }
        trav = trav->next;
    }
}

void new_macroexpr(char* name, struct LL* args, struct LL* cont) {
    struct macroexpr* temp = (struct macroexpr*)malloc(sizeof(struct macroexpr));
    temp->name = name;
    temp->arguments = (struct LL*)malloc(sizeof(struct LL));
    temp->arguments = args;
    temp->content = (struct LL*)malloc(sizeof(struct LL));
    temp->content = cont;
    if(heade==NULL) { 
        heade = temp;
        taile = temp;
    }
    else    { 
        taile->next = temp;
        taile = temp;
    }
}

void new_macrostmt(char* name, struct LL* args, struct LL* cont) {
    struct macrostmt* temp = (struct macrostmt*)malloc(sizeof(struct macrostmt));
    temp->name = name;
    temp->arguments = (struct LL*)malloc(sizeof(struct LL));
    temp->arguments = args;
    temp->content = (struct LL*)malloc(sizeof(struct LL));
    temp->content = cont;
    if(heads==NULL) { 
        heads = temp;
        tails = temp;
    }
    else    {
        tails->next = temp;
        tails = temp;
    }
}

/* void print_macros()
{
    struct macroexpr* temp1 = (struct macroexpr*)malloc(sizeof(struct macroexpr));
    struct macrostmt* temp2 = (struct macrostmt*)malloc(sizeof(struct macrostmt));
    temp1 = heade;
    temp2 = heads;
    while(temp1!=NULL)
    {
        printf("%s\n", temp1->name);
        print(temp1->arguments);
        printf("\n%d\n", args_counter(temp1->arguments));
        print(temp1->content);
        temp1 = temp1->next;
    }
    while(temp2!=NULL)
    {
        printf("%s\n", temp2->name);
        print(temp2->arguments);
        printf("\n%d\n", args_counter(temp2->arguments));
        print(temp2->content);
        temp2 = temp2->next;
    }
} */
 
int args_counter(struct LL* LL) {
    int count = 0;
    struct Node* trav = LL->head;
    while (trav != NULL) {
        if(strcmp(trav->str, ",") == 0) 
            count++;
        trav = trav->next;
    }
    return count;
}

int check_args(struct LL* LL1, struct LL* LL2)
{
    struct Node* node1=LL1->head;
    struct Node* node2=LL2->head;
    int count1=0, count2=0;
    while(node1!=NULL && node2!=NULL) {
        if(node1!=NULL && strcmp(node1->str, ",")==0)
            count1++;
        if(node2!=NULL && strcmp(node2->str, ",")==0)
            count2++;
        node1 = node1->next;
        node2 = node2->next;
    }
    if(count1==count2)
        return 1;
    return 0;
}

struct LL* change_search(char* name, struct LL* args, struct LL* arg) { 
    struct Node* trav1 = args->head;
    int index = 0, flag = 0;
    while(trav1 != NULL) {
        if(strcmp(trav1->str, name) == 0) {
            flag = 1;
            break;
        }
        else if(strcmp(trav1->str, ",") == 0) {
            index++;
        }
        trav1 = trav1->next;
    }
    if(flag==0) {
        struct Node* node = new_node(name, NULL);
        return new_list(node, node);
    }
    struct Node* trav2 = arg->head;
    struct LL* ret_list = new_list(NULL, NULL);
    int ind = 0;
    while(trav2 != NULL) {
        if(strcmp(trav2->str, ",") == 0) {
            ind++;
            if(ind > index) 
                return ret_list;
        }
        else if(ind == index) {
            struct Node* node = new_node(trav2->str, NULL);
            ret_list = join_list(ret_list, new_list(node, node));
        }
        trav2 = trav2->next;
    }
    return ret_list;
}

struct LL* content_shift(struct LL* content, struct LL* args, struct LL* arg) { 
    struct LL* ret_list = new_list(NULL, NULL);
    struct Node* trav = content->head;
    while(trav != NULL) {
        ret_list = join_list(ret_list, change_search(trav->str, args, arg));
        trav = trav->next;
    }
    return ret_list;
}

struct LL* check_macrostmt(struct LL* ident, struct LL* leftp, struct LL* args, struct LL* rightp, struct LL* semicol) {
    struct macrostmt* trav = heads;
    while(trav!=NULL) {
        if(strcmp(trav->name, ident->head->str)==0 && check_args(args, trav->arguments)) {
            if(args_counter(args)==0)
                return trav->content;
            struct LL* nlist = content_shift(trav->content, trav->arguments, args);
            return nlist;
        }
        trav = trav->next;
    }
    return join_list(join_list_3(ident, leftp, args), join_list(rightp, semicol));
}
struct LL* check_macroexpr(struct LL* ident, struct LL* leftp, struct LL* exprs, struct LL* rightp) {
    struct macroexpr* trav = heade;
    while(trav!=NULL) {
        if(strcmp(trav->name, ident->head->str)==0 && check_args(exprs, trav->arguments)) {
            if(args_counter(exprs)==0)
                return trav->content;
            struct LL* nlist = content_shift(trav->content, trav->arguments, exprs);
            return nlist;
        }
        trav = trav->next;
    }
    return join_list(join_list_3(ident, leftp, exprs), rightp);
}


#include "lex.yy.c"