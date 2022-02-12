/* File parser.mly */
%token <string> INT
%token <string> VAL
%token <string> IDENT
%token PLUS ARROW 
%token COMMA
%token SEMICOLON
%token LPAREN RPAREN
%token EOL
%token EOF
%start reactions        /* the entry point */
%type <Types.reaction list> reactions
%%
reactions:
    reaction_star EOF                   { $1  }
;

reaction_star:
  /* empty */                           { [] }
| reaction reaction_star                { $1::$2 }
;

reaction:
  float_item COMMA int_item COMMA int_item COMMA int_item COMMA int_item COMMA int_item SEMICOLON   { ($1,$3,($5,$7),($9,$11))  }
;
                 
int_item:   
 INT       { int_of_string $1 };    

float_item:   
 VAL       { float_of_string $1 };    

 /*  I removed this from the definition of the species    */
 /*  | each                             { [$1] } */
     