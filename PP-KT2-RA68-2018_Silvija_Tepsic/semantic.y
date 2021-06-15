%{
  #include <stdio.h>
  #include <stdlib.h>
  #include "defs.h"
  #include "symtab.h"
  #include "codegen.h"	  

  int yyparse(void);
  int yylex(void);
  int yyerror(char *s);
  void warning(char *s);

  extern int yylineno;
  int out_lin = 0;
  char char_buffer[CHAR_BUFFER_LENGTH];
  int error_count = 0;
  int warning_count = 0;
  int var_num = 0;
  int fun_idx = -1;
  int fcall_idx = -1;
  int lab_num = -1;
  FILE *output;
 
  int tip_konstante[100];
  int broj_konstanti=0;
  int indeksi_konstanti[100];
  int tip_parametra[100];
  int par_num=0;
  int arg_num=0;
  int tiptip=-1;
 int ret_num=0;


int iterate_provera=0;
int iterate_provera2=0;
int check_provera=0;
 
int generisanje_visestruka_dodela=0;
int inicijalizovano[100];

struct parametri_struktura{
	int tipovi[100];
};
struct parametri_struktura par[64];

int pamtiArgumente[64];

int reg=-1;

int case_flag = 1;

int check_lab_num=-1;

int iterate_lab_num=-1;



%}

%union {
  int i;
  char *s;
}

%token <i> _TYPE
%token _IF
%token _ELSE
%token _RETURN
%token <s> _ID
%token <s> _INT_NUMBER
%token <s> _UINT_NUMBER
%token _LPAREN
%token _RPAREN
%token _LBRACKET
%token _RBRACKET
%token _ASSIGN
%token _SEMICOLON
%token <i> _AROP
%token <i> _RELOP
%token _ITERATE
%token _STEP
%token _TO
%token _CHECK
%token _FINISH
%token _DEFAULT
%token _CASE
%token _IMPLIES
%token _COMMA
%token _INC
%token _QUESTIONMARK
%token _COLON

%type <i> num_exp exp literal function_call rel_exp pomocna_statement if_part conditional_exp

%nonassoc ONLY_IF
%nonassoc _ELSE

%%

program
  : globalne_lista function_list
      {  
        if(lookup_symbol("main", FUN) == NO_INDEX)
          err("undefined reference to 'main'");
       }
  ;

globalne_lista
 :  globalne_lista globalna
 |
 ;

globalna
 : _TYPE _ID _SEMICOLON
  { 
	if( $1 == VOID){
		err("Global variable cannot be void.");
			}
	else if(lookup_symbol($2, GLOVAR) == NO_INDEX){
          insert_symbol($2, GLOVAR, $1, NO_ATR, NO_ATR);
	}
        else 
           err("redefinition of '%s'", $2);
	
        code("\n%s:\n\t\tWORD\t1", $2);
      }
 ;
function_list
  : function
  | function_list function
  ;

function
  : _TYPE _ID
      {
        fun_idx = lookup_symbol($2, FUN);
        if(fun_idx == NO_INDEX){
          fun_idx = insert_symbol($2, FUN, $1, NO_ATR, NO_ATR);
		inicijalizovano[0]=1;}
        else 
          err("redefinition of function '%s'", $2);
	code("\n%s:", $2);
        code("\n\t\tPUSH\t%%14");
        code("\n\t\tMOV \t%%15,%%14");
      }
    _LPAREN parameter _RPAREN body
      {
	for(int x=0;x<=var_num;x++){
		if(inicijalizovano[x]==0)  
		{
		err("Not all variables have been initialized");
		break;}
		}
        clear_symbols(fun_idx + 1);
        var_num = 0;
	par_num = 0;
	if((get_type(fun_idx)!=VOID) && (ret_num==0))
	warning("Function should have return statement if its type is not void");
	ret_num=0; 
	 code("\n@%s_exit:", $2);
        code("\n\t\tMOV \t%%14,%%15");
        code("\n\t\tPOP \t%%14");
        code("\n\t\tRET");
      }
  ;

parameter
  : /* empty */
      { set_atr1(fun_idx, 0); }
  | parameters {set_atr1(fun_idx, par_num);}
  ;
parameters
  : _TYPE _ID
      {
	if($1 == VOID)
		{
			err("Parameter can not be void.");
		}
	else{
	par_num++;
	par[fun_idx].tipovi[par_num]=$1;
        insert_symbol($2, PAR, $1, par_num, NO_ATR);
        }
      }
  | parameters _COMMA _TYPE _ID
	{
	if($3 == VOID)
		{
			err("Parameter can not be void.");
		}
	else{
	par_num++;
	par[fun_idx].tipovi[par_num]=$3;
	insert_symbol($4, PAR, $3, par_num, NO_ATR);
        }
	}
  ;

body
  : _LBRACKET variable_list
      {
        if(var_num)
          code("\n\t\tSUBS\t%%15,$%d,%%15", 4*var_num);
        code("\n@%s_body:", get_name(fun_idx));
      }
    statement_list _RBRACKET
  ;

variable_list
  : /* empty */
  | variable_list variable
  ;

variable
  : variable_continued _SEMICOLON
  ;
variable_continued
  : _TYPE _ID pomocna_statement
	{ tiptip=$1;
	if($1== VOID) err("Variable can not be void.");
	if($1 != $3 && $3 !=13)
		err("Types are not matching");
	else if(lookup_symbol($2, VAR|PAR) == NO_INDEX){
        int simx=insert_symbol($2, VAR, $1, ++var_num, NO_ATR);
		if($3==1 || $3==2){
		inicijalizovano[var_num]=1;
		gen_mov(generisanje_visestruka_dodela,simx);
		}
		else
		inicijalizovano[var_num]=0;
		}
        else 
           err("redefinition of '%s'", $2);
	}
  | variable_continued _COMMA _ID
	{
	if(lookup_symbol($3, VAR|PAR) == NO_INDEX){
           insert_symbol($3, VAR, tiptip, ++var_num, NO_ATR);
		inicijalizovano[var_num]=0;
		
			}
	 else 
           err("redefinition of '%s'", $3);
	}
  | variable_continued _COMMA _ID _ASSIGN literal
	{
	if(tiptip != get_type($5))
		err("Types are not matching");
	else if(lookup_symbol($3, VAR|PAR) == NO_INDEX){
          int simx=insert_symbol($3, VAR, tiptip, ++var_num, NO_ATR);
		inicijalizovano[var_num]=1;
		gen_mov($5, simx);
		}
        else 
           err("redefinition of '%s'", $3);
 	}
  ;
 
pomocna_statement
 : {$$=13;}
 | _ASSIGN literal
	{$$=get_type($2);
	generisanje_visestruka_dodela=$2;}
 ;
statement_list
  : /* empty */
  | statement_list statement
  ;

statement
  : compound_statement
  | assignment_statement
  | if_statement
  | inc_statement
  | iterate_statement
  | check_statement {
	broj_konstanti=0;
		}
  | return_statement
  ;

compound_statement
  : _LBRACKET statement_list _RBRACKET
  ;

assignment_statement
  : _ID _ASSIGN num_exp _SEMICOLON
      {
        int idx = lookup_symbol($1, VAR|PAR|GLOVAR);
        if(idx == NO_INDEX)
          err("invalid lvalue '%s' in assignment", $1);
        else if(get_type(idx) != get_type($3))
            err("incompatible types in assignment");
	else if(get_kind(idx) == VAR)
	inicijalizovano[get_atr1(idx)]=1;
	gen_mov($3, idx);
      }
  ;

num_exp
  : exp
  | num_exp _AROP exp
      {
        if(get_type($1) != get_type($3))
          err("invalid operands: arithmetic operation");

else{
	 int t1 = get_type($1);    
        code("\n\t\t%s\t", ar_instructions[$2 + (t1 - 1) * AROP_NUMBER]);
        gen_sym_name($1);
        code(",");
        gen_sym_name($3);
        code(",");
        free_if_reg($3);
        free_if_reg($1);
        $$ = take_reg();
        gen_sym_name($$);
        set_type($$, t1);
	}
}
  ;

exp
  : literal
  | _LPAREN rel_exp _RPAREN _QUESTIONMARK conditional_exp _COLON conditional_exp
	{
	if(get_type($5)!=get_type($7))
		err("Types in ternary operator do not match.");

	int regx = take_reg();
        	lab_num++;

	code("\n\t\t%s\t@false%d", opp_jumps[$2],lab_num);
        	code("\n@true%d:", lab_num);
        	gen_mov($5, regx);
        	code("\n\t\tJMP \t@exit%d", lab_num);

        	code("\n@false%d:", lab_num);
        	gen_mov($7, regx);

        	code("\n@exit%d:", lab_num);

        	$$ = regx;
	}
  | _ID
      {
        $$ = lookup_symbol($1, VAR|PAR|GLOVAR);
        if($$ == NO_INDEX)
          err("'%s' undeclared", $1);
      }
  | function_call
	 {
        $$ = take_reg();
        gen_mov(FUN_REG, $$);
      }
  |  _ID _INC
	{ if(lookup_symbol($1, FUN) != NO_INDEX)
		{
		err("increment won't work on a function");
		 $$ = lookup_symbol($1, FUN);
		}
	else if(lookup_symbol($1, VAR|PAR|GLOVAR) == NO_INDEX)
		{
		err("Not found");
		}
	else{
		int simx=lookup_symbol($1, VAR|PAR|GLOVAR);
		int pom_reg=take_reg();
		gen_mov(simx, pom_reg);
		if( get_type(simx)== INT){
			 code("\n\t\tADDS\t");
			gen_sym_name(simx);
          		code(",");
	  		code("$1");
          		code(",");
          		gen_sym_name(simx);
		}
		if( get_type(simx)== UINT){
			code("\n\t\tADDU\t");
			gen_sym_name(simx);
         		code(",");
	  		code("$1");
          		code(",");
          		gen_sym_name(simx);
		}
	  $$ = pom_reg;
	}
	}
  | _LPAREN num_exp _RPAREN
      { $$ = $2; }
  ;

conditional_exp
 : literal
 | _ID {
	if( ($$ = lookup_symbol($1, (VAR|PAR|GLOVAR))) == NO_INDEX )
          err("'%s' undeclared", $1);
	}
 ;
literal
  : _INT_NUMBER
      { $$ = insert_literal($1, INT); 
	iterate_provera=lookup_symbol($1, LIT);}

  | _UINT_NUMBER
      { $$ = insert_literal($1, UINT);
	iterate_provera=lookup_symbol($1, LIT); }
  ;

function_call
  : _ID 
      {
        fcall_idx = lookup_symbol($1, FUN);
        if(fcall_idx == NO_INDEX)
          err("'%s' is not a function", $1);
      }
    _LPAREN argument_list _RPAREN
      {
        if(get_atr1(fcall_idx) != arg_num){
          err("wrong number of args to function '%s'", 
              get_name(fcall_idx));
	}
	int arg_num1=arg_num;
	if(arg_num!=0){
		while(arg_num--){
			code("\n\t\tPUSH\t");
      			gen_sym_name(pamtiArgumente[arg_num]);
			}
		}
	 code("\n\t\tCALL\t%s", get_name(fcall_idx));
	if(arg_num1!=0){
	 code("\n\t\tADDS\t%%15,$%d,%%15", 4 * arg_num1);
	}
	arg_num=0;
        set_type(FUN_REG, get_type(fcall_idx));
        $$ = FUN_REG;
      }
  ;

argument_list
  : /* empty */
  | arguments
   ;
arguments
: num_exp
    { 
     if(get_type($1)!=par[fcall_idx].tipovi[arg_num+1])
	  err("incompatible type for argument in '%s'",get_name(fcall_idx));
	pamtiArgumente[arg_num]=$1;
	arg_num++;
	free_if_reg($1);
    }
 | arguments _COMMA num_exp {
	if(get_type($3)!=par[fcall_idx].tipovi[arg_num+1])
	  err("incompatible type for argument in '%s'",get_name(fcall_idx));
	pamtiArgumente[arg_num]=$3;
	arg_num++;
	free_if_reg($3);
	}
  ;

if_statement
  : if_part %prec ONLY_IF
      { code("\n@exit%d:", $1); }

  | if_part _ELSE statement
      { code("\n@exit%d:", $1); }
  ;

if_part
  : _IF _LPAREN
      {
        $<i>$ = ++lab_num;
        code("\n@if%d:", lab_num);
      }
    rel_exp
      {
        code("\n\t\t%s\t@false%d", opp_jumps[$4], $<i>3);
        code("\n@true%d:", $<i>3);
      }
    _RPAREN statement
      {
        code("\n\t\tJMP \t@exit%d", $<i>3);
        code("\n@false%d:", $<i>3);
        $$ = $<i>3;
      }
  ;

rel_exp
  : num_exp _RELOP num_exp
      {
        if(get_type($1) != get_type($3))
          err("invalid operands: relational operator");
	 $$ = $2 + ((get_type($1) - 1) * RELOP_NUMBER);
        gen_cmp($1, $3);
      }
  ;

return_statement
  : _RETURN num_exp _SEMICOLON
      {
	ret_num++;
	if(get_type(fun_idx) == VOID) {err("Return type does not match with void");}
 	else if(get_type(fun_idx) != get_type($2))
          err("incompatible types in return");
	 gen_mov($2, FUN_REG);
        code("\n\t\tJMP \t@%s_exit", get_name(fun_idx));     
	}
| _RETURN _SEMICOLON
{
   ret_num++;
   if(get_type(fun_idx)!=VOID)
	warning("Function should be void.");   
	}
  ;
inc_statement
   : _ID _INC _SEMICOLON
	{ if(lookup_symbol($1, FUN) != NO_INDEX)
		{
		err("increment won't work on a function");
		}
	else if(lookup_symbol($1, VAR|PAR|GLOVAR) == NO_INDEX)
		{
		err("Not found");
		} else{
	int simx=lookup_symbol($1, VAR|PAR|GLOVAR);
	if(get_type(simx)==1){
			code("\n\t\tADDS\t");
			gen_sym_name(simx);
        		code(",$1,");
        		gen_sym_name(simx);
			}
	else{ code("\n\t\tADDU\t");
			gen_sym_name(simx);
        		code(",$1,");
        		gen_sym_name(simx);
		}
	}
	}
   ;
iterate_statement
  : _ITERATE _ID 
	{
	int pomocna=lookup_symbol($2, VAR|PAR|GLOVAR);
	if(pomocna == NO_INDEX)
	err("Variable is not declared.");
	else if(get_kind(pomocna)==VAR) {
		inicijalizovano[get_atr1(pomocna)]=1;
		}
	iterate_lab_num= ++lab_num;
	$<i>$=iterate_lab_num;
	}
  _STEP literal
	{
	iterate_provera2=iterate_provera;
	if(get_type(lookup_symbol($2, VAR|PAR|GLOVAR))!=get_type(iterate_provera)){
	 err("Types for counter and literal not the same in iterate statement.");}
	
	int itx=lookup_symbol($2, VAR|PAR|GLOVAR);
	code("\n\t\tMOV \t");
  	code("$1");
  	code(",");
  	gen_sym_name(itx);
	code("\n@iterate%d:", iterate_lab_num);
	}
 _TO literal 
{
	if(get_type(iterate_provera)!=get_type(iterate_provera2)){
	 err("Types for literals are not the same in iterate statement.");}
	gen_cmp(lookup_symbol($2, VAR|PAR|GLOVAR),$8);
	if(get_type(lookup_symbol($2, VAR|PAR|GLOVAR))==INT){
	code("\n\t\tJGTS\t@iterate_exit%d",iterate_lab_num);
	}else{
	code("\n\t\tJGTU\t@iterate_exit%d",iterate_lab_num);
	}
	}
  statement
	{
	int itx=lookup_symbol($2, VAR|PAR|GLOVAR);
	if(get_type(itx) == INT)
    			code("\n\t\tADDS \t");
		else
    			code("\n\t\tADDU \t");
		gen_sym_name(itx);
 		code(",");
		gen_sym_name($5);
		code(",");
  		gen_sym_name(itx);
	code("\n\t\tJMP\t@iterate%d",$<i>3);
	code("\n@iterate_exit%d:", $<i>3);
	}
  ;
check_statement
  
  : _CHECK _LPAREN _ID 
       {
	if(lookup_symbol($3,GLOVAR|VAR)==NO_INDEX){
		err("Variable in check is not declared.");
	}
	check_provera=get_type(lookup_symbol($3,GLOVAR|VAR));
	

	check_lab_num= ++lab_num;	
	code("\n@check%d:", lab_num);
	int indeks=lookup_symbol($3,VAR|GLOVAR);			
	reg = take_reg();
	gen_mov(indeks,reg);
	}
  _RPAREN _LBRACKET dodatak_statement default_statement _RBRACKET
	{
	code("\n@exit%d:", check_lab_num);
		free_if_reg(reg);	
	}
  ;

dodatak_statement
 : _CASE literal 
	{
	tip_konstante[broj_konstanti]=get_type($2);
	indeksi_konstanti[broj_konstanti]=$2;
	if(check_provera!=get_type($2)){
			err("Type of constant in check is not right");
			break;
			}
	for(int x=0;x<broj_konstanti;x++){
		if($2==indeksi_konstanti[x]){
			err("Constants in cases in check must be unique");
			break;
		}
		}
	broj_konstanti++;
	}
  _IMPLIES
	{
	code("\n@case%d:",atoi(get_name($2)));
	if(case_flag==1){
	if(get_type($2) == INT)
    			code("\n\t\tCMPS \t");
  		else
    			code("\n\t\tCMPU \t");
  		gen_sym_name($2);
  		code(",");
  		gen_sym_name(reg);
		code("\n\t\tJEQ \t@case_exit%d",atoi(get_name($2)));
		code("\n\t\tJNE \t@case_exit_really%d", atoi(get_name($2)));
	}
	code("\n@case_exit%d:", atoi(get_name($2)));
	} statement finish_statement
	{
	code("\n@case_exit_really%d:",atoi(get_name($2)));
	}
 | dodatak_statement _CASE literal 
	{
	tip_konstante[broj_konstanti]=get_type($3);
	indeksi_konstanti[broj_konstanti]=$3;
	if(check_provera!=get_type($3)){
			err("Type of constant in check is not right");
			break;
			}
	for(int x=0;x<broj_konstanti;x++){
		if($3==indeksi_konstanti[x]){
			err("Constants in cases in check must be unique");
			break;
		}
		}
	broj_konstanti++;
	}
 _IMPLIES 
	{
	code("\n@case%d:",atoi(get_name($3)));

	if(case_flag == 1)
			{
	if(get_type($3) == INT)
    			code("\n\t\tCMPS \t");
  		else
    			code("\n\t\tCMPU \t");
  		gen_sym_name($3);
  		code(",");
  		gen_sym_name(reg);
		code("\n\t\tJEQ \t@case_exit%d",atoi(get_name($3)));
		code("\n\t\tJNE \t@case_exit_really%d", atoi(get_name($3)));
	}
	code("\n@case_exit%d:", atoi(get_name($3)));
	}
statement finish_statement
	{
	code("\n@case_exit_really%d:",atoi(get_name($3)));
	}
 ;

finish_statement
 : _FINISH _SEMICOLON
	{
	case_flag=1;
	code("\n\t\tJMP \t@exit%d", check_lab_num);
	}
 | {case_flag = 0;}
 ;

default_statement
 : _DEFAULT _IMPLIES 
	{
	code("\n@default%d:",check_lab_num);
	}
  statement
 |
 ;

%%

int yyerror(char *s) {
  fprintf(stderr, "\nline %d: ERROR: %s", yylineno, s);
  error_count++;
  return 0;
}

void warning(char *s) {
  fprintf(stderr, "\nline %d: WARNING: %s", yylineno, s);
  warning_count++;
}

int main() {
  int synerr;
  init_symtab();
  output = fopen("output.asm", "w+");

  synerr = yyparse();

  clear_symtab();
  fclose(output);
  
  if(warning_count)
    printf("\n%d warning(s).\n", warning_count);

  if(error_count) {
    remove("output.asm");
    printf("\n%d error(s).\n", error_count);
  }

  if(synerr)
    return -1;  //syntax error
  else if(error_count)
    return error_count & 127; //semantic errors
  else if(warning_count)
    return (warning_count & 127) + 127; //warnings
  else
    return 0; //OK
}

