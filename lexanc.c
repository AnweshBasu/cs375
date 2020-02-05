/* lex1.c         14 Feb 01; 31 May 12       */

/*
Name: S. Ram Janarthana Raja	
UT EID: rs53992
*/


/* This file contains code stubs for the lexical analyzer.
   Rename this file to be lexanc.c and fill in the stubs.    */

/* Copyright (c) 2001 Gordon S. Novak Jr. and
   The University of Texas at Austin. */

/*
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
utput);
   GNU General Public License for more details.
a	if ( num > INT_MAX ) {
			exponent ++;
			intError = 1;
		} if ( num > INT_MAX ) {
			exponent ++;
			intError = 1;
		} 
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include <float.h>
#include "token.h"
#include "lexan.h"

/* This file will work as given with an input file consisting only
   of integers separated by blanks:
   make lex1
   lex1
   12345 123    345  357
 */

char* operators[] = { "+", "-", "*", "/", ":=", "=", "<>", "<",
	"<=", ">=", ">", "^", ".", "and", "or", "not",
	"div", "mod", "in" };

char* delimiters[] = { ",", ";", ":", "(", ")", "[", "]", ".."};

char* reservedWords[] = { "array", "begin", "case", "const", "do",
	"downto", "else", "end", "file", "for", 
	"function", "goto", "if", "label", "nil", 
	"of", "packed", "procedure", "program", "record",
	"repeat", "set", "then", "to", "type", "until",
	"var", "while", "with"};



/* Skip blanks and whitespace.  Expand this function to skip comments too. */
void skipblanks ()
{
	int c;
	int d;
	while ((c = peekchar()) != EOF ){
		if (c == ' ' || c == '\n' || c == '\t') 
		{
			getchar();
		} 
		else if (c == '{')
		{
			getchar();
			c = peekchar();
			while (c != EOF && (c != '}')) 
			{
				getchar();
				c = peekchar();
			}
			getchar();
		} 
		else if (c == '(' && (d = peek2char()) != EOF && d == '*')
		{
			getchar();
			getchar(); 
			c = peekchar();
        		d = peek2char();
			while (c != EOF && d != EOF && !(c == '*' && d== ')')) 
			{
				getchar();
				c = peekchar();
        			d = peek2char();
			}
			getchar();
			getchar();  
		} 
		else 
		{
			break;
		}
	}
}

TOKEN getReservedWordTok(int val, TOKEN tok) {
	tok->tokentype = RESERVED;
	tok->whichval = val;
	return tok;
}

TOKEN getIdentifierTok(char word[], TOKEN tok) {
	tok->tokentype = IDENTIFIERTOK;
	strcpy(tok->stringval, word);
	return tok;
}

TOKEN getDelimiterTok(int val, TOKEN tok) {
	tok->tokentype = DELIMITER;
	tok->whichval = val;
	return tok;
}

TOKEN getOperatorTok(int val, TOKEN tok) {
	tok->tokentype = OPERATOR;
	tok->whichval = val;
	return tok;
}

TOKEN getStringTok(char word[], TOKEN tok) {
	tok->tokentype = STRINGTOK;
	strcpy(tok->stringval, word);
	return tok;
} 

TOKEN getIntegerTok(int val, TOKEN tok) {
	tok->tokentype = NUMBERTOK;
	tok->basicdt = INTEGER;
	tok->intval = val;
	return tok;
}

TOKEN getRealTok(double val, TOKEN tok) {
	tok->tokentype = NUMBERTOK;
	tok->basicdt = REAL;
	tok->realval = val;
	return tok;
}
/* Get identifiers and reserved words */
TOKEN identifier (TOKEN tok)
{
  int  c, count, size = 0;    
  char variable[15];
  while ( (c = peekchar()) != EOF
          && (size < 16) &&(CHARCLASS[c] == ALPHA || CHARCLASS[c] == NUMERIC)) 
  {
    variable[size] = getchar();
    size += 1;
  }
  variable[size] = '\0';
  // check if it is a word operator
  for (count = 13; count <= 18 ; count++)
  {
    if (strcmp(variable, operators[count]) == 0)
    {
      tok->tokentype = OPERATOR;
      tok->whichval = count+1;
      return tok;
    }
  }

  //check if it is a reserved Word
  for (count = 0;  count <= 28; count++)
  {
    if (strcmp(variable, reservedWords[count]) == 0)
    {
      tok->tokentype = RESERVED;
      tok->whichval = count+1;
      return tok;
    }
  }
  //it is an identifier
  tok->tokentype = IDENTIFIERTOK;
  strcpy(tok->stringval, variable);
  return tok;


}

TOKEN getstring (TOKEN tok)
{
	getchar();
	int c, d, e, size = 0;
	char string[256];
	while ((c = peekchar()) != EOF) {
		e = peek2char();
		d = getchar();
		if( d == '\'') {
			if(e != EOF && e != '\''){
				break;
			} else {
				getchar();
			}
		}
		string[size] = c;
		size ++;
	}
	if (size > 15) {
		size = 15;
	}
	string[size] = '\0';
	
	tok->tokentype = STRINGTOK;
	strcpy(tok->stringval, string);
	return tok; 

}


TOKEN special (TOKEN tok)
{
	int c, d, size = 0, flag = 0, val = 0, i;
	char oper[3];

	while ( (c = peekchar()) != EOF
			&& CHARCLASS[c] == SPECIAL) {
		c = getchar();	
		oper[size] = c;
		size ++;

		d = peekchar();
		oper[size] = d;	
		size ++;
		
		oper[size] = '\0';

		for(i = 0; i < 19; i ++){
			if(strcmp(oper, operators[i]) == 0) {
				flag = 1;
				break;
			}
		}
		if (flag == 1) {
			getchar();
			return getOperatorTok(i + 1, tok);
		} 
		
		for(i = 0; i < 8; i ++){
			if(strcmp(oper, delimiters[i]) == 0) {
				flag = 1;
				break;
			}
		}
		if (flag == 1) {
			getchar();
			return getDelimiterTok(i + 1, tok);
		}
		
		oper[size - 1] = '\0';

		for(i = 0; i < 19; i ++){
			if(strcmp(oper, operators[i]) == 0) {
				flag = 1;
				break;
			}
		}
		if (flag == 1) {
			return getOperatorTok(i + 1, tok);
		} 
		
		for(i = 0; i < 8; i ++){
			if(strcmp(oper, delimiters[i]) == 0) {
				flag = 1;
				break;
			}
		}
		if (flag == 1) {
			return getDelimiterTok(i + 1, tok);
		}
	}
	
}

TOKEN handleRealError(TOKEN tok){
	printf("Real number out of range \n");
	return getRealTok(0.0, tok);
}

TOKEN returnRealTok(double real, TOKEN tok){
	if (real > FLT_MAX || real < FLT_MIN) {
		return handleRealError(tok);
	} else {
		return getRealTok(real, tok);
	}
}


/* Get and convert unsigned numbers of all types. */
TOKEN number (TOKEN tok)
{ 	
	long num = 0, exponent = 0, expValue = 0;
	double real = 0.0, decimal = 0.0, multiplier = 10.0;
	int  c, d, charval, dFlag = 0, negFlag = 0, eFlag = 0, intError = 0, floatError = 0;

	while ((c = peekchar()) != EOF
			&& (CHARCLASS[c] == NUMERIC))
	{   
		c = getchar();
		charval = c - '0';

		if ( num > INT_MAX ) {
			exponent ++;
			intError = 1;
		} else {
			num = num * 10 + charval;
		}
	
	}

	if ( num > INT_MAX ) {
		exponent ++;
		intError = 1;
	} 

	//The part after the decimal point
	if(c == '.' && (d = peek2char()) != EOF && CHARCLASS[d] == NUMERIC) {
		intError = 0;
		dFlag = 1;
		getchar();
		while ((c = peekchar()) != EOF
				&& (CHARCLASS[c] == NUMERIC)) {
			c = getchar();
			charval = c - '0';
			decimal = decimal + ((double) charval / multiplier);
			multiplier *= 10;
		}	

		real = (double) num + decimal;

	}

	//The exponent part
	if(c == 'e') {
		eFlag = 1;
		getchar();
		c = peekchar();
		if (c == '-') {
			negFlag = 1;
			getchar();
		} else if (c == '+') {
			getchar();
		}

		while ((c = peekchar()) != EOF 
				&& CHARCLASS[c] == NUMERIC) {	
			c = getchar();
			charval = c - '0';

			if ( expValue > INT_MAX ){
				continue ;
			}
			expValue = expValue * 10 + charval;
		}

	}

	if (dFlag) {
		if (eFlag) {
			if (negFlag) {
				exponent = exponent - expValue;
				real = real / pow (10, exponent);
			} else {
				exponent = exponent + expValue;
				real = real * pow (10, exponent);
			}

			return returnRealTok(real, tok);

		} else {

			return returnRealTok(real, tok);

		}

	}
	
	if (eFlag)  {
		real = (double) num;
		if (negFlag) {
			exponent = exponent - expValue;
			real = real / pow(10, exponent);
		} else {
			exponent = exponent + expValue;
			real = real * pow(10, exponent);
		}
		return returnRealTok(real, tok);		
	}


	if (intError) {
		printf("Integer number out of range \n");
		return getIntegerTok(0, tok);
	} else {
		return getIntegerTok(num, tok);
	}
}
