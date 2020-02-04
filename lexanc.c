/* lex1.c         14 Feb 01; 31 May 12; 11 Jan 18       */

/* This file contains code stubs for the lexical analyzer.
   Rename this file to be lexanc.c and fill in the stubs.    */

/* Copyright (c) 2018 Gordon S. Novak Jr. and
   The University of Texas at Austin. */

/*
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <limits.h>
#include <float.h>
#include "token.h"
#include "lexan.h"


/* This file will work as given with an input file consisting only
   of integers separated by blanks:
   make lex1
   lex1
   12345 123    345  357
   */

const char* operators[] = { '+', '-', '*', '/', ':=', '=', '<>', '<',
  '<=', '>=', '>', '^', '.', 'and', 'or', 'not',
  'div', 'mod', 'in' };

const char* delimiters[] = { ',', ';', ':', '(', ')', '[', ']', '..'};

const char* reservedWords[] = { 'array', 'begin', 'case', 'const', 'do',
  'downto', 'else', 'end', 'file', 'for', 
  'function', 'goto', 'if', 'label', 'nil', 
  'of', 'packed', 'procedure', 'program', 'record',
  'repeat', 'set', 'then', 'to', 'type', 'until',
  'var', 'while', 'with'};

/* Skip blanks and whitespace.  Expand this function to skip comments too. */
void skipblanks ()
  {
    int c;
    int d;
    while (c = peekchar() != EOF) {
      if ((c == ' ' || c == '\n' || c == '\t')) {
        getchar();
      } 
      else if (c =='{') {
        getchar();
        c = peekchar();
        while ((c != EOF) && (c == '}')) {
          getchar();
          c = peekchar();
        }
        getchar();
      }
      else if ((c == '(') && (d = peek2char() == '*')) {
        getchar(); 
        getchar();
        c = peekchar();
        d = peek2char();
        while (c != EOF && d!= EOF && !(c=='*' && d==')')) {
          getchar();
          getchar();
          c = peekchar();
          d = peek2char();
        } 
        getchar(); 
        getchar();
      }
    }
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
  strcpy(tok->stringval, identifier);
  return tok;
}

TOKEN getstring (TOKEN tok)
  {
    }

TOKEN special (TOKEN tok)
  {
    }

/* Get and convert unsigned numbers of all types. */
TOKEN number (TOKEN tok)
  { long num;
    int  c, d, intVal;
    int floatNo = 0, exponentNo = 0, negativeNo = 0, exponent = 0, exponentVal;
    int intError;
    int multiplier = 1;
    double decimal, real = 0.0;
    while ( (c = peekchar()) != EOF
            && CHARCLASS[c] == NUMERIC)
      {   
        intVal = (getchar() - '0');
          if ( num > INT_MAX ) {
			exponent ++;
			intError = 1;
		} else {
			num = num * 10 + intVal;
		}
        }

    if ( num > INT_MAX ) {
		exponent ++;
		intError = 1;
	} 

	if (c = peekchar() != EOF &&  c == ".") {
		if (d = peek2char() != EOF && CHARCLASS[d] == NUMERIC) {
			c = getchar();
			intError = 0; //floating point number has higher max
			floatNo = 1;
			while ((c = peekchar()) != EOF && (CHARCLASS[c] == NUMERIC)) {
				intVal = getchar() - '0';
				multiplier *= 10;
				decimal = decimal + ((double)intVal/multiplier);
			}
			real = (double) num + decimal; 

		}
	}

	if (c = peekchar() != EOF &&  c == "e"){
		c = getchar();
		exponentNo = 1;
		int sign = 1;
        sign *= (c = peekchar()) == '-' ? -1 : 1;
		while ((c = peekchar()) != EOF && CHARCLASS[c] == NUMERIC &&  exponentVal > INT_MAX) {
			intVal = getchar() - '0';
			exponentVal = exponentVal * 10 + intVal;		
		}
		exponent = exponent + sign*exponentVal;
		real = real / pow (10, exponent);
		
		if (real > FLT_MAX || real < FLT_MIN) {
			printf("Floating number too big or small \n");
			// return getRealTok(0.0, tok);
		} else {
			tok->tokentype = NUMBERTOK;
			tok->basicst = REAL;
			tok->realval = real;
			return tok;
		}

	}

	if (floatNo) {
		if (real > FLT_MAX || real < FLT_MIN) {
			printf("Floating number too big or small \n");
			// return getRealTok(0.0, tok);
		} else {
			tok->tokentype = NUMBERTOK;
			tok->basicdt = REAL;
			tok->realval = real;
			return tok;
		}
	}

	if (intError) {
		printf("Integer is too big\n");
	} else 
{	    tok->tokentype = NUMBERTOK; 
	    tok->basicdt = INTEGER;
	    tok->intval = num;
	    return (tok);
	}
  }
