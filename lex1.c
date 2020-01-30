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
#include 'token.h'
#include 'lexan.h'

/* This file will work as given with an input file consisting only
   of integers separated by blanks:
   make lex1
   lex1
   12345 123    345  357
   */

char* operators[] = { '+', '-', '*', '/', ':=', '=', '<>', '<',
  '<=', '>=', '>', '^', '.', 'and', 'or', 'not',
  'div', 'mod', 'in' };

char* delimiters[] = { ',', ';', ':', '(', ')', '[', ']', '..'};

char* reservedWords[] = { 'array', 'begin', 'case', 'const', 'do',
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
  num = 0;
  while ( (c = peekchar()) != EOF
          && (size < 16) &&(CHARCLASS[c] == AlPHA || CHARCLASS[c] == NUMERIC)) 
  {
    variable[size] = getchar();
    size += 1;
  }
  variable[size] = '\0';
  // check if it is a word operator
  for (count = 13; count <= 18 ; count++)
  {
    if (strcmp(word, operators[i]) == 0)
    {
      tok->tokentype = OPERATOR;
      tok->whichval = i+1;
      return tok;
    }
  }

  //check if it is a reserved Word
  for (count = 0;  count <= 28; count++)
  {
    if (strcmp(word, reservedWords[i]) == 0)
    {
      tok->tokentype = RESERVED;
      tok->whichval = i+1
      return tok;
    }
  }
  //it is an identifier
  tok->tokentype = IDENTIFIERTOK;
  strcpy(tok->stringval, word);
  return tok


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
    int  c, charval;
    num = 0;
    while ( (c = peekchar()) != EOF
            && CHARCLASS[c] == NUMERIC)
      {   c = getchar();
          charval = (c - '0');
          num = num * 10 + charval;
        }
    tok->tokentype = NUMBERTOK;
    tok->basicdt = INTEGER;
    tok->intval = num;
    return (tok);
  }

