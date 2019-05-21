import ply.lex as lex
import ply.yacc as yacc
from sys import *


reserved = {
    
    'true'      : 'TRUE',
    'false'     : 'FALSE',
    'and'       : 'AND',
    'or'        : 'OR',
    'not'       : 'NOT',

    'if'        : 'IF',
    'elseif'    : 'ELSE_IF',
    'else'      : 'ELSE',
    'then'      : 'THEN',
    'while'     : 'WHILE',
    'stop'      : 'STOP',

    'PRINT'     : 'PRINT',
    'READ'      : 'READ'
}

tokens = [

    'INT','FLOAT','STRING','NAME',
    'PLUS','MINUS','DIVIDE','MULTIPLY','MODULO','EQUALS','TRUE','FALSE',
    'LD', 'LDEQ', 'GD', 'GDEQ', 'NOTEQ', 'EQUALEQUAL',
    'AND', 'OR', 'NOT',
    'LPAREN', 'RPAREN',
    'IF', 'ELSE_IF', 'ELSE', 'THEN','WHILE', 'STOP', 
    'PRINT', 'READ'      
]



t_EQUALEQUAL    =   r'\=\='
t_LDEQ          =   r'\<\='
t_NOTEQ          =   r'\!\='

t_LPAREN        =   r'\('
t_RPAREN        =   r'\)'
t_PLUS          =   r'\+'
t_MINUS         =   r'\-'
t_MULTIPLY      =   r'\*'
t_DIVIDE        =   r'\/'
t_MODULO        =   r'\%'
t_EQUALS        =   r'\='
t_LD            =   r'\<'
t_GD            =   r'\>'

t_ignore = r' '

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_COMMENT(t):
    r'\#.*'
    pass        #no return value, token discarded

def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    # print("test:", t.value)
    return t

def t_STRING(t):
    r'\"([^\\"]|(\\.))*\"'
    t.value = t.value[1:-1]
    return t

def t_NAME(t):
    r'[a-zA-Z_][a-zA-z_0-9]*'
    t.type = reserved.get(t.value,'NAME') # Check if name is a reserved word

    if t.type == 'TRUE':
        t.value = True
    elif t.type == 'FALSE':
        t.value = False

    return t

def t_error(t):
    print("Illegal characters!")
    t.lexer.skip(1)

lexer = lex.lex()

precedence = (

    ('left', 'AND', 'OR'),
    ('nonassoc', 'EQUALEQUAL', 'NOTEQ'),
    ('nonassoc', 'LD', 'LDEQ', 'GD', 'GDEQ'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULTIPLY', 'DIVIDE', 'MODULO'),
    ('right', 'NOT')

)

#start of grammar

def p_start(p):
    '''
    start : language
          | empty
    '''
    print("START")
    p[0] = p[1]
    res = run(p[0])
    if res != None:
        print(res)


#grammar for multiple lines

def p_language(p):
    '''
    language : language line
             | line
    '''
    print("LANGUAGE")
    if len(p) == 3:
        p[0] = ('MULTILINES', p[1], p[2])
    else:
        p[0] = p[1]

#grammar for each line of code

def p_line(p):
    '''
    line : expression
         | var_assign
         | iterative
         | conditional
         | output
         | input
    '''
    print("LINE")
    p[0] = p[1]

#grammar for reading

def p_input(p):
    '''
    input : READ LPAREN NAME RPAREN
    '''

    print("INPUT")
    p[0] = ('READ',p[3])

#grammar for printing

def p_output(p):
    '''
    output : output_print
    '''

    print("OUTPUT")
    p[0] = p[1]

#printing without newline

def p_output_print(p):
    '''
    output_print : PRINT LPAREN expression RPAREN
    '''

    print("OUTPUT PRINT")
    p[0] = ('PRINT', p[3])

#grammar for assigning values

def p_expression_var_assign(p):
    '''
    expression : NAME
    '''
    print("EXPRESSION VAR ASSIGN")
    p[0] = ('var', p[1])

#grammar for equating values

def p_var_assign(p):
    '''
    var_assign : NAME EQUALS expression
    '''
    print("VAR ASSIGN")
    p[0] = ('=', p[1], p[3])

#grammar for deciding operators

def p_expression(p):
    '''
    expression : expression_operation
    '''
    print("EXPRESSION")
    p[0] = p[1]

#grammar for infix operators

def p_expression_operation(p):
    '''
    expression_operation : expression MULTIPLY expression
                         | expression DIVIDE expression
                         | expression MODULO expression
                         | expression PLUS expression
                         | expression MINUS expression
                         | expression LD expression
                         | expression LDEQ expression
                         | expression GD expression
                         | expression GDEQ expression
                         | expression EQUALEQUAL expression
                         | expression NOTEQ expression
                         | expression AND expression
                         | expression OR expression
    '''
    print("EXPRESSION OPERATION")
    p[0] = (p[2],p[1],p[3])

def p_expression_operation_not(p):
    '''
    expression_operation : NOT expression
    '''
    print("EXPRESSION OPERATION NOT")
    p[0] = (p[1], p[2])
#grammar for data types

def p_expression_data_type(p):
    '''
    expression : INT
               | FLOAT
               | STRING
               | TRUE
               | FALSE
    '''
    print("EXPRESSION TO DATATYPE")
    p[0] = p[1]

#grammar for parenthesis

def p_expression_parenthesis(p):
    '''
    expression : LPAREN expression RPAREN
    '''
    print("EXPRESSION PARENTHESIS")
    p[0] = p[2]

#grammar for if statement

def p_conditional(p):
    '''
    conditional : IF expression THEN language else_if_blocks else_block STOP
    '''
    print("CONDITONAL")
    p[0] = ('IF', p[2], p[4], p[5], p[6])
        
#grammar for deciding if 0 or more else if statements

def p_else_if_blocks(p):
    '''
    else_if_blocks : empty
                   | else_if_blocks else_if_block
    '''
    if len(p) == 2:
        p[0] = 'NO ELSE IF'
    else:
        p[0] = ('MULTIELSEIF', p[1], p[2])

#grammar for else if statement

def p_else_if_block(p):
    '''
    else_if_block : ELSE_IF expression THEN language
    '''
    print("CONDITOINAL ELSE IF")
    p[0] = ('ELSEIF',p[2],p[4])

#grammar for else statement

def p_else_block(p):
    '''
    else_block : ELSE language
               | empty
    '''
    if len(p) == 2:
        p[0] = 'NO ELSE'
    else:
        print("ELSE")
        p[0] = ('ELSE',p[2])

#grammar for while

def p_iterative(p):
    '''
    iterative : WHILE expression THEN language STOP
    '''
    print("ITERATIVE")
    p[0] = ('WHILE', p[2],p[4])

#-------------------------------------------------#

# def p_calc(p):
#     '''
#     calc    : expression
#             | var_assign
#             | empty

#     '''

#     res = run(p[1])
#     if res != None:
#         print(res)
#     # print(run(p[1]))

# def p_var_assign(p):
#     '''

#     var_assign    :   NAME EQUALS expression
#     '''

#     p[0] = ('=', p[1], p[3])

# def p_expression(p):
#     '''
#     expression  :   expression MULTIPLY expression
#                 |   expression DIVIDE expression
#                 |   expression PLUS expression
#                 |   expression MINUS expression
#                 |   expression MODULO expression
#     '''

#     p[0] = (p[2], p[1], p[3])

# def p_expression_int_float(p):
#     '''
#     expression  :   INT
#                 |   FLOAT
#     '''
#     p[0] = p[1]

# def p_expression_bool(p):
#     '''
#     expression  :   BOOL
#     '''
#     p[0] = ('bool', p[1])

# def p_expression_var(p):
#     '''
#     expression  :   NAME
#     '''
#     p[0] = ('var', p[1])


def p_error(p):
    print("OFFNDING LINE:",p)
    print("Syntax Error, oh no")

def p_empty(p):
    '''
    empty : 

    '''
    p[0] = None

parser = yacc.yacc()
env = {}

def run(p):
    global env
    
    if type(p) == tuple:
        if p[0] == '+':
            return run(p[1]) + run(p[2])

        elif p[0] == '-':
            return run(p[1]) - run(p[2])

        elif p[0] == '*':
            return run(p[1]) * run(p[2])

        elif p[0] == '/':
            return run(p[1]) / run(p[2])

        elif p[0] == '=':
            env[p[1]] = run(p[2])
            return env[p[1]]

        elif p[0] == 'var':
            if p[1] not in env:
                 print('Undeclared variable found! - ', p[1])               
                 return
            else:
                 return env[p[1]]
            return env[p[1]]

        elif p[0] == '%':
            return run(p[1]) % run(p[2])

        elif p[0] == '<':
            return run(p[1]) < run(p[2])

        elif p[0] == '<=':
            return run(p[1]) <= run(p[2])

        elif p[0] == '>':
            return run(p[1]) > run(p[2])

        elif p[0] == '>=':
            return run(p[1]) >= run(p[2])

        elif p[0] == '==':
            return run(p[1]) == run(p[2])

        elif p[0] == '!=':
            return run(p[1]) != run(p[2])

        elif p[0] == 'and':
            return run(p[1]) and run(p[2])

        elif p[0] == 'or':
            return run(p[1]) or run(p[2])

        elif p[0] == 'not':
            return not run(p[1])

        elif p[0] == 'MULTILINES':
            run(p[1])
            run(p[2])

        elif p[0] == 'IF':
            if run(p[1]) != 0:
                run(p[2])

        elif p[0] == 'IFELSE':
            if run(p[1]) != 0:
                run(p[2])
            else:
                run(p[3])

        elif p[0] == 'WHILE':
            while run(p[1]) != 0:
                run(p[2])

        elif p[0] == 'PRINT':
            print(run(p[1]))

        elif p[0] == 'READ':
            env[p[1]] = input()
    else:
        return p

    #     ### boolean?? not sure where to add yet ###
    #     if p[0] == 'bool':
    #         print("testing bool stuff")
    #         return

    #     ### Check if env Exists ###
    #     if p[0] == 'var':
    #         if p[1] not in env:
    #             print('Undeclared variable found! - ', p[1])               
    #             return
    #         else:
    #             return env[p[1]]

    #     ### Assign Value to Variable ###
    #     if p[0] == '=':
    #         env[p[1]] = run(p[2])
    #         return


    #     a = run(p[1])
    #     b = run(p[2])

    #     if type(a) != int or type(b) != int:
    #         return

    #     ### Evaluate Expressions ###
    #     if p[0] == '+':
    #         return a + b
    #     if p[0] == '-':
    #         return a - b
    #     if p[0] == '*':
    #         return a * b
    #     if p[0] == '/':
    #         return a / b
    #     if p[0] == '%':
    #         return a % b
    # else:
    #     return p


while True:             #we should be able to read from files ye? - hans
    try:
        s = input(">>> ")
        if(s == "UMU"):              # added exit code via cmd
            break
    except EOFError:
        print()
        break
    parser.parse(s)
