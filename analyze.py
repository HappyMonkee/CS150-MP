import ply.lex as lex
import ply.yacc as yacc
from sys import *

tokens = [

    'INT','FLOAT','STRING','NAME',
    'PLUS','MINUS','DIVIDE','MULTIPLY','MODULO','EQUALS','BOOL',
    'LD', 'LDEQ', 'GD', 'GDEQ', 'NOTEQ', 'EQUAL', 'EQUALEQUAL',
    'AND', 'OR', 'NOT',
    'LPAREN', 'RPAREN',
    'IF', 'ELSE', 'THEN','WHILE', 'STOP', 
    'PRINT', 'READ'      
]

reserved = {
    # 'PLUS'      : 'PLUS',
    # 'MINUS'     : 'MINUS',
    # 'DIVIDE'    : 'DIVIDE',
    # 'MULTIPLY'  : 'MULTIPLY',
    # 'MODULO'    : 'MODULO',

    'LD'        : 'LD',
    'LDEQ'      : 'LDEQ',
    'GD'        : 'GD',
    'GDEQ'      : 'GDEQ',
    'NOTEQ'     : 'NOTEQ',
    'EQUAL'     : 'EQUAL',
    'EQUALEQUAL': 'EQUALEQUAL',

    'AND'       : 'AND',
    'OR'        : 'OR',
    'NOT'       : 'NOT',

    'IF'        : 'IF',
    'ELSE'      : 'ELSE',
    'THEN'      : 'THEN',
    'WHILE'     : 'WHILE',
    'STOP'      : 'STOP',

    'PRINT'     : 'PRINT',
    'READ'      : 'READ'
}


t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_MODULO = r'\%'
t_EQUALS = r'\='
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
    return t

def t_BOOL(t):
    r'true|false'
    t.type = "BOOL"
    return t

def t_STRING(t):
    r'\"([^\\"]|(\\.))*\"'
    t.value = t.value[1:-1]
    return t

def t_NAME(t):
    r'[a-zA-Z_][a-zA-z_0-9]*'
    t.type = 'NAME'
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
    ('left', 'MULTIPLY', 'DIVIDE', 'MODULO')

)

#start of grammar

def p_start(p):
    '''
    start : language
          | empty
    '''
    p[0] = p[1]
    run(p[0])

#grammar for multiple lines

def p_language(p):
    '''
    language : language line
             | line
    '''
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
    p[0] = p[1]

#grammar for reading

def p_input(p):
    '''
    input : READ LPAREN NAME RPAREN
    '''
    p[0] = ('READ',p[3])

#grammar for printing

def p_output(p):
    '''
    output : output_print
    '''
    p[0] = p[1]

#printing without newline

def p_output_print(p):
    '''
    output_print : PRINT LPAREN expression RPAREN
    '''
    p[0] = ('PRINT', p[3])

#grammar for assigning values

def p_expression_var_assign(p):
    '''
    expression : NAME
    '''
    p[0] = ('var', p[1])

#grammar for equating values

def p_var_assign(p):
    '''
    var_assign : NAME EQUAL expression
    '''
    p[0] = ('EQUAL', p[1], p[3])

#grammar for deciding operators

def p_expression(p):
    '''
    expression : expression_operation
    '''
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
                         | expression AND expression
                         | expression OR expression
    '''
    p[0] = (p[2],p[1],p[3])

#grammar for data types

def p_expression_data_type(p):
    '''
    expression : INT
               | FLOAT
               | STRING
    '''
    p[0] = p[1]

#grammar for parenthesis

def p_expression_parenthesis(p):
    '''
    expression : LPAREN expression RPAREN
    '''
    p[0] = p[2]

#grammar for deciding between if or if else

def p_conditional(p):
    '''
    conditional : conditional_if
                | conditional_if_else
    '''
    p[0] = p[1]

#grammar for if else statement

def p_conditional_if_else(p):
    '''
    conditional_if_else : IF expression THEN language STOP ELSE THEN language STOP
    '''
    p[0] = ('IFELSE',p[2],p[4],p[8])

#grammar for if statement

def p_conditional_if(p):
    '''
    conditional_if : IF expression THEN language STOP
    '''
    p[0] = ('IF',p[2],p[4])

#grammar for while

def p_iterative(p):
    '''
    iterative : WHILE expression THEN language STOP
    '''
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
        if p[0] == 'PLUS':
            return run(p[1]) + run(p[2])
        elif p[0] == 'MINUS':
            return run(p[1]) - run(p[2])
        elif p[0] == 'MULTIPLY':
            return run(p[1]) * run(p[2])
        elif p[0] == 'DIVIDE':
            return run(p[1]) / run(p[2])
        elif p[0] == 'EQUAL':
            env[p[1]] = run(p[2])
        elif p[0] == 'var':
            if p[1] not in env:
                 print('Undeclared variable found! - ', p[1])               
                 return
            else:
                 return env[p[1]]
            return env[p[1]]
        elif p[0] == 'MODULO':
            return run(p[1]) % run(p[2])
        elif p[0] == 'LD':
            return run(p[1]) < run(p[2])
        elif p[0] == 'LDEQ':
            return run(p[1]) <= run(p[2])
        elif p[0] == 'GD':
            return run(p[1]) > run(p[2])
        elif p[0] == 'GDEQ':
            return run(p[1]) >= run(p[2])
        elif p[0] == 'EQUALEQUAL':
            return run(p[1]) == run(p[2])
        elif p[0] == 'AND':
            return run(p[1]) and run(p[2])
        elif p[0] == 'OR':
            return run(p[1]) or run(p[2])
        elif p[0] == 'NOT':
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
