import ply.lex as lex
import ply.yacc as yacc
from sys import *

tokens = [

    'INT','FLOAT','STRING','NAME',
    'PLUS','MINUS','DIVIDE','MULTIPLY','MODULO','EQUALS','BOOL',
    'LD', 'LDEQ', 'GD', 'GDEQ', 'NOTEQ', 'EQUAL', 'EQUALEQUAL',
    'AND', 'OR', 'NOT',
    'LPAREN', 'RPAREN',
    'IF', 'ELSE', 'WHILE',
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
    'WHILE'     : 'WHILE',

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

def p_start(p):         #start of grammar
    '''
    start   : branch
            | empty
    '''
    p[0] = p[1]
    #run(p[0])           #comment out first

def p_branch(p):        #grammar for multiple lines
    '''
    branch  : branch choice
            | choice

    '''
    if len(p) == 3:
        p[0] = ('MULTILINES', p[1], p[2])
    else:
        p[0] = p[1]

def p_choice(p):        #grammar for each line of code
    '''
    choice : var_assign
           | expression
           | print_stmt
           | scan_stmt
           | while_stmt
           | for_stmt
           | if_stmt
    '''
    p[0] = p[1]


#-------------------------------------------------#

def p_calc(p):
    '''
    calc    : expression
            | var_assign
            | empty

    '''

    res = run(p[1])
    if res != None:
        print(res)
    # print(run(p[1]))

def p_var_assign(p):
    '''

    var_assign    :   NAME EQUALS expression
    '''

    p[0] = ('=', p[1], p[3])

def p_expression(p):
    '''
    expression  :   expression MULTIPLY expression
                |   expression DIVIDE expression
                |   expression PLUS expression
                |   expression MINUS expression
                |   expression MODULO expression
    '''

    p[0] = (p[2], p[1], p[3])

def p_expression_int_float(p):
    '''
    expression  :   INT
                |   FLOAT
    '''
    p[0] = p[1]

def p_expression_bool(p):
    '''
    expression  :   BOOL
    '''
    p[0] = ('bool', p[1])

def p_expression_var(p):
    '''
    expression  :   NAME
    '''
    p[0] = ('var', p[1])


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

        ### boolean?? not sure where to add yet ###
        if p[0] == 'bool':
            print("testing bool stuff")
            return

        ### Check if Variables Exists ###
        if p[0] == 'var':
            if p[1] not in env:
                print('Undeclared variable found! - ', p[1])               
                return
            else:
                return env[p[1]]

        ### Assign Value to Variable ###
        if p[0] == '=':
            env[p[1]] = run(p[2])
            return


        a = run(p[1])
        b = run(p[2])

        if type(a) != int or type(b) != int:
            return

        ### Evaluate Expressions ###
        if p[0] == '+':
            return a + b
        if p[0] == '-':
            return a - b
        if p[0] == '*':
            return a * b
        if p[0] == '/':
            return a / b
        if p[0] == '%':
            return a % b
    else:
        return p




while True:
    try:
        s = input(">>> ")
        if(s == "UMU"):              # added exit code via cmd
            break
    except EOFError:
        print()
        break
    parser.parse(s)
