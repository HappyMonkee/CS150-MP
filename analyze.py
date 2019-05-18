import ply.lex as lex
import ply.yacc as yacc
import sys

tokens = [

    'INT',
    'FLOAT',
    'NAME',
    'PLUS',
    'MINUS',
    'DIVIDE',
    'MULTIPLY',
    'EQUALS',
]


t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_EQUALS = r'\='
t_ignore = r' '

def t_COMMENT(t):
    r'\#.*'
    pass

def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INT(t):
    r'\d+'
    t.value = int(t.value)
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

    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULTIPLY', 'DIVIDE')

)


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
    '''

    p[0] = (p[2], p[1], p[3])

def p_expression_int_float(p):
    '''
    expression  :   INT
                |   FLOAT
    '''
    p[0] = p[1]


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
    else:
        return p




while True:
    try:
        s = input(">>> ")
    except EOFError:
        print()
        break
    parser.parse(s)
