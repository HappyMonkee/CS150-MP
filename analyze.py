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

    'int'       : 'INT_TYPE',
    'float'     : 'FLOAT_TYPE',
    'string'    : 'STRING_TYPE',
    'void'      : 'VOID_TYPE',
    'return'    : 'RETURN',
    # 'stop'      : 'STOP',

    'print'     : 'PRINT',
    'read'      : 'READ'
}

tokens = [

    'INT','FLOAT','STRING','NAME',
    'PLUS','MINUS','DIVIDE','MULTIPLY','MODULO','EQUALS','TRUE','FALSE',
    'LD', 'LDEQ', 'GD', 'GDEQ', 'NOTEQ', 'EQUALEQUAL',
    'AND', 'OR', 'NOT',
    'LPAREN', 'RPAREN',
    'LBRACE', 'RBRACE',
    'IF', 'ELSE_IF', 'ELSE', 'WHILE', 
    'START', 'STOP', 
    'PRINT', 'READ',
    'NEWLINE',
    'INT_TYPE','FLOAT_TYPE','STRING_TYPE','VOID_TYPE','COMMA','RETURN'      
]



t_EQUALEQUAL    =   r'\=\='
t_LDEQ          =   r'\<\='
t_NOTEQ         =   r'\!\='

t_COMMA         =   r'\,'
t_START         =   r'\{'
t_STOP          =   r'\}'
t_LPAREN        =   r'\('
t_RPAREN        =   r'\)'
t_LBRACE        =   r'\['
t_RBRACE        =   r'\]'
t_PLUS          =   r'\+'
t_MINUS         =   r'\-'
t_MULTIPLY      =   r'\*'
t_DIVIDE        =   r'\/'
t_MODULO        =   r'\%'
t_EQUALS        =   r'\='
t_LD            =   r'\<'
t_GD            =   r'\>'

t_ignore = r' '

def t_NEWLINE(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    # print(len(t.value))
    return t;

def t_COMMENT(t):
    r'\#.*\n+'
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
    if p[0] != None:
        print()
        print()
        print(p[0])


#grammar for multiple lines

def p_language(p):
    '''
    language : language NEWLINE line
             | line
    '''
    if len(p) == 4:

        if p[3] != None:
            p[0] = ('MULTILINES', p[1], p[3])
            print("LANGUAGE MULTILINES:",p[0])
        else:
            p[0] = ('LANGUAGE', p[1])
            print("LANGUAGE:", p[0])
    # elif len(p) == 3:
    #     p[0] = ('MULTILINES', p[1])
    #     print("LANGUAGE MULTILINES:",p[0])

    else:
        p[0] = p[1]
        print("LANGUAGE:",p[0])

#grammar for each line of code

def p_line(p):
    '''
    line : expression
         | var_assign
         | func_assign
         | iterative
         | conditional
         | output
         | input
         | empty
    '''
    p[0] = p[1]
    print("LINE:",p[0])

#grammar for reading

def p_func_assign(p):
    '''
    func_assign     :   datatype NAME LPAREN parameters RPAREN START NEWLINE language return_stmt STOP
    ''' 
    p[0] = ('FUNCTION', p[1], p[2], p[4], p[8], p[9])
    print("FUNCTION:",p[0])


def p_return_stmt(p):
    '''
    return_stmt     :   RETURN expression NEWLINE
                    |   RETURN NEWLINE
                    |   empty

    '''
    if len(p) == 4:
        p[0] = ('RETURN', p[1], p[2])
    else:
        p[0] = ('RETURN', p[1])
    

    print("RETURN STMT:", p[0])

def p_parameters(p):
    '''

    parameters      :   first_param
                    |   empty
    '''

    p[0] = p[1]
    print("PARAMETERS:",p[0])

def p_first_param(p):
    '''
    first_param     :   datatype NAME COMMA first_param
                    |   datatype NAME

    '''
    
    if len(p) == 5:
        p[0] = (p[1], p[2], p[4])
    else:
        p[0] = (p[1], p[2])
    print("FIRST PARAM:",p[0]) 

def p_datatype(p):
    '''

    datatype    :   INT_TYPE
                |   FLOAT_TYPE
                |   STRING_TYPE
                |   VOID_TYPE
    '''
    p[0] = p[1]
    print("DATATYPE:", p[0])

def p_input(p):
    '''
    input : READ LPAREN NAME RPAREN
    '''

    p[0] = ('READ',p[3])
    print("INPUT:",p[0])

#grammar for printing

def p_output(p):
    '''
    output : output_print
    '''

    p[0] = p[1]
    print("OUTPUT:",p[0])

#printing without newline

def p_output_print(p):
    '''
    output_print : PRINT LPAREN expression RPAREN
    '''

    p[0] = ('PRINT', p[3])
    print("OUTPUT PRINT:",p[0])

#grammar for assigning values

def p_expression_var_assign(p):
    '''
    expression : NAME
    '''
    p[0] = ('var', p[1])
    print("EXPRESSION VAR ASSIGN:", p[0])

#grammar for equating values

def p_var_assign(p):
    '''
    var_assign : NAME EQUALS expression
    '''
    p[0] = ('=', p[1], p[3])
    print("VAR ASSIGN:",p[0])

#grammar for deciding operators

def p_expression(p):
    '''
    expression : expression_operation
    '''
    p[0] = p[1]
    print("EXPRESSION:", p[0])

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
    p[0] = (p[2],p[1],p[3])
    print("EXPRESSION OPERATION:", p[0])

def p_expression_operation_not(p):
    '''
    expression_operation : NOT expression
    '''
    p[0] = (p[1], p[2])
    print("EXPRESSION OPERATION NOT:", p[0])
#grammar for data types

def p_expression_data_type(p):
    '''
    expression : INT
               | FLOAT
               | STRING
               | TRUE
               | FALSE
    '''
    p[0] = p[1]
    print("EXPRESSION TO DATATYPE:", p[0])

def p_expression_int_array(p):
    '''
    expression : LBRACE int_type_array RBRACE
    '''
    p[0] = ('ARRAY', 'INT', p[2])
    print("EXPRESSION TO ARRAY:", p[0])

def p_expression_float_array(p):
    '''
    expression : LBRACE float_type_array RBRACE
    '''
    p[0] = ('ARRAY','FLOAT', p[2])
    print("EXPRESSION TO ARRAY:", p[0])

def p_expression_string_array(p):
    '''
    expression : LBRACE string_type_array RBRACE
    '''
    p[0] = ('ARRAY','STRING', p[2])
    print("EXPRESSION TO ARRAY:", p[0])

def p_expression_bool_array(p):
    '''
    expression : LBRACE bool_type_array RBRACE
    '''
    p[0] = ('ARRAY','BOOL', p[2])
    print("EXPRESSION TO ARRAY:", p[0])


def p_int_type_array(p):
    '''
    int_type_array : empty
                   | INT int_type_array
    '''
    if len(p) == 3:
        if p[2] == None:
            p[0] = (p[1])
        else:
            p[0] = (p[1], p[2])
    else:
        p[0] = None
    print("ARRAY IS INT-TYPED:", p[0])
    
def p_float_type_array(p):
    '''
    float_type_array : empty
                     | FLOAT float_type_array
    '''
    if len(p) == 3:
        if p[2] == None:
            p[0] = (p[1])
        else:
            p[0] = (p[1], p[2])
    else:
        p[0] = None
    print("ARRAY IS FLOAT-TYPED:", p[0])
    
def p_string_type_array(p):
    '''
    string_type_array : empty
                      | STRING string_type_array
    '''
    if len(p) == 3:
        if p[2] == None:
            p[0] = (p[1])
        else:
            p[0] = (p[1], p[2])
    else:
        p[0] = None
    print("ARRAY IS STRING-TYPED:", p[0])

def p_bool_type_array(p):
    '''
    bool_type_array : empty
                      | TRUE bool_type_array
                      | FALSE bool_type_array
    '''
    if len(p) == 3:
        if p[2] == None:
            p[0] = (p[1])
        else:
            p[0] = (p[1], p[2])
    else:
        p[0] = None
    print("ARRAY IS STRING-TYPED:", p[0])

#grammar for parenthesis

def p_expression_parenthesis(p):
    '''
    expression : LPAREN expression RPAREN
    '''
    p[0] = p[2]
    print("EXPRESSION PARENTHESIS", p[0])

#grammar for if statement

def p_conditional(p):
    '''
    conditional : IF expression START NEWLINE language STOP else_if_blocks else_block 
    '''
    p[0] = ('IF', p[2], p[5], p[7], p[8])
    print("CONDITIONAL", p[0])
        
#grammar for deciding if 0 or more else if statements

def p_else_if_blocks(p):
    '''
    else_if_blocks : empty
                   | else_if_blocks else_if_block
    '''
    if len(p) == 2:
        p[0] = 'NO ELSE IF'
    elif p[1] == 'NO ELSE IF':
        p[0] = p[2]
        print("ELSEIF:",p[0])
    else:
        p[0] = ('MULTIELSEIF', p[2], p[1])
        print("MULTIELSEIF:", p[0])

#grammar for else if statement

def p_else_if_block(p):
    '''
    else_if_block : ELSE_IF expression START NEWLINE language STOP
    '''
    p[0] = ('ELSEIF',p[2],p[5])
    print("CONDITIONAL ELSE IF:", p[0])

#grammar for else statement

def p_else_block(p):
    '''
    else_block : ELSE START NEWLINE language STOP
               | empty
    '''
    if len(p) == 2:
        p[0] = 'NO ELSE'
        print("NO ELSE:", p[0])
    else:
        p[0] = ('ELSE',p[4])
        print("ELSE:",p[0])

#grammar for while

def p_iterative(p):
    '''
    iterative : WHILE expression START NEWLINE language STOP
    '''
    p[0] = ('WHILE', p[2],p[5])
    print("ITERATIVE:", p[0])


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




def open_file(filename):
    data = open(filename,"r")
    return data

if len(argv) < 2:
    while True:             #we should be able to read from files ye? - hans
        try:
            s = input(">>> ")
            if(s == "UMU"):              # added exit code via cmd
                break
        except EOFError:
            print()
            break
        parser.parse(s)

elif len(argv) == 2:
    extenstion = argv[1].split('.')[1]
    if extenstion != 'uwu':
        print("Invalid File Name")
        quit()

    filename = open_file(argv[1])

    while True:
        try:
            s = filename.read()
            if s == "":
                break
        except EOFError:
            break
        parser.parse(s)
