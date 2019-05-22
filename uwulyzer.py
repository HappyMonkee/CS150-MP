import ply.lex as lex
import ply.yacc as yacc
from sys import *



reserved = {
    
    'Tutuwu'    : 'TRUE',
    'UnU'       : 'FALSE',
    'lyk'       : 'IF',
    'ewselyk'   : 'ELSE_IF',
    'ewse'      : 'ELSE',
    'wiw'       : 'WHILE',
    'and'       : 'AND',
    'or'        : 'OR',
    'not'       : 'NOT',

    'int'       : 'INT_TYPE',
    'float'     : 'FLOAT_TYPE',
    'string'    : 'STRING_TYPE',
    'void'      : 'VOID_TYPE',
    'return'    : 'RETURN',
    'ish'       : 'EQUALS', 
    # 'stop'      : 'STOP',

    'pwint'     : 'PRINT',
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


op = {
    '+w+'   : '+',
    '-w-'   : '-',
    '*w*'   : '*',
    '\\w/'  : '/',
    '%%w%%' : '%',
    '=w='   : '==',
    '>w<'   : '!=',
    '<w<'   : '<',
    '=w<'   : '<=',
    '>w='   : '>=',
    '>w>'   : '>',
    'and'   : 'and',
    'or'    : 'or',
    'not'   : '!'
}

t_START         =   r'\{\.\w\.\}\/'
t_STOP          =   r'\\\{\.\w\.\}'

t_PLUS          =   r'\+\w\+'
t_MINUS         =   r'\-\w\-'
t_MULTIPLY      =   r'\*\w\*'
t_DIVIDE        =   r'\\\w\/'
t_MODULO        =   r'\%\w\%'
t_EQUALEQUAL    =   r'\=\w\='
t_NOTEQ         =   r'\>\w\<'
t_LD            =   r'\<\w\<'
t_GD            =   r'\>\w\>'
t_LDEQ          =   r'\=\w\<'
t_GDEQ          =   r'\>\w\='

t_COMMA         =   r'\,'
t_LPAREN        =   r'\('
t_RPAREN        =   r'\)'
t_LBRACE        =   r'\['
t_RBRACE        =   r'\]'

t_ignore = r' '

def t_NEWLINE(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    # print(len(t.value))
    return t;

def t_COMMENT(t):
    r'\#.*\n+|\#.*'
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
    print("Illegal character - %d" % t.value[0])
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
functions = list()

def p_start(p):
    '''
    start : language
          | empty
    '''
    global functions


    p[0] = p[1]

    print("\n\n\n\n")

    result = run(p[0])
    # result = " "

    # print(p[0])
    # if p[0] != None:
    #     print()
    #     print()
    #     print(p[0])

    #     for i in c_file:
    #         fo.write(i + '\n')
    #     # fo.write(str(p[0]))
    fo = open("fileout.c", "w")

    c_file = list()

    libraries = ("\n".join(["#include <stdio.h>", "#include <string.h>", "#include <stdbool.h>", "#include <math.h>", "#include <stdlib.h>","\n\n\n\n"]))
    c_file.append(libraries)
    c_file.append("\n".join(functions))
    main = "int main() {"
    c_file.append(main)
    c_file.append(result)
    c_file.append("\nreturn 0;\n}\n")

    for i in c_file:
        fo.write(i)
    print("\n".join(c_file))
    fo.close()
    #     fo.close()


#grammar for multiple lines

def p_language(p):
    '''
    language : language NEWLINE line
             | line
    '''
    if len(p) == 4:

        if p[3] != None:
            p[0] = ('MULTILINES', p[1], p[3])
        else:
            p[0] = ('LANGUAGE', p[1])

    else:
        p[0] = p[1]

#grammar for each line of code

def p_line(p):
    '''
    line : expression
         | var_assign
         | func_assign
         | iterative
         | conditional
         | func_call
         | output
         | input
         | empty
    '''
    p[0] = ('LINE', p[1])

#grammar for reading

def p_func_call(p):
    '''
    func_call : NAME LPAREN pars RPAREN
    '''
    p[0] = ('FUNC_CALL', p[1], p[3])

def p_pars(p):
    '''
    pars : expression
         | expression COMMA expression
    '''

    if len(p) == 2:
        p[0] = ('PARS', p[1], None)
    else:
        p[0] = ('PARS', p[1], p[3])        

def p_func_assign(p):
    '''
    func_assign     :   datatype NAME LPAREN parameters RPAREN START NEWLINE language return_stmt STOP
    ''' 
    datatype = p[1].upper()
    p[0] = ('FUNCTION', datatype, p[2], p[4], p[8], p[9])


def p_return_stmt(p):
    '''
    return_stmt     :   RETURN expression NEWLINE
                    |   RETURN NEWLINE
                    |   empty

    '''
    if len(p) == 4:
        p[0] = ('RETURN', p[2])
    elif len(p) == 3:
        p[0] = ('RETURN', 'void')
    else:
        p[0] = None
    


def p_parameters(p):
    '''

    parameters      :   first_param
                    |   empty
    '''

    p[0] = ('PARAMETERS', p[1])

def p_first_param(p):
    '''
    first_param     :   datatype NAME COMMA first_param
                    |   datatype NAME

    '''
    datatype = p[1].upper()
    if len(p) == 5:
        p[0] = ('FIRST_PARAM',datatype, p[2], p[4])
    else:
        p[0] = ('FIRST_PARAM', datatype, p[2], None)

def p_datatype(p):
    '''

    datatype    :   INT_TYPE
                |   FLOAT_TYPE
                |   STRING_TYPE
                |   VOID_TYPE
    '''
    p[0] = p[1]

def p_input(p):
    '''
    input : READ LPAREN NAME RPAREN
    '''

    p[0] = ('READ', ('VAR',p[3]) )

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
    p[0] = ('VAR', p[1])

#grammar for equating values

def p_var_assign(p):
    '''
    var_assign : datatype NAME EQUALS expression
               | NAME EQUALS expression
    '''

    if len(p) == 5:
        datatype = p[1].upper()
        p[0] = ('ASSIGN', datatype, p[2], p[4])
    else:
        p[0] = ('RENAME', p[1] , p[3])



#grammar for deciding operators

def p_expression_to_int(p):
    '''
    expression : math

    '''
    p[0] = ('MATH', p[1])

#grammar for infix operators

def p_expression_to_str(p):
    '''
    expression : str

    '''
    p[0] = ('STRING', p[1])

def p_math(p):
    '''
    math : math MULTIPLY math
         | math DIVIDE math
         | math MODULO math
         | math PLUS math
         | math MINUS math
         | math LD math
         | math LDEQ math
         | math GD math
         | math GDEQ math
         | math EQUALEQUAL math
         | math NOTEQ math
         | math AND math
         | math OR math
         | NOT math
         | MINUS math

    '''
    if len(p) == 4:
        p[0] = ('MATH_EXP', p[2], p[1], p[3])
    else:
        p[0] = ('MATH_EXP', p[1], p[2])

def p_math_expression_expression_data_type(p):
    '''
    math    : INT
            | FLOAT
            | TRUE
            | FALSE
    '''
    p[0] = ('MATH', p[1])


def p_str(p):
    '''
    str  : str PLUS str
         | str LD str
         | str LDEQ str
         | str GD str
         | str GDEQ str
         | str EQUALEQUAL str
         | str NOTEQ str

    '''
    p[0] = ('STR_EXP',p[2], p[1], p[3])


def p_str_datatype(p):
    '''
    str     : STRING
    '''
    p[0] = ('STRING', p[1])

def p_math_var(p):
    '''
    math : NAME
    '''
    p[0] = ('VAR', p[1])

def p_str_var(p):
    '''
    str : NAME
    '''
    p[0] = ('VAR', p[1])


def p_expression_int_array(p):
    '''
    expression : LBRACE int_type_array RBRACE
    '''
    p[0] = ('ARRAY', p[2])


def p_expression_string_array(p):
    '''
    expression : LBRACE string_type_array RBRACE
    '''
    p[0] = ('ARRAY', p[2])


def p_math_type_array(p):
    '''
    int_type_array : math
                   | math COMMA int_type_array 
    '''
    if len(p) == 4:
        # if p[3] == None:
        #     p[0] = (p[1])
        # else:
        p[0] = ("ELEMENTS", p[1], p[3])
    else:
        p[0] = ("ELEMENTS", p[1], None)
    

    
def p_string_type_array(p):
    '''
    string_type_array : str
                      | str COMMA string_type_array
    '''
    if len(p) == 4:
        p[0] = ("ELEMENTS", p[1], p[3])
    else:
        p[0] = ("ELEMENTS", p[1], None)


# def p_bool_type_array(p):
#     '''
#     bool_type_array : empty
#                       | TRUE bool_type_array
#                       | FALSE bool_type_array
#     '''
#     if len(p) == 3:
#         if p[2] == None:
#             p[0] = (p[1])
#         else:
#             p[0] = (p[1], p[2])
#     else:
#         p[0] = None
#     print("ARRAY IS STRING-TYPED:", p[0])

#grammar for parenthesis

# def p_expression_parenthesis(p):
#     '''
#     expression : LPAREN expression RPAREN
#     '''
#     p[0] = p[2]

def p_math_parenthesis(p):
    '''
    math : LPAREN math RPAREN
    '''
    p[0] = ("PAREN", p[2])

def p_str_parenthesis(p):
    '''
    str : LPAREN str RPAREN
    '''
    p[0] = ("PAREN", p[2])

#grammar for if statement

def p_conditional(p):
    '''
    conditional : IF expression START NEWLINE language STOP else_if_blocks else_block 
    '''
    p[0] = ('IF', p[2], p[5], p[7], p[8])
        
#grammar for deciding if 0 or more else if statements

def p_else_if_blocks(p):
    '''
    else_if_blocks : empty
                   | else_if_block else_if_blocks
    '''
    if len(p) == 2:
        p[0] = None
    else:
        p[0] = ('MULTIELSEIF', p[1], p[2])

#grammar for else if statement

def p_else_if_block(p):
    '''
    else_if_block : ELSE_IF expression START NEWLINE language STOP
    '''
    p[0] = ('ELSEIF',p[2],p[5])

#grammar for else statement

def p_else_block(p):
    '''
    else_block : ELSE START NEWLINE language STOP
               | empty
    '''
    if len(p) == 2:
        p[0] = None
    else:
        p[0] = ('ELSE', p[4])

#grammar for while

def p_iterative(p):
    '''
    iterative : WHILE expression START NEWLINE language STOP
    '''
    p[0] = ('WHILE', p[2],p[5])



def p_error(p):

    print("Error - ", p)
    print("Syntax Error ;w;")

def p_empty(p):
    '''
    empty : 

    '''
    p[0] = None

parser = yacc.yacc()
env = {}
func_env = {}
dtype = {
    'INT':'int',
    'FLOAT':'float',
    'BOOL':'bool',
    'void': 'void',
    'STRING' : 'char *'
}

def run(p):
    global env
    global func_env
    global functions

    if type(p) == tuple:
        if p[0] == 'LANGUAGE':
            if p[1] == None:
                return ""
            return str(run(p[1]))

        if p[0] == 'MULTILINES':
            if p[2] == None:
                return str(run(p[1])) + '\n'
            return str(run(p[1])) + "\n" + str(run(p[2]))

        if p[0] == 'LINE':
            if p[1] == None:
                return " "

            return str(run(p[1]))

        if p[0] == 'FUNC_CALL':
            func_name = p[1]
            if func_name not in func_env:
                print("Undeclared function ;w; - ", func_name)
                return ""
            params = run(p[2])
            params = ",".join(params)

            return func_name + "(" + params + ");\n"


        if p[0] == 'PARS':
            head = str(run(p[1]))
            tail = p[2]
            if tail == None:
                tail = []

            return [head] + tail

        if p[0] == 'FUNCTION':
            datatype = dtype[p[1]]
            name = p[2]
            func_env[name] = {}
            parameters = run(p[3])

            params = []
            # print(parameters)
            for haha in parameters:
                func_env[haha[1]] = haha[0]
                env[haha[1]] = haha[0].upper()
                params.append(" ".join(haha))

            params = ",".join(params)

            body = str(run(p[4]))
            ret = run(p[5])
            if ret == None:
                ret = ""

            func = datatype + " " + name + "(" + params + ")" + "{\n" + body + "\n" + ret + "\n}\n"

            functions.append(func)
            return "\n"

        if p[0] == 'RETURN':
            if p[1] == 'void':
                return 'return;\n'
            if p[1] == None:
                return ""
            retval = str(run(p[1]))
            return 'return + ' + retval + ";\n"


        if p[0] == 'PARAMETERS':
            params = run(p[1])

            return params

        if p[0] == 'FIRST_PARAM':
            dataype = dtype[str(run(p[1]))]
            name = str(run(p[2]))
            others = p[3]


            if others != None:
                all_param = [[dataype,name]] + run(others)
            else:
                all_param = [[dataype,name]]

            return all_param

        if p[0] == 'PRINT':
            
            to_print = run(p[1])

            if p[1][0] == 'VAR':
                a = run(p[1])

                if env[a] == 'INT':
                    stmt = "printf(\"%%d\\n\", %s);" % (a)
                elif env[a] == 'STRING':
                    stmt = "printf(\"%%s\\n\", %s);" % (a)
                elif env[a] == 'FLOAT':
                    stmt = "printf(\"%%f\\n\", %s);" % (a)

            if p[1][0] == 'STRING':

                stmt = "printf(\"%s\\n\");" % str(run(p[1]))

            if p[1][0] == 'MATH':
                stmt = "printf(\"%%d\\n\", %s);" % str(run(p[1]))

            return stmt

        if p[0] == 'READ':
            # (READ, name)
            a = run(p[1])
            if env[a] == 'INT':
                stmt = "scanf(\"%%d\", &%s);" % (a)
            elif env[a] == 'STRING:':
                stmt = "scanf(\"%%s\", &%s);" % (a)
            elif env[a] == 'FLOAT':
                stmt = "scanf(\"%%f\", &%s);" % (a)

            return stmt
        if p[0] == 'ARRAY':
            ### Deal with this in var ###
            elements = run(p[1])

            return "{" + ",".join(elements) + "}"


        if p[0] == 'ELEMENTS':
            head = run(p[1])

            if p[2] != None:
                tail = run(p[2])
            else:
                tail = []

            return [head] + tail

        if p[0] == 'PAREN':
            return '(%s)' % (str(run([1])))

        if p[0] == 'IF':
            condition = str(run(p[1]))
            body = str(run(p[2]))
            elseif = p[3]
            els = p[4]


            if elseif != None:
                elseif = str(run(p[3]))
            else:
                elseif = ""

            if els != None:
                els = str(run(p[4]))
            else:
                els = ""

            stmt = ("if (%s) {\n%s\n}\n" % (condition, body))+ elseif + "\n" + els 
            return stmt

        if p[0] == 'ELSEIF':
            condition = str(run(p[1]))
            body = str(run(p[2]))

            return "else if (%s) {\n%s\n}" % (condition, body)

        if p[0] == "MULTIELSEIF":
            head = str(run(p[1]))
            tail = p[2]
            if tail != None:
                tail = run(p[2])
            else:
                tail = ""

            return head + "\n" +  tail

        if p[0] == "ELSE":
            body = str(run(p[1]))
            return "else {\n%s\n}\n" % (body)

        if p[0] == 'WHILE':
            condition = str(run(p[1]))
            body = str(run(p[2]))

            stmt = "while (%s) {\n%s\n}\n" % (condition, body)
            return stmt

        if p[0] == 'MATH_EXP' or p[0] == 'STR_EXP':

            if len(p) == 4:
                operator = op[p[1]]
                val1 = str(run(p[2]))
                val2 = str(run(p[3]))

                return val1 + " " + operator + " " + val2 
            else:
                operator = op[p[1]]
                val = str(run(p[2]))

                return operator + val

        if p[0] == 'ASSIGN':
            datatype = p[1].upper()
            name = p[2]
            expression = p[3]
            expression_type = p[3][0]

            internaltype = ""
            if datatype == 'INT' or datatype == 'FLOAT' or datatype == 'TRUE' or datatype == 'FALSE':
                internaltype = 'MATH'
            if datatype == 'STRING':
                internaltype = 'STRING'

            if expression_type == "STR_EXP":
                expression_type = 'STRING'

            if expression_type == 'MATH_EXP':
                expression_type = 'MATH'


            if internaltype != expression_type:
                print("Variable type and value are not of compatible types at ASSIGN! ", internaltype, expression_type)
                return

            if name in env:
                print("Variable has been declared before! - ", name)
                return


            env[name] = datatype
            expression = str(run(expression))

            stmt = datatype.lower() + " " + name + " = " +  expression + ";\n"
            return stmt

        if p[0] == 'RENAME':
            name = p[1]
            if name not in env:
                print("Undeclared variable found! - '", p[1])
                return

            expression = p[2] 
            expression_type = p[2][0]

            if expression_type == "STR_EXP":
                expression_type = 'STRING'

            if expression_type == 'MATH_EXP':
                expression_type = 'MATH'


            datatype = env[name].upper()

            internaltype = ""
            if datatype == 'INT' or datatype == 'FLOAT' or datatype == 'TRUE' or datatype == 'FALSE':
                internaltype = 'MATH'
            if datatype == "STRING":
                internaltype = 'STRING'

            if internaltype != expression_type:
                print("Variable type and value are not of compatible types at REASSIGN! ", internaltype, expression_type )
                return

            expression = str(run(expression))
            stmt = name + " = " + expression + ";\n"
            return stmt

        if p[0] == 'VAR':
            if p[1] not in env:
                 print('Undeclared variable found! - ', p[1])               
                 return
            else:
                 return p[1]

        if p[0] == 'MATH':
            return str(run(p[1]))

        if p[0] == 'STRING':
            return str(run(p[1]))


        
    else:
        if p == "Tutuwu":
            return 1
        if p == "UnU":
            return 0
        return p


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

