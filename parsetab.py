
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'leftANDORnonassocEQUALEQUALNOTEQnonassocLDLDEQGDGDEQleftPLUSMINUSleftMULTIPLYDIVIDEMODULOrightNOTAND COMMA DIVIDE ELSE ELSE_IF EQUALEQUAL EQUALS FALSE FLOAT FLOAT_TYPE GD GDEQ IF INT INT_TYPE LBRACE LD LDEQ LPAREN MINUS MODULO MULTIPLY NAME NEWLINE NOT NOTEQ OR PLUS PRINT RBRACE READ RETURN RPAREN START STOP STRING STRING_TYPE TRUE VOID_TYPE WHILE\n    start : language\n          | empty\n    \n    language : language NEWLINE line\n             | line\n    \n    line : expression\n         | var_assign\n         | func_assign\n         | iterative\n         | conditional\n         | output\n         | input\n         | empty\n    \n    func_assign     :   datatype NAME LPAREN parameters RPAREN START NEWLINE language return_stmt STOP\n    \n    return_stmt     :   RETURN expression NEWLINE\n                    |   RETURN NEWLINE\n                    |   empty\n\n    \n\n    parameters      :   first_param\n                    |   empty\n    \n    first_param     :   datatype NAME COMMA first_param\n                    |   datatype NAME\n\n    \n\n    datatype    :   INT_TYPE\n                |   FLOAT_TYPE\n                |   STRING_TYPE\n                |   VOID_TYPE\n    \n    input : READ LPAREN NAME RPAREN\n    \n    output : output_print\n    \n    output_print : PRINT LPAREN expression RPAREN\n    \n    expression : NAME\n    \n    var_assign : datatype NAME EQUALS expression\n               | NAME EQUALS expression\n    \n    expression : math\n\n    \n    expression : str\n\n    \n    math : math MULTIPLY math\n         | math DIVIDE math\n         | math MODULO math\n         | math PLUS math\n         | math MINUS math\n         | math LD math\n         | math LDEQ math\n         | math GD math\n         | math GDEQ math\n         | math EQUALEQUAL math\n         | math NOTEQ math\n         | math AND math\n         | math OR math\n         | NOT math\n         | MINUS math\n\n    \n    math    : INT\n            | FLOAT\n            | TRUE\n            | FALSE\n    \n    str  : str PLUS str\n         | str LD str\n         | str LDEQ str\n         | str GD str\n         | str GDEQ str\n         | str EQUALEQUAL str\n         | str NOTEQ str\n\n    \n    str     : STRING\n    \n    math : NAME\n    \n    str : NAME\n    \n    expression : LBRACE int_type_array RBRACE\n    \n    expression : LBRACE string_type_array RBRACE\n    \n    int_type_array : math\n                   | math COMMA int_type_array \n    \n    string_type_array : str\n                      | str COMMA string_type_array\n    \n    math : LPAREN math RPAREN\n    \n    str : LPAREN str RPAREN\n    \n    conditional : IF expression START NEWLINE language STOP else_if_blocks else_block \n    \n    else_if_blocks : empty\n                   | else_if_block else_if_blocks\n    \n    else_if_block : ELSE_IF expression START NEWLINE language STOP\n    \n    else_block : ELSE START NEWLINE language STOP\n               | empty\n    \n    iterative : WHILE expression START NEWLINE language STOP\n    \n    empty : \n\n    '
    
_lr_action_items = {'$end':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,20,24,25,26,27,28,34,65,68,69,71,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,92,93,94,95,96,97,98,99,104,105,112,119,120,127,128,131,132,133,136,138,139,146,154,155,],[-77,0,-1,-2,-4,-5,-6,-7,-8,-9,-10,-11,-28,-31,-32,-26,-48,-49,-50,-51,-59,-77,-28,-47,-60,-46,-3,-12,-30,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-43,-44,-45,-52,-61,-53,-54,-55,-56,-57,-58,-62,-63,-68,-69,-29,-25,-27,-76,-77,-77,-71,-77,-70,-75,-72,-13,-74,-73,]),'NEWLINE':([0,2,3,4,5,6,7,8,9,10,11,12,13,14,20,24,25,26,27,28,34,65,68,69,71,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,92,93,94,95,96,97,98,99,104,105,106,107,112,117,118,119,120,123,124,126,127,128,130,131,132,133,135,136,138,139,142,144,145,146,147,149,150,152,153,154,155,],[-77,34,-12,-4,-5,-6,-7,-8,-9,-10,-11,-28,-31,-32,-26,-48,-49,-50,-51,-59,-77,-28,-47,-60,-46,-3,-12,-30,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-43,-44,-45,-52,-61,-53,-54,-55,-56,-57,-58,-62,-63,-68,-69,117,118,-29,-77,-77,-25,-27,34,34,130,-76,-77,-77,-77,-71,-77,34,-70,-75,-72,148,149,150,-13,151,-77,-77,34,34,-74,-73,]),'NAME':([0,15,16,17,18,19,22,23,29,30,31,32,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,67,70,72,91,100,101,102,113,117,118,130,134,142,149,150,],[12,60,61,60,65,65,69,69,-21,-22,-23,-24,12,65,69,69,69,69,69,69,69,69,69,69,69,69,69,90,90,90,90,90,90,90,108,69,65,90,69,90,65,121,12,12,12,65,65,12,12,]),'LBRACE':([0,18,19,34,35,72,102,117,118,130,134,142,149,150,],[15,15,15,15,15,15,15,15,15,15,15,15,15,15,]),'WHILE':([0,34,117,118,130,149,150,],[18,18,18,18,18,18,18,]),'IF':([0,34,117,118,130,149,150,],[19,19,19,19,19,19,19,]),'READ':([0,34,117,118,130,149,150,],[21,21,21,21,21,21,21,]),'NOT':([0,15,17,18,19,22,23,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,70,72,100,102,117,118,130,134,142,149,150,],[23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,]),'MINUS':([0,12,13,15,17,18,19,22,23,24,25,26,27,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,58,60,62,65,68,69,70,71,72,76,77,78,79,80,81,82,83,84,85,86,87,88,100,102,104,117,118,130,134,142,149,150,],[22,-60,40,22,22,22,22,22,22,-48,-49,-50,-51,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,40,-60,40,-60,-47,-60,22,-46,22,-33,-34,-35,-36,-37,40,40,40,40,40,40,40,40,22,22,-68,22,22,22,22,22,22,22,]),'INT':([0,15,17,18,19,22,23,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,70,72,100,102,117,118,130,134,142,149,150,],[24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,]),'FLOAT':([0,15,17,18,19,22,23,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,70,72,100,102,117,118,130,134,142,149,150,],[25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,]),'TRUE':([0,15,17,18,19,22,23,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,70,72,100,102,117,118,130,134,142,149,150,],[26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,]),'FALSE':([0,15,17,18,19,22,23,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,70,72,100,102,117,118,130,134,142,149,150,],[27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,]),'LPAREN':([0,15,17,18,19,21,22,23,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,61,70,72,91,100,101,102,117,118,130,134,142,149,150,],[17,17,17,17,17,67,70,70,72,17,17,70,70,70,70,70,70,70,70,70,70,70,70,70,91,91,91,91,91,91,91,103,70,17,91,70,91,17,17,17,17,17,17,17,17,]),'STRING':([0,15,17,18,19,34,35,49,50,51,52,53,54,55,72,91,101,102,117,118,130,134,142,149,150,],[28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,]),'INT_TYPE':([0,34,103,117,118,125,130,149,150,],[29,29,29,29,29,29,29,29,29,]),'FLOAT_TYPE':([0,34,103,117,118,125,130,149,150,],[30,30,30,30,30,30,30,30,30,]),'STRING_TYPE':([0,34,103,117,118,125,130,149,150,],[31,31,31,31,31,31,31,31,31,]),'VOID_TYPE':([0,34,103,117,118,125,130,149,150,],[32,32,32,32,32,32,32,32,32,]),'PRINT':([0,34,117,118,130,149,150,],[33,33,33,33,33,33,33,]),'STOP':([4,5,6,7,8,9,10,11,12,13,14,20,24,25,26,27,28,34,65,68,69,71,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,92,93,94,95,96,97,98,99,104,105,112,117,118,119,120,123,124,127,128,130,131,132,133,135,136,138,139,141,143,146,148,149,150,151,152,153,154,155,],[-4,-5,-6,-7,-8,-9,-10,-11,-28,-31,-32,-26,-48,-49,-50,-51,-59,-77,-28,-47,-60,-46,-3,-12,-30,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-43,-44,-45,-52,-61,-53,-54,-55,-56,-57,-58,-62,-63,-68,-69,-29,-77,-77,-25,-27,127,128,-76,-77,-77,-77,-71,-77,-77,-70,-75,-72,146,-16,-13,-15,-77,-77,-14,154,155,-74,-73,]),'RETURN':([4,5,6,7,8,9,10,11,12,13,14,20,24,25,26,27,28,34,65,68,69,71,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,92,93,94,95,96,97,98,99,104,105,112,119,120,127,128,130,131,132,133,135,136,138,139,146,154,155,],[-4,-5,-6,-7,-8,-9,-10,-11,-28,-31,-32,-26,-48,-49,-50,-51,-59,-77,-28,-47,-60,-46,-3,-12,-30,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-43,-44,-45,-52,-61,-53,-54,-55,-56,-57,-58,-62,-63,-68,-69,-29,-25,-27,-76,-77,-77,-77,-71,-77,142,-70,-75,-72,-13,-74,-73,]),'EQUALS':([12,61,],[35,102,]),'MULTIPLY':([12,13,24,25,26,27,58,60,62,65,68,69,71,76,77,78,79,80,81,82,83,84,85,86,87,88,104,],[-60,36,-48,-49,-50,-51,36,-60,36,-60,36,-60,-46,-33,-34,-35,36,36,36,36,36,36,36,36,36,36,-68,]),'DIVIDE':([12,13,24,25,26,27,58,60,62,65,68,69,71,76,77,78,79,80,81,82,83,84,85,86,87,88,104,],[-60,37,-48,-49,-50,-51,37,-60,37,-60,37,-60,-46,-33,-34,-35,37,37,37,37,37,37,37,37,37,37,-68,]),'MODULO':([12,13,24,25,26,27,58,60,62,65,68,69,71,76,77,78,79,80,81,82,83,84,85,86,87,88,104,],[-60,38,-48,-49,-50,-51,38,-60,38,-60,38,-60,-46,-33,-34,-35,38,38,38,38,38,38,38,38,38,38,-68,]),'PLUS':([12,13,14,24,25,26,27,28,58,59,60,62,63,65,68,69,71,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,92,93,94,95,96,97,104,105,],[-60,39,49,-48,-49,-50,-51,-59,39,49,-60,39,49,-60,-47,-60,-46,-33,-34,-35,-36,-37,39,39,39,39,39,39,39,39,-52,-61,49,49,49,49,49,49,-68,-69,]),'LD':([12,13,14,24,25,26,27,28,58,59,60,62,63,65,68,69,71,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,92,93,94,95,96,97,104,105,],[-60,41,50,-48,-49,-50,-51,-59,41,50,-60,41,50,-60,-47,-60,-46,-33,-34,-35,-36,-37,None,None,None,None,41,41,41,41,-52,-61,None,None,None,None,50,50,-68,-69,]),'LDEQ':([12,13,14,24,25,26,27,28,58,59,60,62,63,65,68,69,71,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,92,93,94,95,96,97,104,105,],[-60,42,51,-48,-49,-50,-51,-59,42,51,-60,42,51,-60,-47,-60,-46,-33,-34,-35,-36,-37,None,None,None,None,42,42,42,42,-52,-61,None,None,None,None,51,51,-68,-69,]),'GD':([12,13,14,24,25,26,27,28,58,59,60,62,63,65,68,69,71,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,92,93,94,95,96,97,104,105,],[-60,43,52,-48,-49,-50,-51,-59,43,52,-60,43,52,-60,-47,-60,-46,-33,-34,-35,-36,-37,None,None,None,None,43,43,43,43,-52,-61,None,None,None,None,52,52,-68,-69,]),'GDEQ':([12,13,14,24,25,26,27,28,58,59,60,62,63,65,68,69,71,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,92,93,94,95,96,97,104,105,],[-60,44,53,-48,-49,-50,-51,-59,44,53,-60,44,53,-60,-47,-60,-46,-33,-34,-35,-36,-37,None,None,None,None,44,44,44,44,-52,-61,None,None,None,None,53,53,-68,-69,]),'EQUALEQUAL':([12,13,14,24,25,26,27,28,58,59,60,62,63,65,68,69,71,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,92,93,94,95,96,97,104,105,],[-60,45,54,-48,-49,-50,-51,-59,45,54,-60,45,54,-60,-47,-60,-46,-33,-34,-35,-36,-37,-38,-39,-40,-41,None,None,45,45,-52,-61,-53,-54,-55,-56,None,None,-68,-69,]),'NOTEQ':([12,13,14,24,25,26,27,28,58,59,60,62,63,65,68,69,71,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,92,93,94,95,96,97,104,105,],[-60,46,55,-48,-49,-50,-51,-59,46,55,-60,46,55,-60,-47,-60,-46,-33,-34,-35,-36,-37,-38,-39,-40,-41,None,None,46,46,-52,-61,-53,-54,-55,-56,None,None,-68,-69,]),'AND':([12,13,24,25,26,27,58,60,62,65,68,69,71,76,77,78,79,80,81,82,83,84,85,86,87,88,104,],[-60,47,-48,-49,-50,-51,47,-60,47,-60,-47,-60,-46,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-43,-44,-45,-68,]),'OR':([12,13,24,25,26,27,58,60,62,65,68,69,71,76,77,78,79,80,81,82,83,84,85,86,87,88,104,],[-60,48,-48,-49,-50,-51,48,-60,48,-60,-47,-60,-46,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-43,-44,-45,-68,]),'START':([13,14,24,25,26,27,28,64,65,66,68,69,71,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,92,93,94,95,96,97,98,99,104,105,122,137,140,],[-31,-32,-48,-49,-50,-51,-59,106,-28,107,-47,-60,-46,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-43,-44,-45,-52,-61,-53,-54,-55,-56,-57,-58,-62,-63,-68,-69,126,144,145,]),'RPAREN':([13,14,24,25,26,27,28,60,62,63,65,68,69,71,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,92,93,94,95,96,97,98,99,103,104,105,108,109,114,115,116,121,129,],[-31,-32,-48,-49,-50,-51,-59,-60,104,105,-28,-47,-60,-46,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-43,-44,-45,-52,-61,-53,-54,-55,-56,-57,-58,-62,-63,-77,-68,-69,119,120,122,-17,-18,-20,-19,]),'COMMA':([24,25,26,27,28,58,59,60,68,69,71,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,92,93,94,95,96,97,104,105,121,],[-48,-49,-50,-51,-59,100,101,-60,-47,-60,-46,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-43,-44,-45,-52,-61,-53,-54,-55,-56,-57,-58,-68,-69,125,]),'RBRACE':([24,25,26,27,28,56,57,58,59,60,68,69,71,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,92,93,94,95,96,97,104,105,110,111,],[-48,-49,-50,-51,-59,98,99,-64,-66,-60,-47,-60,-46,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-43,-44,-45,-52,-61,-53,-54,-55,-56,-57,-58,-68,-69,-65,-67,]),'ELSE':([128,131,132,133,139,155,],[-77,137,-71,-77,-72,-73,]),'ELSE_IF':([128,133,155,],[134,134,-73,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'start':([0,],[1,]),'language':([0,117,118,130,149,150,],[2,123,124,135,152,153,]),'empty':([0,34,103,117,118,128,130,131,133,135,149,150,],[3,74,116,74,74,132,74,138,132,143,74,74,]),'line':([0,34,117,118,130,149,150,],[4,73,4,4,4,4,4,]),'expression':([0,18,19,34,35,72,102,117,118,130,134,142,149,150,],[5,64,66,5,75,109,112,5,5,5,140,147,5,5,]),'var_assign':([0,34,117,118,130,149,150,],[6,6,6,6,6,6,6,]),'func_assign':([0,34,117,118,130,149,150,],[7,7,7,7,7,7,7,]),'iterative':([0,34,117,118,130,149,150,],[8,8,8,8,8,8,8,]),'conditional':([0,34,117,118,130,149,150,],[9,9,9,9,9,9,9,]),'output':([0,34,117,118,130,149,150,],[10,10,10,10,10,10,10,]),'input':([0,34,117,118,130,149,150,],[11,11,11,11,11,11,11,]),'math':([0,15,17,18,19,22,23,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,70,72,100,102,117,118,130,134,142,149,150,],[13,58,62,13,13,68,71,13,13,76,77,78,79,80,81,82,83,84,85,86,87,88,62,13,58,13,13,13,13,13,13,13,13,]),'str':([0,15,17,18,19,34,35,49,50,51,52,53,54,55,72,91,101,102,117,118,130,134,142,149,150,],[14,59,63,14,14,14,14,89,92,93,94,95,96,97,14,63,59,14,14,14,14,14,14,14,14,]),'datatype':([0,34,103,117,118,125,130,149,150,],[16,16,113,16,16,113,16,16,16,]),'output_print':([0,34,117,118,130,149,150,],[20,20,20,20,20,20,20,]),'int_type_array':([15,100,],[56,110,]),'string_type_array':([15,101,],[57,111,]),'parameters':([103,],[114,]),'first_param':([103,125,],[115,129,]),'else_if_blocks':([128,133,],[131,139,]),'else_if_block':([128,133,],[133,133,]),'else_block':([131,],[136,]),'return_stmt':([135,],[141,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> start","S'",1,None,None,None),
  ('start -> language','start',1,'p_start','uwulyzer.py',154),
  ('start -> empty','start',1,'p_start','uwulyzer.py',155),
  ('language -> language NEWLINE line','language',3,'p_language','uwulyzer.py',186),
  ('language -> line','language',1,'p_language','uwulyzer.py',187),
  ('line -> expression','line',1,'p_line','uwulyzer.py',203),
  ('line -> var_assign','line',1,'p_line','uwulyzer.py',204),
  ('line -> func_assign','line',1,'p_line','uwulyzer.py',205),
  ('line -> iterative','line',1,'p_line','uwulyzer.py',206),
  ('line -> conditional','line',1,'p_line','uwulyzer.py',207),
  ('line -> output','line',1,'p_line','uwulyzer.py',208),
  ('line -> input','line',1,'p_line','uwulyzer.py',209),
  ('line -> empty','line',1,'p_line','uwulyzer.py',210),
  ('func_assign -> datatype NAME LPAREN parameters RPAREN START NEWLINE language return_stmt STOP','func_assign',10,'p_func_assign','uwulyzer.py',218),
  ('return_stmt -> RETURN expression NEWLINE','return_stmt',3,'p_return_stmt','uwulyzer.py',226),
  ('return_stmt -> RETURN NEWLINE','return_stmt',2,'p_return_stmt','uwulyzer.py',227),
  ('return_stmt -> empty','return_stmt',1,'p_return_stmt','uwulyzer.py',228),
  ('parameters -> first_param','parameters',1,'p_parameters','uwulyzer.py',243),
  ('parameters -> empty','parameters',1,'p_parameters','uwulyzer.py',244),
  ('first_param -> datatype NAME COMMA first_param','first_param',4,'p_first_param','uwulyzer.py',251),
  ('first_param -> datatype NAME','first_param',2,'p_first_param','uwulyzer.py',252),
  ('datatype -> INT_TYPE','datatype',1,'p_datatype','uwulyzer.py',264),
  ('datatype -> FLOAT_TYPE','datatype',1,'p_datatype','uwulyzer.py',265),
  ('datatype -> STRING_TYPE','datatype',1,'p_datatype','uwulyzer.py',266),
  ('datatype -> VOID_TYPE','datatype',1,'p_datatype','uwulyzer.py',267),
  ('input -> READ LPAREN NAME RPAREN','input',4,'p_input','uwulyzer.py',273),
  ('output -> output_print','output',1,'p_output','uwulyzer.py',282),
  ('output_print -> PRINT LPAREN expression RPAREN','output_print',4,'p_output_print','uwulyzer.py',291),
  ('expression -> NAME','expression',1,'p_expression_var_assign','uwulyzer.py',300),
  ('var_assign -> datatype NAME EQUALS expression','var_assign',4,'p_var_assign','uwulyzer.py',308),
  ('var_assign -> NAME EQUALS expression','var_assign',3,'p_var_assign','uwulyzer.py',309),
  ('expression -> math','expression',1,'p_expression_to_int','uwulyzer.py',324),
  ('expression -> str','expression',1,'p_expression_to_str','uwulyzer.py',333),
  ('math -> math MULTIPLY math','math',3,'p_math','uwulyzer.py',340),
  ('math -> math DIVIDE math','math',3,'p_math','uwulyzer.py',341),
  ('math -> math MODULO math','math',3,'p_math','uwulyzer.py',342),
  ('math -> math PLUS math','math',3,'p_math','uwulyzer.py',343),
  ('math -> math MINUS math','math',3,'p_math','uwulyzer.py',344),
  ('math -> math LD math','math',3,'p_math','uwulyzer.py',345),
  ('math -> math LDEQ math','math',3,'p_math','uwulyzer.py',346),
  ('math -> math GD math','math',3,'p_math','uwulyzer.py',347),
  ('math -> math GDEQ math','math',3,'p_math','uwulyzer.py',348),
  ('math -> math EQUALEQUAL math','math',3,'p_math','uwulyzer.py',349),
  ('math -> math NOTEQ math','math',3,'p_math','uwulyzer.py',350),
  ('math -> math AND math','math',3,'p_math','uwulyzer.py',351),
  ('math -> math OR math','math',3,'p_math','uwulyzer.py',352),
  ('math -> NOT math','math',2,'p_math','uwulyzer.py',353),
  ('math -> MINUS math','math',2,'p_math','uwulyzer.py',354),
  ('math -> INT','math',1,'p_math_expression_expression_data_type','uwulyzer.py',364),
  ('math -> FLOAT','math',1,'p_math_expression_expression_data_type','uwulyzer.py',365),
  ('math -> TRUE','math',1,'p_math_expression_expression_data_type','uwulyzer.py',366),
  ('math -> FALSE','math',1,'p_math_expression_expression_data_type','uwulyzer.py',367),
  ('str -> str PLUS str','str',3,'p_str','uwulyzer.py',374),
  ('str -> str LD str','str',3,'p_str','uwulyzer.py',375),
  ('str -> str LDEQ str','str',3,'p_str','uwulyzer.py',376),
  ('str -> str GD str','str',3,'p_str','uwulyzer.py',377),
  ('str -> str GDEQ str','str',3,'p_str','uwulyzer.py',378),
  ('str -> str EQUALEQUAL str','str',3,'p_str','uwulyzer.py',379),
  ('str -> str NOTEQ str','str',3,'p_str','uwulyzer.py',380),
  ('str -> STRING','str',1,'p_str_datatype','uwulyzer.py',388),
  ('math -> NAME','math',1,'p_math_var','uwulyzer.py',394),
  ('str -> NAME','str',1,'p_str_var','uwulyzer.py',400),
  ('expression -> LBRACE int_type_array RBRACE','expression',3,'p_expression_int_array','uwulyzer.py',407),
  ('expression -> LBRACE string_type_array RBRACE','expression',3,'p_expression_string_array','uwulyzer.py',415),
  ('int_type_array -> math','int_type_array',1,'p_math_type_array','uwulyzer.py',423),
  ('int_type_array -> math COMMA int_type_array','int_type_array',3,'p_math_type_array','uwulyzer.py',424),
  ('string_type_array -> str','string_type_array',1,'p_string_type_array','uwulyzer.py',439),
  ('string_type_array -> str COMMA string_type_array','string_type_array',3,'p_string_type_array','uwulyzer.py',440),
  ('math -> LPAREN math RPAREN','math',3,'p_math_parenthesis','uwulyzer.py',474),
  ('str -> LPAREN str RPAREN','str',3,'p_str_parenthesis','uwulyzer.py',480),
  ('conditional -> IF expression START NEWLINE language STOP else_if_blocks else_block','conditional',8,'p_conditional','uwulyzer.py',488),
  ('else_if_blocks -> empty','else_if_blocks',1,'p_else_if_blocks','uwulyzer.py',496),
  ('else_if_blocks -> else_if_block else_if_blocks','else_if_blocks',2,'p_else_if_blocks','uwulyzer.py',497),
  ('else_if_block -> ELSE_IF expression START NEWLINE language STOP','else_if_block',6,'p_else_if_block','uwulyzer.py',508),
  ('else_block -> ELSE START NEWLINE language STOP','else_block',5,'p_else_block','uwulyzer.py',516),
  ('else_block -> empty','else_block',1,'p_else_block','uwulyzer.py',517),
  ('iterative -> WHILE expression START NEWLINE language STOP','iterative',6,'p_iterative','uwulyzer.py',528),
  ('empty -> <empty>','empty',0,'p_empty','uwulyzer.py',542),
]
