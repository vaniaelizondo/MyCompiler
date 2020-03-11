# Se corre con Python3
# VAEM language
import ply.lex as lex
import ply.yacc as yacc

# Reserved words
reserved = {
    'program': 'PROGRAM',
    'subroutine': 'SUBROUTINE',
    'begin': 'BEGIN',
    'end': 'END',
    'integer': 'INTEGER',
    'real': 'REAL',
    'call': 'CALL',
    'print': 'PRINT',
    'read': 'READ',
    'if': 'IF',
    'then': 'THEN',
    'else': 'ELSE',
    'elsif': 'ELSIF',
    'exit': 'EXIT',
    'do': 'DO',
    'or': 'OR',
    'and': 'AND',
    'not': 'NOT'
}

# Valid token types
tokens = [
    'ID',
    'CTE',
    'FLECHA',
    'SUMA',
    'RESTA',
    'MULTIPLICACION',
    'DIVISION',
    'LPAREN',
    'RPAREN',
    'TEXT',
    'MENOR',
    'MENOR_IGUAL',
    'MAYOR_IGUAL',
    'MAYOR',
    'IGUAL',
    'DESIGUAL'
] + list(reserved.values())

# Definition of Tokens
t_CTE = r'[0-9]+'
t_FLECHA = r'->'
t_SUMA = r'\+'
t_RESTA = r'-'
t_MULTIPLICACION = r'\*'
t_DIVISION = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_MENOR = r'<'
t_MENOR_IGUAL = r'<='
t_MAYOR = r'>'
t_MAYOR_IGUAL = r'>='
t_IGUAL = r'=='
t_DESIGUAL = r'!='
t_ignore = ' \t'
t_ignore_COMMENT = r'\#.*'

literals = [',', '=', '<', '>']

# Definition of lex
def t_ID(t):
    r'[a-zA-ZñÑ_][a-zA-ZñÑ0-9_]*'
    # Check for reserved words
    t.type = reserved.get(t.value, 'ID')
    return t

def t_TEXT(t):
    r'(\".*\")'
    t.value = t.value[1:-1]
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print("Illegal character '{0}' at line {1}".format(t.value[0], t.lineno))
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# global variables
error = False
simbolos = {}
indiceSimbolos = 0
cuadruplos = {}
indiceCuadruplos = 0
pilaOperandos = {}
indiceOperandos = 0
pilaOperadores = {}
indiceOperadores = 0
indiceTemporales = 0
subrutinas = {}
indiceSubrutinas = 0
pilaSaltos = {}
indiceSaltos = 0
pilaExit = {}
indiceExit = 0
pilaEjecucion = {}
indiceEjecucion = 0
simbolosDim = {}
indiceSimDim = 0
PC = 0

# Definition of parser
def p_PROGRAMA(p):
    'programa : doprogram ID var rut main END'
    global indiceCuadruplos
    cuadruplos[indiceCuadruplos] = ['finprograma']
    indiceCuadruplos += 1

def p_INICIO(p):
    'doprogram : PROGRAM'
    global indiceCuadruplos
    cuadruplos[indiceCuadruplos] = ['goto']
    indiceCuadruplos += 1

def p_MAIN(p):
    'main : BEGIN est END'

def p_VARIABLES(p):
    ''' var : v
            | var v
    '''

def p_RUTINAS(p):
    ''' rut : r
            | rut r
            | empty
    '''
    global indiceCuadruplos
    if len(cuadruplos[0]) > 1:
        cuadruplos[0].pop(1)
    cuadruplos[0].insert(1, '$%d' %indiceCuadruplos)

def p_EMPTY(p):
    'empty :'
    pass

def p_TIPOS_VARIABLES(p):
    ''' v   : INTEGER c1
            | REAL c1
    '''

def p_VARIABLES_SIMPLES(p):
    ''' c1  : ID
            | c1 ',' ID
    '''
    global indiceSimbolos
    if len(p) == 2:
        simbolos[indiceSimbolos] = [p[1], p[-1]]
        indiceSimbolos += 1
    elif len(p) == 4:
        simbolos[indiceSimbolos] = [p[3], p[-1]]
        indiceSimbolos += 1

# direccion   id   tipo   col   dim   base   valor
# 0           0    1      2     3     4      5      
def p_VARIABLES_UNA_DIMENSION(p):
    ''' c1  : ID LPAREN CTE RPAREN
            | c1 ',' ID LPAREN CTE RPAREN
    '''
    global indiceSimDim
    base = indiceSimDim
    if len(p) == 5:
        for x in range(int(p[3])):
            simbolosDim[indiceSimDim] = [p[1], p[-1], x, p[3], base]
            indiceSimDim += 1
    elif len(p) == 7:
        for x in range(int(p[5])):
            simbolosDim[indiceSimDim] = [p[3], p[-1], x, p[5], base]
            indiceSimDim += 1
    
# direccion   id   tipo   ren   col   dimR   dimC   base   valor
# 0           0    1      2     3     4      5      6      7
def p_VARIABLES_DOS_DIMENSIONES(p):
    ''' c1  : ID LPAREN CTE ',' CTE RPAREN
            | c1 ',' ID LPAREN CTE ',' CTE RPAREN
    '''
    global indiceSimDim
    base = indiceSimDim
    if len(p) == 7:
        for x in range(int(p[3])):
            for y in range(int(p[5])):
                simbolosDim[indiceSimDim] = [p[1], p[-1], x, y, p[3], p[5], base]
                indiceSimDim += 1
    elif len(p) == 9:
        for x in range(int(p[5])):
            for y in range(int(p[7])):
                simbolosDim[indiceSimDim] = [p[3], p[-1], x, y, p[5], p[7], base]
                indiceSimDim += 1

def p_SUBRUTINAS(p):
    'r : SUBROUTINE idsubroutine est END'
    global indiceCuadruplos
    cuadruplos[indiceCuadruplos] = ['return']
    indiceCuadruplos += 1

def p_ID_SUBRUTINA(p):
    'idsubroutine : ID'
    global indiceSubrutinas, indiceCuadruplos, error
    if p[1] in subrutinas.values():
        error = True
        print("SUBRUTINA EXISTENTE")
    else:
        subrutinas[indiceSubrutinas] = [p[1], indiceCuadruplos]
        indiceSubrutinas += 1

def p_ESTATUTO_CALL(p):
    's : CALL ID LPAREN RPAREN'
    global indiceCuadruplos
    cuadruplos[indiceCuadruplos] = ['gotoS', p[2]]
    indiceCuadruplos += 1

def p_REPETICION_SUBRUTINAS(p):
    ''' est : s
            | est s
    '''

def p_ID_UNA_DIMENSION(p):
    'iddim : pushid LPAREN ea RPAREN'
    global indiceOperandos, indiceCuadruplos, indiceTemporales, indiceSimbolos
    for x in simbolosDim.values():
        if x[0] == p[1]:
            dimC = int(x[3])
            base = int(x[4])
    Si = pilaOperandos[indiceOperandos-1]
    indiceOperandos -= 1    
    cuadruplos[indiceCuadruplos] = ['ver', Si, 0, (dimC-1)]
    indiceCuadruplos += 1
    Temp = "V" + str(indiceTemporales)
    indiceTemporales += 1
    simbolos[indiceSimbolos] = [Temp, 'integer']
    indiceSimbolos += 1
    cuadruplos[indiceCuadruplos] = ['+', Si, base, Temp]
    indiceCuadruplos += 1
    pilaOperandos[indiceOperandos] = Temp
    indiceOperandos += 1
    p[0] = p[1]

def p_ID_DOS_DIMENSIONES(p):
    'iddim : pushid LPAREN ea "," ea RPAREN'
    global indiceOperandos, indiceCuadruplos, indiceTemporales, indiceSimbolos
    for x in simbolosDim.values():
        if x[0] == p[1]:
            dimR = int(x[4])
            dimC = int(x[5])
            base = int(x[6])
    SiC = pilaOperandos[indiceOperandos-1]
    indiceOperandos -= 1 
    SiR = pilaOperandos[indiceOperandos-1]
    indiceOperandos -= 1 
    cuadruplos[indiceCuadruplos] = ['ver', SiR, 0, (dimR-1)]
    indiceCuadruplos += 1
    TempR = "T" + str(indiceTemporales)
    indiceTemporales += 1
    simbolos[indiceSimbolos] = [TempR, 'integer']
    indiceSimbolos += 1
    cuadruplos[indiceCuadruplos] = ['*', SiR, dimC, TempR]
    indiceCuadruplos += 1
    cuadruplos[indiceCuadruplos] = ['ver', SiC, 0, (dimC-1)]
    indiceCuadruplos += 1
    TempC = "T" + str(indiceTemporales)
    indiceTemporales += 1
    simbolos[indiceSimbolos] = [TempC, 'integer']
    indiceSimbolos += 1
    cuadruplos[indiceCuadruplos] = ['+', TempR, SiC, TempC]
    indiceCuadruplos += 1
    Temp = "V" + str(indiceTemporales)
    indiceTemporales += 1
    simbolos[indiceSimbolos] = [Temp, 'integer']
    indiceSimbolos += 1
    cuadruplos[indiceCuadruplos] = ['+', TempC, base, Temp]
    indiceCuadruplos += 1
    pilaOperandos[indiceOperandos] = Temp
    indiceOperandos += 1
    p[0] = p[1]

def p_PRINT_ID_UNA_DIMENSION(p):
    'printiddim : pushid LPAREN ea RPAREN'
    global indiceOperandos, indiceCuadruplos, indiceTemporales, indiceSimbolos
    for x in simbolosDim.values():
        if x[0] == p[1]:
            dimC = int(x[3])
            base = int(x[4])
    Si = pilaOperandos[indiceOperandos-1]
    indiceOperandos -= 1    
    cuadruplos[indiceCuadruplos] = ['ver', Si, 0, (dimC-1)]
    indiceCuadruplos += 1
    Temp = "V" + str(indiceTemporales)
    indiceTemporales += 1
    simbolos[indiceSimbolos] = [Temp, 'integer']
    indiceSimbolos += 1
    cuadruplos[indiceCuadruplos] = ['+', Si, base, Temp]
    indiceCuadruplos += 1
    p[0] = Temp

def p_PRINT_ID_DOS_DIMENSIONES(p):
    'printiddim : pushid LPAREN ea "," ea RPAREN'
    global indiceOperandos, indiceCuadruplos, indiceTemporales, indiceSimbolos
    for x in simbolosDim.values():
        if x[0] == p[1]:
            dimR = int(x[4])
            dimC = int(x[5])
            base = int(x[6])
    SiC = pilaOperandos[indiceOperandos-1]
    indiceOperandos -= 1 
    SiR = pilaOperandos[indiceOperandos-1]
    indiceOperandos -= 1 
    cuadruplos[indiceCuadruplos] = ['ver', SiR, 0, (dimR-1)]
    indiceCuadruplos += 1
    TempR = "T" + str(indiceTemporales)
    indiceTemporales += 1
    simbolos[indiceSimbolos] = [TempR, 'integer']
    indiceSimbolos += 1
    cuadruplos[indiceCuadruplos] = ['*', SiR, dimC, TempR]
    indiceCuadruplos += 1
    cuadruplos[indiceCuadruplos] = ['ver', SiC, 0, (dimC-1)]
    indiceCuadruplos += 1
    TempC = "T" + str(indiceTemporales)
    indiceTemporales += 1
    simbolos[indiceSimbolos] = [TempC, 'integer']
    indiceSimbolos += 1
    cuadruplos[indiceCuadruplos] = ['+', TempR, SiC, TempC]
    indiceCuadruplos += 1
    Temp = "V" + str(indiceTemporales)
    indiceTemporales += 1
    simbolos[indiceSimbolos] = [Temp, 'integer']
    indiceSimbolos += 1
    cuadruplos[indiceCuadruplos] = ['+', TempC, base, Temp]
    indiceCuadruplos += 1
    p[0] = Temp

def p_ESTATUTO_ASIGNACION_SIMPLE(p):
    's : pushid "=" ea'
    global indiceOperandos, indiceCuadruplos
    res = pilaOperandos[indiceOperandos-1]
    indiceOperandos -= 1
    op1 = pilaOperandos[indiceOperandos-1]
    indiceOperandos -= 1
    cuadruplos[indiceCuadruplos] = ['=', res, op1]
    indiceCuadruplos += 1

def p_ESTATUTO_ASIGNACION_DIMENSIONADA(p):
    's : iddim "=" ea'
    global indiceOperandos, indiceCuadruplos
    res = pilaOperandos[indiceOperandos-1]
    indiceOperandos -= 1
    op1 = pilaOperandos[indiceOperandos-1]
    indiceOperandos -= 1
    cuadruplos[indiceCuadruplos] = ['=', res, op1]
    indiceCuadruplos += 1

def p_PUSH_ID(p):
    'pushid : ID'
    global indiceOperandos, error
    existe = False
    for x in simbolos.items():
        if x[1][0] == p[1]:
            # pilaOperandos[indiceOperandos] = '$%d' %x[0]       # guarda dirección de id
            pilaOperandos[indiceOperandos] = p[1]       # guarda id
            indiceOperandos += 1
            existe = True
            p[0] = p[1]
    for x in simbolosDim.items():
        if x[1][0] == p[1] and not existe:
            existe = True
            p[0] = p[1]
    if not(existe):
        error = True
        print("La variable '%s' no está declarada en la tabla de símbolos" % p[1])
        
def p_ESTATUTO_READ(p):
    's : READ FLECHA ID'
    global indiceCuadruplos, error
    existe = False
    for x in simbolos.items():
        if x[1][0] == p[3]:
            # cuadruplos[indiceCuadruplos] = ['STORE', '$%s' %x[0]] # direccion de memoria
            cuadruplos[indiceCuadruplos] = ['STORE', p[3]]  # id de subrutina
            indiceCuadruplos += 1
            existe = True
    if not(existe):
        error = True
        print("La variable '%s' no está declarada en la tabla de símbolos" % p[3])

def p_ESTATUTO_PRINT(p):
    ''' s  	: PRINT FLECHA TEXT
            | PRINT FLECHA checkid'''
    global indiceCuadruplos, indiceOperandos
    cuadruplos[indiceCuadruplos] = ['PRINT', p[3]]
    indiceCuadruplos += 1

def p_CHECK_ID(p):
    '''checkid  : ID
                | printiddim'''
    global error
    existe = False
    for x in simbolos.items():
        if x[1][0] == p[1]:
            existe = True
            p[0] = p[1]
    for x in simbolosDim.items():
        if x[1][0] == p[1] and not existe:
            existe = True
            p[0] = p[1]
    if not(existe):
        error = True
        print("La variable '%s' no está declarada en la tabla de símbolos" % p[1])

def p_ESTATUTO_IF(p):
    ''' s  	: estif c2 estelse est END
            | estif c2 END
    '''
    global indiceCuadruplos, indiceSaltos
    while pilaSaltos[indiceSaltos-1] != '&':
        address = pilaSaltos[indiceSaltos-1]
        indiceSaltos -= 1
        cuadruplos[address].insert(2, '$%d' % indiceCuadruplos)
    indiceSaltos -= 1

def p_IF(p):
    'estif : IF'
    global indiceSaltos
    pilaSaltos[indiceSaltos] = '&'
    indiceSaltos += 1

def p_IF_EL_THEN(p):
    'c2 : el estthen est'

def p_IF_ELSIF_EST(p):
    'c2 : c2 estelsif el estthen est'

def p_THEN(p):
    'estthen : THEN'
    global indiceOperandos, indiceCuadruplos, indiceSaltos
    op1 = pilaOperandos[indiceOperandos-1]
    indiceOperandos -= 1
    cuadruplos[indiceCuadruplos] = ['gotoF', op1]
    pilaSaltos[indiceSaltos] = indiceCuadruplos
    indiceCuadruplos += 1
    indiceSaltos += 1

def p_ELSE(p):
    'estelse : ELSE'
    global indiceCuadruplos, indiceSaltos
    address = pilaSaltos[indiceSaltos-1]
    indiceSaltos -= 1
    cuadruplos[indiceCuadruplos] = ['goto']
    pilaSaltos[indiceSaltos] = indiceCuadruplos
    indiceCuadruplos += 1
    indiceSaltos += 1
    cuadruplos[address].insert(2, '$%d' % indiceCuadruplos)

def p_ELSIF(p):
    'estelsif : ELSIF'
    global indiceCuadruplos, indiceSaltos
    address = pilaSaltos[indiceSaltos-1]
    indiceSaltos -= 1
    cuadruplos[indiceCuadruplos] = ['goto']
    pilaSaltos[indiceSaltos] = indiceCuadruplos
    indiceCuadruplos += 1
    indiceSaltos += 1
    cuadruplos[address].insert(2, '$%d' % indiceCuadruplos)

def p_ESTATUTO_DO_INCREMENT(p):
    's : DO pushid "=" first "," last "," increment est END'
    global indiceOperandos, indiceSaltos, indiceCuadruplos
    increment = pilaOperandos[indiceOperandos-1]
    indiceOperandos -= 1
    op = pilaOperandos[indiceOperandos-1]
    indiceOperandos -= 1
    cuadruplos[indiceCuadruplos] = ['+', op, increment, op]
    indiceCuadruplos += 1
    address = pilaSaltos[indiceSaltos-1]
    indiceSaltos -= 1
    cuadruplos[indiceCuadruplos] = ['goto', '$%d' % address]
    indiceCuadruplos += 1
    cuadruplos[address+1].insert(2, '$%d' % indiceCuadruplos)

def p_ESTATUTO_DO_FOR(p):
    's : DO pushid "=" first "," last est END'
    global indiceOperandos, indiceSaltos, indiceCuadruplos
    op = pilaOperandos[indiceOperandos-1]
    indiceOperandos -= 1
    cuadruplos[indiceCuadruplos] = ['+', op, 1, op]
    indiceCuadruplos += 1
    address = pilaSaltos[indiceSaltos-1]
    indiceSaltos -= 1
    cuadruplos[indiceCuadruplos] = ['goto', '$%d' % address]
    indiceCuadruplos += 1
    cuadruplos[address+1].insert(2, '$%d' % indiceCuadruplos)

def p_ESTATUTO_DO_WHILE(p):
    's : estdo est END'
    global indiceSaltos, indiceCuadruplos, indiceExit
    address = pilaSaltos[indiceSaltos-1]
    indiceSaltos -= 1
    cuadruplos[indiceCuadruplos] = ['goto', '$%d' % address]
    indiceCuadruplos += 1
    while pilaExit[indiceExit-1] != '#':
        address = pilaExit[indiceExit-1]
        indiceExit -= 1
        cuadruplos[address].insert(1, '$%d' % indiceCuadruplos)
    indiceExit -= 1

def p_ESTATUTO_EXIT(p):
    's : EXIT'
    global indiceExit, indiceCuadruplos
    pilaExit[indiceExit] = indiceCuadruplos
    indiceExit += 1
    cuadruplos[indiceCuadruplos] = ['goto']
    indiceCuadruplos += 1

def p_DO(p):
    'estdo : DO'
    global indiceSaltos, indiceCuadruplos, indiceExit
    pilaSaltos[indiceSaltos] = indiceCuadruplos
    indiceSaltos += 1
    pilaExit[indiceExit] = '#'
    indiceExit += 1

def p_DO_FIRST(p):
    'first : cteid'
    global indiceOperandos, indiceCuadruplos
    res = pilaOperandos[indiceOperandos-1]
    indiceOperandos -= 1
    op1 = pilaOperandos[indiceOperandos-1]
    cuadruplos[indiceCuadruplos] = ['=', res, op1]
    indiceCuadruplos += 1

def p_DO_CTE(p):
    'cteid : pushid'

def p_DO_ID(p):
    'cteid : pushcte'

def p_DO_LAST(p):
    'last : cteid'
    global indiceOperandos, indiceCuadruplos, indiceSaltos, indiceTemporales, indiceSimbolos
    pilaSaltos[indiceSaltos] = indiceCuadruplos
    indiceSaltos += 1
    op2 = pilaOperandos[indiceOperandos-1]
    indiceOperandos -= 1
    op1 = pilaOperandos[indiceOperandos-1]
    Temp = "T" + str(indiceTemporales)
    indiceTemporales += 1
    simbolos[indiceSimbolos] = [Temp, 'bool']
    indiceSimbolos += 1
    cuadruplos[indiceCuadruplos] = ['<=', op1, op2, Temp]
    indiceCuadruplos += 1
    cuadruplos[indiceCuadruplos] = ['gotoF', Temp]
    indiceCuadruplos += 1

def p_DO_INCREMENT(p):
    'increment : pushcte'

def p_EXPRESIONES_ARITMETICAS(p):
    'ea : md'
    p[0] = p[1]

def p_EA_SUMA(p):
    'ea : ea hacesuma md'
    global indiceOperadores, indiceOperandos, indiceCuadruplos, indiceTemporales, indiceSimbolos
    if pilaOperadores[indiceOperadores-1] == '+':
        indiceOperadores -= 1
        op2 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        op1 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        Temp = "T" + str(indiceTemporales)
        indiceTemporales += 1
        simbolos[indiceSimbolos] = [Temp, 'integer']
        indiceSimbolos += 1
        cuadruplos[indiceCuadruplos] = ['+', op1, op2, Temp]
        indiceCuadruplos += 1
        pilaOperandos[indiceOperandos] = Temp
        indiceOperandos += 1

def p_SUMA(p):
    'hacesuma : SUMA'
    global indiceOperadores
    pilaOperadores[indiceOperadores] = p[1]
    indiceOperadores += 1

def p_EA_RESTA(p):
    'ea : ea haceresta md'
    global indiceOperadores, indiceOperandos, indiceCuadruplos, indiceTemporales, indiceSimbolos
    if pilaOperadores[indiceOperadores-1] == '-':
        indiceOperadores -= 1
        op2 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        op1 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        Temp = "T" + str(indiceTemporales)
        indiceTemporales += 1
        simbolos[indiceSimbolos] = [Temp, 'integer']
        indiceSimbolos += 1
        cuadruplos[indiceCuadruplos] = ['-', op1, op2, Temp]
        indiceCuadruplos += 1
        pilaOperandos[indiceOperandos] = Temp
        indiceOperandos += 1

def p_RESTA(p):
    'haceresta : RESTA'
    global indiceOperadores
    pilaOperadores[indiceOperadores] = p[1]
    indiceOperadores += 1

def p_EA_MD(p):
    'md : exp'
    p[0] = p[1]

def p_EA_MULTIPLICACION(p):
    'md : md hacemult exp'
    global indiceOperadores, indiceCuadruplos, indiceTemporales, indiceOperandos, indiceSimbolos
    if pilaOperadores[indiceOperadores-1] == '*':
        indiceOperadores -= 1
        op2 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        op1 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        Temp = "T" + str(indiceTemporales)
        indiceTemporales += 1
        simbolos[indiceSimbolos] = [Temp, 'integer']
        indiceSimbolos += 1
        cuadruplos[indiceCuadruplos] = ['*', op1, op2, Temp]
        indiceCuadruplos += 1
        pilaOperandos[indiceOperandos] = Temp
        indiceOperandos += 1

def p_MULTIPLICACION(p):
    'hacemult : MULTIPLICACION'
    global indiceOperadores
    pilaOperadores[indiceOperadores] = p[1]
    indiceOperadores += 1

def p_EA_DIVISION(p):
    'md : md hacediv exp'
    global indiceOperadores, indiceCuadruplos, indiceTemporales, indiceOperandos, indiceSimbolos
    if pilaOperadores[indiceOperadores-1] == '/':
        indiceOperadores -= 1
        op2 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        op1 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        Temp = "T" + str(indiceTemporales)
        indiceTemporales += 1
        simbolos[indiceSimbolos] = [Temp, 'integer']
        indiceSimbolos += 1
        cuadruplos[indiceCuadruplos] = ['/', op1, op2, Temp]
        indiceCuadruplos += 1
        pilaOperandos[indiceOperandos] = Temp
        indiceOperandos += 1

def p_DIVISION(p):
    'hacediv : DIVISION'
    global indiceOperadores
    pilaOperadores[indiceOperadores] = p[1]
    indiceOperadores += 1

def p_EXPRESION_ID(p):
    ''' exp	: pushid
            | iddim
    '''
    p[0] = p[1]
    
def p_EXPRESION_CTE(p):
    'exp : pushcte'
    p[0] = p[1]

def p_PUSH_CTE(p):
    'pushcte : CTE'
    global indiceOperandos
    pilaOperandos[indiceOperandos] = p[1]
    indiceOperandos += 1
    p[0] = p[1]

def p_EXPRESION_PARENTESIS(p):
    'exp : parenIZQ ea parenDER'
    p[0] = p[2]

def p_PARENTESIS_IZQUIERDO(p):
    'parenIZQ : LPAREN'
    global indiceOperadores
    pilaOperadores[indiceOperadores] = p[1]
    indiceOperadores += 1

def p_PARENTESIS_DERECHO(p):
    'parenDER : RPAREN'
    global indiceOperadores
    indiceOperadores -= 1

def p_EL(p):
    'el : eand'

def p_EL_OR(p):
    'el : el haceor eand'
    global indiceOperadores, indiceOperandos, indiceCuadruplos, indiceTemporales, indiceSimbolos
    if pilaOperadores[indiceOperadores-1] == 'or':
        indiceOperadores -= 1
        op2 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        op1 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        Temp = "T" + str(indiceTemporales)
        indiceTemporales += 1
        simbolos[indiceSimbolos] = [Temp, 'bool']
        indiceSimbolos += 1
        cuadruplos[indiceCuadruplos] = ['or', op1, op2, Temp]
        indiceCuadruplos += 1
        pilaOperandos[indiceOperandos] = Temp
        indiceOperandos += 1

def p_OR(p):
    'haceor : OR'
    global indiceOperadores
    pilaOperadores[indiceOperadores] = p[1]
    indiceOperadores += 1

def p_EAND(p):
    'eand : enot'

def p_EL_AND(p):
    'eand : eand haceand enot'
    global indiceOperadores, indiceCuadruplos, indiceTemporales, indiceOperandos, indiceSimbolos
    if pilaOperadores[indiceOperadores-1] == 'and':
        indiceOperadores -= 1
        op2 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        op1 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        Temp = "T" + str(indiceTemporales)
        indiceTemporales += 1
        simbolos[indiceSimbolos] = [Temp, 'bool']
        indiceSimbolos += 1
        cuadruplos[indiceCuadruplos] = ['and', op1, op2, Temp]
        indiceCuadruplos += 1
        pilaOperandos[indiceOperandos] = Temp
        indiceOperandos += 1

def p_AND(p):
    'haceand : AND'
    global indiceOperadores
    pilaOperadores[indiceOperadores] = p[1]
    indiceOperadores += 1

def p_ENOT(p):
    'enot : er'

def p_EL_NOT(p):
    'enot : hacenot pushid'
    global indiceOperadores, indiceCuadruplos, indiceTemporales, indiceOperandos, indiceSimbolos
    if pilaOperadores[indiceOperadores-1] == 'not':
        indiceOperadores -= 1
        op1 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        Temp = "T" + str(indiceTemporales)
        indiceTemporales += 1
        simbolos[indiceSimbolos] = [Temp, 'bool']
        indiceSimbolos += 1
        cuadruplos[indiceCuadruplos] = ['not', op1, Temp]
        indiceCuadruplos += 1
        pilaOperandos[indiceOperandos] = Temp
        indiceOperandos += 1

def p_NOT(p):
    'hacenot : NOT'
    global indiceOperadores
    pilaOperadores[indiceOperadores] = p[1]
    indiceOperadores += 1

def p_ER_PARENTESIS(p):
    'er : parenIZQ el parenDER'

def p_ER_MENOR(p):
    'er : c4 hacemenor c5'
    global indiceOperadores, indiceCuadruplos, indiceTemporales, indiceOperandos, indiceSimbolos
    if pilaOperadores[indiceOperadores-1] == '<':
        indiceOperadores -= 1
        op2 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        op1 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        Temp = "T" + str(indiceTemporales)
        indiceTemporales += 1
        simbolos[indiceSimbolos] = [Temp, 'bool']
        indiceSimbolos += 1
        cuadruplos[indiceCuadruplos] = ['<', op1, op2, Temp]
        indiceCuadruplos += 1
        pilaOperandos[indiceOperandos] = Temp
        indiceOperandos += 1

def p_MENOR(p):
    'hacemenor : MENOR'
    global indiceOperadores
    pilaOperadores[indiceOperadores] = p[1]
    indiceOperadores += 1

def p_ER_MENOR_IGUAL(p):
    'er : c4 hacemenorigual c5'
    global indiceOperadores, indiceCuadruplos, indiceTemporales, indiceOperandos, indiceSimbolos
    if pilaOperadores[indiceOperadores-1] == '<=':
        indiceOperadores -= 1
        op2 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        op1 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        Temp = "T" + str(indiceTemporales)
        indiceTemporales += 1
        simbolos[indiceSimbolos] = [Temp, 'bool']
        indiceSimbolos += 1
        cuadruplos[indiceCuadruplos] = ['<=', op1, op2, Temp]
        indiceCuadruplos += 1
        pilaOperandos[indiceOperandos] = Temp
        indiceOperandos += 1

def p_MENOR_IGUAL(p):
    'hacemenorigual : MENOR_IGUAL'
    global indiceOperadores
    pilaOperadores[indiceOperadores] = p[1]
    indiceOperadores += 1

def p_ER_MAYOR(p):
    'er : c4 hacemayor c5'
    global indiceOperadores, indiceCuadruplos, indiceTemporales, indiceOperandos, indiceSimbolos
    if pilaOperadores[indiceOperadores-1] == '>':
        indiceOperadores -= 1
        op2 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        op1 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        Temp = "T" + str(indiceTemporales)
        indiceTemporales += 1
        simbolos[indiceSimbolos] = [Temp, 'bool']
        indiceSimbolos += 1
        cuadruplos[indiceCuadruplos] = ['>', op1, op2, Temp]
        indiceCuadruplos += 1
        pilaOperandos[indiceOperandos] = Temp
        indiceOperandos += 1

def p_MAYOR(p):
    'hacemayor : MAYOR'
    global indiceOperadores
    pilaOperadores[indiceOperadores] = p[1]
    indiceOperadores += 1

def p_ER_MAYOR_IGUAL(p):
    'er : c4 hacemayorigual c5'
    global indiceOperadores, indiceCuadruplos, indiceTemporales, indiceOperandos, indiceSimbolos
    if pilaOperadores[indiceOperadores-1] == '>=':
        indiceOperadores -= 1
        op2 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        op1 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        Temp = "T" + str(indiceTemporales)
        indiceTemporales += 1
        simbolos[indiceSimbolos] = [Temp, 'bool']
        indiceSimbolos += 1
        cuadruplos[indiceCuadruplos] = ['>=', op1, op2, Temp]
        indiceCuadruplos += 1
        pilaOperandos[indiceOperandos] = Temp
        indiceOperandos += 1

def p_MAYOR_IGUAL(p):
    'hacemayorigual : MAYOR_IGUAL'
    global indiceOperadores
    pilaOperadores[indiceOperadores] = p[1]
    indiceOperadores += 1

def p_ER_IGUAL(p):
    'er : c4 haceigual c5'
    global indiceOperadores, indiceCuadruplos, indiceTemporales, indiceOperandos, indiceSimbolos
    if pilaOperadores[indiceOperadores-1] == '==':
        indiceOperadores -= 1
        op2 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        op1 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        Temp = "T" + str(indiceTemporales)
        indiceTemporales += 1
        simbolos[indiceSimbolos] = [Temp, 'bool']
        indiceSimbolos += 1
        cuadruplos[indiceCuadruplos] = ['EQ', op1, op2, Temp]
        indiceCuadruplos += 1
        pilaOperandos[indiceOperandos] = Temp
        indiceOperandos += 1

def p_IGUAL(p):
    'haceigual : IGUAL'
    global indiceOperadores
    pilaOperadores[indiceOperadores] = p[1]
    indiceOperadores += 1

def p_ER_DESIGUAL(p):
    'er : c4 hacedesigual c5'
    global indiceOperadores, indiceCuadruplos, indiceTemporales, indiceOperandos, indiceSimbolos
    if pilaOperadores[indiceOperadores-1] == '!=':
        indiceOperadores -= 1
        op2 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        op1 = pilaOperandos[indiceOperandos-1]
        indiceOperandos -= 1
        Temp = "T" + str(indiceTemporales)
        indiceTemporales += 1
        simbolos[indiceSimbolos] = [Temp, 'bool']
        indiceSimbolos += 1
        cuadruplos[indiceCuadruplos] = ['NEQ', op1, op2, Temp]
        indiceCuadruplos += 1
        pilaOperandos[indiceOperandos] = Temp
        indiceOperandos += 1

def p_DESIGUAL(p):
    'hacedesigual : DESIGUAL'
    global indiceOperadores
    pilaOperadores[indiceOperadores] = p[1]
    indiceOperadores += 1

def p_ID_DIM(p):
    ''' c4  : pushid
            | iddim
    '''

def p_ER_CTE_ID(p):
    ''' c5  : c4
            | pushcte
    '''

def p_error(p):
    print(p)
    global error
    error = True

# Build parser
parser = yacc.yacc()

# Program
f = open("programa1.txt").read()

while True:
    try:
        lexer.input(f)
        parser.parse(f)
        tok = lexer.token()
        if not tok:
            break
    except EOFError:
        break

if error:
    print("Syntax error :( -----------------------------------------------")
else:
    print("\n--------------------------------------------------CORRECT USE OF SYNTAX !! :D")
    if bool(subrutinas):
        print("\nSUBRUTINAS:")
        for x, y in subrutinas.items():
            print(x, y[0], y[1], sep='\t\t')
    if bool(simbolos):
        print("\nVARIABLES SIMPLES:")
        for x, y in simbolos.items():
            print(x, y[0], y[1], sep='\t\t') 
    if bool(simbolosDim):
        print("\nVARIABLES DIMENSIONADAS:")
        for x, y in simbolosDim.items():
            if len(y) == 5:
                print(x, y[0], y[1], y[2], y[3], y[4], sep='\t') 
            elif len(y) == 7:
                print(x, y[0], y[1], y[2], y[3], y[4], y[5], y[6], sep='\t') 
    print("\nCUADRUPLOS:")
    for x, y in cuadruplos.items():
        if len(y) == 1:
            print(x, y[0], sep='\t\t')
        elif len(y) == 2:
            print(x, y[0], y[1], sep='\t\t')
        elif len(y) == 3:
            print(x, y[0], y[1], y[2], sep='\t\t')
        elif len(y) == 4:
            print(x, y[0], y[1], y[2], y[3], sep='\t\t')
    print("\nINICIO EJECUCION:")

    while True:
        if cuadruplos[PC][0] == '=':			# asignacion
            address = 0
            value = 0
            if cuadruplos[PC][2][0] == 'V':
                for x in simbolos.items():
                    if x[1][0] == cuadruplos[PC][2]:
                        address = int(x[1][2])
                if len(simbolosDim[address]) == 8:
                    simbolosDim[address].pop(7)
                elif len(simbolosDim[address]) == 6:
                    simbolosDim[address].pop(5)
                if len(simbolosDim[address]) == 7:
                    simbolosDim[address].insert(7, cuadruplos[PC][1])   # matriz2dim = cte
                    for y in simbolos.items():
                        if y[1][0] == simbolosDim[address][7]:
                            if y[1][0][0] == 'V':                       # matriz2dim = matriz
                                simbolosDim[address].pop(7)
                                direccion = int(y[1][2])
                                simbolosDim[address].insert(7, simbolosDim[direccion][7])
                            else:                                       # matriz2dim = var
                                simbolosDim[address].pop(7)
                                simbolosDim[address].insert(7, y[1][2])
                elif len(simbolosDim[address]) == 5:
                    simbolosDim[address].insert(5, cuadruplos[PC][1])   # matriz1dim = cte
                    for y in simbolos.items():
                        if y[1][0] == simbolosDim[address][5]:
                            if y[1][0][0] == 'V':                       # matriz1dim = matriz
                                simbolosDim[address].pop(5)
                                direccion = int(y[1][2])
                                simbolosDim[address].insert(5, simbolosDim[direccion][5])
                            else:                                       # matriz1dim = var
                                simbolosDim[address].pop(5)
                                simbolosDim[address].insert(5, y[1][2])
            elif cuadruplos[PC][1][0] == 'V':       # var = matriz
                for x in simbolos.items():
                    if x[1][0] == cuadruplos[PC][1]:
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:
                            value = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:
                            value = simbolosDim[address][5]
                for x in simbolos.items():
                    if x[1][0] == cuadruplos[PC][2]:
                        x[1].insert(2, value)
            else:                                   # var = cte
                for x in simbolos.items():
                    if x[1][0] == cuadruplos[PC][2]:
                        if len(x[1]) > 2:
                            x[1].pop(2)
                        x[1].insert(2, cuadruplos[PC][1])
                        for y in simbolos.items():
                            if y[1][0] == x[1][2]:  # var = var
                                x[1].pop(2)
                                x[1].insert(2, y[1][2])
            PC += 1
        elif cuadruplos[PC][0] == 'STORE':		# read
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][1]:
                    if len(x[1]) > 2:
                        x[1].pop(2)
                    x[1].insert(2, input())
            PC += 1
        elif cuadruplos[PC][0] == 'PRINT':
            existe = False
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][1]:
                    if cuadruplos[PC][1][0] == 'V':
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:
                            value = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:
                            value = simbolosDim[address][5]
                        print(value)
                        existe = True
                    else:
                        print(x[1][2])
                        existe = True
            if not existe:
                print(cuadruplos[PC][1])
            PC += 1
        elif cuadruplos[PC][0] == 'return':		# end subroutine
            PC = pilaEjecucion[indiceEjecucion-1]
            indiceEjecucion -= 1
        elif cuadruplos[PC][0] == 'gotoS':		# call()
            pilaEjecucion[indiceEjecucion] = PC+1
            indiceEjecucion += 1
            for x in subrutinas.items():
                if x[1][0] == cuadruplos[PC][1]:
                    PC = x[1][1]
        elif cuadruplos[PC][0] == 'gotoF':
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][1]:
                    value = x[1][2]
            if value == False:
                PC = int(cuadruplos[PC][2][1:])
            else:
                PC += 1
        elif cuadruplos[PC][0] == 'goto':
            PC = int(cuadruplos[PC][1][1:])
        elif cuadruplos[PC][0] == 'ver':
            existe = False
            for x in simbolos.values():
                if x[0] == cuadruplos[PC][1]:
                    value = int(x[2])
                    existe = True
            if not existe:
                value = int(cuadruplos[PC][1])
            dim = int(cuadruplos[PC][3])
            if value > dim:
                print("Índice superior a la dimensión permitida por la matriz")
                break
            PC += 1
        elif cuadruplos[PC][0] == 'finprograma':
            break
        elif cuadruplos[PC][0] == '+':
            existeOp1 = False
            existeOp2 = False
            real = False
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][1]:
                    if x[1][0][0] == 'V': 
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op1 = mat2dim
                            op1 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op1 = mat1dim
                            op1 = simbolosDim[address][5]
                        if simbolosDim[address][1] == 'real':
                            real = True
                        existeOp1 = True
                    else:
                        op1 = x[1][2]                           # op1 = var
                        existeOp1 = True
                        if x[1][1] == 'real':
                            real = True
                if x[1][0] == cuadruplos[PC][2]:
                    if x[1][0][0] == 'V':
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op2 = mat2dim
                            op2 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op2 = mat1dim
                            op2 = simbolosDim[address][5]
                        if simbolosDim[address][1] == 'real':
                            real = True
                        existeOp2 = True
                    else: 
                        op2 = x[1][2]                           # op2 = var
                        existeOp2 = True
                        if x[1][1] == 'real':
                            real = True
            if not existeOp1:                                   # op1 = cte
                op1 = cuadruplos[PC][1]
            if not existeOp2:                                   # op2 = cte
                op2 = cuadruplos[PC][2]
            if real:
                res = float(op1) + float(op2)
            else:
                res = int(op1) + int(op2)
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][3]:
                    if len(x[1]) > 2:
                        x[1].pop(2)
                    x[1].insert(2, res)
            PC += 1
        elif cuadruplos[PC][0] == '-':
            existeOp1 = False
            existeOp2 = False
            real = False
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][1]:
                    if x[1][0][0] == 'V': 
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op1 = mat2dim
                            op1 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op1 = mat1dim
                            op1 = simbolosDim[address][5]
                        if simbolosDim[address][1] == 'real':
                            real = True
                        existeOp1 = True
                    else:
                        op1 = x[1][2]                           # op1 = var
                        existeOp1 = True
                        if x[1][1] == 'real':
                            real = True
                if x[1][0] == cuadruplos[PC][2]:
                    if x[1][0][0] == 'V':
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op2 = mat2dim
                            op2 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op2 = mat1dim
                            op2 = simbolosDim[address][5]
                        if simbolosDim[address][1] == 'real':
                            real = True
                        existeOp2 = True
                    else: 
                        op2 = x[1][2]                           # op2 = var
                        existeOp2 = True
                        if x[1][1] == 'real':
                            real = True
            if not existeOp1:                                   # op1 = cte
                op1 = cuadruplos[PC][1]
            if not existeOp2:                                   # op2 = cte
                op2 = cuadruplos[PC][2]
            if real:
                res = float(op1) - float(op2)
            else:
                res = int(op1) - int(op2)
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][3]:
                    if len(x[1]) > 2:
                        x[1].pop(2)
                    x[1].insert(2, res)
            PC += 1
        elif cuadruplos[PC][0] == '*':
            existeOp1 = False
            existeOp2 = False
            real = False
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][1]:
                    if x[1][0][0] == 'V': 
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op1 = mat2dim
                            op1 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op1 = mat1dim
                            op1 = simbolosDim[address][5]
                        if simbolosDim[address][1] == 'real':
                            real = True
                        existeOp1 = True
                    else:
                        op1 = x[1][2]                           # op1 = var
                        existeOp1 = True
                        if x[1][1] == 'real':
                            real = True
                if x[1][0] == cuadruplos[PC][2]:
                    if x[1][0][0] == 'V':
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op2 = mat2dim
                            op2 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op2 = mat1dim
                            op2 = simbolosDim[address][5]
                        if simbolosDim[address][1] == 'real':
                            real = True
                        existeOp2 = True
                    else: 
                        op2 = x[1][2]                           # op2 = var
                        existeOp2 = True
                        if x[1][1] == 'real':
                            real = True
            if not existeOp1:                                   # op1 = cte
                op1 = cuadruplos[PC][1]
            if not existeOp2:                                   # op2 = cte
                op2 = cuadruplos[PC][2]
            if real:
                res = float(op1) * float(op2)
            else:
                res = int(op1) * int(op2)
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][3]:
                    if len(x[1]) > 2:
                        x[1].pop(2)
                    x[1].insert(2, res)
            PC += 1
        elif cuadruplos[PC][0] == '/':
            existeOp1 = False
            existeOp2 = False
            real = False
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][1]:
                    if x[1][0][0] == 'V': 
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op1 = mat2dim
                            op1 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op1 = mat1dim
                            op1 = simbolosDim[address][5]
                        if simbolosDim[address][1] == 'real':
                            real = True
                        existeOp1 = True
                    else:
                        op1 = x[1][2]                           # op1 = var
                        existeOp1 = True
                        if x[1][1] == 'real':
                            real = True
                if x[1][0] == cuadruplos[PC][2]:
                    if x[1][0][0] == 'V':
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op2 = mat2dim
                            op2 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op2 = mat1dim
                            op2 = simbolosDim[address][5]
                        if simbolosDim[address][1] == 'real':
                            real = True
                        existeOp2 = True
                    else: 
                        op2 = x[1][2]                           # op2 = var
                        existeOp2 = True
                        if x[1][1] == 'real':
                            real = True
            if not existeOp1:                                   # op1 = cte
                op1 = cuadruplos[PC][1]
            if not existeOp2:                                   # op2 = cte
                op2 = cuadruplos[PC][2]
            if real:
                res = float(op1) / float(op2)
            else:
                res = int(op1) / int(op2)
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][3]:
                    if len(x[1]) > 2:
                        x[1].pop(2)
                    x[1].insert(2, res)
            PC += 1
        elif cuadruplos[PC][0] == 'or':
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][1]:
                    op1 = x[1][2]
                elif x[1][0] == cuadruplos[PC][2]:
                    op2 = x[1][2]
                elif x[1][0] == cuadruplos[PC][3]:
                    if len(x[1]) > 2:
                        x[1].pop(2)
                    x[1].insert(2, (op1 or op2))
            PC += 1
        elif cuadruplos[PC][0] == 'and':
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][1]:
                    op1 = x[1][2]
                elif x[1][0] == cuadruplos[PC][2]:
                    op2 = x[1][2]
                elif x[1][0] == cuadruplos[PC][3]:
                    if len(x[1]) > 2:
                        x[1].pop(2)
                    x[1].insert(2, (op1 and op2))
            PC += 1
        elif cuadruplos[PC][0] == 'not':
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][1]:
                    op1 = x[1][2]
                elif x[1][0] == cuadruplos[PC][2]:
                    if len(x[1]) > 2:
                        x[1].pop(2)
                    x[1].insert(2, not op1)
            PC += 1
        elif cuadruplos[PC][0] == '<':
            existe = False
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][1]:
                    if x[1][0][0] == 'V':
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op1 = mat2dim
                            op1 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op1 = mat1dim
                            op1 = simbolosDim[address][5]
                    else:
                        op1 = x[1][2]
                elif x[1][0] == cuadruplos[PC][2]:
                    if x[1][0][0] == 'V':
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op2 = mat2dim
                            op2 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op2 = mat1dim
                            op2 = simbolosDim[address][5]
                    else:
                        op2 = x[1][2]
                    existe = True
            if not existe:
                op2 = cuadruplos[PC][2]
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][3]:
                    if len(x[1]) > 2:
                        x[1].pop(2)
                    x[1].insert(2, (int(op1) < int(op2)))
            PC += 1
        elif cuadruplos[PC][0] == '<=':
            existe = False
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][1]:
                    if x[1][0][0] == 'V':
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op1 = mat2dim
                            op1 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op1 = mat1dim
                            op1 = simbolosDim[address][5]
                    else:
                        op1 = x[1][2]
                elif x[1][0] == cuadruplos[PC][2]:
                    if x[1][0][0] == 'V':
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op2 = mat2dim
                            op2 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op2 = mat1dim
                            op2 = simbolosDim[address][5]
                    else:
                        op2 = x[1][2]
                    existe = True
            if not existe:
                op2 = cuadruplos[PC][2]
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][3]:
                    if len(x[1]) > 2:
                        x[1].pop(2)
                    x[1].insert(2, (int(op1) <= int(op2)))
            PC += 1
        elif cuadruplos[PC][0] == '>':
            existe = False
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][1]:
                    if x[1][0][0] == 'V':
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op1 = mat2dim
                            op1 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op1 = mat1dim
                            op1 = simbolosDim[address][5]
                    else:
                        op1 = x[1][2]
                elif x[1][0] == cuadruplos[PC][2]:
                    if x[1][0][0] == 'V':
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op2 = mat2dim
                            op2 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op2 = mat1dim
                            op2 = simbolosDim[address][5]
                    else:
                        op2 = x[1][2]
                    existe = True
            if not existe:
                op2 = cuadruplos[PC][2]
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][3]:
                    if len(x[1]) > 2:
                        x[1].pop(2)
                    x[1].insert(2, (int(op1) > int(op2)))
            PC += 1
        elif cuadruplos[PC][0] == '>=':
            existe = False
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][1]:
                    if x[1][0][0] == 'V':
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op1 = mat2dim
                            op1 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op1 = mat1dim
                            op1 = simbolosDim[address][5]
                    else:
                        op1 = x[1][2]
                elif x[1][0] == cuadruplos[PC][2]:
                    if x[1][0][0] == 'V':
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op2 = mat2dim
                            op2 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op2 = mat1dim
                            op2 = simbolosDim[address][5]
                    else:
                        op2 = x[1][2]
                    existe = True
            if not existe:
                op2 = cuadruplos[PC][2]
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][3]:
                    if len(x[1]) > 2:
                        x[1].pop(2)
                    x[1].insert(2, (int(op1) >= int(op2)))
            PC += 1
        elif cuadruplos[PC][0] == 'EQ':
            existe = False
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][1]:
                    if x[1][0][0] == 'V':
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op1 = mat2dim
                            op1 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op1 = mat1dim
                            op1 = simbolosDim[address][5]
                    else:
                        op1 = x[1][2]
                elif x[1][0] == cuadruplos[PC][2]:
                    if x[1][0][0] == 'V':
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op2 = mat2dim
                            op2 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op2 = mat1dim
                            op2 = simbolosDim[address][5]
                    else:
                        op2 = x[1][2]
                    existe = True
            if not existe:
                op2 = cuadruplos[PC][2]
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][3]:
                    if len(x[1]) > 2:
                        x[1].pop(2)
                    x[1].insert(2, (int(op1) == int(op2)))
            PC += 1
        elif cuadruplos[PC][0] == 'NEQ':
            existe = False
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][1]:
                    if x[1][0][0] == 'V':
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op1 = mat2dim
                            op1 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op1 = mat1dim
                            op1 = simbolosDim[address][5]
                    else:
                        op1 = x[1][2]
                elif x[1][0] == cuadruplos[PC][2]:
                    if x[1][0][0] == 'V':
                        address = int(x[1][2])
                        if len(simbolosDim[address]) == 8:      # op2 = mat2dim
                            op2 = simbolosDim[address][7]
                        elif len(simbolosDim[address]) == 6:    # op2 = mat1dim
                            op2 = simbolosDim[address][5]
                    else:
                        op2 = x[1][2]
                    existe = True
            if not existe:
                op2 = cuadruplos[PC][2]
            for x in simbolos.items():
                if x[1][0] == cuadruplos[PC][3]:
                    if len(x[1]) > 2:
                        x[1].pop(2)
                    x[1].insert(2, (int(op1) != int(op2)))
            PC += 1
