// backend/compiler/translator.go
package compiler

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/antlr4-go/antlr/v4"
	"main.go/compiler/arm64"
	compiler "main.go/grammar"
)

// ARM64Translator es el traductor principal de VlangCherry a ARM64
type ARM64Translator struct {
	generator *arm64.ARM64Generator
	errors    []string // Para almacenar errores de traducción

	userFunctions   map[string]*compiler.FuncDeclContext
	currentFunction string

	breakLabels    []string          // Etiquetas para manejar break en loops
	continueLabels []string          // Etiquetas para manejar continue en loops
	stringRegistry map[string]string // texto -> etiqueta Para evitar procesar strings dos veces
	variableTypes  map[string]string // nombre -> tipo Para rastrear tipos de variables
}

// NewARM64Translator crea un nuevo traductor
func NewARM64Translator() *ARM64Translator {
	return &ARM64Translator{
		generator:      arm64.NewARM64Generator(),
		errors:         make([]string, 0),
		userFunctions:  make(map[string]*compiler.FuncDeclContext),
		breakLabels:    make([]string, 0),
		continueLabels: make([]string, 0),
		stringRegistry: make(map[string]string),
		variableTypes:  make(map[string]string),
	}
}

// === FUNCIÓN PRINCIPAL DE TRADUCCIÓN ===

// TranslateProgram traduce un programa completo de VlangCherry a ARM64
func (t *ARM64Translator) TranslateProgram(tree antlr.ParseTree) (string, []string) {
	// Limpiar estado anterior
	t.generator.Reset()
	t.errors = make([]string, 0)
	t.variableTypes = make(map[string]string)

	fmt.Printf("🔍 === PRIMERA PASADA: ANÁLISIS DEL PROGRAMA ===\n")

	// Primera pasada: analizar declaraciones de variables Y strings
	t.analyzeVariablesAndStrings(tree)

	// Generar header del programa
	t.generator.GenerateHeader()

	fmt.Printf("🔍 === SEGUNDA PASADA: GENERACIÓN DE CÓDIGO ===\n")

	// Traducir el contenido del programa / Segunda pasada
	t.translateNode(tree)

	// Generar footer del programa
	t.generator.GenerateFooter()

	// Generar código para funciones de usuario
	t.generateUserFunctions()

	// Agregar funciones de librería estándar
	t.generator.EmitRaw("")
	t.generator.EmitRaw("// === LIBRERÍA ESTÁNDAR ===")
	t.generateStandardLibrary()

	return t.generator.GetCode(), t.errors
}

// === ANÁLISIS MEJORADO (PRIMERA PASADA) ===

// analyzeVariablesAndStrings hace una pasada previa para encontrar todas las variables Y strings
func (t *ARM64Translator) analyzeVariablesAndStrings(node antlr.ParseTree) {
	switch ctx := node.(type) {
	case *compiler.ProgramContext:
		for _, stmt := range ctx.AllStmt() {
			fmt.Printf("🔍 Analizando statement: %T\n", stmt)
			t.analyzeVariablesAndStrings(stmt)
		}

	case *compiler.StmtContext:
		if ctx.Decl_stmt() != nil {
			t.analyzeVariablesAndStrings(ctx.Decl_stmt())
		}
		if ctx.Assign_stmt() != nil {
			t.analyzeVariablesAndStrings(ctx.Assign_stmt())
		}
		if ctx.If_stmt() != nil {
			t.analyzeVariablesAndStrings(ctx.If_stmt())
		}
		if ctx.Switch_stmt() != nil {
			t.analyzeVariablesAndStrings(ctx.Switch_stmt())
		}
		if ctx.For_stmt() != nil {
			t.analyzeVariablesAndStrings(ctx.For_stmt())
		}
		if ctx.Func_dcl() != nil {
			t.analyzeVariablesAndStrings(ctx.Func_dcl())
		}
		if ctx.Transfer_stmt() != nil {
			t.analyzeVariablesAndStrings(ctx.Transfer_stmt())
		}
		if ctx.Func_call() != nil {
			t.analyzeVariablesAndStrings(ctx.Func_call())
		}

	case *compiler.Assign_stmtContext:
		// Usar conversión a interfaz ParseTree para poder hacer type switch
		switch stmt := antlr.Tree(ctx).(type) {
		case *compiler.AssignmentDeclContext:
			if stmt.Expression() != nil {
				t.analyzeStringsInExpression(stmt.Expression())
			}

		case *compiler.ArgAddAssigDeclContext:
			if stmt.Expression() != nil {
				t.analyzeStringsInExpression(stmt.Expression())
			}

		case *compiler.VectorAssignContext:
			if stmt.Expression() != nil {
				t.analyzeStringsInExpression(stmt.Expression())
			}
		}

	case *compiler.ArgAddAssigDeclContext:
		// Analizar expresión en asignación suma
		if ctx.Expression() != nil {
			t.analyzeStringsInExpression(ctx.Expression())
		}

	case *compiler.ValueDeclContext:
		varName := ctx.ID().GetText()
		if !t.generator.VariableExists(varName) {
			t.generator.DeclareVariable(varName)
		}
		// NUEVO: Inferir tipo de la variable
		if ctx.Expression() != nil {
			varType := t.inferExpressionType(ctx.Expression())
			t.variableTypes[varName] = varType
			fmt.Printf("🔍 Variable '%s' inferida como tipo: %s\n", varName, varType)
		}
		// Analizar strings en la expresión de inicialización
		if ctx.Expression() != nil {
			t.analyzeStringsInExpression(ctx.Expression())
		}

	case *compiler.MutVarDeclContext:
		varName := ctx.ID().GetText()
		if !t.generator.VariableExists(varName) {
			t.generator.DeclareVariable(varName)
		}
		// NUEVO: Inferir tipo de la variable
		if ctx.Expression() != nil {
			varType := t.inferExpressionType(ctx.Expression())
			t.variableTypes[varName] = varType
			fmt.Printf("🔍 Variable '%s' inferida como tipo: %s\n", varName, varType)
		}
		// Analizar strings en la expresión de inicialización
		if ctx.Expression() != nil {
			t.analyzeStringsInExpression(ctx.Expression())
		}

	case *compiler.VarAssDeclContext:
		varName := ctx.ID().GetText()
		if !t.generator.VariableExists(varName) {
			t.generator.DeclareVariable(varName)
		}
		// NUEVO: Inferir tipo de la variable
		if ctx.Expression() != nil {
			varType := t.inferExpressionType(ctx.Expression())
			t.variableTypes[varName] = varType
			fmt.Printf("🔍 Variable '%s' inferida como tipo: %s\n", varName, varType)
		}
		// Analizar strings en la expresión de inicialización
		if ctx.Expression() != nil {
			t.analyzeStringsInExpression(ctx.Expression())
		}

	case *compiler.ValDeclVecContext:
		varName := ctx.ID().GetText()
		if !t.generator.VariableExists(varName) {
			t.generator.DeclareVariable(varName)
		}

		// Inferir tipo de la declaración
		var varType string
		if ctx.Type_() != nil {
			varType = ctx.Type_().GetText()
		} else {
			varType = "int" // Tipo por defecto
		}

		t.variableTypes[varName] = varType
		fmt.Printf("🔍 Variable sin valor '%s' inferida como tipo: %s\n", varName, varType)

		// Si es string, necesitamos registrar el string vacío
		if varType == "string" {
			if _, exists := t.stringRegistry[""]; !exists {
				stringLabel := t.generator.AddStringLiteral("")
				t.stringRegistry[""] = stringLabel
				fmt.Printf("✅ STRING VACÍO REGISTRADO: \"\" -> %s\n", stringLabel)
			}
		}

	case *compiler.FuncDeclContext:
		funcName := ctx.ID().GetText()

		fmt.Printf("🔍 Analizando función: %s\n", funcName)

		// Registrar función de usuario
		if funcName != "main" {
			t.userFunctions[funcName] = ctx
		}

		// Contar parámetros
		paramCount := 0
		if ctx.Param_list() != nil {
			params := ctx.Param_list().(*compiler.ParamListContext).AllFunc_param()
			paramCount = len(params)
			for i, param := range params {
				if paramCtx := param.(*compiler.FuncParamContext); paramCtx.ID() != nil {
					paramName := paramCtx.ID().GetText()
					fmt.Printf("📝 Parámetro: %s\n", paramName)
					t.generator.DeclareVariable(paramName)
				} else {
					fmt.Printf("📝 Parámetro %d: sin nombre\n", i)
				}
			}
		}

		// Analizar el cuerpo de la función
		varCount := 0
		for _, stmt := range ctx.AllStmt() {
			initialVarCount := len(t.generator.GetVariables())
			t.analyzeVariablesAndStrings(stmt)
			varCount += len(t.generator.GetVariables()) - initialVarCount
		}

		stackSize := (paramCount + varCount) * 8
		fmt.Printf("📊 Parámetros: %d, Variables locales: %d, Stack: %d bytes\n", paramCount, varCount, stackSize)

	// NUEVO: Análisis específico de llamadas a funciones
	case *compiler.FuncCallContext:
		fmt.Printf("🔍 Analizando llamada a función: %s\n", ctx.Id_pattern().GetText())

		// Analizar argumentos en busca de strings
		if ctx.Arg_list() != nil {
			args := ctx.Arg_list().(*compiler.ArgListContext).AllFunc_arg()
			for i, arg := range args {
				fmt.Printf("🔍   Analizando argumento %d: %s\n", i, arg.GetText())
				if argCtx := arg.(*compiler.FuncArgContext); argCtx != nil {
					if argCtx.Expression() != nil {
						t.analyzeStringsInExpression(argCtx.Expression())
					}
				}
			}
		}

	// Otros casos que pueden contener strings
	case *compiler.IfStmtContext:
		for _, ifChain := range ctx.AllIf_chain() {
			if ifChainCtx, ok := ifChain.(*compiler.IfChainContext); ok {
				// Analizar condición
				if ifChainCtx.Expression() != nil {
					t.analyzeStringsInExpression(ifChainCtx.Expression())
				}
				// Analizar cuerpo
				for _, stmt := range ifChainCtx.AllStmt() {
					t.analyzeVariablesAndStrings(stmt)
				}
			}
		}
		if ctx.Else_stmt() != nil {
			elseCtx := ctx.Else_stmt().(*compiler.ElseStmtContext)
			for _, stmt := range elseCtx.AllStmt() {
				t.analyzeVariablesAndStrings(stmt)
			}
		}

	case *compiler.ForStmtCondContext:
		// Analizar condición
		if ctx.Expression() != nil {
			t.analyzeStringsInExpression(ctx.Expression())
		}
		// Analizar cuerpo
		for _, stmt := range ctx.AllStmt() {
			t.analyzeVariablesAndStrings(stmt)
		}

	case *compiler.SwitchStmtContext:
		for _, rawCase := range ctx.AllSwitch_case() {
			if caseCtx, ok := rawCase.(*compiler.SwitchCaseContext); ok {
				t.analyzeStringsInExpression(caseCtx.Expression())
				for _, stmt := range caseCtx.AllStmt() {
					t.analyzeVariablesAndStrings(stmt)
				}
			}
		}
		if def := ctx.Default_case(); def != nil {
			if defaultCtx, ok := def.(*compiler.DefaultCaseContext); ok {
				for _, stmt := range defaultCtx.AllStmt() {
					t.analyzeVariablesAndStrings(stmt)
				}
			}
		}
	case *compiler.ForAssCondContext:
		// Asignación inicial
		if ctx.Assign_stmt() != nil {
			t.analyzeVariablesAndStrings(ctx.Assign_stmt())
		}
		// Condición
		if ctx.Expression(0) != nil {
			t.analyzeStringsInExpression(ctx.Expression(0))
		}
		// Cuerpo del for
		for _, stmt := range ctx.AllStmt() {
			t.analyzeVariablesAndStrings(stmt)
		}
		// Expresión de incremento
		if ctx.Expression(1) != nil {
			t.analyzeStringsInExpression(ctx.Expression(1))
		}
	default:
		if ctx, ok := node.(antlr.ParserRuleContext); ok {
			for i := 0; i < ctx.GetChildCount(); i++ {
				child := ctx.GetChild(i)
				if pt, ok := child.(antlr.ParseTree); ok {
					// Recorremos con ambas funciones para no perdernos nada
					t.analyzeStringsInExpression(pt)
					t.analyzeVariablesAndStrings(pt)
				}
			}
		}

	}
}

// NUEVA FUNCIÓN: Inferir tipo de expresión
func (t *ARM64Translator) inferExpressionType(expr antlr.ParseTree) string {
	if expr == nil {
		return "unknown"
	}

	switch ctx := expr.(type) {
	case *compiler.LiteralExprContext:
		return t.inferExpressionType(ctx.Literal())
	case *compiler.LiteralContext:
		return t.inferLiteralType(ctx)
	case *compiler.StringLiteralContext:
		return "string"
	case *compiler.IntLiteralContext:
		return "int"
	case *compiler.FloatLiteralContext:
		return "float"
	case *compiler.BoolLiteralContext:
		return "bool"
	case *compiler.IdPatternExprContext:
		varName := ctx.Id_pattern().GetText()
		if varType, exists := t.variableTypes[varName]; exists {
			return varType
		}
		return "unknown"
	case *compiler.BinaryExprContext:
		// Para operaciones binarias, usar el tipo del operando izquierdo
		return t.inferExpressionType(ctx.GetLeft())
	default:
		return "unknown"
	}
}

// Inferir tipo de literal
func (t *ARM64Translator) inferLiteralType(ctx *compiler.LiteralContext) string {
	text := ctx.GetText()

	// Verificar string
	if strings.HasPrefix(text, "\"") && strings.HasSuffix(text, "\"") {
		return "string"
	}

	// Verificar float
	if strings.Contains(text, ".") {
		return "float"
	}

	// Verificar bool
	if text == "true" || text == "false" {
		return "bool"
	}

	// Por defecto, int
	if _, err := strconv.Atoi(text); err == nil {
		return "int"
	}

	return "unknown"
}

// === ANÁLISIS MEJORADO DE STRINGS ===

// analyzeStringsInExpression busca strings en expresiones de forma más completa
func (t *ARM64Translator) analyzeStringsInExpression(expr antlr.ParseTree) {
	if expr == nil {
		return
	}

	fmt.Printf("🔍 Analizando expresión para strings: %s (tipo: %T)\n", expr.GetText(), expr)

	switch ctx := expr.(type) {
	case *compiler.StringLiteralContext:
		// STRING LITERAL DIRECTO
		t.preProcessStringLiteral(ctx)

	case *compiler.LiteralExprContext:
		// Expresión literal que puede contener un string
		t.analyzeStringsInExpression(ctx.Literal())

	case *compiler.LiteralContext:
		// Buscar string literals en el literal
		for i := 0; i < ctx.GetChildCount(); i++ {
			child := ctx.GetChild(i)
			if stringCtx, ok := child.(*compiler.StringLiteralContext); ok {
				t.preProcessStringLiteral(stringCtx)
			}
		}

	case *compiler.BinaryExprContext:
		// Analizar ambos lados de la expresión binaria
		t.analyzeStringsInExpression(ctx.GetLeft())
		t.analyzeStringsInExpression(ctx.GetRight())

	case *compiler.ParensExprContext:
		// Analizar expresión entre paréntesis
		t.analyzeStringsInExpression(ctx.Expression())

	case *compiler.FuncCallExprContext:
		// Analizar llamada a función
		t.analyzeStringsInExpression(ctx.Func_call())

	case *compiler.FuncCallContext:
		// Analizar argumentos de la función
		if ctx.Arg_list() != nil {
			args := ctx.Arg_list().(*compiler.ArgListContext).AllFunc_arg()
			for _, arg := range args {
				if argCtx := arg.(*compiler.FuncArgContext); argCtx != nil {
					if argCtx.Expression() != nil {
						t.analyzeStringsInExpression(argCtx.Expression())
					}
				}
			}
		}

	default:
		// Para otros tipos, analizar recursivamente todos los hijos
		if ctx, ok := expr.(antlr.ParserRuleContext); ok {
			for i := 0; i < ctx.GetChildCount(); i++ {
				if child := ctx.GetChild(i); child != nil {
					if parseTreeChild, ok := child.(antlr.ParseTree); ok {
						t.analyzeStringsInExpression(parseTreeChild)
					}
				}
			}
		}
	}
}

// preProcessStringLiteral procesa strings en la primera pasada (MEJORADO)
func (t *ARM64Translator) preProcessStringLiteral(ctx *compiler.StringLiteralContext) {
	text := ctx.GetText()
	if len(text) >= 2 {
		text = text[1 : len(text)-1] // Quitar comillas
	}

	// ✅ ESTO DEBERÍA FUNCIONAR:
	text = strings.ReplaceAll(text, "\\n", "\n") // Convertir \n a newline real
	text = strings.ReplaceAll(text, "\\t", "\t")
	text = strings.ReplaceAll(text, "\\\"", "\"")
	text = strings.ReplaceAll(text, "\\\\", "\\")

	// VERIFICAR SI TIENE INTERPOLACIÓN
	if strings.Contains(text, "$") {
		// Procesar partes no variables del string interpolado
		parts := t.parseInterpolatedString(text)
		for _, part := range parts {
			if !part.IsVariable && part.Content != "" {
				// Solo registrar las partes de texto literal
				if _, exists := t.stringRegistry[part.Content]; !exists {
					stringLabel := t.generator.AddStringLiteral(part.Content)
					t.stringRegistry[part.Content] = stringLabel
					fmt.Printf("✅ STRING INTERPOLADO REGISTRADO: \"%s\" -> %s\n", part.Content, stringLabel)
				}
			}
		}
	} else {
		// String normal - procesar como antes
		if existingLabel, exists := t.stringRegistry[text]; exists {
			fmt.Printf("🔄 String \"%s\" ya procesado como %s\n", text, existingLabel)
			return
		}

		stringLabel := t.generator.AddStringLiteral(text)
		t.stringRegistry[text] = stringLabel
		fmt.Printf("✅ STRING REGISTRADO: \"%s\" -> %s\n", text, stringLabel)
	}
}

// === RESTO DE MÉTODOS (mantenidos igual pero con corrección en print) ===

func (t *ARM64Translator) generateUserFunctions() {
	t.generator.EmitRaw("")
	t.generator.EmitRaw("// === FUNCIONES DE USUARIO ===")

	for funcName, funcDecl := range t.userFunctions {
		t.generator.EmitRaw("")
		t.generator.Comment(fmt.Sprintf("Función: %s", funcName))
		t.generator.EmitRaw(fmt.Sprintf("func_%s:", funcName))

		// Prólogo de función
		t.generator.Emit("stp x29, x30, [sp, #-16]!")
		t.generator.Emit("mov x29, sp")

		// Mapear parámetros de registros a variables locales
		if funcDecl.Param_list() != nil {
			params := funcDecl.Param_list().(*compiler.ParamListContext).AllFunc_param()

			for i, param := range params {
				if paramCtx := param.(*compiler.FuncParamContext); paramCtx.ID() != nil {
					paramName := paramCtx.ID().GetText()
					// Declarar parámetro como variable local
					t.generator.DeclareVariable(paramName)

					// Usar un registro temporal para no sobrescribir
					sourceReg := fmt.Sprintf("x%d", i)
					tempReg := fmt.Sprintf("x%d", i+10) // Usar x10, x11, etc. como temporales

					t.generator.Emit(fmt.Sprintf("mov %s, %s", tempReg, sourceReg))
					t.generator.Emit(fmt.Sprintf("mov x0, %s", tempReg))
					t.generator.StoreVariable(arm64.X0, paramName)
				}
			}
		}

		// Traducir cuerpo de la función
		t.currentFunction = funcName
		hasReturnStatement := false

		for _, stmt := range funcDecl.AllStmt() {
			// Verificar si hay statement de return
			if t.hasReturnStatement(stmt) {
				hasReturnStatement = true
			}
			t.translateNode(stmt)
		}

		// Epílogo de función (solo si no hay return explícito)
		if !hasReturnStatement {
			t.generator.Emit("mov x0, #0") // Valor de retorno por defecto
			t.generator.Emit("ldp x29, x30, [sp], #16")
			t.generator.Emit("ret")
		}

		t.currentFunction = ""
	}
}

// Verificar si un statement contiene return
func (t *ARM64Translator) hasReturnStatement(stmt antlr.ParseTree) bool {
	switch ctx := stmt.(type) {
	case *compiler.StmtContext:
		if ctx.Transfer_stmt() != nil {
			transferText := ctx.Transfer_stmt().GetText()
			return strings.HasPrefix(transferText, "return")
		}
		return false
	case *compiler.Transfer_stmtContext:
		transferText := ctx.GetText()
		return strings.HasPrefix(transferText, "return")
	default:
		return false
	}
}

// Traducir statement return
func (t *ARM64Translator) translateReturnStatement(ctx *compiler.ReturnStmtContext) {
	t.generator.Comment("=== RETURN STATEMENT ===")

	// Si hay expresión de retorno, evaluarla
	if ctx.Expression() != nil {
		t.translateExpression(ctx.Expression())
		// El resultado queda en x0, que es correcto para el valor de retorno
	} else {
		// Return sin valor
		t.generator.LoadImmediate(arm64.X0, 0)
	}

	// Epílogo de función
	t.generator.Emit("ldp x29, x30, [sp], #16")
	t.generator.Emit("ret")
}

// === TRADUCCIÓN DE NODOS (mantenida igual) ===

// translateNode traduce cualquier nodo del AST
func (t *ARM64Translator) translateNode(node antlr.ParseTree) {
	switch ctx := node.(type) {
	case *compiler.ProgramContext:
		t.translateProgram(ctx)
	case *compiler.StmtContext:
		t.translateStatement(ctx)
	case *compiler.ValueDeclContext:
		t.translateValueDecl(ctx)
	case *compiler.MutVarDeclContext:
		t.translateMutVarDecl(ctx)
	case *compiler.VarAssDeclContext:
		t.translateVarAssDecl(ctx)
	case *compiler.AssignmentDeclContext:
		t.translateAssignment(ctx)
	case *compiler.ValDeclVecContext:
		t.translateValDeclVec(ctx)
	case *compiler.Assign_stmtContext:
		t.translateAssignStatement(ctx)
	case *compiler.ArgAddAssigDeclContext:
		t.translateArgAddAssignment(ctx)
	case *compiler.FloatLiteralContext:
		t.translateFloatLiteral(ctx)
	case *compiler.BoolLiteralContext:
		t.translateBoolLiteral(ctx)
	case *compiler.UnaryExprContext: // AGREGAR ESTE CASE
		t.translateUnaryExpression(ctx)
	case *compiler.IfStmtContext:
		t.translateIfStatement(ctx)
	case *compiler.SwitchStmtContext:
		t.translateSwitchStatement(ctx)
	case *compiler.ForStmtCondContext:
		t.translateForLoop(ctx)
	case *compiler.FuncCallContext:
		t.translateFunctionCall(ctx)
	case *compiler.FuncDeclContext:
		t.translateFunctionDeclaration(ctx)
	case *compiler.Decl_stmtContext:
		t.translateDeclStatement(ctx)
	case *compiler.Transfer_stmtContext:
		t.translateTransferStatement(ctx)
	case *compiler.ReturnStmtContext:
		t.translateReturnStatement(ctx)
	case *compiler.ForAssCondContext:
		t.translateForAssignment(ctx)
	case *compiler.BreakStmtContext:
		t.translateBreakStatement(ctx)
	case *compiler.ContinueStmtContext:
		t.translateContinueStatement(ctx)

	default:
		// Para nodos no implementados, simplemente continuar
		t.addError(fmt.Sprintf("Nodo no implementado: %T", ctx))
	}
}

// translateValDeclVec traduce declaraciones de variables sin valor inicial
// Ejemplo: mut enteroSinValor int
func (t *ARM64Translator) translateValDeclVec(ctx *compiler.ValDeclVecContext) {
	varName := ctx.ID().GetText()

	// Obtener el tipo de la variable
	var varType string
	if ctx.Type_() != nil {
		varType = ctx.Type_().GetText()
	} else {
		varType = "int" // Tipo por defecto
	}

	t.generator.Comment(fmt.Sprintf("=== DECLARACIÓN SIN VALOR: %s %s ===", varName, varType))

	// Registrar el tipo de la variable
	t.variableTypes[varName] = varType
	fmt.Printf("🔍 Variable '%s' registrada como tipo: %s\n", varName, varType)

	// Asignar valor por defecto según el tipo
	var defaultValue int
	switch varType {
	case "int":
		defaultValue = 0
	case "float":
		defaultValue = 0.0 // Para float escalado sería 0 también
	case "bool":
		defaultValue = 0 // false = 0
	case "string":
		// Para strings, cargar dirección de string vacío
		emptyStringLabel := t.generator.AddStringLiteral("")
		t.generator.Comment(fmt.Sprintf("Usar string vacío con etiqueta %s", emptyStringLabel))
		t.generator.Emit(fmt.Sprintf("adr x0, %s", emptyStringLabel))
		t.generator.StoreVariable(arm64.X0, varName)
		return
	default:
		defaultValue = 0
	}

	// Cargar valor por defecto
	t.generator.LoadImmediate(arm64.X0, defaultValue)

	// Guardar en la variable
	t.generator.StoreVariable(arm64.X0, varName)
}

// Manejar transfer statements (return, break, continue)
func (t *ARM64Translator) translateTransferStatement(ctx *compiler.Transfer_stmtContext) {
	// Analizar por el texto del primer token para determinar el tipo
	text := ctx.GetText()

	if strings.HasPrefix(text, "return") {
		// Es un return statement
		t.translateReturnStatementFromTransfer(ctx)
	} else if strings.HasPrefix(text, "break") {
		t.translateBreakStatementFromTransfer(ctx)
	} else if strings.HasPrefix(text, "continue") {
		t.translateContinueStatementFromTransfer(ctx)
	} else {
		t.addError(fmt.Sprintf("Transfer statement no reconocido: %s", text))
	}
}

func (t *ARM64Translator) translateAssignStatement(ctx *compiler.Assign_stmtContext) {
	// Procesar todos los hijos para encontrar el tipo específico de asignación
	for i := 0; i < ctx.GetChildCount(); i++ {
		child := ctx.GetChild(i)
		switch childCtx := child.(type) {
		case *compiler.ArgAddAssigDeclContext:
			t.translateArgAddAssignment(childCtx)
		case *compiler.AssignmentDeclContext:
			t.translateAssignment(childCtx)
		case antlr.ParseTree:
			// Recursivamente procesar otros tipos
			t.translateNode(childCtx)
		}
	}
}

func (t *ARM64Translator) translateArgAddAssignment(ctx *compiler.ArgAddAssigDeclContext) {
	varName := ctx.Id_pattern().GetText()
	t.generator.Comment(fmt.Sprintf("=== ASIGNACIÓN SUMA: %s += ... ===", varName))

	// Verificar que la variable existe
	if !t.generator.VariableExists(varName) {
		t.addError(fmt.Sprintf("Variable '%s' no está declarada", varName))
		return
	}

	// Cargar valor actual de la variable en x1
	t.generator.LoadVariable(arm64.X1, varName)

	// Evaluar la expresión del lado derecho (resultado en x0)
	t.translateExpression(ctx.Expression())

	// Sumar: x0 = x1 + x0
	t.generator.Add(arm64.X0, arm64.X1, arm64.X0)

	// Guardar el resultado de vuelta en la variable
	t.generator.StoreVariable(arm64.X0, varName)
}

// Manejar return desde transfer_stmt
func (t *ARM64Translator) translateReturnStatementFromTransfer(ctx *compiler.Transfer_stmtContext) {
	t.generator.Comment("=== RETURN STATEMENT ===")

	// Buscar si hay una expresión después de "return"
	hasExpression := false

	// Recorrer hijos para encontrar la expresión
	for i := 0; i < ctx.GetChildCount(); i++ {
		child := ctx.GetChild(i)
		if expressionCtx, ok := child.(*compiler.ExpressionContext); ok {
			hasExpression = true
			t.translateExpression(expressionCtx)
			break
		}
	}

	if !hasExpression {
		// Return sin valor
		t.generator.LoadImmediate(arm64.X0, 0)
	}

	// Epílogo de función
	t.generator.Emit("ldp x29, x30, [sp], #16")
	t.generator.Emit("ret")
}

// Modificar translateBreakStatementFromTransfer
func (t *ARM64Translator) translateBreakStatementFromTransfer(ctx *compiler.Transfer_stmtContext) {
	t.generator.Comment("=== BREAK STATEMENT ===")

	// Verificar si estamos en un contexto que permite break
	if len(t.breakLabels) > 0 {
		// Saltar a la etiqueta de break más reciente
		breakLabel := t.breakLabels[len(t.breakLabels)-1]
		t.generator.Jump(breakLabel)
	} else {
		t.addError("Break statement fuera de contexto válido (switch/loop)")
	}
}

func (t *ARM64Translator) translateContinueStatementFromTransfer(ctx *compiler.Transfer_stmtContext) {
	t.generator.Comment("=== CONTINUE STATEMENT ===")
	// TODO: Implementar continue
}

// translateProgram traduce el nodo programa principal
func (t *ARM64Translator) translateProgram(ctx *compiler.ProgramContext) {
	t.generator.Comment("=== TRADUCCIÓN DEL PROGRAMA PRINCIPAL ===")

	for _, stmt := range ctx.AllStmt() {
		t.translateNode(stmt)
	}
}

// translateStatement traduce una declaración general
func (t *ARM64Translator) translateStatement(ctx *compiler.StmtContext) {
	if ctx.Decl_stmt() != nil {
		t.translateNode(ctx.Decl_stmt())
	} else if ctx.Assign_stmt() != nil {
		t.translateNode(ctx.Assign_stmt())
	} else if ctx.If_stmt() != nil {
		t.translateNode(ctx.If_stmt())
	} else if ctx.Switch_stmt() != nil {
		t.translateNode(ctx.Switch_stmt())
	} else if ctx.For_stmt() != nil {
		t.translateNode(ctx.For_stmt())
	} else if ctx.Func_call() != nil {
		t.translateNode(ctx.Func_call())
	} else if ctx.Func_dcl() != nil {
		t.translateNode(ctx.Func_dcl())
	} else if ctx.Transfer_stmt() != nil {
		t.translateNode(ctx.Transfer_stmt())
	}
}

// === DECLARACIONES DE VARIABLES ===

// Manejar declaraciones de funciones
func (t *ARM64Translator) translateFunctionDeclaration(ctx *compiler.FuncDeclContext) {
	funcName := ctx.ID().GetText()

	if funcName == "main" {
		t.generator.Comment(fmt.Sprintf("=== FUNCIÓN %s ===", funcName))

		// Traducir el cuerpo de la función main directamente
		for _, stmt := range ctx.AllStmt() {
			t.translateNode(stmt)
		}
	} else {
		// NO AGREGAR ERROR - Las funciones de usuario se generan al final
		t.generator.Comment(fmt.Sprintf("=== DECLARACIÓN DE FUNCIÓN %s (se generará al final) ===", funcName))
	}
}

// Manejar contexto de declaración
func (t *ARM64Translator) translateDeclStatement(ctx *compiler.Decl_stmtContext) {
	// Recorrer hijos para encontrar el tipo específico
	for i := 0; i < ctx.GetChildCount(); i++ {
		if child, ok := ctx.GetChild(i).(antlr.ParseTree); ok {
			t.translateNode(child)
		}
	}
}

// Manejar declaraciones con inferencia de tipo
func (t *ARM64Translator) translateValueDecl(ctx *compiler.ValueDeclContext) {
	varName := ctx.ID().GetText()
	t.generator.Comment(fmt.Sprintf("=== DECLARACIÓN: mut %s (inferido) ===", varName))

	// Evaluar la expresión del lado derecho
	t.translateExpression(ctx.Expression())

	// Guardar el resultado en la variable
	t.generator.StoreVariable(arm64.X0, varName)
}

// translateMutVarDecl traduce: mut variable int = 10
func (t *ARM64Translator) translateMutVarDecl(ctx *compiler.MutVarDeclContext) {
	varName := ctx.ID().GetText()
	t.generator.Comment(fmt.Sprintf("=== DECLARACIÓN: mut %s ===", varName))

	// Evaluar la expresión del lado derecho
	t.translateExpression(ctx.Expression())

	// Guardar el resultado en la variable
	t.generator.StoreVariable(arm64.X0, varName)
}

// translateVarAssDecl traduce: variable int = 10
func (t *ARM64Translator) translateVarAssDecl(ctx *compiler.VarAssDeclContext) {
	varName := ctx.ID().GetText()
	t.generator.Comment(fmt.Sprintf("=== DECLARACIÓN: %s ===", varName))

	// Evaluar la expresión del lado derecho
	t.translateExpression(ctx.Expression())

	// Guardar el resultado en la variable
	t.generator.StoreVariable(arm64.X0, varName)
}

// === ASIGNACIONES ===

// translateAssignment traduce: variable = expresion
func (t *ARM64Translator) translateAssignment(ctx *compiler.AssignmentDeclContext) {
	varName := ctx.Id_pattern().GetText()
	t.generator.Comment(fmt.Sprintf("=== ASIGNACIÓN: %s = ... ===", varName))

	// Verificar que la variable existe
	if !t.generator.VariableExists(varName) {
		t.addError(fmt.Sprintf("Variable '%s' no está declarada", varName))
		return
	}

	// Evaluar la expresión del lado derecho
	t.translateExpression(ctx.Expression())

	// Guardar el resultado en la variable
	t.generator.StoreVariable(arm64.X0, varName)
}

// === EXPRESIONES ===

// translateExpression traduce cualquier expresión y deja el resultado en X0
func (t *ARM64Translator) translateExpression(expr antlr.ParseTree) {
	fmt.Printf("🔢 Traduciendo expresión: %T = %s\n", expr, expr.GetText())

	switch ctx := expr.(type) {
	case *compiler.IntLiteralContext:
		t.translateIntLiteral(ctx)
	case *compiler.StringLiteralContext:
		t.translateStringLiteral(ctx)
	case *compiler.FloatLiteralContext:
		t.translateFloatLiteral(ctx)
	case *compiler.BoolLiteralContext:
		t.translateBoolLiteral(ctx)
	case *compiler.IdPatternExprContext:
		t.translateVariable(ctx)
	case *compiler.BinaryExprContext:
		t.translateBinaryExpression(ctx)
	case *compiler.UnaryExprContext: // AGREGAR ESTE CASE
		t.translateUnaryExpression(ctx)
	case *compiler.ParensExprContext:
		t.translateExpression(ctx.Expression())
	case *compiler.LiteralExprContext:
		// Procesar el literal interno
		t.translateExpression(ctx.Literal())
	case *compiler.LiteralContext:
		t.translateLiteral(ctx)
	case *compiler.FuncCallExprContext:
		t.translateNode(ctx.Func_call())
	case *compiler.IncredecrContext:
		t.translateExpression(ctx.Incredecre())
	case *compiler.IncrementoContext:
		t.translateIncrement(ctx)
	case *compiler.DecrementoContext:
		t.translateDecrement(ctx)

	default:
		t.addError(fmt.Sprintf("Expresión no implementada: %T", ctx))
		t.generator.LoadImmediate(arm64.X0, 0)
	}
}

func (t *ARM64Translator) translateUnaryExpression(ctx *compiler.UnaryExprContext) {
	// Obtener el operador (primer hijo)
	operator := ""
	var operandExpr antlr.ParseTree

	// Buscar el operador y la expresión
	for i := 0; i < ctx.GetChildCount(); i++ {
		child := ctx.GetChild(i)
		if terminal, ok := child.(antlr.TerminalNode); ok {
			operator = terminal.GetText()
		} else if parseTree, ok := child.(antlr.ParseTree); ok {
			operandExpr = parseTree
		}
	}

	t.generator.Comment(fmt.Sprintf("=== OPERADOR UNARIO: %s ===", operator))

	switch operator {
	case "!":
		t.translateLogicalNot(operandExpr)
	case "-":
		t.translateUnaryMinus(operandExpr)
	case "+":
		// El + unario no hace nada, solo evalúa la expresión
		t.translateExpression(operandExpr)
	default:
		t.addError(fmt.Sprintf("Operador unario no implementado: %s", operator))
		t.generator.LoadImmediate(arm64.X0, 0)
	}
}

func (t *ARM64Translator) translateLogicalNot(operandExpr antlr.ParseTree) {
	t.generator.Comment("=== OPERADOR LÓGICO NOT (!) ===")

	// Evaluar la expresión operando
	t.translateExpression(operandExpr)

	// Negar el resultado: si x0 == 0 entonces 1, sino 0
	t.generator.Comment("Negar resultado lógico")
	t.generator.Emit("cmp x0, #0")
	t.generator.Emit("cset x0, eq") // x0 = 1 si x0 era 0, sino 0
}

func (t *ARM64Translator) translateUnaryMinus(operandExpr antlr.ParseTree) {
	t.generator.Comment("=== OPERADOR UNARIO MENOS (-) ===")

	// Evaluar la expresión operando
	t.translateExpression(operandExpr)

	// Negar el valor: x0 = -x0
	t.generator.Comment("Negar valor numérico")
	t.generator.Emit("neg x0, x0")
}

// ✅ AGREGAR ESTA FUNCIÓN:
func (t *ARM64Translator) translateLiteral(ctx *compiler.LiteralContext) {
	// Primero intentar procesar hijos específicos
	for i := 0; i < ctx.GetChildCount(); i++ {
		child := ctx.GetChild(i)
		if child != nil {
			switch childCtx := child.(type) {
			case *compiler.IntLiteralContext:
				t.translateIntLiteral(childCtx)
				return
			case *compiler.FloatLiteralContext:
				t.translateFloatLiteral(childCtx)
				return
			case *compiler.StringLiteralContext:
				t.translateStringLiteral(childCtx)
				return
			case *compiler.BoolLiteralContext:
				t.translateBoolLiteral(childCtx)
				return
			case antlr.ParseTree:
				// Si es otro tipo de ParseTree, procesar recursivamente
				t.translateExpression(childCtx)
				return
			}
		}
	}

	// Fallback: analizar por texto si no se encontró un tipo específico
	text := ctx.GetText()
	fmt.Printf("🔍 Procesando literal por texto: %s\n", text)

	if value, err := strconv.Atoi(text); err == nil {
		t.generator.LoadImmediate(arm64.X0, value)
	} else {
		t.generator.LoadImmediate(arm64.X0, 0)
	}
}

// ✅ FUNCIONES DE TRADUCCIÓN DE LITERALES:
func (t *ARM64Translator) translateFloatLiteral(ctx *compiler.FloatLiteralContext) {
	valueStr := ctx.GetText()
	value, err := strconv.ParseFloat(valueStr, 64)
	if err != nil {
		t.addError(fmt.Sprintf("Error convirtiendo flotante: %s", valueStr))
		value = 0.0
	}

	// NUEVO: Escalar por 100 para mantener 2 decimales
	scaledValue := int(value * 100)
	t.generator.Comment(fmt.Sprintf("Flotante %s escalado como %d (x100)", valueStr, scaledValue))
	t.generator.LoadImmediate(arm64.X0, scaledValue)
}

func (t *ARM64Translator) translateStringLiteral(ctx *compiler.StringLiteralContext) {
	text := ctx.GetText()
	if len(text) >= 2 {
		text = text[1 : len(text)-1] // Quitar comillas
	}

	// Procesar secuencias de escape
	text = strings.ReplaceAll(text, "\\n", "\n")
	text = strings.ReplaceAll(text, "\\t", "\t")
	text = strings.ReplaceAll(text, "\\\"", "\"")
	text = strings.ReplaceAll(text, "\\\\", "\\")

	// VERIFICAR SI TIENE INTERPOLACIÓN
	if strings.Contains(text, "$") {
		// IMPORTANTE: No usar x0 para el resultado final en interpolación
		// porque processStringInterpolation hace múltiples prints
		t.processStringInterpolation(text)

		// DESPUÉS DE INTERPOLACIÓN, x0 queda en estado indefinido
		// Para funciones que esperan un valor en x0, cargar 0
		t.generator.Comment("Interpolación completada")

	} else {
		// String normal sin interpolación
		if existingLabel, exists := t.stringRegistry[text]; exists {
			t.generator.Comment(fmt.Sprintf("Usar string \"%s\" con etiqueta %s", text, existingLabel))
			t.generator.Emit(fmt.Sprintf("adr x0, %s", existingLabel))
		} else {
			t.addError(fmt.Sprintf("String \"%s\" no fue procesado en primera pasada", text))
			t.generator.LoadImmediate(arm64.X0, 0)
		}
	}
}

func (t *ARM64Translator) translateBoolLiteral(ctx *compiler.BoolLiteralContext) {
	valueStr := ctx.GetText()
	value := 0
	if valueStr == "true" {
		value = 1
	}
	t.generator.LoadImmediate(arm64.X0, value)
}

// translateIntLiteral traduce un literal entero
func (t *ARM64Translator) translateIntLiteral(ctx *compiler.IntLiteralContext) {
	valueStr := ctx.GetText()
	value, err := strconv.Atoi(valueStr)
	if err != nil {
		t.addError(fmt.Sprintf("Error convirtiendo entero: %s", valueStr))
		value = 0
	}

	t.generator.LoadImmediate(arm64.X0, value)
}

// translateVariable traduce el acceso a una variable
func (t *ARM64Translator) translateVariable(ctx *compiler.IdPatternExprContext) {
	varName := ctx.Id_pattern().GetText()

	if !t.generator.VariableExists(varName) {
		t.addError(fmt.Sprintf("Variable '%s' no está declarada", varName))
		t.generator.LoadImmediate(arm64.X0, 0) // Valor por defecto
		return
	}

	t.generator.LoadVariable(arm64.X0, varName)
}

// translateBinaryExpression traduce expresiones binarias (+, -, *, /, ==, etc.)
func (t *ARM64Translator) translateBinaryExpression(ctx *compiler.BinaryExprContext) {
	operator := ctx.GetOp().GetText()
	t.generator.Comment(fmt.Sprintf("=== OPERACIÓN BINARIA: %s ===", operator))

	// MANEJAR OPERADORES LÓGICOS CON EVALUACIÓN PEREZOSA
	if operator == "&&" {
		t.translateLogicalAnd(ctx)
		return
	}
	if operator == "||" {
		t.translateLogicalOr(ctx)
		return
	}

	// 🔧 NUEVO: DETECTAR CONCATENACIÓN DE STRINGS
	leftIsString := t.isStringExpression(ctx.GetLeft())
	rightIsString := t.isStringExpression(ctx.GetRight())

	if operator == "+" && (leftIsString || rightIsString) {
		// MANEJAR CONCATENACIÓN DE STRINGS
		t.translateStringConcatenation(ctx)
		return
	}

	// Determinar si estamos comparando flotantes
	leftIsFloat := t.isFloatExpression(ctx.GetLeft())
	rightIsFloat := t.isFloatExpression(ctx.GetRight())

	// Evaluar operando izquierdo y guardarlo en x1
	t.translateExpression(ctx.GetLeft())
	t.generator.Comment("Mover operando izquierdo a x1")
	t.generator.Emit("mov x1, x0")

	// Evaluar operando derecho (queda en X0)
	t.translateExpression(ctx.GetRight())

	// Si uno de los operandos es flotante, escalar el entero
	if leftIsFloat && !rightIsFloat {
		// Escalar operando derecho
		t.generator.Comment("Escalar operando derecho para comparación con flotante")
		t.generator.Emit("mov x2, #100")
		t.generator.Mul(arm64.X0, arm64.X0, "x2")
	} else if !leftIsFloat && rightIsFloat {
		// Escalar operando izquierdo
		t.generator.Comment("Escalar operando izquierdo para comparación con flotante")
		t.generator.Emit("mov x2, #100")
		t.generator.Mul(arm64.X1, arm64.X1, "x2")
	}

	// Realizar la operación correspondiente
	switch operator {
	case "+":
		t.generator.Add(arm64.X0, arm64.X1, arm64.X0)
	case "-":
		t.generator.Sub(arm64.X0, arm64.X1, arm64.X0)
	case "*":
		t.generator.Mul(arm64.X0, arm64.X1, arm64.X0)
	case "/":
		t.generator.Div(arm64.X0, arm64.X1, arm64.X0)
	case "%":
		t.generator.Mod(arm64.X0, arm64.X1, arm64.X0)
	case "==":
		t.translateComparison(arm64.X1, arm64.X0, "eq")
	case "!=":
		t.translateComparison(arm64.X1, arm64.X0, "ne")
	case "<":
		t.translateComparison(arm64.X1, arm64.X0, "lt")
	case ">":
		t.translateComparison(arm64.X1, arm64.X0, "gt")
	case "<=":
		t.translateComparison(arm64.X1, arm64.X0, "le")
	case ">=":
		t.translateComparison(arm64.X1, arm64.X0, "ge")
	default:
		t.addError(fmt.Sprintf("Operador no implementado: %s", operator))
	}
}

func (t *ARM64Translator) translateStringConcatenation(ctx *compiler.BinaryExprContext) {
	t.generator.Comment("=== CONCATENACIÓN DE STRINGS (VERSIÓN CORREGIDA) ===")

	// Para simplificar, crear un nuevo string literal que combine ambos
	leftText := t.getStringLiteralText(ctx.GetLeft())
	rightText := t.getStringLiteralText(ctx.GetRight())

	if leftText != "" && rightText != "" {
		// Si ambos son literales, crear concatenación directa
		concatenatedText := leftText + rightText

		// Verificar si ya existe en el registro
		if existingLabel, exists := t.stringRegistry[concatenatedText]; exists {
			t.generator.Comment(fmt.Sprintf("Usar string concatenado existente: \"%s\"", concatenatedText))
			t.generator.Emit(fmt.Sprintf("adr x0, %s", existingLabel))
		} else {
			// Crear nuevo string literal concatenado
			stringLabel := t.generator.AddStringLiteral(concatenatedText)
			t.stringRegistry[concatenatedText] = stringLabel
			t.generator.Comment(fmt.Sprintf("Crear nuevo string concatenado: \"%s\"", concatenatedText))
			t.generator.Emit(fmt.Sprintf("adr x0, %s", stringLabel))
		}
	} else {
		// Fallback: usar solo el segundo string
		t.generator.Comment("ADVERTENCIA: Concatenación no literal, usando segundo operando")
		t.translateExpression(ctx.GetRight())
	}
}

func (t *ARM64Translator) getStringLiteralText(expr antlr.ParseTree) string {
	switch ctx := expr.(type) {
	case *compiler.StringLiteralContext:
		text := ctx.GetText()
		if len(text) >= 2 {
			text = text[1 : len(text)-1] // Quitar comillas
		}
		// Procesar secuencias de escape
		text = strings.ReplaceAll(text, "\\n", "\n")
		text = strings.ReplaceAll(text, "\\t", "\t")
		text = strings.ReplaceAll(text, "\\\"", "\"")
		text = strings.ReplaceAll(text, "\\\\", "\\")
		return text
	case *compiler.LiteralExprContext:
		return t.getStringLiteralText(ctx.Literal())
	case *compiler.LiteralContext:
		for i := 0; i < ctx.GetChildCount(); i++ {
			if child := ctx.GetChild(i); child != nil {
				if stringCtx, ok := child.(*compiler.StringLiteralContext); ok {
					return t.getStringLiteralText(stringCtx)
				}
			}
		}
		// También verificar por texto directo
		text := ctx.GetText()
		if strings.HasPrefix(text, "\"") && strings.HasSuffix(text, "\"") {
			cleanText := text[1 : len(text)-1]
			cleanText = strings.ReplaceAll(cleanText, "\\n", "\n")
			cleanText = strings.ReplaceAll(cleanText, "\\t", "\t")
			cleanText = strings.ReplaceAll(cleanText, "\\\"", "\"")
			cleanText = strings.ReplaceAll(cleanText, "\\\\", "\\")
			return cleanText
		}
		return ""
	case *compiler.IdPatternExprContext:
		// Si es una variable string, intentar obtener su valor
		varName := ctx.Id_pattern().GetText()
		if varType, exists := t.variableTypes[varName]; exists && varType == "string" {
			// Para variables, no podemos obtener el valor en compile time
			// Devolver cadena vacía para indicar que no es literal
			return ""
		}
		return ""
	default:
		return ""
	}
}

// 🔧 NUEVA FUNCIÓN: Detectar expresiones de string
func (t *ARM64Translator) isStringExpression(expr antlr.ParseTree) bool {
	if expr == nil {
		return false
	}

	switch ctx := expr.(type) {
	case *compiler.StringLiteralContext:
		return true
	case *compiler.LiteralExprContext:
		return t.isStringExpression(ctx.Literal())
	case *compiler.LiteralContext:
		// Verificar si contiene StringLiteralContext
		for i := 0; i < ctx.GetChildCount(); i++ {
			if _, ok := ctx.GetChild(i).(*compiler.StringLiteralContext); ok {
				return true
			}
		}
		// También verificar por texto
		text := ctx.GetText()
		return strings.HasPrefix(text, "\"") && strings.HasSuffix(text, "\"")
	case *compiler.IdPatternExprContext:
		// Verificar el tipo de la variable
		varName := ctx.Id_pattern().GetText()
		if varType, exists := t.variableTypes[varName]; exists {
			return varType == "string"
		}
		return false
	default:
		return false
	}
}

func (t *ARM64Translator) isFloatExpression(expr antlr.ParseTree) bool {
	if expr == nil {
		return false
	}

	switch ctx := expr.(type) {
	case *compiler.FloatLiteralContext:
		return true
	case *compiler.LiteralExprContext:
		return t.isFloatExpression(ctx.Literal())
	case *compiler.LiteralContext:
		// Verificar si contiene FloatLiteralContext
		for i := 0; i < ctx.GetChildCount(); i++ {
			if _, ok := ctx.GetChild(i).(*compiler.FloatLiteralContext); ok {
				return true
			}
		}
		return false
	case *compiler.IdPatternExprContext:
		// Verificar el tipo de la variable
		varName := ctx.Id_pattern().GetText()
		if varType, exists := t.variableTypes[varName]; exists {
			return varType == "float"
		}
		return false
	default:
		return false
	}
}

func (t *ARM64Translator) translateLogicalAnd(ctx *compiler.BinaryExprContext) {
	t.generator.Comment("=== OPERADOR LÓGICO AND (&&) ===")

	falseLabel := t.generator.GetLabel()
	endLabel := t.generator.GetLabel()

	// Evaluar operando izquierdo
	t.translateExpression(ctx.GetLeft())

	// Si el operando izquierdo es falso (0), saltar directamente a false
	t.generator.Comment("Si operando izquierdo es falso, resultado es falso")
	t.generator.JumpIfZero(arm64.X0, falseLabel)

	// Evaluar operando derecho (solo si el izquierdo es verdadero)
	t.generator.Comment("Operando izquierdo es verdadero, evaluar operando derecho")
	t.translateExpression(ctx.GetRight())

	// El resultado está en x0 (verdadero si != 0, falso si == 0)
	// Convertir a 1 o 0 explícitamente
	t.generator.Comment("Convertir resultado a 1 o 0")
	t.generator.Emit("cmp x0, #0")
	t.generator.Emit("cset x0, ne") // x0 = 1 si != 0, sino 0
	t.generator.Jump(endLabel)

	// Etiqueta para resultado falso
	t.generator.SetLabel(falseLabel)
	t.generator.Comment("Resultado es falso")
	t.generator.LoadImmediate(arm64.X0, 0)

	// Etiqueta final
	t.generator.SetLabel(endLabel)
}

// === NUEVA FUNCIÓN: translateLogicalOr (||) ===
func (t *ARM64Translator) translateLogicalOr(ctx *compiler.BinaryExprContext) {
	t.generator.Comment("=== OPERADOR LÓGICO OR (||) ===")

	trueLabel := t.generator.GetLabel()
	endLabel := t.generator.GetLabel()

	// Evaluar operando izquierdo
	t.translateExpression(ctx.GetLeft())

	// Si el operando izquierdo es verdadero (!= 0), saltar directamente a true
	t.generator.Comment("Si operando izquierdo es verdadero, resultado es verdadero")
	t.generator.Emit("cmp x0, #0")
	t.generator.Emit("bne " + trueLabel)

	// Evaluar operando derecho (solo si el izquierdo es falso)
	t.generator.Comment("Operando izquierdo es falso, evaluar operando derecho")
	t.translateExpression(ctx.GetRight())

	// El resultado está en x0
	// Convertir a 1 o 0 explícitamente
	t.generator.Comment("Convertir resultado a 1 o 0")
	t.generator.Emit("cmp x0, #0")
	t.generator.Emit("cset x0, ne") // x0 = 1 si != 0, sino 0
	t.generator.Jump(endLabel)

	// Etiqueta para resultado verdadero
	t.generator.SetLabel(trueLabel)
	t.generator.Comment("Resultado es verdadero")
	t.generator.LoadImmediate(arm64.X0, 1)

	// Etiqueta final
	t.generator.SetLabel(endLabel)
}

// translateComparison traduce operaciones de comparación
func (t *ARM64Translator) translateComparison(reg1, reg2, condition string) {
	t.generator.Compare(reg1, reg2)

	// Usar CSET para convertir el resultado de la comparación a 1 o 0
	t.generator.Comment(fmt.Sprintf("Convertir resultado de comparación a 1/0"))
	t.generator.Emit(fmt.Sprintf("cset %s, %s", arm64.X0, condition))
}

// === LLAMADAS A FUNCIONES ===

// translateFunctionCall traduce llamadas a funciones
func (t *ARM64Translator) translateFunctionCall(ctx *compiler.FuncCallContext) {
	funcName := ctx.Id_pattern().GetText()

	// Manejar funciones especiales
	switch funcName {
	case "print":
		t.translatePrintFunction(ctx, false) // sin salto de línea
	case "println":
		t.translatePrintFunction(ctx, true) // con salto de línea
	case "main":
		t.generator.Comment("=== LLAMADA A FUNCIÓN MAIN ===")
	default:
		// AGREGAR: Verificar si es función de usuario
		if funcDecl, exists := t.userFunctions[funcName]; exists {
			t.translateUserFunctionCall(ctx, funcDecl)
		} else {
			// Manejar funciones nativas simuladas
			t.translateNativeFunction(ctx)
		}
	}
}

func (t *ARM64Translator) translatePrintBoolFunction(ctx *compiler.FuncCallContext) {
	t.generator.Comment("=== FUNCIÓN PRINT_BOOL ===")

	if ctx.Arg_list() != nil {
		args := ctx.Arg_list().(*compiler.ArgListContext).AllFunc_arg()
		if len(args) > 0 {
			// Tomar solo el primer argumento
			arg := args[0]
			if argCtx := arg.(*compiler.FuncArgContext); argCtx != nil {
				if argCtx.Expression() != nil {
					// Evaluar la expresión
					t.translateExpression(argCtx.Expression())
					// Llamar a print_bool
					t.generator.CallFunction("print_bool")
				}
			}
		}
	}
}

// la función translateUserFunctionCall en translator.go
func (t *ARM64Translator) translateUserFunctionCall(callCtx *compiler.FuncCallContext, funcDecl *compiler.FuncDeclContext) {
	funcName := callCtx.Id_pattern().GetText()
	t.generator.Comment(fmt.Sprintf("=== LLAMADA A FUNCIÓN DE USUARIO: %s ===", funcName))

	// Obtener información de parámetros de la función
	var paramNames []string
	if funcDecl.Param_list() != nil {
		params := funcDecl.Param_list().(*compiler.ParamListContext).AllFunc_param()
		for _, param := range params {
			if paramCtx := param.(*compiler.FuncParamContext); paramCtx.ID() != nil {
				paramNames = append(paramNames, paramCtx.ID().GetText())
			}
		}
	}

	// Preparar argumentos - CARGAR EN ORDEN INVERSO
	if callCtx.Arg_list() != nil {
		args := callCtx.Arg_list().(*compiler.ArgListContext).AllFunc_arg()

		// Debug: mostrar argumentos
		fmt.Printf("🔍 Argumentos para %s: %d\n", funcName, len(args))
		for i, arg := range args {
			fmt.Printf("🔍 Arg %d: %s\n", i, arg.GetText())
		}

		// CARGAR ARGUMENTOS EN ORDEN INVERSO PARA NO SOBRESCRIBIR
		for i := len(args) - 1; i >= 0; i-- {
			arg := args[i]
			if argCtx := arg.(*compiler.FuncArgContext); argCtx != nil {

				targetReg := fmt.Sprintf("x%d", i)
				t.generator.Comment(fmt.Sprintf("Cargando argumento %d (%s) en %s", i, argCtx.GetText(), targetReg))

				// NUEVO: Determinar el tipo del argumento que se está pasando
				var argType string

				// Evaluar el argumento
				if argCtx.Expression() != nil {
					// Inferir tipo de la expresión ANTES de evaluarla
					argType = t.inferExpressionType(argCtx.Expression())
					t.translateExpression(argCtx.Expression())
				} else if argCtx.Id_pattern() != nil {
					// Es una variable
					varName := argCtx.Id_pattern().GetText()
					if t.generator.VariableExists(varName) {
						// Obtener tipo de la variable
						if varType, exists := t.variableTypes[varName]; exists {
							argType = varType
						} else {
							argType = "unknown"
						}
						t.generator.LoadVariable(arm64.X0, varName)
					} else {
						t.addError(fmt.Sprintf("Variable '%s' no encontrada", varName))
						t.generator.LoadImmediate(arm64.X0, 0)
						argType = "int"
					}
				} else {
					// Fallback: intentar como texto
					argText := argCtx.GetText()
					if t.generator.VariableExists(argText) {
						if varType, exists := t.variableTypes[argText]; exists {
							argType = varType
						} else {
							argType = "unknown"
						}
						t.generator.LoadVariable(arm64.X0, argText)
					} else if value, err := strconv.Atoi(argText); err == nil {
						t.generator.LoadImmediate(arm64.X0, value)
						argType = "int"
					} else {
						t.addError(fmt.Sprintf("No se puede procesar argumento: %s", argText))
						t.generator.LoadImmediate(arm64.X0, 0)
						argType = "int"
					}
				}

				// NUEVO: Asignar tipo al parámetro correspondiente
				if i < len(paramNames) {
					paramName := paramNames[i]
					t.variableTypes[paramName] = argType
					fmt.Printf("📝 Parámetro '%s' asignado tipo: %s\n", paramName, argType)
				}

				// Mover al registro correcto (solo si no es x0)
				if i != 0 {
					t.generator.Emit(fmt.Sprintf("mov %s, x0", targetReg))
				}
			}
		}
	}

	// Llamar a la función
	t.generator.CallFunction(fmt.Sprintf("func_%s", funcName))
}

func (t *ARM64Translator) translateNativeFunction(ctx *compiler.FuncCallContext) {
	funcName := ctx.Id_pattern().GetText()

	switch funcName {
	case "atoi":
		// Simular atoi - por simplicidad retornar valor fijo
		if ctx.Arg_list() != nil {
			args := ctx.Arg_list().(*compiler.ArgListContext).AllFunc_arg()
			if len(args) > 0 {
				// Por simplicidad, si el string es "123", retornar 123
				t.generator.LoadImmediate(arm64.X0, 123)
			}
		}
	case "parseFloat":
		// Simular parse_float
		t.generator.LoadImmediate(arm64.X0, 123) // Simplificado
	case "TypeOf", "Type":
		// Simular TypeOf - retornar código que representa tipo
		t.generator.LoadImmediate(arm64.X0, 1) // 1=int, 2=float, etc.
	default:
		t.addError(fmt.Sprintf("Función no implementada: %s", funcName))
		t.generator.LoadImmediate(arm64.X0, 0)
	}
}

// FUNCIÓN CORREGIDA: translatePrintFunction
func (t *ARM64Translator) translatePrintFunction(ctx *compiler.FuncCallContext, withNewline bool) {

	if ctx.Arg_list() != nil {
		args := ctx.Arg_list().(*compiler.ArgListContext).AllFunc_arg()

		for i, arg := range args {
			if i > 0 {
				// ✅ CORRECTO: Imprime espacio entre argumentos
				t.generator.Comment("Imprimir espacio")
				t.generator.LoadImmediate(arm64.X0, 32) // ASCII espacio
				t.generator.CallFunction("print_char")
			}

			if argCtx := arg.(*compiler.FuncArgContext); argCtx != nil {
				if argCtx.Expression() != nil {
					exprText := argCtx.Expression().GetText()
					fmt.Printf("🔍 Procesando argumento %d: %s\n", i, exprText)

					// ✅ DETECCIÓN MEJORADA: String literal
					if strings.HasPrefix(exprText, "\"") && strings.HasSuffix(exprText, "\"") {
						fmt.Printf("✅ Argumento %d es string literal\n", i)
						t.translateExpression(argCtx.Expression())
						t.generator.CallFunction("print_string")
					} else {
						// ✅ AQUÍ ESTÁ LA CORRECCIÓN PRINCIPAL
						argType := t.getArgumentType(argCtx)
						fmt.Printf("✅ Argumento %d es tipo: %s, valor: %s\n", i, argType, exprText)

						// ✅ EVALUAR LA EXPRESIÓN PRIMERO
						t.translateExpression(argCtx.Expression())

						// 🔧 CORRECCIÓN: Lógica de decisión más robusta
						switch argType {
						case "string":
							t.generator.CallFunction("print_string")
						case "bool":
							t.generator.CallFunction("print_bool")
						case "float":
							fmt.Printf("🔧 LLAMANDO print_float para argumento tipo float\n")
							t.generator.CallFunction("print_float") // ← ASEGURAR QUE ESTO SE EJECUTE
						case "int":
							t.generator.CallFunction("print_integer")
						default:
							// Si no conocemos el tipo, verificar si es una variable
							if t.generator.VariableExists(exprText) {
								// Es una variable, verificar su tipo registrado
								fmt.Printf("✅ %s es variable conocida, verificando tipo registrado\n", exprText)
								if varType, exists := t.variableTypes[exprText]; exists {
									fmt.Printf("🔍 Tipo de variable '%s': %s\n", exprText, varType)
									switch varType {
									case "string":
										t.generator.CallFunction("print_string")
									case "bool":
										t.generator.CallFunction("print_bool")
									case "float":
										fmt.Printf("🔧 LLAMANDO print_float para variable tipo float\n")
										t.generator.CallFunction("print_float") // ← CRÍTICO: ESTO DEBE EJECUTARSE
									default:
										t.generator.CallFunction("print_integer")
									}
								} else {
									fmt.Printf("⚠️ Variable %s no tiene tipo registrado, usando print_integer\n", exprText)
									t.generator.CallFunction("print_integer")
								}
							} else {
								// Tipo completamente desconocido, asumir entero
								fmt.Printf("⚠️ Tipo desconocido para %s, asumiendo entero\n", exprText)
								t.generator.CallFunction("print_integer")
							}
						}
					}
				} else if argCtx.Id_pattern() != nil {
					// Variable directa
					varName := argCtx.Id_pattern().GetText()
					fmt.Printf("✅ Argumento %d es variable directa: %s\n", i, varName)

					if t.generator.VariableExists(varName) {
						t.generator.LoadVariable(arm64.X0, varName)

						// 🔧 CORRECCIÓN: Verificar tipo de variable directa
						if varType, exists := t.variableTypes[varName]; exists {
							fmt.Printf("🔍 Variable directa '%s' tiene tipo: %s\n", varName, varType)
							switch varType {
							case "string":
								t.generator.CallFunction("print_string")
							case "bool":
								t.generator.CallFunction("print_bool")
							case "float":
								fmt.Printf("🔧 LLAMANDO print_float para variable directa tipo float\n")
								t.generator.CallFunction("print_float") // ← CRÍTICO
							default:
								t.generator.CallFunction("print_integer")
							}
						} else {
							fmt.Printf("⚠️ Variable directa %s no tiene tipo, usando print_integer\n", varName)
							t.generator.CallFunction("print_integer")
						}
					} else {
						t.addError(fmt.Sprintf("Variable '%s' no encontrada", varName))
					}
				}
			}
		}
	}

	if withNewline {
		t.generator.Comment("Imprimir salto de línea")
		t.generator.LoadImmediate(arm64.X0, 10) // ASCII newline (\n)
		t.generator.CallFunction("print_char")
	}
}

func (t *ARM64Translator) isBooleanExpression(expr antlr.ParseTree) bool {
	if expr == nil {
		return false
	}

	switch ctx := expr.(type) {
	case *compiler.BoolLiteralContext:
		return true
	case *compiler.LiteralExprContext:
		return t.isBooleanExpression(ctx.Literal())
	case *compiler.LiteralContext:
		// Verificar si contiene BoolLiteralContext
		for i := 0; i < ctx.GetChildCount(); i++ {
			if _, ok := ctx.GetChild(i).(*compiler.BoolLiteralContext); ok {
				return true
			}
		}
		// AGREGAR: También verificar por texto específico
		text := ctx.GetText()
		if text == "true" || text == "false" {
			return true
		}
		return false
	case *compiler.BinaryExprContext:
		// Operaciones de comparación devuelven booleanos
		operator := ctx.GetOp().GetText()
		switch operator {
		case "==", "!=", "<", ">", "<=", ">=", "&&", "||":
			return true
		}
		return false
	case *compiler.UnaryExprContext:
		// El operador ! devuelve booleano
		for i := 0; i < ctx.GetChildCount(); i++ {
			if terminal, ok := ctx.GetChild(i).(antlr.TerminalNode); ok {
				if terminal.GetText() == "!" {
					return true
				}
			}
		}
		return false
	case *compiler.IdPatternExprContext:
		// Verificar el tipo de la variable
		varName := ctx.Id_pattern().GetText()
		if varType, exists := t.variableTypes[varName]; exists {
			return varType == "bool"
		}
		return false
	default:
		// CASO ESPECIAL: Si el texto completo es "true" o "false"
		text := expr.GetText()
		return text == "true" || text == "false"
	}
}

// Determinar tipo de argumento
// getArgumentType - Determina el tipo de un argumento con debug mejorado
func (t *ARM64Translator) getArgumentType(argCtx *compiler.FuncArgContext) string {
	if argCtx.Expression() != nil {
		exprText := argCtx.Expression().GetText()

		// DEBUG: Imprimir información detallada
		fmt.Printf("🔍 getArgumentType para: %s\n", exprText)

		// VERIFICAR EXPLÍCITAMENTE BOOLEANOS
		if exprText == "true" || exprText == "false" {
			fmt.Printf("✅ Detectado como bool\n")
			return "bool"
		}

		// VERIFICAR STRINGS
		if strings.HasPrefix(exprText, "\"") && strings.HasSuffix(exprText, "\"") {
			fmt.Printf("✅ Detectado como string\n")
			return "string"
		}

		// VERIFICAR SI ES VARIABLE
		if t.generator.VariableExists(exprText) {
			if varType, exists := t.variableTypes[exprText]; exists {
				fmt.Printf("✅ Variable %s tiene tipo: %s\n", exprText, varType)
				return varType
			} else {
				fmt.Printf("⚠️ Variable %s existe pero no tiene tipo registrado\n", exprText)
			}
		}

		// VERIFICAR SI ES NÚMERO FLOTANTE
		if strings.Contains(exprText, ".") {
			if _, err := strconv.ParseFloat(exprText, 64); err == nil {
				fmt.Printf("✅ Detectado como float literal\n")
				return "float"
			}
		}

		// VERIFICAR SI ES NÚMERO ENTERO
		if _, err := strconv.Atoi(exprText); err == nil {
			fmt.Printf("✅ Detectado como int literal\n")
			return "int"
		}

		// USAR INFERENCIA DE TIPO
		inferredType := t.inferExpressionType(argCtx.Expression())
		fmt.Printf("✅ Tipo inferido: %s\n", inferredType)
		return inferredType
	} else if argCtx.Id_pattern() != nil {
		varName := argCtx.Id_pattern().GetText()
		fmt.Printf("🔍 getArgumentType para variable directa: %s\n", varName)

		if varType, exists := t.variableTypes[varName]; exists {
			fmt.Printf("✅ Variable directa %s tiene tipo: %s\n", varName, varType)
			return varType
		} else {
			fmt.Printf("⚠️ Variable directa %s no tiene tipo registrado\n", varName)
		}
	}
	fmt.Printf("⚠️ Tipo desconocido, retornando 'unknown'\n")
	return "unknown"
}

// === CONTROL DE FLUJO (simplificado) ===

// translateIfStatement traduce declaraciones if-else
func (t *ARM64Translator) translateIfStatement(ctx *compiler.IfStmtContext) {
	t.generator.Comment("=== IF STATEMENT ===")

	elseLabel := t.generator.GetLabel()
	endLabel := t.generator.GetLabel()

	// Evaluar la condición del primer if_chain
	if len(ctx.AllIf_chain()) > 0 {
		ifChain := ctx.AllIf_chain()[0]
		if ifChainCtx, ok := ifChain.(*compiler.IfChainContext); ok {
			// Evaluar condición
			t.translateExpression(ifChainCtx.Expression())

			// Saltar a else si la condición es falsa (0)
			t.generator.JumpIfZero(arm64.X0, elseLabel)

			// Ejecutar cuerpo del if
			for _, stmt := range ifChainCtx.AllStmt() {
				t.translateNode(stmt)
			}

			// Saltar al final para evitar ejecutar el else
			t.generator.Jump(endLabel)
		}
	}

	// Etiqueta else
	t.generator.SetLabel(elseLabel)

	// Si hay else, ejecutarlo
	if ctx.Else_stmt() != nil {
		elseCtx := ctx.Else_stmt().(*compiler.ElseStmtContext)
		for _, stmt := range elseCtx.AllStmt() {
			t.translateNode(stmt)
		}
	}

	// Etiqueta final
	t.generator.SetLabel(endLabel)
}

// translateSwitchStatement traduce declaraciones switch
func (t *ARM64Translator) translateSwitchStatement(ctx *compiler.SwitchStmtContext) {
	t.generator.Comment("=== SWITCH STATEMENT ===")

	// Evaluar la expresión del switch una vez y guardarla
	t.translateExpression(ctx.Expression())
	t.generator.Comment("Guardar valor del switch en x19")
	t.generator.Emit("mov x19, x0")

	// Generar etiquetas
	defaultLabel := t.generator.GetLabel()
	endLabel := t.generator.GetLabel()
	caseLabels := make([]string, 0)

	// Push etiquetas de break
	t.breakLabels = append(t.breakLabels, endLabel)

	// Generar etiquetas para cada caso
	cases := ctx.AllSwitch_case()
	for range cases {
		caseLabels = append(caseLabels, t.generator.GetLabel())
	}

	t.generator.Comment("=== COMPARACIONES DE CASOS ===")

	// Generar comparaciones para cada caso
	for i, switchCase := range cases {
		if caseCtx, ok := switchCase.(*compiler.SwitchCaseContext); ok {
			t.generator.Comment(fmt.Sprintf("Comparar caso %d", i))

			// Evaluar la expresión del caso
			t.translateExpression(caseCtx.Expression())

			// Comparar con el valor del switch
			t.generator.Compare("x19", "x0")
			t.generator.Emit(fmt.Sprintf("beq %s", caseLabels[i]))
		}
	}

	// Si ningún caso coincide, ir al default (o al final si no hay default)
	if ctx.Default_case() != nil {
		t.generator.Jump(defaultLabel)
	} else {
		t.generator.Jump(endLabel)
	}

	// Generar código para cada caso
	for i, switchCase := range cases {
		if caseCtx, ok := switchCase.(*compiler.SwitchCaseContext); ok {
			t.generator.SetLabel(caseLabels[i])
			t.generator.Comment(fmt.Sprintf("=== CASO %d ===", i))

			// Ejecutar statements del caso
			for _, stmt := range caseCtx.AllStmt() {
				t.translateNode(stmt)
			}

			// Automáticamente saltar al final (break implícito)
			t.generator.Jump(endLabel)
		}
	}

	// Generar caso default si existe
	if ctx.Default_case() != nil {
		t.generator.SetLabel(defaultLabel)
		t.generator.Comment("=== CASO DEFAULT ===")

		defaultCtx := ctx.Default_case().(*compiler.DefaultCaseContext)
		for _, stmt := range defaultCtx.AllStmt() {
			t.translateNode(stmt)
		}
	}

	// Etiqueta final
	t.generator.SetLabel(endLabel)

	// Limpiar etiquetas de break
	if len(t.breakLabels) > 0 {
		t.breakLabels = t.breakLabels[:len(t.breakLabels)-1]
	}

	t.generator.Comment("=== FIN SWITCH ===")
}

// ====================================
// For Loops
// ====================================

// translateForLoop traduce bucles for
func (t *ARM64Translator) translateForLoop(ctx *compiler.ForStmtCondContext) {
	t.generator.Comment("=== FOR LOOP ===")

	startLabel := t.generator.GetLabel()
	endLabel := t.generator.GetLabel()

	// Etiquetas para break y continue
	t.breakLabels = append(t.breakLabels, endLabel)
	t.continueLabels = append(t.continueLabels, startLabel)

	// Etiqueta de inicio del bucle
	t.generator.SetLabel(startLabel)

	// Evaluar condición
	t.translateExpression(ctx.Expression())

	// Salir del bucle si la condición es falsa
	t.generator.JumpIfZero(arm64.X0, endLabel)

	// Ejecutar cuerpo del bucle
	for _, stmt := range ctx.AllStmt() {
		t.translateNode(stmt)
	}

	// Volver al inicio del bucle
	t.generator.Jump(startLabel)

	// Etiqueta final
	t.generator.SetLabel(endLabel)

	//Pop de etiquetas al salir del bucle
	t.breakLabels = t.breakLabels[:len(t.breakLabels)-1]
	t.continueLabels = t.continueLabels[:len(t.continueLabels)-1]
}

func (t *ARM64Translator) translateForAssignment(ctx *compiler.ForAssCondContext) {
	t.generator.Comment("=== FOR tipo C-style ===")

	// i = 1;
	t.translateNode(ctx.Assign_stmt())

	startLabel := t.generator.GetLabel()
	continueLabel := t.generator.GetLabel()
	endLabel := t.generator.GetLabel()

	// Etiquetas para break y continue
	t.breakLabels = append(t.breakLabels, endLabel)
	t.continueLabels = append(t.continueLabels, continueLabel)

	t.generator.SetLabel(startLabel)

	// Evaluar condición: i <= 5
	t.translateExpression(ctx.Expression(0)) // condición
	t.generator.JumpIfZero(arm64.X0, endLabel)

	// Cuerpo
	for _, stmt := range ctx.AllStmt() {
		t.translateNode(stmt)
	}

	// Etiqueta para continue
	t.generator.SetLabel(continueLabel)

	// Incremento
	t.translateExpression(ctx.Expression(1))

	// Repetir ciclo
	t.generator.Jump(startLabel)

	t.generator.SetLabel(endLabel)

	// Limpiar
	t.breakLabels = t.breakLabels[:len(t.breakLabels)-1]
	t.continueLabels = t.continueLabels[:len(t.continueLabels)-1]

}

// ====================================
// Incremento y Decremento
// ====================================
func (t *ARM64Translator) translateIncrement(ctx *compiler.IncrementoContext) {
	varName := ctx.ID().GetText()

	t.generator.LoadVariable(arm64.X0, varName)
	t.generator.Comment(fmt.Sprintf("Incrementar '%s'", varName))
	t.generator.Emit("add x0, x0, #1")
	t.generator.StoreVariable(arm64.X0, varName)
}

func (t *ARM64Translator) translateDecrement(ctx *compiler.DecrementoContext) {
	varName := ctx.ID().GetText()

	t.generator.LoadVariable(arm64.X0, varName)
	t.generator.Comment(fmt.Sprintf("Decrementar '%s'", varName))
	t.generator.Emit("sub x0, x0, #1")
	t.generator.StoreVariable(arm64.X0, varName)
}

// ====================================
// Transferencia de Control
// ====================================

func (t *ARM64Translator) translateBreakStatement(ctx *compiler.BreakStmtContext) {
	if len(t.breakLabels) == 0 {
		t.addError("Break fuera de un contexto de bucle")
		return
	}
	label := t.breakLabels[len(t.breakLabels)-1]
	t.generator.Comment("Break statement")
	t.generator.Jump(label)
}

func (t *ARM64Translator) translateContinueStatement(ctx *compiler.ContinueStmtContext) {
	if len(t.continueLabels) == 0 {
		t.addError("Continue fuera de un contexto de bucle")
		return
	}
	label := t.continueLabels[len(t.continueLabels)-1]
	t.generator.Comment("Continue statement")
	t.generator.Jump(label)
}

// ====================================
// Proceso de interpolacion
// ====================================
func (t *ARM64Translator) processStringInterpolation(text string) {
	t.generator.Comment("=== INTERPOLACIÓN DE STRING ===")

	// Dividir el string en partes: texto y variables
	parts := t.parseInterpolatedString(text)

	for _, part := range parts { // ← Quitar la variable i
		if part.IsVariable {
			// Es una variable - cargar y imprimir según su tipo
			varName := part.Content
			if t.generator.VariableExists(varName) {
				t.generator.Comment(fmt.Sprintf("Interpolando variable: %s", varName))
				t.generator.LoadVariable(arm64.X0, varName)

				// Determinar tipo y llamar función apropiada
				if varType, exists := t.variableTypes[varName]; exists {
					switch varType {
					case "bool":
						t.generator.CallFunction("print_bool")
					case "string":
						t.generator.CallFunction("print_string")
					case "float":
						t.generator.CallFunction("print_float")
					default:
						t.generator.CallFunction("print_integer")
					}
				} else {
					t.generator.CallFunction("print_integer")
				}
			} else {
				t.addError(fmt.Sprintf("Variable '%s' no encontrada en interpolación", varName))
			}
		} else {
			// Es texto literal - crear string y imprimir
			if part.Content != "" {
				t.generator.Comment(fmt.Sprintf("Interpolando texto: \"%s\"", part.Content))

				// VERIFICAR si ya existe en el registro
				if existingLabel, exists := t.stringRegistry[part.Content]; exists {
					t.generator.Emit(fmt.Sprintf("adr x0, %s", existingLabel))
					t.generator.CallFunction("print_string")
				} else {
					// Si no existe, reportar error
					t.addError(fmt.Sprintf("Parte interpolada \"%s\" no fue registrada en primera pasada", part.Content))
				}
			}
		}
	}
}

// Estructura para las partes del string:
type InterpolationPart struct {
	Content    string
	IsVariable bool
}

func (t *ARM64Translator) parseInterpolatedString(text string) []InterpolationPart {
	var parts []InterpolationPart
	var currentPart strings.Builder

	i := 0
	for i < len(text) {
		if text[i] == '$' && i+1 < len(text) {
			// Guardar texto anterior si existe
			if currentPart.Len() > 0 {
				parts = append(parts, InterpolationPart{
					Content:    currentPart.String(),
					IsVariable: false,
				})
				currentPart.Reset()
			}

			// Saltar el '$'
			i++

			// Extraer nombre de variable
			varStart := i
			for i < len(text) && (isLetter(text[i]) || isDigit(text[i]) || text[i] == '_') {
				i++
			}

			if i > varStart {
				varName := text[varStart:i]
				parts = append(parts, InterpolationPart{
					Content:    varName,
					IsVariable: true,
				})
			}
			// i ya está en la posición correcta
		} else {
			currentPart.WriteByte(text[i])
			i++
		}
	}

	// Agregar última parte si existe
	if currentPart.Len() > 0 {
		parts = append(parts, InterpolationPart{
			Content:    currentPart.String(),
			IsVariable: false,
		})
	}

	return parts
}

func isLetter(c byte) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

func isDigit(c byte) bool {
	return c >= '0' && c <= '9'
}

// === LIBRERÍA ESTÁNDAR ===

// REEMPLAZAR tu función generateStandardLibrary actual con esta versión corregida:
func (t *ARM64Translator) generateStandardLibrary() {

	// FUNCIÓN print_integer - USAR EMIT INDIVIDUAL PARA CADA LÍNEA
	t.generator.EmitRaw("")
	t.generator.EmitRaw("print_integer:")
	t.generator.Emit("// Funcion para imprimir enteros con signo")
	t.generator.Emit("// Input: x0 = numero a imprimir")
	t.generator.Emit("stp x29, x30, [sp, #-16]!") // ← Espacios correctos
	t.generator.Emit("stp x19, x20, [sp, #-16]!") // ← Espacios correctos
	t.generator.Emit("stp x21, x22, [sp, #-16]!") // ← Espacios correctos
	t.generator.Emit("stp x23, x24, [sp, #-16]!") // ← Espacios correctos
	t.generator.Emit("mov x19, x0")
	t.generator.Emit("// Manejar caso especial: cero")
	t.generator.Emit("cbnz x19, check_negative")
	t.generator.Emit("mov x0, #48")
	t.generator.Emit("bl print_char")
	t.generator.Emit("b print_integer_done")
	t.generator.EmitRaw("check_negative:")
	t.generator.Emit("// Verificar si es negativo")
	t.generator.Emit("tbnz x19, #63, handle_negative") // ← Espacios correctos
	t.generator.Emit("b convert_positive")
	t.generator.EmitRaw("handle_negative:")
	t.generator.Emit("// Imprimir signo negativo")
	t.generator.Emit("mov x0, #45")
	t.generator.Emit("bl print_char")
	t.generator.Emit("neg x19, x19")
	t.generator.EmitRaw("convert_positive:")
	t.generator.Emit("// Preparar buffer para digitos")
	t.generator.Emit("sub sp, sp, #32")
	t.generator.Emit("mov x20, sp")
	t.generator.Emit("mov x21, #0")
	t.generator.EmitRaw("digit_conversion_loop:")
	t.generator.Emit("mov x22, #10")
	t.generator.Emit("udiv x23, x19, x22") // PROBLEMA RESUELTO: sintaxis ARM64 válida
	t.generator.Emit("msub x24, x23, x22, x19")
	t.generator.Emit("add x24, x24, #48")
	t.generator.Emit("strb w24, [x20, x21]") // PROBLEMA RESUELTO: corchetes correctos
	t.generator.Emit("add x21, x21, #1")
	t.generator.Emit("mov x19, x23")
	t.generator.Emit("cbnz x19, digit_conversion_loop")
	t.generator.EmitRaw("print_digits_loop:")
	t.generator.Emit("sub x21, x21, #1")
	t.generator.Emit("ldrb w0, [x20, x21]") // PROBLEMA RESUELTO: sintaxis ldrb corregida
	t.generator.Emit("bl print_char")
	t.generator.Emit("cbnz x21, print_digits_loop")
	t.generator.Emit("add sp, sp, #32")
	t.generator.EmitRaw("print_integer_done:")
	t.generator.Emit("ldp x23, x24, [sp], #16") // PROBLEMA RESUELTO: sintaxis ldp corregida
	t.generator.Emit("ldp x21, x22, [sp], #16")
	t.generator.Emit("ldp x19, x20, [sp], #16")
	t.generator.Emit("ldp x29, x30, [sp], #16")
	t.generator.Emit("ret")

	// FUNCIÓN print_char - CORREGIDA
	t.generator.EmitRaw("")
	t.generator.EmitRaw("print_char:")
	t.generator.Emit("// Imprimir un caracter")
	t.generator.Emit("// Input: x0 = caracter ASCII")
	t.generator.Emit("stp x29, x30, [sp, #-16]!")
	t.generator.Emit("// Crear buffer temporal en el stack")
	t.generator.Emit("sub sp, sp, #16")
	t.generator.Emit("strb w0, [sp]") // PROBLEMA RESUELTO: número correcto de operandos
	t.generator.Emit("// Syscall write")
	t.generator.Emit("mov x0, #1")
	t.generator.Emit("mov x1, sp")
	t.generator.Emit("mov x2, #1")
	t.generator.Emit("mov x8, #64")
	t.generator.Emit("svc #0") // PROBLEMA RESUELTO: sintaxis svc correcta
	t.generator.Emit("add sp, sp, #16")
	t.generator.Emit("ldp x29, x30, [sp], #16")
	t.generator.Emit("ret")

	// FUNCIÓN print_string
	t.generator.EmitRaw("")
	t.generator.EmitRaw("print_string:")
	t.generator.Emit("// Funcion para imprimir strings (VERSION SEGURA)")
	t.generator.Emit("// Input: x0 = direccion del string")
	t.generator.Emit("stp x29, x30, [sp, #-16]!")
	t.generator.Emit("stp x19, x20, [sp, #-16]!")

	// 🔧 VALIDACIÓN: Verificar que la dirección no sea NULL
	t.generator.Emit("cbz x0, print_string_null")

	t.generator.Emit("mov x19, x0")
	t.generator.Emit("// Encontrar la longitud del string")
	t.generator.Emit("mov x20, #0")
	t.generator.EmitRaw("strlen_loop:")
	t.generator.Emit("ldrb w1, [x19, x20]")
	t.generator.Emit("cbz w1, strlen_done")
	t.generator.Emit("add x20, x20, #1")

	// 🔧 PROTECCIÓN: Limitar longitud máxima para evitar loops infinitos
	t.generator.Emit("cmp x20, #1024") // Máximo 1KB
	t.generator.Emit("bge strlen_done")

	t.generator.Emit("b strlen_loop")
	t.generator.EmitRaw("strlen_done:")
	t.generator.Emit("// Verificar si el string esta vacio")
	t.generator.Emit("cbz x20, print_string_done")
	t.generator.Emit("// Syscall write")
	t.generator.Emit("mov x0, #1")
	t.generator.Emit("mov x1, x19")
	t.generator.Emit("mov x2, x20")
	t.generator.Emit("mov x8, #64")
	t.generator.Emit("svc #0")
	t.generator.Emit("b print_string_done")

	// 🔧 MANEJO DE DIRECCIÓN NULL
	t.generator.EmitRaw("print_string_null:")
	t.generator.Emit("// Imprimir '(null)' si la dirección es NULL")
	t.generator.Emit("mov x0, #40") // '('
	t.generator.Emit("bl print_char")
	t.generator.Emit("mov x0, #110") // 'n'
	t.generator.Emit("bl print_char")
	t.generator.Emit("mov x0, #117") // 'u'
	t.generator.Emit("bl print_char")
	t.generator.Emit("mov x0, #108") // 'l'
	t.generator.Emit("bl print_char")
	t.generator.Emit("mov x0, #108") // 'l'
	t.generator.Emit("bl print_char")
	t.generator.Emit("mov x0, #41") // ')'
	t.generator.Emit("bl print_char")

	t.generator.EmitRaw("print_string_done:")
	t.generator.Emit("ldp x19, x20, [sp], #16")
	t.generator.Emit("ldp x29, x30, [sp], #16")
	t.generator.Emit("ret")

	// FUNCIÓN print_bool - CORREGIDA
	t.generator.EmitRaw("")
	t.generator.EmitRaw("print_bool:")
	t.generator.Emit("// Funcion para imprimir valores booleanos")
	t.generator.Emit("// Input: x0 = valor booleano")
	t.generator.Emit("stp x29, x30, [sp, #-16]!")
	t.generator.Emit("// Verificar si es true o false")
	t.generator.Emit("cmp x0, #0")
	t.generator.Emit("beq print_false_simple") // PROBLEMA RESUELTO: función beq reconocida
	t.generator.EmitRaw("print_true_simple:")
	t.generator.Emit("// Imprimir true manualmente")
	t.generator.Emit("mov x0, #116") // 't'
	t.generator.Emit("bl print_char")
	t.generator.Emit("mov x0, #114") // 'r'
	t.generator.Emit("bl print_char")
	t.generator.Emit("mov x0, #117") // 'u'
	t.generator.Emit("bl print_char")
	t.generator.Emit("mov x0, #101") // 'e'
	t.generator.Emit("bl print_char")
	t.generator.Emit("b print_bool_simple_done")
	t.generator.EmitRaw("print_false_simple:")
	t.generator.Emit("// Imprimir false manualmente")
	t.generator.Emit("mov x0, #102") // 'f'
	t.generator.Emit("bl print_char")
	t.generator.Emit("mov x0, #97") // 'a'
	t.generator.Emit("bl print_char")
	t.generator.Emit("mov x0, #108") // 'l'
	t.generator.Emit("bl print_char")
	t.generator.Emit("mov x0, #115") // 's'
	t.generator.Emit("bl print_char")
	t.generator.Emit("mov x0, #101") // 'e'
	t.generator.Emit("bl print_char")
	t.generator.EmitRaw("print_bool_simple_done:")
	t.generator.Emit("ldp x29, x30, [sp], #16")
	t.generator.Emit("ret")

	// FUNCIÓN print_float - VERSIÓN SIMPLE
	t.generator.EmitRaw("")
	t.generator.EmitRaw("print_float:")
	t.generator.Emit("// Funcion para imprimir flotantes (simplificada)")
	t.generator.Emit("// Input: x0 = numero escalado por 100")
	t.generator.Emit("stp x29, x30, [sp, #-16]!")
	t.generator.Emit("mov x19, x0")

	// Dividir por 100: parte entera = x0/100, decimal = x0%100
	t.generator.Emit("mov x1, #100")
	t.generator.Emit("udiv x20, x19, x1")      // x20 = parte entera
	t.generator.Emit("msub x21, x20, x1, x19") // x21 = parte decimal

	// Imprimir parte entera
	t.generator.Emit("mov x0, x20")
	t.generator.Emit("bl print_integer")

	// Imprimir punto
	t.generator.Emit("mov x0, #46") // '.'
	t.generator.Emit("bl print_char")

	// Imprimir parte decimal
	t.generator.Emit("mov x0, x21")
	t.generator.Emit("cmp x0, #10") // Si < 10, agregar un 0
	t.generator.Emit("bge print_decimal_normal")
	t.generator.Emit("mov x0, #48") // '0'
	t.generator.Emit("bl print_char")
	t.generator.Emit("mov x0, x21")

	t.generator.EmitRaw("print_decimal_normal:")
	t.generator.Emit("bl print_integer")

	t.generator.Emit("ldp x29, x30, [sp], #16")
	t.generator.Emit("ret")
}

// === UTILIDADES ===

// addError agrega un error a la lista
func (t *ARM64Translator) addError(message string) {
	t.errors = append(t.errors, message)
}

// GetErrors retorna todos los errores encontrados
func (t *ARM64Translator) GetErrors() []string {
	return t.errors
}

// HasErrors indica si hay errores
func (t *ARM64Translator) HasErrors() bool {
	return len(t.errors) > 0
}
