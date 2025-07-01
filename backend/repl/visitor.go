package repl

import (
	"fmt"
	"log"
	"strconv"
	"strings"

	"github.com/antlr4-go/antlr/v4"
	compiler "main.go/grammar"
	"main.go/value"
)

/*
ReplVisitor es una estructura que implementa el visitor para el REPL (Read-Eval-Print Loop).
*/
type ReplVisitor struct {
	compiler.BaseVLangGrammarVisitor
	ScopeTrace  *ScopeTrace
	CallStack   *CallStack
	Console     *Console
	ErrorTable  *ErrorTable
	StructNames []string
}

func NewVisitor(dclVisitor *DclVisitor) *ReplVisitor {
	return &ReplVisitor{
		ScopeTrace:  dclVisitor.ScopeTrace,
		ErrorTable:  dclVisitor.ErrorTable,
		StructNames: dclVisitor.StructNames,
		CallStack:   NewCallStack(),
		Console:     NewConsole(),
	}
}

func (v *ReplVisitor) GetReplContext() *ReplContext {
	return &ReplContext{
		Console:    v.Console,
		ScopeTrace: v.ScopeTrace,
		CallStack:  v.CallStack,
		ErrorTable: v.ErrorTable,
	}
}

func (v *ReplVisitor) ValidType(_type string) bool {
	return v.ScopeTrace.GlobalScope.ValidType(_type)
}

func (v *ReplVisitor) Visit(tree antlr.ParseTree) interface{} {
	fmt.Printf("-------------------------------------------\n")
	fmt.Printf("🔹 ReplVisitor.Visit llamado con: %T\n", tree)

	switch val := tree.(type) {
	case *antlr.ErrorNodeImpl:
		fmt.Printf("❌ ERROR NODE en ReplVisitor: %s\n", val.GetText())
		log.Fatal(val.GetText())
		return nil
	case *compiler.FuncCallExprContext:
		return v.VisitFuncCall(val.Func_call().(*compiler.FuncCallContext))
	default:
		fmt.Printf("🔹 ReplVisitor aceptando tree\n")
		return tree.Accept(v)
	}
}

func (v *ReplVisitor) VisitProgram(ctx *compiler.ProgramContext) interface{} {
	fmt.Printf("🎯 ¡ENTRANDO A ReplVisitor.VisitProgram!\n")
	fmt.Printf("🔹 Número de statements: %d\n", len(ctx.AllStmt()))

	for i, stmt := range ctx.AllStmt() {
		fmt.Printf("🔹 Procesando statement %d: %s\n", i, stmt.GetText())
		v.Visit(stmt)
	}
	return nil
}

func (v *ReplVisitor) VisitStmt(ctx *compiler.StmtContext) interface{} {

	if ctx.Decl_stmt() != nil {
		v.Visit(ctx.Decl_stmt())
	} else if ctx.Assign_stmt() != nil {
		v.Visit(ctx.Assign_stmt())
	} else if ctx.Block_ind() != nil {
		v.Visit(ctx.Block_ind())
	} else if ctx.Transfer_stmt() != nil {
		v.Visit(ctx.Transfer_stmt())
	} else if ctx.Func_call() != nil {
		v.Visit(ctx.Func_call())
	} else if ctx.Func_dcl() != nil {
		v.Visit(ctx.Func_dcl())
	} else if ctx.If_stmt() != nil {
		v.Visit(ctx.If_stmt())
	} else if ctx.Switch_stmt() != nil {
		v.Visit(ctx.Switch_stmt())
	} else if ctx.For_stmt() != nil {
		v.Visit(ctx.For_stmt())
	} else if ctx.Strct_dcl() != nil {
		v.Visit(ctx.Strct_dcl())
	} else {
		log.Fatal("Statement not recognized: ", ctx.GetText())
	}

	return nil
}

// En el enunciado no hay constantes, solo variables mut
/*
func isDeclConst(lexval string) bool {
	return lexval == "let"
}
*/

// Ejemplo: Mut variable_1 int = 10
// Ejemplo: Mut variable_2 int
func (v *ReplVisitor) VisitMutVarDecl(ctx *compiler.MutVarDeclContext) interface{} {

	// Si hubiera constantes se validan aquí
	// isConst := isDeclConst(ctx.Var_type().GetText())
	isConst := false

	// Obtenemos el context de la declaración MutVarDecl
	varName := ctx.ID().GetText()
	varType := v.Visit(ctx.Type_()).(string)
	varValue := v.Visit(ctx.Expression()).(value.IVOR)

	// copy object
	if obj, ok := varValue.(*ObjectValue); ok {
		varValue = obj.Copy()
	}

	variable, msg := v.ScopeTrace.AddVariable(varName, varType, varValue, isConst, false, ctx.GetStart())

	// Variable already exists
	if variable == nil {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), msg)
	}

	return nil
}

func (v *ReplVisitor) VisitValueDecl(ctx *compiler.ValueDeclContext) interface{} {

	isConst := false
	varName := ctx.ID().GetText()
	varValue := v.Visit(ctx.Expression()).(value.IVOR)
	varType := varValue.Type()

	if varType == "[]" {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "No se puede inferir el tipo de un vector vacio '"+varName+"'")
		return nil
	}

	// copy object
	if obj, ok := varValue.(*ObjectValue); ok {
		varValue = obj.Copy()
	}

	variable, msg := v.ScopeTrace.AddVariable(varName, varType, varValue, isConst, false, ctx.GetStart())

	// Variable already exists
	if variable == nil {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), msg)
	}
	return nil
}

// Ejemplo: variable_1 int = 10
func (v *ReplVisitor) VisitVarDecl(ctx *compiler.VarAssDeclContext) interface{} {

	// Si hubiera constantes se validan aquí
	// isConst := isDeclConst(ctx.Var_type().GetText())
	isConst := false

	// Obtenemos el context de la declaración VarAssDecñ
	exprName := ctx.ID().GetText()
	exprType := v.Visit(ctx.Type_()).(string)

	exprValue := v.Visit(ctx.Expression()).(value.IVOR)

	// Validar tipo de expresión
	if obj, ok := exprValue.(*ObjectValue); ok {
		exprValue = obj.Copy()
	}

	variable, msg := v.ScopeTrace.AddVariable(exprName, exprType, exprValue, isConst, false, ctx.GetStart())

	// Si la variable ya existe, se lanza un error
	if variable == nil {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), msg)
	}

	return nil
}

// Declaracion de vectores
// Ejemplo: vector_1 []int = [1, 2, 3]
// cibtexto VarVectDecl
func (v *ReplVisitor) VisitVarVectDecl(ctx *compiler.VarVectDeclContext) interface{} {
	fmt.Printf("🔹 Visitando VarVectDecl (nueva gramática): %s\n", ctx.GetText())

	// No hay constantes en este contexto
	isConst := false

	// Obtener información del contexto con la nueva gramática
	varName := ctx.ID().GetText()                        // nombre de variable
	vectorType := v.Visit(ctx.Vector_type()).(string)    // tipo del vector (ej: "[]int")
	vectorValue := v.Visit(ctx.Vect_expr()).(value.IVOR) // expresión del vector (ej: {1,2,3})

	fmt.Printf("   Variable: '%s'\n", varName)
	fmt.Printf("   Tipo Vector: '%s'\n", vectorType)
	fmt.Printf("   Valor Vector: %v (tipo: %s)\n", vectorValue, vectorValue.Type())

	// Validar que el tipo declarado sea un vector válido
	if !IsVectorType(vectorType) {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "El tipo '"+vectorType+"' no es un tipo de vector válido")
		return nil
	}

	// Validar que el valor sea compatible con el tipo declarado
	if vectorValue.Type() != vectorType && vectorValue.Type() != "[]" {
		// Verificar compatibilidad de tipos de elementos
		if IsVectorType(vectorValue.Type()) {
			declaredItemType := RemoveBrackets(vectorType)
			valueItemType := RemoveBrackets(vectorValue.Type())

			if declaredItemType != valueItemType {
				v.ErrorTable.NewSemanticError(ctx.GetStart(), "No se puede asignar un vector de tipo '"+vectorValue.Type()+"' a una variable de tipo '"+vectorType+"'")
				return nil
			}
		} else {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "No se puede asignar un valor de tipo '"+vectorValue.Type()+"' a una variable de tipo '"+vectorType+"'")
			return nil
		}
	}

	// Manejar copia de vectores para evitar referencias compartidas
	if IsVectorType(vectorValue.Type()) {
		vectorValue = vectorValue.Copy()
	}

	// Agregar variable al scope
	variable, msg := v.ScopeTrace.AddVariable(varName, vectorType, vectorValue, isConst, false, ctx.GetStart())

	if variable == nil {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), msg)
		return nil
	}

	fmt.Printf("✅ Variable vector '%s' declarada exitosamente con tipo '%s'\n", varName, vectorType)
	return nil
}

// Contexto decl_stmt
// visitor MutSliceDecl // mut slice []int
// Ejemplo: mut slice []int
func (v *ReplVisitor) VisitValDeclVec(ctx *compiler.ValDeclVecContext) interface{} {
	fmt.Printf("🔹 Visitando ValDeclVec: %s\n", ctx.GetText())

	// En este contexto no hay constantes, solo variables mut
	isConst := false

	// Obtener el nombre de la variable
	varName := ctx.ID().GetText()

	// Obtener el tipo del vector (ej: "[]int")
	varType := v.Visit(ctx.Type_()).(string)

	// Validar que sea un tipo de vector válido
	if !IsVectorType(varType) {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "El tipo '"+varType+"' no es un tipo de vector válido")
		return nil
	}

	// Extraer el tipo de los elementos del vector (ej: "int" de "[]int")
	itemType := RemoveBrackets(varType)

	// Validar que el tipo del elemento sea válido
	if !v.ValidType(itemType) {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "El tipo de elemento '"+itemType+"' no es válido para un vector")
		return nil
	}

	// Crear un vector vacío del tipo especificado
	emptyVector := NewVectorValue([]value.IVOR{}, varType, itemType)

	// Agregar la variable al scope actual
	variable, msg := v.ScopeTrace.AddVariable(varName, varType, emptyVector, isConst, false, ctx.GetStart())

	// Si la variable ya existe o hay otro error, reportarlo
	if variable == nil {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), msg)
		return nil
	}

	fmt.Printf("✅ Vector '%s' de tipo '%s' declarado exitosamente\n", varName, varType)
	return nil
}

// Contextos matrices

// Contextos VectorItemLis
// Ejemplo: {1, 2, 3}
func (v *ReplVisitor) VisitVectorItemLis(ctx *compiler.VectorItemLisContext) interface{} {
	fmt.Printf("🔹 Visitando VectorItemLis: %s\n", ctx.GetText())
	var vectorItems []value.IVOR

	if len(ctx.AllExpression()) == 0 {
		return NewVectorValue(vectorItems, "[]", value.IVOR_ANY)
	}

	for _, item := range ctx.AllExpression() {
		vectorItems = append(vectorItems, v.Visit(item).(value.IVOR))
	}

	var itemType = value.IVOR_NIL

	if ctx.Expression(0) != nil {
		itemType = vectorItems[0].Type()

		for _, item := range vectorItems {
			if item.Type() != itemType {
				v.ErrorTable.NewSemanticError(ctx.GetStart(), "Todos los items de la coleccion deben ser del mismo tipo")
				return value.DefaultNilValue
			}
		}
	}

	_type := "[" + "]" + itemType

	if IsVectorType(_type) {
		return NewVectorValue(vectorItems, _type, itemType)
	}

	v.ErrorTable.NewSemanticError(ctx.GetStart(), "Tipo "+_type+" no encontrado")
	return value.DefaultNilValue
}

// Faltan Vectores aca Vector Item - vect_expr

// VisitType es el visitor para el tipo de dato en la declaración de variables.
func (v *ReplVisitor) VisitType(ctx *compiler.TypeContext) interface{} {

	// remove white spaces
	_type := ctx.GetText()

	if v.ValidType(_type) {
		return _type
	}

	if IsVectorType(_type) {
		// remove [ ]
		internType := RemoveBrackets(_type)
		if v.ValidType(internType) {
			return _type
		}

		v.ErrorTable.NewSemanticError(ctx.GetStart(), "El tipo "+internType+" no es valido para un vector")
		return value.IVOR_NIL
	}

	/*


		if IsMatrixType(_type) {
			// remove [[]]
			internType := RemoveBrackets(_type)
			if value.IsPrimitiveType(internType) {
				return _type
			}

			v.ErrorTable.NewSemanticError(ctx.GetStart(), "Las matrices solo pueden contener tipos primitivos")
			return value.IVOR_NIL
		}
	*/

	v.ErrorTable.NewSemanticError(ctx.GetStart(), "Tipo "+ctx.GetText()+" no encontrado")
	return value.IVOR_NIL
}

func (v *ReplVisitor) VisitVector_type(ctx *compiler.Vector_typeContext) interface{} {
	return ctx.GetText()
}

func (v *ReplVisitor) VisitVectorItem(ctx *compiler.VectorItemContext) interface{} {

	varName := ctx.Id_pattern().GetText()

	variable := v.ScopeTrace.GetVariable(varName)
	if variable == nil {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "Variable "+varName+" no encontrada")
		return nil
	}

	// Validar que la variable sea vector o matriz
	if !(IsVectorType(variable.Type)) && !(IsMatrixType(variable.Type)) {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "La variable "+varName+" no es un vector o matriz")
		return nil
	}

	// Obtener todos los índices
	var indexes []int
	for _, expr := range ctx.AllExpression() {
		val := v.Visit(expr).(value.IVOR)
		if val.Type() != value.IVOR_INT {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "Los índices deben ser enteros")
			return nil
		}
		indexes = append(indexes, val.Value().(int))
	}
	// Verificar si es acceso a vector (1D)
	// Acceso con 1 índice
	if len(indexes) == 1 {
		index := indexes[0]

		// Si es vector
		if vectorValue, ok := variable.Value.(*VectorValue); ok {

			if !vectorValue.ValidIndex(index) {
				v.ErrorTable.NewSemanticError(ctx.GetStart(), "Índice "+strconv.Itoa(index)+" fuera de rango")
				return nil
			}
			return &VectorItemReference{
				Vector: vectorValue,
				Index:  index,
				Value:  vectorValue.Get(index),
			}
		}

		// Si es matriz y se accede a la fila completa
		if matrixValue, ok := variable.Value.(*MatrixValue); ok {
			if index < 0 || index >= len(matrixValue.Items) {
				v.ErrorTable.NewSemanticError(ctx.GetStart(), "Fila "+strconv.Itoa(index)+" fuera de rango")
				return nil
			}
			filaOriginal := matrixValue.Items[index]

			//  copia independiente
			nuevaFila := make([]value.IVOR, 0, len(filaOriginal))

			for _, item := range filaOriginal {
				copia := item.Copy()
				nuevaFila = append(nuevaFila, copia)
			}

			vectorValue := &VectorValue{
				InternalValue: nuevaFila,
				ItemType:      matrixValue.ItemType,
				FullType:      "[]" + matrixValue.ItemType,
				SizeValue:     &value.IntValue{InternalValue: len(nuevaFila)},
				IsEmpty:       &value.BoolValue{InternalValue: len(nuevaFila) == 0},
			}

			return vectorValue

		}

		v.ErrorTable.NewSemanticError(ctx.GetStart(), "Acceso inválido con un solo índice a variable "+varName)
		return nil
	}
	// Verificar si es acceso a matriz (2D)
	if len(indexes) == 2 {
		i, j := indexes[0], indexes[1]
		matrixValue, ok := variable.Value.(*MatrixValue)
		if !ok {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "La variable "+varName+" no es una matriz")
			return nil
		}
		if i < 0 || i >= len(matrixValue.Items) {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "Fila "+strconv.Itoa(i)+" fuera de rango")
			return nil
		}
		if j < 0 || j >= len(matrixValue.Items[i]) {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "Columna "+strconv.Itoa(j)+" fuera de rango")
			return nil
		}
		return &MatrixItemReference{
			Matrix: matrixValue,
			Index:  []int{i, j},
			Value:  matrixValue.Items[i][j],
		}
	}

	v.ErrorTable.NewSemanticError(ctx.GetStart(), "Número de índices inválido")
	return nil
}

// Falta el visit repeating
// Falta todo de Vectores
func (v *ReplVisitor) VisitAssignmentDecl(ctx *compiler.AssignmentDeclContext) interface{} {
	fmt.Printf("🔹 Visitando AssignmentDecl: %s\n", ctx.GetText())

	varName := v.Visit(ctx.Id_pattern()).(string)
	varValue := v.Visit(ctx.Expression()).(value.IVOR)

	fmt.Printf("   Variable: '%s'\n", varName)
	fmt.Printf("   Valor: %v (tipo: %s)\n", varValue, varValue.Type())

	// Buscar la variable en el scope
	if strings.Contains(varName, ".") {
		parts := strings.Split(varName, ".")
		if len(parts) != 2 {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "Asignación con acceso encadenado inválido: '"+varName+"'")
			return nil
		}
		baseName := parts[0]
		fieldName := parts[1]

		baseVar := v.ScopeTrace.GetVariable(baseName)
		if baseVar == nil {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "Variable '"+baseName+"' no encontrada")
			return nil
		}

		structVal, ok := baseVar.Value.(*value.StructValue)
		if !ok {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "Variable '"+baseName+"' no es un struct")
			return nil
		}

		// Verifica si el campo existe
		if _, exists := structVal.Instance.Fields[fieldName]; !exists {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "El campo '"+fieldName+"' no existe en el struct '"+baseName+"'")
			return nil
		}

		// Asignación
		structVal.Instance.Fields[fieldName] = varValue
		fmt.Printf("✅ Campo '%s' del struct '%s' actualizado a: %v\n", fieldName, baseName, varValue)
		return nil
	}

	// Buscar la variable en el scope
	variable := v.ScopeTrace.GetVariable(varName)

	if variable == nil {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "Variable '"+varName+"' no encontrada")
		return nil
	}

	fmt.Printf("   Variable encontrada con tipo: '%s'\n", variable.Type)

	// Validaciones específicas para vectores
	if IsVectorType(variable.Type) {
		fmt.Printf("   Procesando asignación de vector\n")

		// Si el valor es un vector, validar compatibilidad
		if IsVectorType(varValue.Type()) {
			varItemType := RemoveBrackets(variable.Type)
			valueItemType := RemoveBrackets(varValue.Type())

			fmt.Printf("   Tipo de elemento variable: '%s'\n", varItemType)
			fmt.Printf("   Tipo de elemento valor: '%s'\n", valueItemType)

			if varItemType != valueItemType {
				v.ErrorTable.NewSemanticError(ctx.GetStart(), "No se puede asignar un vector de tipo '"+varValue.Type()+"' a una variable de tipo '"+variable.Type+"'")
				return nil
			}
		} else if varValue.Type() != "[]" {
			// El valor no es un vector ni un vector vacío
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "No se puede asignar un valor de tipo '"+varValue.Type()+"' a una variable vector de tipo '"+variable.Type+"'")
			return nil
		}
	}

	// Manejar copia de objetos
	if obj, ok := varValue.(*ObjectValue); ok {
		varValue = obj.Copy()
		fmt.Printf("   Objeto copiado\n")
	}

	// Manejar copia de vectores para evitar referencias compartidas
	if IsVectorType(varValue.Type()) {
		varValue = varValue.Copy()
		fmt.Printf("   Vector copiado\n")
	}

	// Verificar contexto de mutación (para propiedades de struct)
	canMutate := true
	if v.ScopeTrace.CurrentScope.isStruct {
		canMutate = v.ScopeTrace.IsMutatingEnvironment()
	}

	// Realizar la asignación
	ok, msg := variable.AssignValue(varValue, canMutate)

	if !ok {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), msg)
		fmt.Printf("❌ Error en asignación: %s\n", msg)
		return nil
	}

	fmt.Printf("✅ Asignación completada exitosamente: '%s' = valor de tipo '%s'\n", varName, varValue.Type())
	return nil
}

func (v *ReplVisitor) VisitArgAddAssigDecl(ctx *compiler.ArgAddAssigDeclContext) interface{} {
	varName := v.Visit(ctx.Id_pattern()).(string)

	variable := v.ScopeTrace.GetVariable(varName)

	if variable == nil {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "Variable "+varName+" no encontrada")
	} else {

		leftValue := variable.Value
		rightValue := v.Visit(ctx.Expression()).(value.IVOR)

		op := string(ctx.GetOp().GetText()[0])

		strat, ok := BinaryStrats[op]

		if !ok {
			log.Fatal("Binary operator not found")
		}

		ok, msg, varValue := strat.Validate(leftValue, rightValue)

		if !ok {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), msg)
			return nil
		}

		canMutate := true

		if v.ScopeTrace.CurrentScope.isStruct {
			canMutate = v.ScopeTrace.IsMutatingEnvironment()
		}

		ok, msg = variable.AssignValue(varValue, canMutate)

		if !ok {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), msg)
		}
	}

	return nil
}

// Falta  VisitVectorAssign
func (v *ReplVisitor) VisitVectorAssign(ctx *compiler.VectorAssignContext) interface{} {

	rightValue := v.Visit(ctx.Expression()).(value.IVOR)

	switch itemRef := v.Visit(ctx.Vect_item()).(type) {
	case *VectorItemReference:

		leftValue := itemRef.Value

		// check type, todo: improve cast -> ¿? idk what i was thinking
		if rightValue.Type() != itemRef.Vector.ItemType {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "No se puede asignar un valor de tipo "+rightValue.Type()+" a un vector de tipo "+itemRef.Vector.ItemType)
			return nil
		}
		op := string(ctx.GetOp().GetText()[0])

		if op == "=" {
			itemRef.Vector.InternalValue[itemRef.Index] = rightValue
			return nil
		}

		strat, ok := BinaryStrats[op]

		if !ok {
			log.Fatal("Binary operator not found")
		}

		ok, msg, varValue := strat.Validate(leftValue, rightValue)

		if !ok {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), msg)
			return nil
		}

		itemRef.Vector.InternalValue[itemRef.Index] = varValue

		return nil
	case *MatrixItemReference:
		leftValue := itemRef.Value

		// check type, todo: improve cast -> ¿? idk what i was thinking
		if rightValue.Type() != RemoveBrackets(itemRef.Matrix.Type()) {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "No se puede asignar un valor de tipo "+rightValue.Type()+" a una matriz de tipo "+RemoveBrackets(itemRef.Matrix.Type()))
			return nil
		}

		op := string(ctx.GetOp().GetText()[0])

		if op == "=" {
			itemRef.Matrix.Set(itemRef.Index, rightValue)
			return nil
		}

		strat, ok := BinaryStrats[op]

		if !ok {
			log.Fatal("Binary operator not found")
		}

		ok, msg, varValue := strat.Validate(leftValue, rightValue)

		if !ok {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), msg)
			return nil
		}

		itemRef.Matrix.Set(itemRef.Index, varValue)
		return nil
	}

	return nil
}

// Id parents
func (v *ReplVisitor) VisitIdPattern(ctx *compiler.IdPatternContext) interface{} {
	return ctx.GetText()
}

// literales
//
//	Literal Int
func (v *ReplVisitor) VisitIntLiteral(ctx *compiler.IntLiteralContext) interface{} {

	intVal, _ := strconv.Atoi(ctx.GetText())

	return &value.IntValue{
		InternalValue: intVal,
	}

}

// literal Float
func (v *ReplVisitor) VisitFloatLiteral(ctx *compiler.FloatLiteralContext) interface{} {

	floatVal, _ := strconv.ParseFloat(ctx.GetText(), 64)

	return &value.FloatValue{
		InternalValue: floatVal,
	}

}

// literal String
// literal String
func (v *ReplVisitor) VisitStringLiteral(ctx *compiler.StringLiteralContext) interface{} {
	// Remover las comillas
	stringVal := ctx.GetText()[1 : len(ctx.GetText())-1]

	// Procesar secuencias de escape
	stringVal = strings.ReplaceAll(stringVal, "\\\"", "\"")
	stringVal = strings.ReplaceAll(stringVal, "\\\\", "\\")
	stringVal = strings.ReplaceAll(stringVal, "\\n", "\n")
	stringVal = strings.ReplaceAll(stringVal, "\\r", "\r")

	// ✨ NUEVA FUNCIONALIDAD: Procesar interpolación de strings
	if HasInterpolation(stringVal) {
		stringVal = v.InterpolateString(stringVal, ctx.GetStart())
	}

	// Character literal (un solo carácter)
	if len(stringVal) == 1 {
		return &value.CharacterValue{
			InternalValue: stringVal,
		}
	}

	// String literal
	return &value.StringValue{
		InternalValue: stringVal,
	}
}

// literal Bool
func (v *ReplVisitor) VisitBoolLiteral(ctx *compiler.BoolLiteralContext) interface{} {

	boolVal, _ := strconv.ParseBool(ctx.GetText())

	return &value.BoolValue{
		InternalValue: boolVal,
	}

}

// literal Nil
func (v *ReplVisitor) VisitNilLiteral(ctx *compiler.NilLiteralContext) interface{} {
	return value.DefaultNilValue
}

// literal en Exp
func (v *ReplVisitor) VisitLiteralExpr(ctx *compiler.LiteralExprContext) interface{} {
	fmt.Print("El valor de LiteralExp es: " + ctx.GetText() + "\n")
	return v.Visit(ctx.Literal())
}

// VisitIncredecr maneja las expresiones de incremento y decremento
// Ejemplo: i++ o i-- dentro de una expresión
func (v *ReplVisitor) VisitIncredecr(ctx *compiler.IncredecrContext) interface{} {
	return v.Visit(ctx.Incredecre())
}

// VisitIncremento maneja el incremento (ID++)
// Comportamiento: Post-incremento - retorna el valor actual, luego incrementa
func (v *ReplVisitor) VisitIncremento(ctx *compiler.IncrementoContext) interface{} {
	fmt.Printf("🔹 Visitando Incremento: %s\n", ctx.GetText())

	// Obtener el nombre de la variable
	varName := ctx.ID().GetText()

	// Buscar la variable en el scope
	variable := v.ScopeTrace.GetVariable(varName)
	if variable == nil {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "Variable '"+varName+"' no encontrada")
		return value.DefaultNilValue
	}

	// Verificar que la variable sea de tipo entero
	if variable.Value.Type() != value.IVOR_INT {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "El operador ++ solo puede aplicarse a variables de tipo int")
		return value.DefaultNilValue
	}

	// Verificar que no sea constante
	if variable.IsConst {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "No se puede incrementar una variable constante")
		return value.DefaultNilValue
	}

	// Obtener el valor actual (para retornarlo - post-incremento)
	currentValue := variable.Value.(*value.IntValue).InternalValue

	// Crear el nuevo valor incrementado
	newValue := &value.IntValue{
		InternalValue: currentValue + 1,
	}

	// Verificar contexto de mutación (para propiedades de struct)
	canMutate := true
	if v.ScopeTrace.CurrentScope.isStruct {
		canMutate = v.ScopeTrace.IsMutatingEnvironment()
	}

	// Asignar el nuevo valor a la variable
	ok, msg := variable.AssignValue(newValue, canMutate)
	if !ok {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), msg)
		return value.DefaultNilValue
	}

	// Retornar el valor original (comportamiento post-incremento)
	return &value.IntValue{
		InternalValue: currentValue,
	}
}

// VisitDecremento maneja el decremento (ID--)
// Comportamiento: Post-decremento - retorna el valor actual, luego decrementa
func (v *ReplVisitor) VisitDecremento(ctx *compiler.DecrementoContext) interface{} {
	fmt.Printf("🔹 Visitando Decremento: %s\n", ctx.GetText())

	// Obtener el nombre de la variable
	varName := ctx.ID().GetText()

	// Buscar la variable en el scope
	variable := v.ScopeTrace.GetVariable(varName)
	if variable == nil {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "Variable '"+varName+"' no encontrada")
		return value.DefaultNilValue
	}

	// Verificar que la variable sea de tipo entero
	if variable.Value.Type() != value.IVOR_INT {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "El operador -- solo puede aplicarse a variables de tipo int")
		return value.DefaultNilValue
	}

	// Verificar que no sea constante
	if variable.IsConst {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "No se puede decrementar una variable constante")
		return value.DefaultNilValue
	}

	// Obtener el valor actual (para retornarlo - post-decremento)
	currentValue := variable.Value.(*value.IntValue).InternalValue

	// Crear el nuevo valor decrementado
	newValue := &value.IntValue{
		InternalValue: currentValue - 1,
	}

	// Verificar contexto de mutación (para propiedades de struct)
	canMutate := true
	if v.ScopeTrace.CurrentScope.isStruct {
		canMutate = v.ScopeTrace.IsMutatingEnvironment()
	}

	// Asignar el nuevo valor a la variable
	ok, msg := variable.AssignValue(newValue, canMutate)
	if !ok {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), msg)
		return value.DefaultNilValue
	}

	// Retornar el valor original (comportamiento post-decremento)
	return &value.IntValue{
		InternalValue: currentValue,
	}
}

func (v *ReplVisitor) VisitIdPatternExpr(ctx *compiler.IdPatternExprContext) interface{} {
	idCtx := ctx.Id_pattern().(*compiler.IdPatternContext)

	// Extraer todos los IDs del acceso encadenado
	ids := []string{idCtx.GetHead().GetText()}
	for _, t := range idCtx.GetTail() {
		ids = append(ids, t.GetText())
	}

	// Inicia la resolución
	varName := ids[0]
	variable := v.ScopeTrace.GetVariable(varName)

	if variable == nil {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "Variable '"+varName+"' no encontrada")
		return value.DefaultNilValue
	}

	// Empieza con el valor de la variable
	valueRef := variable.Value

	for i := 1; i < len(ids); i++ {
		attr := ids[i]

		structVal, ok := valueRef.(*value.StructValue)
		if !ok {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "No se puede acceder a '"+attr+"' porque '"+ids[i-1]+"' no es un struct")
			return value.DefaultNilValue
		}

		val, ok := structVal.Instance.Fields[attr]
		if !ok {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "El atributo '"+attr+"' no existe en el struct '"+structVal.Instance.StructName+"'")
			return value.DefaultNilValue
		}

		// Actualiza el valor de referencia
		valueRef = val
	}

	return valueRef
}

// Expresiones con parentesis
func (v *ReplVisitor) VisitParensExpr(ctx *compiler.ParensExprContext) interface{} {
	fmt.Print("El valor de ParenExp es: " + ctx.GetText() + "\n")
	return v.Visit(ctx.Expression())
}

// Expresiones con vectores
func (v *ReplVisitor) VisitVectorItemExpr(ctx *compiler.VectorItemExprContext) interface{} {

	switch itemRef := v.Visit(ctx.Vect_item()).(type) {
	case *VectorItemReference:
		return itemRef.Value
	case *MatrixItemReference:
		return itemRef.Value
	case *VectorValue:
		return itemRef
	}
	return value.DefaultNilValue
}

// Expresiones con vectores
func (v *ReplVisitor) VisitVectorExpr(ctx *compiler.VectorExprContext) interface{} {
	return v.Visit(ctx.Vect_expr())
}

// Funciones con expresiones
func (v *ReplVisitor) VisitFuncCallExpr(ctx *compiler.FuncCallExprContext) interface{} {
	return v.Visit(ctx.Func_call())
}

func (v *ReplVisitor) VisitUnaryExpr(ctx *compiler.UnaryExprContext) interface{} {

	exp := v.Visit(ctx.Expression()).(value.IVOR)

	strat, ok := UnaryStrats[ctx.GetOp().GetText()]

	if !ok {
		log.Fatal("Unary operator not found")
	}

	ok, msg, result := strat.Validate(exp)

	if !ok {
		v.ErrorTable.NewSemanticError(ctx.GetOp(), msg)
		return value.DefaultNilValue
	}

	return result

}

func (v *ReplVisitor) VisitBinaryExpr(ctx *compiler.BinaryExprContext) interface{} {

	op := ctx.GetOp().GetText()
	left := v.Visit(ctx.GetLeft()).(value.IVOR)

	earlyCheck, ok := EarlyReturnStrats[op]

	if ok {
		ok, _, result := earlyCheck.Validate(left)

		if ok {
			return result
		}
	}

	//
	right := v.Visit(ctx.GetRight()).(value.IVOR)

	// Si right es un IVOR, lo convertimos a IVOR

	strat, ok := BinaryStrats[op]

	if !ok {
		log.Fatal("Binary operator not found")
	}

	ok, msg, result := strat.Validate(left, right)

	if !ok {
		v.ErrorTable.NewSemanticError(ctx.GetOp(), msg)
		return value.DefaultNilValue
	}

	return result
}

func (v *ReplVisitor) VisitIfStmt(ctx *compiler.IfStmtContext) interface{} {

	runChain := true

	for _, ifStmt := range ctx.AllIf_chain() {

		runChain = !v.Visit(ifStmt).(bool)

		if !runChain {
			break
		}
	}

	if runChain && ctx.Else_stmt() != nil {
		v.Visit(ctx.Else_stmt())
	}

	return nil
}

func (v *ReplVisitor) VisitIfChain(ctx *compiler.IfChainContext) interface{} {

	condition := v.Visit(ctx.Expression()).(value.IVOR)

	if condition.Type() != value.IVOR_BOOL {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "La condicion del if debe ser un booleano")
		return false

	}

	if condition.(*value.BoolValue).InternalValue {

		// Push scope
		v.ScopeTrace.PushScope("if")

		for _, stmt := range ctx.AllStmt() {
			v.Visit(stmt)
		}

		// Pop scope
		v.ScopeTrace.PopScope()

		return true
	}

	return false
}

func (v *ReplVisitor) VisitElseStmt(ctx *compiler.ElseStmtContext) interface{} {

	// Push scope
	v.ScopeTrace.PushScope("else")

	for _, stmt := range ctx.AllStmt() {
		v.Visit(stmt)
	}

	// Pop scope
	v.ScopeTrace.PopScope()

	return nil
}

func (v *ReplVisitor) VisitForStmtCond(ctx *compiler.ForStmtCondContext) interface{} {
	condition := ctx.Expression()

	forItem := &CallStackItem{ReturnValue: value.DefaultNilValue, Type: []string{BreakItem, ContinueItem}}
	v.CallStack.Push(forItem)
	v.ScopeTrace.PushScope("for_cond")

	defer func() {
		v.ScopeTrace.PopScope()
		v.CallStack.Clean(forItem)
	}()

	for {
		condValue, ok := v.Visit(condition).(value.IVOR)
		if !ok {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "Error evaluando la condición del for")
			return nil
		}

		if condValue.Type() != value.IVOR_BOOL {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "La condición del for debe ser un booleano")
			return nil
		}

		boolVal := condValue.Value().(bool)
		if !boolVal {
			break
		}

		// Variable para controlar el flujo
		shouldBreak := false
		shouldContinue := false

		// Defer para capturar continue/break dentro del cuerpo del bucle
		func() {
			defer func() {
				if item, ok := recover().(*CallStackItem); item != nil && ok {
					// Si no es el for actual, propaga el panic hacia arriba
					if item != forItem {
						panic(item)
					}

					// Si es un continue, marcamos para continuar
					if item.IsAction(ContinueItem) {
						item.ResetAction()
						shouldContinue = true
						return
					}

					// Si es un break, marcamos para terminar
					if item.IsAction(BreakItem) {
						item.ResetAction()
						shouldBreak = true
						return
					}
				}
			}()

			// Ejecutar todas las statements del cuerpo del bucle
			for _, stmt := range ctx.AllStmt() {
				v.Visit(stmt)
			}
		}()

		// Verificar las acciones después de ejecutar el cuerpo
		if shouldBreak {
			break
		}
		if shouldContinue {
			continue // Saltar a la siguiente iteración
		}
	}

	return nil
}

func (v *ReplVisitor) VisitForAssCond(ctx *compiler.ForAssCondContext) interface{} {
	// Obtener las tres partes del for: inicialización, condición e incremento
	initAssign := ctx.Assign_stmt()    // i = 0
	condition := ctx.Expression(0)     // i < 5  (primera expresión)
	incrementExpr := ctx.Expression(1) // i++    (segunda expresión)

	// Crear nuevo scope para el for
	v.ScopeTrace.PushScope("for_assignment")

	// Ejecutar la inicialización (i = 0)
	v.Visit(initAssign)

	// Crear item para manejo de break/continue
	forItem := &CallStackItem{
		ReturnValue: value.DefaultNilValue,
		Type:        []string{BreakItem, ContinueItem},
	}
	v.CallStack.Push(forItem)

	defer func() {
		v.ScopeTrace.PopScope()    // Limpiar scope
		v.CallStack.Clean(forItem) // Limpiar call stack
	}()

	// Bucle principal
	for {
		// Evaluar condición (i < 5)
		condValue := v.Visit(condition)
		if condValue == nil {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "Error evaluando la condición del for")
			break
		}

		condIVOR, ok := condValue.(value.IVOR)
		if !ok {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "La condición del for debe evaluar a un valor IVOR")
			break
		}

		// Verificar que la condición sea booleana
		if condIVOR.Type() != value.IVOR_BOOL {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "La condición del for debe ser un booleano")
			break
		}

		// Obtener valor booleano
		boolVal := condIVOR.Value().(bool)
		if !boolVal {
			break // Condición falsa, salir del bucle
		}

		// Variables para controlar el flujo
		shouldBreak := false
		shouldContinue := false

		// Ejecutar cuerpo del bucle con manejo de continue/break
		func() {
			defer func() {
				if item, ok := recover().(*CallStackItem); item != nil && ok {
					// Si no es nuestro forItem, propagar panic hacia arriba
					if item != forItem {
						panic(item)
					}

					// Si es continue, marcamos para continuar
					if item.IsAction(ContinueItem) {
						item.ResetAction()
						shouldContinue = true
						return
					}

					// Si es break, marcamos para terminar
					if item.IsAction(BreakItem) {
						item.ResetAction()
						shouldBreak = true
						return
					}
				}
			}()

			// Ejecutar todas las statements del cuerpo del bucle
			for _, stmt := range ctx.AllStmt() {
				v.Visit(stmt)
			}
		}()

		// Verificar las acciones después de ejecutar el cuerpo
		if shouldBreak {
			break
		}
		if shouldContinue {
			// Ejecutar incremento antes de continuar a la siguiente iteración
			v.Visit(incrementExpr)
			continue
		}

		// Ejecutar incremento (i++) para ejecución normal
		v.Visit(incrementExpr)
	}

	return nil
}

func (v *ReplVisitor) VisitReturnStmt(ctx *compiler.ReturnStmtContext) interface{} {

	exits, item := v.CallStack.IsReturnEnv()

	if !exits {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "La sentencia return debe estar dentro de una funcion")
		return nil
	}

	item.ReturnValue = value.DefaultNilValue
	item.Action = ReturnItem

	if ctx.Expression() != nil {
		item.ReturnValue = v.Visit(ctx.Expression()).(value.IVOR)
	}

	panic(item)
}

func (v *ReplVisitor) VisitBreakStmt(ctx *compiler.BreakStmtContext) interface{} {

	exits, item := v.CallStack.IsBreakEnv()

	if !exits {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "La sentencia break debe estar dentro de un ciclo o un switch")
		return nil
	}

	item.Action = BreakItem
	panic(item)
}

func (v *ReplVisitor) VisitContinueStmt(ctx *compiler.ContinueStmtContext) interface{} {

	exits, item := v.CallStack.IsContinueEnv()

	if !exits {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "La sentencia continue debe estar dentro de un ciclo")
		return nil
	}

	item.Action = ContinueItem
	panic(item)
}

func (v *ReplVisitor) VisitFuncCall(ctx *compiler.FuncCallContext) interface{} {

	canditateName := v.Visit(ctx.Id_pattern()).(string)
	funcObj, msg1 := v.ScopeTrace.GetFunction(canditateName)
	structObj, msg2 := v.ScopeTrace.GlobalScope.GetStruct(canditateName)

	if funcObj == nil && structObj == nil {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), msg1+msg2)
		return value.DefaultNilValue
	}

	args := make([]*Argument, 0)
	if ctx.Arg_list() != nil {
		fmt.Printf("🔹 Visitando Arg_list: %s\n", ctx.Arg_list().GetText())
		args = v.Visit(ctx.Arg_list()).([]*Argument)
	}

	// Aca van estrcuturas
	if structObj != nil {
		if IsArgValidForStruct(args) {
			return NewObjectValue(v, canditateName, ctx.Id_pattern().GetStart(), args, false)
		} else {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "Si bien "+canditateName+" es un struct, no se puede llamar a su constructor con los argumentos especificados. Ni tampoco es una funcion.")
			return value.DefaultNilValue
		}
	}

	switch funcObj := funcObj.(type) {
	case *BuiltInFunction:
		returnValue, ok, msg := funcObj.Exec(v.GetReplContext(), args)

		if !ok {

			if msg != "" {
				v.ErrorTable.NewSemanticError(ctx.GetStart(), msg)
			}

			return value.DefaultNilValue

		}

		return returnValue

	case *Function:
		funcObj.Exec(v, args, ctx.GetStart())
		return funcObj.ReturnValue

	case *ObjectBuiltInFunction:
		funcObj.Exec(v, args, ctx.GetStart())
		return funcObj.ReturnValue

	default:
		log.Fatal("Function type not found")
	}

	return value.DefaultNilValue
}

func (v *ReplVisitor) VisitArgList(ctx *compiler.ArgListContext) interface{} {

	args := make([]*Argument, 0)

	for _, arg := range ctx.AllFunc_arg() {
		// Visualizar lo que tiene arg
		fmt.Printf("🔹 Visitando FuncArg: %s\n", arg.GetText())
		args = append(args, v.Visit(arg).(*Argument))
	}

	return args

}

func (v *ReplVisitor) VisitFuncArg(ctx *compiler.FuncArgContext) interface{} {
	fmt.Printf("🔹 Visitando FuncArg: %s\n", ctx.GetText())
	argName := ""
	passByReference := false

	var argValue value.IVOR = value.DefaultNilValue
	var argVariableRef *Variable = nil

	if ctx.Id_pattern() != nil {
		// Because is a reference to a variable, the treatment is a bit different
		argName = ctx.Id_pattern().GetText()
		argVariableRef = v.ScopeTrace.GetVariable(argName)

		if argVariableRef != nil {
			argValue = argVariableRef.Value
		} else {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "Variable "+argName+" no encontrada")
		}
	} else {
		val := v.Visit(ctx.Expression())
		fmt.Printf("Tipo retornado por v.Visit(ctx.Expression()): %T\n", val)
		ivor, ok := val.(value.IVOR)
		if !ok {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "El argumento no es un valor válido")
			argValue = value.DefaultNilValue
		} else {
			argValue = ivor
		}
	}

	if ctx.ID() != nil {
		argName = ctx.ID().GetText()
	}

	return &Argument{
		Name:            argName,
		Value:           argValue,
		PassByReference: passByReference,
		Token:           ctx.GetStart(),
		VariableRef:     argVariableRef,
	}

}

func (v *ReplVisitor) VisitFuncDecl(ctx *compiler.FuncDeclContext) interface{} {

	if v.ScopeTrace.CurrentScope == v.ScopeTrace.GlobalScope {
		// aready declared by dcl_visitor
		return nil
	}

	if v.ScopeTrace.CurrentScope != v.ScopeTrace.GlobalScope && !v.ScopeTrace.CurrentScope.isStruct {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "Las funciones solo pueden ser declaradas en el scope global o en un struct")
	}

	funcName := ctx.ID().GetText()

	params := make([]*Param, 0)

	if ctx.Param_list() != nil {
		params = v.Visit(ctx.Param_list()).([]*Param)
	}

	if len(params) > 0 {

		baseParamType := params[0].ParamType()

		for _, param := range params {
			if param.ParamType() != baseParamType {
				v.ErrorTable.NewSemanticError(param.Token, "Todos los parametros de la funcion deben ser del mismo tipo")
				return nil
			}
		}
	}

	returnType := value.IVOR_NIL
	var returnTypeToken antlr.Token = nil

	if ctx.Type_() != nil {
		returnType = v.Visit(ctx.Type_()).(string)
		returnTypeToken = ctx.Type_().GetStart()
	}

	body := ctx.AllStmt()

	function := &Function{ // pointer ?
		Name:            funcName,
		Param:           params,
		ReturnType:      returnType,
		Body:            body,
		DeclScope:       v.ScopeTrace.CurrentScope,
		ReturnTypeToken: returnTypeToken,
		Token:           ctx.GetStart(),
	}

	ok, msg := v.ScopeTrace.AddFunction(funcName, function)

	if !ok {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), msg)
		return nil
	}

	return function
}

func (v *ReplVisitor) VisitParamList(ctx *compiler.ParamListContext) interface{} {

	params := make([]*Param, 0)

	for _, param := range ctx.AllFunc_param() {
		params = append(params, v.Visit(param).(*Param))
	}

	return params
}

func (v *ReplVisitor) VisitFuncParam(ctx *compiler.FuncParamContext) interface{} {

	externName := ""
	innerName := ""

	// at least ID(0) is defined
	// only 1 ID defined
	if ctx.ID() == nil {
		// innerName : type
		// _ : type
		innerName = "_"
	} else {
		// externName innerName : type
		externName = "_"
		innerName = ctx.ID().GetText()
	}

	passByReference := false

	paramType := v.Visit(ctx.Type_()).(string)

	return &Param{
		ExternName:      externName,
		InnerName:       innerName,
		PassByReference: passByReference,
		Type:            paramType,
		Token:           ctx.GetStart(),
	}

}

func (v *ReplVisitor) VisitSwitchStmt(ctx *compiler.SwitchStmtContext) interface{} {

	mainValue := v.Visit(ctx.Expression()).(value.IVOR)

	v.ScopeTrace.PushScope("switch")

	// Push break switchItem to call stack [breakable]
	switchItem := &CallStackItem{
		ReturnValue: value.DefaultNilValue,
		Type: []string{
			BreakItem,
		},
	}

	v.CallStack.Push(switchItem)

	// handle break statements from call stack
	defer func() {

		v.ScopeTrace.PopScope()       // pop switch scope
		v.CallStack.Clean(switchItem) // clean item if it's still in call stack

		if item, ok := recover().(*CallStackItem); item != nil && ok {

			// Not a switch item, propagate panic
			if item != switchItem {
				panic(item)
			}

			return // break
		}
	}()

	visited := false

	// evaluate cases
	for _, switchCase := range ctx.AllSwitch_case() {

		caseValue := v.GetCaseValue(switchCase)

		// ? use binary strat
		if caseValue.Type() != mainValue.Type() {
			// warning
			continue
		}

		if caseValue.Value() == mainValue.Value() {
			v.Visit(switchCase)
			visited = true
			break // implicit break
		}

	}

	// evaluate default
	if ctx.Default_case() != nil && !visited {
		v.Visit(ctx.Default_case())
	}

	return nil
}

func (v *ReplVisitor) GetCaseValue(tree antlr.ParseTree) value.IVOR {

	switch val := tree.(type) {
	case *compiler.SwitchCaseContext:
		return v.Visit(val.Expression()).(value.IVOR)
	default:
		return nil
	}

}

func (v *ReplVisitor) VisitSwitchCase(ctx *compiler.SwitchCaseContext) interface{} {

	// * all cases inside switch case will share the same scope

	for _, stmt := range ctx.AllStmt() {
		v.Visit(stmt)
	}
	return nil
}

func (v *ReplVisitor) VisitDefaultCase(ctx *compiler.DefaultCaseContext) interface{} {
	for _, stmt := range ctx.AllStmt() {
		v.Visit(stmt)
	}
	return nil
}

func (v *ReplVisitor) VisitBlockInd(ctx *compiler.BlockIndContext) interface{} {
	fmt.Printf("🔹 Visitando BlockInd (bloque independiente): %s\n", ctx.GetText())

	// Push scope para crear un nuevo ámbito local
	v.ScopeTrace.PushScope("block")

	// Ejecutar todas las sentencias dentro del bloque
	for _, stmt := range ctx.AllStmt() {
		v.Visit(stmt)
	}

	// Pop scope para restaurar el ámbito anterior
	v.ScopeTrace.PopScope()

	return nil
}

func (v *ReplVisitor) VisitForStmt(ctx *compiler.ForStmtContext) interface{} {

	indexName := ctx.ID(0).GetText()
	valueName := ctx.ID(1).GetText()

	iterableValue := v.Visit(ctx.Expression()).(value.IVOR)

	var iterableItem *VectorValue

	if IsVectorType(iterableValue.Type()) {
		iterableItem = iterableValue.(*VectorValue)
	} else if iterableValue.Type() == value.IVOR_STRING {
		iterableItem = StringToVector(iterableValue.(*value.StringValue))
	} else {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "El valor del for debe ser un vector o una cadena")
		return nil
	}

	if iterableItem.Size() == 0 {
		return nil
	}

	outerForScope := v.ScopeTrace.PushScope("outer_for")

	// Declarar índice y valor
	indexVar, msg1 := outerForScope.AddVariable(indexName, value.IVOR_INT, &value.IntValue{InternalValue: 0}, true, false, ctx.ID(0).GetSymbol())
	valueVar, msg2 := outerForScope.AddVariable(valueName, iterableItem.ItemType, iterableItem.Current(), true, false, ctx.ID(1).GetSymbol())

	if indexVar == nil || valueVar == nil {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), msg1+" "+msg2)
		log.Fatal("Error declarando variables del for")
		return nil
	}

	forItem := &CallStackItem{
		ReturnValue: value.DefaultNilValue,
		Type:        []string{BreakItem, ContinueItem},
	}

	v.CallStack.Push(forItem)
	innerForScope := v.ScopeTrace.PushScope("inner_for")

	v.VisitInnerForWithIndex(ctx, outerForScope, innerForScope, forItem, iterableItem, indexVar, valueVar)

	iterableItem.Reset()
	v.ScopeTrace.PopScope()
	v.ScopeTrace.PopScope()
	v.CallStack.Clean(forItem)
	return nil
}

func (v *ReplVisitor) VisitInnerForWithIndex(ctx *compiler.ForStmtContext, outerForScope *BaseScopeTrace, innerForScope *BaseScopeTrace, forItem *CallStackItem, iterableItem *VectorValue, indexVar *Variable, valueVar *Variable) {

	defer func() {
		innerForScope.Reset()
		if item, ok := recover().(*CallStackItem); item != nil && ok {
			if item != forItem {
				panic(item)
			}
			if item.IsAction(ContinueItem) {
				item.ResetAction()
				iterableItem.Next()
				v.VisitInnerForWithIndex(ctx, outerForScope, innerForScope, forItem, iterableItem, indexVar, valueVar)
			}
			if item.IsAction(BreakItem) {
				return
			}
		}
	}()

	for iterableItem.CurrentIndex < iterableItem.Size() {
		indexVar.Value = &value.IntValue{InternalValue: iterableItem.CurrentIndex}
		valueVar.Value = iterableItem.Current()

		for _, stmt := range ctx.AllStmt() {
			v.Visit(stmt)
		}

		iterableItem.Next()
		innerForScope.Reset()
	}
}

// Structs
func (v *ReplVisitor) VisitStructDecl(ctx *compiler.StructDeclContext) interface{} {
	if v.ScopeTrace.CurrentScope != v.ScopeTrace.GlobalScope {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "Los structs solo pueden ser declaradas en el scope global")
		return nil
	}

	structAdded, msg := v.ScopeTrace.GlobalScope.AddStruct(ctx.ID().GetText(), &Struct{
		Name:   ctx.ID().GetText(),
		Fields: ctx.AllStruct_prop(),
		Token:  ctx.GetStart(),
	})

	if !structAdded {
		v.ErrorTable.NewSemanticError(ctx.ID().GetSymbol(), msg)
	}

	return nil
}

func (v *ReplVisitor) VisitStructAttr(ctx *compiler.StructAttrContext) interface{} {
	varName := ctx.ID().GetText()
	explicitType := ""
	finalType := ""
	var varValue value.IVOR = value.DefaultUnInitializedValue

	if ctx.Type_() != nil {
		explicitType = v.Visit(ctx.Type_()).(string)
		finalType = explicitType
	} else {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "Los atributos de un struct deben tener tipo explícito")
		return nil
	}

	variable, msg := v.ScopeTrace.AddVariable(varName, finalType, varValue, true, true, ctx.ID().GetSymbol())

	if variable == nil {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), msg)
	}

	return nil
}

type StructInstance struct {
	StructName string
	Fields     map[string]value.IVOR
}

func (v *ReplVisitor) VisitStructInstantiationExpr(ctx *compiler.StructInstantiationExprContext) interface{} {
	idToken := ctx.ID()
	if idToken == nil {
		log.Println("Error: no se pudo obtener el ID del struct")
		return nil
	}
	structName := idToken.GetText()
	log.Printf("Nombre del struct a instanciar: %s\n", structName)

	params := ctx.Struct_param_list()
	fieldsMap := make(map[string]value.IVOR)

	if params != nil {
		i := 0
		for {
			paramCtx := params.Struct_param(i)
			if paramCtx == nil {
				break
			}
			paramName := paramCtx.ID().GetText()
			exprValue := v.Visit(paramCtx.Expression()).(value.IVOR)
			log.Printf("Param: %s = %v\n", paramName, exprValue)
			fieldsMap[paramName] = exprValue
			i++
		}
	}

	structValue := &value.StructValue{
		Instance: &value.StructInstance{
			StructName: structName,
			Fields:     fieldsMap,
		},
	}

	log.Println("Instancia creada correctamente:", structValue.ToString())
	return structValue
}

func (v *ReplVisitor) VisitStruct_param(ctx *compiler.Struct_paramContext) interface{} {
	fieldName := ctx.ID().GetText()
	val := v.Visit(ctx.Expression()).(value.IVOR)

	return &StructFieldValue{
		Name:  fieldName,
		Value: val,
	}
}

type StructFieldValue struct {
	Name  string
	Value value.IVOR
}

// Declaracion de matrices
// Ejemplo: matrix [][]int = { {1,2,3}, {4,5,6}, {7,8,9} }
func (v *ReplVisitor) VisitVarMatrixDecl(ctx *compiler.VarMatrixDeclContext) interface{} {
	fmt.Printf("🔹 Visitando VarMatrixDecl: %s\n", ctx.GetText())

	isConst := false

	// Obtener información
	varName := ctx.ID().GetText()
	matrixType := v.Visit(ctx.Matrix_type()).(string)
	matrixValue := v.Visit(ctx.Matrix_expr()).(value.IVOR)

	fmt.Printf("   Variable: '%s'\n", varName)
	fmt.Printf("   Tipo Matrix: '%s'\n", matrixType)
	fmt.Printf("   Valor Matrix: %v (tipo: %s)\n", matrixValue, matrixValue.Type())

	// Validar tipo
	if !IsMatrixType(matrixType) {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "El tipo '"+matrixType+"' no es un tipo de matriz válido")
		return nil
	}

	// Validar tipo de datos dentro de la matriz
	if matrixValue.Type() != matrixType && matrixValue.Type() != "[][]" {
		if IsMatrixType(matrixValue.Type()) {
			declaredItemType := RemoveMatrixBrackets(matrixType)
			valueItemType := RemoveMatrixBrackets(matrixValue.Type())

			if declaredItemType != valueItemType {
				v.ErrorTable.NewSemanticError(ctx.GetStart(), "No se puede asignar una matriz de tipo '"+matrixValue.Type()+"' a '"+matrixType+"'")
				return nil
			}
		} else {
			v.ErrorTable.NewSemanticError(ctx.GetStart(), "No se puede asignar un valor de tipo '"+matrixValue.Type()+"' a '"+matrixType+"'")
			return nil
		}
	}

	// Copia de matrices (muy recomendable para evitar referencias compartidas)
	if IsMatrixType(matrixValue.Type()) {
		matrixValue = matrixValue.Copy()
	}

	// Agregar al scope
	variable, msg := v.ScopeTrace.AddVariable(varName, matrixType, matrixValue, isConst, false, ctx.GetStart())

	if variable == nil {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), msg)
		return nil
	}

	fmt.Printf("✅ Variable matriz '%s' declarada exitosamente con tipo '%s'\n", varName, matrixType)
	return nil
}

// Procesamiento de la expresion literal de la matriz
// Ejemplo: { {1,2,3}, {4,5,6}, {7,8,9} }
func (v *ReplVisitor) VisitMatrixItemList(ctx *compiler.MatrixItemListContext) interface{} {
	fmt.Printf("🔹 Visitando MatrixItemList: %s\n", ctx.GetText())

	var matrixItems [][]value.IVOR
	var innerType string = value.IVOR_NIL

	for i, row := range ctx.AllVect_expr() {
		rowValue := v.Visit(row).(*VectorValue)
		matrixItems = append(matrixItems, rowValue.InternalValue)

		if i == 0 {
			innerType = rowValue.ItemType
		} else {
			if rowValue.ItemType != innerType {
				v.ErrorTable.NewSemanticError(ctx.GetStart(), "Todos los elementos de la matriz deben ser del mismo tipo")
				return value.DefaultNilValue
			}
		}
	}

	_type := "[[]]" + innerType

	if IsMatrixType(_type) {
		return NewMatrixValue(matrixItems, _type, innerType)
	}

	v.ErrorTable.NewSemanticError(ctx.GetStart(), "Tipo "+_type+" no encontrado")
	return value.DefaultNilValue
}

func (v *ReplVisitor) VisitMatrix_type(ctx *compiler.Matrix_typeContext) interface{} {
	// Extraemos el tipo base
	baseType := ctx.ID().GetText()

	// Formamos el tipo completo de matriz
	return "[[]]" + baseType
}
