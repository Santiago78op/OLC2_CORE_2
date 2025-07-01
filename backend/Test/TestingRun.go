/*
	TestingRun.go es un archivo que contiene la lógica para ejecutar pruebas de código en un entorno de backend.
*/

package Test

/*


// TestingRun es una función que simula la ejecución de pruebas de código.
func TestingRun(code string) (string, error) {
	// Aquí podrías agregar la lógica para ejecutar el código y devolver el resultado.
	// Por ahora, simplemente retornamos el código recibido como una simulación.
	if code == "" {
		return "", fmt.Errorf("el código no puede estar vacío")
	}

	// 1. Análisis Léxico
	// Para verificar errores
	//lexicalErrorListener := errors.NewLexicalErrorListener()
	//
	lexer := compiler.NewVLangParserLexer(antlr.NewInputStream(code))

	lexer.RemoveErrorListeners()
	//lexer.AddErrorListener(lexicalErrorListener)

	// 2. Tokens
	stream := antlr.NewCommonTokenStream(lexer, antlr.TokenDefaultChannel)

	// 3. Parser + errores sintácticos
	// New<Nombre de mi gramatica>(Stream)
	parser := compiler.NewVLangParserParser(stream)
	parser.BuildParseTrees = true

	tree := parser.Prog()

	evaluator := NewVLangVisitor()
	resultado := evaluator.Visit(tree)

	fmt.Printf("Resultado de la evaluación: %v\n", resultado)

	return fmt.Sprintf("Código ejecutado: %s", code), nil
}


// Implementar visitor
type VLangVisitor struct {
	*compiler.BaseVLangParserVisitor
}

// Es como un constructor para el visitor
func NewVLangVisitor() *VLangVisitor {
	return &VLangVisitor{
		BaseVLangParserVisitor: &compiler.BaseVLangParserVisitor{},
	}
}

func (v *VLangVisitor) VisitMulDiv(ctx *compiler.BinaryExprContext) interface{} {
	izq := v.Visit(ctx.Expr(0)).(int)
	der := v.Visit(ctx.Expr(1)).(int)
	op := ctx.GetText()
	fmt.Printf("Visitando operación: %s %s %s\n", izq, op, der)

	return op
}

// Vistor de la expresion
func (v *VLangVisitor) VisitExpr(ctx *compiler.ExprContext) interface{} {
	if ctx.GetChildCount() == 1 {
		// Si solo hay un hijo, es un número
		num := ctx.GetChild(0).(antlr.TerminalNode).GetSymbol().GetText()
		fmt.Printf("Visitando número: %s\n", num)
		return num // Retorna el número como resultado
	}
	return nil
}

func (v *VLangVisitor) Visit(tree antlr.ParseTree) interface{} {

	switch val := tree.(type) {
	case *antlr.ErrorNodeImpl:
		log.Fatal(val.GetText())
		return nil
	default:
		return tree.Accept(v) //<---- devolvemos el metodo recursivo que nos da el arbol
	}

}

*/
