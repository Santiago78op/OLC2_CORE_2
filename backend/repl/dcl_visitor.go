package repl

import (
	"fmt"
	"log"

	"github.com/antlr4-go/antlr/v4"
	compiler "main.go/grammar"
	"main.go/value"
)

type DclVisitor struct {
	compiler.BaseVLangGrammarVisitor
	ScopeTrace  *ScopeTrace
	ErrorTable  *ErrorTable
	StructNames []string
}

func NewDclVisitor(errorTable *ErrorTable) *DclVisitor {
	return &DclVisitor{
		ScopeTrace:  NewScopeTrace(),
		ErrorTable:  errorTable,
		StructNames: []string{},
	}
}

func (v *DclVisitor) Visit(tree antlr.ParseTree) interface{} {
	fmt.Printf("🔹 DclVisitor.Visit llamado con: %T\n", tree)

	switch val := tree.(type) {
	case *antlr.ErrorNodeImpl:
		fmt.Printf("❌ ERROR NODE ENCONTRADO: %s\n", val.GetText())
		log.Fatal(val.GetText()) // ⚠️ AQUÍ SE PUEDE ESTAR DETENIENDO
		return nil
	default:
		fmt.Printf("🔹 Aceptando tree con visitor\n")
		return tree.Accept(v)
	}
}

func (v *DclVisitor) VisitProgram(ctx *compiler.ProgramContext) interface{} {
	fmt.Printf("🔹 DclVisitor.VisitProgram EJECUTADO\n")
	fmt.Printf("🔹 Número de statements: %d\n", len(ctx.AllStmt()))

	for _, stmt := range ctx.AllStmt() {
		fmt.Printf("🔹 DclVisitor procesando stmt: %s\n", stmt.GetText())
		v.Visit(stmt)
	}
	return nil
}

func (v *DclVisitor) VisitStmt(ctx *compiler.StmtContext) interface{} {

	if ctx.Func_dcl() != nil {
		v.Visit(ctx.Func_dcl())
	} else if ctx.Strct_dcl() != nil {
		v.Visit(ctx.Strct_dcl())
	}

	return nil
}

func (v *DclVisitor) VisitFuncDecl(ctx *compiler.FuncDeclContext) interface{} {

	if v.ScopeTrace.CurrentScope != v.ScopeTrace.GlobalScope {
		v.ErrorTable.NewSemanticError(ctx.GetStart(), "Las funciones solo pueden ser declaradas en el scope global")
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
		returnType = ctx.Type_().GetText()
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
	}

	return nil
}

func (v *DclVisitor) VisitParamList(ctx *compiler.ParamListContext) interface{} {

	params := make([]*Param, 0)

	for _, param := range ctx.AllFunc_param() {
		params = append(params, v.Visit(param).(*Param))
	}

	return params
}

func (v *DclVisitor) VisitFuncParam(ctx *compiler.FuncParamContext) interface{} {

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

	paramType := ctx.Type_().GetText()

	return &Param{
		ExternName:      externName,
		InnerName:       innerName,
		PassByReference: passByReference,
		Type:            paramType,
		Token:           ctx.GetStart(),
	}

}

func (v *DclVisitor) VisitStructDecl(ctx *compiler.StructDeclContext) interface{} {
	v.StructNames = append(v.StructNames, ctx.ID().GetText())
	return nil
}
