package repl

import (
	"fmt"

	"github.com/antlr4-go/antlr/v4"
	compiler "main.go/grammar"
	"main.go/value"
)

type Function struct {
	Name            string
	Param           []*Param
	ReturnType      string
	ReturnTypeToken antlr.Token
	Body            []compiler.IStmtContext
	DeclScope       *BaseScopeTrace
	ReturnValue     value.IVOR
	IsMutating      bool
	DefaultScope    *BaseScopeTrace
	Token           antlr.Token
}

func (f *Function) Value() interface{} {
	return f
}

func (f *Function) Type() string {
	return value.IVOR_FUNCTION
}

func (f *Function) Copy() value.IVOR {
	return f
}

func (f *Function) Exec(visitor *ReplVisitor, args []*Argument, token antlr.Token) {

	context := visitor.GetReplContext()

	// validate args
	argsOk, argsMap := f.ValidateArgs(context, args, token)

	if !argsOk {
		f.ReturnValue = value.DefaultNilValue
		return
	}

	// create new scope
	initialScope := context.ScopeTrace.CurrentScope // save current scope, scope at call time

	if f.DefaultScope != nil {
		context.ScopeTrace.CurrentScope = f.DefaultScope // set function default scope as current scope
	} else {
		context.ScopeTrace.CurrentScope = f.DeclScope            // set function declaration scope as current scope
		context.ScopeTrace.PushScope("func: " + token.GetText()) // push a new function scope
	}

	wasMutating := context.ScopeTrace.CurrentScope.IsMutating
	context.ScopeTrace.CurrentScope.IsMutating = f.IsMutating

	// push return item to callstack
	funcItem := &CallStackItem{
		ReturnValue: value.DefaultNilValue,
		Type: []string{
			ReturnItem,
		},
	}
	context.CallStack.Push(funcItem)

	// ✅ CORRECCIÓN: handle return from callstack - PRIMERO recover, DESPUÉS cleanup
	defer func() {
		// 1. PRIMERO: Manejar panic/return
		if item, ok := recover().(*CallStackItem); item != nil && ok {

			if item != funcItem {
				context.ErrorTable.NewSemanticError(token, "Return invalido")
				f.ReturnValue = value.DefaultNilValue
			} else {
				// validate return type
				f.ValidateReturn(context, item.ReturnValue, token) // return value from return statement
			}
		} else {
			// No hay return explícito, usar valor por defecto
			f.ValidateReturn(context, value.DefaultNilValue, token)
		}

		// 2. DESPUÉS: Limpiar call stack y restaurar scope
		context.CallStack.Clean(funcItem)                        // clean callstack
		context.ScopeTrace.PopScope()                            // pop function scope
		context.ScopeTrace.CurrentScope.IsMutating = wasMutating // restore mutating flag
		context.ScopeTrace.CurrentScope = initialScope           // restore the call time scope
	}()

	// push args to scope
	for varName, arg := range argsMap {

		// special treatment for pass by reference
		if arg.PassByReference {

			if arg.VariableRef == nil {
				context.ErrorTable.NewSemanticError(arg.Token, "No es posible pasar por referencia un valor que no este asociado a una variable")
				f.ValidateReturn(context, value.DefaultNilValue, token)
				return
			}

			// create the pointer
			pointer := &PointerValue{
				AssocVariable: arg.VariableRef,
			}

			// add pointer to scope
			context.ScopeTrace.CurrentScope.AddVariable(varName, value.IVOR_POINTER, pointer, false, false, arg.Token)
			continue
		}

		context.ScopeTrace.CurrentScope.AddVariable(varName, arg.Value.Type(), arg.Value.Copy(), false, false, arg.Token)
	}

	// evaluate body
	for _, stmt := range f.Body {
		visitor.Visit(stmt)
	}
}

func (f *Function) ValidateArgs(context *ReplContext, args []*Argument, token antlr.Token) (bool, map[string]*Argument) {

	// validate arg count
	if len(args) != len(f.Param) {
		context.ErrorTable.NewSemanticError(token, "Numero de argumentos invalido")
		return false, nil
	}

	argsMap := make(map[string]*Argument)
	finalArgsMap := make(map[string]*Argument)

	for _, arg := range args {
		argsMap[arg.Name] = arg
	}

	errorFound := false

	for i, param := range f.Param {

		// determine param type
		var argToValidate *Argument = nil

		if param.ExternName == "" {
			// inner = arg name
			argToValidate = argsMap[param.InnerName]

		} else if param.ExternName == "_" {
			// positional arg
			argToValidate = args[i]
		} else {
			// extern = arg name
			argToValidate = argsMap[param.ExternName]
		}

		// validate arg exists
		if argToValidate == nil {
			context.ErrorTable.NewSemanticError(token, fmt.Sprintf("Argumento %s no especificado", param.InnerName))
			errorFound = true
			continue
		}

		// ✅ CORRECCIÓN: Usar conversión implícita en lugar de comparación directa
		if argToValidate.Value.Type() != param.Type && param.Type != value.IVOR_ANY {
			// Intentar conversión implícita
			convertedValue, canConvert := value.ImplicitCast(param.Type, argToValidate.Value)

			if canConvert {
				// Actualizar el argumento con el valor convertido
				argToValidate.Value = convertedValue
				fmt.Printf("🔄 DEBUG: Conversión implícita %s -> %s para parámetro %s\n",
					argToValidate.Value.Type(), param.Type, param.InnerName)
			} else {
				context.ErrorTable.NewSemanticError(token, fmt.Sprintf("Tipo de argumento %s invalido, esperado %s, recibido %s", param.InnerName, param.Type, argToValidate.Value.Type()))
				errorFound = true
				continue
			}
		}

		// validate pass by reference
		if argToValidate.PassByReference != param.PassByReference {
			context.ErrorTable.NewSemanticError(token, fmt.Sprintf("Argumento %s no es pasado por referencia", param.InnerName))
			errorFound = true
			continue
		}

		// add to final args map
		finalArgsMap[param.InnerName] = argToValidate
	}

	if errorFound {
		return false, nil
	}

	return true, finalArgsMap
}

func (f *Function) ValidateReturn(context *ReplContext, val value.IVOR, token antlr.Token) {

	if val.Type() != f.ReturnType {
		if f.ReturnTypeToken != nil {
			context.ErrorTable.NewSemanticError(f.ReturnTypeToken, fmt.Sprintf("Tipo de retorno invalido, se esperaba %s, se obtuvo %s", f.ReturnType, val.Type()))
		} else {
			context.ErrorTable.NewSemanticError(token, fmt.Sprintf("Tipo de retorno invalido, se esperaba %s, se obtuvo %s", f.ReturnType, val.Type()))
		}

		f.ReturnValue = value.DefaultNilValue
		return
	}

	f.ReturnValue = val
}
