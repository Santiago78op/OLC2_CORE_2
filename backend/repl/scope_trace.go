package repl

import (
	"log"
	"strings"

	"github.com/antlr4-go/antlr/v4"
	"main.go/value"
)

// BaseScopeTrace representa un ámbito en la traza de ejecución del REPL.
// Contiene información sobre el nombre del ámbito, su padre, sus hijos,
// las variables y funciones definidas en él. Esta estructura es útil para
// rastrear el contexto de ejecución y la visibilidad de las variables y funciones
// en diferentes niveles de anidamiento dentro del REPL.
type BaseScopeTrace struct {
	name       string                // Nombre del ámbito
	parent     *BaseScopeTrace       // Ámbito padre
	children   []*BaseScopeTrace     // Ámbitos hijos
	variables  map[string]*Variable  // Variables en el ámbito
	functions  map[string]value.IVOR // Funciones en el ámbito
	structs    map[string]*Struct    // Estructuras definidas en el ámbito
	IsMutating bool                  // Indica si el ámbito actual está en modo de mutación
	isStruct   bool                  // Indica si el ámbito actual es un ámbito de estructura
}

// Name devuelve el nombre del ámbito actual.
func (s *BaseScopeTrace) Name() string {
	return s.name
}

// Parent devuelve el ámbito padre del ámbito actual.
func (s *BaseScopeTrace) Parent() *BaseScopeTrace {
	return s.parent
}

// Children devuelve los ámbitos hijos del ámbito actual.
func (s *BaseScopeTrace) Children() []*BaseScopeTrace {
	return s.children
}

// Valida si el tipo de dato es válido para el ámbito actual.
func (s *BaseScopeTrace) ValidType(_type string) bool {

	_, isStructType := s.structs[_type]

	return value.IsPrimitiveType(_type) || isStructType
}

func (s *BaseScopeTrace) AddChild(child *BaseScopeTrace) {
	// Agrega un ámbito hijo al ámbito actual
	s.children = append(s.children, child)
	// Establece el ámbito padre del hijo como el ámbito actual
	child.parent = s
}

func (s *BaseScopeTrace) variableExists(variable *Variable) bool {
	// Verifica si una variable con el nombre dado ya existe en el ámbito
	if _, exists := s.variables[variable.Name]; exists {
		return true
	}

	// Si no existe, retorna false
	return false
}

func (s *BaseScopeTrace) AddVariable(name string, varType string, value value.IVOR, isConst bool, allowNil bool, token antlr.Token) (*Variable, string) {
	// Crea una nueva variable con los parámetros dados
	variable := &Variable{
		Name:     name,
		Type:     varType,
		Value:    value,
		IsConst:  isConst,
		AllowNil: allowNil,
		Token:    token,
	}

	// Verifica si la variable ya existe en el ámbito
	if s.variableExists(variable) {
		msg := "La variable '" + name + "' ya existe en el ámbito actual"
		return nil, msg
	}

	// Valida el tipo de la variable
	typesExists, msg := variable.TypeValidation()

	// Agrega la variable al mapa de variables del ámbito
	s.variables[name] = variable

	if !typesExists {
		// Si la validación falla, retorna nil y el mensaje de error
		return nil, msg
	}

	// Si la validación es exitosa, agrega la variable al ámbito
	return variable, ""
}

// GetVariable busca una variable por su nombre en el ámbito actual.
// Si la variable existe, retorna un puntero a la variable y true.
func (s *BaseScopeTrace) GetVariable(name string) *Variable {

	// Si el nombre contiene un punto, se asume que es una variable de objeto
	if strings.Contains(name, ".") {
		return s.searchObjectVariable(name, nil)
	}

	// Busca la variable en el ámbito actual
	initialScope := s

	for {
		if variable, ok := initialScope.variables[name]; ok {

			if variable.Type == value.IVOR_POINTER {
				// Si la variable es un puntero, retorna el valor apuntado
				return variable.Value.(*PointerValue).AssocVariable // Retorna el valor apuntado por el puntero
			}

			// Si la variable existe, retorna un puntero a la variable
			return variable
		}

		if initialScope.parent == nil {
			break // Si no hay un ámbito padre, termina la búsqueda
		}

		// Si no se encuentra la variable, sube al ámbito padre
		initialScope = initialScope.parent
	}

	// Si no se encuentra la variable, retorna nil
	return nil
}

func (s *BaseScopeTrace) searchObjectVariable(name string, lastObj value.IVOR) *Variable {

	// split name by dot
	parts := strings.Split(name, ".")

	if len(parts) == 0 {
		log.Fatal("idk what u did, cant split by dot")
		return nil
	}

	if len(parts) == 1 {
		obj, ok := lastObj.(*ObjectValue)

		if ok {
			return obj.InternalScope.GetVariable(name)
		}

		log.Fatal("idk what u did, cant convert to object")
		return nil
	}

	// then parts should be 2 or more

	if lastObj == nil {
		variable := s.GetVariable(parts[0])

		if variable == nil {
			return nil
		}

		obj := variable.Value

		// obj must be an object/struct or vector
		switch obj := obj.(type) {
		case *ObjectValue:
			lastObj = obj
		case *VectorValue:
			lastObj = obj.ObjectValue
		default:
			return nil
		}

		return s.searchObjectVariable(strings.Join(parts[1:], "."), lastObj)
	}

	obj, ok := lastObj.(*ObjectValue)

	if ok {
		lastObj = obj.InternalScope.GetVariable(parts[0]).Value

		return s.searchObjectVariable(strings.Join(parts[1:], "."), lastObj)
	} else {
		log.Fatal("idk what u did, cant convert to object")
		return nil
	}
}

func (s *BaseScopeTrace) AddFunction(name string, function value.IVOR) (bool, string) {
	// Verifica si la función ya existe en el ámbito actual
	if _, ok := s.functions[name]; ok {
		return false, "La funcion " + name + " ya existe"
	}

	s.functions[name] = function

	return true, ""
}

func (s *BaseScopeTrace) GetFunction(name string) (value.IVOR, string) {

	// verify if is refering to and object/struct function
	if strings.Contains(name, ".") {
		return s.searchObjectFunction(name, nil)
	}

	initialScope := s

	for {
		if function, ok := initialScope.functions[name]; ok {
			return function, ""
		}

		if initialScope.parent == nil {
			break
		}

		initialScope = initialScope.parent
	}

	return nil, "La funcion " + name + " no existe"
}

func (s *BaseScopeTrace) searchObjectFunction(name string, lastObj value.IVOR) (value.IVOR, string) {

	// split name by dot
	parts := strings.Split(name, ".")

	if len(parts) == 0 {
		log.Fatal("idk what u did, cant split by dot")
		return nil, ""
	}

	if len(parts) == 1 {
		obj, ok := lastObj.(*ObjectValue)

		if ok {
			return obj.InternalScope.GetFunction(name)
		}

		log.Fatal("idk what u did, cant convert to object")
		return nil, ""
	}

	// then parts should be 2 or more

	if lastObj == nil {
		variable := s.GetVariable(parts[0])

		if variable == nil {
			return nil, "No se puede acceder a la propiedad " + parts[0]
		}

		obj := variable.Value

		// obj must be an object/struct or vector

		switch obj := obj.(type) {
		case *ObjectValue:
			lastObj = obj
		case *VectorValue:
			lastObj = obj.ObjectValue
		default:
			return nil, "La propiedad '" + variable.Name + "' de tipo " + obj.Type() + " no tiene propiedades"
		}

		return s.searchObjectFunction(strings.Join(parts[1:], "."), lastObj)
	}

	obj, ok := lastObj.(*ObjectValue)

	if ok {
		lastObj = obj.InternalScope.GetVariable(parts[0]).Value

		return s.searchObjectFunction(strings.Join(parts[1:], "."), lastObj)
	} else {
		log.Fatal("idk what u did, cant convert to object")
		return nil, ""
	}
}

/*
func (s *BaseScopeTrace) searchObjectFunction(name string, lastObj value.IVOR) (value.IVOR, string) {

	// split name by dot
	parts := strings.Split(name, ".")

	if len(parts) == 0 {
		log.Fatal("idk what u did, cant split by dot")
		return nil, ""
	}

	if len(parts) == 1 {
		obj, ok := lastObj.(*ObjectValue)

		if ok {
			return obj.InternalScope.GetFunction(name)
		}

		log.Fatal("idk what u did, cant convert to object")
		return nil, ""
	}

	// then parts should be 2 or more

	if lastObj == nil {
		variable := s.GetVariable(parts[0])

		if variable == nil {
			return nil, "No se puede acceder a la propiedad " + parts[0]
		}

		obj := variable.Value

		// obj must be an object/struct or vector

		switch obj := obj.(type) {
		case *ObjectValue:
			lastObj = obj
		case *VectorValue:
			lastObj = obj.ObjectValue
		default:
			return nil, "La propiedad '" + variable.Name + "' de tipo " + obj.Type() + " no tiene propiedades"
		}

		return s.searchObjectFunction(strings.Join(parts[1:], "."), lastObj)
	}

	obj, ok := lastObj.(*ObjectValue)

	if ok {
		lastObj = obj.InternalScope.GetVariable(parts[0]).Value

		return s.searchObjectFunction(strings.Join(parts[1:], "."), lastObj)
	} else {
		log.Fatal("idk what u did, cant convert to object")
		return nil, ""
	}
}
*/

func (s *BaseScopeTrace) AddStruct(name string, structValue *Struct) (bool, string) {

	if _, ok := s.structs[name]; ok {
		return false, "La estructura " + name + " ya existe"
	}

	s.structs[name] = structValue
	return true, ""
}

func (s *BaseScopeTrace) GetStruct(name string) (*Struct, string) {

	initialScope := s

	for {
		if structValue, ok := initialScope.structs[name]; ok {
			return structValue, ""
		}

		if initialScope.parent == nil {
			break
		}

		initialScope = initialScope.parent
	}

	return nil, "La estructura " + name + " no existe"
}

// Reset reinicializa el ámbito actual, eliminando todas las variables y funciones definidas en él.
func (s *BaseScopeTrace) Reset() {
	s.variables = make(map[string]*Variable)
	s.children = make([]*BaseScopeTrace, 0)
	s.functions = make(map[string]value.IVOR)
}

// IsMutatingScope verifica si el ámbito actual o alguno de sus padres está en modo de mutación.
func (s *BaseScopeTrace) IsMutatingScope() bool {
	temp := s

	for {
		if temp.IsMutating {
			return true
		}

		if temp.parent == nil {
			break
		}

		temp = temp.parent
	}

	return false
}

// NewGlobalScopeTrace crea un nuevo ámbito global para el REPL.
// Este ámbito global es el punto de partida para todas las ejecuciones en el REPL.
// Se inicializa con un nombre específico y un mapa vacío de variables y funciones.
// Este ámbito es utilizado para almacenar variables y funciones globales que pueden ser accedidas desde cualquier parte del REPL.
func NewGlobalScope() *BaseScopeTrace {

	// Falta registra la contruccion de funciones
	funcs := make(map[string]value.IVOR)

	for k, v := range DefaultBuiltInFunctions {
		funcs[k] = v
	}

	// Crea un nuevo ámbito global con un nombre específico
	return &BaseScopeTrace{
		name:      "global",
		variables: make(map[string]*Variable),
		children:  make([]*BaseScopeTrace, 0),
		structs:   make(map[string]*Struct),
		parent:    nil,
		functions: funcs,
	}
}

// NewCurrentScopeTrace crea un nuevo ámbito local para el REPL.
// Este ámbito local es utilizado para almacenar variables y funciones que son
// específicas de una ejecución o contexto particular dentro del REPL.
// Se inicializa con un nombre específico y un mapa vacío de variables y funciones.
func NewLocalScope(name string) *BaseScopeTrace {
	return &BaseScopeTrace{
		name:      name,
		variables: make(map[string]*Variable),
		functions: make(map[string]value.IVOR),
		children:  make([]*BaseScopeTrace, 0),
		parent:    nil,
	}
}

// ScopeTrace representa la traza de ejecución del REPL, que incluye el ámbito global y el ámbito local.
type ScopeTrace struct {
	GlobalScope  *BaseScopeTrace // Ámbito global del REPL
	CurrentScope *BaseScopeTrace // Ámbito local del REPL
}

// PushScope crea un nuevo ámbito local dentro de la traza de ejecución del REPL.
func (s *ScopeTrace) PushScope(name string) *BaseScopeTrace {

	newScope := NewLocalScope(name)
	s.CurrentScope.AddChild(newScope)
	s.CurrentScope = newScope

	return s.CurrentScope
}

// PopScope elimina el ámbito local actual de la traza de ejecución del REPL,
func (s *ScopeTrace) PopScope() {
	s.CurrentScope = s.CurrentScope.Parent()
}

// Reset reinicializa el ámbito local actual, estableciendo el ámbito local al ámbito global.
func (s *ScopeTrace) Reset() {
	s.CurrentScope = s.GlobalScope
}

// AddVariable agrega una nueva variable al ámbito local actual de la traza de ejecución del REPL.
func (s *ScopeTrace) AddVariable(name string, varType string, value value.IVOR, isConst bool, allowNil bool, token antlr.Token) (*Variable, string) {
	return s.CurrentScope.AddVariable(name, varType, value, isConst, allowNil, token)
}

// GetVariable busca una variable por su nombre en el ámbito local actual de la traza de ejecución del REPL.
func (s *ScopeTrace) GetVariable(name string) *Variable {
	return s.CurrentScope.GetVariable(name)
}

// AddFunction agrega una nueva función al ámbito local actual de la traza de ejecución del REPL.
func (s *ScopeTrace) AddFunction(name string, function value.IVOR) (bool, string) {
	return s.CurrentScope.AddFunction(name, function)
}

// GetFunction busca una función por su nombre en el ámbito local actual de la traza de ejecución del REPL.
func (s *ScopeTrace) GetFunction(name string) (value.IVOR, string) {
	return s.CurrentScope.GetFunction(name)
}

// IsMutatingEnvironment verifica si el ámbito local actual o alguno de sus padres está en modo de mutación.
func (s *ScopeTrace) IsMutatingEnvironment() bool {
	return s.CurrentScope.IsMutatingScope()
}

// NewScopeTrace crea una nueva traza de ejecución del REPL con un ámbito global y un ámbito local inicial.
// Este ámbito local es el punto de partida para todas las ejecuciones en el REPL.
func NewScopeTrace() *ScopeTrace {
	globalScope := NewGlobalScope()
	return &ScopeTrace{
		GlobalScope:  globalScope,
		CurrentScope: globalScope,
	}
}

func NewVectorScope() *BaseScopeTrace {
	var scope = &BaseScopeTrace{
		name:      "vector",
		variables: make(map[string]*Variable),
		children:  make([]*BaseScopeTrace, 0),
		functions: make(map[string]value.IVOR),
		parent:    nil,
	}

	// register object built-in functions

	return scope
}

func NewStructScope() *BaseScopeTrace {

	newGlobal := NewGlobalScope()

	return &BaseScopeTrace{
		name:      "struct",
		variables: make(map[string]*Variable),
		children:  make([]*BaseScopeTrace, 0),
		functions: make(map[string]value.IVOR),
		structs:   make(map[string]*Struct),
		parent:    newGlobal,
		isStruct:  true,
	}
}

// Reporteria
type ReportTable struct {
	GlobalScope ReportScope
}

type ReportScope struct {
	Name        string
	Vars        []ReportSymbol
	Funcs       []ReportSymbol
	Structs     []ReportSymbol
	ChildScopes []ReportScope
}

type ReportSymbol struct {
	Name   string `json:"name"`
	Type   string `json:"type"`
	Line   int    `json:"line"`
	Column int    `json:"column"`
	Scope  string `json:"scope"` // Agregar este campo si no existe
}

func (s *ScopeTrace) Report() ReportTable {
	return ReportTable{
		GlobalScope: s.CurrentScope.Report(),
	}
}

func (s *BaseScopeTrace) Report() ReportScope {
	reportScope := ReportScope{
		Name:        s.name,
		Vars:        make([]ReportSymbol, 0),
		Funcs:       make([]ReportSymbol, 0),
		Structs:     make([]ReportSymbol, 0),
		ChildScopes: make([]ReportScope, 0),
	}

	for _, v := range s.variables {
		token := v.Token
		line := 0
		column := 0

		if token != nil {
			line = token.GetLine()
			column = token.GetColumn()
		}

		reportScope.Vars = append(reportScope.Vars, ReportSymbol{
			Name:   v.Name,
			Type:   v.Type,
			Line:   line,
			Column: column,
		})
	}

	for _, f := range s.functions {
		switch function := f.(type) {
		case *BuiltInFunction:
			reportScope.Funcs = append(reportScope.Funcs, ReportSymbol{
				Name:   function.Name,
				Type:   "Embebida: " + function.Name,
				Line:   0,
				Column: 0,
			})
		case *Function:
			line := 0
			column := 0

			if function.Token != nil {
				line = function.Token.GetLine()
				column = function.Token.GetColumn()
			}

			reportScope.Funcs = append(reportScope.Funcs, ReportSymbol{
				Name:   function.Name,
				Type:   function.ReturnType,
				Line:   line,
				Column: column,
			})
		case *ObjectBuiltInFunction:
			break
		default:
			log.Fatal("Function type not found")
		}
	}

	for _, v := range s.structs {
		reportScope.Structs = append(reportScope.Structs, ReportSymbol{
			Name:   v.Name,
			Type:   v.Name,
			Line:   v.Token.GetLine(),
			Column: v.Token.GetColumn(),
		})
	}

	for _, v := range s.children {
		reportScope.ChildScopes = append(reportScope.ChildScopes, v.Report())
	}

	return reportScope
}
