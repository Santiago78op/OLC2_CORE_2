// Code generated from grammar/VLangGrammar.g4 by ANTLR 4.13.2. DO NOT EDIT.

package compiler // VLangGrammar
import "github.com/antlr4-go/antlr/v4"

// BaseVLangGrammarListener is a complete listener for a parse tree produced by VLangGrammar.
type BaseVLangGrammarListener struct{}

var _ VLangGrammarListener = &BaseVLangGrammarListener{}

// VisitTerminal is called when a terminal node is visited.
func (s *BaseVLangGrammarListener) VisitTerminal(node antlr.TerminalNode) {}

// VisitErrorNode is called when an error node is visited.
func (s *BaseVLangGrammarListener) VisitErrorNode(node antlr.ErrorNode) {}

// EnterEveryRule is called when any rule is entered.
func (s *BaseVLangGrammarListener) EnterEveryRule(ctx antlr.ParserRuleContext) {}

// ExitEveryRule is called when any rule is exited.
func (s *BaseVLangGrammarListener) ExitEveryRule(ctx antlr.ParserRuleContext) {}

// EnterProgram is called when production program is entered.
func (s *BaseVLangGrammarListener) EnterProgram(ctx *ProgramContext) {}

// ExitProgram is called when production program is exited.
func (s *BaseVLangGrammarListener) ExitProgram(ctx *ProgramContext) {}

// EnterStmt is called when production stmt is entered.
func (s *BaseVLangGrammarListener) EnterStmt(ctx *StmtContext) {}

// ExitStmt is called when production stmt is exited.
func (s *BaseVLangGrammarListener) ExitStmt(ctx *StmtContext) {}

// EnterMutVarDecl is called when production MutVarDecl is entered.
func (s *BaseVLangGrammarListener) EnterMutVarDecl(ctx *MutVarDeclContext) {}

// ExitMutVarDecl is called when production MutVarDecl is exited.
func (s *BaseVLangGrammarListener) ExitMutVarDecl(ctx *MutVarDeclContext) {}

// EnterValueDecl is called when production ValueDecl is entered.
func (s *BaseVLangGrammarListener) EnterValueDecl(ctx *ValueDeclContext) {}

// ExitValueDecl is called when production ValueDecl is exited.
func (s *BaseVLangGrammarListener) ExitValueDecl(ctx *ValueDeclContext) {}

// EnterValDeclVec is called when production ValDeclVec is entered.
func (s *BaseVLangGrammarListener) EnterValDeclVec(ctx *ValDeclVecContext) {}

// ExitValDeclVec is called when production ValDeclVec is exited.
func (s *BaseVLangGrammarListener) ExitValDeclVec(ctx *ValDeclVecContext) {}

// EnterVarAssDecl is called when production VarAssDecl is entered.
func (s *BaseVLangGrammarListener) EnterVarAssDecl(ctx *VarAssDeclContext) {}

// ExitVarAssDecl is called when production VarAssDecl is exited.
func (s *BaseVLangGrammarListener) ExitVarAssDecl(ctx *VarAssDeclContext) {}

// EnterVarVectDecl is called when production VarVectDecl is entered.
func (s *BaseVLangGrammarListener) EnterVarVectDecl(ctx *VarVectDeclContext) {}

// ExitVarVectDecl is called when production VarVectDecl is exited.
func (s *BaseVLangGrammarListener) ExitVarVectDecl(ctx *VarVectDeclContext) {}

// EnterVarMatrixDecl is called when production VarMatrixDecl is entered.
func (s *BaseVLangGrammarListener) EnterVarMatrixDecl(ctx *VarMatrixDeclContext) {}

// ExitVarMatrixDecl is called when production VarMatrixDecl is exited.
func (s *BaseVLangGrammarListener) ExitVarMatrixDecl(ctx *VarMatrixDeclContext) {}

// EnterVar_type is called when production var_type is entered.
func (s *BaseVLangGrammarListener) EnterVar_type(ctx *Var_typeContext) {}

// ExitVar_type is called when production var_type is exited.
func (s *BaseVLangGrammarListener) ExitVar_type(ctx *Var_typeContext) {}

// EnterVectorItemLis is called when production VectorItemLis is entered.
func (s *BaseVLangGrammarListener) EnterVectorItemLis(ctx *VectorItemLisContext) {}

// ExitVectorItemLis is called when production VectorItemLis is exited.
func (s *BaseVLangGrammarListener) ExitVectorItemLis(ctx *VectorItemLisContext) {}

// EnterVectorItem is called when production VectorItem is entered.
func (s *BaseVLangGrammarListener) EnterVectorItem(ctx *VectorItemContext) {}

// ExitVectorItem is called when production VectorItem is exited.
func (s *BaseVLangGrammarListener) ExitVectorItem(ctx *VectorItemContext) {}

// EnterVectorProperty is called when production VectorProperty is entered.
func (s *BaseVLangGrammarListener) EnterVectorProperty(ctx *VectorPropertyContext) {}

// ExitVectorProperty is called when production VectorProperty is exited.
func (s *BaseVLangGrammarListener) ExitVectorProperty(ctx *VectorPropertyContext) {}

// EnterVectorFuncCall is called when production VectorFuncCall is entered.
func (s *BaseVLangGrammarListener) EnterVectorFuncCall(ctx *VectorFuncCallContext) {}

// ExitVectorFuncCall is called when production VectorFuncCall is exited.
func (s *BaseVLangGrammarListener) ExitVectorFuncCall(ctx *VectorFuncCallContext) {}

// EnterRepeatingDecl is called when production RepeatingDecl is entered.
func (s *BaseVLangGrammarListener) EnterRepeatingDecl(ctx *RepeatingDeclContext) {}

// ExitRepeatingDecl is called when production RepeatingDecl is exited.
func (s *BaseVLangGrammarListener) ExitRepeatingDecl(ctx *RepeatingDeclContext) {}

// EnterVector_type is called when production vector_type is entered.
func (s *BaseVLangGrammarListener) EnterVector_type(ctx *Vector_typeContext) {}

// ExitVector_type is called when production vector_type is exited.
func (s *BaseVLangGrammarListener) ExitVector_type(ctx *Vector_typeContext) {}

// EnterMatrix_type is called when production matrix_type is entered.
func (s *BaseVLangGrammarListener) EnterMatrix_type(ctx *Matrix_typeContext) {}

// ExitMatrix_type is called when production matrix_type is exited.
func (s *BaseVLangGrammarListener) ExitMatrix_type(ctx *Matrix_typeContext) {}

// EnterMatrixItemList is called when production MatrixItemList is entered.
func (s *BaseVLangGrammarListener) EnterMatrixItemList(ctx *MatrixItemListContext) {}

// ExitMatrixItemList is called when production MatrixItemList is exited.
func (s *BaseVLangGrammarListener) ExitMatrixItemList(ctx *MatrixItemListContext) {}

// EnterType is called when production type is entered.
func (s *BaseVLangGrammarListener) EnterType(ctx *TypeContext) {}

// ExitType is called when production type is exited.
func (s *BaseVLangGrammarListener) ExitType(ctx *TypeContext) {}

// EnterAssignmentDecl is called when production AssignmentDecl is entered.
func (s *BaseVLangGrammarListener) EnterAssignmentDecl(ctx *AssignmentDeclContext) {}

// ExitAssignmentDecl is called when production AssignmentDecl is exited.
func (s *BaseVLangGrammarListener) ExitAssignmentDecl(ctx *AssignmentDeclContext) {}

// EnterArgAddAssigDecl is called when production ArgAddAssigDecl is entered.
func (s *BaseVLangGrammarListener) EnterArgAddAssigDecl(ctx *ArgAddAssigDeclContext) {}

// ExitArgAddAssigDecl is called when production ArgAddAssigDecl is exited.
func (s *BaseVLangGrammarListener) ExitArgAddAssigDecl(ctx *ArgAddAssigDeclContext) {}

// EnterVectorAssign is called when production VectorAssign is entered.
func (s *BaseVLangGrammarListener) EnterVectorAssign(ctx *VectorAssignContext) {}

// ExitVectorAssign is called when production VectorAssign is exited.
func (s *BaseVLangGrammarListener) ExitVectorAssign(ctx *VectorAssignContext) {}

// EnterIdPattern is called when production IdPattern is entered.
func (s *BaseVLangGrammarListener) EnterIdPattern(ctx *IdPatternContext) {}

// ExitIdPattern is called when production IdPattern is exited.
func (s *BaseVLangGrammarListener) ExitIdPattern(ctx *IdPatternContext) {}

// EnterIntLiteral is called when production IntLiteral is entered.
func (s *BaseVLangGrammarListener) EnterIntLiteral(ctx *IntLiteralContext) {}

// ExitIntLiteral is called when production IntLiteral is exited.
func (s *BaseVLangGrammarListener) ExitIntLiteral(ctx *IntLiteralContext) {}

// EnterFloatLiteral is called when production FloatLiteral is entered.
func (s *BaseVLangGrammarListener) EnterFloatLiteral(ctx *FloatLiteralContext) {}

// ExitFloatLiteral is called when production FloatLiteral is exited.
func (s *BaseVLangGrammarListener) ExitFloatLiteral(ctx *FloatLiteralContext) {}

// EnterStringLiteral is called when production StringLiteral is entered.
func (s *BaseVLangGrammarListener) EnterStringLiteral(ctx *StringLiteralContext) {}

// ExitStringLiteral is called when production StringLiteral is exited.
func (s *BaseVLangGrammarListener) ExitStringLiteral(ctx *StringLiteralContext) {}

// EnterInterpolatedStringLiteral is called when production InterpolatedStringLiteral is entered.
func (s *BaseVLangGrammarListener) EnterInterpolatedStringLiteral(ctx *InterpolatedStringLiteralContext) {
}

// ExitInterpolatedStringLiteral is called when production InterpolatedStringLiteral is exited.
func (s *BaseVLangGrammarListener) ExitInterpolatedStringLiteral(ctx *InterpolatedStringLiteralContext) {
}

// EnterBoolLiteral is called when production BoolLiteral is entered.
func (s *BaseVLangGrammarListener) EnterBoolLiteral(ctx *BoolLiteralContext) {}

// ExitBoolLiteral is called when production BoolLiteral is exited.
func (s *BaseVLangGrammarListener) ExitBoolLiteral(ctx *BoolLiteralContext) {}

// EnterNilLiteral is called when production NilLiteral is entered.
func (s *BaseVLangGrammarListener) EnterNilLiteral(ctx *NilLiteralContext) {}

// ExitNilLiteral is called when production NilLiteral is exited.
func (s *BaseVLangGrammarListener) ExitNilLiteral(ctx *NilLiteralContext) {}

// EnterInterpolatedString is called when production InterpolatedString is entered.
func (s *BaseVLangGrammarListener) EnterInterpolatedString(ctx *InterpolatedStringContext) {}

// ExitInterpolatedString is called when production InterpolatedString is exited.
func (s *BaseVLangGrammarListener) ExitInterpolatedString(ctx *InterpolatedStringContext) {}

// EnterIncremento is called when production incremento is entered.
func (s *BaseVLangGrammarListener) EnterIncremento(ctx *IncrementoContext) {}

// ExitIncremento is called when production incremento is exited.
func (s *BaseVLangGrammarListener) ExitIncremento(ctx *IncrementoContext) {}

// EnterDecremento is called when production decremento is entered.
func (s *BaseVLangGrammarListener) EnterDecremento(ctx *DecrementoContext) {}

// ExitDecremento is called when production decremento is exited.
func (s *BaseVLangGrammarListener) ExitDecremento(ctx *DecrementoContext) {}

// EnterRepeatingExpr is called when production RepeatingExpr is entered.
func (s *BaseVLangGrammarListener) EnterRepeatingExpr(ctx *RepeatingExprContext) {}

// ExitRepeatingExpr is called when production RepeatingExpr is exited.
func (s *BaseVLangGrammarListener) ExitRepeatingExpr(ctx *RepeatingExprContext) {}

// EnterIncredecr is called when production incredecr is entered.
func (s *BaseVLangGrammarListener) EnterIncredecr(ctx *IncredecrContext) {}

// ExitIncredecr is called when production incredecr is exited.
func (s *BaseVLangGrammarListener) ExitIncredecr(ctx *IncredecrContext) {}

// EnterBinaryExpr is called when production BinaryExpr is entered.
func (s *BaseVLangGrammarListener) EnterBinaryExpr(ctx *BinaryExprContext) {}

// ExitBinaryExpr is called when production BinaryExpr is exited.
func (s *BaseVLangGrammarListener) ExitBinaryExpr(ctx *BinaryExprContext) {}

// EnterStructInstantiationExpr is called when production StructInstantiationExpr is entered.
func (s *BaseVLangGrammarListener) EnterStructInstantiationExpr(ctx *StructInstantiationExprContext) {
}

// ExitStructInstantiationExpr is called when production StructInstantiationExpr is exited.
func (s *BaseVLangGrammarListener) ExitStructInstantiationExpr(ctx *StructInstantiationExprContext) {}

// EnterUnaryExpr is called when production UnaryExpr is entered.
func (s *BaseVLangGrammarListener) EnterUnaryExpr(ctx *UnaryExprContext) {}

// ExitUnaryExpr is called when production UnaryExpr is exited.
func (s *BaseVLangGrammarListener) ExitUnaryExpr(ctx *UnaryExprContext) {}

// EnterIdPatternExpr is called when production IdPatternExpr is entered.
func (s *BaseVLangGrammarListener) EnterIdPatternExpr(ctx *IdPatternExprContext) {}

// ExitIdPatternExpr is called when production IdPatternExpr is exited.
func (s *BaseVLangGrammarListener) ExitIdPatternExpr(ctx *IdPatternExprContext) {}

// EnterVectorPropertyExpr is called when production VectorPropertyExpr is entered.
func (s *BaseVLangGrammarListener) EnterVectorPropertyExpr(ctx *VectorPropertyExprContext) {}

// ExitVectorPropertyExpr is called when production VectorPropertyExpr is exited.
func (s *BaseVLangGrammarListener) ExitVectorPropertyExpr(ctx *VectorPropertyExprContext) {}

// EnterVectorItemExpr is called when production VectorItemExpr is entered.
func (s *BaseVLangGrammarListener) EnterVectorItemExpr(ctx *VectorItemExprContext) {}

// ExitVectorItemExpr is called when production VectorItemExpr is exited.
func (s *BaseVLangGrammarListener) ExitVectorItemExpr(ctx *VectorItemExprContext) {}

// EnterParensExpr is called when production ParensExpr is entered.
func (s *BaseVLangGrammarListener) EnterParensExpr(ctx *ParensExprContext) {}

// ExitParensExpr is called when production ParensExpr is exited.
func (s *BaseVLangGrammarListener) ExitParensExpr(ctx *ParensExprContext) {}

// EnterLiteralExpr is called when production LiteralExpr is entered.
func (s *BaseVLangGrammarListener) EnterLiteralExpr(ctx *LiteralExprContext) {}

// ExitLiteralExpr is called when production LiteralExpr is exited.
func (s *BaseVLangGrammarListener) ExitLiteralExpr(ctx *LiteralExprContext) {}

// EnterVectorFuncCallExpr is called when production VectorFuncCallExpr is entered.
func (s *BaseVLangGrammarListener) EnterVectorFuncCallExpr(ctx *VectorFuncCallExprContext) {}

// ExitVectorFuncCallExpr is called when production VectorFuncCallExpr is exited.
func (s *BaseVLangGrammarListener) ExitVectorFuncCallExpr(ctx *VectorFuncCallExprContext) {}

// EnterVectorExpr is called when production VectorExpr is entered.
func (s *BaseVLangGrammarListener) EnterVectorExpr(ctx *VectorExprContext) {}

// ExitVectorExpr is called when production VectorExpr is exited.
func (s *BaseVLangGrammarListener) ExitVectorExpr(ctx *VectorExprContext) {}

// EnterFuncCallExpr is called when production FuncCallExpr is entered.
func (s *BaseVLangGrammarListener) EnterFuncCallExpr(ctx *FuncCallExprContext) {}

// ExitFuncCallExpr is called when production FuncCallExpr is exited.
func (s *BaseVLangGrammarListener) ExitFuncCallExpr(ctx *FuncCallExprContext) {}

// EnterIfStmt is called when production IfStmt is entered.
func (s *BaseVLangGrammarListener) EnterIfStmt(ctx *IfStmtContext) {}

// ExitIfStmt is called when production IfStmt is exited.
func (s *BaseVLangGrammarListener) ExitIfStmt(ctx *IfStmtContext) {}

// EnterIfChain is called when production IfChain is entered.
func (s *BaseVLangGrammarListener) EnterIfChain(ctx *IfChainContext) {}

// ExitIfChain is called when production IfChain is exited.
func (s *BaseVLangGrammarListener) ExitIfChain(ctx *IfChainContext) {}

// EnterElseStmt is called when production ElseStmt is entered.
func (s *BaseVLangGrammarListener) EnterElseStmt(ctx *ElseStmtContext) {}

// ExitElseStmt is called when production ElseStmt is exited.
func (s *BaseVLangGrammarListener) ExitElseStmt(ctx *ElseStmtContext) {}

// EnterSwitchStmt is called when production SwitchStmt is entered.
func (s *BaseVLangGrammarListener) EnterSwitchStmt(ctx *SwitchStmtContext) {}

// ExitSwitchStmt is called when production SwitchStmt is exited.
func (s *BaseVLangGrammarListener) ExitSwitchStmt(ctx *SwitchStmtContext) {}

// EnterSwitchCase is called when production SwitchCase is entered.
func (s *BaseVLangGrammarListener) EnterSwitchCase(ctx *SwitchCaseContext) {}

// ExitSwitchCase is called when production SwitchCase is exited.
func (s *BaseVLangGrammarListener) ExitSwitchCase(ctx *SwitchCaseContext) {}

// EnterDefaultCase is called when production DefaultCase is entered.
func (s *BaseVLangGrammarListener) EnterDefaultCase(ctx *DefaultCaseContext) {}

// ExitDefaultCase is called when production DefaultCase is exited.
func (s *BaseVLangGrammarListener) ExitDefaultCase(ctx *DefaultCaseContext) {}

// EnterWhileStmt is called when production WhileStmt is entered.
func (s *BaseVLangGrammarListener) EnterWhileStmt(ctx *WhileStmtContext) {}

// ExitWhileStmt is called when production WhileStmt is exited.
func (s *BaseVLangGrammarListener) ExitWhileStmt(ctx *WhileStmtContext) {}

// EnterForStmtCond is called when production ForStmtCond is entered.
func (s *BaseVLangGrammarListener) EnterForStmtCond(ctx *ForStmtCondContext) {}

// ExitForStmtCond is called when production ForStmtCond is exited.
func (s *BaseVLangGrammarListener) ExitForStmtCond(ctx *ForStmtCondContext) {}

// EnterForAssCond is called when production ForAssCond is entered.
func (s *BaseVLangGrammarListener) EnterForAssCond(ctx *ForAssCondContext) {}

// ExitForAssCond is called when production ForAssCond is exited.
func (s *BaseVLangGrammarListener) ExitForAssCond(ctx *ForAssCondContext) {}

// EnterForStmt is called when production ForStmt is entered.
func (s *BaseVLangGrammarListener) EnterForStmt(ctx *ForStmtContext) {}

// ExitForStmt is called when production ForStmt is exited.
func (s *BaseVLangGrammarListener) ExitForStmt(ctx *ForStmtContext) {}

// EnterNumericRange is called when production NumericRange is entered.
func (s *BaseVLangGrammarListener) EnterNumericRange(ctx *NumericRangeContext) {}

// ExitNumericRange is called when production NumericRange is exited.
func (s *BaseVLangGrammarListener) ExitNumericRange(ctx *NumericRangeContext) {}

// EnterReturnStmt is called when production ReturnStmt is entered.
func (s *BaseVLangGrammarListener) EnterReturnStmt(ctx *ReturnStmtContext) {}

// ExitReturnStmt is called when production ReturnStmt is exited.
func (s *BaseVLangGrammarListener) ExitReturnStmt(ctx *ReturnStmtContext) {}

// EnterBreakStmt is called when production BreakStmt is entered.
func (s *BaseVLangGrammarListener) EnterBreakStmt(ctx *BreakStmtContext) {}

// ExitBreakStmt is called when production BreakStmt is exited.
func (s *BaseVLangGrammarListener) ExitBreakStmt(ctx *BreakStmtContext) {}

// EnterContinueStmt is called when production ContinueStmt is entered.
func (s *BaseVLangGrammarListener) EnterContinueStmt(ctx *ContinueStmtContext) {}

// ExitContinueStmt is called when production ContinueStmt is exited.
func (s *BaseVLangGrammarListener) ExitContinueStmt(ctx *ContinueStmtContext) {}

// EnterFuncCall is called when production FuncCall is entered.
func (s *BaseVLangGrammarListener) EnterFuncCall(ctx *FuncCallContext) {}

// ExitFuncCall is called when production FuncCall is exited.
func (s *BaseVLangGrammarListener) ExitFuncCall(ctx *FuncCallContext) {}

// EnterBlockInd is called when production BlockInd is entered.
func (s *BaseVLangGrammarListener) EnterBlockInd(ctx *BlockIndContext) {}

// ExitBlockInd is called when production BlockInd is exited.
func (s *BaseVLangGrammarListener) ExitBlockInd(ctx *BlockIndContext) {}

// EnterArgList is called when production ArgList is entered.
func (s *BaseVLangGrammarListener) EnterArgList(ctx *ArgListContext) {}

// ExitArgList is called when production ArgList is exited.
func (s *BaseVLangGrammarListener) ExitArgList(ctx *ArgListContext) {}

// EnterFuncArg is called when production FuncArg is entered.
func (s *BaseVLangGrammarListener) EnterFuncArg(ctx *FuncArgContext) {}

// ExitFuncArg is called when production FuncArg is exited.
func (s *BaseVLangGrammarListener) ExitFuncArg(ctx *FuncArgContext) {}

// EnterFuncDecl is called when production FuncDecl is entered.
func (s *BaseVLangGrammarListener) EnterFuncDecl(ctx *FuncDeclContext) {}

// ExitFuncDecl is called when production FuncDecl is exited.
func (s *BaseVLangGrammarListener) ExitFuncDecl(ctx *FuncDeclContext) {}

// EnterParamList is called when production ParamList is entered.
func (s *BaseVLangGrammarListener) EnterParamList(ctx *ParamListContext) {}

// ExitParamList is called when production ParamList is exited.
func (s *BaseVLangGrammarListener) ExitParamList(ctx *ParamListContext) {}

// EnterFuncParam is called when production FuncParam is entered.
func (s *BaseVLangGrammarListener) EnterFuncParam(ctx *FuncParamContext) {}

// ExitFuncParam is called when production FuncParam is exited.
func (s *BaseVLangGrammarListener) ExitFuncParam(ctx *FuncParamContext) {}

// EnterStructDecl is called when production StructDecl is entered.
func (s *BaseVLangGrammarListener) EnterStructDecl(ctx *StructDeclContext) {}

// ExitStructDecl is called when production StructDecl is exited.
func (s *BaseVLangGrammarListener) ExitStructDecl(ctx *StructDeclContext) {}

// EnterStructAttr is called when production StructAttr is entered.
func (s *BaseVLangGrammarListener) EnterStructAttr(ctx *StructAttrContext) {}

// ExitStructAttr is called when production StructAttr is exited.
func (s *BaseVLangGrammarListener) ExitStructAttr(ctx *StructAttrContext) {}

// EnterStruct_param_list is called when production struct_param_list is entered.
func (s *BaseVLangGrammarListener) EnterStruct_param_list(ctx *Struct_param_listContext) {}

// ExitStruct_param_list is called when production struct_param_list is exited.
func (s *BaseVLangGrammarListener) ExitStruct_param_list(ctx *Struct_param_listContext) {}

// EnterStruct_param is called when production struct_param is entered.
func (s *BaseVLangGrammarListener) EnterStruct_param(ctx *Struct_paramContext) {}

// ExitStruct_param is called when production struct_param is exited.
func (s *BaseVLangGrammarListener) ExitStruct_param(ctx *Struct_paramContext) {}
