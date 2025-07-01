// Code generated from grammar/VLangGrammar.g4 by ANTLR 4.13.2. DO NOT EDIT.

package compiler // VLangGrammar
import "github.com/antlr4-go/antlr/v4"

type BaseVLangGrammarVisitor struct {
	*antlr.BaseParseTreeVisitor
}

func (v *BaseVLangGrammarVisitor) VisitProgram(ctx *ProgramContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitStmt(ctx *StmtContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitMutVarDecl(ctx *MutVarDeclContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitValueDecl(ctx *ValueDeclContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitValDeclVec(ctx *ValDeclVecContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitVarAssDecl(ctx *VarAssDeclContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitVarVectDecl(ctx *VarVectDeclContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitVarMatrixDecl(ctx *VarMatrixDeclContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitVar_type(ctx *Var_typeContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitVectorItemLis(ctx *VectorItemLisContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitVectorItem(ctx *VectorItemContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitVectorProperty(ctx *VectorPropertyContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitVectorFuncCall(ctx *VectorFuncCallContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitRepeatingDecl(ctx *RepeatingDeclContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitVector_type(ctx *Vector_typeContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitMatrix_type(ctx *Matrix_typeContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitMatrixItemList(ctx *MatrixItemListContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitType(ctx *TypeContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitAssignmentDecl(ctx *AssignmentDeclContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitArgAddAssigDecl(ctx *ArgAddAssigDeclContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitVectorAssign(ctx *VectorAssignContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitIdPattern(ctx *IdPatternContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitIntLiteral(ctx *IntLiteralContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitFloatLiteral(ctx *FloatLiteralContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitStringLiteral(ctx *StringLiteralContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitInterpolatedStringLiteral(ctx *InterpolatedStringLiteralContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitBoolLiteral(ctx *BoolLiteralContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitNilLiteral(ctx *NilLiteralContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitInterpolatedString(ctx *InterpolatedStringContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitIncremento(ctx *IncrementoContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitDecremento(ctx *DecrementoContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitRepeatingExpr(ctx *RepeatingExprContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitIncredecr(ctx *IncredecrContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitBinaryExpr(ctx *BinaryExprContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitStructInstantiationExpr(ctx *StructInstantiationExprContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitUnaryExpr(ctx *UnaryExprContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitIdPatternExpr(ctx *IdPatternExprContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitVectorPropertyExpr(ctx *VectorPropertyExprContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitVectorItemExpr(ctx *VectorItemExprContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitParensExpr(ctx *ParensExprContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitLiteralExpr(ctx *LiteralExprContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitVectorFuncCallExpr(ctx *VectorFuncCallExprContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitVectorExpr(ctx *VectorExprContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitFuncCallExpr(ctx *FuncCallExprContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitIfStmt(ctx *IfStmtContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitIfChain(ctx *IfChainContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitElseStmt(ctx *ElseStmtContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitSwitchStmt(ctx *SwitchStmtContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitSwitchCase(ctx *SwitchCaseContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitDefaultCase(ctx *DefaultCaseContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitWhileStmt(ctx *WhileStmtContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitForStmtCond(ctx *ForStmtCondContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitForAssCond(ctx *ForAssCondContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitForStmt(ctx *ForStmtContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitNumericRange(ctx *NumericRangeContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitReturnStmt(ctx *ReturnStmtContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitBreakStmt(ctx *BreakStmtContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitContinueStmt(ctx *ContinueStmtContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitFuncCall(ctx *FuncCallContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitBlockInd(ctx *BlockIndContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitArgList(ctx *ArgListContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitFuncArg(ctx *FuncArgContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitFuncDecl(ctx *FuncDeclContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitParamList(ctx *ParamListContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitFuncParam(ctx *FuncParamContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitStructDecl(ctx *StructDeclContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitStructAttr(ctx *StructAttrContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitStruct_param_list(ctx *Struct_param_listContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *BaseVLangGrammarVisitor) VisitStruct_param(ctx *Struct_paramContext) interface{} {
	return v.VisitChildren(ctx)
}
