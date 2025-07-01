// Code generated from grammar/VLangGrammar.g4 by ANTLR 4.13.2. DO NOT EDIT.

package compiler // VLangGrammar
import "github.com/antlr4-go/antlr/v4"

// A complete Visitor for a parse tree produced by VLangGrammar.
type VLangGrammarVisitor interface {
	antlr.ParseTreeVisitor

	// Visit a parse tree produced by VLangGrammar#program.
	VisitProgram(ctx *ProgramContext) interface{}

	// Visit a parse tree produced by VLangGrammar#stmt.
	VisitStmt(ctx *StmtContext) interface{}

	// Visit a parse tree produced by VLangGrammar#MutVarDecl.
	VisitMutVarDecl(ctx *MutVarDeclContext) interface{}

	// Visit a parse tree produced by VLangGrammar#ValueDecl.
	VisitValueDecl(ctx *ValueDeclContext) interface{}

	// Visit a parse tree produced by VLangGrammar#ValDeclVec.
	VisitValDeclVec(ctx *ValDeclVecContext) interface{}

	// Visit a parse tree produced by VLangGrammar#VarAssDecl.
	VisitVarAssDecl(ctx *VarAssDeclContext) interface{}

	// Visit a parse tree produced by VLangGrammar#VarVectDecl.
	VisitVarVectDecl(ctx *VarVectDeclContext) interface{}

	// Visit a parse tree produced by VLangGrammar#VarMatrixDecl.
	VisitVarMatrixDecl(ctx *VarMatrixDeclContext) interface{}

	// Visit a parse tree produced by VLangGrammar#var_type.
	VisitVar_type(ctx *Var_typeContext) interface{}

	// Visit a parse tree produced by VLangGrammar#VectorItemLis.
	VisitVectorItemLis(ctx *VectorItemLisContext) interface{}

	// Visit a parse tree produced by VLangGrammar#VectorItem.
	VisitVectorItem(ctx *VectorItemContext) interface{}

	// Visit a parse tree produced by VLangGrammar#VectorProperty.
	VisitVectorProperty(ctx *VectorPropertyContext) interface{}

	// Visit a parse tree produced by VLangGrammar#VectorFuncCall.
	VisitVectorFuncCall(ctx *VectorFuncCallContext) interface{}

	// Visit a parse tree produced by VLangGrammar#RepeatingDecl.
	VisitRepeatingDecl(ctx *RepeatingDeclContext) interface{}

	// Visit a parse tree produced by VLangGrammar#vector_type.
	VisitVector_type(ctx *Vector_typeContext) interface{}

	// Visit a parse tree produced by VLangGrammar#matrix_type.
	VisitMatrix_type(ctx *Matrix_typeContext) interface{}

	// Visit a parse tree produced by VLangGrammar#MatrixItemList.
	VisitMatrixItemList(ctx *MatrixItemListContext) interface{}

	// Visit a parse tree produced by VLangGrammar#type.
	VisitType(ctx *TypeContext) interface{}

	// Visit a parse tree produced by VLangGrammar#AssignmentDecl.
	VisitAssignmentDecl(ctx *AssignmentDeclContext) interface{}

	// Visit a parse tree produced by VLangGrammar#ArgAddAssigDecl.
	VisitArgAddAssigDecl(ctx *ArgAddAssigDeclContext) interface{}

	// Visit a parse tree produced by VLangGrammar#VectorAssign.
	VisitVectorAssign(ctx *VectorAssignContext) interface{}

	// Visit a parse tree produced by VLangGrammar#IdPattern.
	VisitIdPattern(ctx *IdPatternContext) interface{}

	// Visit a parse tree produced by VLangGrammar#IntLiteral.
	VisitIntLiteral(ctx *IntLiteralContext) interface{}

	// Visit a parse tree produced by VLangGrammar#FloatLiteral.
	VisitFloatLiteral(ctx *FloatLiteralContext) interface{}

	// Visit a parse tree produced by VLangGrammar#StringLiteral.
	VisitStringLiteral(ctx *StringLiteralContext) interface{}

	// Visit a parse tree produced by VLangGrammar#InterpolatedStringLiteral.
	VisitInterpolatedStringLiteral(ctx *InterpolatedStringLiteralContext) interface{}

	// Visit a parse tree produced by VLangGrammar#BoolLiteral.
	VisitBoolLiteral(ctx *BoolLiteralContext) interface{}

	// Visit a parse tree produced by VLangGrammar#NilLiteral.
	VisitNilLiteral(ctx *NilLiteralContext) interface{}

	// Visit a parse tree produced by VLangGrammar#InterpolatedString.
	VisitInterpolatedString(ctx *InterpolatedStringContext) interface{}

	// Visit a parse tree produced by VLangGrammar#incremento.
	VisitIncremento(ctx *IncrementoContext) interface{}

	// Visit a parse tree produced by VLangGrammar#decremento.
	VisitDecremento(ctx *DecrementoContext) interface{}

	// Visit a parse tree produced by VLangGrammar#RepeatingExpr.
	VisitRepeatingExpr(ctx *RepeatingExprContext) interface{}

	// Visit a parse tree produced by VLangGrammar#incredecr.
	VisitIncredecr(ctx *IncredecrContext) interface{}

	// Visit a parse tree produced by VLangGrammar#BinaryExpr.
	VisitBinaryExpr(ctx *BinaryExprContext) interface{}

	// Visit a parse tree produced by VLangGrammar#StructInstantiationExpr.
	VisitStructInstantiationExpr(ctx *StructInstantiationExprContext) interface{}

	// Visit a parse tree produced by VLangGrammar#UnaryExpr.
	VisitUnaryExpr(ctx *UnaryExprContext) interface{}

	// Visit a parse tree produced by VLangGrammar#IdPatternExpr.
	VisitIdPatternExpr(ctx *IdPatternExprContext) interface{}

	// Visit a parse tree produced by VLangGrammar#VectorPropertyExpr.
	VisitVectorPropertyExpr(ctx *VectorPropertyExprContext) interface{}

	// Visit a parse tree produced by VLangGrammar#VectorItemExpr.
	VisitVectorItemExpr(ctx *VectorItemExprContext) interface{}

	// Visit a parse tree produced by VLangGrammar#ParensExpr.
	VisitParensExpr(ctx *ParensExprContext) interface{}

	// Visit a parse tree produced by VLangGrammar#LiteralExpr.
	VisitLiteralExpr(ctx *LiteralExprContext) interface{}

	// Visit a parse tree produced by VLangGrammar#VectorFuncCallExpr.
	VisitVectorFuncCallExpr(ctx *VectorFuncCallExprContext) interface{}

	// Visit a parse tree produced by VLangGrammar#VectorExpr.
	VisitVectorExpr(ctx *VectorExprContext) interface{}

	// Visit a parse tree produced by VLangGrammar#FuncCallExpr.
	VisitFuncCallExpr(ctx *FuncCallExprContext) interface{}

	// Visit a parse tree produced by VLangGrammar#IfStmt.
	VisitIfStmt(ctx *IfStmtContext) interface{}

	// Visit a parse tree produced by VLangGrammar#IfChain.
	VisitIfChain(ctx *IfChainContext) interface{}

	// Visit a parse tree produced by VLangGrammar#ElseStmt.
	VisitElseStmt(ctx *ElseStmtContext) interface{}

	// Visit a parse tree produced by VLangGrammar#SwitchStmt.
	VisitSwitchStmt(ctx *SwitchStmtContext) interface{}

	// Visit a parse tree produced by VLangGrammar#SwitchCase.
	VisitSwitchCase(ctx *SwitchCaseContext) interface{}

	// Visit a parse tree produced by VLangGrammar#DefaultCase.
	VisitDefaultCase(ctx *DefaultCaseContext) interface{}

	// Visit a parse tree produced by VLangGrammar#WhileStmt.
	VisitWhileStmt(ctx *WhileStmtContext) interface{}

	// Visit a parse tree produced by VLangGrammar#ForStmtCond.
	VisitForStmtCond(ctx *ForStmtCondContext) interface{}

	// Visit a parse tree produced by VLangGrammar#ForAssCond.
	VisitForAssCond(ctx *ForAssCondContext) interface{}

	// Visit a parse tree produced by VLangGrammar#ForStmt.
	VisitForStmt(ctx *ForStmtContext) interface{}

	// Visit a parse tree produced by VLangGrammar#NumericRange.
	VisitNumericRange(ctx *NumericRangeContext) interface{}

	// Visit a parse tree produced by VLangGrammar#ReturnStmt.
	VisitReturnStmt(ctx *ReturnStmtContext) interface{}

	// Visit a parse tree produced by VLangGrammar#BreakStmt.
	VisitBreakStmt(ctx *BreakStmtContext) interface{}

	// Visit a parse tree produced by VLangGrammar#ContinueStmt.
	VisitContinueStmt(ctx *ContinueStmtContext) interface{}

	// Visit a parse tree produced by VLangGrammar#FuncCall.
	VisitFuncCall(ctx *FuncCallContext) interface{}

	// Visit a parse tree produced by VLangGrammar#BlockInd.
	VisitBlockInd(ctx *BlockIndContext) interface{}

	// Visit a parse tree produced by VLangGrammar#ArgList.
	VisitArgList(ctx *ArgListContext) interface{}

	// Visit a parse tree produced by VLangGrammar#FuncArg.
	VisitFuncArg(ctx *FuncArgContext) interface{}

	// Visit a parse tree produced by VLangGrammar#FuncDecl.
	VisitFuncDecl(ctx *FuncDeclContext) interface{}

	// Visit a parse tree produced by VLangGrammar#ParamList.
	VisitParamList(ctx *ParamListContext) interface{}

	// Visit a parse tree produced by VLangGrammar#FuncParam.
	VisitFuncParam(ctx *FuncParamContext) interface{}

	// Visit a parse tree produced by VLangGrammar#StructDecl.
	VisitStructDecl(ctx *StructDeclContext) interface{}

	// Visit a parse tree produced by VLangGrammar#StructAttr.
	VisitStructAttr(ctx *StructAttrContext) interface{}

	// Visit a parse tree produced by VLangGrammar#struct_param_list.
	VisitStruct_param_list(ctx *Struct_param_listContext) interface{}

	// Visit a parse tree produced by VLangGrammar#struct_param.
	VisitStruct_param(ctx *Struct_paramContext) interface{}
}
