// Code generated from grammar/VLangGrammar.g4 by ANTLR 4.13.2. DO NOT EDIT.

package compiler // VLangGrammar
import "github.com/antlr4-go/antlr/v4"

// VLangGrammarListener is a complete listener for a parse tree produced by VLangGrammar.
type VLangGrammarListener interface {
	antlr.ParseTreeListener

	// EnterProgram is called when entering the program production.
	EnterProgram(c *ProgramContext)

	// EnterStmt is called when entering the stmt production.
	EnterStmt(c *StmtContext)

	// EnterMutVarDecl is called when entering the MutVarDecl production.
	EnterMutVarDecl(c *MutVarDeclContext)

	// EnterValueDecl is called when entering the ValueDecl production.
	EnterValueDecl(c *ValueDeclContext)

	// EnterValDeclVec is called when entering the ValDeclVec production.
	EnterValDeclVec(c *ValDeclVecContext)

	// EnterVarAssDecl is called when entering the VarAssDecl production.
	EnterVarAssDecl(c *VarAssDeclContext)

	// EnterVarVectDecl is called when entering the VarVectDecl production.
	EnterVarVectDecl(c *VarVectDeclContext)

	// EnterVarMatrixDecl is called when entering the VarMatrixDecl production.
	EnterVarMatrixDecl(c *VarMatrixDeclContext)

	// EnterVar_type is called when entering the var_type production.
	EnterVar_type(c *Var_typeContext)

	// EnterVectorItemLis is called when entering the VectorItemLis production.
	EnterVectorItemLis(c *VectorItemLisContext)

	// EnterVectorItem is called when entering the VectorItem production.
	EnterVectorItem(c *VectorItemContext)

	// EnterVectorProperty is called when entering the VectorProperty production.
	EnterVectorProperty(c *VectorPropertyContext)

	// EnterVectorFuncCall is called when entering the VectorFuncCall production.
	EnterVectorFuncCall(c *VectorFuncCallContext)

	// EnterRepeatingDecl is called when entering the RepeatingDecl production.
	EnterRepeatingDecl(c *RepeatingDeclContext)

	// EnterVector_type is called when entering the vector_type production.
	EnterVector_type(c *Vector_typeContext)

	// EnterMatrix_type is called when entering the matrix_type production.
	EnterMatrix_type(c *Matrix_typeContext)

	// EnterMatrixItemList is called when entering the MatrixItemList production.
	EnterMatrixItemList(c *MatrixItemListContext)

	// EnterType is called when entering the type production.
	EnterType(c *TypeContext)

	// EnterAssignmentDecl is called when entering the AssignmentDecl production.
	EnterAssignmentDecl(c *AssignmentDeclContext)

	// EnterArgAddAssigDecl is called when entering the ArgAddAssigDecl production.
	EnterArgAddAssigDecl(c *ArgAddAssigDeclContext)

	// EnterVectorAssign is called when entering the VectorAssign production.
	EnterVectorAssign(c *VectorAssignContext)

	// EnterIdPattern is called when entering the IdPattern production.
	EnterIdPattern(c *IdPatternContext)

	// EnterIntLiteral is called when entering the IntLiteral production.
	EnterIntLiteral(c *IntLiteralContext)

	// EnterFloatLiteral is called when entering the FloatLiteral production.
	EnterFloatLiteral(c *FloatLiteralContext)

	// EnterStringLiteral is called when entering the StringLiteral production.
	EnterStringLiteral(c *StringLiteralContext)

	// EnterInterpolatedStringLiteral is called when entering the InterpolatedStringLiteral production.
	EnterInterpolatedStringLiteral(c *InterpolatedStringLiteralContext)

	// EnterBoolLiteral is called when entering the BoolLiteral production.
	EnterBoolLiteral(c *BoolLiteralContext)

	// EnterNilLiteral is called when entering the NilLiteral production.
	EnterNilLiteral(c *NilLiteralContext)

	// EnterInterpolatedString is called when entering the InterpolatedString production.
	EnterInterpolatedString(c *InterpolatedStringContext)

	// EnterIncremento is called when entering the incremento production.
	EnterIncremento(c *IncrementoContext)

	// EnterDecremento is called when entering the decremento production.
	EnterDecremento(c *DecrementoContext)

	// EnterRepeatingExpr is called when entering the RepeatingExpr production.
	EnterRepeatingExpr(c *RepeatingExprContext)

	// EnterIncredecr is called when entering the incredecr production.
	EnterIncredecr(c *IncredecrContext)

	// EnterBinaryExpr is called when entering the BinaryExpr production.
	EnterBinaryExpr(c *BinaryExprContext)

	// EnterStructInstantiationExpr is called when entering the StructInstantiationExpr production.
	EnterStructInstantiationExpr(c *StructInstantiationExprContext)

	// EnterUnaryExpr is called when entering the UnaryExpr production.
	EnterUnaryExpr(c *UnaryExprContext)

	// EnterIdPatternExpr is called when entering the IdPatternExpr production.
	EnterIdPatternExpr(c *IdPatternExprContext)

	// EnterVectorPropertyExpr is called when entering the VectorPropertyExpr production.
	EnterVectorPropertyExpr(c *VectorPropertyExprContext)

	// EnterVectorItemExpr is called when entering the VectorItemExpr production.
	EnterVectorItemExpr(c *VectorItemExprContext)

	// EnterParensExpr is called when entering the ParensExpr production.
	EnterParensExpr(c *ParensExprContext)

	// EnterLiteralExpr is called when entering the LiteralExpr production.
	EnterLiteralExpr(c *LiteralExprContext)

	// EnterVectorFuncCallExpr is called when entering the VectorFuncCallExpr production.
	EnterVectorFuncCallExpr(c *VectorFuncCallExprContext)

	// EnterVectorExpr is called when entering the VectorExpr production.
	EnterVectorExpr(c *VectorExprContext)

	// EnterFuncCallExpr is called when entering the FuncCallExpr production.
	EnterFuncCallExpr(c *FuncCallExprContext)

	// EnterIfStmt is called when entering the IfStmt production.
	EnterIfStmt(c *IfStmtContext)

	// EnterIfChain is called when entering the IfChain production.
	EnterIfChain(c *IfChainContext)

	// EnterElseStmt is called when entering the ElseStmt production.
	EnterElseStmt(c *ElseStmtContext)

	// EnterSwitchStmt is called when entering the SwitchStmt production.
	EnterSwitchStmt(c *SwitchStmtContext)

	// EnterSwitchCase is called when entering the SwitchCase production.
	EnterSwitchCase(c *SwitchCaseContext)

	// EnterDefaultCase is called when entering the DefaultCase production.
	EnterDefaultCase(c *DefaultCaseContext)

	// EnterWhileStmt is called when entering the WhileStmt production.
	EnterWhileStmt(c *WhileStmtContext)

	// EnterForStmtCond is called when entering the ForStmtCond production.
	EnterForStmtCond(c *ForStmtCondContext)

	// EnterForAssCond is called when entering the ForAssCond production.
	EnterForAssCond(c *ForAssCondContext)

	// EnterForStmt is called when entering the ForStmt production.
	EnterForStmt(c *ForStmtContext)

	// EnterNumericRange is called when entering the NumericRange production.
	EnterNumericRange(c *NumericRangeContext)

	// EnterReturnStmt is called when entering the ReturnStmt production.
	EnterReturnStmt(c *ReturnStmtContext)

	// EnterBreakStmt is called when entering the BreakStmt production.
	EnterBreakStmt(c *BreakStmtContext)

	// EnterContinueStmt is called when entering the ContinueStmt production.
	EnterContinueStmt(c *ContinueStmtContext)

	// EnterFuncCall is called when entering the FuncCall production.
	EnterFuncCall(c *FuncCallContext)

	// EnterBlockInd is called when entering the BlockInd production.
	EnterBlockInd(c *BlockIndContext)

	// EnterArgList is called when entering the ArgList production.
	EnterArgList(c *ArgListContext)

	// EnterFuncArg is called when entering the FuncArg production.
	EnterFuncArg(c *FuncArgContext)

	// EnterFuncDecl is called when entering the FuncDecl production.
	EnterFuncDecl(c *FuncDeclContext)

	// EnterParamList is called when entering the ParamList production.
	EnterParamList(c *ParamListContext)

	// EnterFuncParam is called when entering the FuncParam production.
	EnterFuncParam(c *FuncParamContext)

	// EnterStructDecl is called when entering the StructDecl production.
	EnterStructDecl(c *StructDeclContext)

	// EnterStructAttr is called when entering the StructAttr production.
	EnterStructAttr(c *StructAttrContext)

	// EnterStruct_param_list is called when entering the struct_param_list production.
	EnterStruct_param_list(c *Struct_param_listContext)

	// EnterStruct_param is called when entering the struct_param production.
	EnterStruct_param(c *Struct_paramContext)

	// ExitProgram is called when exiting the program production.
	ExitProgram(c *ProgramContext)

	// ExitStmt is called when exiting the stmt production.
	ExitStmt(c *StmtContext)

	// ExitMutVarDecl is called when exiting the MutVarDecl production.
	ExitMutVarDecl(c *MutVarDeclContext)

	// ExitValueDecl is called when exiting the ValueDecl production.
	ExitValueDecl(c *ValueDeclContext)

	// ExitValDeclVec is called when exiting the ValDeclVec production.
	ExitValDeclVec(c *ValDeclVecContext)

	// ExitVarAssDecl is called when exiting the VarAssDecl production.
	ExitVarAssDecl(c *VarAssDeclContext)

	// ExitVarVectDecl is called when exiting the VarVectDecl production.
	ExitVarVectDecl(c *VarVectDeclContext)

	// ExitVarMatrixDecl is called when exiting the VarMatrixDecl production.
	ExitVarMatrixDecl(c *VarMatrixDeclContext)

	// ExitVar_type is called when exiting the var_type production.
	ExitVar_type(c *Var_typeContext)

	// ExitVectorItemLis is called when exiting the VectorItemLis production.
	ExitVectorItemLis(c *VectorItemLisContext)

	// ExitVectorItem is called when exiting the VectorItem production.
	ExitVectorItem(c *VectorItemContext)

	// ExitVectorProperty is called when exiting the VectorProperty production.
	ExitVectorProperty(c *VectorPropertyContext)

	// ExitVectorFuncCall is called when exiting the VectorFuncCall production.
	ExitVectorFuncCall(c *VectorFuncCallContext)

	// ExitRepeatingDecl is called when exiting the RepeatingDecl production.
	ExitRepeatingDecl(c *RepeatingDeclContext)

	// ExitVector_type is called when exiting the vector_type production.
	ExitVector_type(c *Vector_typeContext)

	// ExitMatrix_type is called when exiting the matrix_type production.
	ExitMatrix_type(c *Matrix_typeContext)

	// ExitMatrixItemList is called when exiting the MatrixItemList production.
	ExitMatrixItemList(c *MatrixItemListContext)

	// ExitType is called when exiting the type production.
	ExitType(c *TypeContext)

	// ExitAssignmentDecl is called when exiting the AssignmentDecl production.
	ExitAssignmentDecl(c *AssignmentDeclContext)

	// ExitArgAddAssigDecl is called when exiting the ArgAddAssigDecl production.
	ExitArgAddAssigDecl(c *ArgAddAssigDeclContext)

	// ExitVectorAssign is called when exiting the VectorAssign production.
	ExitVectorAssign(c *VectorAssignContext)

	// ExitIdPattern is called when exiting the IdPattern production.
	ExitIdPattern(c *IdPatternContext)

	// ExitIntLiteral is called when exiting the IntLiteral production.
	ExitIntLiteral(c *IntLiteralContext)

	// ExitFloatLiteral is called when exiting the FloatLiteral production.
	ExitFloatLiteral(c *FloatLiteralContext)

	// ExitStringLiteral is called when exiting the StringLiteral production.
	ExitStringLiteral(c *StringLiteralContext)

	// ExitInterpolatedStringLiteral is called when exiting the InterpolatedStringLiteral production.
	ExitInterpolatedStringLiteral(c *InterpolatedStringLiteralContext)

	// ExitBoolLiteral is called when exiting the BoolLiteral production.
	ExitBoolLiteral(c *BoolLiteralContext)

	// ExitNilLiteral is called when exiting the NilLiteral production.
	ExitNilLiteral(c *NilLiteralContext)

	// ExitInterpolatedString is called when exiting the InterpolatedString production.
	ExitInterpolatedString(c *InterpolatedStringContext)

	// ExitIncremento is called when exiting the incremento production.
	ExitIncremento(c *IncrementoContext)

	// ExitDecremento is called when exiting the decremento production.
	ExitDecremento(c *DecrementoContext)

	// ExitRepeatingExpr is called when exiting the RepeatingExpr production.
	ExitRepeatingExpr(c *RepeatingExprContext)

	// ExitIncredecr is called when exiting the incredecr production.
	ExitIncredecr(c *IncredecrContext)

	// ExitBinaryExpr is called when exiting the BinaryExpr production.
	ExitBinaryExpr(c *BinaryExprContext)

	// ExitStructInstantiationExpr is called when exiting the StructInstantiationExpr production.
	ExitStructInstantiationExpr(c *StructInstantiationExprContext)

	// ExitUnaryExpr is called when exiting the UnaryExpr production.
	ExitUnaryExpr(c *UnaryExprContext)

	// ExitIdPatternExpr is called when exiting the IdPatternExpr production.
	ExitIdPatternExpr(c *IdPatternExprContext)

	// ExitVectorPropertyExpr is called when exiting the VectorPropertyExpr production.
	ExitVectorPropertyExpr(c *VectorPropertyExprContext)

	// ExitVectorItemExpr is called when exiting the VectorItemExpr production.
	ExitVectorItemExpr(c *VectorItemExprContext)

	// ExitParensExpr is called when exiting the ParensExpr production.
	ExitParensExpr(c *ParensExprContext)

	// ExitLiteralExpr is called when exiting the LiteralExpr production.
	ExitLiteralExpr(c *LiteralExprContext)

	// ExitVectorFuncCallExpr is called when exiting the VectorFuncCallExpr production.
	ExitVectorFuncCallExpr(c *VectorFuncCallExprContext)

	// ExitVectorExpr is called when exiting the VectorExpr production.
	ExitVectorExpr(c *VectorExprContext)

	// ExitFuncCallExpr is called when exiting the FuncCallExpr production.
	ExitFuncCallExpr(c *FuncCallExprContext)

	// ExitIfStmt is called when exiting the IfStmt production.
	ExitIfStmt(c *IfStmtContext)

	// ExitIfChain is called when exiting the IfChain production.
	ExitIfChain(c *IfChainContext)

	// ExitElseStmt is called when exiting the ElseStmt production.
	ExitElseStmt(c *ElseStmtContext)

	// ExitSwitchStmt is called when exiting the SwitchStmt production.
	ExitSwitchStmt(c *SwitchStmtContext)

	// ExitSwitchCase is called when exiting the SwitchCase production.
	ExitSwitchCase(c *SwitchCaseContext)

	// ExitDefaultCase is called when exiting the DefaultCase production.
	ExitDefaultCase(c *DefaultCaseContext)

	// ExitWhileStmt is called when exiting the WhileStmt production.
	ExitWhileStmt(c *WhileStmtContext)

	// ExitForStmtCond is called when exiting the ForStmtCond production.
	ExitForStmtCond(c *ForStmtCondContext)

	// ExitForAssCond is called when exiting the ForAssCond production.
	ExitForAssCond(c *ForAssCondContext)

	// ExitForStmt is called when exiting the ForStmt production.
	ExitForStmt(c *ForStmtContext)

	// ExitNumericRange is called when exiting the NumericRange production.
	ExitNumericRange(c *NumericRangeContext)

	// ExitReturnStmt is called when exiting the ReturnStmt production.
	ExitReturnStmt(c *ReturnStmtContext)

	// ExitBreakStmt is called when exiting the BreakStmt production.
	ExitBreakStmt(c *BreakStmtContext)

	// ExitContinueStmt is called when exiting the ContinueStmt production.
	ExitContinueStmt(c *ContinueStmtContext)

	// ExitFuncCall is called when exiting the FuncCall production.
	ExitFuncCall(c *FuncCallContext)

	// ExitBlockInd is called when exiting the BlockInd production.
	ExitBlockInd(c *BlockIndContext)

	// ExitArgList is called when exiting the ArgList production.
	ExitArgList(c *ArgListContext)

	// ExitFuncArg is called when exiting the FuncArg production.
	ExitFuncArg(c *FuncArgContext)

	// ExitFuncDecl is called when exiting the FuncDecl production.
	ExitFuncDecl(c *FuncDeclContext)

	// ExitParamList is called when exiting the ParamList production.
	ExitParamList(c *ParamListContext)

	// ExitFuncParam is called when exiting the FuncParam production.
	ExitFuncParam(c *FuncParamContext)

	// ExitStructDecl is called when exiting the StructDecl production.
	ExitStructDecl(c *StructDeclContext)

	// ExitStructAttr is called when exiting the StructAttr production.
	ExitStructAttr(c *StructAttrContext)

	// ExitStruct_param_list is called when exiting the struct_param_list production.
	ExitStruct_param_list(c *Struct_param_listContext)

	// ExitStruct_param is called when exiting the struct_param production.
	ExitStruct_param(c *Struct_paramContext)
}
