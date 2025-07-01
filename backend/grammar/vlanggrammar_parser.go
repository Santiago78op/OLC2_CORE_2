// Code generated from grammar/VLangGrammar.g4 by ANTLR 4.13.2. DO NOT EDIT.

package compiler // VLangGrammar
import (
	"fmt"
	"strconv"
	"sync"

	"github.com/antlr4-go/antlr/v4"
)

// Suppress unused import errors
var _ = fmt.Printf
var _ = strconv.Itoa
var _ = sync.Once{}

type VLangGrammar struct {
	*antlr.BaseParser
}

var VLangGrammarParserStaticData struct {
	once                   sync.Once
	serializedATN          []int32
	LiteralNames           []string
	SymbolicNames          []string
	RuleNames              []string
	PredictionContextCache *antlr.PredictionContextCache
	atn                    *antlr.ATN
	decisionToDFA          []*antlr.DFA
}

func vlanggrammarParserInit() {
	staticData := &VLangGrammarParserStaticData
	staticData.LiteralNames = []string{
		"", "'mut'", "'fn'", "'struct'", "'if'", "'else'", "'switch'", "'case'",
		"'default'", "'for'", "'while'", "'in'", "'break'", "'continue'", "'return'",
		"'--'", "'++'", "'+'", "'-'", "'*'", "'/'", "'%'", "'='", "'+='", "'-='",
		"'=='", "'!='", "'<'", "'<='", "'>'", "'>='", "'&&'", "'||'", "'!'",
		"'('", "')'", "'{'", "'}'", "'['", "']'", "';'", "':'", "'.'", "','",
		"'$'", "", "", "", "", "'nil'",
	}
	staticData.SymbolicNames = []string{
		"", "MUT", "FUNC", "STR", "IF_KW", "ELSE_KW", "SWITCH_KW", "CASE_KW",
		"DEFAULT_KW", "FOR_KW", "WHILE_KW", "IN_KW", "BREAK_KW", "CONTINUE_KW",
		"RETURN_KW", "DEC", "INC", "PLUS", "MINUS", "MULT", "DIV", "MOD", "ASSIGN",
		"PLUS_ASSIGN", "MINUS_ASSIGN", "EQ", "NE", "LT", "LE", "GT", "GE", "AND",
		"OR", "NOT", "LPAREN", "RPAREN", "LBRACE", "RBRACE", "LBRACK", "RBRACK",
		"SEMI", "COLON", "DOT", "COMMA", "DOLLAR", "INT_LITERAL", "FLOAT_LITERAL",
		"STRING_LITERAL", "BOOL_LITERAL", "NIL_LITERAL", "ID", "WS", "LINE_COMMENT",
		"BLOCK_COMMENT",
	}
	staticData.RuleNames = []string{
		"program", "stmt", "decl_stmt", "var_type", "vect_expr", "vect_item",
		"vect_prop", "vect_func", "repeating", "vector_type", "matrix_type",
		"matrix_expr", "type", "assign_stmt", "id_pattern", "literal", "interpolated_string",
		"incredecre", "expression", "if_stmt", "if_chain", "else_stmt", "switch_stmt",
		"switch_case", "default_case", "while_stmt", "for_stmt", "range", "transfer_stmt",
		"func_call", "block_ind", "arg_list", "func_arg", "func_dcl", "param_list",
		"func_param", "strct_dcl", "struct_prop", "struct_param_list", "struct_param",
	}
	staticData.PredictionContextCache = antlr.NewPredictionContextCache()
	staticData.serializedATN = []int32{
		4, 1, 53, 513, 2, 0, 7, 0, 2, 1, 7, 1, 2, 2, 7, 2, 2, 3, 7, 3, 2, 4, 7,
		4, 2, 5, 7, 5, 2, 6, 7, 6, 2, 7, 7, 7, 2, 8, 7, 8, 2, 9, 7, 9, 2, 10, 7,
		10, 2, 11, 7, 11, 2, 12, 7, 12, 2, 13, 7, 13, 2, 14, 7, 14, 2, 15, 7, 15,
		2, 16, 7, 16, 2, 17, 7, 17, 2, 18, 7, 18, 2, 19, 7, 19, 2, 20, 7, 20, 2,
		21, 7, 21, 2, 22, 7, 22, 2, 23, 7, 23, 2, 24, 7, 24, 2, 25, 7, 25, 2, 26,
		7, 26, 2, 27, 7, 27, 2, 28, 7, 28, 2, 29, 7, 29, 2, 30, 7, 30, 2, 31, 7,
		31, 2, 32, 7, 32, 2, 33, 7, 33, 2, 34, 7, 34, 2, 35, 7, 35, 2, 36, 7, 36,
		2, 37, 7, 37, 2, 38, 7, 38, 2, 39, 7, 39, 1, 0, 5, 0, 82, 8, 0, 10, 0,
		12, 0, 85, 9, 0, 1, 0, 3, 0, 88, 8, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 102, 8, 1, 1, 2, 1, 2, 1,
		2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
		2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
		2, 1, 2, 1, 2, 1, 2, 3, 2, 134, 8, 2, 1, 3, 1, 3, 1, 4, 1, 4, 1, 4, 1,
		4, 5, 4, 142, 8, 4, 10, 4, 12, 4, 145, 9, 4, 3, 4, 147, 8, 4, 1, 4, 1,
		4, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 4, 5, 156, 8, 5, 11, 5, 12, 5, 157, 1,
		6, 1, 6, 1, 6, 1, 6, 1, 7, 1, 7, 1, 7, 1, 7, 1, 8, 1, 8, 3, 8, 170, 8,
		8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 9, 1,
		9, 1, 9, 1, 9, 1, 10, 1, 10, 1, 10, 1, 10, 1, 10, 1, 10, 1, 11, 1, 11,
		1, 11, 1, 11, 5, 11, 196, 8, 11, 10, 11, 12, 11, 199, 9, 11, 1, 11, 1,
		11, 1, 12, 1, 12, 1, 12, 3, 12, 206, 8, 12, 1, 13, 1, 13, 1, 13, 1, 13,
		1, 13, 1, 13, 1, 13, 1, 13, 1, 13, 1, 13, 1, 13, 1, 13, 3, 13, 220, 8,
		13, 1, 14, 1, 14, 1, 14, 5, 14, 225, 8, 14, 10, 14, 12, 14, 228, 9, 14,
		1, 15, 1, 15, 1, 15, 1, 15, 1, 15, 1, 15, 3, 15, 236, 8, 15, 1, 16, 1,
		16, 1, 17, 1, 17, 1, 17, 1, 17, 3, 17, 244, 8, 17, 1, 18, 1, 18, 1, 18,
		1, 18, 1, 18, 1, 18, 1, 18, 1, 18, 1, 18, 1, 18, 1, 18, 1, 18, 1, 18, 1,
		18, 1, 18, 1, 18, 1, 18, 1, 18, 1, 18, 3, 18, 265, 8, 18, 1, 18, 3, 18,
		268, 8, 18, 1, 18, 1, 18, 1, 18, 1, 18, 1, 18, 1, 18, 1, 18, 1, 18, 1,
		18, 1, 18, 1, 18, 1, 18, 1, 18, 1, 18, 1, 18, 1, 18, 1, 18, 1, 18, 5, 18,
		288, 8, 18, 10, 18, 12, 18, 291, 9, 18, 1, 19, 1, 19, 1, 19, 5, 19, 296,
		8, 19, 10, 19, 12, 19, 299, 9, 19, 1, 19, 3, 19, 302, 8, 19, 1, 20, 1,
		20, 1, 20, 1, 20, 5, 20, 308, 8, 20, 10, 20, 12, 20, 311, 9, 20, 1, 20,
		1, 20, 1, 21, 1, 21, 1, 21, 5, 21, 318, 8, 21, 10, 21, 12, 21, 321, 9,
		21, 1, 21, 1, 21, 1, 22, 1, 22, 1, 22, 1, 22, 5, 22, 329, 8, 22, 10, 22,
		12, 22, 332, 9, 22, 1, 22, 3, 22, 335, 8, 22, 1, 22, 1, 22, 1, 23, 1, 23,
		1, 23, 1, 23, 5, 23, 343, 8, 23, 10, 23, 12, 23, 346, 9, 23, 1, 24, 1,
		24, 1, 24, 5, 24, 351, 8, 24, 10, 24, 12, 24, 354, 9, 24, 1, 25, 1, 25,
		1, 25, 1, 25, 5, 25, 360, 8, 25, 10, 25, 12, 25, 363, 9, 25, 1, 25, 1,
		25, 1, 26, 1, 26, 1, 26, 1, 26, 5, 26, 371, 8, 26, 10, 26, 12, 26, 374,
		9, 26, 1, 26, 1, 26, 1, 26, 1, 26, 1, 26, 1, 26, 1, 26, 1, 26, 1, 26, 1,
		26, 5, 26, 386, 8, 26, 10, 26, 12, 26, 389, 9, 26, 1, 26, 1, 26, 1, 26,
		1, 26, 1, 26, 1, 26, 1, 26, 1, 26, 1, 26, 1, 26, 5, 26, 401, 8, 26, 10,
		26, 12, 26, 404, 9, 26, 1, 26, 1, 26, 3, 26, 408, 8, 26, 1, 27, 1, 27,
		1, 27, 1, 27, 1, 27, 1, 27, 1, 28, 1, 28, 3, 28, 418, 8, 28, 1, 28, 1,
		28, 3, 28, 422, 8, 28, 1, 29, 1, 29, 1, 29, 3, 29, 427, 8, 29, 1, 29, 1,
		29, 1, 30, 1, 30, 5, 30, 433, 8, 30, 10, 30, 12, 30, 436, 9, 30, 1, 30,
		1, 30, 1, 31, 1, 31, 1, 31, 5, 31, 443, 8, 31, 10, 31, 12, 31, 446, 9,
		31, 1, 32, 3, 32, 449, 8, 32, 1, 32, 1, 32, 3, 32, 453, 8, 32, 1, 33, 1,
		33, 1, 33, 1, 33, 3, 33, 459, 8, 33, 1, 33, 1, 33, 3, 33, 463, 8, 33, 1,
		33, 1, 33, 5, 33, 467, 8, 33, 10, 33, 12, 33, 470, 9, 33, 1, 33, 1, 33,
		1, 34, 1, 34, 1, 34, 5, 34, 477, 8, 34, 10, 34, 12, 34, 480, 9, 34, 1,
		35, 1, 35, 1, 35, 1, 36, 1, 36, 1, 36, 1, 36, 4, 36, 489, 8, 36, 11, 36,
		12, 36, 490, 1, 36, 1, 36, 1, 37, 1, 37, 1, 37, 1, 38, 1, 38, 1, 38, 5,
		38, 501, 8, 38, 10, 38, 12, 38, 504, 9, 38, 1, 38, 3, 38, 507, 8, 38, 1,
		39, 1, 39, 1, 39, 1, 39, 1, 39, 0, 1, 36, 40, 0, 2, 4, 6, 8, 10, 12, 14,
		16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50,
		52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72, 74, 76, 78, 0, 7, 1, 0, 23,
		24, 1, 0, 22, 24, 2, 0, 18, 18, 33, 33, 1, 0, 19, 21, 1, 0, 17, 18, 1,
		0, 27, 30, 1, 0, 25, 26, 553, 0, 83, 1, 0, 0, 0, 2, 101, 1, 0, 0, 0, 4,
		133, 1, 0, 0, 0, 6, 135, 1, 0, 0, 0, 8, 137, 1, 0, 0, 0, 10, 150, 1, 0,
		0, 0, 12, 159, 1, 0, 0, 0, 14, 163, 1, 0, 0, 0, 16, 169, 1, 0, 0, 0, 18,
		181, 1, 0, 0, 0, 20, 185, 1, 0, 0, 0, 22, 191, 1, 0, 0, 0, 24, 205, 1,
		0, 0, 0, 26, 219, 1, 0, 0, 0, 28, 221, 1, 0, 0, 0, 30, 235, 1, 0, 0, 0,
		32, 237, 1, 0, 0, 0, 34, 243, 1, 0, 0, 0, 36, 267, 1, 0, 0, 0, 38, 292,
		1, 0, 0, 0, 40, 303, 1, 0, 0, 0, 42, 314, 1, 0, 0, 0, 44, 324, 1, 0, 0,
		0, 46, 338, 1, 0, 0, 0, 48, 347, 1, 0, 0, 0, 50, 355, 1, 0, 0, 0, 52, 407,
		1, 0, 0, 0, 54, 409, 1, 0, 0, 0, 56, 421, 1, 0, 0, 0, 58, 423, 1, 0, 0,
		0, 60, 430, 1, 0, 0, 0, 62, 439, 1, 0, 0, 0, 64, 448, 1, 0, 0, 0, 66, 454,
		1, 0, 0, 0, 68, 473, 1, 0, 0, 0, 70, 481, 1, 0, 0, 0, 72, 484, 1, 0, 0,
		0, 74, 494, 1, 0, 0, 0, 76, 497, 1, 0, 0, 0, 78, 508, 1, 0, 0, 0, 80, 82,
		3, 2, 1, 0, 81, 80, 1, 0, 0, 0, 82, 85, 1, 0, 0, 0, 83, 81, 1, 0, 0, 0,
		83, 84, 1, 0, 0, 0, 84, 87, 1, 0, 0, 0, 85, 83, 1, 0, 0, 0, 86, 88, 5,
		0, 0, 1, 87, 86, 1, 0, 0, 0, 87, 88, 1, 0, 0, 0, 88, 1, 1, 0, 0, 0, 89,
		102, 3, 4, 2, 0, 90, 102, 3, 26, 13, 0, 91, 102, 3, 60, 30, 0, 92, 102,
		3, 56, 28, 0, 93, 102, 3, 38, 19, 0, 94, 102, 3, 44, 22, 0, 95, 102, 3,
		50, 25, 0, 96, 102, 3, 52, 26, 0, 97, 102, 3, 58, 29, 0, 98, 102, 3, 14,
		7, 0, 99, 102, 3, 66, 33, 0, 100, 102, 3, 72, 36, 0, 101, 89, 1, 0, 0,
		0, 101, 90, 1, 0, 0, 0, 101, 91, 1, 0, 0, 0, 101, 92, 1, 0, 0, 0, 101,
		93, 1, 0, 0, 0, 101, 94, 1, 0, 0, 0, 101, 95, 1, 0, 0, 0, 101, 96, 1, 0,
		0, 0, 101, 97, 1, 0, 0, 0, 101, 98, 1, 0, 0, 0, 101, 99, 1, 0, 0, 0, 101,
		100, 1, 0, 0, 0, 102, 3, 1, 0, 0, 0, 103, 104, 3, 6, 3, 0, 104, 105, 5,
		50, 0, 0, 105, 106, 3, 24, 12, 0, 106, 107, 5, 22, 0, 0, 107, 108, 3, 36,
		18, 0, 108, 134, 1, 0, 0, 0, 109, 110, 3, 6, 3, 0, 110, 111, 5, 50, 0,
		0, 111, 112, 5, 22, 0, 0, 112, 113, 3, 36, 18, 0, 113, 134, 1, 0, 0, 0,
		114, 115, 3, 6, 3, 0, 115, 116, 5, 50, 0, 0, 116, 117, 3, 24, 12, 0, 117,
		134, 1, 0, 0, 0, 118, 119, 5, 50, 0, 0, 119, 120, 3, 24, 12, 0, 120, 121,
		5, 22, 0, 0, 121, 122, 3, 36, 18, 0, 122, 134, 1, 0, 0, 0, 123, 124, 5,
		50, 0, 0, 124, 125, 5, 22, 0, 0, 125, 126, 3, 18, 9, 0, 126, 127, 3, 8,
		4, 0, 127, 134, 1, 0, 0, 0, 128, 129, 5, 50, 0, 0, 129, 130, 5, 22, 0,
		0, 130, 131, 3, 20, 10, 0, 131, 132, 3, 22, 11, 0, 132, 134, 1, 0, 0, 0,
		133, 103, 1, 0, 0, 0, 133, 109, 1, 0, 0, 0, 133, 114, 1, 0, 0, 0, 133,
		118, 1, 0, 0, 0, 133, 123, 1, 0, 0, 0, 133, 128, 1, 0, 0, 0, 134, 5, 1,
		0, 0, 0, 135, 136, 5, 1, 0, 0, 136, 7, 1, 0, 0, 0, 137, 146, 5, 36, 0,
		0, 138, 143, 3, 36, 18, 0, 139, 140, 5, 43, 0, 0, 140, 142, 3, 36, 18,
		0, 141, 139, 1, 0, 0, 0, 142, 145, 1, 0, 0, 0, 143, 141, 1, 0, 0, 0, 143,
		144, 1, 0, 0, 0, 144, 147, 1, 0, 0, 0, 145, 143, 1, 0, 0, 0, 146, 138,
		1, 0, 0, 0, 146, 147, 1, 0, 0, 0, 147, 148, 1, 0, 0, 0, 148, 149, 5, 37,
		0, 0, 149, 9, 1, 0, 0, 0, 150, 155, 3, 28, 14, 0, 151, 152, 5, 38, 0, 0,
		152, 153, 3, 36, 18, 0, 153, 154, 5, 39, 0, 0, 154, 156, 1, 0, 0, 0, 155,
		151, 1, 0, 0, 0, 156, 157, 1, 0, 0, 0, 157, 155, 1, 0, 0, 0, 157, 158,
		1, 0, 0, 0, 158, 11, 1, 0, 0, 0, 159, 160, 3, 10, 5, 0, 160, 161, 5, 42,
		0, 0, 161, 162, 3, 28, 14, 0, 162, 13, 1, 0, 0, 0, 163, 164, 3, 10, 5,
		0, 164, 165, 5, 42, 0, 0, 165, 166, 3, 58, 29, 0, 166, 15, 1, 0, 0, 0,
		167, 170, 3, 18, 9, 0, 168, 170, 3, 20, 10, 0, 169, 167, 1, 0, 0, 0, 169,
		168, 1, 0, 0, 0, 170, 171, 1, 0, 0, 0, 171, 172, 5, 34, 0, 0, 172, 173,
		5, 50, 0, 0, 173, 174, 5, 41, 0, 0, 174, 175, 3, 36, 18, 0, 175, 176, 5,
		43, 0, 0, 176, 177, 5, 50, 0, 0, 177, 178, 5, 41, 0, 0, 178, 179, 3, 36,
		18, 0, 179, 180, 5, 35, 0, 0, 180, 17, 1, 0, 0, 0, 181, 182, 5, 38, 0,
		0, 182, 183, 5, 39, 0, 0, 183, 184, 5, 50, 0, 0, 184, 19, 1, 0, 0, 0, 185,
		186, 5, 38, 0, 0, 186, 187, 5, 39, 0, 0, 187, 188, 5, 38, 0, 0, 188, 189,
		5, 39, 0, 0, 189, 190, 5, 50, 0, 0, 190, 21, 1, 0, 0, 0, 191, 192, 5, 36,
		0, 0, 192, 197, 3, 8, 4, 0, 193, 194, 5, 43, 0, 0, 194, 196, 3, 8, 4, 0,
		195, 193, 1, 0, 0, 0, 196, 199, 1, 0, 0, 0, 197, 195, 1, 0, 0, 0, 197,
		198, 1, 0, 0, 0, 198, 200, 1, 0, 0, 0, 199, 197, 1, 0, 0, 0, 200, 201,
		5, 37, 0, 0, 201, 23, 1, 0, 0, 0, 202, 206, 5, 50, 0, 0, 203, 206, 3, 18,
		9, 0, 204, 206, 3, 20, 10, 0, 205, 202, 1, 0, 0, 0, 205, 203, 1, 0, 0,
		0, 205, 204, 1, 0, 0, 0, 206, 25, 1, 0, 0, 0, 207, 208, 3, 28, 14, 0, 208,
		209, 5, 22, 0, 0, 209, 210, 3, 36, 18, 0, 210, 220, 1, 0, 0, 0, 211, 212,
		3, 28, 14, 0, 212, 213, 7, 0, 0, 0, 213, 214, 3, 36, 18, 0, 214, 220, 1,
		0, 0, 0, 215, 216, 3, 10, 5, 0, 216, 217, 7, 1, 0, 0, 217, 218, 3, 36,
		18, 0, 218, 220, 1, 0, 0, 0, 219, 207, 1, 0, 0, 0, 219, 211, 1, 0, 0, 0,
		219, 215, 1, 0, 0, 0, 220, 27, 1, 0, 0, 0, 221, 226, 5, 50, 0, 0, 222,
		223, 5, 42, 0, 0, 223, 225, 5, 50, 0, 0, 224, 222, 1, 0, 0, 0, 225, 228,
		1, 0, 0, 0, 226, 224, 1, 0, 0, 0, 226, 227, 1, 0, 0, 0, 227, 29, 1, 0,
		0, 0, 228, 226, 1, 0, 0, 0, 229, 236, 5, 45, 0, 0, 230, 236, 5, 46, 0,
		0, 231, 236, 5, 47, 0, 0, 232, 236, 3, 32, 16, 0, 233, 236, 5, 48, 0, 0,
		234, 236, 5, 49, 0, 0, 235, 229, 1, 0, 0, 0, 235, 230, 1, 0, 0, 0, 235,
		231, 1, 0, 0, 0, 235, 232, 1, 0, 0, 0, 235, 233, 1, 0, 0, 0, 235, 234,
		1, 0, 0, 0, 236, 31, 1, 0, 0, 0, 237, 238, 5, 47, 0, 0, 238, 33, 1, 0,
		0, 0, 239, 240, 5, 50, 0, 0, 240, 244, 5, 16, 0, 0, 241, 242, 5, 50, 0,
		0, 242, 244, 5, 15, 0, 0, 243, 239, 1, 0, 0, 0, 243, 241, 1, 0, 0, 0, 244,
		35, 1, 0, 0, 0, 245, 246, 6, 18, -1, 0, 246, 247, 5, 34, 0, 0, 247, 248,
		3, 36, 18, 0, 248, 249, 5, 35, 0, 0, 249, 268, 1, 0, 0, 0, 250, 268, 3,
		58, 29, 0, 251, 268, 3, 28, 14, 0, 252, 268, 3, 10, 5, 0, 253, 268, 3,
		12, 6, 0, 254, 268, 3, 14, 7, 0, 255, 268, 3, 30, 15, 0, 256, 268, 3, 8,
		4, 0, 257, 268, 3, 16, 8, 0, 258, 268, 3, 34, 17, 0, 259, 260, 7, 2, 0,
		0, 260, 268, 3, 36, 18, 8, 261, 262, 5, 50, 0, 0, 262, 264, 5, 36, 0, 0,
		263, 265, 3, 76, 38, 0, 264, 263, 1, 0, 0, 0, 264, 265, 1, 0, 0, 0, 265,
		266, 1, 0, 0, 0, 266, 268, 5, 37, 0, 0, 267, 245, 1, 0, 0, 0, 267, 250,
		1, 0, 0, 0, 267, 251, 1, 0, 0, 0, 267, 252, 1, 0, 0, 0, 267, 253, 1, 0,
		0, 0, 267, 254, 1, 0, 0, 0, 267, 255, 1, 0, 0, 0, 267, 256, 1, 0, 0, 0,
		267, 257, 1, 0, 0, 0, 267, 258, 1, 0, 0, 0, 267, 259, 1, 0, 0, 0, 267,
		261, 1, 0, 0, 0, 268, 289, 1, 0, 0, 0, 269, 270, 10, 7, 0, 0, 270, 271,
		7, 3, 0, 0, 271, 288, 3, 36, 18, 8, 272, 273, 10, 6, 0, 0, 273, 274, 7,
		4, 0, 0, 274, 288, 3, 36, 18, 7, 275, 276, 10, 5, 0, 0, 276, 277, 7, 5,
		0, 0, 277, 288, 3, 36, 18, 6, 278, 279, 10, 4, 0, 0, 279, 280, 7, 6, 0,
		0, 280, 288, 3, 36, 18, 5, 281, 282, 10, 3, 0, 0, 282, 283, 5, 31, 0, 0,
		283, 288, 3, 36, 18, 4, 284, 285, 10, 2, 0, 0, 285, 286, 5, 32, 0, 0, 286,
		288, 3, 36, 18, 3, 287, 269, 1, 0, 0, 0, 287, 272, 1, 0, 0, 0, 287, 275,
		1, 0, 0, 0, 287, 278, 1, 0, 0, 0, 287, 281, 1, 0, 0, 0, 287, 284, 1, 0,
		0, 0, 288, 291, 1, 0, 0, 0, 289, 287, 1, 0, 0, 0, 289, 290, 1, 0, 0, 0,
		290, 37, 1, 0, 0, 0, 291, 289, 1, 0, 0, 0, 292, 297, 3, 40, 20, 0, 293,
		294, 5, 5, 0, 0, 294, 296, 3, 40, 20, 0, 295, 293, 1, 0, 0, 0, 296, 299,
		1, 0, 0, 0, 297, 295, 1, 0, 0, 0, 297, 298, 1, 0, 0, 0, 298, 301, 1, 0,
		0, 0, 299, 297, 1, 0, 0, 0, 300, 302, 3, 42, 21, 0, 301, 300, 1, 0, 0,
		0, 301, 302, 1, 0, 0, 0, 302, 39, 1, 0, 0, 0, 303, 304, 5, 4, 0, 0, 304,
		305, 3, 36, 18, 0, 305, 309, 5, 36, 0, 0, 306, 308, 3, 2, 1, 0, 307, 306,
		1, 0, 0, 0, 308, 311, 1, 0, 0, 0, 309, 307, 1, 0, 0, 0, 309, 310, 1, 0,
		0, 0, 310, 312, 1, 0, 0, 0, 311, 309, 1, 0, 0, 0, 312, 313, 5, 37, 0, 0,
		313, 41, 1, 0, 0, 0, 314, 315, 5, 5, 0, 0, 315, 319, 5, 36, 0, 0, 316,
		318, 3, 2, 1, 0, 317, 316, 1, 0, 0, 0, 318, 321, 1, 0, 0, 0, 319, 317,
		1, 0, 0, 0, 319, 320, 1, 0, 0, 0, 320, 322, 1, 0, 0, 0, 321, 319, 1, 0,
		0, 0, 322, 323, 5, 37, 0, 0, 323, 43, 1, 0, 0, 0, 324, 325, 5, 6, 0, 0,
		325, 326, 3, 36, 18, 0, 326, 330, 5, 36, 0, 0, 327, 329, 3, 46, 23, 0,
		328, 327, 1, 0, 0, 0, 329, 332, 1, 0, 0, 0, 330, 328, 1, 0, 0, 0, 330,
		331, 1, 0, 0, 0, 331, 334, 1, 0, 0, 0, 332, 330, 1, 0, 0, 0, 333, 335,
		3, 48, 24, 0, 334, 333, 1, 0, 0, 0, 334, 335, 1, 0, 0, 0, 335, 336, 1,
		0, 0, 0, 336, 337, 5, 37, 0, 0, 337, 45, 1, 0, 0, 0, 338, 339, 5, 7, 0,
		0, 339, 340, 3, 36, 18, 0, 340, 344, 5, 41, 0, 0, 341, 343, 3, 2, 1, 0,
		342, 341, 1, 0, 0, 0, 343, 346, 1, 0, 0, 0, 344, 342, 1, 0, 0, 0, 344,
		345, 1, 0, 0, 0, 345, 47, 1, 0, 0, 0, 346, 344, 1, 0, 0, 0, 347, 348, 5,
		8, 0, 0, 348, 352, 5, 41, 0, 0, 349, 351, 3, 2, 1, 0, 350, 349, 1, 0, 0,
		0, 351, 354, 1, 0, 0, 0, 352, 350, 1, 0, 0, 0, 352, 353, 1, 0, 0, 0, 353,
		49, 1, 0, 0, 0, 354, 352, 1, 0, 0, 0, 355, 356, 5, 10, 0, 0, 356, 357,
		3, 36, 18, 0, 357, 361, 5, 36, 0, 0, 358, 360, 3, 2, 1, 0, 359, 358, 1,
		0, 0, 0, 360, 363, 1, 0, 0, 0, 361, 359, 1, 0, 0, 0, 361, 362, 1, 0, 0,
		0, 362, 364, 1, 0, 0, 0, 363, 361, 1, 0, 0, 0, 364, 365, 5, 37, 0, 0, 365,
		51, 1, 0, 0, 0, 366, 367, 5, 9, 0, 0, 367, 368, 3, 36, 18, 0, 368, 372,
		5, 36, 0, 0, 369, 371, 3, 2, 1, 0, 370, 369, 1, 0, 0, 0, 371, 374, 1, 0,
		0, 0, 372, 370, 1, 0, 0, 0, 372, 373, 1, 0, 0, 0, 373, 375, 1, 0, 0, 0,
		374, 372, 1, 0, 0, 0, 375, 376, 5, 37, 0, 0, 376, 408, 1, 0, 0, 0, 377,
		378, 5, 9, 0, 0, 378, 379, 3, 26, 13, 0, 379, 380, 5, 40, 0, 0, 380, 381,
		3, 36, 18, 0, 381, 382, 5, 40, 0, 0, 382, 383, 3, 36, 18, 0, 383, 387,
		5, 36, 0, 0, 384, 386, 3, 2, 1, 0, 385, 384, 1, 0, 0, 0, 386, 389, 1, 0,
		0, 0, 387, 385, 1, 0, 0, 0, 387, 388, 1, 0, 0, 0, 388, 390, 1, 0, 0, 0,
		389, 387, 1, 0, 0, 0, 390, 391, 5, 37, 0, 0, 391, 408, 1, 0, 0, 0, 392,
		393, 5, 9, 0, 0, 393, 394, 5, 50, 0, 0, 394, 395, 5, 43, 0, 0, 395, 396,
		5, 50, 0, 0, 396, 397, 5, 11, 0, 0, 397, 398, 3, 36, 18, 0, 398, 402, 5,
		36, 0, 0, 399, 401, 3, 2, 1, 0, 400, 399, 1, 0, 0, 0, 401, 404, 1, 0, 0,
		0, 402, 400, 1, 0, 0, 0, 402, 403, 1, 0, 0, 0, 403, 405, 1, 0, 0, 0, 404,
		402, 1, 0, 0, 0, 405, 406, 5, 37, 0, 0, 406, 408, 1, 0, 0, 0, 407, 366,
		1, 0, 0, 0, 407, 377, 1, 0, 0, 0, 407, 392, 1, 0, 0, 0, 408, 53, 1, 0,
		0, 0, 409, 410, 3, 36, 18, 0, 410, 411, 5, 42, 0, 0, 411, 412, 5, 42, 0,
		0, 412, 413, 5, 42, 0, 0, 413, 414, 3, 36, 18, 0, 414, 55, 1, 0, 0, 0,
		415, 417, 5, 14, 0, 0, 416, 418, 3, 36, 18, 0, 417, 416, 1, 0, 0, 0, 417,
		418, 1, 0, 0, 0, 418, 422, 1, 0, 0, 0, 419, 422, 5, 12, 0, 0, 420, 422,
		5, 13, 0, 0, 421, 415, 1, 0, 0, 0, 421, 419, 1, 0, 0, 0, 421, 420, 1, 0,
		0, 0, 422, 57, 1, 0, 0, 0, 423, 424, 3, 28, 14, 0, 424, 426, 5, 34, 0,
		0, 425, 427, 3, 62, 31, 0, 426, 425, 1, 0, 0, 0, 426, 427, 1, 0, 0, 0,
		427, 428, 1, 0, 0, 0, 428, 429, 5, 35, 0, 0, 429, 59, 1, 0, 0, 0, 430,
		434, 5, 36, 0, 0, 431, 433, 3, 2, 1, 0, 432, 431, 1, 0, 0, 0, 433, 436,
		1, 0, 0, 0, 434, 432, 1, 0, 0, 0, 434, 435, 1, 0, 0, 0, 435, 437, 1, 0,
		0, 0, 436, 434, 1, 0, 0, 0, 437, 438, 5, 37, 0, 0, 438, 61, 1, 0, 0, 0,
		439, 444, 3, 64, 32, 0, 440, 441, 5, 43, 0, 0, 441, 443, 3, 64, 32, 0,
		442, 440, 1, 0, 0, 0, 443, 446, 1, 0, 0, 0, 444, 442, 1, 0, 0, 0, 444,
		445, 1, 0, 0, 0, 445, 63, 1, 0, 0, 0, 446, 444, 1, 0, 0, 0, 447, 449, 5,
		50, 0, 0, 448, 447, 1, 0, 0, 0, 448, 449, 1, 0, 0, 0, 449, 452, 1, 0, 0,
		0, 450, 453, 3, 28, 14, 0, 451, 453, 3, 36, 18, 0, 452, 450, 1, 0, 0, 0,
		452, 451, 1, 0, 0, 0, 453, 65, 1, 0, 0, 0, 454, 455, 5, 2, 0, 0, 455, 456,
		5, 50, 0, 0, 456, 458, 5, 34, 0, 0, 457, 459, 3, 68, 34, 0, 458, 457, 1,
		0, 0, 0, 458, 459, 1, 0, 0, 0, 459, 460, 1, 0, 0, 0, 460, 462, 5, 35, 0,
		0, 461, 463, 3, 24, 12, 0, 462, 461, 1, 0, 0, 0, 462, 463, 1, 0, 0, 0,
		463, 464, 1, 0, 0, 0, 464, 468, 5, 36, 0, 0, 465, 467, 3, 2, 1, 0, 466,
		465, 1, 0, 0, 0, 467, 470, 1, 0, 0, 0, 468, 466, 1, 0, 0, 0, 468, 469,
		1, 0, 0, 0, 469, 471, 1, 0, 0, 0, 470, 468, 1, 0, 0, 0, 471, 472, 5, 37,
		0, 0, 472, 67, 1, 0, 0, 0, 473, 478, 3, 70, 35, 0, 474, 475, 5, 43, 0,
		0, 475, 477, 3, 70, 35, 0, 476, 474, 1, 0, 0, 0, 477, 480, 1, 0, 0, 0,
		478, 476, 1, 0, 0, 0, 478, 479, 1, 0, 0, 0, 479, 69, 1, 0, 0, 0, 480, 478,
		1, 0, 0, 0, 481, 482, 5, 50, 0, 0, 482, 483, 3, 24, 12, 0, 483, 71, 1,
		0, 0, 0, 484, 485, 5, 3, 0, 0, 485, 486, 5, 50, 0, 0, 486, 488, 5, 36,
		0, 0, 487, 489, 3, 74, 37, 0, 488, 487, 1, 0, 0, 0, 489, 490, 1, 0, 0,
		0, 490, 488, 1, 0, 0, 0, 490, 491, 1, 0, 0, 0, 491, 492, 1, 0, 0, 0, 492,
		493, 5, 37, 0, 0, 493, 73, 1, 0, 0, 0, 494, 495, 3, 24, 12, 0, 495, 496,
		5, 50, 0, 0, 496, 75, 1, 0, 0, 0, 497, 502, 3, 78, 39, 0, 498, 499, 5,
		43, 0, 0, 499, 501, 3, 78, 39, 0, 500, 498, 1, 0, 0, 0, 501, 504, 1, 0,
		0, 0, 502, 500, 1, 0, 0, 0, 502, 503, 1, 0, 0, 0, 503, 506, 1, 0, 0, 0,
		504, 502, 1, 0, 0, 0, 505, 507, 5, 43, 0, 0, 506, 505, 1, 0, 0, 0, 506,
		507, 1, 0, 0, 0, 507, 77, 1, 0, 0, 0, 508, 509, 5, 50, 0, 0, 509, 510,
		5, 41, 0, 0, 510, 511, 3, 36, 18, 0, 511, 79, 1, 0, 0, 0, 45, 83, 87, 101,
		133, 143, 146, 157, 169, 197, 205, 219, 226, 235, 243, 264, 267, 287, 289,
		297, 301, 309, 319, 330, 334, 344, 352, 361, 372, 387, 402, 407, 417, 421,
		426, 434, 444, 448, 452, 458, 462, 468, 478, 490, 502, 506,
	}
	deserializer := antlr.NewATNDeserializer(nil)
	staticData.atn = deserializer.Deserialize(staticData.serializedATN)
	atn := staticData.atn
	staticData.decisionToDFA = make([]*antlr.DFA, len(atn.DecisionToState))
	decisionToDFA := staticData.decisionToDFA
	for index, state := range atn.DecisionToState {
		decisionToDFA[index] = antlr.NewDFA(state, index)
	}
}

// VLangGrammarInit initializes any static state used to implement VLangGrammar. By default the
// static state used to implement the parser is lazily initialized during the first call to
// NewVLangGrammar(). You can call this function if you wish to initialize the static state ahead
// of time.
func VLangGrammarInit() {
	staticData := &VLangGrammarParserStaticData
	staticData.once.Do(vlanggrammarParserInit)
}

// NewVLangGrammar produces a new parser instance for the optional input antlr.TokenStream.
func NewVLangGrammar(input antlr.TokenStream) *VLangGrammar {
	VLangGrammarInit()
	this := new(VLangGrammar)
	this.BaseParser = antlr.NewBaseParser(input)
	staticData := &VLangGrammarParserStaticData
	this.Interpreter = antlr.NewParserATNSimulator(this, staticData.atn, staticData.decisionToDFA, staticData.PredictionContextCache)
	this.RuleNames = staticData.RuleNames
	this.LiteralNames = staticData.LiteralNames
	this.SymbolicNames = staticData.SymbolicNames
	this.GrammarFileName = "VLangGrammar.g4"

	return this
}

// VLangGrammar tokens.
const (
	VLangGrammarEOF            = antlr.TokenEOF
	VLangGrammarMUT            = 1
	VLangGrammarFUNC           = 2
	VLangGrammarSTR            = 3
	VLangGrammarIF_KW          = 4
	VLangGrammarELSE_KW        = 5
	VLangGrammarSWITCH_KW      = 6
	VLangGrammarCASE_KW        = 7
	VLangGrammarDEFAULT_KW     = 8
	VLangGrammarFOR_KW         = 9
	VLangGrammarWHILE_KW       = 10
	VLangGrammarIN_KW          = 11
	VLangGrammarBREAK_KW       = 12
	VLangGrammarCONTINUE_KW    = 13
	VLangGrammarRETURN_KW      = 14
	VLangGrammarDEC            = 15
	VLangGrammarINC            = 16
	VLangGrammarPLUS           = 17
	VLangGrammarMINUS          = 18
	VLangGrammarMULT           = 19
	VLangGrammarDIV            = 20
	VLangGrammarMOD            = 21
	VLangGrammarASSIGN         = 22
	VLangGrammarPLUS_ASSIGN    = 23
	VLangGrammarMINUS_ASSIGN   = 24
	VLangGrammarEQ             = 25
	VLangGrammarNE             = 26
	VLangGrammarLT             = 27
	VLangGrammarLE             = 28
	VLangGrammarGT             = 29
	VLangGrammarGE             = 30
	VLangGrammarAND            = 31
	VLangGrammarOR             = 32
	VLangGrammarNOT            = 33
	VLangGrammarLPAREN         = 34
	VLangGrammarRPAREN         = 35
	VLangGrammarLBRACE         = 36
	VLangGrammarRBRACE         = 37
	VLangGrammarLBRACK         = 38
	VLangGrammarRBRACK         = 39
	VLangGrammarSEMI           = 40
	VLangGrammarCOLON          = 41
	VLangGrammarDOT            = 42
	VLangGrammarCOMMA          = 43
	VLangGrammarDOLLAR         = 44
	VLangGrammarINT_LITERAL    = 45
	VLangGrammarFLOAT_LITERAL  = 46
	VLangGrammarSTRING_LITERAL = 47
	VLangGrammarBOOL_LITERAL   = 48
	VLangGrammarNIL_LITERAL    = 49
	VLangGrammarID             = 50
	VLangGrammarWS             = 51
	VLangGrammarLINE_COMMENT   = 52
	VLangGrammarBLOCK_COMMENT  = 53
)

// VLangGrammar rules.
const (
	VLangGrammarRULE_program             = 0
	VLangGrammarRULE_stmt                = 1
	VLangGrammarRULE_decl_stmt           = 2
	VLangGrammarRULE_var_type            = 3
	VLangGrammarRULE_vect_expr           = 4
	VLangGrammarRULE_vect_item           = 5
	VLangGrammarRULE_vect_prop           = 6
	VLangGrammarRULE_vect_func           = 7
	VLangGrammarRULE_repeating           = 8
	VLangGrammarRULE_vector_type         = 9
	VLangGrammarRULE_matrix_type         = 10
	VLangGrammarRULE_matrix_expr         = 11
	VLangGrammarRULE_type                = 12
	VLangGrammarRULE_assign_stmt         = 13
	VLangGrammarRULE_id_pattern          = 14
	VLangGrammarRULE_literal             = 15
	VLangGrammarRULE_interpolated_string = 16
	VLangGrammarRULE_incredecre          = 17
	VLangGrammarRULE_expression          = 18
	VLangGrammarRULE_if_stmt             = 19
	VLangGrammarRULE_if_chain            = 20
	VLangGrammarRULE_else_stmt           = 21
	VLangGrammarRULE_switch_stmt         = 22
	VLangGrammarRULE_switch_case         = 23
	VLangGrammarRULE_default_case        = 24
	VLangGrammarRULE_while_stmt          = 25
	VLangGrammarRULE_for_stmt            = 26
	VLangGrammarRULE_range               = 27
	VLangGrammarRULE_transfer_stmt       = 28
	VLangGrammarRULE_func_call           = 29
	VLangGrammarRULE_block_ind           = 30
	VLangGrammarRULE_arg_list            = 31
	VLangGrammarRULE_func_arg            = 32
	VLangGrammarRULE_func_dcl            = 33
	VLangGrammarRULE_param_list          = 34
	VLangGrammarRULE_func_param          = 35
	VLangGrammarRULE_strct_dcl           = 36
	VLangGrammarRULE_struct_prop         = 37
	VLangGrammarRULE_struct_param_list   = 38
	VLangGrammarRULE_struct_param        = 39
)

// IProgramContext is an interface to support dynamic dispatch.
type IProgramContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser

	// Getter signatures
	AllStmt() []IStmtContext
	Stmt(i int) IStmtContext
	EOF() antlr.TerminalNode

	// IsProgramContext differentiates from other interfaces.
	IsProgramContext()
}

type ProgramContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyProgramContext() *ProgramContext {
	var p = new(ProgramContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_program
	return p
}

func InitEmptyProgramContext(p *ProgramContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_program
}

func (*ProgramContext) IsProgramContext() {}

func NewProgramContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *ProgramContext {
	var p = new(ProgramContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_program

	return p
}

func (s *ProgramContext) GetParser() antlr.Parser { return s.parser }

func (s *ProgramContext) AllStmt() []IStmtContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IStmtContext); ok {
			len++
		}
	}

	tst := make([]IStmtContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IStmtContext); ok {
			tst[i] = t.(IStmtContext)
			i++
		}
	}

	return tst
}

func (s *ProgramContext) Stmt(i int) IStmtContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IStmtContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IStmtContext)
}

func (s *ProgramContext) EOF() antlr.TerminalNode {
	return s.GetToken(VLangGrammarEOF, 0)
}

func (s *ProgramContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *ProgramContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

func (s *ProgramContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterProgram(s)
	}
}

func (s *ProgramContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitProgram(s)
	}
}

func (s *ProgramContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitProgram(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Program() (localctx IProgramContext) {
	localctx = NewProgramContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 0, VLangGrammarRULE_program)
	var _la int

	p.EnterOuterAlt(localctx, 1)
	p.SetState(83)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	for (int64(_la) & ^0x3f) == 0 && ((int64(1)<<_la)&1125968626349662) != 0 {
		{
			p.SetState(80)
			p.Stmt()
		}

		p.SetState(85)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_la = p.GetTokenStream().LA(1)
	}
	p.SetState(87)
	p.GetErrorHandler().Sync(p)

	if p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 1, p.GetParserRuleContext()) == 1 {
		{
			p.SetState(86)
			p.Match(VLangGrammarEOF)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}

	} else if p.HasError() { // JIM
		goto errorExit
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IStmtContext is an interface to support dynamic dispatch.
type IStmtContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser

	// Getter signatures
	Decl_stmt() IDecl_stmtContext
	Assign_stmt() IAssign_stmtContext
	Block_ind() IBlock_indContext
	Transfer_stmt() ITransfer_stmtContext
	If_stmt() IIf_stmtContext
	Switch_stmt() ISwitch_stmtContext
	While_stmt() IWhile_stmtContext
	For_stmt() IFor_stmtContext
	Func_call() IFunc_callContext
	Vect_func() IVect_funcContext
	Func_dcl() IFunc_dclContext
	Strct_dcl() IStrct_dclContext

	// IsStmtContext differentiates from other interfaces.
	IsStmtContext()
}

type StmtContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyStmtContext() *StmtContext {
	var p = new(StmtContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_stmt
	return p
}

func InitEmptyStmtContext(p *StmtContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_stmt
}

func (*StmtContext) IsStmtContext() {}

func NewStmtContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *StmtContext {
	var p = new(StmtContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_stmt

	return p
}

func (s *StmtContext) GetParser() antlr.Parser { return s.parser }

func (s *StmtContext) Decl_stmt() IDecl_stmtContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IDecl_stmtContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IDecl_stmtContext)
}

func (s *StmtContext) Assign_stmt() IAssign_stmtContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IAssign_stmtContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IAssign_stmtContext)
}

func (s *StmtContext) Block_ind() IBlock_indContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IBlock_indContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IBlock_indContext)
}

func (s *StmtContext) Transfer_stmt() ITransfer_stmtContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(ITransfer_stmtContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(ITransfer_stmtContext)
}

func (s *StmtContext) If_stmt() IIf_stmtContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IIf_stmtContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IIf_stmtContext)
}

func (s *StmtContext) Switch_stmt() ISwitch_stmtContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(ISwitch_stmtContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(ISwitch_stmtContext)
}

func (s *StmtContext) While_stmt() IWhile_stmtContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IWhile_stmtContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IWhile_stmtContext)
}

func (s *StmtContext) For_stmt() IFor_stmtContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IFor_stmtContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IFor_stmtContext)
}

func (s *StmtContext) Func_call() IFunc_callContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IFunc_callContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IFunc_callContext)
}

func (s *StmtContext) Vect_func() IVect_funcContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IVect_funcContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IVect_funcContext)
}

func (s *StmtContext) Func_dcl() IFunc_dclContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IFunc_dclContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IFunc_dclContext)
}

func (s *StmtContext) Strct_dcl() IStrct_dclContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IStrct_dclContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IStrct_dclContext)
}

func (s *StmtContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *StmtContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

func (s *StmtContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterStmt(s)
	}
}

func (s *StmtContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitStmt(s)
	}
}

func (s *StmtContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitStmt(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Stmt() (localctx IStmtContext) {
	localctx = NewStmtContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 2, VLangGrammarRULE_stmt)
	p.SetState(101)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}

	switch p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 2, p.GetParserRuleContext()) {
	case 1:
		p.EnterOuterAlt(localctx, 1)
		{
			p.SetState(89)
			p.Decl_stmt()
		}

	case 2:
		p.EnterOuterAlt(localctx, 2)
		{
			p.SetState(90)
			p.Assign_stmt()
		}

	case 3:
		p.EnterOuterAlt(localctx, 3)
		{
			p.SetState(91)
			p.Block_ind()
		}

	case 4:
		p.EnterOuterAlt(localctx, 4)
		{
			p.SetState(92)
			p.Transfer_stmt()
		}

	case 5:
		p.EnterOuterAlt(localctx, 5)
		{
			p.SetState(93)
			p.If_stmt()
		}

	case 6:
		p.EnterOuterAlt(localctx, 6)
		{
			p.SetState(94)
			p.Switch_stmt()
		}

	case 7:
		p.EnterOuterAlt(localctx, 7)
		{
			p.SetState(95)
			p.While_stmt()
		}

	case 8:
		p.EnterOuterAlt(localctx, 8)
		{
			p.SetState(96)
			p.For_stmt()
		}

	case 9:
		p.EnterOuterAlt(localctx, 9)
		{
			p.SetState(97)
			p.Func_call()
		}

	case 10:
		p.EnterOuterAlt(localctx, 10)
		{
			p.SetState(98)
			p.Vect_func()
		}

	case 11:
		p.EnterOuterAlt(localctx, 11)
		{
			p.SetState(99)
			p.Func_dcl()
		}

	case 12:
		p.EnterOuterAlt(localctx, 12)
		{
			p.SetState(100)
			p.Strct_dcl()
		}

	case antlr.ATNInvalidAltNumber:
		goto errorExit
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IDecl_stmtContext is an interface to support dynamic dispatch.
type IDecl_stmtContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsDecl_stmtContext differentiates from other interfaces.
	IsDecl_stmtContext()
}

type Decl_stmtContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyDecl_stmtContext() *Decl_stmtContext {
	var p = new(Decl_stmtContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_decl_stmt
	return p
}

func InitEmptyDecl_stmtContext(p *Decl_stmtContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_decl_stmt
}

func (*Decl_stmtContext) IsDecl_stmtContext() {}

func NewDecl_stmtContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Decl_stmtContext {
	var p = new(Decl_stmtContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_decl_stmt

	return p
}

func (s *Decl_stmtContext) GetParser() antlr.Parser { return s.parser }

func (s *Decl_stmtContext) CopyAll(ctx *Decl_stmtContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Decl_stmtContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Decl_stmtContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type VarAssDeclContext struct {
	Decl_stmtContext
}

func NewVarAssDeclContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *VarAssDeclContext {
	var p = new(VarAssDeclContext)

	InitEmptyDecl_stmtContext(&p.Decl_stmtContext)
	p.parser = parser
	p.CopyAll(ctx.(*Decl_stmtContext))

	return p
}

func (s *VarAssDeclContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *VarAssDeclContext) ID() antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, 0)
}

func (s *VarAssDeclContext) Type_() ITypeContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(ITypeContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(ITypeContext)
}

func (s *VarAssDeclContext) ASSIGN() antlr.TerminalNode {
	return s.GetToken(VLangGrammarASSIGN, 0)
}

func (s *VarAssDeclContext) Expression() IExpressionContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *VarAssDeclContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterVarAssDecl(s)
	}
}

func (s *VarAssDeclContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitVarAssDecl(s)
	}
}

func (s *VarAssDeclContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitVarAssDecl(s)

	default:
		return t.VisitChildren(s)
	}
}

type VarMatrixDeclContext struct {
	Decl_stmtContext
}

func NewVarMatrixDeclContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *VarMatrixDeclContext {
	var p = new(VarMatrixDeclContext)

	InitEmptyDecl_stmtContext(&p.Decl_stmtContext)
	p.parser = parser
	p.CopyAll(ctx.(*Decl_stmtContext))

	return p
}

func (s *VarMatrixDeclContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *VarMatrixDeclContext) ID() antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, 0)
}

func (s *VarMatrixDeclContext) ASSIGN() antlr.TerminalNode {
	return s.GetToken(VLangGrammarASSIGN, 0)
}

func (s *VarMatrixDeclContext) Matrix_type() IMatrix_typeContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IMatrix_typeContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IMatrix_typeContext)
}

func (s *VarMatrixDeclContext) Matrix_expr() IMatrix_exprContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IMatrix_exprContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IMatrix_exprContext)
}

func (s *VarMatrixDeclContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterVarMatrixDecl(s)
	}
}

func (s *VarMatrixDeclContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitVarMatrixDecl(s)
	}
}

func (s *VarMatrixDeclContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitVarMatrixDecl(s)

	default:
		return t.VisitChildren(s)
	}
}

type ValDeclVecContext struct {
	Decl_stmtContext
}

func NewValDeclVecContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *ValDeclVecContext {
	var p = new(ValDeclVecContext)

	InitEmptyDecl_stmtContext(&p.Decl_stmtContext)
	p.parser = parser
	p.CopyAll(ctx.(*Decl_stmtContext))

	return p
}

func (s *ValDeclVecContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *ValDeclVecContext) Var_type() IVar_typeContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IVar_typeContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IVar_typeContext)
}

func (s *ValDeclVecContext) ID() antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, 0)
}

func (s *ValDeclVecContext) Type_() ITypeContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(ITypeContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(ITypeContext)
}

func (s *ValDeclVecContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterValDeclVec(s)
	}
}

func (s *ValDeclVecContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitValDeclVec(s)
	}
}

func (s *ValDeclVecContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitValDeclVec(s)

	default:
		return t.VisitChildren(s)
	}
}

type ValueDeclContext struct {
	Decl_stmtContext
}

func NewValueDeclContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *ValueDeclContext {
	var p = new(ValueDeclContext)

	InitEmptyDecl_stmtContext(&p.Decl_stmtContext)
	p.parser = parser
	p.CopyAll(ctx.(*Decl_stmtContext))

	return p
}

func (s *ValueDeclContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *ValueDeclContext) Var_type() IVar_typeContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IVar_typeContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IVar_typeContext)
}

func (s *ValueDeclContext) ID() antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, 0)
}

func (s *ValueDeclContext) ASSIGN() antlr.TerminalNode {
	return s.GetToken(VLangGrammarASSIGN, 0)
}

func (s *ValueDeclContext) Expression() IExpressionContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *ValueDeclContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterValueDecl(s)
	}
}

func (s *ValueDeclContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitValueDecl(s)
	}
}

func (s *ValueDeclContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitValueDecl(s)

	default:
		return t.VisitChildren(s)
	}
}

type MutVarDeclContext struct {
	Decl_stmtContext
}

func NewMutVarDeclContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *MutVarDeclContext {
	var p = new(MutVarDeclContext)

	InitEmptyDecl_stmtContext(&p.Decl_stmtContext)
	p.parser = parser
	p.CopyAll(ctx.(*Decl_stmtContext))

	return p
}

func (s *MutVarDeclContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *MutVarDeclContext) Var_type() IVar_typeContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IVar_typeContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IVar_typeContext)
}

func (s *MutVarDeclContext) ID() antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, 0)
}

func (s *MutVarDeclContext) Type_() ITypeContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(ITypeContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(ITypeContext)
}

func (s *MutVarDeclContext) ASSIGN() antlr.TerminalNode {
	return s.GetToken(VLangGrammarASSIGN, 0)
}

func (s *MutVarDeclContext) Expression() IExpressionContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *MutVarDeclContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterMutVarDecl(s)
	}
}

func (s *MutVarDeclContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitMutVarDecl(s)
	}
}

func (s *MutVarDeclContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitMutVarDecl(s)

	default:
		return t.VisitChildren(s)
	}
}

type VarVectDeclContext struct {
	Decl_stmtContext
}

func NewVarVectDeclContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *VarVectDeclContext {
	var p = new(VarVectDeclContext)

	InitEmptyDecl_stmtContext(&p.Decl_stmtContext)
	p.parser = parser
	p.CopyAll(ctx.(*Decl_stmtContext))

	return p
}

func (s *VarVectDeclContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *VarVectDeclContext) ID() antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, 0)
}

func (s *VarVectDeclContext) ASSIGN() antlr.TerminalNode {
	return s.GetToken(VLangGrammarASSIGN, 0)
}

func (s *VarVectDeclContext) Vector_type() IVector_typeContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IVector_typeContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IVector_typeContext)
}

func (s *VarVectDeclContext) Vect_expr() IVect_exprContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IVect_exprContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IVect_exprContext)
}

func (s *VarVectDeclContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterVarVectDecl(s)
	}
}

func (s *VarVectDeclContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitVarVectDecl(s)
	}
}

func (s *VarVectDeclContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitVarVectDecl(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Decl_stmt() (localctx IDecl_stmtContext) {
	localctx = NewDecl_stmtContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 4, VLangGrammarRULE_decl_stmt)
	p.SetState(133)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}

	switch p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 3, p.GetParserRuleContext()) {
	case 1:
		localctx = NewMutVarDeclContext(p, localctx)
		p.EnterOuterAlt(localctx, 1)
		{
			p.SetState(103)
			p.Var_type()
		}
		{
			p.SetState(104)
			p.Match(VLangGrammarID)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(105)
			p.Type_()
		}
		{
			p.SetState(106)
			p.Match(VLangGrammarASSIGN)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(107)
			p.expression(0)
		}

	case 2:
		localctx = NewValueDeclContext(p, localctx)
		p.EnterOuterAlt(localctx, 2)
		{
			p.SetState(109)
			p.Var_type()
		}
		{
			p.SetState(110)
			p.Match(VLangGrammarID)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(111)
			p.Match(VLangGrammarASSIGN)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(112)
			p.expression(0)
		}

	case 3:
		localctx = NewValDeclVecContext(p, localctx)
		p.EnterOuterAlt(localctx, 3)
		{
			p.SetState(114)
			p.Var_type()
		}
		{
			p.SetState(115)
			p.Match(VLangGrammarID)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(116)
			p.Type_()
		}

	case 4:
		localctx = NewVarAssDeclContext(p, localctx)
		p.EnterOuterAlt(localctx, 4)
		{
			p.SetState(118)
			p.Match(VLangGrammarID)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(119)
			p.Type_()
		}
		{
			p.SetState(120)
			p.Match(VLangGrammarASSIGN)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(121)
			p.expression(0)
		}

	case 5:
		localctx = NewVarVectDeclContext(p, localctx)
		p.EnterOuterAlt(localctx, 5)
		{
			p.SetState(123)
			p.Match(VLangGrammarID)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(124)
			p.Match(VLangGrammarASSIGN)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(125)
			p.Vector_type()
		}
		{
			p.SetState(126)
			p.Vect_expr()
		}

	case 6:
		localctx = NewVarMatrixDeclContext(p, localctx)
		p.EnterOuterAlt(localctx, 6)
		{
			p.SetState(128)
			p.Match(VLangGrammarID)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(129)
			p.Match(VLangGrammarASSIGN)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(130)
			p.Matrix_type()
		}
		{
			p.SetState(131)
			p.Matrix_expr()
		}

	case antlr.ATNInvalidAltNumber:
		goto errorExit
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IVar_typeContext is an interface to support dynamic dispatch.
type IVar_typeContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser

	// Getter signatures
	MUT() antlr.TerminalNode

	// IsVar_typeContext differentiates from other interfaces.
	IsVar_typeContext()
}

type Var_typeContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyVar_typeContext() *Var_typeContext {
	var p = new(Var_typeContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_var_type
	return p
}

func InitEmptyVar_typeContext(p *Var_typeContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_var_type
}

func (*Var_typeContext) IsVar_typeContext() {}

func NewVar_typeContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Var_typeContext {
	var p = new(Var_typeContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_var_type

	return p
}

func (s *Var_typeContext) GetParser() antlr.Parser { return s.parser }

func (s *Var_typeContext) MUT() antlr.TerminalNode {
	return s.GetToken(VLangGrammarMUT, 0)
}

func (s *Var_typeContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Var_typeContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

func (s *Var_typeContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterVar_type(s)
	}
}

func (s *Var_typeContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitVar_type(s)
	}
}

func (s *Var_typeContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitVar_type(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Var_type() (localctx IVar_typeContext) {
	localctx = NewVar_typeContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 6, VLangGrammarRULE_var_type)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(135)
		p.Match(VLangGrammarMUT)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IVect_exprContext is an interface to support dynamic dispatch.
type IVect_exprContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsVect_exprContext differentiates from other interfaces.
	IsVect_exprContext()
}

type Vect_exprContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyVect_exprContext() *Vect_exprContext {
	var p = new(Vect_exprContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_vect_expr
	return p
}

func InitEmptyVect_exprContext(p *Vect_exprContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_vect_expr
}

func (*Vect_exprContext) IsVect_exprContext() {}

func NewVect_exprContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Vect_exprContext {
	var p = new(Vect_exprContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_vect_expr

	return p
}

func (s *Vect_exprContext) GetParser() antlr.Parser { return s.parser }

func (s *Vect_exprContext) CopyAll(ctx *Vect_exprContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Vect_exprContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Vect_exprContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type VectorItemLisContext struct {
	Vect_exprContext
}

func NewVectorItemLisContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *VectorItemLisContext {
	var p = new(VectorItemLisContext)

	InitEmptyVect_exprContext(&p.Vect_exprContext)
	p.parser = parser
	p.CopyAll(ctx.(*Vect_exprContext))

	return p
}

func (s *VectorItemLisContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *VectorItemLisContext) LBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLBRACE, 0)
}

func (s *VectorItemLisContext) RBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarRBRACE, 0)
}

func (s *VectorItemLisContext) AllExpression() []IExpressionContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IExpressionContext); ok {
			len++
		}
	}

	tst := make([]IExpressionContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IExpressionContext); ok {
			tst[i] = t.(IExpressionContext)
			i++
		}
	}

	return tst
}

func (s *VectorItemLisContext) Expression(i int) IExpressionContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *VectorItemLisContext) AllCOMMA() []antlr.TerminalNode {
	return s.GetTokens(VLangGrammarCOMMA)
}

func (s *VectorItemLisContext) COMMA(i int) antlr.TerminalNode {
	return s.GetToken(VLangGrammarCOMMA, i)
}

func (s *VectorItemLisContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterVectorItemLis(s)
	}
}

func (s *VectorItemLisContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitVectorItemLis(s)
	}
}

func (s *VectorItemLisContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitVectorItemLis(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Vect_expr() (localctx IVect_exprContext) {
	localctx = NewVect_exprContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 8, VLangGrammarRULE_vect_expr)
	var _la int

	localctx = NewVectorItemLisContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(137)
		p.Match(VLangGrammarLBRACE)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	p.SetState(146)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	if (int64(_la) & ^0x3f) == 0 && ((int64(1)<<_la)&2216984809046016) != 0 {
		{
			p.SetState(138)
			p.expression(0)
		}
		p.SetState(143)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_la = p.GetTokenStream().LA(1)

		for _la == VLangGrammarCOMMA {
			{
				p.SetState(139)
				p.Match(VLangGrammarCOMMA)
				if p.HasError() {
					// Recognition error - abort rule
					goto errorExit
				}
			}
			{
				p.SetState(140)
				p.expression(0)
			}

			p.SetState(145)
			p.GetErrorHandler().Sync(p)
			if p.HasError() {
				goto errorExit
			}
			_la = p.GetTokenStream().LA(1)
		}

	}
	{
		p.SetState(148)
		p.Match(VLangGrammarRBRACE)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IVect_itemContext is an interface to support dynamic dispatch.
type IVect_itemContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsVect_itemContext differentiates from other interfaces.
	IsVect_itemContext()
}

type Vect_itemContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyVect_itemContext() *Vect_itemContext {
	var p = new(Vect_itemContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_vect_item
	return p
}

func InitEmptyVect_itemContext(p *Vect_itemContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_vect_item
}

func (*Vect_itemContext) IsVect_itemContext() {}

func NewVect_itemContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Vect_itemContext {
	var p = new(Vect_itemContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_vect_item

	return p
}

func (s *Vect_itemContext) GetParser() antlr.Parser { return s.parser }

func (s *Vect_itemContext) CopyAll(ctx *Vect_itemContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Vect_itemContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Vect_itemContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type VectorItemContext struct {
	Vect_itemContext
}

func NewVectorItemContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *VectorItemContext {
	var p = new(VectorItemContext)

	InitEmptyVect_itemContext(&p.Vect_itemContext)
	p.parser = parser
	p.CopyAll(ctx.(*Vect_itemContext))

	return p
}

func (s *VectorItemContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *VectorItemContext) Id_pattern() IId_patternContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IId_patternContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IId_patternContext)
}

func (s *VectorItemContext) AllLBRACK() []antlr.TerminalNode {
	return s.GetTokens(VLangGrammarLBRACK)
}

func (s *VectorItemContext) LBRACK(i int) antlr.TerminalNode {
	return s.GetToken(VLangGrammarLBRACK, i)
}

func (s *VectorItemContext) AllExpression() []IExpressionContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IExpressionContext); ok {
			len++
		}
	}

	tst := make([]IExpressionContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IExpressionContext); ok {
			tst[i] = t.(IExpressionContext)
			i++
		}
	}

	return tst
}

func (s *VectorItemContext) Expression(i int) IExpressionContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *VectorItemContext) AllRBRACK() []antlr.TerminalNode {
	return s.GetTokens(VLangGrammarRBRACK)
}

func (s *VectorItemContext) RBRACK(i int) antlr.TerminalNode {
	return s.GetToken(VLangGrammarRBRACK, i)
}

func (s *VectorItemContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterVectorItem(s)
	}
}

func (s *VectorItemContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitVectorItem(s)
	}
}

func (s *VectorItemContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitVectorItem(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Vect_item() (localctx IVect_itemContext) {
	localctx = NewVect_itemContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 10, VLangGrammarRULE_vect_item)
	var _alt int

	localctx = NewVectorItemContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(150)
		p.Id_pattern()
	}
	p.SetState(155)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_alt = 1
	for ok := true; ok; ok = _alt != 2 && _alt != antlr.ATNInvalidAltNumber {
		switch _alt {
		case 1:
			{
				p.SetState(151)
				p.Match(VLangGrammarLBRACK)
				if p.HasError() {
					// Recognition error - abort rule
					goto errorExit
				}
			}
			{
				p.SetState(152)
				p.expression(0)
			}
			{
				p.SetState(153)
				p.Match(VLangGrammarRBRACK)
				if p.HasError() {
					// Recognition error - abort rule
					goto errorExit
				}
			}

		default:
			p.SetError(antlr.NewNoViableAltException(p, nil, nil, nil, nil, nil))
			goto errorExit
		}

		p.SetState(157)
		p.GetErrorHandler().Sync(p)
		_alt = p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 6, p.GetParserRuleContext())
		if p.HasError() {
			goto errorExit
		}
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IVect_propContext is an interface to support dynamic dispatch.
type IVect_propContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsVect_propContext differentiates from other interfaces.
	IsVect_propContext()
}

type Vect_propContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyVect_propContext() *Vect_propContext {
	var p = new(Vect_propContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_vect_prop
	return p
}

func InitEmptyVect_propContext(p *Vect_propContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_vect_prop
}

func (*Vect_propContext) IsVect_propContext() {}

func NewVect_propContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Vect_propContext {
	var p = new(Vect_propContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_vect_prop

	return p
}

func (s *Vect_propContext) GetParser() antlr.Parser { return s.parser }

func (s *Vect_propContext) CopyAll(ctx *Vect_propContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Vect_propContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Vect_propContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type VectorPropertyContext struct {
	Vect_propContext
}

func NewVectorPropertyContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *VectorPropertyContext {
	var p = new(VectorPropertyContext)

	InitEmptyVect_propContext(&p.Vect_propContext)
	p.parser = parser
	p.CopyAll(ctx.(*Vect_propContext))

	return p
}

func (s *VectorPropertyContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *VectorPropertyContext) Vect_item() IVect_itemContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IVect_itemContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IVect_itemContext)
}

func (s *VectorPropertyContext) DOT() antlr.TerminalNode {
	return s.GetToken(VLangGrammarDOT, 0)
}

func (s *VectorPropertyContext) Id_pattern() IId_patternContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IId_patternContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IId_patternContext)
}

func (s *VectorPropertyContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterVectorProperty(s)
	}
}

func (s *VectorPropertyContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitVectorProperty(s)
	}
}

func (s *VectorPropertyContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitVectorProperty(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Vect_prop() (localctx IVect_propContext) {
	localctx = NewVect_propContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 12, VLangGrammarRULE_vect_prop)
	localctx = NewVectorPropertyContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(159)
		p.Vect_item()
	}
	{
		p.SetState(160)
		p.Match(VLangGrammarDOT)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(161)
		p.Id_pattern()
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IVect_funcContext is an interface to support dynamic dispatch.
type IVect_funcContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsVect_funcContext differentiates from other interfaces.
	IsVect_funcContext()
}

type Vect_funcContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyVect_funcContext() *Vect_funcContext {
	var p = new(Vect_funcContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_vect_func
	return p
}

func InitEmptyVect_funcContext(p *Vect_funcContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_vect_func
}

func (*Vect_funcContext) IsVect_funcContext() {}

func NewVect_funcContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Vect_funcContext {
	var p = new(Vect_funcContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_vect_func

	return p
}

func (s *Vect_funcContext) GetParser() antlr.Parser { return s.parser }

func (s *Vect_funcContext) CopyAll(ctx *Vect_funcContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Vect_funcContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Vect_funcContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type VectorFuncCallContext struct {
	Vect_funcContext
}

func NewVectorFuncCallContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *VectorFuncCallContext {
	var p = new(VectorFuncCallContext)

	InitEmptyVect_funcContext(&p.Vect_funcContext)
	p.parser = parser
	p.CopyAll(ctx.(*Vect_funcContext))

	return p
}

func (s *VectorFuncCallContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *VectorFuncCallContext) Vect_item() IVect_itemContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IVect_itemContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IVect_itemContext)
}

func (s *VectorFuncCallContext) DOT() antlr.TerminalNode {
	return s.GetToken(VLangGrammarDOT, 0)
}

func (s *VectorFuncCallContext) Func_call() IFunc_callContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IFunc_callContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IFunc_callContext)
}

func (s *VectorFuncCallContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterVectorFuncCall(s)
	}
}

func (s *VectorFuncCallContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitVectorFuncCall(s)
	}
}

func (s *VectorFuncCallContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitVectorFuncCall(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Vect_func() (localctx IVect_funcContext) {
	localctx = NewVect_funcContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 14, VLangGrammarRULE_vect_func)
	localctx = NewVectorFuncCallContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(163)
		p.Vect_item()
	}
	{
		p.SetState(164)
		p.Match(VLangGrammarDOT)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(165)
		p.Func_call()
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IRepeatingContext is an interface to support dynamic dispatch.
type IRepeatingContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsRepeatingContext differentiates from other interfaces.
	IsRepeatingContext()
}

type RepeatingContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyRepeatingContext() *RepeatingContext {
	var p = new(RepeatingContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_repeating
	return p
}

func InitEmptyRepeatingContext(p *RepeatingContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_repeating
}

func (*RepeatingContext) IsRepeatingContext() {}

func NewRepeatingContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *RepeatingContext {
	var p = new(RepeatingContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_repeating

	return p
}

func (s *RepeatingContext) GetParser() antlr.Parser { return s.parser }

func (s *RepeatingContext) CopyAll(ctx *RepeatingContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *RepeatingContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *RepeatingContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type RepeatingDeclContext struct {
	RepeatingContext
}

func NewRepeatingDeclContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *RepeatingDeclContext {
	var p = new(RepeatingDeclContext)

	InitEmptyRepeatingContext(&p.RepeatingContext)
	p.parser = parser
	p.CopyAll(ctx.(*RepeatingContext))

	return p
}

func (s *RepeatingDeclContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *RepeatingDeclContext) LPAREN() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLPAREN, 0)
}

func (s *RepeatingDeclContext) AllID() []antlr.TerminalNode {
	return s.GetTokens(VLangGrammarID)
}

func (s *RepeatingDeclContext) ID(i int) antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, i)
}

func (s *RepeatingDeclContext) AllCOLON() []antlr.TerminalNode {
	return s.GetTokens(VLangGrammarCOLON)
}

func (s *RepeatingDeclContext) COLON(i int) antlr.TerminalNode {
	return s.GetToken(VLangGrammarCOLON, i)
}

func (s *RepeatingDeclContext) AllExpression() []IExpressionContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IExpressionContext); ok {
			len++
		}
	}

	tst := make([]IExpressionContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IExpressionContext); ok {
			tst[i] = t.(IExpressionContext)
			i++
		}
	}

	return tst
}

func (s *RepeatingDeclContext) Expression(i int) IExpressionContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *RepeatingDeclContext) COMMA() antlr.TerminalNode {
	return s.GetToken(VLangGrammarCOMMA, 0)
}

func (s *RepeatingDeclContext) RPAREN() antlr.TerminalNode {
	return s.GetToken(VLangGrammarRPAREN, 0)
}

func (s *RepeatingDeclContext) Vector_type() IVector_typeContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IVector_typeContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IVector_typeContext)
}

func (s *RepeatingDeclContext) Matrix_type() IMatrix_typeContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IMatrix_typeContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IMatrix_typeContext)
}

func (s *RepeatingDeclContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterRepeatingDecl(s)
	}
}

func (s *RepeatingDeclContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitRepeatingDecl(s)
	}
}

func (s *RepeatingDeclContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitRepeatingDecl(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Repeating() (localctx IRepeatingContext) {
	localctx = NewRepeatingContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 16, VLangGrammarRULE_repeating)
	localctx = NewRepeatingDeclContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	p.SetState(169)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}

	switch p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 7, p.GetParserRuleContext()) {
	case 1:
		{
			p.SetState(167)
			p.Vector_type()
		}

	case 2:
		{
			p.SetState(168)
			p.Matrix_type()
		}

	case antlr.ATNInvalidAltNumber:
		goto errorExit
	}
	{
		p.SetState(171)
		p.Match(VLangGrammarLPAREN)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(172)
		p.Match(VLangGrammarID)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(173)
		p.Match(VLangGrammarCOLON)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(174)
		p.expression(0)
	}
	{
		p.SetState(175)
		p.Match(VLangGrammarCOMMA)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(176)
		p.Match(VLangGrammarID)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(177)
		p.Match(VLangGrammarCOLON)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(178)
		p.expression(0)
	}
	{
		p.SetState(179)
		p.Match(VLangGrammarRPAREN)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IVector_typeContext is an interface to support dynamic dispatch.
type IVector_typeContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser

	// Getter signatures
	LBRACK() antlr.TerminalNode
	RBRACK() antlr.TerminalNode
	ID() antlr.TerminalNode

	// IsVector_typeContext differentiates from other interfaces.
	IsVector_typeContext()
}

type Vector_typeContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyVector_typeContext() *Vector_typeContext {
	var p = new(Vector_typeContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_vector_type
	return p
}

func InitEmptyVector_typeContext(p *Vector_typeContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_vector_type
}

func (*Vector_typeContext) IsVector_typeContext() {}

func NewVector_typeContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Vector_typeContext {
	var p = new(Vector_typeContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_vector_type

	return p
}

func (s *Vector_typeContext) GetParser() antlr.Parser { return s.parser }

func (s *Vector_typeContext) LBRACK() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLBRACK, 0)
}

func (s *Vector_typeContext) RBRACK() antlr.TerminalNode {
	return s.GetToken(VLangGrammarRBRACK, 0)
}

func (s *Vector_typeContext) ID() antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, 0)
}

func (s *Vector_typeContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Vector_typeContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

func (s *Vector_typeContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterVector_type(s)
	}
}

func (s *Vector_typeContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitVector_type(s)
	}
}

func (s *Vector_typeContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitVector_type(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Vector_type() (localctx IVector_typeContext) {
	localctx = NewVector_typeContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 18, VLangGrammarRULE_vector_type)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(181)
		p.Match(VLangGrammarLBRACK)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(182)
		p.Match(VLangGrammarRBRACK)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(183)
		p.Match(VLangGrammarID)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IMatrix_typeContext is an interface to support dynamic dispatch.
type IMatrix_typeContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser

	// Getter signatures
	AllLBRACK() []antlr.TerminalNode
	LBRACK(i int) antlr.TerminalNode
	AllRBRACK() []antlr.TerminalNode
	RBRACK(i int) antlr.TerminalNode
	ID() antlr.TerminalNode

	// IsMatrix_typeContext differentiates from other interfaces.
	IsMatrix_typeContext()
}

type Matrix_typeContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyMatrix_typeContext() *Matrix_typeContext {
	var p = new(Matrix_typeContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_matrix_type
	return p
}

func InitEmptyMatrix_typeContext(p *Matrix_typeContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_matrix_type
}

func (*Matrix_typeContext) IsMatrix_typeContext() {}

func NewMatrix_typeContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Matrix_typeContext {
	var p = new(Matrix_typeContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_matrix_type

	return p
}

func (s *Matrix_typeContext) GetParser() antlr.Parser { return s.parser }

func (s *Matrix_typeContext) AllLBRACK() []antlr.TerminalNode {
	return s.GetTokens(VLangGrammarLBRACK)
}

func (s *Matrix_typeContext) LBRACK(i int) antlr.TerminalNode {
	return s.GetToken(VLangGrammarLBRACK, i)
}

func (s *Matrix_typeContext) AllRBRACK() []antlr.TerminalNode {
	return s.GetTokens(VLangGrammarRBRACK)
}

func (s *Matrix_typeContext) RBRACK(i int) antlr.TerminalNode {
	return s.GetToken(VLangGrammarRBRACK, i)
}

func (s *Matrix_typeContext) ID() antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, 0)
}

func (s *Matrix_typeContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Matrix_typeContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

func (s *Matrix_typeContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterMatrix_type(s)
	}
}

func (s *Matrix_typeContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitMatrix_type(s)
	}
}

func (s *Matrix_typeContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitMatrix_type(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Matrix_type() (localctx IMatrix_typeContext) {
	localctx = NewMatrix_typeContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 20, VLangGrammarRULE_matrix_type)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(185)
		p.Match(VLangGrammarLBRACK)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(186)
		p.Match(VLangGrammarRBRACK)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(187)
		p.Match(VLangGrammarLBRACK)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(188)
		p.Match(VLangGrammarRBRACK)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(189)
		p.Match(VLangGrammarID)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IMatrix_exprContext is an interface to support dynamic dispatch.
type IMatrix_exprContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsMatrix_exprContext differentiates from other interfaces.
	IsMatrix_exprContext()
}

type Matrix_exprContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyMatrix_exprContext() *Matrix_exprContext {
	var p = new(Matrix_exprContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_matrix_expr
	return p
}

func InitEmptyMatrix_exprContext(p *Matrix_exprContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_matrix_expr
}

func (*Matrix_exprContext) IsMatrix_exprContext() {}

func NewMatrix_exprContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Matrix_exprContext {
	var p = new(Matrix_exprContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_matrix_expr

	return p
}

func (s *Matrix_exprContext) GetParser() antlr.Parser { return s.parser }

func (s *Matrix_exprContext) CopyAll(ctx *Matrix_exprContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Matrix_exprContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Matrix_exprContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type MatrixItemListContext struct {
	Matrix_exprContext
}

func NewMatrixItemListContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *MatrixItemListContext {
	var p = new(MatrixItemListContext)

	InitEmptyMatrix_exprContext(&p.Matrix_exprContext)
	p.parser = parser
	p.CopyAll(ctx.(*Matrix_exprContext))

	return p
}

func (s *MatrixItemListContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *MatrixItemListContext) LBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLBRACE, 0)
}

func (s *MatrixItemListContext) AllVect_expr() []IVect_exprContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IVect_exprContext); ok {
			len++
		}
	}

	tst := make([]IVect_exprContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IVect_exprContext); ok {
			tst[i] = t.(IVect_exprContext)
			i++
		}
	}

	return tst
}

func (s *MatrixItemListContext) Vect_expr(i int) IVect_exprContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IVect_exprContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IVect_exprContext)
}

func (s *MatrixItemListContext) RBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarRBRACE, 0)
}

func (s *MatrixItemListContext) AllCOMMA() []antlr.TerminalNode {
	return s.GetTokens(VLangGrammarCOMMA)
}

func (s *MatrixItemListContext) COMMA(i int) antlr.TerminalNode {
	return s.GetToken(VLangGrammarCOMMA, i)
}

func (s *MatrixItemListContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterMatrixItemList(s)
	}
}

func (s *MatrixItemListContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitMatrixItemList(s)
	}
}

func (s *MatrixItemListContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitMatrixItemList(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Matrix_expr() (localctx IMatrix_exprContext) {
	localctx = NewMatrix_exprContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 22, VLangGrammarRULE_matrix_expr)
	var _la int

	localctx = NewMatrixItemListContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(191)
		p.Match(VLangGrammarLBRACE)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(192)
		p.Vect_expr()
	}
	p.SetState(197)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	for _la == VLangGrammarCOMMA {
		{
			p.SetState(193)
			p.Match(VLangGrammarCOMMA)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(194)
			p.Vect_expr()
		}

		p.SetState(199)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_la = p.GetTokenStream().LA(1)
	}
	{
		p.SetState(200)
		p.Match(VLangGrammarRBRACE)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// ITypeContext is an interface to support dynamic dispatch.
type ITypeContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser

	// Getter signatures
	ID() antlr.TerminalNode
	Vector_type() IVector_typeContext
	Matrix_type() IMatrix_typeContext

	// IsTypeContext differentiates from other interfaces.
	IsTypeContext()
}

type TypeContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyTypeContext() *TypeContext {
	var p = new(TypeContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_type
	return p
}

func InitEmptyTypeContext(p *TypeContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_type
}

func (*TypeContext) IsTypeContext() {}

func NewTypeContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *TypeContext {
	var p = new(TypeContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_type

	return p
}

func (s *TypeContext) GetParser() antlr.Parser { return s.parser }

func (s *TypeContext) ID() antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, 0)
}

func (s *TypeContext) Vector_type() IVector_typeContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IVector_typeContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IVector_typeContext)
}

func (s *TypeContext) Matrix_type() IMatrix_typeContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IMatrix_typeContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IMatrix_typeContext)
}

func (s *TypeContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *TypeContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

func (s *TypeContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterType(s)
	}
}

func (s *TypeContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitType(s)
	}
}

func (s *TypeContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitType(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Type_() (localctx ITypeContext) {
	localctx = NewTypeContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 24, VLangGrammarRULE_type)
	p.SetState(205)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}

	switch p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 9, p.GetParserRuleContext()) {
	case 1:
		p.EnterOuterAlt(localctx, 1)
		{
			p.SetState(202)
			p.Match(VLangGrammarID)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}

	case 2:
		p.EnterOuterAlt(localctx, 2)
		{
			p.SetState(203)
			p.Vector_type()
		}

	case 3:
		p.EnterOuterAlt(localctx, 3)
		{
			p.SetState(204)
			p.Matrix_type()
		}

	case antlr.ATNInvalidAltNumber:
		goto errorExit
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IAssign_stmtContext is an interface to support dynamic dispatch.
type IAssign_stmtContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsAssign_stmtContext differentiates from other interfaces.
	IsAssign_stmtContext()
}

type Assign_stmtContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyAssign_stmtContext() *Assign_stmtContext {
	var p = new(Assign_stmtContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_assign_stmt
	return p
}

func InitEmptyAssign_stmtContext(p *Assign_stmtContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_assign_stmt
}

func (*Assign_stmtContext) IsAssign_stmtContext() {}

func NewAssign_stmtContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Assign_stmtContext {
	var p = new(Assign_stmtContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_assign_stmt

	return p
}

func (s *Assign_stmtContext) GetParser() antlr.Parser { return s.parser }

func (s *Assign_stmtContext) CopyAll(ctx *Assign_stmtContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Assign_stmtContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Assign_stmtContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type ArgAddAssigDeclContext struct {
	Assign_stmtContext
	op antlr.Token
}

func NewArgAddAssigDeclContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *ArgAddAssigDeclContext {
	var p = new(ArgAddAssigDeclContext)

	InitEmptyAssign_stmtContext(&p.Assign_stmtContext)
	p.parser = parser
	p.CopyAll(ctx.(*Assign_stmtContext))

	return p
}

func (s *ArgAddAssigDeclContext) GetOp() antlr.Token { return s.op }

func (s *ArgAddAssigDeclContext) SetOp(v antlr.Token) { s.op = v }

func (s *ArgAddAssigDeclContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *ArgAddAssigDeclContext) Id_pattern() IId_patternContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IId_patternContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IId_patternContext)
}

func (s *ArgAddAssigDeclContext) Expression() IExpressionContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *ArgAddAssigDeclContext) PLUS_ASSIGN() antlr.TerminalNode {
	return s.GetToken(VLangGrammarPLUS_ASSIGN, 0)
}

func (s *ArgAddAssigDeclContext) MINUS_ASSIGN() antlr.TerminalNode {
	return s.GetToken(VLangGrammarMINUS_ASSIGN, 0)
}

func (s *ArgAddAssigDeclContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterArgAddAssigDecl(s)
	}
}

func (s *ArgAddAssigDeclContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitArgAddAssigDecl(s)
	}
}

func (s *ArgAddAssigDeclContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitArgAddAssigDecl(s)

	default:
		return t.VisitChildren(s)
	}
}

type VectorAssignContext struct {
	Assign_stmtContext
	op antlr.Token
}

func NewVectorAssignContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *VectorAssignContext {
	var p = new(VectorAssignContext)

	InitEmptyAssign_stmtContext(&p.Assign_stmtContext)
	p.parser = parser
	p.CopyAll(ctx.(*Assign_stmtContext))

	return p
}

func (s *VectorAssignContext) GetOp() antlr.Token { return s.op }

func (s *VectorAssignContext) SetOp(v antlr.Token) { s.op = v }

func (s *VectorAssignContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *VectorAssignContext) Vect_item() IVect_itemContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IVect_itemContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IVect_itemContext)
}

func (s *VectorAssignContext) Expression() IExpressionContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *VectorAssignContext) PLUS_ASSIGN() antlr.TerminalNode {
	return s.GetToken(VLangGrammarPLUS_ASSIGN, 0)
}

func (s *VectorAssignContext) MINUS_ASSIGN() antlr.TerminalNode {
	return s.GetToken(VLangGrammarMINUS_ASSIGN, 0)
}

func (s *VectorAssignContext) ASSIGN() antlr.TerminalNode {
	return s.GetToken(VLangGrammarASSIGN, 0)
}

func (s *VectorAssignContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterVectorAssign(s)
	}
}

func (s *VectorAssignContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitVectorAssign(s)
	}
}

func (s *VectorAssignContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitVectorAssign(s)

	default:
		return t.VisitChildren(s)
	}
}

type AssignmentDeclContext struct {
	Assign_stmtContext
}

func NewAssignmentDeclContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *AssignmentDeclContext {
	var p = new(AssignmentDeclContext)

	InitEmptyAssign_stmtContext(&p.Assign_stmtContext)
	p.parser = parser
	p.CopyAll(ctx.(*Assign_stmtContext))

	return p
}

func (s *AssignmentDeclContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *AssignmentDeclContext) Id_pattern() IId_patternContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IId_patternContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IId_patternContext)
}

func (s *AssignmentDeclContext) ASSIGN() antlr.TerminalNode {
	return s.GetToken(VLangGrammarASSIGN, 0)
}

func (s *AssignmentDeclContext) Expression() IExpressionContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *AssignmentDeclContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterAssignmentDecl(s)
	}
}

func (s *AssignmentDeclContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitAssignmentDecl(s)
	}
}

func (s *AssignmentDeclContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitAssignmentDecl(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Assign_stmt() (localctx IAssign_stmtContext) {
	localctx = NewAssign_stmtContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 26, VLangGrammarRULE_assign_stmt)
	var _la int

	p.SetState(219)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}

	switch p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 10, p.GetParserRuleContext()) {
	case 1:
		localctx = NewAssignmentDeclContext(p, localctx)
		p.EnterOuterAlt(localctx, 1)
		{
			p.SetState(207)
			p.Id_pattern()
		}
		{
			p.SetState(208)
			p.Match(VLangGrammarASSIGN)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(209)
			p.expression(0)
		}

	case 2:
		localctx = NewArgAddAssigDeclContext(p, localctx)
		p.EnterOuterAlt(localctx, 2)
		{
			p.SetState(211)
			p.Id_pattern()
		}
		{
			p.SetState(212)

			var _lt = p.GetTokenStream().LT(1)

			localctx.(*ArgAddAssigDeclContext).op = _lt

			_la = p.GetTokenStream().LA(1)

			if !(_la == VLangGrammarPLUS_ASSIGN || _la == VLangGrammarMINUS_ASSIGN) {
				var _ri = p.GetErrorHandler().RecoverInline(p)

				localctx.(*ArgAddAssigDeclContext).op = _ri
			} else {
				p.GetErrorHandler().ReportMatch(p)
				p.Consume()
			}
		}
		{
			p.SetState(213)
			p.expression(0)
		}

	case 3:
		localctx = NewVectorAssignContext(p, localctx)
		p.EnterOuterAlt(localctx, 3)
		{
			p.SetState(215)
			p.Vect_item()
		}
		{
			p.SetState(216)

			var _lt = p.GetTokenStream().LT(1)

			localctx.(*VectorAssignContext).op = _lt

			_la = p.GetTokenStream().LA(1)

			if !((int64(_la) & ^0x3f) == 0 && ((int64(1)<<_la)&29360128) != 0) {
				var _ri = p.GetErrorHandler().RecoverInline(p)

				localctx.(*VectorAssignContext).op = _ri
			} else {
				p.GetErrorHandler().ReportMatch(p)
				p.Consume()
			}
		}
		{
			p.SetState(217)
			p.expression(0)
		}

	case antlr.ATNInvalidAltNumber:
		goto errorExit
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IId_patternContext is an interface to support dynamic dispatch.
type IId_patternContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsId_patternContext differentiates from other interfaces.
	IsId_patternContext()
}

type Id_patternContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyId_patternContext() *Id_patternContext {
	var p = new(Id_patternContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_id_pattern
	return p
}

func InitEmptyId_patternContext(p *Id_patternContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_id_pattern
}

func (*Id_patternContext) IsId_patternContext() {}

func NewId_patternContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Id_patternContext {
	var p = new(Id_patternContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_id_pattern

	return p
}

func (s *Id_patternContext) GetParser() antlr.Parser { return s.parser }

func (s *Id_patternContext) CopyAll(ctx *Id_patternContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Id_patternContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Id_patternContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type IdPatternContext struct {
	Id_patternContext
	head antlr.Token
	_ID  antlr.Token
	tail []antlr.Token
}

func NewIdPatternContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *IdPatternContext {
	var p = new(IdPatternContext)

	InitEmptyId_patternContext(&p.Id_patternContext)
	p.parser = parser
	p.CopyAll(ctx.(*Id_patternContext))

	return p
}

func (s *IdPatternContext) GetHead() antlr.Token { return s.head }

func (s *IdPatternContext) Get_ID() antlr.Token { return s._ID }

func (s *IdPatternContext) SetHead(v antlr.Token) { s.head = v }

func (s *IdPatternContext) Set_ID(v antlr.Token) { s._ID = v }

func (s *IdPatternContext) GetTail() []antlr.Token { return s.tail }

func (s *IdPatternContext) SetTail(v []antlr.Token) { s.tail = v }

func (s *IdPatternContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *IdPatternContext) AllID() []antlr.TerminalNode {
	return s.GetTokens(VLangGrammarID)
}

func (s *IdPatternContext) ID(i int) antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, i)
}

func (s *IdPatternContext) AllDOT() []antlr.TerminalNode {
	return s.GetTokens(VLangGrammarDOT)
}

func (s *IdPatternContext) DOT(i int) antlr.TerminalNode {
	return s.GetToken(VLangGrammarDOT, i)
}

func (s *IdPatternContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterIdPattern(s)
	}
}

func (s *IdPatternContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitIdPattern(s)
	}
}

func (s *IdPatternContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitIdPattern(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Id_pattern() (localctx IId_patternContext) {
	localctx = NewId_patternContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 28, VLangGrammarRULE_id_pattern)
	var _alt int

	localctx = NewIdPatternContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(221)

		var _m = p.Match(VLangGrammarID)

		localctx.(*IdPatternContext).head = _m
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	p.SetState(226)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_alt = p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 11, p.GetParserRuleContext())
	if p.HasError() {
		goto errorExit
	}
	for _alt != 2 && _alt != antlr.ATNInvalidAltNumber {
		if _alt == 1 {
			{
				p.SetState(222)
				p.Match(VLangGrammarDOT)
				if p.HasError() {
					// Recognition error - abort rule
					goto errorExit
				}
			}
			{
				p.SetState(223)

				var _m = p.Match(VLangGrammarID)

				localctx.(*IdPatternContext)._ID = _m
				if p.HasError() {
					// Recognition error - abort rule
					goto errorExit
				}
			}
			localctx.(*IdPatternContext).tail = append(localctx.(*IdPatternContext).tail, localctx.(*IdPatternContext)._ID)

		}
		p.SetState(228)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_alt = p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 11, p.GetParserRuleContext())
		if p.HasError() {
			goto errorExit
		}
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// ILiteralContext is an interface to support dynamic dispatch.
type ILiteralContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsLiteralContext differentiates from other interfaces.
	IsLiteralContext()
}

type LiteralContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyLiteralContext() *LiteralContext {
	var p = new(LiteralContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_literal
	return p
}

func InitEmptyLiteralContext(p *LiteralContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_literal
}

func (*LiteralContext) IsLiteralContext() {}

func NewLiteralContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *LiteralContext {
	var p = new(LiteralContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_literal

	return p
}

func (s *LiteralContext) GetParser() antlr.Parser { return s.parser }

func (s *LiteralContext) CopyAll(ctx *LiteralContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *LiteralContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *LiteralContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type InterpolatedStringLiteralContext struct {
	LiteralContext
}

func NewInterpolatedStringLiteralContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *InterpolatedStringLiteralContext {
	var p = new(InterpolatedStringLiteralContext)

	InitEmptyLiteralContext(&p.LiteralContext)
	p.parser = parser
	p.CopyAll(ctx.(*LiteralContext))

	return p
}

func (s *InterpolatedStringLiteralContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *InterpolatedStringLiteralContext) Interpolated_string() IInterpolated_stringContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IInterpolated_stringContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IInterpolated_stringContext)
}

func (s *InterpolatedStringLiteralContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterInterpolatedStringLiteral(s)
	}
}

func (s *InterpolatedStringLiteralContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitInterpolatedStringLiteral(s)
	}
}

func (s *InterpolatedStringLiteralContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitInterpolatedStringLiteral(s)

	default:
		return t.VisitChildren(s)
	}
}

type StringLiteralContext struct {
	LiteralContext
}

func NewStringLiteralContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *StringLiteralContext {
	var p = new(StringLiteralContext)

	InitEmptyLiteralContext(&p.LiteralContext)
	p.parser = parser
	p.CopyAll(ctx.(*LiteralContext))

	return p
}

func (s *StringLiteralContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *StringLiteralContext) STRING_LITERAL() antlr.TerminalNode {
	return s.GetToken(VLangGrammarSTRING_LITERAL, 0)
}

func (s *StringLiteralContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterStringLiteral(s)
	}
}

func (s *StringLiteralContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitStringLiteral(s)
	}
}

func (s *StringLiteralContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitStringLiteral(s)

	default:
		return t.VisitChildren(s)
	}
}

type BoolLiteralContext struct {
	LiteralContext
}

func NewBoolLiteralContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *BoolLiteralContext {
	var p = new(BoolLiteralContext)

	InitEmptyLiteralContext(&p.LiteralContext)
	p.parser = parser
	p.CopyAll(ctx.(*LiteralContext))

	return p
}

func (s *BoolLiteralContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *BoolLiteralContext) BOOL_LITERAL() antlr.TerminalNode {
	return s.GetToken(VLangGrammarBOOL_LITERAL, 0)
}

func (s *BoolLiteralContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterBoolLiteral(s)
	}
}

func (s *BoolLiteralContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitBoolLiteral(s)
	}
}

func (s *BoolLiteralContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitBoolLiteral(s)

	default:
		return t.VisitChildren(s)
	}
}

type FloatLiteralContext struct {
	LiteralContext
}

func NewFloatLiteralContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *FloatLiteralContext {
	var p = new(FloatLiteralContext)

	InitEmptyLiteralContext(&p.LiteralContext)
	p.parser = parser
	p.CopyAll(ctx.(*LiteralContext))

	return p
}

func (s *FloatLiteralContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *FloatLiteralContext) FLOAT_LITERAL() antlr.TerminalNode {
	return s.GetToken(VLangGrammarFLOAT_LITERAL, 0)
}

func (s *FloatLiteralContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterFloatLiteral(s)
	}
}

func (s *FloatLiteralContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitFloatLiteral(s)
	}
}

func (s *FloatLiteralContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitFloatLiteral(s)

	default:
		return t.VisitChildren(s)
	}
}

type NilLiteralContext struct {
	LiteralContext
}

func NewNilLiteralContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *NilLiteralContext {
	var p = new(NilLiteralContext)

	InitEmptyLiteralContext(&p.LiteralContext)
	p.parser = parser
	p.CopyAll(ctx.(*LiteralContext))

	return p
}

func (s *NilLiteralContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *NilLiteralContext) NIL_LITERAL() antlr.TerminalNode {
	return s.GetToken(VLangGrammarNIL_LITERAL, 0)
}

func (s *NilLiteralContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterNilLiteral(s)
	}
}

func (s *NilLiteralContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitNilLiteral(s)
	}
}

func (s *NilLiteralContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitNilLiteral(s)

	default:
		return t.VisitChildren(s)
	}
}

type IntLiteralContext struct {
	LiteralContext
}

func NewIntLiteralContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *IntLiteralContext {
	var p = new(IntLiteralContext)

	InitEmptyLiteralContext(&p.LiteralContext)
	p.parser = parser
	p.CopyAll(ctx.(*LiteralContext))

	return p
}

func (s *IntLiteralContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *IntLiteralContext) INT_LITERAL() antlr.TerminalNode {
	return s.GetToken(VLangGrammarINT_LITERAL, 0)
}

func (s *IntLiteralContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterIntLiteral(s)
	}
}

func (s *IntLiteralContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitIntLiteral(s)
	}
}

func (s *IntLiteralContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitIntLiteral(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Literal() (localctx ILiteralContext) {
	localctx = NewLiteralContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 30, VLangGrammarRULE_literal)
	p.SetState(235)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}

	switch p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 12, p.GetParserRuleContext()) {
	case 1:
		localctx = NewIntLiteralContext(p, localctx)
		p.EnterOuterAlt(localctx, 1)
		{
			p.SetState(229)
			p.Match(VLangGrammarINT_LITERAL)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}

	case 2:
		localctx = NewFloatLiteralContext(p, localctx)
		p.EnterOuterAlt(localctx, 2)
		{
			p.SetState(230)
			p.Match(VLangGrammarFLOAT_LITERAL)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}

	case 3:
		localctx = NewStringLiteralContext(p, localctx)
		p.EnterOuterAlt(localctx, 3)
		{
			p.SetState(231)
			p.Match(VLangGrammarSTRING_LITERAL)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}

	case 4:
		localctx = NewInterpolatedStringLiteralContext(p, localctx)
		p.EnterOuterAlt(localctx, 4)
		{
			p.SetState(232)
			p.Interpolated_string()
		}

	case 5:
		localctx = NewBoolLiteralContext(p, localctx)
		p.EnterOuterAlt(localctx, 5)
		{
			p.SetState(233)
			p.Match(VLangGrammarBOOL_LITERAL)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}

	case 6:
		localctx = NewNilLiteralContext(p, localctx)
		p.EnterOuterAlt(localctx, 6)
		{
			p.SetState(234)
			p.Match(VLangGrammarNIL_LITERAL)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}

	case antlr.ATNInvalidAltNumber:
		goto errorExit
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IInterpolated_stringContext is an interface to support dynamic dispatch.
type IInterpolated_stringContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsInterpolated_stringContext differentiates from other interfaces.
	IsInterpolated_stringContext()
}

type Interpolated_stringContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyInterpolated_stringContext() *Interpolated_stringContext {
	var p = new(Interpolated_stringContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_interpolated_string
	return p
}

func InitEmptyInterpolated_stringContext(p *Interpolated_stringContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_interpolated_string
}

func (*Interpolated_stringContext) IsInterpolated_stringContext() {}

func NewInterpolated_stringContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Interpolated_stringContext {
	var p = new(Interpolated_stringContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_interpolated_string

	return p
}

func (s *Interpolated_stringContext) GetParser() antlr.Parser { return s.parser }

func (s *Interpolated_stringContext) CopyAll(ctx *Interpolated_stringContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Interpolated_stringContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Interpolated_stringContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type InterpolatedStringContext struct {
	Interpolated_stringContext
}

func NewInterpolatedStringContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *InterpolatedStringContext {
	var p = new(InterpolatedStringContext)

	InitEmptyInterpolated_stringContext(&p.Interpolated_stringContext)
	p.parser = parser
	p.CopyAll(ctx.(*Interpolated_stringContext))

	return p
}

func (s *InterpolatedStringContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *InterpolatedStringContext) STRING_LITERAL() antlr.TerminalNode {
	return s.GetToken(VLangGrammarSTRING_LITERAL, 0)
}

func (s *InterpolatedStringContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterInterpolatedString(s)
	}
}

func (s *InterpolatedStringContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitInterpolatedString(s)
	}
}

func (s *InterpolatedStringContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitInterpolatedString(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Interpolated_string() (localctx IInterpolated_stringContext) {
	localctx = NewInterpolated_stringContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 32, VLangGrammarRULE_interpolated_string)
	localctx = NewInterpolatedStringContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(237)
		p.Match(VLangGrammarSTRING_LITERAL)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IIncredecreContext is an interface to support dynamic dispatch.
type IIncredecreContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsIncredecreContext differentiates from other interfaces.
	IsIncredecreContext()
}

type IncredecreContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyIncredecreContext() *IncredecreContext {
	var p = new(IncredecreContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_incredecre
	return p
}

func InitEmptyIncredecreContext(p *IncredecreContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_incredecre
}

func (*IncredecreContext) IsIncredecreContext() {}

func NewIncredecreContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *IncredecreContext {
	var p = new(IncredecreContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_incredecre

	return p
}

func (s *IncredecreContext) GetParser() antlr.Parser { return s.parser }

func (s *IncredecreContext) CopyAll(ctx *IncredecreContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *IncredecreContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *IncredecreContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type IncrementoContext struct {
	IncredecreContext
}

func NewIncrementoContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *IncrementoContext {
	var p = new(IncrementoContext)

	InitEmptyIncredecreContext(&p.IncredecreContext)
	p.parser = parser
	p.CopyAll(ctx.(*IncredecreContext))

	return p
}

func (s *IncrementoContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *IncrementoContext) ID() antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, 0)
}

func (s *IncrementoContext) INC() antlr.TerminalNode {
	return s.GetToken(VLangGrammarINC, 0)
}

func (s *IncrementoContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterIncremento(s)
	}
}

func (s *IncrementoContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitIncremento(s)
	}
}

func (s *IncrementoContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitIncremento(s)

	default:
		return t.VisitChildren(s)
	}
}

type DecrementoContext struct {
	IncredecreContext
}

func NewDecrementoContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *DecrementoContext {
	var p = new(DecrementoContext)

	InitEmptyIncredecreContext(&p.IncredecreContext)
	p.parser = parser
	p.CopyAll(ctx.(*IncredecreContext))

	return p
}

func (s *DecrementoContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *DecrementoContext) ID() antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, 0)
}

func (s *DecrementoContext) DEC() antlr.TerminalNode {
	return s.GetToken(VLangGrammarDEC, 0)
}

func (s *DecrementoContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterDecremento(s)
	}
}

func (s *DecrementoContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitDecremento(s)
	}
}

func (s *DecrementoContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitDecremento(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Incredecre() (localctx IIncredecreContext) {
	localctx = NewIncredecreContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 34, VLangGrammarRULE_incredecre)
	p.SetState(243)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}

	switch p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 13, p.GetParserRuleContext()) {
	case 1:
		localctx = NewIncrementoContext(p, localctx)
		p.EnterOuterAlt(localctx, 1)
		{
			p.SetState(239)
			p.Match(VLangGrammarID)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(240)
			p.Match(VLangGrammarINC)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}

	case 2:
		localctx = NewDecrementoContext(p, localctx)
		p.EnterOuterAlt(localctx, 2)
		{
			p.SetState(241)
			p.Match(VLangGrammarID)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(242)
			p.Match(VLangGrammarDEC)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}

	case antlr.ATNInvalidAltNumber:
		goto errorExit
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IExpressionContext is an interface to support dynamic dispatch.
type IExpressionContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsExpressionContext differentiates from other interfaces.
	IsExpressionContext()
}

type ExpressionContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyExpressionContext() *ExpressionContext {
	var p = new(ExpressionContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_expression
	return p
}

func InitEmptyExpressionContext(p *ExpressionContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_expression
}

func (*ExpressionContext) IsExpressionContext() {}

func NewExpressionContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *ExpressionContext {
	var p = new(ExpressionContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_expression

	return p
}

func (s *ExpressionContext) GetParser() antlr.Parser { return s.parser }

func (s *ExpressionContext) CopyAll(ctx *ExpressionContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *ExpressionContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *ExpressionContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type RepeatingExprContext struct {
	ExpressionContext
}

func NewRepeatingExprContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *RepeatingExprContext {
	var p = new(RepeatingExprContext)

	InitEmptyExpressionContext(&p.ExpressionContext)
	p.parser = parser
	p.CopyAll(ctx.(*ExpressionContext))

	return p
}

func (s *RepeatingExprContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *RepeatingExprContext) Repeating() IRepeatingContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IRepeatingContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IRepeatingContext)
}

func (s *RepeatingExprContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterRepeatingExpr(s)
	}
}

func (s *RepeatingExprContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitRepeatingExpr(s)
	}
}

func (s *RepeatingExprContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitRepeatingExpr(s)

	default:
		return t.VisitChildren(s)
	}
}

type IncredecrContext struct {
	ExpressionContext
}

func NewIncredecrContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *IncredecrContext {
	var p = new(IncredecrContext)

	InitEmptyExpressionContext(&p.ExpressionContext)
	p.parser = parser
	p.CopyAll(ctx.(*ExpressionContext))

	return p
}

func (s *IncredecrContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *IncredecrContext) Incredecre() IIncredecreContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IIncredecreContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IIncredecreContext)
}

func (s *IncredecrContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterIncredecr(s)
	}
}

func (s *IncredecrContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitIncredecr(s)
	}
}

func (s *IncredecrContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitIncredecr(s)

	default:
		return t.VisitChildren(s)
	}
}

type BinaryExprContext struct {
	ExpressionContext
	left  IExpressionContext
	op    antlr.Token
	right IExpressionContext
}

func NewBinaryExprContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *BinaryExprContext {
	var p = new(BinaryExprContext)

	InitEmptyExpressionContext(&p.ExpressionContext)
	p.parser = parser
	p.CopyAll(ctx.(*ExpressionContext))

	return p
}

func (s *BinaryExprContext) GetOp() antlr.Token { return s.op }

func (s *BinaryExprContext) SetOp(v antlr.Token) { s.op = v }

func (s *BinaryExprContext) GetLeft() IExpressionContext { return s.left }

func (s *BinaryExprContext) GetRight() IExpressionContext { return s.right }

func (s *BinaryExprContext) SetLeft(v IExpressionContext) { s.left = v }

func (s *BinaryExprContext) SetRight(v IExpressionContext) { s.right = v }

func (s *BinaryExprContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *BinaryExprContext) AllExpression() []IExpressionContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IExpressionContext); ok {
			len++
		}
	}

	tst := make([]IExpressionContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IExpressionContext); ok {
			tst[i] = t.(IExpressionContext)
			i++
		}
	}

	return tst
}

func (s *BinaryExprContext) Expression(i int) IExpressionContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *BinaryExprContext) MULT() antlr.TerminalNode {
	return s.GetToken(VLangGrammarMULT, 0)
}

func (s *BinaryExprContext) DIV() antlr.TerminalNode {
	return s.GetToken(VLangGrammarDIV, 0)
}

func (s *BinaryExprContext) MOD() antlr.TerminalNode {
	return s.GetToken(VLangGrammarMOD, 0)
}

func (s *BinaryExprContext) PLUS() antlr.TerminalNode {
	return s.GetToken(VLangGrammarPLUS, 0)
}

func (s *BinaryExprContext) MINUS() antlr.TerminalNode {
	return s.GetToken(VLangGrammarMINUS, 0)
}

func (s *BinaryExprContext) LE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLE, 0)
}

func (s *BinaryExprContext) LT() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLT, 0)
}

func (s *BinaryExprContext) GE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarGE, 0)
}

func (s *BinaryExprContext) GT() antlr.TerminalNode {
	return s.GetToken(VLangGrammarGT, 0)
}

func (s *BinaryExprContext) EQ() antlr.TerminalNode {
	return s.GetToken(VLangGrammarEQ, 0)
}

func (s *BinaryExprContext) NE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarNE, 0)
}

func (s *BinaryExprContext) AND() antlr.TerminalNode {
	return s.GetToken(VLangGrammarAND, 0)
}

func (s *BinaryExprContext) OR() antlr.TerminalNode {
	return s.GetToken(VLangGrammarOR, 0)
}

func (s *BinaryExprContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterBinaryExpr(s)
	}
}

func (s *BinaryExprContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitBinaryExpr(s)
	}
}

func (s *BinaryExprContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitBinaryExpr(s)

	default:
		return t.VisitChildren(s)
	}
}

type StructInstantiationExprContext struct {
	ExpressionContext
}

func NewStructInstantiationExprContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *StructInstantiationExprContext {
	var p = new(StructInstantiationExprContext)

	InitEmptyExpressionContext(&p.ExpressionContext)
	p.parser = parser
	p.CopyAll(ctx.(*ExpressionContext))

	return p
}

func (s *StructInstantiationExprContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *StructInstantiationExprContext) ID() antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, 0)
}

func (s *StructInstantiationExprContext) LBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLBRACE, 0)
}

func (s *StructInstantiationExprContext) RBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarRBRACE, 0)
}

func (s *StructInstantiationExprContext) Struct_param_list() IStruct_param_listContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IStruct_param_listContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IStruct_param_listContext)
}

func (s *StructInstantiationExprContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterStructInstantiationExpr(s)
	}
}

func (s *StructInstantiationExprContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitStructInstantiationExpr(s)
	}
}

func (s *StructInstantiationExprContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitStructInstantiationExpr(s)

	default:
		return t.VisitChildren(s)
	}
}

type UnaryExprContext struct {
	ExpressionContext
	op antlr.Token
}

func NewUnaryExprContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *UnaryExprContext {
	var p = new(UnaryExprContext)

	InitEmptyExpressionContext(&p.ExpressionContext)
	p.parser = parser
	p.CopyAll(ctx.(*ExpressionContext))

	return p
}

func (s *UnaryExprContext) GetOp() antlr.Token { return s.op }

func (s *UnaryExprContext) SetOp(v antlr.Token) { s.op = v }

func (s *UnaryExprContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *UnaryExprContext) Expression() IExpressionContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *UnaryExprContext) NOT() antlr.TerminalNode {
	return s.GetToken(VLangGrammarNOT, 0)
}

func (s *UnaryExprContext) MINUS() antlr.TerminalNode {
	return s.GetToken(VLangGrammarMINUS, 0)
}

func (s *UnaryExprContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterUnaryExpr(s)
	}
}

func (s *UnaryExprContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitUnaryExpr(s)
	}
}

func (s *UnaryExprContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitUnaryExpr(s)

	default:
		return t.VisitChildren(s)
	}
}

type IdPatternExprContext struct {
	ExpressionContext
}

func NewIdPatternExprContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *IdPatternExprContext {
	var p = new(IdPatternExprContext)

	InitEmptyExpressionContext(&p.ExpressionContext)
	p.parser = parser
	p.CopyAll(ctx.(*ExpressionContext))

	return p
}

func (s *IdPatternExprContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *IdPatternExprContext) Id_pattern() IId_patternContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IId_patternContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IId_patternContext)
}

func (s *IdPatternExprContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterIdPatternExpr(s)
	}
}

func (s *IdPatternExprContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitIdPatternExpr(s)
	}
}

func (s *IdPatternExprContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitIdPatternExpr(s)

	default:
		return t.VisitChildren(s)
	}
}

type VectorPropertyExprContext struct {
	ExpressionContext
}

func NewVectorPropertyExprContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *VectorPropertyExprContext {
	var p = new(VectorPropertyExprContext)

	InitEmptyExpressionContext(&p.ExpressionContext)
	p.parser = parser
	p.CopyAll(ctx.(*ExpressionContext))

	return p
}

func (s *VectorPropertyExprContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *VectorPropertyExprContext) Vect_prop() IVect_propContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IVect_propContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IVect_propContext)
}

func (s *VectorPropertyExprContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterVectorPropertyExpr(s)
	}
}

func (s *VectorPropertyExprContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitVectorPropertyExpr(s)
	}
}

func (s *VectorPropertyExprContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitVectorPropertyExpr(s)

	default:
		return t.VisitChildren(s)
	}
}

type VectorItemExprContext struct {
	ExpressionContext
}

func NewVectorItemExprContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *VectorItemExprContext {
	var p = new(VectorItemExprContext)

	InitEmptyExpressionContext(&p.ExpressionContext)
	p.parser = parser
	p.CopyAll(ctx.(*ExpressionContext))

	return p
}

func (s *VectorItemExprContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *VectorItemExprContext) Vect_item() IVect_itemContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IVect_itemContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IVect_itemContext)
}

func (s *VectorItemExprContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterVectorItemExpr(s)
	}
}

func (s *VectorItemExprContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitVectorItemExpr(s)
	}
}

func (s *VectorItemExprContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitVectorItemExpr(s)

	default:
		return t.VisitChildren(s)
	}
}

type ParensExprContext struct {
	ExpressionContext
}

func NewParensExprContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *ParensExprContext {
	var p = new(ParensExprContext)

	InitEmptyExpressionContext(&p.ExpressionContext)
	p.parser = parser
	p.CopyAll(ctx.(*ExpressionContext))

	return p
}

func (s *ParensExprContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *ParensExprContext) LPAREN() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLPAREN, 0)
}

func (s *ParensExprContext) Expression() IExpressionContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *ParensExprContext) RPAREN() antlr.TerminalNode {
	return s.GetToken(VLangGrammarRPAREN, 0)
}

func (s *ParensExprContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterParensExpr(s)
	}
}

func (s *ParensExprContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitParensExpr(s)
	}
}

func (s *ParensExprContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitParensExpr(s)

	default:
		return t.VisitChildren(s)
	}
}

type LiteralExprContext struct {
	ExpressionContext
}

func NewLiteralExprContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *LiteralExprContext {
	var p = new(LiteralExprContext)

	InitEmptyExpressionContext(&p.ExpressionContext)
	p.parser = parser
	p.CopyAll(ctx.(*ExpressionContext))

	return p
}

func (s *LiteralExprContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *LiteralExprContext) Literal() ILiteralContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(ILiteralContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(ILiteralContext)
}

func (s *LiteralExprContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterLiteralExpr(s)
	}
}

func (s *LiteralExprContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitLiteralExpr(s)
	}
}

func (s *LiteralExprContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitLiteralExpr(s)

	default:
		return t.VisitChildren(s)
	}
}

type VectorFuncCallExprContext struct {
	ExpressionContext
}

func NewVectorFuncCallExprContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *VectorFuncCallExprContext {
	var p = new(VectorFuncCallExprContext)

	InitEmptyExpressionContext(&p.ExpressionContext)
	p.parser = parser
	p.CopyAll(ctx.(*ExpressionContext))

	return p
}

func (s *VectorFuncCallExprContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *VectorFuncCallExprContext) Vect_func() IVect_funcContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IVect_funcContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IVect_funcContext)
}

func (s *VectorFuncCallExprContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterVectorFuncCallExpr(s)
	}
}

func (s *VectorFuncCallExprContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitVectorFuncCallExpr(s)
	}
}

func (s *VectorFuncCallExprContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitVectorFuncCallExpr(s)

	default:
		return t.VisitChildren(s)
	}
}

type VectorExprContext struct {
	ExpressionContext
}

func NewVectorExprContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *VectorExprContext {
	var p = new(VectorExprContext)

	InitEmptyExpressionContext(&p.ExpressionContext)
	p.parser = parser
	p.CopyAll(ctx.(*ExpressionContext))

	return p
}

func (s *VectorExprContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *VectorExprContext) Vect_expr() IVect_exprContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IVect_exprContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IVect_exprContext)
}

func (s *VectorExprContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterVectorExpr(s)
	}
}

func (s *VectorExprContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitVectorExpr(s)
	}
}

func (s *VectorExprContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitVectorExpr(s)

	default:
		return t.VisitChildren(s)
	}
}

type FuncCallExprContext struct {
	ExpressionContext
}

func NewFuncCallExprContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *FuncCallExprContext {
	var p = new(FuncCallExprContext)

	InitEmptyExpressionContext(&p.ExpressionContext)
	p.parser = parser
	p.CopyAll(ctx.(*ExpressionContext))

	return p
}

func (s *FuncCallExprContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *FuncCallExprContext) Func_call() IFunc_callContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IFunc_callContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IFunc_callContext)
}

func (s *FuncCallExprContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterFuncCallExpr(s)
	}
}

func (s *FuncCallExprContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitFuncCallExpr(s)
	}
}

func (s *FuncCallExprContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitFuncCallExpr(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Expression() (localctx IExpressionContext) {
	return p.expression(0)
}

func (p *VLangGrammar) expression(_p int) (localctx IExpressionContext) {
	var _parentctx antlr.ParserRuleContext = p.GetParserRuleContext()

	_parentState := p.GetState()
	localctx = NewExpressionContext(p, p.GetParserRuleContext(), _parentState)
	var _prevctx IExpressionContext = localctx
	var _ antlr.ParserRuleContext = _prevctx // TODO: To prevent unused variable warning.
	_startState := 36
	p.EnterRecursionRule(localctx, 36, VLangGrammarRULE_expression, _p)
	var _la int

	var _alt int

	p.EnterOuterAlt(localctx, 1)
	p.SetState(267)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}

	switch p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 15, p.GetParserRuleContext()) {
	case 1:
		localctx = NewParensExprContext(p, localctx)
		p.SetParserRuleContext(localctx)
		_prevctx = localctx

		{
			p.SetState(246)
			p.Match(VLangGrammarLPAREN)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(247)
			p.expression(0)
		}
		{
			p.SetState(248)
			p.Match(VLangGrammarRPAREN)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}

	case 2:
		localctx = NewFuncCallExprContext(p, localctx)
		p.SetParserRuleContext(localctx)
		_prevctx = localctx
		{
			p.SetState(250)
			p.Func_call()
		}

	case 3:
		localctx = NewIdPatternExprContext(p, localctx)
		p.SetParserRuleContext(localctx)
		_prevctx = localctx
		{
			p.SetState(251)
			p.Id_pattern()
		}

	case 4:
		localctx = NewVectorItemExprContext(p, localctx)
		p.SetParserRuleContext(localctx)
		_prevctx = localctx
		{
			p.SetState(252)
			p.Vect_item()
		}

	case 5:
		localctx = NewVectorPropertyExprContext(p, localctx)
		p.SetParserRuleContext(localctx)
		_prevctx = localctx
		{
			p.SetState(253)
			p.Vect_prop()
		}

	case 6:
		localctx = NewVectorFuncCallExprContext(p, localctx)
		p.SetParserRuleContext(localctx)
		_prevctx = localctx
		{
			p.SetState(254)
			p.Vect_func()
		}

	case 7:
		localctx = NewLiteralExprContext(p, localctx)
		p.SetParserRuleContext(localctx)
		_prevctx = localctx
		{
			p.SetState(255)
			p.Literal()
		}

	case 8:
		localctx = NewVectorExprContext(p, localctx)
		p.SetParserRuleContext(localctx)
		_prevctx = localctx
		{
			p.SetState(256)
			p.Vect_expr()
		}

	case 9:
		localctx = NewRepeatingExprContext(p, localctx)
		p.SetParserRuleContext(localctx)
		_prevctx = localctx
		{
			p.SetState(257)
			p.Repeating()
		}

	case 10:
		localctx = NewIncredecrContext(p, localctx)
		p.SetParserRuleContext(localctx)
		_prevctx = localctx
		{
			p.SetState(258)
			p.Incredecre()
		}

	case 11:
		localctx = NewUnaryExprContext(p, localctx)
		p.SetParserRuleContext(localctx)
		_prevctx = localctx
		{
			p.SetState(259)

			var _lt = p.GetTokenStream().LT(1)

			localctx.(*UnaryExprContext).op = _lt

			_la = p.GetTokenStream().LA(1)

			if !(_la == VLangGrammarMINUS || _la == VLangGrammarNOT) {
				var _ri = p.GetErrorHandler().RecoverInline(p)

				localctx.(*UnaryExprContext).op = _ri
			} else {
				p.GetErrorHandler().ReportMatch(p)
				p.Consume()
			}
		}
		{
			p.SetState(260)
			p.expression(8)
		}

	case 12:
		localctx = NewStructInstantiationExprContext(p, localctx)
		p.SetParserRuleContext(localctx)
		_prevctx = localctx
		{
			p.SetState(261)
			p.Match(VLangGrammarID)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(262)
			p.Match(VLangGrammarLBRACE)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		p.SetState(264)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_la = p.GetTokenStream().LA(1)

		if _la == VLangGrammarID {
			{
				p.SetState(263)
				p.Struct_param_list()
			}

		}
		{
			p.SetState(266)
			p.Match(VLangGrammarRBRACE)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}

	case antlr.ATNInvalidAltNumber:
		goto errorExit
	}
	p.GetParserRuleContext().SetStop(p.GetTokenStream().LT(-1))
	p.SetState(289)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_alt = p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 17, p.GetParserRuleContext())
	if p.HasError() {
		goto errorExit
	}
	for _alt != 2 && _alt != antlr.ATNInvalidAltNumber {
		if _alt == 1 {
			if p.GetParseListeners() != nil {
				p.TriggerExitRuleEvent()
			}
			_prevctx = localctx
			p.SetState(287)
			p.GetErrorHandler().Sync(p)
			if p.HasError() {
				goto errorExit
			}

			switch p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 16, p.GetParserRuleContext()) {
			case 1:
				localctx = NewBinaryExprContext(p, NewExpressionContext(p, _parentctx, _parentState))
				localctx.(*BinaryExprContext).left = _prevctx

				p.PushNewRecursionContext(localctx, _startState, VLangGrammarRULE_expression)
				p.SetState(269)

				if !(p.Precpred(p.GetParserRuleContext(), 7)) {
					p.SetError(antlr.NewFailedPredicateException(p, "p.Precpred(p.GetParserRuleContext(), 7)", ""))
					goto errorExit
				}
				{
					p.SetState(270)

					var _lt = p.GetTokenStream().LT(1)

					localctx.(*BinaryExprContext).op = _lt

					_la = p.GetTokenStream().LA(1)

					if !((int64(_la) & ^0x3f) == 0 && ((int64(1)<<_la)&3670016) != 0) {
						var _ri = p.GetErrorHandler().RecoverInline(p)

						localctx.(*BinaryExprContext).op = _ri
					} else {
						p.GetErrorHandler().ReportMatch(p)
						p.Consume()
					}
				}
				{
					p.SetState(271)

					var _x = p.expression(8)

					localctx.(*BinaryExprContext).right = _x
				}

			case 2:
				localctx = NewBinaryExprContext(p, NewExpressionContext(p, _parentctx, _parentState))
				localctx.(*BinaryExprContext).left = _prevctx

				p.PushNewRecursionContext(localctx, _startState, VLangGrammarRULE_expression)
				p.SetState(272)

				if !(p.Precpred(p.GetParserRuleContext(), 6)) {
					p.SetError(antlr.NewFailedPredicateException(p, "p.Precpred(p.GetParserRuleContext(), 6)", ""))
					goto errorExit
				}
				{
					p.SetState(273)

					var _lt = p.GetTokenStream().LT(1)

					localctx.(*BinaryExprContext).op = _lt

					_la = p.GetTokenStream().LA(1)

					if !(_la == VLangGrammarPLUS || _la == VLangGrammarMINUS) {
						var _ri = p.GetErrorHandler().RecoverInline(p)

						localctx.(*BinaryExprContext).op = _ri
					} else {
						p.GetErrorHandler().ReportMatch(p)
						p.Consume()
					}
				}
				{
					p.SetState(274)

					var _x = p.expression(7)

					localctx.(*BinaryExprContext).right = _x
				}

			case 3:
				localctx = NewBinaryExprContext(p, NewExpressionContext(p, _parentctx, _parentState))
				localctx.(*BinaryExprContext).left = _prevctx

				p.PushNewRecursionContext(localctx, _startState, VLangGrammarRULE_expression)
				p.SetState(275)

				if !(p.Precpred(p.GetParserRuleContext(), 5)) {
					p.SetError(antlr.NewFailedPredicateException(p, "p.Precpred(p.GetParserRuleContext(), 5)", ""))
					goto errorExit
				}
				{
					p.SetState(276)

					var _lt = p.GetTokenStream().LT(1)

					localctx.(*BinaryExprContext).op = _lt

					_la = p.GetTokenStream().LA(1)

					if !((int64(_la) & ^0x3f) == 0 && ((int64(1)<<_la)&2013265920) != 0) {
						var _ri = p.GetErrorHandler().RecoverInline(p)

						localctx.(*BinaryExprContext).op = _ri
					} else {
						p.GetErrorHandler().ReportMatch(p)
						p.Consume()
					}
				}
				{
					p.SetState(277)

					var _x = p.expression(6)

					localctx.(*BinaryExprContext).right = _x
				}

			case 4:
				localctx = NewBinaryExprContext(p, NewExpressionContext(p, _parentctx, _parentState))
				localctx.(*BinaryExprContext).left = _prevctx

				p.PushNewRecursionContext(localctx, _startState, VLangGrammarRULE_expression)
				p.SetState(278)

				if !(p.Precpred(p.GetParserRuleContext(), 4)) {
					p.SetError(antlr.NewFailedPredicateException(p, "p.Precpred(p.GetParserRuleContext(), 4)", ""))
					goto errorExit
				}
				{
					p.SetState(279)

					var _lt = p.GetTokenStream().LT(1)

					localctx.(*BinaryExprContext).op = _lt

					_la = p.GetTokenStream().LA(1)

					if !(_la == VLangGrammarEQ || _la == VLangGrammarNE) {
						var _ri = p.GetErrorHandler().RecoverInline(p)

						localctx.(*BinaryExprContext).op = _ri
					} else {
						p.GetErrorHandler().ReportMatch(p)
						p.Consume()
					}
				}
				{
					p.SetState(280)

					var _x = p.expression(5)

					localctx.(*BinaryExprContext).right = _x
				}

			case 5:
				localctx = NewBinaryExprContext(p, NewExpressionContext(p, _parentctx, _parentState))
				localctx.(*BinaryExprContext).left = _prevctx

				p.PushNewRecursionContext(localctx, _startState, VLangGrammarRULE_expression)
				p.SetState(281)

				if !(p.Precpred(p.GetParserRuleContext(), 3)) {
					p.SetError(antlr.NewFailedPredicateException(p, "p.Precpred(p.GetParserRuleContext(), 3)", ""))
					goto errorExit
				}
				{
					p.SetState(282)

					var _m = p.Match(VLangGrammarAND)

					localctx.(*BinaryExprContext).op = _m
					if p.HasError() {
						// Recognition error - abort rule
						goto errorExit
					}
				}
				{
					p.SetState(283)

					var _x = p.expression(4)

					localctx.(*BinaryExprContext).right = _x
				}

			case 6:
				localctx = NewBinaryExprContext(p, NewExpressionContext(p, _parentctx, _parentState))
				localctx.(*BinaryExprContext).left = _prevctx

				p.PushNewRecursionContext(localctx, _startState, VLangGrammarRULE_expression)
				p.SetState(284)

				if !(p.Precpred(p.GetParserRuleContext(), 2)) {
					p.SetError(antlr.NewFailedPredicateException(p, "p.Precpred(p.GetParserRuleContext(), 2)", ""))
					goto errorExit
				}
				{
					p.SetState(285)

					var _m = p.Match(VLangGrammarOR)

					localctx.(*BinaryExprContext).op = _m
					if p.HasError() {
						// Recognition error - abort rule
						goto errorExit
					}
				}
				{
					p.SetState(286)

					var _x = p.expression(3)

					localctx.(*BinaryExprContext).right = _x
				}

			case antlr.ATNInvalidAltNumber:
				goto errorExit
			}

		}
		p.SetState(291)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_alt = p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 17, p.GetParserRuleContext())
		if p.HasError() {
			goto errorExit
		}
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.UnrollRecursionContexts(_parentctx)
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IIf_stmtContext is an interface to support dynamic dispatch.
type IIf_stmtContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsIf_stmtContext differentiates from other interfaces.
	IsIf_stmtContext()
}

type If_stmtContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyIf_stmtContext() *If_stmtContext {
	var p = new(If_stmtContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_if_stmt
	return p
}

func InitEmptyIf_stmtContext(p *If_stmtContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_if_stmt
}

func (*If_stmtContext) IsIf_stmtContext() {}

func NewIf_stmtContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *If_stmtContext {
	var p = new(If_stmtContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_if_stmt

	return p
}

func (s *If_stmtContext) GetParser() antlr.Parser { return s.parser }

func (s *If_stmtContext) CopyAll(ctx *If_stmtContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *If_stmtContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *If_stmtContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type IfStmtContext struct {
	If_stmtContext
}

func NewIfStmtContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *IfStmtContext {
	var p = new(IfStmtContext)

	InitEmptyIf_stmtContext(&p.If_stmtContext)
	p.parser = parser
	p.CopyAll(ctx.(*If_stmtContext))

	return p
}

func (s *IfStmtContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *IfStmtContext) AllIf_chain() []IIf_chainContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IIf_chainContext); ok {
			len++
		}
	}

	tst := make([]IIf_chainContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IIf_chainContext); ok {
			tst[i] = t.(IIf_chainContext)
			i++
		}
	}

	return tst
}

func (s *IfStmtContext) If_chain(i int) IIf_chainContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IIf_chainContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IIf_chainContext)
}

func (s *IfStmtContext) AllELSE_KW() []antlr.TerminalNode {
	return s.GetTokens(VLangGrammarELSE_KW)
}

func (s *IfStmtContext) ELSE_KW(i int) antlr.TerminalNode {
	return s.GetToken(VLangGrammarELSE_KW, i)
}

func (s *IfStmtContext) Else_stmt() IElse_stmtContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IElse_stmtContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IElse_stmtContext)
}

func (s *IfStmtContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterIfStmt(s)
	}
}

func (s *IfStmtContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitIfStmt(s)
	}
}

func (s *IfStmtContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitIfStmt(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) If_stmt() (localctx IIf_stmtContext) {
	localctx = NewIf_stmtContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 38, VLangGrammarRULE_if_stmt)
	var _la int

	var _alt int

	localctx = NewIfStmtContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(292)
		p.If_chain()
	}
	p.SetState(297)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_alt = p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 18, p.GetParserRuleContext())
	if p.HasError() {
		goto errorExit
	}
	for _alt != 2 && _alt != antlr.ATNInvalidAltNumber {
		if _alt == 1 {
			{
				p.SetState(293)
				p.Match(VLangGrammarELSE_KW)
				if p.HasError() {
					// Recognition error - abort rule
					goto errorExit
				}
			}
			{
				p.SetState(294)
				p.If_chain()
			}

		}
		p.SetState(299)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_alt = p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 18, p.GetParserRuleContext())
		if p.HasError() {
			goto errorExit
		}
	}
	p.SetState(301)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	if _la == VLangGrammarELSE_KW {
		{
			p.SetState(300)
			p.Else_stmt()
		}

	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IIf_chainContext is an interface to support dynamic dispatch.
type IIf_chainContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsIf_chainContext differentiates from other interfaces.
	IsIf_chainContext()
}

type If_chainContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyIf_chainContext() *If_chainContext {
	var p = new(If_chainContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_if_chain
	return p
}

func InitEmptyIf_chainContext(p *If_chainContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_if_chain
}

func (*If_chainContext) IsIf_chainContext() {}

func NewIf_chainContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *If_chainContext {
	var p = new(If_chainContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_if_chain

	return p
}

func (s *If_chainContext) GetParser() antlr.Parser { return s.parser }

func (s *If_chainContext) CopyAll(ctx *If_chainContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *If_chainContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *If_chainContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type IfChainContext struct {
	If_chainContext
}

func NewIfChainContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *IfChainContext {
	var p = new(IfChainContext)

	InitEmptyIf_chainContext(&p.If_chainContext)
	p.parser = parser
	p.CopyAll(ctx.(*If_chainContext))

	return p
}

func (s *IfChainContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *IfChainContext) IF_KW() antlr.TerminalNode {
	return s.GetToken(VLangGrammarIF_KW, 0)
}

func (s *IfChainContext) Expression() IExpressionContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *IfChainContext) LBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLBRACE, 0)
}

func (s *IfChainContext) RBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarRBRACE, 0)
}

func (s *IfChainContext) AllStmt() []IStmtContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IStmtContext); ok {
			len++
		}
	}

	tst := make([]IStmtContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IStmtContext); ok {
			tst[i] = t.(IStmtContext)
			i++
		}
	}

	return tst
}

func (s *IfChainContext) Stmt(i int) IStmtContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IStmtContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IStmtContext)
}

func (s *IfChainContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterIfChain(s)
	}
}

func (s *IfChainContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitIfChain(s)
	}
}

func (s *IfChainContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitIfChain(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) If_chain() (localctx IIf_chainContext) {
	localctx = NewIf_chainContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 40, VLangGrammarRULE_if_chain)
	var _la int

	localctx = NewIfChainContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(303)
		p.Match(VLangGrammarIF_KW)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(304)
		p.expression(0)
	}
	{
		p.SetState(305)
		p.Match(VLangGrammarLBRACE)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	p.SetState(309)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	for (int64(_la) & ^0x3f) == 0 && ((int64(1)<<_la)&1125968626349662) != 0 {
		{
			p.SetState(306)
			p.Stmt()
		}

		p.SetState(311)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_la = p.GetTokenStream().LA(1)
	}
	{
		p.SetState(312)
		p.Match(VLangGrammarRBRACE)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IElse_stmtContext is an interface to support dynamic dispatch.
type IElse_stmtContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsElse_stmtContext differentiates from other interfaces.
	IsElse_stmtContext()
}

type Else_stmtContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyElse_stmtContext() *Else_stmtContext {
	var p = new(Else_stmtContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_else_stmt
	return p
}

func InitEmptyElse_stmtContext(p *Else_stmtContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_else_stmt
}

func (*Else_stmtContext) IsElse_stmtContext() {}

func NewElse_stmtContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Else_stmtContext {
	var p = new(Else_stmtContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_else_stmt

	return p
}

func (s *Else_stmtContext) GetParser() antlr.Parser { return s.parser }

func (s *Else_stmtContext) CopyAll(ctx *Else_stmtContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Else_stmtContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Else_stmtContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type ElseStmtContext struct {
	Else_stmtContext
}

func NewElseStmtContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *ElseStmtContext {
	var p = new(ElseStmtContext)

	InitEmptyElse_stmtContext(&p.Else_stmtContext)
	p.parser = parser
	p.CopyAll(ctx.(*Else_stmtContext))

	return p
}

func (s *ElseStmtContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *ElseStmtContext) ELSE_KW() antlr.TerminalNode {
	return s.GetToken(VLangGrammarELSE_KW, 0)
}

func (s *ElseStmtContext) LBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLBRACE, 0)
}

func (s *ElseStmtContext) RBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarRBRACE, 0)
}

func (s *ElseStmtContext) AllStmt() []IStmtContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IStmtContext); ok {
			len++
		}
	}

	tst := make([]IStmtContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IStmtContext); ok {
			tst[i] = t.(IStmtContext)
			i++
		}
	}

	return tst
}

func (s *ElseStmtContext) Stmt(i int) IStmtContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IStmtContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IStmtContext)
}

func (s *ElseStmtContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterElseStmt(s)
	}
}

func (s *ElseStmtContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitElseStmt(s)
	}
}

func (s *ElseStmtContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitElseStmt(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Else_stmt() (localctx IElse_stmtContext) {
	localctx = NewElse_stmtContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 42, VLangGrammarRULE_else_stmt)
	var _la int

	localctx = NewElseStmtContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(314)
		p.Match(VLangGrammarELSE_KW)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(315)
		p.Match(VLangGrammarLBRACE)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	p.SetState(319)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	for (int64(_la) & ^0x3f) == 0 && ((int64(1)<<_la)&1125968626349662) != 0 {
		{
			p.SetState(316)
			p.Stmt()
		}

		p.SetState(321)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_la = p.GetTokenStream().LA(1)
	}
	{
		p.SetState(322)
		p.Match(VLangGrammarRBRACE)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// ISwitch_stmtContext is an interface to support dynamic dispatch.
type ISwitch_stmtContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsSwitch_stmtContext differentiates from other interfaces.
	IsSwitch_stmtContext()
}

type Switch_stmtContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptySwitch_stmtContext() *Switch_stmtContext {
	var p = new(Switch_stmtContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_switch_stmt
	return p
}

func InitEmptySwitch_stmtContext(p *Switch_stmtContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_switch_stmt
}

func (*Switch_stmtContext) IsSwitch_stmtContext() {}

func NewSwitch_stmtContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Switch_stmtContext {
	var p = new(Switch_stmtContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_switch_stmt

	return p
}

func (s *Switch_stmtContext) GetParser() antlr.Parser { return s.parser }

func (s *Switch_stmtContext) CopyAll(ctx *Switch_stmtContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Switch_stmtContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Switch_stmtContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type SwitchStmtContext struct {
	Switch_stmtContext
}

func NewSwitchStmtContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *SwitchStmtContext {
	var p = new(SwitchStmtContext)

	InitEmptySwitch_stmtContext(&p.Switch_stmtContext)
	p.parser = parser
	p.CopyAll(ctx.(*Switch_stmtContext))

	return p
}

func (s *SwitchStmtContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *SwitchStmtContext) SWITCH_KW() antlr.TerminalNode {
	return s.GetToken(VLangGrammarSWITCH_KW, 0)
}

func (s *SwitchStmtContext) Expression() IExpressionContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *SwitchStmtContext) LBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLBRACE, 0)
}

func (s *SwitchStmtContext) RBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarRBRACE, 0)
}

func (s *SwitchStmtContext) AllSwitch_case() []ISwitch_caseContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(ISwitch_caseContext); ok {
			len++
		}
	}

	tst := make([]ISwitch_caseContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(ISwitch_caseContext); ok {
			tst[i] = t.(ISwitch_caseContext)
			i++
		}
	}

	return tst
}

func (s *SwitchStmtContext) Switch_case(i int) ISwitch_caseContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(ISwitch_caseContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(ISwitch_caseContext)
}

func (s *SwitchStmtContext) Default_case() IDefault_caseContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IDefault_caseContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IDefault_caseContext)
}

func (s *SwitchStmtContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterSwitchStmt(s)
	}
}

func (s *SwitchStmtContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitSwitchStmt(s)
	}
}

func (s *SwitchStmtContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitSwitchStmt(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Switch_stmt() (localctx ISwitch_stmtContext) {
	localctx = NewSwitch_stmtContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 44, VLangGrammarRULE_switch_stmt)
	var _la int

	localctx = NewSwitchStmtContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(324)
		p.Match(VLangGrammarSWITCH_KW)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(325)
		p.expression(0)
	}
	{
		p.SetState(326)
		p.Match(VLangGrammarLBRACE)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	p.SetState(330)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	for _la == VLangGrammarCASE_KW {
		{
			p.SetState(327)
			p.Switch_case()
		}

		p.SetState(332)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_la = p.GetTokenStream().LA(1)
	}
	p.SetState(334)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	if _la == VLangGrammarDEFAULT_KW {
		{
			p.SetState(333)
			p.Default_case()
		}

	}
	{
		p.SetState(336)
		p.Match(VLangGrammarRBRACE)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// ISwitch_caseContext is an interface to support dynamic dispatch.
type ISwitch_caseContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsSwitch_caseContext differentiates from other interfaces.
	IsSwitch_caseContext()
}

type Switch_caseContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptySwitch_caseContext() *Switch_caseContext {
	var p = new(Switch_caseContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_switch_case
	return p
}

func InitEmptySwitch_caseContext(p *Switch_caseContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_switch_case
}

func (*Switch_caseContext) IsSwitch_caseContext() {}

func NewSwitch_caseContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Switch_caseContext {
	var p = new(Switch_caseContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_switch_case

	return p
}

func (s *Switch_caseContext) GetParser() antlr.Parser { return s.parser }

func (s *Switch_caseContext) CopyAll(ctx *Switch_caseContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Switch_caseContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Switch_caseContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type SwitchCaseContext struct {
	Switch_caseContext
}

func NewSwitchCaseContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *SwitchCaseContext {
	var p = new(SwitchCaseContext)

	InitEmptySwitch_caseContext(&p.Switch_caseContext)
	p.parser = parser
	p.CopyAll(ctx.(*Switch_caseContext))

	return p
}

func (s *SwitchCaseContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *SwitchCaseContext) CASE_KW() antlr.TerminalNode {
	return s.GetToken(VLangGrammarCASE_KW, 0)
}

func (s *SwitchCaseContext) Expression() IExpressionContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *SwitchCaseContext) COLON() antlr.TerminalNode {
	return s.GetToken(VLangGrammarCOLON, 0)
}

func (s *SwitchCaseContext) AllStmt() []IStmtContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IStmtContext); ok {
			len++
		}
	}

	tst := make([]IStmtContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IStmtContext); ok {
			tst[i] = t.(IStmtContext)
			i++
		}
	}

	return tst
}

func (s *SwitchCaseContext) Stmt(i int) IStmtContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IStmtContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IStmtContext)
}

func (s *SwitchCaseContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterSwitchCase(s)
	}
}

func (s *SwitchCaseContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitSwitchCase(s)
	}
}

func (s *SwitchCaseContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitSwitchCase(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Switch_case() (localctx ISwitch_caseContext) {
	localctx = NewSwitch_caseContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 46, VLangGrammarRULE_switch_case)
	var _la int

	localctx = NewSwitchCaseContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(338)
		p.Match(VLangGrammarCASE_KW)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(339)
		p.expression(0)
	}
	{
		p.SetState(340)
		p.Match(VLangGrammarCOLON)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	p.SetState(344)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	for (int64(_la) & ^0x3f) == 0 && ((int64(1)<<_la)&1125968626349662) != 0 {
		{
			p.SetState(341)
			p.Stmt()
		}

		p.SetState(346)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_la = p.GetTokenStream().LA(1)
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IDefault_caseContext is an interface to support dynamic dispatch.
type IDefault_caseContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsDefault_caseContext differentiates from other interfaces.
	IsDefault_caseContext()
}

type Default_caseContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyDefault_caseContext() *Default_caseContext {
	var p = new(Default_caseContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_default_case
	return p
}

func InitEmptyDefault_caseContext(p *Default_caseContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_default_case
}

func (*Default_caseContext) IsDefault_caseContext() {}

func NewDefault_caseContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Default_caseContext {
	var p = new(Default_caseContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_default_case

	return p
}

func (s *Default_caseContext) GetParser() antlr.Parser { return s.parser }

func (s *Default_caseContext) CopyAll(ctx *Default_caseContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Default_caseContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Default_caseContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type DefaultCaseContext struct {
	Default_caseContext
}

func NewDefaultCaseContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *DefaultCaseContext {
	var p = new(DefaultCaseContext)

	InitEmptyDefault_caseContext(&p.Default_caseContext)
	p.parser = parser
	p.CopyAll(ctx.(*Default_caseContext))

	return p
}

func (s *DefaultCaseContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *DefaultCaseContext) DEFAULT_KW() antlr.TerminalNode {
	return s.GetToken(VLangGrammarDEFAULT_KW, 0)
}

func (s *DefaultCaseContext) COLON() antlr.TerminalNode {
	return s.GetToken(VLangGrammarCOLON, 0)
}

func (s *DefaultCaseContext) AllStmt() []IStmtContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IStmtContext); ok {
			len++
		}
	}

	tst := make([]IStmtContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IStmtContext); ok {
			tst[i] = t.(IStmtContext)
			i++
		}
	}

	return tst
}

func (s *DefaultCaseContext) Stmt(i int) IStmtContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IStmtContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IStmtContext)
}

func (s *DefaultCaseContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterDefaultCase(s)
	}
}

func (s *DefaultCaseContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitDefaultCase(s)
	}
}

func (s *DefaultCaseContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitDefaultCase(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Default_case() (localctx IDefault_caseContext) {
	localctx = NewDefault_caseContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 48, VLangGrammarRULE_default_case)
	var _la int

	localctx = NewDefaultCaseContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(347)
		p.Match(VLangGrammarDEFAULT_KW)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(348)
		p.Match(VLangGrammarCOLON)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	p.SetState(352)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	for (int64(_la) & ^0x3f) == 0 && ((int64(1)<<_la)&1125968626349662) != 0 {
		{
			p.SetState(349)
			p.Stmt()
		}

		p.SetState(354)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_la = p.GetTokenStream().LA(1)
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IWhile_stmtContext is an interface to support dynamic dispatch.
type IWhile_stmtContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsWhile_stmtContext differentiates from other interfaces.
	IsWhile_stmtContext()
}

type While_stmtContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyWhile_stmtContext() *While_stmtContext {
	var p = new(While_stmtContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_while_stmt
	return p
}

func InitEmptyWhile_stmtContext(p *While_stmtContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_while_stmt
}

func (*While_stmtContext) IsWhile_stmtContext() {}

func NewWhile_stmtContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *While_stmtContext {
	var p = new(While_stmtContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_while_stmt

	return p
}

func (s *While_stmtContext) GetParser() antlr.Parser { return s.parser }

func (s *While_stmtContext) CopyAll(ctx *While_stmtContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *While_stmtContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *While_stmtContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type WhileStmtContext struct {
	While_stmtContext
}

func NewWhileStmtContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *WhileStmtContext {
	var p = new(WhileStmtContext)

	InitEmptyWhile_stmtContext(&p.While_stmtContext)
	p.parser = parser
	p.CopyAll(ctx.(*While_stmtContext))

	return p
}

func (s *WhileStmtContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *WhileStmtContext) WHILE_KW() antlr.TerminalNode {
	return s.GetToken(VLangGrammarWHILE_KW, 0)
}

func (s *WhileStmtContext) Expression() IExpressionContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *WhileStmtContext) LBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLBRACE, 0)
}

func (s *WhileStmtContext) RBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarRBRACE, 0)
}

func (s *WhileStmtContext) AllStmt() []IStmtContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IStmtContext); ok {
			len++
		}
	}

	tst := make([]IStmtContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IStmtContext); ok {
			tst[i] = t.(IStmtContext)
			i++
		}
	}

	return tst
}

func (s *WhileStmtContext) Stmt(i int) IStmtContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IStmtContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IStmtContext)
}

func (s *WhileStmtContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterWhileStmt(s)
	}
}

func (s *WhileStmtContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitWhileStmt(s)
	}
}

func (s *WhileStmtContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitWhileStmt(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) While_stmt() (localctx IWhile_stmtContext) {
	localctx = NewWhile_stmtContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 50, VLangGrammarRULE_while_stmt)
	var _la int

	localctx = NewWhileStmtContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(355)
		p.Match(VLangGrammarWHILE_KW)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(356)
		p.expression(0)
	}
	{
		p.SetState(357)
		p.Match(VLangGrammarLBRACE)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	p.SetState(361)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	for (int64(_la) & ^0x3f) == 0 && ((int64(1)<<_la)&1125968626349662) != 0 {
		{
			p.SetState(358)
			p.Stmt()
		}

		p.SetState(363)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_la = p.GetTokenStream().LA(1)
	}
	{
		p.SetState(364)
		p.Match(VLangGrammarRBRACE)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IFor_stmtContext is an interface to support dynamic dispatch.
type IFor_stmtContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsFor_stmtContext differentiates from other interfaces.
	IsFor_stmtContext()
}

type For_stmtContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyFor_stmtContext() *For_stmtContext {
	var p = new(For_stmtContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_for_stmt
	return p
}

func InitEmptyFor_stmtContext(p *For_stmtContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_for_stmt
}

func (*For_stmtContext) IsFor_stmtContext() {}

func NewFor_stmtContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *For_stmtContext {
	var p = new(For_stmtContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_for_stmt

	return p
}

func (s *For_stmtContext) GetParser() antlr.Parser { return s.parser }

func (s *For_stmtContext) CopyAll(ctx *For_stmtContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *For_stmtContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *For_stmtContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type ForStmtCondContext struct {
	For_stmtContext
}

func NewForStmtCondContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *ForStmtCondContext {
	var p = new(ForStmtCondContext)

	InitEmptyFor_stmtContext(&p.For_stmtContext)
	p.parser = parser
	p.CopyAll(ctx.(*For_stmtContext))

	return p
}

func (s *ForStmtCondContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *ForStmtCondContext) FOR_KW() antlr.TerminalNode {
	return s.GetToken(VLangGrammarFOR_KW, 0)
}

func (s *ForStmtCondContext) Expression() IExpressionContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *ForStmtCondContext) LBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLBRACE, 0)
}

func (s *ForStmtCondContext) RBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarRBRACE, 0)
}

func (s *ForStmtCondContext) AllStmt() []IStmtContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IStmtContext); ok {
			len++
		}
	}

	tst := make([]IStmtContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IStmtContext); ok {
			tst[i] = t.(IStmtContext)
			i++
		}
	}

	return tst
}

func (s *ForStmtCondContext) Stmt(i int) IStmtContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IStmtContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IStmtContext)
}

func (s *ForStmtCondContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterForStmtCond(s)
	}
}

func (s *ForStmtCondContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitForStmtCond(s)
	}
}

func (s *ForStmtCondContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitForStmtCond(s)

	default:
		return t.VisitChildren(s)
	}
}

type ForAssCondContext struct {
	For_stmtContext
}

func NewForAssCondContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *ForAssCondContext {
	var p = new(ForAssCondContext)

	InitEmptyFor_stmtContext(&p.For_stmtContext)
	p.parser = parser
	p.CopyAll(ctx.(*For_stmtContext))

	return p
}

func (s *ForAssCondContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *ForAssCondContext) FOR_KW() antlr.TerminalNode {
	return s.GetToken(VLangGrammarFOR_KW, 0)
}

func (s *ForAssCondContext) Assign_stmt() IAssign_stmtContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IAssign_stmtContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IAssign_stmtContext)
}

func (s *ForAssCondContext) AllSEMI() []antlr.TerminalNode {
	return s.GetTokens(VLangGrammarSEMI)
}

func (s *ForAssCondContext) SEMI(i int) antlr.TerminalNode {
	return s.GetToken(VLangGrammarSEMI, i)
}

func (s *ForAssCondContext) AllExpression() []IExpressionContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IExpressionContext); ok {
			len++
		}
	}

	tst := make([]IExpressionContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IExpressionContext); ok {
			tst[i] = t.(IExpressionContext)
			i++
		}
	}

	return tst
}

func (s *ForAssCondContext) Expression(i int) IExpressionContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *ForAssCondContext) LBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLBRACE, 0)
}

func (s *ForAssCondContext) RBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarRBRACE, 0)
}

func (s *ForAssCondContext) AllStmt() []IStmtContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IStmtContext); ok {
			len++
		}
	}

	tst := make([]IStmtContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IStmtContext); ok {
			tst[i] = t.(IStmtContext)
			i++
		}
	}

	return tst
}

func (s *ForAssCondContext) Stmt(i int) IStmtContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IStmtContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IStmtContext)
}

func (s *ForAssCondContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterForAssCond(s)
	}
}

func (s *ForAssCondContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitForAssCond(s)
	}
}

func (s *ForAssCondContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitForAssCond(s)

	default:
		return t.VisitChildren(s)
	}
}

type ForStmtContext struct {
	For_stmtContext
}

func NewForStmtContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *ForStmtContext {
	var p = new(ForStmtContext)

	InitEmptyFor_stmtContext(&p.For_stmtContext)
	p.parser = parser
	p.CopyAll(ctx.(*For_stmtContext))

	return p
}

func (s *ForStmtContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *ForStmtContext) FOR_KW() antlr.TerminalNode {
	return s.GetToken(VLangGrammarFOR_KW, 0)
}

func (s *ForStmtContext) AllID() []antlr.TerminalNode {
	return s.GetTokens(VLangGrammarID)
}

func (s *ForStmtContext) ID(i int) antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, i)
}

func (s *ForStmtContext) COMMA() antlr.TerminalNode {
	return s.GetToken(VLangGrammarCOMMA, 0)
}

func (s *ForStmtContext) IN_KW() antlr.TerminalNode {
	return s.GetToken(VLangGrammarIN_KW, 0)
}

func (s *ForStmtContext) Expression() IExpressionContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *ForStmtContext) LBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLBRACE, 0)
}

func (s *ForStmtContext) RBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarRBRACE, 0)
}

func (s *ForStmtContext) AllStmt() []IStmtContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IStmtContext); ok {
			len++
		}
	}

	tst := make([]IStmtContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IStmtContext); ok {
			tst[i] = t.(IStmtContext)
			i++
		}
	}

	return tst
}

func (s *ForStmtContext) Stmt(i int) IStmtContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IStmtContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IStmtContext)
}

func (s *ForStmtContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterForStmt(s)
	}
}

func (s *ForStmtContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitForStmt(s)
	}
}

func (s *ForStmtContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitForStmt(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) For_stmt() (localctx IFor_stmtContext) {
	localctx = NewFor_stmtContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 52, VLangGrammarRULE_for_stmt)
	var _la int

	p.SetState(407)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}

	switch p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 30, p.GetParserRuleContext()) {
	case 1:
		localctx = NewForStmtCondContext(p, localctx)
		p.EnterOuterAlt(localctx, 1)
		{
			p.SetState(366)
			p.Match(VLangGrammarFOR_KW)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(367)
			p.expression(0)
		}
		{
			p.SetState(368)
			p.Match(VLangGrammarLBRACE)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		p.SetState(372)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_la = p.GetTokenStream().LA(1)

		for (int64(_la) & ^0x3f) == 0 && ((int64(1)<<_la)&1125968626349662) != 0 {
			{
				p.SetState(369)
				p.Stmt()
			}

			p.SetState(374)
			p.GetErrorHandler().Sync(p)
			if p.HasError() {
				goto errorExit
			}
			_la = p.GetTokenStream().LA(1)
		}
		{
			p.SetState(375)
			p.Match(VLangGrammarRBRACE)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}

	case 2:
		localctx = NewForAssCondContext(p, localctx)
		p.EnterOuterAlt(localctx, 2)
		{
			p.SetState(377)
			p.Match(VLangGrammarFOR_KW)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(378)
			p.Assign_stmt()
		}
		{
			p.SetState(379)
			p.Match(VLangGrammarSEMI)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(380)
			p.expression(0)
		}
		{
			p.SetState(381)
			p.Match(VLangGrammarSEMI)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(382)
			p.expression(0)
		}
		{
			p.SetState(383)
			p.Match(VLangGrammarLBRACE)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		p.SetState(387)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_la = p.GetTokenStream().LA(1)

		for (int64(_la) & ^0x3f) == 0 && ((int64(1)<<_la)&1125968626349662) != 0 {
			{
				p.SetState(384)
				p.Stmt()
			}

			p.SetState(389)
			p.GetErrorHandler().Sync(p)
			if p.HasError() {
				goto errorExit
			}
			_la = p.GetTokenStream().LA(1)
		}
		{
			p.SetState(390)
			p.Match(VLangGrammarRBRACE)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}

	case 3:
		localctx = NewForStmtContext(p, localctx)
		p.EnterOuterAlt(localctx, 3)
		{
			p.SetState(392)
			p.Match(VLangGrammarFOR_KW)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(393)
			p.Match(VLangGrammarID)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(394)
			p.Match(VLangGrammarCOMMA)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(395)
			p.Match(VLangGrammarID)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(396)
			p.Match(VLangGrammarIN_KW)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(397)
			p.expression(0)
		}
		{
			p.SetState(398)
			p.Match(VLangGrammarLBRACE)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		p.SetState(402)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_la = p.GetTokenStream().LA(1)

		for (int64(_la) & ^0x3f) == 0 && ((int64(1)<<_la)&1125968626349662) != 0 {
			{
				p.SetState(399)
				p.Stmt()
			}

			p.SetState(404)
			p.GetErrorHandler().Sync(p)
			if p.HasError() {
				goto errorExit
			}
			_la = p.GetTokenStream().LA(1)
		}
		{
			p.SetState(405)
			p.Match(VLangGrammarRBRACE)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}

	case antlr.ATNInvalidAltNumber:
		goto errorExit
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IRangeContext is an interface to support dynamic dispatch.
type IRangeContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsRangeContext differentiates from other interfaces.
	IsRangeContext()
}

type RangeContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyRangeContext() *RangeContext {
	var p = new(RangeContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_range
	return p
}

func InitEmptyRangeContext(p *RangeContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_range
}

func (*RangeContext) IsRangeContext() {}

func NewRangeContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *RangeContext {
	var p = new(RangeContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_range

	return p
}

func (s *RangeContext) GetParser() antlr.Parser { return s.parser }

func (s *RangeContext) CopyAll(ctx *RangeContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *RangeContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *RangeContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type NumericRangeContext struct {
	RangeContext
}

func NewNumericRangeContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *NumericRangeContext {
	var p = new(NumericRangeContext)

	InitEmptyRangeContext(&p.RangeContext)
	p.parser = parser
	p.CopyAll(ctx.(*RangeContext))

	return p
}

func (s *NumericRangeContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *NumericRangeContext) AllExpression() []IExpressionContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IExpressionContext); ok {
			len++
		}
	}

	tst := make([]IExpressionContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IExpressionContext); ok {
			tst[i] = t.(IExpressionContext)
			i++
		}
	}

	return tst
}

func (s *NumericRangeContext) Expression(i int) IExpressionContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *NumericRangeContext) AllDOT() []antlr.TerminalNode {
	return s.GetTokens(VLangGrammarDOT)
}

func (s *NumericRangeContext) DOT(i int) antlr.TerminalNode {
	return s.GetToken(VLangGrammarDOT, i)
}

func (s *NumericRangeContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterNumericRange(s)
	}
}

func (s *NumericRangeContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitNumericRange(s)
	}
}

func (s *NumericRangeContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitNumericRange(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Range_() (localctx IRangeContext) {
	localctx = NewRangeContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 54, VLangGrammarRULE_range)
	localctx = NewNumericRangeContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(409)
		p.expression(0)
	}
	{
		p.SetState(410)
		p.Match(VLangGrammarDOT)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(411)
		p.Match(VLangGrammarDOT)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(412)
		p.Match(VLangGrammarDOT)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(413)
		p.expression(0)
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// ITransfer_stmtContext is an interface to support dynamic dispatch.
type ITransfer_stmtContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsTransfer_stmtContext differentiates from other interfaces.
	IsTransfer_stmtContext()
}

type Transfer_stmtContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyTransfer_stmtContext() *Transfer_stmtContext {
	var p = new(Transfer_stmtContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_transfer_stmt
	return p
}

func InitEmptyTransfer_stmtContext(p *Transfer_stmtContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_transfer_stmt
}

func (*Transfer_stmtContext) IsTransfer_stmtContext() {}

func NewTransfer_stmtContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Transfer_stmtContext {
	var p = new(Transfer_stmtContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_transfer_stmt

	return p
}

func (s *Transfer_stmtContext) GetParser() antlr.Parser { return s.parser }

func (s *Transfer_stmtContext) CopyAll(ctx *Transfer_stmtContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Transfer_stmtContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Transfer_stmtContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type ContinueStmtContext struct {
	Transfer_stmtContext
}

func NewContinueStmtContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *ContinueStmtContext {
	var p = new(ContinueStmtContext)

	InitEmptyTransfer_stmtContext(&p.Transfer_stmtContext)
	p.parser = parser
	p.CopyAll(ctx.(*Transfer_stmtContext))

	return p
}

func (s *ContinueStmtContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *ContinueStmtContext) CONTINUE_KW() antlr.TerminalNode {
	return s.GetToken(VLangGrammarCONTINUE_KW, 0)
}

func (s *ContinueStmtContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterContinueStmt(s)
	}
}

func (s *ContinueStmtContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitContinueStmt(s)
	}
}

func (s *ContinueStmtContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitContinueStmt(s)

	default:
		return t.VisitChildren(s)
	}
}

type BreakStmtContext struct {
	Transfer_stmtContext
}

func NewBreakStmtContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *BreakStmtContext {
	var p = new(BreakStmtContext)

	InitEmptyTransfer_stmtContext(&p.Transfer_stmtContext)
	p.parser = parser
	p.CopyAll(ctx.(*Transfer_stmtContext))

	return p
}

func (s *BreakStmtContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *BreakStmtContext) BREAK_KW() antlr.TerminalNode {
	return s.GetToken(VLangGrammarBREAK_KW, 0)
}

func (s *BreakStmtContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterBreakStmt(s)
	}
}

func (s *BreakStmtContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitBreakStmt(s)
	}
}

func (s *BreakStmtContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitBreakStmt(s)

	default:
		return t.VisitChildren(s)
	}
}

type ReturnStmtContext struct {
	Transfer_stmtContext
}

func NewReturnStmtContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *ReturnStmtContext {
	var p = new(ReturnStmtContext)

	InitEmptyTransfer_stmtContext(&p.Transfer_stmtContext)
	p.parser = parser
	p.CopyAll(ctx.(*Transfer_stmtContext))

	return p
}

func (s *ReturnStmtContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *ReturnStmtContext) RETURN_KW() antlr.TerminalNode {
	return s.GetToken(VLangGrammarRETURN_KW, 0)
}

func (s *ReturnStmtContext) Expression() IExpressionContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *ReturnStmtContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterReturnStmt(s)
	}
}

func (s *ReturnStmtContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitReturnStmt(s)
	}
}

func (s *ReturnStmtContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitReturnStmt(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Transfer_stmt() (localctx ITransfer_stmtContext) {
	localctx = NewTransfer_stmtContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 56, VLangGrammarRULE_transfer_stmt)
	p.SetState(421)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}

	switch p.GetTokenStream().LA(1) {
	case VLangGrammarRETURN_KW:
		localctx = NewReturnStmtContext(p, localctx)
		p.EnterOuterAlt(localctx, 1)
		{
			p.SetState(415)
			p.Match(VLangGrammarRETURN_KW)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		p.SetState(417)
		p.GetErrorHandler().Sync(p)

		if p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 31, p.GetParserRuleContext()) == 1 {
			{
				p.SetState(416)
				p.expression(0)
			}

		} else if p.HasError() { // JIM
			goto errorExit
		}

	case VLangGrammarBREAK_KW:
		localctx = NewBreakStmtContext(p, localctx)
		p.EnterOuterAlt(localctx, 2)
		{
			p.SetState(419)
			p.Match(VLangGrammarBREAK_KW)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}

	case VLangGrammarCONTINUE_KW:
		localctx = NewContinueStmtContext(p, localctx)
		p.EnterOuterAlt(localctx, 3)
		{
			p.SetState(420)
			p.Match(VLangGrammarCONTINUE_KW)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}

	default:
		p.SetError(antlr.NewNoViableAltException(p, nil, nil, nil, nil, nil))
		goto errorExit
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IFunc_callContext is an interface to support dynamic dispatch.
type IFunc_callContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsFunc_callContext differentiates from other interfaces.
	IsFunc_callContext()
}

type Func_callContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyFunc_callContext() *Func_callContext {
	var p = new(Func_callContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_func_call
	return p
}

func InitEmptyFunc_callContext(p *Func_callContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_func_call
}

func (*Func_callContext) IsFunc_callContext() {}

func NewFunc_callContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Func_callContext {
	var p = new(Func_callContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_func_call

	return p
}

func (s *Func_callContext) GetParser() antlr.Parser { return s.parser }

func (s *Func_callContext) CopyAll(ctx *Func_callContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Func_callContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Func_callContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type FuncCallContext struct {
	Func_callContext
}

func NewFuncCallContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *FuncCallContext {
	var p = new(FuncCallContext)

	InitEmptyFunc_callContext(&p.Func_callContext)
	p.parser = parser
	p.CopyAll(ctx.(*Func_callContext))

	return p
}

func (s *FuncCallContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *FuncCallContext) Id_pattern() IId_patternContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IId_patternContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IId_patternContext)
}

func (s *FuncCallContext) LPAREN() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLPAREN, 0)
}

func (s *FuncCallContext) RPAREN() antlr.TerminalNode {
	return s.GetToken(VLangGrammarRPAREN, 0)
}

func (s *FuncCallContext) Arg_list() IArg_listContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IArg_listContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IArg_listContext)
}

func (s *FuncCallContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterFuncCall(s)
	}
}

func (s *FuncCallContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitFuncCall(s)
	}
}

func (s *FuncCallContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitFuncCall(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Func_call() (localctx IFunc_callContext) {
	localctx = NewFunc_callContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 58, VLangGrammarRULE_func_call)
	var _la int

	localctx = NewFuncCallContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(423)
		p.Id_pattern()
	}
	{
		p.SetState(424)
		p.Match(VLangGrammarLPAREN)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	p.SetState(426)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	if (int64(_la) & ^0x3f) == 0 && ((int64(1)<<_la)&2216984809046016) != 0 {
		{
			p.SetState(425)
			p.Arg_list()
		}

	}
	{
		p.SetState(428)
		p.Match(VLangGrammarRPAREN)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IBlock_indContext is an interface to support dynamic dispatch.
type IBlock_indContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsBlock_indContext differentiates from other interfaces.
	IsBlock_indContext()
}

type Block_indContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyBlock_indContext() *Block_indContext {
	var p = new(Block_indContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_block_ind
	return p
}

func InitEmptyBlock_indContext(p *Block_indContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_block_ind
}

func (*Block_indContext) IsBlock_indContext() {}

func NewBlock_indContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Block_indContext {
	var p = new(Block_indContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_block_ind

	return p
}

func (s *Block_indContext) GetParser() antlr.Parser { return s.parser }

func (s *Block_indContext) CopyAll(ctx *Block_indContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Block_indContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Block_indContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type BlockIndContext struct {
	Block_indContext
}

func NewBlockIndContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *BlockIndContext {
	var p = new(BlockIndContext)

	InitEmptyBlock_indContext(&p.Block_indContext)
	p.parser = parser
	p.CopyAll(ctx.(*Block_indContext))

	return p
}

func (s *BlockIndContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *BlockIndContext) LBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLBRACE, 0)
}

func (s *BlockIndContext) RBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarRBRACE, 0)
}

func (s *BlockIndContext) AllStmt() []IStmtContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IStmtContext); ok {
			len++
		}
	}

	tst := make([]IStmtContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IStmtContext); ok {
			tst[i] = t.(IStmtContext)
			i++
		}
	}

	return tst
}

func (s *BlockIndContext) Stmt(i int) IStmtContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IStmtContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IStmtContext)
}

func (s *BlockIndContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterBlockInd(s)
	}
}

func (s *BlockIndContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitBlockInd(s)
	}
}

func (s *BlockIndContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitBlockInd(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Block_ind() (localctx IBlock_indContext) {
	localctx = NewBlock_indContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 60, VLangGrammarRULE_block_ind)
	var _la int

	localctx = NewBlockIndContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(430)
		p.Match(VLangGrammarLBRACE)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	p.SetState(434)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	for (int64(_la) & ^0x3f) == 0 && ((int64(1)<<_la)&1125968626349662) != 0 {
		{
			p.SetState(431)
			p.Stmt()
		}

		p.SetState(436)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_la = p.GetTokenStream().LA(1)
	}
	{
		p.SetState(437)
		p.Match(VLangGrammarRBRACE)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IArg_listContext is an interface to support dynamic dispatch.
type IArg_listContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsArg_listContext differentiates from other interfaces.
	IsArg_listContext()
}

type Arg_listContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyArg_listContext() *Arg_listContext {
	var p = new(Arg_listContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_arg_list
	return p
}

func InitEmptyArg_listContext(p *Arg_listContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_arg_list
}

func (*Arg_listContext) IsArg_listContext() {}

func NewArg_listContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Arg_listContext {
	var p = new(Arg_listContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_arg_list

	return p
}

func (s *Arg_listContext) GetParser() antlr.Parser { return s.parser }

func (s *Arg_listContext) CopyAll(ctx *Arg_listContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Arg_listContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Arg_listContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type ArgListContext struct {
	Arg_listContext
}

func NewArgListContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *ArgListContext {
	var p = new(ArgListContext)

	InitEmptyArg_listContext(&p.Arg_listContext)
	p.parser = parser
	p.CopyAll(ctx.(*Arg_listContext))

	return p
}

func (s *ArgListContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *ArgListContext) AllFunc_arg() []IFunc_argContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IFunc_argContext); ok {
			len++
		}
	}

	tst := make([]IFunc_argContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IFunc_argContext); ok {
			tst[i] = t.(IFunc_argContext)
			i++
		}
	}

	return tst
}

func (s *ArgListContext) Func_arg(i int) IFunc_argContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IFunc_argContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IFunc_argContext)
}

func (s *ArgListContext) AllCOMMA() []antlr.TerminalNode {
	return s.GetTokens(VLangGrammarCOMMA)
}

func (s *ArgListContext) COMMA(i int) antlr.TerminalNode {
	return s.GetToken(VLangGrammarCOMMA, i)
}

func (s *ArgListContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterArgList(s)
	}
}

func (s *ArgListContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitArgList(s)
	}
}

func (s *ArgListContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitArgList(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Arg_list() (localctx IArg_listContext) {
	localctx = NewArg_listContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 62, VLangGrammarRULE_arg_list)
	var _la int

	localctx = NewArgListContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(439)
		p.Func_arg()
	}
	p.SetState(444)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	for _la == VLangGrammarCOMMA {
		{
			p.SetState(440)
			p.Match(VLangGrammarCOMMA)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(441)
			p.Func_arg()
		}

		p.SetState(446)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_la = p.GetTokenStream().LA(1)
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IFunc_argContext is an interface to support dynamic dispatch.
type IFunc_argContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsFunc_argContext differentiates from other interfaces.
	IsFunc_argContext()
}

type Func_argContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyFunc_argContext() *Func_argContext {
	var p = new(Func_argContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_func_arg
	return p
}

func InitEmptyFunc_argContext(p *Func_argContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_func_arg
}

func (*Func_argContext) IsFunc_argContext() {}

func NewFunc_argContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Func_argContext {
	var p = new(Func_argContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_func_arg

	return p
}

func (s *Func_argContext) GetParser() antlr.Parser { return s.parser }

func (s *Func_argContext) CopyAll(ctx *Func_argContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Func_argContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Func_argContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type FuncArgContext struct {
	Func_argContext
}

func NewFuncArgContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *FuncArgContext {
	var p = new(FuncArgContext)

	InitEmptyFunc_argContext(&p.Func_argContext)
	p.parser = parser
	p.CopyAll(ctx.(*Func_argContext))

	return p
}

func (s *FuncArgContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *FuncArgContext) Id_pattern() IId_patternContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IId_patternContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IId_patternContext)
}

func (s *FuncArgContext) Expression() IExpressionContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *FuncArgContext) ID() antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, 0)
}

func (s *FuncArgContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterFuncArg(s)
	}
}

func (s *FuncArgContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitFuncArg(s)
	}
}

func (s *FuncArgContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitFuncArg(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Func_arg() (localctx IFunc_argContext) {
	localctx = NewFunc_argContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 64, VLangGrammarRULE_func_arg)
	localctx = NewFuncArgContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	p.SetState(448)
	p.GetErrorHandler().Sync(p)

	if p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 36, p.GetParserRuleContext()) == 1 {
		{
			p.SetState(447)
			p.Match(VLangGrammarID)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}

	} else if p.HasError() { // JIM
		goto errorExit
	}
	p.SetState(452)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}

	switch p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 37, p.GetParserRuleContext()) {
	case 1:
		{
			p.SetState(450)
			p.Id_pattern()
		}

	case 2:
		{
			p.SetState(451)
			p.expression(0)
		}

	case antlr.ATNInvalidAltNumber:
		goto errorExit
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IFunc_dclContext is an interface to support dynamic dispatch.
type IFunc_dclContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsFunc_dclContext differentiates from other interfaces.
	IsFunc_dclContext()
}

type Func_dclContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyFunc_dclContext() *Func_dclContext {
	var p = new(Func_dclContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_func_dcl
	return p
}

func InitEmptyFunc_dclContext(p *Func_dclContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_func_dcl
}

func (*Func_dclContext) IsFunc_dclContext() {}

func NewFunc_dclContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Func_dclContext {
	var p = new(Func_dclContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_func_dcl

	return p
}

func (s *Func_dclContext) GetParser() antlr.Parser { return s.parser }

func (s *Func_dclContext) CopyAll(ctx *Func_dclContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Func_dclContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Func_dclContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type FuncDeclContext struct {
	Func_dclContext
}

func NewFuncDeclContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *FuncDeclContext {
	var p = new(FuncDeclContext)

	InitEmptyFunc_dclContext(&p.Func_dclContext)
	p.parser = parser
	p.CopyAll(ctx.(*Func_dclContext))

	return p
}

func (s *FuncDeclContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *FuncDeclContext) FUNC() antlr.TerminalNode {
	return s.GetToken(VLangGrammarFUNC, 0)
}

func (s *FuncDeclContext) ID() antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, 0)
}

func (s *FuncDeclContext) LPAREN() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLPAREN, 0)
}

func (s *FuncDeclContext) RPAREN() antlr.TerminalNode {
	return s.GetToken(VLangGrammarRPAREN, 0)
}

func (s *FuncDeclContext) LBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLBRACE, 0)
}

func (s *FuncDeclContext) RBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarRBRACE, 0)
}

func (s *FuncDeclContext) Param_list() IParam_listContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IParam_listContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IParam_listContext)
}

func (s *FuncDeclContext) Type_() ITypeContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(ITypeContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(ITypeContext)
}

func (s *FuncDeclContext) AllStmt() []IStmtContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IStmtContext); ok {
			len++
		}
	}

	tst := make([]IStmtContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IStmtContext); ok {
			tst[i] = t.(IStmtContext)
			i++
		}
	}

	return tst
}

func (s *FuncDeclContext) Stmt(i int) IStmtContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IStmtContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IStmtContext)
}

func (s *FuncDeclContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterFuncDecl(s)
	}
}

func (s *FuncDeclContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitFuncDecl(s)
	}
}

func (s *FuncDeclContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitFuncDecl(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Func_dcl() (localctx IFunc_dclContext) {
	localctx = NewFunc_dclContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 66, VLangGrammarRULE_func_dcl)
	var _la int

	localctx = NewFuncDeclContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(454)
		p.Match(VLangGrammarFUNC)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(455)
		p.Match(VLangGrammarID)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(456)
		p.Match(VLangGrammarLPAREN)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	p.SetState(458)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	if _la == VLangGrammarID {
		{
			p.SetState(457)
			p.Param_list()
		}

	}
	{
		p.SetState(460)
		p.Match(VLangGrammarRPAREN)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	p.SetState(462)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	if _la == VLangGrammarLBRACK || _la == VLangGrammarID {
		{
			p.SetState(461)
			p.Type_()
		}

	}
	{
		p.SetState(464)
		p.Match(VLangGrammarLBRACE)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	p.SetState(468)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	for (int64(_la) & ^0x3f) == 0 && ((int64(1)<<_la)&1125968626349662) != 0 {
		{
			p.SetState(465)
			p.Stmt()
		}

		p.SetState(470)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_la = p.GetTokenStream().LA(1)
	}
	{
		p.SetState(471)
		p.Match(VLangGrammarRBRACE)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IParam_listContext is an interface to support dynamic dispatch.
type IParam_listContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsParam_listContext differentiates from other interfaces.
	IsParam_listContext()
}

type Param_listContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyParam_listContext() *Param_listContext {
	var p = new(Param_listContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_param_list
	return p
}

func InitEmptyParam_listContext(p *Param_listContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_param_list
}

func (*Param_listContext) IsParam_listContext() {}

func NewParam_listContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Param_listContext {
	var p = new(Param_listContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_param_list

	return p
}

func (s *Param_listContext) GetParser() antlr.Parser { return s.parser }

func (s *Param_listContext) CopyAll(ctx *Param_listContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Param_listContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Param_listContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type ParamListContext struct {
	Param_listContext
}

func NewParamListContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *ParamListContext {
	var p = new(ParamListContext)

	InitEmptyParam_listContext(&p.Param_listContext)
	p.parser = parser
	p.CopyAll(ctx.(*Param_listContext))

	return p
}

func (s *ParamListContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *ParamListContext) AllFunc_param() []IFunc_paramContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IFunc_paramContext); ok {
			len++
		}
	}

	tst := make([]IFunc_paramContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IFunc_paramContext); ok {
			tst[i] = t.(IFunc_paramContext)
			i++
		}
	}

	return tst
}

func (s *ParamListContext) Func_param(i int) IFunc_paramContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IFunc_paramContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IFunc_paramContext)
}

func (s *ParamListContext) AllCOMMA() []antlr.TerminalNode {
	return s.GetTokens(VLangGrammarCOMMA)
}

func (s *ParamListContext) COMMA(i int) antlr.TerminalNode {
	return s.GetToken(VLangGrammarCOMMA, i)
}

func (s *ParamListContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterParamList(s)
	}
}

func (s *ParamListContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitParamList(s)
	}
}

func (s *ParamListContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitParamList(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Param_list() (localctx IParam_listContext) {
	localctx = NewParam_listContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 68, VLangGrammarRULE_param_list)
	var _la int

	localctx = NewParamListContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(473)
		p.Func_param()
	}
	p.SetState(478)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	for _la == VLangGrammarCOMMA {
		{
			p.SetState(474)
			p.Match(VLangGrammarCOMMA)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}
		{
			p.SetState(475)
			p.Func_param()
		}

		p.SetState(480)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_la = p.GetTokenStream().LA(1)
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IFunc_paramContext is an interface to support dynamic dispatch.
type IFunc_paramContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsFunc_paramContext differentiates from other interfaces.
	IsFunc_paramContext()
}

type Func_paramContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyFunc_paramContext() *Func_paramContext {
	var p = new(Func_paramContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_func_param
	return p
}

func InitEmptyFunc_paramContext(p *Func_paramContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_func_param
}

func (*Func_paramContext) IsFunc_paramContext() {}

func NewFunc_paramContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Func_paramContext {
	var p = new(Func_paramContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_func_param

	return p
}

func (s *Func_paramContext) GetParser() antlr.Parser { return s.parser }

func (s *Func_paramContext) CopyAll(ctx *Func_paramContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Func_paramContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Func_paramContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type FuncParamContext struct {
	Func_paramContext
}

func NewFuncParamContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *FuncParamContext {
	var p = new(FuncParamContext)

	InitEmptyFunc_paramContext(&p.Func_paramContext)
	p.parser = parser
	p.CopyAll(ctx.(*Func_paramContext))

	return p
}

func (s *FuncParamContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *FuncParamContext) ID() antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, 0)
}

func (s *FuncParamContext) Type_() ITypeContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(ITypeContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(ITypeContext)
}

func (s *FuncParamContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterFuncParam(s)
	}
}

func (s *FuncParamContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitFuncParam(s)
	}
}

func (s *FuncParamContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitFuncParam(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Func_param() (localctx IFunc_paramContext) {
	localctx = NewFunc_paramContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 70, VLangGrammarRULE_func_param)
	localctx = NewFuncParamContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(481)
		p.Match(VLangGrammarID)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(482)
		p.Type_()
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IStrct_dclContext is an interface to support dynamic dispatch.
type IStrct_dclContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsStrct_dclContext differentiates from other interfaces.
	IsStrct_dclContext()
}

type Strct_dclContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyStrct_dclContext() *Strct_dclContext {
	var p = new(Strct_dclContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_strct_dcl
	return p
}

func InitEmptyStrct_dclContext(p *Strct_dclContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_strct_dcl
}

func (*Strct_dclContext) IsStrct_dclContext() {}

func NewStrct_dclContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Strct_dclContext {
	var p = new(Strct_dclContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_strct_dcl

	return p
}

func (s *Strct_dclContext) GetParser() antlr.Parser { return s.parser }

func (s *Strct_dclContext) CopyAll(ctx *Strct_dclContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Strct_dclContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Strct_dclContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type StructDeclContext struct {
	Strct_dclContext
}

func NewStructDeclContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *StructDeclContext {
	var p = new(StructDeclContext)

	InitEmptyStrct_dclContext(&p.Strct_dclContext)
	p.parser = parser
	p.CopyAll(ctx.(*Strct_dclContext))

	return p
}

func (s *StructDeclContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *StructDeclContext) STR() antlr.TerminalNode {
	return s.GetToken(VLangGrammarSTR, 0)
}

func (s *StructDeclContext) ID() antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, 0)
}

func (s *StructDeclContext) LBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarLBRACE, 0)
}

func (s *StructDeclContext) RBRACE() antlr.TerminalNode {
	return s.GetToken(VLangGrammarRBRACE, 0)
}

func (s *StructDeclContext) AllStruct_prop() []IStruct_propContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IStruct_propContext); ok {
			len++
		}
	}

	tst := make([]IStruct_propContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IStruct_propContext); ok {
			tst[i] = t.(IStruct_propContext)
			i++
		}
	}

	return tst
}

func (s *StructDeclContext) Struct_prop(i int) IStruct_propContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IStruct_propContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IStruct_propContext)
}

func (s *StructDeclContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterStructDecl(s)
	}
}

func (s *StructDeclContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitStructDecl(s)
	}
}

func (s *StructDeclContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitStructDecl(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Strct_dcl() (localctx IStrct_dclContext) {
	localctx = NewStrct_dclContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 72, VLangGrammarRULE_strct_dcl)
	var _la int

	localctx = NewStructDeclContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(484)
		p.Match(VLangGrammarSTR)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(485)
		p.Match(VLangGrammarID)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(486)
		p.Match(VLangGrammarLBRACE)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	p.SetState(488)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	for ok := true; ok; ok = _la == VLangGrammarLBRACK || _la == VLangGrammarID {
		{
			p.SetState(487)
			p.Struct_prop()
		}

		p.SetState(490)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_la = p.GetTokenStream().LA(1)
	}
	{
		p.SetState(492)
		p.Match(VLangGrammarRBRACE)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IStruct_propContext is an interface to support dynamic dispatch.
type IStruct_propContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser
	// IsStruct_propContext differentiates from other interfaces.
	IsStruct_propContext()
}

type Struct_propContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyStruct_propContext() *Struct_propContext {
	var p = new(Struct_propContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_struct_prop
	return p
}

func InitEmptyStruct_propContext(p *Struct_propContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_struct_prop
}

func (*Struct_propContext) IsStruct_propContext() {}

func NewStruct_propContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Struct_propContext {
	var p = new(Struct_propContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_struct_prop

	return p
}

func (s *Struct_propContext) GetParser() antlr.Parser { return s.parser }

func (s *Struct_propContext) CopyAll(ctx *Struct_propContext) {
	s.CopyFrom(&ctx.BaseParserRuleContext)
}

func (s *Struct_propContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Struct_propContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

type StructAttrContext struct {
	Struct_propContext
}

func NewStructAttrContext(parser antlr.Parser, ctx antlr.ParserRuleContext) *StructAttrContext {
	var p = new(StructAttrContext)

	InitEmptyStruct_propContext(&p.Struct_propContext)
	p.parser = parser
	p.CopyAll(ctx.(*Struct_propContext))

	return p
}

func (s *StructAttrContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *StructAttrContext) Type_() ITypeContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(ITypeContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(ITypeContext)
}

func (s *StructAttrContext) ID() antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, 0)
}

func (s *StructAttrContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterStructAttr(s)
	}
}

func (s *StructAttrContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitStructAttr(s)
	}
}

func (s *StructAttrContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitStructAttr(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Struct_prop() (localctx IStruct_propContext) {
	localctx = NewStruct_propContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 74, VLangGrammarRULE_struct_prop)
	localctx = NewStructAttrContext(p, localctx)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(494)
		p.Type_()
	}
	{
		p.SetState(495)
		p.Match(VLangGrammarID)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IStruct_param_listContext is an interface to support dynamic dispatch.
type IStruct_param_listContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser

	// Getter signatures
	AllStruct_param() []IStruct_paramContext
	Struct_param(i int) IStruct_paramContext
	AllCOMMA() []antlr.TerminalNode
	COMMA(i int) antlr.TerminalNode

	// IsStruct_param_listContext differentiates from other interfaces.
	IsStruct_param_listContext()
}

type Struct_param_listContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyStruct_param_listContext() *Struct_param_listContext {
	var p = new(Struct_param_listContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_struct_param_list
	return p
}

func InitEmptyStruct_param_listContext(p *Struct_param_listContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_struct_param_list
}

func (*Struct_param_listContext) IsStruct_param_listContext() {}

func NewStruct_param_listContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Struct_param_listContext {
	var p = new(Struct_param_listContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_struct_param_list

	return p
}

func (s *Struct_param_listContext) GetParser() antlr.Parser { return s.parser }

func (s *Struct_param_listContext) AllStruct_param() []IStruct_paramContext {
	children := s.GetChildren()
	len := 0
	for _, ctx := range children {
		if _, ok := ctx.(IStruct_paramContext); ok {
			len++
		}
	}

	tst := make([]IStruct_paramContext, len)
	i := 0
	for _, ctx := range children {
		if t, ok := ctx.(IStruct_paramContext); ok {
			tst[i] = t.(IStruct_paramContext)
			i++
		}
	}

	return tst
}

func (s *Struct_param_listContext) Struct_param(i int) IStruct_paramContext {
	var t antlr.RuleContext
	j := 0
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IStruct_paramContext); ok {
			if j == i {
				t = ctx.(antlr.RuleContext)
				break
			}
			j++
		}
	}

	if t == nil {
		return nil
	}

	return t.(IStruct_paramContext)
}

func (s *Struct_param_listContext) AllCOMMA() []antlr.TerminalNode {
	return s.GetTokens(VLangGrammarCOMMA)
}

func (s *Struct_param_listContext) COMMA(i int) antlr.TerminalNode {
	return s.GetToken(VLangGrammarCOMMA, i)
}

func (s *Struct_param_listContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Struct_param_listContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

func (s *Struct_param_listContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterStruct_param_list(s)
	}
}

func (s *Struct_param_listContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitStruct_param_list(s)
	}
}

func (s *Struct_param_listContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitStruct_param_list(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Struct_param_list() (localctx IStruct_param_listContext) {
	localctx = NewStruct_param_listContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 76, VLangGrammarRULE_struct_param_list)
	var _la int

	var _alt int

	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(497)
		p.Struct_param()
	}
	p.SetState(502)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_alt = p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 43, p.GetParserRuleContext())
	if p.HasError() {
		goto errorExit
	}
	for _alt != 2 && _alt != antlr.ATNInvalidAltNumber {
		if _alt == 1 {
			{
				p.SetState(498)
				p.Match(VLangGrammarCOMMA)
				if p.HasError() {
					// Recognition error - abort rule
					goto errorExit
				}
			}
			{
				p.SetState(499)
				p.Struct_param()
			}

		}
		p.SetState(504)
		p.GetErrorHandler().Sync(p)
		if p.HasError() {
			goto errorExit
		}
		_alt = p.GetInterpreter().AdaptivePredict(p.BaseParser, p.GetTokenStream(), 43, p.GetParserRuleContext())
		if p.HasError() {
			goto errorExit
		}
	}
	p.SetState(506)
	p.GetErrorHandler().Sync(p)
	if p.HasError() {
		goto errorExit
	}
	_la = p.GetTokenStream().LA(1)

	if _la == VLangGrammarCOMMA {
		{
			p.SetState(505)
			p.Match(VLangGrammarCOMMA)
			if p.HasError() {
				// Recognition error - abort rule
				goto errorExit
			}
		}

	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

// IStruct_paramContext is an interface to support dynamic dispatch.
type IStruct_paramContext interface {
	antlr.ParserRuleContext

	// GetParser returns the parser.
	GetParser() antlr.Parser

	// Getter signatures
	ID() antlr.TerminalNode
	COLON() antlr.TerminalNode
	Expression() IExpressionContext

	// IsStruct_paramContext differentiates from other interfaces.
	IsStruct_paramContext()
}

type Struct_paramContext struct {
	antlr.BaseParserRuleContext
	parser antlr.Parser
}

func NewEmptyStruct_paramContext() *Struct_paramContext {
	var p = new(Struct_paramContext)
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_struct_param
	return p
}

func InitEmptyStruct_paramContext(p *Struct_paramContext) {
	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, nil, -1)
	p.RuleIndex = VLangGrammarRULE_struct_param
}

func (*Struct_paramContext) IsStruct_paramContext() {}

func NewStruct_paramContext(parser antlr.Parser, parent antlr.ParserRuleContext, invokingState int) *Struct_paramContext {
	var p = new(Struct_paramContext)

	antlr.InitBaseParserRuleContext(&p.BaseParserRuleContext, parent, invokingState)

	p.parser = parser
	p.RuleIndex = VLangGrammarRULE_struct_param

	return p
}

func (s *Struct_paramContext) GetParser() antlr.Parser { return s.parser }

func (s *Struct_paramContext) ID() antlr.TerminalNode {
	return s.GetToken(VLangGrammarID, 0)
}

func (s *Struct_paramContext) COLON() antlr.TerminalNode {
	return s.GetToken(VLangGrammarCOLON, 0)
}

func (s *Struct_paramContext) Expression() IExpressionContext {
	var t antlr.RuleContext
	for _, ctx := range s.GetChildren() {
		if _, ok := ctx.(IExpressionContext); ok {
			t = ctx.(antlr.RuleContext)
			break
		}
	}

	if t == nil {
		return nil
	}

	return t.(IExpressionContext)
}

func (s *Struct_paramContext) GetRuleContext() antlr.RuleContext {
	return s
}

func (s *Struct_paramContext) ToStringTree(ruleNames []string, recog antlr.Recognizer) string {
	return antlr.TreesStringTree(s, ruleNames, recog)
}

func (s *Struct_paramContext) EnterRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.EnterStruct_param(s)
	}
}

func (s *Struct_paramContext) ExitRule(listener antlr.ParseTreeListener) {
	if listenerT, ok := listener.(VLangGrammarListener); ok {
		listenerT.ExitStruct_param(s)
	}
}

func (s *Struct_paramContext) Accept(visitor antlr.ParseTreeVisitor) interface{} {
	switch t := visitor.(type) {
	case VLangGrammarVisitor:
		return t.VisitStruct_param(s)

	default:
		return t.VisitChildren(s)
	}
}

func (p *VLangGrammar) Struct_param() (localctx IStruct_paramContext) {
	localctx = NewStruct_paramContext(p, p.GetParserRuleContext(), p.GetState())
	p.EnterRule(localctx, 78, VLangGrammarRULE_struct_param)
	p.EnterOuterAlt(localctx, 1)
	{
		p.SetState(508)
		p.Match(VLangGrammarID)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(509)
		p.Match(VLangGrammarCOLON)
		if p.HasError() {
			// Recognition error - abort rule
			goto errorExit
		}
	}
	{
		p.SetState(510)
		p.expression(0)
	}

errorExit:
	if p.HasError() {
		v := p.GetError()
		localctx.SetException(v)
		p.GetErrorHandler().ReportError(p, v)
		p.GetErrorHandler().Recover(p, v)
		p.SetError(nil)
	}
	p.ExitRule()
	return localctx
	goto errorExit // Trick to prevent compiler error if the label is not used
}

func (p *VLangGrammar) Sempred(localctx antlr.RuleContext, ruleIndex, predIndex int) bool {
	switch ruleIndex {
	case 18:
		var t *ExpressionContext = nil
		if localctx != nil {
			t = localctx.(*ExpressionContext)
		}
		return p.Expression_Sempred(t, predIndex)

	default:
		panic("No predicate with index: " + fmt.Sprint(ruleIndex))
	}
}

func (p *VLangGrammar) Expression_Sempred(localctx antlr.RuleContext, predIndex int) bool {
	switch predIndex {
	case 0:
		return p.Precpred(p.GetParserRuleContext(), 7)

	case 1:
		return p.Precpred(p.GetParserRuleContext(), 6)

	case 2:
		return p.Precpred(p.GetParserRuleContext(), 5)

	case 3:
		return p.Precpred(p.GetParserRuleContext(), 4)

	case 4:
		return p.Precpred(p.GetParserRuleContext(), 3)

	case 5:
		return p.Precpred(p.GetParserRuleContext(), 2)

	default:
		panic("No predicate with index: " + fmt.Sprint(predIndex))
	}
}
