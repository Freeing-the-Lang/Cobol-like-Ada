with Lexer;
with AST;

package Parser is

   function Parse_Program(Tokens : Lexer.Token_List) return AST.Program;

end Parser;
