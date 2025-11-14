with Lexer;
with AST;

package Parser is

   -- Public alias type for AST.Program
   type Program_Type is new AST.Program;

   -- Main entry
   function Parse_Program(Tokens : Lexer.Token_List) return Program_Type;

end Parser;
