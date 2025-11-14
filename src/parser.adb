with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Lexer;
with AST;

package body Parser is

   Tok : Lexer.Token_List;
   Pos : Natural := 1;

   function Peek return Lexer.Token is
   begin
      return Tok(Pos);
   end Peek;

   function Advance return Lexer.Token is
      T : Lexer.Token := Tok(Pos);
   begin
      Pos := Pos + 1;
      return T;
   end Advance;

   -------------------------------------------------------------
   -- Expression parsing
   -------------------------------------------------------------
   function Parse_Expr return AST.Expr is
      T   : Lexer.Token := Advance;
      E   : AST.Expr;
   begin
      case T.Kind is

         when Lexer.Number_Lit =>
            E.Kind  := AST.Number;
            E.Value := T.Text;

         when Lexer.String_Lit =>
            E.Kind  := AST.String_Lit;
            E.Value := T.Text;

         when Lexer.Ident =>
            E.Kind := AST.Ident;
            E.Name := T.Text;

            -- If IDENT( ... ) => Call
            if Peek.Kind = Lexer.LParen then
               Advance; -- '('
               declare
                  Count : Natural := 0;
               begin
                  while Peek.Kind /= Lexer.RParen loop
                     E.Args(Count) := new AST.Expr'(Parse_Expr);
                     Count := Count + 1;

                     exit when Peek.Kind /= Lexer.Comma;
                     Advance; -- comma
                  end loop;

                  E.Arg_Count := Count;
                  Advance; -- ')'
                  E.Kind := AST.Call_Expr;
               end;
            end if;

         when others =>
            null;
      end case;

      return E;
   end Parse_Expr;

   -------------------------------------------------------------
   -- Statement parsing
   -------------------------------------------------------------
   function Parse_Stmt return AST.Stmt is
      S : AST.Stmt;
      T : Lexer.Token := Peek;
   begin
      -- DISPLAY <expr>
      if T.Kind = Lexer.Ident
        and then To_String(T.Text) = "DISPLAY"
      then
         Advance; -- consume DISPLAY
         S.Kind := AST.Display_Stmt;
         S.Expr_Node := Parse_Expr;

      else
         S.Kind := AST.Call_Stmt;
         S.Expr_Node := Parse_Expr;
      end if;

      return S;
   end Parse_Stmt;

   -------------------------------------------------------------
   -- Program
   -------------------------------------------------------------
   function Parse_Program(Tokens : Lexer.Token_List) return Program_Type is
      P : AST.Program;
   begin
      Tok := Tokens;
      Pos := 1;

      while Peek.Kind /= Lexer.EOF_Token loop
         P.Stmts(P.Count) := new AST.Stmt'(Parse_Stmt);
         P.Count := P.Count + 1;
      end loop;

      return Program_Type(P);
   end Parse_Program;

end Parser;
