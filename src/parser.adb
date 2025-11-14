with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AST;

package body Parser is

   Pos : Natural := 1;
   Tok : Token_List;

   function Peek return Token is
   begin
      return Tok(Pos);
   end;

   function Advance return Token is
      T : Token := Tok(Pos);
   begin
      Pos := Pos + 1;
      return T;
   end;

   function Parse_Expr return AST.Expr is
      T : Token := Advance;
      E : AST.Expr;
   begin
      case T.Kind is

         when Number_Lit =>
            E.Kind  := Number;
            E.Value := T.Text;

         when String_Lit =>
            E.Kind  := String_Lit;
            E.Value := T.Text;

         when Ident =>
            E.Kind := Ident;
            E.Name := T.Text;

            if Peek.Kind = LParen then
               Advance; -- (
               declare
                  Count : Natural := 0;
               begin
                  while Peek.Kind /= RParen loop
                     E.Args(Count) := new AST.Expr'(Parse_Expr);
                     Count := Count + 1;

                     exit when Peek.Kind /= Comma;
                     Advance;
                  end loop;

                  E.Arg_Count := Count;
                  Advance; -- )
               end;
               E.Kind := Call_Expr;
            end if;

         when others =>
            null;

      end case;

      return E;
   end Parse_Expr;

   function Parse_Stmt return AST.Stmt is
      S : AST.Stmt;
      T : Token := Peek;
   begin
      if T.Kind = Ident and then To_String(T.Text) = "DISPLAY" then
         Advance;
         S.Kind := Display_Stmt;
         S.Expr_Node := Parse_Expr;
      else
         S.Kind := Call_Stmt;
         S.Expr_Node := Parse_Expr;
      end if;
      return S;
   end;

   function Parse_Program(Tokens : Token_List) return AST.Program is
      P : AST.Program;
   begin
      Tok := Tokens;
      Pos := 1;

      while Peek.Kind /= EOF_Token loop
         P.Stmts(P.Count) := new AST.Stmt'(Parse_Stmt);
         P.Count := P.Count + 1;
      end loop;

      return P;
   end;

end Parser;
